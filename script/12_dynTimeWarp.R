#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================

#split the dataset by country to form list of datasets
X <- 
  rsv_all %>%  
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) %>%
  dplyr::filter(yr >= 2021) %>%
  group_by(country) %>%
  dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, seqwk, cases) %>%
  dplyr::arrange(country, seqwk, cases) %>%
 
  #dplyr::select(seqwk, seqwk2, cases, country) %>%
  base::split(list(.$country))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[base::unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models & fitted time series data for each country
Gmodels <- list()
DsTs <- list()
DsLog <- list()
Dshc <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

#iterate for each country, extract fitted case values
for (i in names(X)){
  DsTs[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% 
    dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
  
  DsLog[[i]] = data.frame(deriv = diff(log(DsTs[[i]]$fitcases+1))/diff(DsTs[[i]]$seqwk)) %>%
    dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
}

#====================================================================
#DYNAMIC TIME WARPING (DTW)
#====================================================================

#DTW cluster evaluation to determine window size
cfg <- compare_clusterings_configs(
  types = "hierarchical", 
  k = 4L, 
  #preprocs = pdc_configs("preproc", zscore),
  controls = list(hierarchical = hierarchical_control(method = "average")), 
  distances = pdc_configs("distance", hierarchical = list(dtw_basic = list(window.size = seq(from = 1L, to = 104L, by = 1L), norm = c("L1")))),
  no.expand = c("window.size", "norm" ))

evaluators <- cvi_evaluators(c("DBstar", "DB"))
comparison <- compare_clusterings(dtw_Dshc, 
                                  types = "hierarchical", 
                                  configs = cfg, 
                                  seed = 8L, 
                                  score.clus = evaluators$score, 
                                  pick.clus = evaluators$pick)

#dataset for varying window size
hc_wsize <- 
  bind_rows(
  as.data.frame(
    (comparison$results$hierarchical[, c("distance", 
                                         "window.size_distance", 
                                         "DBstar")])) %>%
    rename("value" = "DBstar") %>%
    mutate(cvix = "Modified Davies-Bouldin"),
  
  as.data.frame(
    (comparison$results$hierarchical[, c("distance", 
                                         "window.size_distance", 
                                         "DB")])) %>%
    rename("value" = "DB")%>%
    mutate(cvix = "Davies-Bouldin")) %>%
  group_by(cvix) %>%
  mutate(MinD = min(value),
         OptimalWs = window.size_distance[which.min(value)]) %>%
  ungroup()

#plot varying window sizes and minimum distance to centroid by cluster validation index
A <-
  hc_wsize %>%
  ggplot() +
  geom_line(aes(x = window.size_distance, y = value, color = cvix), size = 1) +
  geom_point(aes(x = OptimalWs, y = MinD, color = cvix), shape = 4, stroke = 1) +
  theme_bw(base_size = 16, base_family = 'Lato') +
  theme(legend.position = c(0.5, 0.7)) + 
  guides(color = guide_legend(title = "Cluster Validation Index")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "DTW window size", y = "Distance to computed centroid", title = "A")

#====================================================================

#create a list for hierarchical clustering
dtw_Dshc <- dplyr::bind_rows(DsTs, .id = "country") 
dtw_Dshc <- dtw_Dshc %>% spread(country, fitcases) %>% dplyr::select(everything(), -seqwk)
dtw_Dshc <- as.list(dtw_Dshc)

#hierarchical clustering with dynamic time-warping (DTW)
dtw_hc <- tsclust(dtw_Dshc,
                   type = "hierarchical",
                   k = 4L,
                   preproc = zscore,
                   distance = "dtw_basic",
                   control = hierarchical_control(method = "average"),
                   args = tsclust_args(dist = list(window.size = 25L))
)

#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:4L)))[["data"]]))
hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:4L)))[["data"]]))

#ggplot methods
B <-
  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 16, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  labs(title = "B", x = "Hierarchical clustering of countries' time series", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#====================================================================

C <-
  dplyr::rows_append(
    hc_members %>%
      dplyr::mutate(cat = "Cluster members") %>%
      dplyr::select(group, colour, x, y, PANEL, cat) %>%
      dplyr::rename("panex" = "PANEL"),
    
    hc_centroid %>% 
      dplyr::mutate(cat = "Prototypes", "group" = NA) %>%
      dplyr::select(group, colour, x, y, PANEL, cat) %>%
      dplyr::rename("panex" = "PANEL")) %>%
  
  ggplot(aes(x = x, y = y, color = colour)) +
  geom_line(size = 1) +
  facet_grid(cat ~ panex) +
  theme_bw(base_size = 16, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 16), strip.text.x.top = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "Weeks from 2021/22 to 2022/23 seasons", y = "z-normalised", title = "C")

#====================================================================

ggsave(here("output", "fig8_dtwClustering.png"),
       plot = (A | B | C | plot_layout(ncol = 3, width = c(1, 2, 2))),
       width = 20, height = 11, unit="in", dpi = 300)


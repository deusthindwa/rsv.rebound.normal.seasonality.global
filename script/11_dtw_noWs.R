#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================

#split the dataset by country to form list of datasets
X <-
  rbind(
    
    #set southern hemisphere countries to start from 1st week of 2020 to 52th week of 2022
    rsv_dtw %>%
      dplyr::filter(country %in% c("Argentina", "Australia", "Costa Rica", "Japan", "Paraguay", "Peru", "South Africa") & #Colombia, India
                      date >= date("2017-01-08") & 
                      date <= date("2022-12-31")), #"2022-10-07"
    
    #set northern hemisphere countries to start from 24th week of 2020 to 23rd week of 2023
    rsv_dtw %>%
      dplyr::filter(country %in% c("Brazil", "Canada", "Denmark", "France", "Germany", "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States") &
                      date >= date("2017-06-11") & #"2017-06-11"
                      date < date("2023-06-04"))) %>% #"2023-03-12"
  
  arrange(country, date) %>%
  group_by(country) %>%
  dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, seqwk, cases) %>%
  dplyr::add_row(country = rep("United States", 12), seqwk = 301:312, cases = rep(0, 12)) %>% #add zeros to US to align time series dimension
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
    dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()),
                  datex = seq.int(from = date("2020-04-08"), by = 7, length.out = n()))
}

#create a list for hierarchical clustering
Dshc <- dplyr::bind_rows(DsTs, .id = "country")
Dshc <- Dshc %>% spread(country, fitcases) %>% dplyr::select(everything(), -seqwk, -datex)
Dshc <- as.list(Dshc)

#====================================================================
#DYNAMIC TIME WARPING (DTW)
#====================================================================

#DTW cluster evaluation to determine window size
cfg <- dtwclust::compare_clusterings_configs(
  types = "hierarchical", 
  k = 2L:25L, 
  controls = list(hierarchical = hierarchical_control(method = "average")), 
  distances = pdc_configs("distance", hierarchical = list(dtw = list(norm = c("L1")))),
  centroids = pdc_configs("centroid", hierarchical = list(dba = list(norm = c("L1")))),
  preprocs = pdc_configs("preproc", hierarchical = list(zscore = list(norm = c("L1")))),
  no.expand = c("norm" ))

evaluators <- cvi_evaluators(c("DBstar"))
comparison <- compare_clusterings(Dshc, 
                                  types = "hierarchical", 
                                  configs = cfg, 
                                  seed = 8L, 
                                  score.clus = evaluators$score, 
                                  pick.clus = evaluators$pick)

#dataset for varying window size
hc_wsize <- 
  #bind_rows(
  as.data.frame(
    (comparison$results$hierarchical[, c("distance", 
                                         "DBstar",
                                         "k")])) %>%
  dplyr::rename("value" = "DBstar") %>%
  dplyr::mutate(cvix = "Modified Davies-Bouldin")

#plot varying window sizes and minimum distance to centroid by cluster validation index
A <-
  hc_wsize %>%
  ggplot() +
  geom_line(aes(x = k, y = value), size = 1) +
  geom_point(aes(x = k, y = value, color = "red"), size = 1.5, shape = 4, stroke = 2) +
  theme_bw(base_size = 16, base_family = 'Lato') +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(1, 25, 2), limits = c(1, 25)) +
  scale_y_continuous(breaks = seq(0.75, 4.25, 0.5), limits = c(0.75, 4.25)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(title = "(C)", x = "Number of clusters", y = "Distance to computed centroid (cluster variation index - CVI)") +
  theme(legend.position = "none")

#hierarchical clustering with dynamic time-warping (DTW)
dtw_hc <- dtwclust::tsclust(Dshc,
                            type = "hierarchical",
                            k = 3L,
                            preproc = zscore,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            trace = TRUE,
                            args = tsclust_args(cent = dba)
)

#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:4L)))[["data"]]))
hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:4L), lty=1))[["data"]]))

#plot ggdendrogram to show hierarchical clustering
labs <- label(dendro_data(as.dendrogram(dtw_hc)))
labs$Cluster <- c(rep("3", 2), rep("2", 2), rep("1", 22))

B <-
  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  labs(title = "(A)", x = "Time series hierarchical clustering", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_point(data = labs, aes(x = x, y = 0, colour = Cluster), size = 4) +
  theme(legend.position = c(0.9,0.8))

#plot prototypes
dtwclustDS <-
  dplyr::rows_append(
    hc_members %>%
      dplyr::mutate(cat = "Cluster members") %>%
      dplyr::select(group, colour, x, y, PANEL, cat) %>%
      dplyr::rename("Cluster" = "PANEL"),
    
    hc_centroid %>% 
      dplyr::mutate(cat = "Prototypes", "group" = NA) %>%
      dplyr::select(group, colour, x, y, PANEL, cat) %>%
      dplyr::rename("Cluster" = "PANEL")) %>%
  
  mutate(Cluster = if_else(Cluster == 1, " Cluster 1",
                           if_else(Cluster == 2, "Cluster 2",
                                   if_else(Cluster == 3, "Cluster 3", NA_character_))))

C <-
  dtwclustDS %>%
  dplyr::filter(cat == "Cluster members") %>%
  ggplot() +
  geom_line(aes(x = x, y = y, color = colour), size = 2) +
  facet_grid(cat ~ Cluster) +
  scale_colour_grey(start = 0.1, end = 0.8) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(axis.text.x = element_blank()) + 
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 18), strip.text.x = element_text(size = 18), strip.background = element_rect(fill="white")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "", y = "z-normalised", title = "(B)")

D <-
  dtwclustDS %>%
  dplyr::filter(cat == "Prototypes") %>%
  ggplot() +
  geom_line(aes(x = x, y = y, color = Cluster), size = 2) +
  facet_grid(cat ~ Cluster) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 18), strip.text.x = element_text(size = 0), strip.background = element_rect(fill="white")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "Weeks since RSV epidemics in 2017", y = "z-normalised", title = "")


ggsave(here("output", "fig4_dtwClustering_noWs.png"),
       plot = ((B / ((C/D) | A | plot_layout(ncol = 2, width = c(2,1))))),
       width = 18, height = 16, unit = "in", dpi = 300)

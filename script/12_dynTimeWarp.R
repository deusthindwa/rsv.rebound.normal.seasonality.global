#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================

#split the dataset by country to form list of datasets
X <-
  rbind(

    #set southern hemisphere countries to start from 1st week of 2020 to 52th week of 2022
      rsv_dtw %>%
      dplyr::filter(country %in% c("Argentina", "Australia", "Costa Rica", "India", "Japan", "Paraguay", "Peru", "South Africa") &
                      date >= date("2017-01-08") & #2020-01-08
                      date <= date("2022-12-31")),

    #set northern hemisphere countries to start from 24th week of 2020 to 23rd week of 2023
      rsv_dtw %>%
      dplyr::filter(country %in% c("Brazil", "Canada", "Denmark", "France", "Germany", "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States") &
                      date >= date("2017-01-08") & #2020-06-11
                      date < date("2023-06-04"))) %>%
  
  arrange(country, date) %>%
  group_by(country) %>%
  dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, seqwk, cases) %>%
 
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

#create a list for hierarchical clustering
Dshc <- dplyr::bind_rows(DsTs, .id = "country") 
Dshc <- Dshc %>% spread(country, fitcases) %>% dplyr::select(everything(), -seqwk)
Dshc <- as.list(Dshc)

#====================================================================
#DYNAMIC TIME WARPING (DTW)
#====================================================================

#DTW cluster evaluation to determine window size
cfg <- dtwclust::compare_clusterings_configs(
  types = "hierarchical", 
  k = 2L:14L, 
  controls = list(hierarchical = hierarchical_control(method = "average")), 
  distances = pdc_configs("distance", hierarchical = list(dtw = list(window.size = seq(from = 1L, to = 100L, by = 1L), norm = c("L1")))),
  centroids = pdc_configs("centroid", hierarchical = list(dba = list(window.size = seq(from = 1L, to = 100L, by = 1L), norm = c("L1")))),
  preprocs = pdc_configs("preproc", hierarchical = list(zscore = list(window.size = seq(from = 1L, to = 100L, by = 1L), norm = c("L1")))),
  no.expand = c("window.size", "norm" ))

evaluators <- cvi_evaluators(c("DBstar", "DB"))
comparison <- compare_clusterings(Dshc, 
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
                                         "DBstar",
                                         "k")])) %>%
    dplyr::rename("value" = "DBstar") %>%
    dplyr::mutate(cvix = "Modified Davies-Bouldin"),
  
  as.data.frame(
    (comparison$results$hierarchical[, c("distance", 
                                         "window.size_distance", 
                                         "DB",
                                         "k")])) %>%
    dplyr::rename("value" = "DB")%>%
    dplyr::mutate(cvix = "Davies-Bouldin")) %>%
  
  dplyr::filter(cvix == "Modified Davies-Bouldin") %>%
  tidyr::pivot_wider(names_from = k, values_from = value) %>%
  dplyr::rename("k2" = `2`, "k3" = `3`, "k4" = `4`) %>%
  dplyr::mutate(Min_k2 = min(k2),
                OptWs_k2 = window.size_distance[which.min(k2)],
                Min_k3 = min(k3),
                OptWs_k3 = window.size_distance[which.min(k3)],
                Min_k4 = min(k4),
                OptWs_k4 = window.size_distance[which.min(k4)])
  

#plot varying window sizes and minimum distance to centroid by cluster validation index
color <- c("k2" = "red", "k3" = "blue",  "k4" = "green")
A <-
  hc_wsize %>%
  ggplot() +
  geom_line(aes(x = window.size_distance, y = k2, color = "k2"), size = 1) +
  geom_point(aes(x = OptWs_k2, y = Min_k2, color = "k2"), shape = 4, stroke = 2) +
  geom_line(aes(x = window.size_distance, y = k3, color = "k3"), size = 1) +
  geom_point(aes(x = OptWs_k3, y = Min_k3, color = "k3"), shape = 4, stroke = 2) +
  geom_line(aes(x = window.size_distance, y = k4, color = "k4"), size = 1) +
  geom_point(aes(x = OptWs_k4, y = Min_k4, color = "k4"), shape = 4, stroke = 2) +
  theme_bw(base_size = 16, base_family = 'Lato') +
  #coord_cartesian(xlim = c(45, 100), ylim = c(0.5, 1.2)) + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(title = "Cluster size, k")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "Warping window size", y = "Distance to computed centroid", title = "")


#hierarchical clustering with dynamic time-warping (DTW)
dtw_hc <- dtwclust::tsclust(Dshc,
                            type = "hierarchical",
                            k = 4L,
                            preproc = zscore,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            args = tsclust_args(dist = list(window.size = 74L), cent = dba)
)

#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:4L)))[["data"]]))
hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:4L)))[["data"]]))

#plot ggdendrogram to show hierarchical clustering
labs <- label(dendro_data(as.dendrogram(dtw_hc)))
labs$Cluster <- c(rep("2", 3), rep("4", 3), rep("3", 13), rep("1", 8))

B <-
  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  labs(title = "(A)", x = "Time series hierarchical clustering", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_point(data = labs, aes(x = x, y = 0, colour = Cluster), size = 4) +
  theme(legend.position = c(0.9,0.8))


#plot prototypes
C <-
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
                                   if_else(Cluster == 3, "Cluster 3",
                                           if_else(Cluster == 4, "Cluster 4", NA_character_))))) %>%

  ggplot() +
  geom_line(aes(x = x, y = y, color = colour), size = 2) +
  facet_grid(cat ~ Cluster) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 18), strip.text.x = element_text(size = 18), strip.background = element_rect(fill="white")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "Weeks since RSV epidemics in 2017", y = "z-normalised", title = "(B)")


ggsave(here("output", "fig4_dtwClustering.png"),
       plot = ((B / C)),
       width = 18, height = 16, unit = "in", dpi = 300)


#====================================================================
#====================================================================
#====================================================================

#Sensitivity analysis

#hierarchical clustering with dynamic time-warping (DTW)
dtw_hc <- dtwclust::tsclust(Dshc,
                            type = "hierarchical",
                            k = 2L,
                            preproc = zscore,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            args = tsclust_args(dist = list(window.size = 66L), cent = dba)
)

#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:3L)))[["data"]]))
hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:3L)))[["data"]]))


#plot ggdendrogram to show hierarchical clustering
labs <- label(dendro_data(as.dendrogram(dtw_hc)))
labs$Cluster <- c(rep("1", 24), rep("2", 3))

S1 <-
  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  labs(title = "(A)", x = "Time series hierarchical clustering", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_point(data = labs, aes(x = x, y = 0, colour = Cluster), size = 4) +
  theme(legend.position = c(0.9,0.9))

#plot prototypes
S2 <-
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
                           if_else(Cluster == 2, "Cluster 2", NA_character_))) %>%
  
  ggplot() +
  geom_line(aes(x = x, y = y, color = colour), size = 2) +
  facet_grid(cat ~ Cluster) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 18), strip.text.x = element_text(size = 18), strip.background = element_rect(fill="white")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "Weeks since COVID-19 pandemic onset", y = "z-normalised", title = "(B)")


#hierarchical clustering with dynamic time-warping (DTW)
dtw_hc <- dtwclust::tsclust(Dshc,
                            type = "hierarchical",
                            k = 3L,
                            preproc = zscore,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            args = tsclust_args(dist = list(window.size = 65L), cent = dba)
)

#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:4L)))[["data"]]))
hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:4L)))[["data"]]))

#plot ggdendrogram to show hierarchical clustering
labs <- label(dendro_data(as.dendrogram(dtw_hc)))
labs$Cluster <- c(rep("3", 3), rep("2", 14), rep("1", 10))

S3 <-
  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  labs(title = "(C)", x = "Time series hierarchical clustering", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_point(data = labs, aes(x = x, y = 0, colour = Cluster), size = 4) +
  theme(legend.position = c(0.9,0.88))

#plot prototypes
S4 <-
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
                                   if_else(Cluster == 3, "Cluster 3", NA_character_)))) %>%
  
  ggplot() +
  geom_line(aes(x = x, y = y, color = colour), size = 2) +
  facet_grid(cat ~ Cluster) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 18), strip.text.x = element_text(size = 18), strip.background = element_rect(fill="white")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "Weeks since COVID-19 pandemic onset", y = "z-normalised", title = "(D)")

ggsave(here("output", "sfig8_dtwwscs.png"), 
       plot = A,
       width = 12, height = 7, unit="in", dpi = 300)

ggsave(here("output", "sfig9_dtwClustering.png"), 
       plot = (S1/S2) | (S3/S4),
       width = 25, height = 17, unit="in", dpi = 300)


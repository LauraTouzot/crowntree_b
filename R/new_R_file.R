identify_controlling_dimensions_v1 <- function() {
  
  ## loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(species, mean_hmax, sd_hmax, se_hmax)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_beta_mean_estimates.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(species, mean_a1, sd_a1) %>% rename(mean_a1_ratio = mean_a1, sd_a1_ratio = sd_a1)
  
  height_dbh <- read.csv(file = "output/height_dbh_summary.csv")
  height_dbh <- height_dbh %>% dplyr::select(-X)
  
  ## computing crown volume 
  # identifying species for which all variables are available
  data <- left_join(diameter_dbh, ratio_dbh, by = "species")
  data <- left_join(data, height_max, by = "species")
  data <- left_join(data, height_dbh, by = "species")
  data <- na.omit(data)
  sp_data <- data$species
  
  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  gp_info <- gp_info[gp_info$sp %in% sp_data,]
  
  ## details about functional groups 
  # A deciduous angiosperm
  # B deciduous gymnosperm
  # C evergreen angiosperm
  # D evergreen gymnosperm

  data_gp <- left_join(gp_info, data, by = c("sp" = "species"))
  
  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_a1_ratio)/2),
                                     volume_30 = 4/3 * pi * (mean_diameter_30/2)*(mean_diameter_30/2) * ((mean_h_30*mean_a1_ratio)/2))
  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_a1_ratio),
                                     volume_30 = 1/2 * pi * (mean_diameter_30/2)*(mean_diameter_30/2) * (mean_h_30*mean_a1_ratio))
  
  
  pca_angio_15 <- angio %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15)
  res_pca_angio_15 <- prcomp(pca_angio_15, scale = TRUE, center = TRUE)
  
  pca_angio_30 <- angio %>% dplyr::select(mean_diameter_30, mean_a1_ratio, mean_hmax, volume_30)
  res_pca_angio_30 <- prcomp(pca_angio_30, scale = TRUE, center = TRUE)
  

  fviz_pca_var(res_pca_angio_15,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 
  
  fviz_pca_var(res_pca_angio_30,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "all") 
  
  
  pca_gymno_15 <- gymno %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15)
  res_pca_gymno_15 <- prcomp(pca_gymno_15, scale = TRUE, center = TRUE)
  
  pca_gymno_30 <- gymno %>% dplyr::select(mean_diameter_30, mean_a1_ratio, mean_hmax, volume_30)
  res_pca_gymno_30 <- prcomp(pca_gymno_30, scale = TRUE, center = TRUE)
  
  
  fviz_pca_var(res_pca_gymno_15,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 
  
  fviz_pca_var(res_pca_gymno_30,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "all") 
  

  all_sp <- rbind(angio, gymno) 
  all_sp <- all_sp %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15)
  res_pca <- prcomp(all_sp, scale = TRUE, center = TRUE)
  
  fviz_pca_var(res_pca,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 
  
}




identify_controlling_dimensions_v2 <- function() {
  
  ## loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(species, mean_hmax, sd_hmax, se_hmax)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_beta_mean_estimates.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(species, mean_a1, sd_a1) %>% 
                             dplyr::rename(mean_a1_ratio = mean_a1, sd_a1_ratio = sd_a1)
  
  height_dbh <- read.csv(file = "output/height_dbh_summary.csv")
  height_dbh <- height_dbh %>% dplyr::select(species, mean_h_15, mean_h_30)
  
  diameter_c1 <- read.csv(file = "output/diameter_dbh_c1_summary.csv")
  diameter_c1 <- diameter_c1 %>% dplyr::select(-X) 
  
  ratio_c1 <- read.csv(file = "output/ratio_dbh_mean_c1_summary.csv")
  ratio_c1 <- ratio_c1 %>% dplyr::select(-X) 
               
  ## computing crown volume 
  # identifying species for which all variables are available
  data <- list(height_max, diameter_dbh, ratio_dbh, height_dbh,
               diameter_c1, ratio_c1)
    
  data_file <- join_all(data, by = "species", type = "left")
  data_file <- na.omit(data_file)
  sp_data <- data_file$species
  
  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  gp_info <- gp_info[gp_info$sp %in% sp_data,]
  
  ## details about functional groups 
  # A deciduous angiosperm
  # B deciduous gymnosperm
  # C evergreen angiosperm
  # D evergreen gymnosperm
  
  data_gp <- left_join(gp_info, data_file, by = c("sp" = "species"))

  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15_0 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_a1_ratio)/2), 
                                     volume_15_5 = 4/3 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * ((mean_h_15*mean_ratio_5)/2),
                                     volume_15_20 = 4/3 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * ((mean_h_15*mean_ratio_20)/2)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)
  
  angio <- angio[angio$sp != "Alnus rubra",]

  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15_0 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_a1_ratio),
                                     volume_15_5 = 1/2 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * (mean_h_15*mean_ratio_5),
                                     volume_15_20 = 1/2 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * (mean_h_15*mean_ratio_20)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)
  
  # computing competition responses
  
  pca_angio_15 <- angio %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15_0, 
                                          diam_response, ratio_response, volume_response)
  res_pca_angio_15 <- prcomp(pca_angio_15, scale = TRUE, center = TRUE)

  fviz_pca_var(res_pca_angio_15,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 

  
  pca_gymno_15 <- gymno %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15_0,
                                          diam_response, ratio_response, volume_response)
  res_pca_gymno_15 <- prcomp(pca_gymno_15, scale = TRUE, center = TRUE)

  fviz_pca_var(res_pca_gymno_15,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 


  all_sp <- rbind(angio, gymno) 
  all_sp <- all_sp %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15_0,
                                     diam_response, ratio_response, volume_response)
  res_pca <- prcomp(all_sp, scale = TRUE, center = TRUE)
  
  fviz_pca_var(res_pca,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 
  
}



identify_controlling_dimensions_v3 <- function() {
  
  ## loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(species, mean_hmax, sd_hmax, se_hmax)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_beta_mean_estimates.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(species, mean_a1, sd_a1) %>% 
                             dplyr::rename(mean_a1_ratio = mean_a1, sd_a1_ratio = sd_a1)
  
  height_dbh <- read.csv(file = "output/height_dbh_summary.csv")
  height_dbh <- height_dbh %>% dplyr::select(species, mean_h_15, mean_h_30)
  
  diameter_c2 <- read.csv(file = "output/diameter_dbh_c2_summary.csv")
  diameter_c2 <- diameter_c2 %>% dplyr::select(-X) 
  
  ratio_c2 <- read.csv(file = "output/ratio_dbh_mean_c2_summary.csv")
  ratio_c2 <- ratio_c2 %>% dplyr::select(-X) 
  
  ## computing crown volume 
  # identifying species for which all variables are available
  data <- list(height_max, diameter_dbh, ratio_dbh, height_dbh,
               diameter_c2, ratio_c2)
  
  data_file <- join_all(data, by = "species", type = "left")
  data_file <- na.omit(data_file)
  sp_data <- data_file$species
  
  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  gp_info <- gp_info[gp_info$sp %in% sp_data,]
  
  ## details about functional groups 
  # A deciduous angiosperm
  # B deciduous gymnosperm
  # C evergreen angiosperm
  # D evergreen gymnosperm
  
  data_gp <- left_join(gp_info, data_file, by = c("sp" = "species"))
  
  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15_0 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_a1_ratio)/2), 
                                     volume_15_5 = 4/3 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * ((mean_h_15*mean_ratio_5)/2),
                                     volume_15_20 = 4/3 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * ((mean_h_15*mean_ratio_20)/2)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)

  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15_0 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_a1_ratio),
                                     volume_15_5 = 1/2 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * (mean_h_15*mean_ratio_5),
                                     volume_15_20 = 1/2 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * (mean_h_15*mean_ratio_20)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)
  
  # computing competition responses
  
  pca_angio_15 <- angio %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15_0, 
                                          diam_response, ratio_response, volume_response)
  res_pca_angio_15 <- prcomp(pca_angio_15, scale = TRUE, center = TRUE)
  
  fviz_pca_var(res_pca_angio_15,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 
  
  
  pca_gymno_15 <- gymno %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15_0,
                                          diam_response, ratio_response, volume_response)
  res_pca_gymno_15 <- prcomp(pca_gymno_15, scale = TRUE, center = TRUE)
  
  fviz_pca_var(res_pca_gymno_15,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 
  
  
  all_sp <- rbind(angio, gymno) 
  all_sp <- all_sp %>% dplyr::select(mean_diameter_15, mean_a1_ratio, mean_hmax, volume_15_0,
                                     diam_response, ratio_response, volume_response)
  res_pca <- prcomp(all_sp, scale = TRUE, center = TRUE)
  
  fviz_pca_var(res_pca,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "none") 
  
}





crown_traits_v1 <- function() {
  
  ## loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(species, mean_hmax, sd_hmax, se_hmax)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_beta_mean_estimates.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(species, mean_a1, sd_a1) %>% 
                             dplyr::rename(mean_a1_ratio = mean_a1, sd_a1_ratio = sd_a1)
  
  height_dbh <- read.csv(file = "output/height_dbh_summary.csv")
  height_dbh <- height_dbh %>% dplyr::select(species, mean_h_15, mean_h_30)
  
  diameter_c1 <- read.csv(file = "output/diameter_dbh_c1_summary.csv")
  diameter_c1 <- diameter_c1 %>% dplyr::select(-X) 
  
  ratio_c1 <- read.csv(file = "output/ratio_dbh_mean_c1_summary.csv")
  ratio_c1 <- ratio_c1 %>% dplyr::select(-X) 
  
  
  ## computing crown volume 
  # identifying species for which all variables are available
  data <- list(height_max, diameter_dbh, ratio_dbh, height_dbh,
               diameter_c1, ratio_c1)
  
  data_file <- join_all(data, by = "species", type = "left")
  sp_data <- data_file$species
  
  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  gp_info <- gp_info[gp_info$sp %in% sp_data,]
  
  ## details about functional groups 
  # A deciduous angiosperm
  # B deciduous gymnosperm
  # C evergreen angiosperm
  # D evergreen gymnosperm
  
  data_gp <- left_join(gp_info, data_file, by = c("sp" = "species"))
  
  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15_0 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_a1_ratio)/2), 
                                     volume_15_5 = 4/3 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * ((mean_h_15*mean_ratio_5)/2),
                                     volume_15_20 = 4/3 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * ((mean_h_15*mean_ratio_20)/2)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)
  
  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15_0 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_a1_ratio),
                                     volume_15_5 = 1/2 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * (mean_h_15*mean_ratio_5),
                                     volume_15_20 = 1/2 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * (mean_h_15*mean_ratio_20)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)
  
  crown <- rbind(angio, gymno)
  
  # adding sp traits
  traits <- read.csv(file = "output/sp_traits.csv")
  traits <- traits %>% dplyr::select(-X)
  
  data <- left_join(crown, traits, by = c("sp" = "species"))
  data <- data %>% dplyr::mutate(p_etp = map - etp)
  
  angio <- data[data$new_gp == "angio",]
  gymno <- data[data$new_gp == "gymno",]
  

  h_cd_angio <- cor.test(angio$mean_hmax, angio$diam_response, method = "pearson", na.rm = TRUE)
  h_cd_gymno <- cor.test(gymno$mean_hmax, gymno$diam_response, method = "pearson", na.rm = TRUE)
  
  h_cr_angio <- cor.test(angio$mean_hmax, angio$ratio_response, method = "pearson", na.rm = TRUE)
  h_cr_gymno <- cor.test(gymno$mean_hmax, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  h_cv_angio <- cor.test(angio$mean_hmax, angio$volume_response, method = "pearson", na.rm = TRUE)
  h_cv_gymno <- cor.test(gymno$mean_hmax, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  h_mat_angio <- cor.test(angio$mean_hmax, angio$mat, method = "pearson", na.rm = TRUE)
  h_mat_gymno <- cor.test(gymno$mean_hmax, gymno$mat, method = "pearson", na.rm = TRUE)
  
  h_tmin_angio <- cor.test(angio$mean_hmax, angio$tmin, method = "pearson", na.rm = TRUE)
  h_tmin_gymno <- cor.test(gymno$mean_hmax, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  h_map_angio <- cor.test(angio$mean_hmax, angio$map, method = "pearson", na.rm = TRUE)
  h_map_gymno <- cor.test(gymno$mean_hmax, gymno$map, method = "pearson", na.rm = TRUE)
  
  h_map_angio <- cor.test(angio$mean_hmax, angio$map, method = "pearson", na.rm = TRUE)
  h_map_gymno <- cor.test(gymno$mean_hmax, gymno$map, method = "pearson", na.rm = TRUE)
  
  h_metp_angio <- cor.test(angio$mean_hmax, angio$p_etp, method = "pearson", na.rm = TRUE)
  h_metp_gymno <- cor.test(gymno$mean_hmax, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  h_sla_angio <- cor.test(angio$mean_hmax, angio$sla, method = "pearson", na.rm = TRUE)
  h_sla_gymno <- cor.test(gymno$mean_hmax, gymno$sla, method = "pearson", na.rm = TRUE)
  
  h_wd_angio <- cor.test(angio$mean_hmax, angio$ssd, method = "pearson", na.rm = TRUE)
  h_wd_gymno <- cor.test(gymno$mean_hmax, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  h_sh_angio <- cor.test(angio$mean_hmax, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  h_sh_gymno <- cor.test(gymno$mean_hmax, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  
  r_cd_angio <- cor.test(angio$mean_a1_ratio, angio$diam_response, method = "pearson", na.rm = TRUE)
  r_cd_gymno <- cor.test(gymno$mean_a1_ratio, gymno$diam_response, method = "pearson", na.rm = TRUE)

  r_cr_angio <- cor.test(angio$mean_a1_ratio, angio$ratio_response, method = "pearson", na.rm = TRUE)
  r_cr_gymno <- cor.test(gymno$mean_a1_ratio, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  r_cv_angio <- cor.test(angio$mean_a1_ratio, angio$volume_response, method = "pearson", na.rm = TRUE)
  r_cv_gymno <- cor.test(gymno$mean_a1_ratio, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  r_mat_angio <- cor.test(angio$mean_a1_ratio, angio$mat, method = "pearson", na.rm = TRUE)
  r_mat_gymno <- cor.test(gymno$mean_a1_ratio, gymno$mat, method = "pearson", na.rm = TRUE)
  
  r_tmin_angio <- cor.test(angio$mean_a1_ratio, angio$tmin, method = "pearson", na.rm = TRUE)
  r_tmin_gymno <- cor.test(gymno$mean_a1_ratio, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  r_map_angio <- cor.test(angio$mean_a1_ratio, angio$map, method = "pearson", na.rm = TRUE)
  r_map_gymno <- cor.test(gymno$mean_a1_ratio, gymno$map, method = "pearson", na.rm = TRUE)
  
  r_metp_angio <- cor.test(angio$mean_a1_ratio, angio$p_etp, method = "pearson", na.rm = TRUE)
  r_metp_gymno <- cor.test(gymno$mean_a1_ratio, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  r_sla_angio <- cor.test(angio$mean_a1_ratio, angio$sla, method = "pearson", na.rm = TRUE)
  r_sla_gymno <- cor.test(gymno$mean_a1_ratio, gymno$sla, method = "pearson", na.rm = TRUE)
  
  r_wd_angio <- cor.test(angio$mean_a1_ratio, angio$ssd, method = "pearson", na.rm = TRUE)
  r_wd_gymno <- cor.test(gymno$mean_a1_ratio, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  r_sh_angio <- cor.test(angio$mean_a1_ratio, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  r_sh_gymno <- cor.test(gymno$mean_a1_ratio, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  d_cd_angio <- cor.test(angio$mean_diameter_15, angio$diam_response, method = "pearson", na.rm = TRUE)
  d_cd_gymno <- cor.test(gymno$mean_diameter_15, gymno$diam_response, method = "pearson", na.rm = TRUE)
  
  d_cr_angio <- cor.test(angio$mean_diameter_15, angio$ratio_response, method = "pearson", na.rm = TRUE)
  d_cr_gymno <- cor.test(gymno$mean_diameter_15, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  d_cv_angio <- cor.test(angio$mean_diameter_15, angio$volume_response, method = "pearson", na.rm = TRUE)
  d_cv_gymno <- cor.test(gymno$mean_diameter_15, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  d_mat_angio <- cor.test(angio$mean_diameter_15, angio$mat, method = "pearson", na.rm = TRUE)
  d_mat_gymno <- cor.test(gymno$mean_diameter_15, gymno$mat, method = "pearson", na.rm = TRUE)
  
  d_tmin_angio <- cor.test(angio$mean_diameter_15, angio$tmin, method = "pearson", na.rm = TRUE)
  d_tmin_gymno <- cor.test(gymno$mean_diameter_15, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  d_map_angio <- cor.test(angio$mean_diameter_15, angio$map, method = "pearson", na.rm = TRUE)
  d_map_gymno <- cor.test(gymno$mean_diameter_15, gymno$map, method = "pearson", na.rm = TRUE)
  
  d_metp_angio <- cor.test(angio$mean_diameter_15, angio$p_etp, method = "pearson", na.rm = TRUE)
  d_metp_gymno <- cor.test(gymno$mean_diameter_15, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  d_sla_angio <- cor.test(angio$mean_diameter_15, angio$sla, method = "pearson", na.rm = TRUE)
  d_sla_gymno <- cor.test(gymno$mean_diameter_15, gymno$sla, method = "pearson", na.rm = TRUE)
  
  d_wd_angio <- cor.test(angio$mean_diameter_15, angio$ssd, method = "pearson", na.rm = TRUE)
  d_wd_gymno <- cor.test(gymno$mean_diameter_15, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  d_sh_angio <- cor.test(angio$mean_diameter_15, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  d_sh_gymno <- cor.test(gymno$mean_diameter_15, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  
  v_cd_angio <- cor.test(angio$volume_15_0, angio$diam_response, method = "pearson", na.rm = TRUE)
  v_cd_gymno <- cor.test(gymno$volume_15_0, gymno$diam_response, method = "pearson", na.rm = TRUE)
  
  v_cr_angio <- cor.test(angio$volume_15_0, angio$ratio_response, method = "pearson", na.rm = TRUE)
  v_cr_gymno <- cor.test(gymno$volume_15_0, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  v_cv_angio <- cor.test(angio$volume_15_0, angio$volume_response, method = "pearson", na.rm = TRUE)
  v_cv_gymno <- cor.test(gymno$volume_15_0, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  v_mat_angio <- cor.test(angio$volume_15_0, angio$mat, method = "pearson", na.rm = TRUE)
  v_mat_gymno <- cor.test(gymno$volume_15_0, gymno$mat, method = "pearson", na.rm = TRUE)
  
  v_tmin_angio <- cor.test(angio$volume_15_0, angio$tmin, method = "pearson", na.rm = TRUE)
  v_tmin_gymno <- cor.test(gymno$volume_15_0, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  v_map_angio <- cor.test(angio$volume_15_0, angio$map, method = "pearson", na.rm = TRUE)
  v_map_gymno <- cor.test(gymno$volume_15_0, gymno$map, method = "pearson", na.rm = TRUE)
  
  v_metp_angio <- cor.test(angio$volume_15_0, angio$p_etp, method = "pearson", na.rm = TRUE)
  v_metp_gymno <- cor.test(gymno$volume_15_0, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  v_sla_angio <- cor.test(angio$volume_15_0, angio$sla, method = "pearson", na.rm = TRUE)
  v_sla_gymno <- cor.test(gymno$volume_15_0, gymno$sla, method = "pearson", na.rm = TRUE)
  
  v_wd_angio <- cor.test(angio$volume_15_0, angio$ssd, method = "pearson", na.rm = TRUE)
  v_wd_gymno <- cor.test(gymno$volume_15_0, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  v_sh_angio <- cor.test(angio$volume_15_0, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  v_sh_gymno <- cor.test(gymno$volume_15_0, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  
  
  cor_angio <- matrix(nrow = 4, ncol = 10)
  cor_gymno <- matrix(nrow = 4, ncol = 10)
  
  p_angio <- matrix(nrow = 4, ncol = 10)
  p_gymno <- matrix(nrow = 4, ncol = 10)
  
  cor_angio[1,1] <- h_cd_angio$estimate
  cor_angio[1,2] <- h_cr_angio$estimate
  cor_angio[1,3] <- h_cv_angio$estimate
  cor_angio[1,4] <- h_mat_angio$estimate
  cor_angio[1,5] <- h_tmin_angio$estimate
  cor_angio[1,6] <- h_map_angio$estimate
  cor_angio[1,7] <- h_metp_angio$estimate
  cor_angio[1,8] <- h_sla_angio$estimate
  cor_angio[1,9] <- h_wd_angio$estimate
  cor_angio[1,10] <- h_sh_angio$estimate
  
  cor_angio[2,1] <- r_cd_angio$estimate
  cor_angio[2,2] <- r_cr_angio$estimate
  cor_angio[2,3] <- r_cv_angio$estimate
  cor_angio[2,4] <- r_mat_angio$estimate
  cor_angio[2,5] <- r_tmin_angio$estimate
  cor_angio[2,6] <- r_map_angio$estimate
  cor_angio[2,7] <- r_metp_angio$estimate
  cor_angio[2,8] <- r_sla_angio$estimate
  cor_angio[2,9] <- r_wd_angio$estimate
  cor_angio[2,10] <- r_sh_angio$estimate
  
  cor_angio[3,1] <- d_cd_angio$estimate
  cor_angio[3,2] <- d_cr_angio$estimate
  cor_angio[3,3] <- d_cv_angio$estimate
  cor_angio[3,4] <- d_mat_angio$estimate
  cor_angio[3,5] <- d_tmin_angio$estimate
  cor_angio[3,6] <- d_map_angio$estimate
  cor_angio[3,7] <- d_metp_angio$estimate
  cor_angio[3,8] <- d_sla_angio$estimate
  cor_angio[3,9] <- d_wd_angio$estimate
  cor_angio[3,10] <- d_sh_angio$estimate
  
  cor_angio[4,1] <- v_cd_angio$estimate
  cor_angio[4,2] <- v_cr_angio$estimate
  cor_angio[4,3] <- v_cv_angio$estimate
  cor_angio[4,4] <- v_mat_angio$estimate
  cor_angio[4,5] <- v_tmin_angio$estimate
  cor_angio[4,6] <- v_map_angio$estimate
  cor_angio[4,7] <- v_metp_angio$estimate
  cor_angio[4,8] <- v_sla_angio$estimate
  cor_angio[4,9] <- v_wd_angio$estimate
  cor_angio[4,10] <- v_sh_angio$estimate
  
  
  p_angio[1,1] <- h_cd_angio$p.value
  p_angio[1,2] <- h_cr_angio$p.value
  p_angio[1,3] <- h_cv_angio$p.value
  p_angio[1,4] <- h_mat_angio$p.value
  p_angio[1,5] <- h_tmin_angio$p.value
  p_angio[1,6] <- h_map_angio$p.value
  p_angio[1,7] <- h_metp_angio$p.value
  p_angio[1,8] <- h_sla_angio$p.value
  p_angio[1,9] <- h_wd_angio$p.value
  p_angio[1,10] <- h_sh_angio$p.value
  
  p_angio[2,1] <- r_cd_angio$p.value
  p_angio[2,2] <- r_cr_angio$p.value
  p_angio[2,3] <- r_cv_angio$p.value
  p_angio[2,4] <- r_mat_angio$p.value
  p_angio[2,5] <- r_tmin_angio$p.value
  p_angio[2,6] <- r_map_angio$p.value
  p_angio[2,7] <- r_metp_angio$p.value
  p_angio[2,8] <- r_sla_angio$p.value
  p_angio[2,9] <- r_wd_angio$p.value
  p_angio[2,10] <- r_sh_angio$p.value
  
  p_angio[3,1] <- d_cd_angio$p.value
  p_angio[3,2] <- d_cr_angio$p.value
  p_angio[3,3] <- d_cv_angio$p.value
  p_angio[3,4] <- d_mat_angio$p.value
  p_angio[3,5] <- d_tmin_angio$p.value
  p_angio[3,6] <- d_map_angio$p.value
  p_angio[3,7] <- d_metp_angio$p.value
  p_angio[3,8] <- d_sla_angio$p.value
  p_angio[3,9] <- d_wd_angio$p.value
  p_angio[3,10] <- d_sh_angio$p.value
  
  p_angio[4,1] <- v_cd_angio$p.value
  p_angio[4,2] <- v_cr_angio$p.value
  p_angio[4,3] <- v_cv_angio$p.value
  p_angio[4,4] <- v_mat_angio$p.value
  p_angio[4,5] <- v_tmin_angio$p.value
  p_angio[4,6] <- v_map_angio$p.value
  p_angio[4,7] <- v_metp_angio$p.value
  p_angio[4,8] <- v_sla_angio$p.value
  p_angio[4,9] <- v_wd_angio$p.value
  p_angio[4,10] <- v_sh_angio$p.value  
  
  
  cor_gymno[1,1] <- h_cd_gymno$estimate
  cor_gymno[1,2] <- h_cr_gymno$estimate
  cor_gymno[1,3] <- h_cv_gymno$estimate
  cor_gymno[1,4] <- h_mat_gymno$estimate
  cor_gymno[1,5] <- h_tmin_gymno$estimate
  cor_gymno[1,6] <- h_map_gymno$estimate
  cor_gymno[1,7] <- h_metp_gymno$estimate
  cor_gymno[1,8] <- h_sla_gymno$estimate
  cor_gymno[1,9] <- h_wd_gymno$estimate
  cor_gymno[1,10] <- h_sh_gymno$estimate
  
  cor_gymno[2,1] <- r_cd_gymno$estimate
  cor_gymno[2,2] <- r_cr_gymno$estimate
  cor_gymno[2,3] <- r_cv_gymno$estimate
  cor_gymno[2,4] <- r_mat_gymno$estimate
  cor_gymno[2,5] <- r_tmin_gymno$estimate
  cor_gymno[2,6] <- r_map_gymno$estimate
  cor_gymno[2,7] <- r_metp_gymno$estimate
  cor_gymno[2,8] <- r_sla_gymno$estimate
  cor_gymno[2,9] <- r_wd_gymno$estimate
  cor_gymno[2,10] <- r_sh_gymno$estimate
  
  cor_gymno[3,1] <- d_cd_gymno$estimate
  cor_gymno[3,2] <- d_cr_gymno$estimate
  cor_gymno[3,3] <- d_cv_gymno$estimate
  cor_gymno[3,4] <- d_mat_gymno$estimate
  cor_gymno[3,5] <- d_tmin_gymno$estimate
  cor_gymno[3,6] <- d_map_gymno$estimate
  cor_gymno[3,7] <- d_metp_gymno$estimate
  cor_gymno[3,8] <- d_sla_gymno$estimate
  cor_gymno[3,9] <- d_wd_gymno$estimate
  cor_gymno[3,10] <- d_sh_gymno$estimate
  
  cor_gymno[4,1] <- v_cd_gymno$estimate
  cor_gymno[4,2] <- v_cr_gymno$estimate
  cor_gymno[4,3] <- v_cv_gymno$estimate
  cor_gymno[4,4] <- v_mat_gymno$estimate
  cor_gymno[4,5] <- v_tmin_gymno$estimate
  cor_gymno[4,6] <- v_map_gymno$estimate
  cor_gymno[4,7] <- v_metp_gymno$estimate
  cor_gymno[4,8] <- v_sla_gymno$estimate
  cor_gymno[4,9] <- v_wd_gymno$estimate
  cor_gymno[4,10] <- v_sh_gymno$estimate
  
  
  p_gymno[1,1] <- h_cd_gymno$p.value
  p_gymno[1,2] <- h_cr_gymno$p.value
  p_gymno[1,3] <- h_cv_gymno$p.value
  p_gymno[1,4] <- h_mat_gymno$p.value
  p_gymno[1,5] <- h_tmin_gymno$p.value
  p_gymno[1,6] <- h_map_gymno$p.value
  p_gymno[1,7] <- h_metp_gymno$p.value
  p_gymno[1,8] <- h_sla_gymno$p.value
  p_gymno[1,9] <- h_wd_gymno$p.value
  p_gymno[1,10] <- h_sh_gymno$p.value
  
  p_gymno[2,1] <- r_cd_gymno$p.value
  p_gymno[2,2] <- r_cr_gymno$p.value
  p_gymno[2,3] <- r_cv_gymno$p.value
  p_gymno[2,4] <- r_mat_gymno$p.value
  p_gymno[2,5] <- r_tmin_gymno$p.value
  p_gymno[2,6] <- r_map_gymno$p.value
  p_gymno[2,7] <- r_metp_gymno$p.value
  p_gymno[2,8] <- r_sla_gymno$p.value
  p_gymno[2,9] <- r_wd_gymno$p.value
  p_gymno[2,10] <- r_sh_gymno$p.value
  
  p_gymno[3,1] <- d_cd_gymno$p.value
  p_gymno[3,2] <- d_cr_gymno$p.value
  p_gymno[3,3] <- d_cv_gymno$p.value
  p_gymno[3,4] <- d_mat_gymno$p.value
  p_gymno[3,5] <- d_tmin_gymno$p.value
  p_gymno[3,6] <- d_map_gymno$p.value
  p_gymno[3,7] <- d_metp_gymno$p.value
  p_gymno[3,8] <- d_sla_gymno$p.value
  p_gymno[3,9] <- d_wd_gymno$p.value
  p_gymno[3,10] <- d_sh_gymno$p.value
  
  p_gymno[4,1] <- v_cd_gymno$p.value
  p_gymno[4,2] <- v_cr_gymno$p.value
  p_gymno[4,3] <- v_cv_gymno$p.value
  p_gymno[4,4] <- v_mat_gymno$p.value
  p_gymno[4,5] <- v_tmin_gymno$p.value
  p_gymno[4,6] <- v_map_gymno$p.value
  p_gymno[4,7] <- v_metp_gymno$p.value
  p_gymno[4,8] <- v_sla_gymno$p.value
  p_gymno[4,9] <- v_wd_gymno$p.value
  p_gymno[4,10] <- v_sh_gymno$p.value  
  
  par(mfrow = c(1,1))
  corrplot(cor_angio, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Oranges"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  corrplot(cor_gymno, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Greens"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  
}





crown_traits_v2 <- function() {
  
  ## loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(species, mean_hmax, sd_hmax, se_hmax)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_beta_mean_estimates.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(species, mean_a1, sd_a1) %>% 
                             dplyr::rename(mean_a1_ratio = mean_a1, sd_a1_ratio = sd_a1)
  
  height_dbh <- read.csv(file = "output/height_dbh_summary.csv")
  height_dbh <- height_dbh %>% dplyr::select(species, mean_h_15, mean_h_30)
  
  diameter_c2 <- read.csv(file = "output/diameter_dbh_c2_summary.csv")
  diameter_c2 <- diameter_c2 %>% dplyr::select(-X) 
  
  ratio_c2 <- read.csv(file = "output/ratio_dbh_mean_c2_summary.csv")
  ratio_c2 <- ratio_c2 %>% dplyr::select(-X) 
  
  
  ## computing crown volume 
  # identifying species for which all variables are available
  data <- list(height_max, diameter_dbh, ratio_dbh, height_dbh,
               diameter_c2, ratio_c2)
  
  data_file <- join_all(data, by = "species", type = "left")
  sp_data <- data_file$species
  
  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  gp_info <- gp_info[gp_info$sp %in% sp_data,]
  
  ## details about functional groups 
  # A deciduous angiosperm
  # B deciduous gymnosperm
  # C evergreen angiosperm
  # D evergreen gymnosperm
  
  data_gp <- left_join(gp_info, data_file, by = c("sp" = "species"))
  
  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15_0 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_a1_ratio)/2), 
                                     volume_15_5 = 4/3 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * ((mean_h_15*mean_ratio_5)/2),
                                     volume_15_20 = 4/3 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * ((mean_h_15*mean_ratio_20)/2)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)
  
  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15_0 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_a1_ratio),
                                     volume_15_5 = 1/2 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * (mean_h_15*mean_ratio_5),
                                     volume_15_20 = 1/2 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * (mean_h_15*mean_ratio_20)) %>%
                       dplyr::mutate(diam_response = mean_diameter_15_20 - mean_diameter_15_5,
                                     ratio_response = mean_ratio_20 - mean_ratio_5,
                                     volume_response = volume_15_20 - volume_15_5)
  
  crown <- rbind(angio, gymno)
  
  # adding sp traits
  traits <- read.csv(file = "output/sp_traits.csv")
  traits <- traits %>% dplyr::select(-X)
  
  data <- left_join(crown, traits, by = c("sp" = "species"))
  data <- data %>% dplyr::mutate(p_etp = map - etp)
  
  angio <- data[data$new_gp == "angio",]
  gymno <- data[data$new_gp == "gymno",]
  
  
  h_cd_angio <- cor.test(angio$mean_hmax, angio$diam_response, method = "pearson", na.rm = TRUE)
  h_cd_gymno <- cor.test(gymno$mean_hmax, gymno$diam_response, method = "pearson", na.rm = TRUE)
  
  h_cr_angio <- cor.test(angio$mean_hmax, angio$ratio_response, method = "pearson", na.rm = TRUE)
  h_cr_gymno <- cor.test(gymno$mean_hmax, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  h_cv_angio <- cor.test(angio$mean_hmax, angio$volume_response, method = "pearson", na.rm = TRUE)
  h_cv_gymno <- cor.test(gymno$mean_hmax, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  h_mat_angio <- cor.test(angio$mean_hmax, angio$mat, method = "pearson", na.rm = TRUE)
  h_mat_gymno <- cor.test(gymno$mean_hmax, gymno$mat, method = "pearson", na.rm = TRUE)
  
  h_tmin_angio <- cor.test(angio$mean_hmax, angio$tmin, method = "pearson", na.rm = TRUE)
  h_tmin_gymno <- cor.test(gymno$mean_hmax, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  h_map_angio <- cor.test(angio$mean_hmax, angio$map, method = "pearson", na.rm = TRUE)
  h_map_gymno <- cor.test(gymno$mean_hmax, gymno$map, method = "pearson", na.rm = TRUE)
  
  h_map_angio <- cor.test(angio$mean_hmax, angio$map, method = "pearson", na.rm = TRUE)
  h_map_gymno <- cor.test(gymno$mean_hmax, gymno$map, method = "pearson", na.rm = TRUE)
  
  h_metp_angio <- cor.test(angio$mean_hmax, angio$p_etp, method = "pearson", na.rm = TRUE)
  h_metp_gymno <- cor.test(gymno$mean_hmax, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  h_sla_angio <- cor.test(angio$mean_hmax, angio$sla, method = "pearson", na.rm = TRUE)
  h_sla_gymno <- cor.test(gymno$mean_hmax, gymno$sla, method = "pearson", na.rm = TRUE)
  
  h_wd_angio <- cor.test(angio$mean_hmax, angio$ssd, method = "pearson", na.rm = TRUE)
  h_wd_gymno <- cor.test(gymno$mean_hmax, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  h_sh_angio <- cor.test(angio$mean_hmax, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  h_sh_gymno <- cor.test(gymno$mean_hmax, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  
  r_cd_angio <- cor.test(angio$mean_a1_ratio, angio$diam_response, method = "pearson", na.rm = TRUE)
  r_cd_gymno <- cor.test(gymno$mean_a1_ratio, gymno$diam_response, method = "pearson", na.rm = TRUE)
  
  r_cr_angio <- cor.test(angio$mean_a1_ratio, angio$ratio_response, method = "pearson", na.rm = TRUE)
  r_cr_gymno <- cor.test(gymno$mean_a1_ratio, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  r_cv_angio <- cor.test(angio$mean_a1_ratio, angio$volume_response, method = "pearson", na.rm = TRUE)
  r_cv_gymno <- cor.test(gymno$mean_a1_ratio, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  r_mat_angio <- cor.test(angio$mean_a1_ratio, angio$mat, method = "pearson", na.rm = TRUE)
  r_mat_gymno <- cor.test(gymno$mean_a1_ratio, gymno$mat, method = "pearson", na.rm = TRUE)
  
  r_tmin_angio <- cor.test(angio$mean_a1_ratio, angio$tmin, method = "pearson", na.rm = TRUE)
  r_tmin_gymno <- cor.test(gymno$mean_a1_ratio, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  r_map_angio <- cor.test(angio$mean_a1_ratio, angio$map, method = "pearson", na.rm = TRUE)
  r_map_gymno <- cor.test(gymno$mean_a1_ratio, gymno$map, method = "pearson", na.rm = TRUE)
  
  r_metp_angio <- cor.test(angio$mean_a1_ratio, angio$p_etp, method = "pearson", na.rm = TRUE)
  r_metp_gymno <- cor.test(gymno$mean_a1_ratio, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  r_sla_angio <- cor.test(angio$mean_a1_ratio, angio$sla, method = "pearson", na.rm = TRUE)
  r_sla_gymno <- cor.test(gymno$mean_a1_ratio, gymno$sla, method = "pearson", na.rm = TRUE)
  
  r_wd_angio <- cor.test(angio$mean_a1_ratio, angio$ssd, method = "pearson", na.rm = TRUE)
  r_wd_gymno <- cor.test(gymno$mean_a1_ratio, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  r_sh_angio <- cor.test(angio$mean_a1_ratio, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  r_sh_gymno <- cor.test(gymno$mean_a1_ratio, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  d_cd_angio <- cor.test(angio$mean_diameter_15, angio$diam_response, method = "pearson", na.rm = TRUE)
  d_cd_gymno <- cor.test(gymno$mean_diameter_15, gymno$diam_response, method = "pearson", na.rm = TRUE)
  
  d_cr_angio <- cor.test(angio$mean_diameter_15, angio$ratio_response, method = "pearson", na.rm = TRUE)
  d_cr_gymno <- cor.test(gymno$mean_diameter_15, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  d_cv_angio <- cor.test(angio$mean_diameter_15, angio$volume_response, method = "pearson", na.rm = TRUE)
  d_cv_gymno <- cor.test(gymno$mean_diameter_15, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  d_mat_angio <- cor.test(angio$mean_diameter_15, angio$mat, method = "pearson", na.rm = TRUE)
  d_mat_gymno <- cor.test(gymno$mean_diameter_15, gymno$mat, method = "pearson", na.rm = TRUE)
  
  d_tmin_angio <- cor.test(angio$mean_diameter_15, angio$tmin, method = "pearson", na.rm = TRUE)
  d_tmin_gymno <- cor.test(gymno$mean_diameter_15, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  d_map_angio <- cor.test(angio$mean_diameter_15, angio$map, method = "pearson", na.rm = TRUE)
  d_map_gymno <- cor.test(gymno$mean_diameter_15, gymno$map, method = "pearson", na.rm = TRUE)
  
  d_metp_angio <- cor.test(angio$mean_diameter_15, angio$p_etp, method = "pearson", na.rm = TRUE)
  d_metp_gymno <- cor.test(gymno$mean_diameter_15, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  d_sla_angio <- cor.test(angio$mean_diameter_15, angio$sla, method = "pearson", na.rm = TRUE)
  d_sla_gymno <- cor.test(gymno$mean_diameter_15, gymno$sla, method = "pearson", na.rm = TRUE)
  
  d_wd_angio <- cor.test(angio$mean_diameter_15, angio$ssd, method = "pearson", na.rm = TRUE)
  d_wd_gymno <- cor.test(gymno$mean_diameter_15, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  d_sh_angio <- cor.test(angio$mean_diameter_15, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  d_sh_gymno <- cor.test(gymno$mean_diameter_15, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  
  v_cd_angio <- cor.test(angio$volume_15_0, angio$diam_response, method = "pearson", na.rm = TRUE)
  v_cd_gymno <- cor.test(gymno$volume_15_0, gymno$diam_response, method = "pearson", na.rm = TRUE)
  
  v_cr_angio <- cor.test(angio$volume_15_0, angio$ratio_response, method = "pearson", na.rm = TRUE)
  v_cr_gymno <- cor.test(gymno$volume_15_0, gymno$ratio_response, method = "pearson", na.rm = TRUE)
  
  v_cv_angio <- cor.test(angio$volume_15_0, angio$volume_response, method = "pearson", na.rm = TRUE)
  v_cv_gymno <- cor.test(gymno$volume_15_0, gymno$volume_response, method = "pearson", na.rm = TRUE)
  
  v_mat_angio <- cor.test(angio$volume_15_0, angio$mat, method = "pearson", na.rm = TRUE)
  v_mat_gymno <- cor.test(gymno$volume_15_0, gymno$mat, method = "pearson", na.rm = TRUE)
  
  v_tmin_angio <- cor.test(angio$volume_15_0, angio$tmin, method = "pearson", na.rm = TRUE)
  v_tmin_gymno <- cor.test(gymno$volume_15_0, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  v_map_angio <- cor.test(angio$volume_15_0, angio$map, method = "pearson", na.rm = TRUE)
  v_map_gymno <- cor.test(gymno$volume_15_0, gymno$map, method = "pearson", na.rm = TRUE)
  
  v_metp_angio <- cor.test(angio$volume_15_0, angio$p_etp, method = "pearson", na.rm = TRUE)
  v_metp_gymno <- cor.test(gymno$volume_15_0, gymno$p_etp, method = "pearson", na.rm = TRUE)
  
  v_sla_angio <- cor.test(angio$volume_15_0, angio$sla, method = "pearson", na.rm = TRUE)
  v_sla_gymno <- cor.test(gymno$volume_15_0, gymno$sla, method = "pearson", na.rm = TRUE)
  
  v_wd_angio <- cor.test(angio$volume_15_0, angio$ssd, method = "pearson", na.rm = TRUE)
  v_wd_gymno <- cor.test(gymno$volume_15_0, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  v_sh_angio <- cor.test(angio$volume_15_0, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  v_sh_gymno <- cor.test(gymno$volume_15_0, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  
  
  cor_angio <- matrix(nrow = 4, ncol = 10)
  cor_gymno <- matrix(nrow = 4, ncol = 10)
  
  p_angio <- matrix(nrow = 4, ncol = 10)
  p_gymno <- matrix(nrow = 4, ncol = 10)
  
  cor_angio[1,1] <- h_cd_angio$estimate
  cor_angio[1,2] <- h_cr_angio$estimate
  cor_angio[1,3] <- h_cv_angio$estimate
  cor_angio[1,4] <- h_mat_angio$estimate
  cor_angio[1,5] <- h_tmin_angio$estimate
  cor_angio[1,6] <- h_map_angio$estimate
  cor_angio[1,7] <- h_metp_angio$estimate
  cor_angio[1,8] <- h_sla_angio$estimate
  cor_angio[1,9] <- h_wd_angio$estimate
  cor_angio[1,10] <- h_sh_angio$estimate
  
  cor_angio[2,1] <- r_cd_angio$estimate
  cor_angio[2,2] <- r_cr_angio$estimate
  cor_angio[2,3] <- r_cv_angio$estimate
  cor_angio[2,4] <- r_mat_angio$estimate
  cor_angio[2,5] <- r_tmin_angio$estimate
  cor_angio[2,6] <- r_map_angio$estimate
  cor_angio[2,7] <- r_metp_angio$estimate
  cor_angio[2,8] <- r_sla_angio$estimate
  cor_angio[2,9] <- r_wd_angio$estimate
  cor_angio[2,10] <- r_sh_angio$estimate
  
  cor_angio[3,1] <- d_cd_angio$estimate
  cor_angio[3,2] <- d_cr_angio$estimate
  cor_angio[3,3] <- d_cv_angio$estimate
  cor_angio[3,4] <- d_mat_angio$estimate
  cor_angio[3,5] <- d_tmin_angio$estimate
  cor_angio[3,6] <- d_map_angio$estimate
  cor_angio[3,7] <- d_metp_angio$estimate
  cor_angio[3,8] <- d_sla_angio$estimate
  cor_angio[3,9] <- d_wd_angio$estimate
  cor_angio[3,10] <- d_sh_angio$estimate
  
  cor_angio[4,1] <- v_cd_angio$estimate
  cor_angio[4,2] <- v_cr_angio$estimate
  cor_angio[4,3] <- v_cv_angio$estimate
  cor_angio[4,4] <- v_mat_angio$estimate
  cor_angio[4,5] <- v_tmin_angio$estimate
  cor_angio[4,6] <- v_map_angio$estimate
  cor_angio[4,7] <- v_metp_angio$estimate
  cor_angio[4,8] <- v_sla_angio$estimate
  cor_angio[4,9] <- v_wd_angio$estimate
  cor_angio[4,10] <- v_sh_angio$estimate
  
  
  p_angio[1,1] <- h_cd_angio$p.value
  p_angio[1,2] <- h_cr_angio$p.value
  p_angio[1,3] <- h_cv_angio$p.value
  p_angio[1,4] <- h_mat_angio$p.value
  p_angio[1,5] <- h_tmin_angio$p.value
  p_angio[1,6] <- h_map_angio$p.value
  p_angio[1,7] <- h_metp_angio$p.value
  p_angio[1,8] <- h_sla_angio$p.value
  p_angio[1,9] <- h_wd_angio$p.value
  p_angio[1,10] <- h_sh_angio$p.value
  
  p_angio[2,1] <- r_cd_angio$p.value
  p_angio[2,2] <- r_cr_angio$p.value
  p_angio[2,3] <- r_cv_angio$p.value
  p_angio[2,4] <- r_mat_angio$p.value
  p_angio[2,5] <- r_tmin_angio$p.value
  p_angio[2,6] <- r_map_angio$p.value
  p_angio[2,7] <- r_metp_angio$p.value
  p_angio[2,8] <- r_sla_angio$p.value
  p_angio[2,9] <- r_wd_angio$p.value
  p_angio[2,10] <- r_sh_angio$p.value
  
  p_angio[3,1] <- d_cd_angio$p.value
  p_angio[3,2] <- d_cr_angio$p.value
  p_angio[3,3] <- d_cv_angio$p.value
  p_angio[3,4] <- d_mat_angio$p.value
  p_angio[3,5] <- d_tmin_angio$p.value
  p_angio[3,6] <- d_map_angio$p.value
  p_angio[3,7] <- d_metp_angio$p.value
  p_angio[3,8] <- d_sla_angio$p.value
  p_angio[3,9] <- d_wd_angio$p.value
  p_angio[3,10] <- d_sh_angio$p.value
  
  p_angio[4,1] <- v_cd_angio$p.value
  p_angio[4,2] <- v_cr_angio$p.value
  p_angio[4,3] <- v_cv_angio$p.value
  p_angio[4,4] <- v_mat_angio$p.value
  p_angio[4,5] <- v_tmin_angio$p.value
  p_angio[4,6] <- v_map_angio$p.value
  p_angio[4,7] <- v_metp_angio$p.value
  p_angio[4,8] <- v_sla_angio$p.value
  p_angio[4,9] <- v_wd_angio$p.value
  p_angio[4,10] <- v_sh_angio$p.value  
  
  
  cor_gymno[1,1] <- h_cd_gymno$estimate
  cor_gymno[1,2] <- h_cr_gymno$estimate
  cor_gymno[1,3] <- h_cv_gymno$estimate
  cor_gymno[1,4] <- h_mat_gymno$estimate
  cor_gymno[1,5] <- h_tmin_gymno$estimate
  cor_gymno[1,6] <- h_map_gymno$estimate
  cor_gymno[1,7] <- h_metp_gymno$estimate
  cor_gymno[1,8] <- h_sla_gymno$estimate
  cor_gymno[1,9] <- h_wd_gymno$estimate
  cor_gymno[1,10] <- h_sh_gymno$estimate
  
  cor_gymno[2,1] <- r_cd_gymno$estimate
  cor_gymno[2,2] <- r_cr_gymno$estimate
  cor_gymno[2,3] <- r_cv_gymno$estimate
  cor_gymno[2,4] <- r_mat_gymno$estimate
  cor_gymno[2,5] <- r_tmin_gymno$estimate
  cor_gymno[2,6] <- r_map_gymno$estimate
  cor_gymno[2,7] <- r_metp_gymno$estimate
  cor_gymno[2,8] <- r_sla_gymno$estimate
  cor_gymno[2,9] <- r_wd_gymno$estimate
  cor_gymno[2,10] <- r_sh_gymno$estimate
  
  cor_gymno[3,1] <- d_cd_gymno$estimate
  cor_gymno[3,2] <- d_cr_gymno$estimate
  cor_gymno[3,3] <- d_cv_gymno$estimate
  cor_gymno[3,4] <- d_mat_gymno$estimate
  cor_gymno[3,5] <- d_tmin_gymno$estimate
  cor_gymno[3,6] <- d_map_gymno$estimate
  cor_gymno[3,7] <- d_metp_gymno$estimate
  cor_gymno[3,8] <- d_sla_gymno$estimate
  cor_gymno[3,9] <- d_wd_gymno$estimate
  cor_gymno[3,10] <- d_sh_gymno$estimate
  
  cor_gymno[4,1] <- v_cd_gymno$estimate
  cor_gymno[4,2] <- v_cr_gymno$estimate
  cor_gymno[4,3] <- v_cv_gymno$estimate
  cor_gymno[4,4] <- v_mat_gymno$estimate
  cor_gymno[4,5] <- v_tmin_gymno$estimate
  cor_gymno[4,6] <- v_map_gymno$estimate
  cor_gymno[4,7] <- v_metp_gymno$estimate
  cor_gymno[4,8] <- v_sla_gymno$estimate
  cor_gymno[4,9] <- v_wd_gymno$estimate
  cor_gymno[4,10] <- v_sh_gymno$estimate
  
  
  p_gymno[1,1] <- h_cd_gymno$p.value
  p_gymno[1,2] <- h_cr_gymno$p.value
  p_gymno[1,3] <- h_cv_gymno$p.value
  p_gymno[1,4] <- h_mat_gymno$p.value
  p_gymno[1,5] <- h_tmin_gymno$p.value
  p_gymno[1,6] <- h_map_gymno$p.value
  p_gymno[1,7] <- h_metp_gymno$p.value
  p_gymno[1,8] <- h_sla_gymno$p.value
  p_gymno[1,9] <- h_wd_gymno$p.value
  p_gymno[1,10] <- h_sh_gymno$p.value
  
  p_gymno[2,1] <- r_cd_gymno$p.value
  p_gymno[2,2] <- r_cr_gymno$p.value
  p_gymno[2,3] <- r_cv_gymno$p.value
  p_gymno[2,4] <- r_mat_gymno$p.value
  p_gymno[2,5] <- r_tmin_gymno$p.value
  p_gymno[2,6] <- r_map_gymno$p.value
  p_gymno[2,7] <- r_metp_gymno$p.value
  p_gymno[2,8] <- r_sla_gymno$p.value
  p_gymno[2,9] <- r_wd_gymno$p.value
  p_gymno[2,10] <- r_sh_gymno$p.value
  
  p_gymno[3,1] <- d_cd_gymno$p.value
  p_gymno[3,2] <- d_cr_gymno$p.value
  p_gymno[3,3] <- d_cv_gymno$p.value
  p_gymno[3,4] <- d_mat_gymno$p.value
  p_gymno[3,5] <- d_tmin_gymno$p.value
  p_gymno[3,6] <- d_map_gymno$p.value
  p_gymno[3,7] <- d_metp_gymno$p.value
  p_gymno[3,8] <- d_sla_gymno$p.value
  p_gymno[3,9] <- d_wd_gymno$p.value
  p_gymno[3,10] <- d_sh_gymno$p.value
  
  p_gymno[4,1] <- v_cd_gymno$p.value
  p_gymno[4,2] <- v_cr_gymno$p.value
  p_gymno[4,3] <- v_cv_gymno$p.value
  p_gymno[4,4] <- v_mat_gymno$p.value
  p_gymno[4,5] <- v_tmin_gymno$p.value
  p_gymno[4,6] <- v_map_gymno$p.value
  p_gymno[4,7] <- v_metp_gymno$p.value
  p_gymno[4,8] <- v_sla_gymno$p.value
  p_gymno[4,9] <- v_wd_gymno$p.value
  p_gymno[4,10] <- v_sh_gymno$p.value  
  
  par(mfrow = c(1,1))
  corrplot(cor_angio, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Oranges"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  corrplot(cor_gymno, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Greens"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  
}









response_to_competition <- function() {
  
  ## loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(-X)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_dbh_nocomp_summary.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(-X)
  
  height_dbh <- read.csv(file = "output/height_dbh_summary.csv")
  height_dbh <- height_dbh %>% dplyr::select(-X)
  
  diameter_c1 <- read.csv(file = "output/diameter_power_parameters_c1.csv")
  diameter_c1 <- diameter_c1 %>% dplyr::rename(mean_comp_diam_c1 = mean_comp) %>% 
                                 dplyr::select(-X) %>% 
                                 dplyr::select(species,mean_comp_diam_c1)
  
  diameter_dbh_c1 <- read.csv(file = "output/diameter_dbh_c1_summary.csv")
  diameter_dbh_c1 <- diameter_dbh_c1 %>% dplyr::select(-X)
  
  diameter_c2 <- read.csv(file = "output/diameter_power_parameters_c2.csv")
  diameter_c2 <- diameter_c2 %>% dplyr::rename(mean_comp_diam_c2 = mean_comp) %>% 
                                 dplyr::select(-X) %>% 
                                 dplyr::select(species,mean_comp_diam_c2)
  
  diameter_dbh_c2 <- read.csv(file = "output/diameter_dbh_c2_summary.csv")
  diameter_dbh_c2 <- diameter_dbh_c2 %>% dplyr::select(-X)
  
  ratio_c1 <- read.csv(file = "output/ratio_beta_c1_parameters.csv")
  ratio_c1 <- ratio_c1 %>% dplyr::rename(mean_comp_ratio_c1 = mean_comp) %>% 
                              dplyr::select(-X) %>% 
                              dplyr::select(species,mean_comp_ratio_c1)
  
  ratio_dbh_c1 <- read.csv(file = "output/ratio_dbh_c1_summary.csv")
  ratio_dbh_c1 <- ratio_dbh_c1 %>% dplyr::select(-X)
  
  ratio_c2 <- read.csv(file = "output/ratio_beta_c2_parameters.csv")
  ratio_c2 <- ratio_c2 %>% dplyr::rename(mean_comp_ratio_c2 = mean_comp) %>% 
                           dplyr::select(-X) %>% 
                           dplyr::select(species,mean_comp_ratio_c2)
  
  ratio_dbh_c2 <- read.csv(file = "output/ratio_dbh_c2_summary.csv")
  ratio_dbh_c2 <- ratio_dbh_c2 %>% dplyr::select(-X)
  
  
  
  ## computing crown volume 
  # identifying species for which all variables are available
  data <- left_join(diameter_dbh, ratio_dbh, by = "species")
  data <- left_join(data, height_max, by = "species")
  data <- left_join(data, height_dbh, by = "species")
  data <- left_join(data, diameter_c1, by = "species")
  data <- left_join(data, diameter_dbh_c1, by = "species")
  data <- left_join(data, ratio_c1, by = "species")
  data <- left_join(data, ratio_dbh_c1, by = "species")
  
  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  data_gp <- left_join(gp_info, data, by = c("sp" = "species"))
  
  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_ratio_15)/2),
                                     volume_15_5 = 4/3 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * ((mean_h_15*mean_ratio_15_5)/2),
                                     volume_30_5 = 4/3 * pi * (mean_diameter_30_5/2)*(mean_diameter_30_5/2) * ((mean_h_30*mean_ratio_30_5)/2),
                                     volume_15_20 = 4/3 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * ((mean_h_15*mean_ratio_15_20)/2),
                                     volume_30_20 = 4/3 * pi * (mean_diameter_30_20/2)*(mean_diameter_30_20/2) * ((mean_h_30*mean_ratio_30_20)/2))
  
  angio <- angio[!(angio$sp %in% c("Alnus rubra", "Quercus nigra")),]
  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_ratio_15),
                                     volume_15_5 = 1/2 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * (mean_h_15*mean_ratio_15_5),
                                     volume_30_5 = 1/2 * pi * (mean_diameter_30_5/2)*(mean_diameter_30_5/2) * (mean_h_30*mean_ratio_30_5),
                                     volume_15_20 = 1/2 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * (mean_h_15*mean_ratio_15_20),
                                     volume_30_20 = 1/2 * pi * (mean_diameter_30_20/2)*(mean_diameter_30_20/2) * (mean_h_30*mean_ratio_30_20))
  
  gymno <- gymno[!(gymno$sp %in% c("Pinus elliottii")),]
  
  all_data <- rbind(angio, gymno)

  angio <- angio[angio$mean_comp_diam_c1 < 0.2,]
  hist(na.omit(angio$mean_comp_diam_c1), breaks = 50, 
       xlim = c(-0.1, 0.2), ylim = c(0,15),
       las = 1, 
       xlab = "response to ba plot", main = "")
  plot(density(na.omit(angio$mean_comp_diam_c1)), type = "l", lwd = 2, col = "darkorange", 
       ylim = c(0,100), 
       las = 1, xlab = "", main = "")
  
  plot(density(na.omit(angio$mean_comp_ratio_c1)), type = "l", lwd = 2, col = "darkorange", 
       ylim = c(0,150), 
       las = 1, xlab = "", main = "")
  
  
  gymno <- gymno[gymno$mean_comp_diam_c1 < 0.2,]
  hist(na.omit(gymno$mean_comp_diam_c1), breaks = 50, 
       xlim = c(-0.05, 0.15), ylim = c(0,15),
       las = 1, 
       xlab = "response to ba plot", main = "")
  lines(density(na.omit(gymno$mean_comp_diam_c1)), lwd = 2, col = "darkgreen")
  
  lines(density(na.omit(gymno$mean_comp_ratio_c1)), lwd = 2, col = "darkgreen")
  

  
## running Pearson correlations on pairs of dimensions 
  # height max - diam comp
  h_dc1_angio <- cor.test(angio$mean_hmax, angio$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  h_dc1_gymno <- cor.test(gymno$mean_hmax, gymno$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  
  # height max - diam dbh 15_5
  h_d15_5_angio <- cor.test(angio$mean_hmax, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  h_d15_5_gymno <- cor.test(gymno$mean_hmax, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # height max - ratio comp
  h_rc1_angio <- cor.test(angio$mean_hmax, angio$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  h_rc1_gymno <- cor.test(gymno$mean_hmax, gymno$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  
  # height max - ratio dbh 15_5
  h_r15_5_angio <- cor.test(angio$mean_hmax, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  h_r15_5_gymno <- cor.test(gymno$mean_hmax, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)

  # height max - volume dbh 15_5
  h_v15_5_angio <- cor.test(angio$mean_hmax, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  h_v15_5_gymno <- cor.test(gymno$mean_hmax, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  
  
  # diameter - diam comp
  d_dc1_angio <- cor.test(angio$mean_diameter_15, angio$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  d_dc1_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  
  # diameter - diam dbh 15_5
  d_d15_5_angio <- cor.test(angio$mean_diameter_15, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  d_d15_5_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # diameter - ratio comp
  d_rc1_angio <- cor.test(angio$mean_diameter_15, angio$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  d_rc1_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  
  # diameter - ratio dbh 15_5
  d_r15_5_angio <- cor.test(angio$mean_diameter_15, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  d_r15_5_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  
  # diameter - volume dbh 15_5
  d_v15_5_angio <- cor.test(angio$mean_diameter_15, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  d_v15_5_gymno <- cor.test(gymno$mean_diameter_15, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  

  # ratio - diam comp
  r_dc1_angio <- cor.test(angio$mean_ratio_15, angio$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  r_dc1_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  
  # ratio - diam dbh 15_5
  r_d15_5_angio <- cor.test(angio$mean_ratio_15, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  r_d15_5_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # ratio - ratio comp
  r_rc1_angio <- cor.test(angio$mean_ratio_15, angio$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  r_rc1_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  
  # ratio - ratio dbh 15_5
  r_r15_5_angio <- cor.test(angio$mean_ratio_15, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  r_r15_5_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  
  # ratio - volume dbh 15_5
  r_v15_5_angio <- cor.test(angio$mean_ratio_15, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  r_v15_5_gymno <- cor.test(gymno$mean_ratio_15, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  
  
  # volume - diam comp
  v_dc1_angio <- cor.test(angio$volume_15, angio$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  v_dc1_gymno <- cor.test(gymno$volume_15, gymno$mean_comp_diam_c1, method = "pearson", na.rm = TRUE)
  
  # volume - diam dbh 15_5
  v_d15_5_angio <- cor.test(angio$volume_15, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  v_d15_5_gymno <- cor.test(gymno$volume_15, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # volume - ratio comp
  v_rc1_angio <- cor.test(angio$volume_15, angio$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  v_rc1_gymno <- cor.test(gymno$volume_15, gymno$mean_comp_ratio_c1, method = "pearson", na.rm = TRUE)
  
  # volume - ratio dbh 15_5
  v_r15_5_angio <- cor.test(angio$volume_15, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  v_r15_5_gymno <- cor.test(gymno$volume_15, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  
  # volume - volume dbh 15_5
  v_v15_5_angio <- cor.test(angio$volume_15, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  v_v15_5_gymno <- cor.test(gymno$volume_15, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  
  par(mfrow = c(1,2))
  
  
  cor_angio <- matrix(nrow = 4, ncol = 5)
  cor_angio[1,1] <- h_dc1_angio$estimate
  cor_angio[1,2] <- h_d15_5_angio$estimate
  cor_angio[1,3] <- h_rc1_angio$estimate
  cor_angio[1,4] <- h_r15_5_angio$estimate
  cor_angio[1,5] <- h_v15_5_angio$estimate
  
  cor_angio[2,1] <- d_dc1_angio$estimate
  cor_angio[2,2] <- d_d15_5_angio$estimate
  cor_angio[2,3] <- d_rc1_angio$estimate
  cor_angio[2,4] <- d_r15_5_angio$estimate
  cor_angio[2,5] <- d_v15_5_angio$estimate
  
  cor_angio[3,1] <- r_dc1_angio$estimate
  cor_angio[3,2] <- r_d15_5_angio$estimate
  cor_angio[3,3] <- r_rc1_angio$estimate
  cor_angio[3,4] <- r_r15_5_angio$estimate
  cor_angio[3,5] <- r_v15_5_angio$estimate
  
  cor_angio[4,1] <- r_dc1_angio$estimate
  cor_angio[4,2] <- r_d15_5_angio$estimate
  cor_angio[4,3] <- r_rc1_angio$estimate
  cor_angio[4,4] <- r_r15_5_angio$estimate
  cor_angio[4,5] <- r_v15_5_angio$estimate
  
  
  p_angio <- matrix(nrow = 4, ncol = 5)
  p_angio[1,1] <- h_dc1_angio$p.value
  p_angio[1,2] <- h_d15_5_angio$p.value
  p_angio[1,3] <- h_rc1_angio$p.value
  p_angio[1,4] <- h_r15_5_angio$p.value
  p_angio[1,5] <- h_v15_5_angio$p.value
  
  p_angio[2,1] <- d_dc1_angio$p.value
  p_angio[2,2] <- d_d15_5_angio$p.value
  p_angio[2,3] <- d_rc1_angio$p.value
  p_angio[2,4] <- d_r15_5_angio$p.value
  p_angio[2,5] <- d_v15_5_angio$p.value
  
  p_angio[3,1] <- r_dc1_angio$p.value
  p_angio[3,2] <- r_d15_5_angio$p.value
  p_angio[3,3] <- r_rc1_angio$p.value
  p_angio[3,4] <- r_r15_5_angio$p.value
  p_angio[3,5] <- r_v15_5_angio$p.value
  
  p_angio[4,1] <- r_dc1_angio$p.value
  p_angio[4,2] <- r_d15_5_angio$p.value
  p_angio[4,3] <- r_rc1_angio$p.value
  p_angio[4,4] <- r_r15_5_angio$p.value
  p_angio[4,5] <- r_v15_5_angio$p.value
  
  
  corrplot(cor_angio, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Oranges"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  
  
  cor_gymno <- matrix(nrow = 4, ncol = 5)
  cor_gymno[1,1] <- h_dc1_gymno$estimate
  cor_gymno[1,2] <- h_d15_5_gymno$estimate
  cor_gymno[1,3] <- h_rc1_gymno$estimate
  cor_gymno[1,4] <- h_r15_5_gymno$estimate
  cor_gymno[1,5] <- h_v15_5_gymno$estimate
  
  cor_gymno[2,1] <- d_dc1_gymno$estimate
  cor_gymno[2,2] <- d_d15_5_gymno$estimate
  cor_gymno[2,3] <- d_rc1_gymno$estimate
  cor_gymno[2,4] <- d_r15_5_gymno$estimate
  cor_gymno[2,5] <- d_v15_5_gymno$estimate
  
  cor_gymno[3,1] <- r_dc1_gymno$estimate
  cor_gymno[3,2] <- r_d15_5_gymno$estimate
  cor_gymno[3,3] <- r_rc1_gymno$estimate
  cor_gymno[3,4] <- r_r15_5_gymno$estimate
  cor_gymno[3,5] <- r_v15_5_gymno$estimate
  
  cor_gymno[4,1] <- r_dc1_gymno$estimate
  cor_gymno[4,2] <- r_d15_5_gymno$estimate
  cor_gymno[4,3] <- r_rc1_gymno$estimate
  cor_gymno[4,4] <- r_r15_5_gymno$estimate
  cor_gymno[4,5] <- r_v15_5_gymno$estimate
  
  
  p_gymno <- matrix(nrow = 4, ncol = 5)
  p_gymno[1,1] <- h_dc1_gymno$p.value
  p_gymno[1,2] <- h_d15_5_gymno$p.value
  p_gymno[1,3] <- h_rc1_gymno$p.value
  p_gymno[1,4] <- h_r15_5_gymno$p.value
  p_gymno[1,5] <- h_v15_5_gymno$p.value
  
  p_gymno[2,1] <- d_dc1_gymno$p.value
  p_gymno[2,2] <- d_d15_5_gymno$p.value
  p_gymno[2,3] <- d_rc1_gymno$p.value
  p_gymno[2,4] <- d_r15_5_gymno$p.value
  p_gymno[2,5] <- d_v15_5_gymno$p.value
  
  p_gymno[3,1] <- r_dc1_gymno$p.value
  p_gymno[3,2] <- r_d15_5_gymno$p.value
  p_gymno[3,3] <- r_rc1_gymno$p.value
  p_gymno[3,4] <- r_r15_5_gymno$p.value
  p_gymno[3,5] <- r_v15_5_gymno$p.value
  
  p_gymno[4,1] <- r_dc1_gymno$p.value
  p_gymno[4,2] <- r_d15_5_gymno$p.value
  p_gymno[4,3] <- r_rc1_gymno$p.value
  p_gymno[4,4] <- r_r15_5_gymno$p.value
  p_gymno[4,5] <- r_v15_5_gymno$p.value
  
  corrplot(cor_gymno, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Greens"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  
  
  
  ## computing crown volume 
  # identifying species for which all variables are available
  data <- left_join(diameter_dbh, ratio_dbh, by = "species")
  data <- left_join(data, height_max, by = "species")
  data <- left_join(data, height_dbh, by = "species")
  data <- left_join(data, diameter_c2, by = "species")
  data <- left_join(data, diameter_dbh_c2, by = "species")
  data <- left_join(data, ratio_c2, by = "species")
  data <- left_join(data, ratio_dbh_c2, by = "species")
  
  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  data_gp <- left_join(gp_info, data, by = c("sp" = "species"))
  
  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_ratio_15)/2),
                                    volume_15_5 = 4/3 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * ((mean_h_15*mean_ratio_15_5)/2),
                                    volume_30_5 = 4/3 * pi * (mean_diameter_30_5/2)*(mean_diameter_30_5/2) * ((mean_h_30*mean_ratio_30_5)/2),
                                    volume_15_20 = 4/3 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * ((mean_h_15*mean_ratio_15_20)/2),
                                    volume_30_20 = 4/3 * pi * (mean_diameter_30_20/2)*(mean_diameter_30_20/2) * ((mean_h_30*mean_ratio_30_20)/2))
  
  angio <- angio[!(angio$sp %in% c("Alnus rubra", "Quercus nigra")),]
  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_ratio_15),
                                    volume_15_5 = 1/2 * pi * (mean_diameter_15_5/2)*(mean_diameter_15_5/2) * (mean_h_15*mean_ratio_15_5),
                                    volume_30_5 = 1/2 * pi * (mean_diameter_30_5/2)*(mean_diameter_30_5/2) * (mean_h_30*mean_ratio_30_5),
                                    volume_15_20 = 1/2 * pi * (mean_diameter_15_20/2)*(mean_diameter_15_20/2) * (mean_h_15*mean_ratio_15_20),
                                    volume_30_20 = 1/2 * pi * (mean_diameter_30_20/2)*(mean_diameter_30_20/2) * (mean_h_30*mean_ratio_30_20))
  
  gymno <- gymno[!(gymno$sp %in% c("Pinus elliottii")),]
  
  all_data <- rbind(angio, gymno)
  
  ## running Pearson correlations on pairs of dimensions 
  # height max - diam comp
  h_dc2_angio <- cor.test(angio$mean_hmax, angio$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  h_dc2_gymno <- cor.test(gymno$mean_hmax, gymno$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  
  # height max - diam dbh 15_5
  h_d15_5_angio <- cor.test(angio$mean_hmax, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  h_d15_5_gymno <- cor.test(gymno$mean_hmax, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # height max - ratio comp
  h_rc2_angio <- cor.test(angio$mean_hmax, angio$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  h_rc2_gymno <- cor.test(gymno$mean_hmax, gymno$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  
  # height max - ratio dbh 15_5
  h_r15_5_angio <- cor.test(angio$mean_hmax, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  h_r15_5_gymno <- cor.test(gymno$mean_hmax, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  
  # height max - volume dbh 15_5
  h_v15_5_angio <- cor.test(angio$mean_hmax, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  h_v15_5_gymno <- cor.test(gymno$mean_hmax, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  
  
  # diameter - diam comp
  d_dc2_angio <- cor.test(angio$mean_diameter_15, angio$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  d_dc2_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  
  # diameter - diam dbh 15_5
  d_d15_5_angio <- cor.test(angio$mean_diameter_15, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  d_d15_5_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # diameter - ratio comp
  d_rc2_angio <- cor.test(angio$mean_diameter_15, angio$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  d_rc2_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  
  # diameter - ratio dbh 15_5
  d_r15_5_angio <- cor.test(angio$mean_diameter_15, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  d_r15_5_gymno <- cor.test(gymno$mean_diameter_15, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  
  # diameter - volume dbh 15_5
  d_v15_5_angio <- cor.test(angio$mean_diameter_15, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  d_v15_5_gymno <- cor.test(gymno$mean_diameter_15, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  
  
  # ratio - diam comp
  r_dc2_angio <- cor.test(angio$mean_ratio_15, angio$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  r_dc2_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  
  # ratio - diam dbh 15_5
  r_d15_5_angio <- cor.test(angio$mean_ratio_15, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  r_d15_5_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # ratio - ratio comp
  r_rc2_angio <- cor.test(angio$mean_ratio_15, angio$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  r_rc2_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  
  # ratio - ratio dbh 15_5
  r_r15_5_angio <- cor.test(angio$mean_ratio_15, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  r_r15_5_gymno <- cor.test(gymno$mean_ratio_15, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  
  # ratio - volume dbh 15_5
  r_v15_5_angio <- cor.test(angio$mean_ratio_15, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  r_v15_5_gymno <- cor.test(gymno$mean_ratio_15, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  
  
  # volume - diam comp
  v_dc2_angio <- cor.test(angio$volume_15, angio$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  v_dc2_gymno <- cor.test(gymno$volume_15, gymno$mean_comp_diam_c2, method = "pearson", na.rm = TRUE)
  
  # volume - diam dbh 15_5
  v_d15_5_angio <- cor.test(angio$volume_15, angio$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  v_d15_5_gymno <- cor.test(gymno$volume_15, gymno$mean_diameter_15_5, method = "pearson", na.rm = TRUE)
  
  # volume - ratio comp
  v_rc2_angio <- cor.test(angio$volume_15, angio$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  v_rc2_gymno <- cor.test(gymno$volume_15, gymno$mean_comp_ratio_c2, method = "pearson", na.rm = TRUE)
  
  # volume - ratio dbh 15_5
  v_r15_5_angio <- cor.test(angio$volume_15, angio$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  v_r15_5_gymno <- cor.test(gymno$volume_15, gymno$mean_ratio_15_5, method = "pearson", na.rm = TRUE)
  
  # volume - volume dbh 15_5
  v_v15_5_angio <- cor.test(angio$volume_15, angio$volume_15_5, method = "pearson", na.rm = TRUE)
  v_v15_5_gymno <- cor.test(gymno$volume_15, gymno$volume_15_5, method = "pearson", na.rm = TRUE)
  
  par(mfrow = c(1,2))
  
  
  cor_angio <- matrix(nrow = 4, ncol = 5)
  cor_angio[1,1] <- h_dc2_angio$estimate
  cor_angio[1,2] <- h_d15_5_angio$estimate
  cor_angio[1,3] <- h_rc2_angio$estimate
  cor_angio[1,4] <- h_r15_5_angio$estimate
  cor_angio[1,5] <- h_v15_5_angio$estimate
  
  cor_angio[2,1] <- d_dc2_angio$estimate
  cor_angio[2,2] <- d_d15_5_angio$estimate
  cor_angio[2,3] <- d_rc2_angio$estimate
  cor_angio[2,4] <- d_r15_5_angio$estimate
  cor_angio[2,5] <- d_v15_5_angio$estimate
  
  cor_angio[3,1] <- r_dc2_angio$estimate
  cor_angio[3,2] <- r_d15_5_angio$estimate
  cor_angio[3,3] <- r_rc2_angio$estimate
  cor_angio[3,4] <- r_r15_5_angio$estimate
  cor_angio[3,5] <- r_v15_5_angio$estimate
  
  cor_angio[4,1] <- r_dc2_angio$estimate
  cor_angio[4,2] <- r_d15_5_angio$estimate
  cor_angio[4,3] <- r_rc2_angio$estimate
  cor_angio[4,4] <- r_r15_5_angio$estimate
  cor_angio[4,5] <- r_v15_5_angio$estimate
  
  
  p_angio <- matrix(nrow = 4, ncol = 5)
  p_angio[1,1] <- h_dc2_angio$p.value
  p_angio[1,2] <- h_d15_5_angio$p.value
  p_angio[1,3] <- h_rc2_angio$p.value
  p_angio[1,4] <- h_r15_5_angio$p.value
  p_angio[1,5] <- h_v15_5_angio$p.value
  
  p_angio[2,1] <- d_dc2_angio$p.value
  p_angio[2,2] <- d_d15_5_angio$p.value
  p_angio[2,3] <- d_rc2_angio$p.value
  p_angio[2,4] <- d_r15_5_angio$p.value
  p_angio[2,5] <- d_v15_5_angio$p.value
  
  p_angio[3,1] <- r_dc2_angio$p.value
  p_angio[3,2] <- r_d15_5_angio$p.value
  p_angio[3,3] <- r_rc2_angio$p.value
  p_angio[3,4] <- r_r15_5_angio$p.value
  p_angio[3,5] <- r_v15_5_angio$p.value
  
  p_angio[4,1] <- r_dc2_angio$p.value
  p_angio[4,2] <- r_d15_5_angio$p.value
  p_angio[4,3] <- r_rc2_angio$p.value
  p_angio[4,4] <- r_r15_5_angio$p.value
  p_angio[4,5] <- r_v15_5_angio$p.value
  
  
  corrplot(cor_angio, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Oranges"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  
  
  cor_gymno <- matrix(nrow = 4, ncol = 5)
  cor_gymno[1,1] <- h_dc2_gymno$estimate
  cor_gymno[1,2] <- h_d15_5_gymno$estimate
  cor_gymno[1,3] <- h_rc2_gymno$estimate
  cor_gymno[1,4] <- h_r15_5_gymno$estimate
  cor_gymno[1,5] <- h_v15_5_gymno$estimate
  
  cor_gymno[2,1] <- d_dc2_gymno$estimate
  cor_gymno[2,2] <- d_d15_5_gymno$estimate
  cor_gymno[2,3] <- d_rc2_gymno$estimate
  cor_gymno[2,4] <- d_r15_5_gymno$estimate
  cor_gymno[2,5] <- d_v15_5_gymno$estimate
  
  cor_gymno[3,1] <- r_dc2_gymno$estimate
  cor_gymno[3,2] <- r_d15_5_gymno$estimate
  cor_gymno[3,3] <- r_rc2_gymno$estimate
  cor_gymno[3,4] <- r_r15_5_gymno$estimate
  cor_gymno[3,5] <- r_v15_5_gymno$estimate
  
  cor_gymno[4,1] <- r_dc2_gymno$estimate
  cor_gymno[4,2] <- r_d15_5_gymno$estimate
  cor_gymno[4,3] <- r_rc2_gymno$estimate
  cor_gymno[4,4] <- r_r15_5_gymno$estimate
  cor_gymno[4,5] <- r_v15_5_gymno$estimate
  
  
  p_gymno <- matrix(nrow = 4, ncol = 5)
  p_gymno[1,1] <- h_dc2_gymno$p.value
  p_gymno[1,2] <- h_d15_5_gymno$p.value
  p_gymno[1,3] <- h_rc2_gymno$p.value
  p_gymno[1,4] <- h_r15_5_gymno$p.value
  p_gymno[1,5] <- h_v15_5_gymno$p.value
  
  p_gymno[2,1] <- d_dc2_gymno$p.value
  p_gymno[2,2] <- d_d15_5_gymno$p.value
  p_gymno[2,3] <- d_rc2_gymno$p.value
  p_gymno[2,4] <- d_r15_5_gymno$p.value
  p_gymno[2,5] <- d_v15_5_gymno$p.value
  
  p_gymno[3,1] <- r_dc2_gymno$p.value
  p_gymno[3,2] <- r_d15_5_gymno$p.value
  p_gymno[3,3] <- r_rc2_gymno$p.value
  p_gymno[3,4] <- r_r15_5_gymno$p.value
  p_gymno[3,5] <- r_v15_5_gymno$p.value
  
  p_gymno[4,1] <- r_dc2_gymno$p.value
  p_gymno[4,2] <- r_d15_5_gymno$p.value
  p_gymno[4,3] <- r_rc2_gymno$p.value
  p_gymno[4,4] <- r_r15_5_gymno$p.value
  p_gymno[4,5] <- r_v15_5_gymno$p.value
  
  corrplot(cor_gymno, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Greens"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
}


crown_traits <- function() {
  
  ## loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(-X)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_dbh_nocomp_summary.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(-X)
  
  height_dbh <- read.csv(file = "output/height_dbh_summary.csv")
  height_dbh <- height_dbh %>% dplyr::select(-X)
  
  data <- left_join(diameter_dbh, ratio_dbh, by = "species")
  data <- left_join(data, height_max, by = "species")
  data <- left_join(data, height_dbh, by = "species")
  
  # adding sp traits
  traits <- read.csv(file = "output/sp_traits.csv")
  traits <- traits %>% dplyr::select(-X)
  

  # adding functional groups
  gp_info <- read.csv(file = "output/all_sp_groups.csv")
  gp_info <- gp_info %>% dplyr::select(sp, group)
  data_gp <- left_join(gp_info, data, by = c("sp" = "species"))
  data_gp <- left_join(data_gp, traits, by = c("sp" = "species"))

  
  angio <- data_gp %>% dplyr::filter(group %in% c("A", "C")) %>%
                       dplyr::mutate(volume_15 = 4/3 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * ((mean_h_15*mean_ratio_15)/2),
                                    volume_30 = 4/3 * pi * (mean_diameter_30/2)*(mean_diameter_30/2) * ((mean_h_30*mean_ratio_30)/2))
  
  gymno <- data_gp %>% dplyr::filter(group %in% c("B", "D")) %>%
                       dplyr::mutate(volume_15 = 1/2 * pi * (mean_diameter_15/2)*(mean_diameter_15/2) * (mean_h_15*mean_ratio_15),
                                      volume_30 = 1/2 * pi * (mean_diameter_30/2)*(mean_diameter_30/2) * (mean_h_30*mean_ratio_30))
  
  
  h_mat_angio <- cor.test(angio$mean_hmax, angio$mat, method = "pearson", na.rm = TRUE)
  h_mat_gymno <- cor.test(gymno$mean_hmax, gymno$mat, method = "pearson", na.rm = TRUE)
  
  h_tmin_angio <- cor.test(angio$mean_hmax, angio$tmin, method = "pearson", na.rm = TRUE)
  h_tmin_gymno <- cor.test(gymno$mean_hmax, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  h_map_angio <- cor.test(angio$mean_hmax, angio$map, method = "pearson", na.rm = TRUE)
  h_map_gymno <- cor.test(gymno$mean_hmax, gymno$map, method = "pearson", na.rm = TRUE)
  
  h_la_angio <- cor.test(angio$mean_hmax, angio$la, method = "pearson", na.rm = TRUE)
  h_la_gymno <- cor.test(gymno$mean_hmax, gymno$la, method = "pearson", na.rm = TRUE)
  
  h_sla_angio <- cor.test(angio$mean_hmax, angio$sla, method = "pearson", na.rm = TRUE)
  h_sla_gymno <- cor.test(gymno$mean_hmax, gymno$sla, method = "pearson", na.rm = TRUE)
  
  h_wd_angio <- cor.test(angio$mean_hmax, angio$ssd, method = "pearson", na.rm = TRUE)
  h_wd_gymno <- cor.test(gymno$mean_hmax, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  h_sh_angio <- cor.test(angio$mean_hmax, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  h_sh_gymno <- cor.test(gymno$mean_hmax, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  r_mat_angio <- cor.test(angio$mean_ratio_15, angio$mat, method = "pearson", na.rm = TRUE)
  r_mat_gymno <- cor.test(gymno$mean_ratio_15, gymno$mat, method = "pearson", na.rm = TRUE)
  
  r_tmin_angio <- cor.test(angio$mean_ratio_15, angio$tmin, method = "pearson", na.rm = TRUE)
  r_tmin_gymno <- cor.test(gymno$mean_ratio_15, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  r_map_angio <- cor.test(angio$mean_ratio_15, angio$map, method = "pearson", na.rm = TRUE)
  r_map_gymno <- cor.test(gymno$mean_ratio_15, gymno$map, method = "pearson", na.rm = TRUE)
  
  r_la_angio <- cor.test(angio$mean_ratio_15, angio$la, method = "pearson", na.rm = TRUE)
  r_la_gymno <- cor.test(gymno$mean_ratio_15, gymno$la, method = "pearson", na.rm = TRUE)
  
  r_sla_angio <- cor.test(angio$mean_ratio_15, angio$sla, method = "pearson", na.rm = TRUE)
  r_sla_gymno <- cor.test(gymno$mean_ratio_15, gymno$sla, method = "pearson", na.rm = TRUE)
  
  r_wd_angio <- cor.test(angio$mean_ratio_15, angio$ssd, method = "pearson", na.rm = TRUE)
  r_wd_gymno <- cor.test(gymno$mean_ratio_15, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  r_sh_angio <- cor.test(angio$mean_ratio_15, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  r_sh_gymno <- cor.test(gymno$mean_ratio_15, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  d_mat_angio <- cor.test(angio$mean_diameter_15, angio$mat, method = "pearson", na.rm = TRUE)
  d_mat_gymno <- cor.test(gymno$mean_diameter_15, gymno$mat, method = "pearson", na.rm = TRUE)

  d_tmin_angio <- cor.test(angio$mean_diameter_15, angio$tmin, method = "pearson", na.rm = TRUE)
  d_tmin_gymno <- cor.test(gymno$mean_diameter_15, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  d_map_angio <- cor.test(angio$mean_diameter_15, angio$map, method = "pearson", na.rm = TRUE)
  d_map_gymno <- cor.test(gymno$mean_diameter_15, gymno$map, method = "pearson", na.rm = TRUE)
  
  d_la_angio <- cor.test(angio$mean_diameter_15, angio$la, method = "pearson", na.rm = TRUE)
  d_la_gymno <- cor.test(gymno$mean_diameter_15, gymno$la, method = "pearson", na.rm = TRUE)
  
  d_sla_angio <- cor.test(angio$mean_diameter_15, angio$sla, method = "pearson", na.rm = TRUE)
  d_sla_gymno <- cor.test(gymno$mean_diameter_15, gymno$sla, method = "pearson", na.rm = TRUE)
  
  d_wd_angio <- cor.test(angio$mean_diameter_15, angio$ssd, method = "pearson", na.rm = TRUE)
  d_wd_gymno <- cor.test(gymno$mean_diameter_15, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  d_sh_angio <- cor.test(angio$mean_diameter_15, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  d_sh_gymno <- cor.test(gymno$mean_diameter_15, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  v_mat_angio <- cor.test(angio$volume_15, angio$mat, method = "pearson", na.rm = TRUE)
  v_mat_gymno <- cor.test(gymno$volume_15, gymno$mat, method = "pearson", na.rm = TRUE)
  
  v_tmin_angio <- cor.test(angio$volume_15, angio$tmin, method = "pearson", na.rm = TRUE)
  v_tmin_gymno <- cor.test(gymno$volume_15, gymno$tmin, method = "pearson", na.rm = TRUE)
  
  v_map_angio <- cor.test(angio$volume_15, angio$map, method = "pearson", na.rm = TRUE)
  v_map_gymno <- cor.test(gymno$volume_15, gymno$map, method = "pearson", na.rm = TRUE)
  
  v_la_angio <- cor.test(angio$volume_15, angio$la, method = "pearson", na.rm = TRUE)
  v_la_gymno <- cor.test(gymno$volume_15, gymno$la, method = "pearson", na.rm = TRUE)
  
  v_sla_angio <- cor.test(angio$volume_15, angio$sla, method = "pearson", na.rm = TRUE)
  v_sla_gymno <- cor.test(gymno$volume_15, gymno$sla, method = "pearson", na.rm = TRUE)
  
  v_wd_angio <- cor.test(angio$volume_15, angio$ssd, method = "pearson", na.rm = TRUE)
  v_wd_gymno <- cor.test(gymno$mean_diameter_15, gymno$ssd, method = "pearson", na.rm = TRUE)
  
  v_sh_angio <- cor.test(angio$volume_15, angio$shade_tol_mean, method = "pearson", na.rm = TRUE)
  v_sh_gymno <- cor.test(gymno$volume_15, gymno$shade_tol_mean, method = "pearson", na.rm = TRUE)
  
  
  
  
  cor_angio <- matrix(nrow = 4, ncol = 7)
  cor_gymno <- matrix(nrow = 4, ncol = 7)
  
  p_angio <- matrix(nrow = 4, ncol = 7)
  p_gymno <- matrix(nrow = 4, ncol = 7)
  
  cor_angio[1,1] <- h_mat_angio$estimate
  cor_angio[1,2] <- h_tmin_angio$estimate
  cor_angio[1,3] <- h_map_angio$estimate
  cor_angio[1,4] <- h_la_angio$estimate
  cor_angio[1,5] <- h_sla_angio$estimate
  cor_angio[1,6] <- h_wd_angio$estimate
  cor_angio[1,7] <- h_sh_angio$estimate
  
  cor_angio[2,1] <- r_mat_angio$estimate
  cor_angio[2,2] <- r_tmin_angio$estimate
  cor_angio[2,3] <- r_map_angio$estimate
  cor_angio[2,4] <- r_la_angio$estimate
  cor_angio[2,5] <- r_sla_angio$estimate
  cor_angio[2,6] <- r_wd_angio$estimate
  cor_angio[2,7] <- r_sh_angio$estimate
  
  cor_angio[3,1] <- d_mat_angio$estimate
  cor_angio[3,2] <- d_tmin_angio$estimate
  cor_angio[3,3] <- d_map_angio$estimate
  cor_angio[3,4] <- d_la_angio$estimate
  cor_angio[3,5] <- d_sla_angio$estimate
  cor_angio[3,6] <- d_wd_angio$estimate
  cor_angio[3,7] <- d_sh_angio$estimate
  
  cor_angio[4,1] <- v_mat_angio$estimate
  cor_angio[4,2] <- v_tmin_angio$estimate
  cor_angio[4,3] <- v_map_angio$estimate
  cor_angio[4,4] <- v_la_angio$estimate
  cor_angio[4,5] <- v_sla_angio$estimate
  cor_angio[4,6] <- v_wd_angio$estimate
  cor_angio[4,7] <- v_sh_angio$estimate
  
  
  p_angio[1,1] <- h_mat_angio$p.value
  p_angio[1,2] <- h_tmin_angio$p.value
  p_angio[1,3] <- h_map_angio$p.value
  p_angio[1,4] <- h_la_angio$p.value
  p_angio[1,5] <- h_sla_angio$p.value
  p_angio[1,6] <- h_wd_angio$p.value
  p_angio[1,7] <- h_sh_angio$p.value
  
  p_angio[2,1] <- r_mat_angio$p.value
  p_angio[2,2] <- r_tmin_angio$p.value
  p_angio[2,3] <- r_map_angio$p.value
  p_angio[2,4] <- r_la_angio$p.value
  p_angio[2,5] <- r_sla_angio$p.value
  p_angio[2,6] <- r_wd_angio$p.value
  p_angio[2,7] <- r_sh_angio$p.value
  
  p_angio[3,1] <- d_mat_angio$p.value
  p_angio[3,2] <- d_tmin_angio$p.value
  p_angio[3,3] <- d_map_angio$p.value
  p_angio[3,4] <- d_la_angio$p.value
  p_angio[3,5] <- d_sla_angio$p.value
  p_angio[3,6] <- d_wd_angio$p.value
  p_angio[3,7] <- d_sh_angio$p.value
  
  p_angio[4,1] <- v_mat_angio$p.value
  p_angio[4,2] <- v_tmin_angio$p.value
  p_angio[4,3] <- v_map_angio$p.value
  p_angio[4,4] <- v_la_angio$p.value
  p_angio[4,5] <- v_sla_angio$p.value
  p_angio[4,6] <- v_wd_angio$p.value
  p_angio[4,7] <- v_sh_angio$p.value
  
  
  
  cor_gymno[1,1] <- h_mat_gymno$estimate
  cor_gymno[1,2] <- h_tmin_gymno$estimate
  cor_gymno[1,3] <- h_map_gymno$estimate
  cor_gymno[1,4] <- h_la_gymno$estimate
  cor_gymno[1,5] <- h_sla_gymno$estimate
  cor_gymno[1,6] <- h_wd_gymno$estimate
  cor_gymno[1,7] <- h_sh_gymno$estimate
  
  cor_gymno[2,1] <- r_mat_gymno$estimate
  cor_gymno[2,2] <- r_tmin_gymno$estimate
  cor_gymno[2,3] <- r_map_gymno$estimate
  cor_gymno[2,4] <- r_la_gymno$estimate
  cor_gymno[2,5] <- r_sla_gymno$estimate
  cor_gymno[2,6] <- r_wd_gymno$estimate
  cor_gymno[2,7] <- r_sh_gymno$estimate
  
  cor_gymno[3,1] <- d_mat_gymno$estimate
  cor_gymno[3,2] <- d_tmin_gymno$estimate
  cor_gymno[3,3] <- d_map_gymno$estimate
  cor_gymno[3,4] <- d_la_gymno$estimate
  cor_gymno[3,5] <- d_sla_gymno$estimate
  cor_gymno[3,6] <- d_wd_gymno$estimate
  cor_gymno[3,7] <- d_sh_gymno$estimate
  
  cor_gymno[4,1] <- v_mat_gymno$estimate
  cor_gymno[4,2] <- v_tmin_gymno$estimate
  cor_gymno[4,3] <- v_map_gymno$estimate
  cor_gymno[4,4] <- v_la_gymno$estimate
  cor_gymno[4,5] <- v_sla_gymno$estimate
  cor_gymno[4,6] <- v_wd_gymno$estimate
  cor_gymno[4,7] <- v_sh_gymno$estimate
  
  
  p_gymno[1,1] <- h_mat_gymno$p.value
  p_gymno[1,2] <- h_tmin_gymno$p.value
  p_gymno[1,3] <- h_map_gymno$p.value
  p_gymno[1,4] <- h_la_gymno$p.value
  p_gymno[1,5] <- h_sla_gymno$p.value
  p_gymno[1,6] <- h_wd_gymno$p.value
  p_gymno[1,7] <- h_sh_gymno$p.value
  
  p_gymno[2,1] <- r_mat_gymno$p.value
  p_gymno[2,2] <- r_tmin_gymno$p.value
  p_gymno[2,3] <- r_map_gymno$p.value
  p_gymno[2,4] <- r_la_gymno$p.value
  p_gymno[2,5] <- r_sla_gymno$p.value
  p_gymno[2,6] <- r_wd_gymno$p.value
  p_gymno[2,7] <- r_sh_gymno$p.value
  
  p_gymno[3,1] <- d_mat_gymno$p.value
  p_gymno[3,2] <- d_tmin_gymno$p.value
  p_gymno[3,3] <- d_map_gymno$p.value
  p_gymno[3,4] <- d_la_gymno$p.value
  p_gymno[3,5] <- d_sla_gymno$p.value
  p_gymno[3,6] <- d_wd_gymno$p.value
  p_gymno[3,7] <- d_sh_gymno$p.value
  
  p_gymno[4,1] <- v_mat_gymno$p.value
  p_gymno[4,2] <- v_tmin_gymno$p.value
  p_gymno[4,3] <- v_map_gymno$p.value
  p_gymno[4,4] <- v_la_gymno$p.value
  p_gymno[4,5] <- v_sla_gymno$p.value
  p_gymno[4,6] <- v_wd_gymno$p.value
  p_gymno[4,7] <- v_sh_gymno$p.value
  
  par(mfrow = c(1,1))
  corrplot(cor_angio, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Oranges"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  corrplot(cor_gymno, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Greens"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  
}
  
  
  
  
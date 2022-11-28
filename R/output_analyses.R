require(RColorBrewer)
require(corrplot)

ecological_strategies <- function () {
  
  height_parameters <- read.csv(file = "output/height_asympt_parameters.csv", sep = ";")
  height_parameters <- height_parameters %>% dplyr::select(-X) %>%
                                             dplyr::select(species, mean_b1, sd_b1, mean_b2, sd_b2, mean_b3, sd_b3) %>%
                                             dplyr::rename(mean_b1_height = mean_b1,
                                                           sd_b1_height = sd_b1,
                                                           mean_b2_height = mean_b2,
                                                           sd_b2_height = sd_b2,
                                                           mean_b3_height = mean_b3,
                                                           sd_b3_height = sd_b3)
  #                                         
  # 
  # ratio_parameters <- read.csv(file = "output/ratio_beta_mean_estimates.csv")
  # ratio_parameters <- ratio_parameters %>% dplyr::select(-X) %>%
  #                                          dplyr::select(species, mean_a1, sd_a1) %>%
  #                                          dplyr::rename(mean_a1_ratio = mean_a1,
  #                                                        sd_a1_ratio = sd_a1)
  # 
  # 
  # diameter_parameters <- read.csv(file = "output/diameter_power_parameters.csv")
  # diameter_parameters <- diameter_parameters %>% dplyr::select(-X) %>%
  #                                                dplyr::select(species, mean_a1, sd_a1, mean_a2, sd_a2) %>%
  #                                                dplyr::rename(mean_a1_diameter = mean_a1,
  #                                                              sd_a1_diameter = sd_a1,
  #                                                              mean_a2_diameter = mean_a2,
  #                                                              sd_a2_diameter = sd_a2)
  # 
  # file_a <- left_join(diameter_parameters, ratio_parameters, by = "species")
  # file_b <- left_join(file_a, height_parameters, by = "species")
  # nocompetition_parameters <- file_b[!is.na(file_b$mean_a1_ratio),]
  # 
  # write.csv(file = "output/allsp_parameters_nocomp.csv", nocompetition_parameters)
  

  se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))

  # loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  depth_dbh <- read.csv(file = "output/depth_dbh_summary.csv")
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  ratio_dbh <- read.csv(file = "output/ratio_dbh_summary.csv")

  # loading file with all sp information
  all_species_file <- read.csv(file = "output/all_sp_groups.csv")

  # loading file with crown shape parameters
  crown_shape <- read.csv(file = "data/crown_shape_parameters.csv", sep = ";")
  crown_shape_b <- separate(crown_shape, species, into = c("genus", "sp"), sep = " ")
  shape_sp <- as.data.frame(crown_shape$species)
  colnames(shape_sp) <- "species"
  crown_shape_b <- cbind(shape_sp, crown_shape_b)

  # computing species list
  all_variables_outputs <- all_species_file %>% dplyr::select(sp, group)

  sp_list_final <- as.data.frame(c(height_max$species, depth_dbh$species, diameter_dbh$species, ratio_dbh$species))
  names(sp_list_final) <- "species"
  dupli <- as.data.frame(duplicated(sp_list_final$species))
  names(dupli) <- "dupli"
  sp_list_final <- cbind(sp_list_final, dupli)
  sp_list_final <- sp_list_final %>% filter(dupli == FALSE)
  sp_list_final <- sp_list_final$species

  all_variables_outputs <- all_variables_outputs[all_variables_outputs$sp %in% sp_list_final,]

  # combining species list with crown volume parameters
  crown_shape_c <- crown_shape_b %>% dplyr::select(species, genus, sp, mean_shape)

  all_variables_outputs_b <- separate(all_variables_outputs, sp, into = c("genus", "sp"), sep = " ")
  species <- as.data.frame(all_variables_outputs$sp)
  colnames(species) <- "species"
  all_variables_outputs_b <- cbind(species, all_variables_outputs_b)

  combined_file_a <- left_join(all_variables_outputs_b, crown_shape_c, by = "species")
  combined_file_a <- combined_file_a %>% dplyr::rename(genus = genus.x, sp = sp.x) %>%
                                         dplyr::select(species, genus, sp, group, mean_shape)

  crown_shape_d <- crown_shape_c[is.na(crown_shape_c$sp),]

  combined_file_b <- combined_file_a %>% dplyr::select(genus)
  combined_file_b <- left_join(combined_file_b, crown_shape_d, by = "genus")
  combined_file_b <- combined_file_b %>% dplyr::select(mean_shape) %>%
                                         dplyr::rename(mean_shape_b = mean_shape)

  combined_file_c <- cbind(combined_file_a, combined_file_b)

  mean_shape_c <- as.data.frame(coalesce(combined_file_a$mean_shape, combined_file_b$mean_shape_b))
  colnames(mean_shape_c) <- "mean_shape_f"

  combined_file_a <- cbind(combined_file_a, mean_shape_c)
  combined_file_a <- combined_file_a %>% dplyr::select(-mean_shape)

  crown_shape_e <- crown_shape_b[!is.na(crown_shape_b$sp),]
  groups <- all_species_file %>% dplyr::select(sp, group)
  crown_shape_e <- left_join(crown_shape_e, groups, by = c("species" = "sp"))
  
  crown_shape_e <- crown_shape_e[!is.na(crown_shape_e$group),]

  crown_shape_f <- crown_shape_e %>% dplyr::filter(group %in% c("A", "C")) %>%
                                     dplyr::mutate(new_gp = "angio")

  crown_shape_g <- crown_shape_e %>% dplyr::filter(group %in% c("B", "D")) %>%
                                     dplyr::mutate(new_gp = "gymno")

  mean(crown_shape_f$mean_shape) # 0.5168182
  mean(crown_shape_g$mean_shape) # 0.3146957

  groups <- as.data.frame(c("A", "B", "C", "D"))
  shape <- as.data.frame(c(0.5168182, 0.3146957, 0.5168182, 0.3146957))
  colnames(groups) <- "groups"
  colnames(shape) <- "shape"

  mean_shapes <- cbind(groups, shape)

  combined_file_d <- combined_file_a[is.na(combined_file_a$mean_shape_f),]
  combined_file_d <- combined_file_d %>% dplyr::select(species, group)
  combined_file_d <- left_join(combined_file_d, mean_shapes, by = c("group" = "groups"))

  combined_file_a <- left_join(combined_file_a, combined_file_d, by = "species")

  complete_shapes <- as.data.frame(coalesce(combined_file_a$mean_shape_f, combined_file_a$shape))
  colnames(complete_shapes) <- "sp_shape"

  combined_file_a <- combined_file_a %>% dplyr::select(species, group.x) %>%
                                         dplyr::rename(group = group.x)

  combined_file_a <- cbind(combined_file_a, complete_shapes)
  combined_file_z <- left_join(combined_file_a, mean_shapes, by = c("group" = "groups"))

  write.csv(file = "crown_parameters_mean_sp.csv", combined_file_z)


  # combining all files
  height_max <- height_max %>% dplyr::select(-X)
  depth_dbh <- depth_dbh %>% dplyr::select(-X)
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  ratio_dbh <- ratio_dbh %>% dplyr::select(-X)

  all_data_volume <- left_join(combined_file_z, height_max, by = "species")
  all_data_volume <- left_join(all_data_volume, depth_dbh, by = "species")
  all_data_volume <- left_join(all_data_volume, diameter_dbh, by = "species")
  all_data_volume <- left_join(all_data_volume, ratio_dbh, by = "species")
  all_data_volume <- all_data_volume %>% dplyr::select(-cv)
  
  write.csv(file = "all_parameters_nocomp.csv", all_data_volume)


  # computing crown volume
  volume_sp <- all_data_volume[!is.na(all_data_volume$mean_depth) & !is.na(all_data_volume$mean_diameter),]$species
  volume_variables_outputs <- all_data_volume[all_data_volume$species %in% volume_sp,]


  for (i in 1:dim(volume_variables_outputs)[1]) {

    n_repetition = 100

    volume_dbh_sp <- as.data.frame(matrix(nrow = n_repetition, ncol = 2))
    volume_dbh_sp[,1] <- rep(volume_variables_outputs[i,"species"], n_repetition)
    names(volume_dbh_sp) <- c("species", "volume")

    volume_dbh_mean <- as.data.frame(matrix(nrow = n_repetition, ncol = 2))
    volume_dbh_mean[,1] <- rep(volume_variables_outputs[i,"species"], n_repetition)
    names(volume_dbh_mean) <- c("species", "volume")

    mean_depth = volume_variables_outputs[i,"mean_depth"]
    sd_depth = volume_variables_outputs[i,"sd_depth"]

    mean_diam = volume_variables_outputs[i,"mean_diameter"]
    sd_diam = volume_variables_outputs[i,"sd_diameter"]

    shape_sp = volume_variables_outputs[i, "sp_shape"]
    shape_mean = volume_variables_outputs[i, "shape"]

    for (j in 1:n_repetition) {

      depth <- rnorm(1, mean = mean_depth, sd = sd_depth)
      diam <- rnorm(1, mean = mean_diam, sd = sd_diam)
      sp_shape <- shape_sp
      mean_shape <- shape_mean

        volume_dbh_sp[j,"volume"] = (pi * (diam/2)^2 * depth)/(2 * sp_shape + 1)
        volume_dbh_mean[j,"volume"] = (pi * (diam/2)^2 * depth)/(2 * mean_shape + 1)

    }

    write.csv(volume_dbh_sp, file =  paste0("output/volume_dbh_sp_", volume_variables_outputs[i,"species"], ".csv"))
    write.csv(volume_dbh_mean, file =  paste0("output/volume_dbh_mean_", volume_variables_outputs[i,"species"], ".csv"))

  }


  dd <- list.files(path = "output/", pattern = "volume_dbh_sp_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)

  volume_shape_sp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  volume_shape_sp <- volume_shape_sp %>% dplyr::group_by(species) %>%
                                         dplyr::summarise(mean_sp = mean(volume),
                                                          sd_sp = sd(volume),
                                                          se_sp = se(volume)) %>%
                                         dplyr::select(species, mean_sp, sd_sp, se_sp) %>%
                                         dplyr::ungroup()



  dd <- list.files(path = "output/", pattern = "volume_dbh_mean_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)

  volume_shape_mean <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  volume_shape_mean <- volume_shape_mean %>% dplyr::group_by(species) %>%
                                             dplyr::summarise(mean_mean = mean(volume),
                                                              sd_mean = sd(volume),
                                                              se_mean = se(volume)) %>%
                                             dplyr::select(species, mean_mean, sd_mean, se_mean) %>%
                                             dplyr::ungroup()

  volume_estimates <- left_join(volume_shape_sp, volume_shape_mean, by = "species")
  volume_estimates <- arrange(volume_estimates, volume_estimates$mean_sp)
  ranking_sp <- as.data.frame(c(1:dim(volume_estimates)[1]))
  names(ranking_sp) <- "ranking_sp"
  volume_estimates <- cbind(volume_estimates, ranking_sp)

  volume_estimates <- arrange(volume_estimates, volume_estimates$mean_mean)
  ranking_mean <- as.data.frame(c(1:dim(volume_estimates)[1]))
  names(ranking_mean) <- "ranking_mean"
  volume_estimates <- cbind(volume_estimates, ranking_mean)

  write.csv(file = "output/volume_estimates.csv", volume_estimates)

  # # plot for georges
  # volume_estimates_georges <- volume_estimates %>% filter(mean_sp != mean_mean)
  # volume_estimates_georges <- volume_estimates_georges %>% dplyr::select(-ranking_sp, -ranking_mean)
  # volume_estimates_georges <- arrange(volume_estimates_georges, volume_estimates_georges$mean_sp)
  # ranking_sp <- as.data.frame(c(1:dim(volume_estimates_georges)[1]))
  # names(ranking_sp) <- "ranking_sp"
  # volume_estimates_georges <- cbind(volume_estimates_georges, ranking_sp)
  #
  # volume_estimates_georges <- arrange(volume_estimates_georges, volume_estimates_georges$mean_mean)
  # ranking_mean <- as.data.frame(c(1:dim(volume_estimates_georges)[1]))
  # names(ranking_mean) <- "ranking_mean"
  # volume_estimates_georges <- bind_cols(volume_estimates_georges, ranking_mean)
  #
  # write.csv(file = "output/volume_estimates_georges.csv", volume_estimates_georges)
  #
  # par(mfrow = c(1,2))
  # plot(volume_estimates_georges$mean_sp, volume_estimates_georges$mean_mean, type = "p", pch = 16,
  #      xlab = "crown shape per sp", ylab = "crown shape per group", las = 1)
  #
  # plot(volume_estimates_georges$ranking_sp, volume_estimates_georges$ranking_mean, type = "p", pch = 16,
  #      xlab = "rankg - crown shape per sp", ylab = "ranking - crown shape per group", las = 1)
  
  
  # running PCA on all dimensions and for both functional groups
  pca_file <- volume_estimates %>% dplyr::select(species, mean_mean) %>%
                                   dplyr::rename(mean_volume = mean_mean)
  
  new_f_group_a <- all_species_file %>% dplyr::select(sp, group) %>%
                                        dplyr::filter(group %in% c("A", "C")) %>%
                                        dplyr::mutate(new_gp = "angio")
  
  new_f_group_b <- all_species_file %>% dplyr::select(sp, group) %>%
                                        dplyr::filter(group %in% c("B", "D")) %>%
                                        dplyr::mutate(new_gp = "gymno")
  
  new_f_group <- rbind(new_f_group_a, new_f_group_b)
  write.csv(file = "data/functional_groups.csv", new_f_group)
  
  pca_file <- left_join(pca_file, new_f_group, by = c("species" = "sp"))
  pca_file <- left_join(pca_file, height_max, by = "species")
  pca_file <- left_join(pca_file, diameter_dbh, by = "species")
  pca_file <- left_join(pca_file, ratio_dbh, by = "species")
  
  pca_file <- pca_file %>% dplyr::select(species, new_gp, mean_volume, 
                                         mean_hmax, mean_diameter, mean_ratio) %>%
                           dplyr::rename(volume = mean_volume, diameter = mean_diameter,
                                         ratio = mean_ratio)
  
  pca_file <- pca_file[!is.na(pca_file$mean_hmax),]
  
  row.names(pca_file) <- pca_file$species
  
  angio <- pca_file %>% dplyr::filter(new_gp == "angio") %>%
                        dplyr::select(-species, -new_gp)
  
  gymno <- pca_file %>% dplyr::filter(new_gp == "gymno") %>%
                        dplyr::select(-species, -new_gp)
  
  
  res_pca_full <- pca_file %>% dplyr::select(-species, -new_gp)
  res_pca_full <- prcomp(res_pca_full, scale = TRUE, center = TRUE)
  
  res_pca_angio <- prcomp(angio, scale = TRUE, center = TRUE)
  res_pca_gymno <- prcomp(gymno, scale = TRUE, center = TRUE)
  
  fviz_eig(res_pca_full, addlabels = TRUE) # axis 1: 43.1% / axis 2: 25.8%
  fviz_eig(res_pca_angio, addlabels = TRUE) # axis 1: 48.7% / axis 2: 29.8%
  fviz_eig(res_pca_gymno, addlabels = TRUE) # axis 1: 48.1% / axis 2: 28.2%
  
  par(mfrow = c(1,2))
  
  fviz_pca_var(res_pca_angio,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "Angiosperms",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "all")     
  
  fviz_pca_var(res_pca_gymno,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "Gymnosperms",
               ggtheme = theme_minimal(),
               lty = 4,
               repel = TRUE, 
               label = "all")     
  
  fviz_pca_var(res_pca_full,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "All species",
               ggtheme = theme_minimal(),
               lty = 4,
               repel = TRUE, 
               label = "none")    
  
  ## running Pearson correlations on pairs of dimensions 
  
  # height - ratio
  height_ratio <- left_join(height_max, ratio_dbh, by = "species")
  height_ratio <- height_ratio[!is.na(height_ratio$mean_ratio),] # 110 sp
  height_ratio <- left_join(height_ratio, new_f_group, by = c("species" = "sp"))
  height_ratio_angio <- height_ratio[height_ratio$new_gp == "angio",]
  height_ratio_gymno <- height_ratio[height_ratio$new_gp == "gymno",]
  
  h_cr_all <- cor.test(height_ratio$mean_hmax, height_ratio$mean_ratio, method = "pearson")
  h_cr_angio <- cor.test(height_ratio_angio$mean_hmax, height_ratio_angio$mean_ratio, method = "pearson")
  h_cr_gymno <- cor.test(height_ratio_gymno$mean_hmax, height_ratio_gymno$mean_ratio, method = "pearson")
  
  # height - diameter
  height_diameter <- left_join(height_max, diameter_dbh, by = "species")
  height_diameter <- height_diameter[!is.na(height_diameter$mean_diameter),] # 58 sp
  height_diameter <- left_join(height_diameter, new_f_group, by = c("species" = "sp"))
  height_diameter_angio <- height_diameter[height_diameter$new_gp == "angio",]
  height_diameter_gymno <- height_diameter[height_diameter$new_gp == "gymno",]
  
  h_cd_all <- cor.test(height_diameter$mean_hmax, height_diameter$mean_diameter, method = "pearson")
  h_cd_angio <- cor.test(height_diameter_angio$mean_hmax, height_diameter_angio$mean_diameter, method = "pearson")
  h_cd_gymno <- cor.test(height_diameter_gymno$mean_hmax, height_diameter_gymno$mean_diameter, method = "pearson")
  
  
  # height - volume
  height_volume <- left_join(height_max, volume_estimates, by = "species")
  height_volume <- height_volume[!is.na(height_volume$mean_mean),] # 46 sp
  height_volume <- left_join(height_volume, new_f_group, by = c("species" = "sp"))
  height_volume_angio <- height_volume[height_volume$new_gp == "angio",]
  height_volume_gymno <- height_volume[height_volume$new_gp == "gymno",]
  
  h_cv_all <- cor.test(height_volume$mean_hmax, height_volume$mean_mean, method = "pearson")
  h_cv_angio <- cor.test(height_volume_angio$mean_hmax, height_volume_angio$mean_mean, method = "pearson")
  h_cv_gymno <- cor.test(height_volume_gymno$mean_hmax, height_volume_gymno$mean_mean, method = "pearson")
  

  # ratio - diameter
  ratio_diameter <- left_join(ratio_dbh, diameter_dbh, by = "species")
  ratio_diameter <- ratio_diameter[!is.na(ratio_diameter$mean_diameter),] # 52 sp
  ratio_diameter <- left_join(ratio_diameter, new_f_group, by = c("species" = "sp"))
  ratio_diameter_angio <- ratio_diameter[ratio_diameter$new_gp == "angio",]
  ratio_diameter_gymno <- ratio_diameter[ratio_diameter$new_gp == "gymno",]
  
  cr_cd_all <- cor.test(ratio_diameter$mean_ratio, ratio_diameter$mean_diameter, method = "pearson")
  cr_cd_angio <- cor.test(ratio_diameter_angio$mean_ratio, ratio_diameter_angio$mean_diameter, method = "pearson")
  cr_cd_gymno <- cor.test(ratio_diameter_gymno$mean_ratio, ratio_diameter_gymno$mean_diameter, method = "pearson")
  
  
  # ratio - volume
  ratio_volume <- left_join(ratio_dbh, volume_estimates, by = "species")
  ratio_volume <- ratio_volume[!is.na(ratio_volume$mean_mean),] # 47 sp
  ratio_volume <- left_join(ratio_volume, new_f_group, by = c("species" = "sp"))
  ratio_volume_angio <- ratio_volume[ratio_volume$new_gp == "angio",]
  ratio_volume_gymno <- ratio_volume[ratio_volume$new_gp == "gymno",]
  
  cr_cv_all <- cor.test(ratio_volume$mean_ratio, ratio_volume$mean_mean, method = "pearson")
  cr_cv_angio <- cor.test(ratio_volume_angio$mean_ratio, ratio_volume_angio$mean_mean, method = "pearson")
  cr_cv_gymno <- cor.test(ratio_volume_gymno$mean_ratio, ratio_volume_gymno$mean_mean, method = "pearson")
  
  
  # diameter - volume
  diameter_volume <- left_join(diameter_dbh, volume_estimates, by = "species")
  diameter_volume <- diameter_volume[!is.na(diameter_volume$mean_mean),] # 47 sp
  diameter_volume <- left_join(diameter_volume, new_f_group, by = c("species" = "sp"))
  diameter_volume_angio <- diameter_volume[diameter_volume$new_gp == "angio",]
  diameter_volume_gymno <- diameter_volume[diameter_volume$new_gp == "gymno",]
  
  cd_cv_all <- cor.test(diameter_volume$mean_diameter, diameter_volume$mean_mean, method = "pearson")
  cd_cv_angio <- cor.test(diameter_volume_angio$mean_diameter, diameter_volume_angio$mean_mean, method = "pearson")
  cd_cv_gymno <- cor.test(diameter_volume_gymno$mean_diameter, diameter_volume_gymno$mean_mean, method = "pearson")
  
  par(mfrow = c(1,2))
  
  cor_angio <- matrix(nrow = 4, ncol = 4)
  cor_angio[1,1] <- 0
  cor_angio[1,2] <- h_cr_angio$estimate
  cor_angio[1,3] <- h_cd_angio$estimate
  cor_angio[1,4] <- h_cv_angio$estimate
  
  cor_angio[2,1] <- 0
  cor_angio[2,2] <- 0
  cor_angio[2,3] <- cr_cd_angio$estimate
  cor_angio[2,4] <- cr_cv_angio$estimate
  
  cor_angio[3,1] <- 0
  cor_angio[3,2] <- 0
  cor_angio[3,3] <- 0
  cor_angio[3,4] <- cd_cv_angio$estimate
  
  cor_angio[4,1] <- 0
  cor_angio[4,2] <- 0
  cor_angio[4,3] <- 0
  cor_angio[4,4] <- 0
  
  
  p_angio <- matrix(nrow = 4, ncol = 4)
  p_angio[1,1] <- 0
  p_angio[1,2] <- h_cr_angio$p.value
  p_angio[1,3] <- h_cd_angio$p.value
  p_angio[1,4] <- h_cv_angio$p.value
  
  p_angio[2,1] <- 0
  p_angio[2,2] <- 0
  p_angio[2,3] <- cr_cd_angio$p.value
  p_angio[2,4] <- cr_cv_angio$p.value
  
  p_angio[3,1] <- 0
  p_angio[3,2] <- 0
  p_angio[3,3] <- 0
  p_angio[3,4] <- cd_cv_angio$p.value
  
  p_angio[4,1] <- 0
  p_angio[4,2] <- 0
  p_angio[4,3] <- 0
  p_angio[4,4] <- 0
  

  corrplot(cor_angio, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Oranges"), addCoef.col = "black", cl.pos = "n",
           diag = FALSE)
  
  
  cor_gymno <- matrix(nrow = 4, ncol = 4)
  cor_gymno[1,1] <- 0
  cor_gymno[1,2] <- 0
  cor_gymno[1,3] <- 0
  cor_gymno[1,4] <- 0
  
  cor_gymno[2,1] <- h_cr_gymno$estimate
  cor_gymno[2,2] <- 0
  cor_gymno[2,3] <- 0
  cor_gymno[2,4] <- 0
  
  cor_gymno[3,1] <- h_cd_gymno$estimate
  cor_gymno[3,2] <- cr_cd_gymno$estimate
  cor_gymno[3,3] <- 0
  cor_gymno[3,4] <- 0
  
  cor_gymno[4,1] <- h_cv_gymno$estimate
  cor_gymno[4,2] <- cr_cv_gymno$estimate
  cor_gymno[4,3] <- cd_cv_gymno$estimate
  cor_gymno[4,4] <- 0
  
  
  p_gymno <- matrix(nrow = 4, ncol = 4)
  p_gymno[1,1] <- 0
  p_gymno[1,2] <- 0
  p_gymno[1,3] <- 0
  p_gymno[1,4] <- 0
  
  p_gymno[2,1] <- h_cr_gymno$p.value
  p_gymno[2,2] <- 0
  p_gymno[2,3] <- 0
  p_gymno[2,4] <- 0
  
  p_gymno[3,1] <- h_cd_gymno$p.value
  p_gymno[3,2] <- cr_cd_gymno$p.value
  p_gymno[3,3] <- 0
  p_gymno[3,4] <- 0
  
  p_gymno[4,1] <- h_cv_gymno$p.value
  p_gymno[4,2] <- cr_cv_gymno$p.value
  p_gymno[4,3] <- cd_cv_gymno$p.value
  p_gymno[4,4] <- 0
  
  
  corrplot(cor_gymno, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Greens"), addCoef.col = "black", cl.pos = "n",
           diag = FALSE)
  
  
}


crown_traits <- function () {
  
  # loading files with all data
  complete_file <- read.csv(file = "output/complete_nocomp_file.csv", sep = ",")
  complete_file <- complete_file %>% dplyr::select(-X)
  
  # height - mean annual temperature
  height_mat <- complete_file[!is.na(complete_file$mean_hmax & complete_file$mat),] # 125 sp
  height_mat_angio <- height_mat[height_mat$new_gp == "angio",]
  height_mat_gymno <- height_mat[height_mat$new_gp == "gymno",]
  
  h_mat_angio <- cor.test(height_mat_angio$mean_hmax, height_mat_angio$mat, method = "pearson")
  h_mat_gymno <- cor.test(height_mat_gymno$mean_hmax, height_mat_gymno$mat, method = "pearson")
  
  
  # height - min annual temperature
  height_tmin <- complete_file[!is.na(complete_file$mean_hmax & complete_file$tmin),] # 125 sp
  height_tmin_angio <- height_mat[height_tmin$new_gp == "angio",]
  height_tmin_gymno <- height_mat[height_tmin$new_gp == "gymno",]
  
  h_tmin_angio <- cor.test(height_tmin_angio$mean_hmax, height_tmin_angio$tmin, method = "pearson")
  h_tmin_gymno <- cor.test(height_tmin_gymno$mean_hmax, height_tmin_gymno$tmin, method = "pearson")
  
  
  # height - map annual temperature
  height_map <- complete_file[!is.na(complete_file$mean_hmax & complete_file$map),] # 125 sp
  height_map_angio <- height_mat[height_map$new_gp == "angio",]
  height_map_gymno <- height_mat[height_map$new_gp == "gymno",]
  
  h_map_angio <- cor.test(height_map_angio$mean_hmax, height_map_angio$map, method = "pearson")
  h_map_gymno <- cor.test(height_map_gymno$mean_hmax, height_map_gymno$map, method = "pearson")
  
  
  # height - leaf area
  height_la <- complete_file[!is.na(complete_file$mean_hmax & complete_file$la),] # 114 sp
  height_la_angio <- height_la[height_la$new_gp == "angio",]
  height_la_gymno <- height_la[height_la$new_gp == "gymno",]
  
  h_la_angio <- cor.test(height_la_angio$mean_hmax, height_la_angio$la, method = "pearson")
  h_la_gymno <- cor.test(height_la_gymno$mean_hmax, height_la_gymno$la, method = "pearson")
  
  
  # height - specific leaf area
  height_sla <- complete_file[!is.na(complete_file$mean_hmax & complete_file$sla),] # 116 sp
  height_sla_angio <- height_sla[height_sla$new_gp == "angio",]
  height_sla_gymno <- height_sla[height_sla$new_gp == "gymno",]
  
  h_sla_angio <- cor.test(height_sla_angio$mean_hmax, height_sla_angio$sla, method = "pearson")
  h_sla_gymno <- cor.test(height_sla_gymno$mean_hmax, height_sla_gymno$sla, method = "pearson")
  
  
  # height - seed mass
  height_sm <- complete_file[!is.na(complete_file$mean_hmax & complete_file$sm),] # 125 sp
  height_sm_angio <- height_sla[height_sm$new_gp == "angio",]
  height_sm_gymno <- height_sla[height_sm$new_gp == "gymno",]
  
  h_sm_angio <- cor.test(height_sm_angio$mean_hmax, height_sm_angio$sm, method = "pearson")
  h_sm_gymno <- cor.test(height_sm_gymno$mean_hmax, height_sm_gymno$sm, method = "pearson")
  
  
  # height - wood density
  height_wd <- complete_file[!is.na(complete_file$mean_hmax & complete_file$ssd),] # 123 sp
  height_wd_angio <- height_wd[height_wd$new_gp == "angio",]
  height_wd_gymno <- height_wd[height_wd$new_gp == "gymno",]
  
  h_wd_angio <- cor.test(height_wd_angio$mean_hmax, height_wd_angio$ssd, method = "pearson")
  h_wd_gymno <- cor.test(height_wd_gymno$mean_hmax, height_wd_gymno$ssd, method = "pearson")
  
  
  # height - shade tolerance
  height_sh <- complete_file[!is.na(complete_file$mean_hmax & complete_file$shade_tol_mean),] # 123 sp
  height_sh_angio <- height_sh[height_sh$new_gp == "angio",]
  height_sh_gymno <- height_sh[height_sh$new_gp == "gymno",]
  
  h_sh_angio <- cor.test(height_sh_angio$mean_hmax, height_sh_angio$shade_tol_mean, method = "pearson")
  h_sh_gymno <- cor.test(height_sh_gymno$mean_hmax, height_sh_gymno$shade_tol_mean, method = "pearson")
  
  
  
  # ratio - mean annual temperature
  ratio_mat <- complete_file[!is.na(complete_file$mean_ratio & complete_file$mat),] # 130 sp
  ratio_mat_angio <- ratio_mat[ratio_mat$new_gp == "angio",]
  ratio_mat_gymno <- ratio_mat[ratio_mat$new_gp == "gymno",]
  
  ratio_mat_angio <- cor.test(ratio_mat_angio$mean_ratio, ratio_mat_angio$mat, method = "pearson")
  ratio_mat_gymno <- cor.test(ratio_mat_gymno$mean_ratio, ratio_mat_gymno$mat, method = "pearson")
  
  
  # ratio - min annual temperature
  ratio_tmin <- complete_file[!is.na(complete_file$mean_ratio & complete_file$tmin),] # 130 sp
  ratio_tmin_angio <- ratio_mat[ratio_tmin$new_gp == "angio",]
  ratio_tmin_gymno <- ratio_mat[ratio_tmin$new_gp == "gymno",]
  
  ratio_tmin_angio <- cor.test(ratio_tmin_angio$mean_ratio, ratio_tmin_angio$tmin, method = "pearson")
  ratio_tmin_gymno <- cor.test(ratio_tmin_gymno$mean_ratio, ratio_tmin_gymno$tmin, method = "pearson")
  
  
  # ratio - map annual temperature
  ratio_map <- complete_file[!is.na(complete_file$mean_ratio & complete_file$map),] # 130 sp
  ratio_map_angio <- ratio_mat[ratio_map$new_gp == "angio",]
  ratio_map_gymno <- ratio_mat[ratio_map$new_gp == "gymno",]
  
  ratio_map_angio <- cor.test(ratio_map_angio$mean_ratio, ratio_map_angio$map, method = "pearson")
  ratio_map_gymno <- cor.test(ratio_map_gymno$mean_ratio, ratio_map_gymno$map, method = "pearson")
  
  
  # ratio - leaf area
  ratio_la <- complete_file[!is.na(complete_file$mean_ratio & complete_file$la),] # 118 sp
  ratio_la_angio <- ratio_la[ratio_la$new_gp == "angio",]
  ratio_la_gymno <- ratio_la[ratio_la$new_gp == "gymno",]
  
  ratio_la_angio <- cor.test(ratio_la_angio$mean_ratio, ratio_la_angio$la, method = "pearson")
  ratio_la_gymno <- cor.test(ratio_la_gymno$mean_ratio, ratio_la_gymno$la, method = "pearson")
  
  
  # ratio - specific leaf area
  ratio_sla <- complete_file[!is.na(complete_file$mean_ratio & complete_file$sla),] # 118 sp
  ratio_sla_angio <- ratio_sla[ratio_sla$new_gp == "angio",]
  ratio_sla_gymno <- ratio_sla[ratio_sla$new_gp == "gymno",]
  
  ratio_sla_angio <- cor.test(ratio_sla_angio$mean_ratio, ratio_sla_angio$sla, method = "pearson")
  ratio_sla_gymno <- cor.test(ratio_sla_gymno$mean_ratio, ratio_sla_gymno$sla, method = "pearson")
  
  
  # ratio - seed mass
  ratio_sm <- complete_file[!is.na(complete_file$mean_ratio & complete_file$sm),] # 130 sp
  ratio_sm_angio <- ratio_sla[ratio_sm$new_gp == "angio",]
  ratio_sm_gymno <- ratio_sla[ratio_sm$new_gp == "gymno",]
  
  ratio_sm_angio <- cor.test(ratio_sm_angio$mean_ratio, ratio_sm_angio$sm, method = "pearson")
  ratio_sm_gymno <- cor.test(ratio_sm_gymno$mean_ratio, ratio_sm_gymno$sm, method = "pearson")
  
  
  # ratio - wood density
  ratio_wd <- complete_file[!is.na(complete_file$mean_ratio & complete_file$ssd),] # 127 sp
  ratio_wd_angio <- ratio_wd[ratio_wd$new_gp == "angio",]
  ratio_wd_gymno <- ratio_wd[ratio_wd$new_gp == "gymno",]
  
  ratio_wd_angio <- cor.test(ratio_wd_angio$mean_ratio, ratio_wd_angio$ssd, method = "pearson")
  ratio_wd_gymno <- cor.test(ratio_wd_gymno$mean_ratio, ratio_wd_gymno$ssd, method = "pearson")
  
  
  # ratio - shade tolerance
  ratio_sh <- complete_file[!is.na(complete_file$mean_ratio & complete_file$shade_tol_mean),] # 119 sp
  ratio_sh_angio <- ratio_sh[ratio_sh$new_gp == "angio",]
  ratio_sh_gymno <- ratio_sh[ratio_sh$new_gp == "gymno",]
  
  ratio_sh_angio <- cor.test(ratio_sh_angio$mean_ratio, ratio_sh_angio$shade_tol_mean, method = "pearson")
  ratio_sh_gymno <- cor.test(ratio_sh_gymno$mean_ratio, ratio_sh_gymno$shade_tol_mean, method = "pearson")
  
  
  
  # diameter - mean annual temperature
  diameter_mat <- complete_file[!is.na(complete_file$mean_diameter & complete_file$mat),] # 61 sp
  diameter_mat_angio <- diameter_mat[diameter_mat$new_gp == "angio",]
  diameter_mat_gymno <- diameter_mat[diameter_mat$new_gp == "gymno",]
  
  diameter_mat_angio <- cor.test(diameter_mat_angio$mean_diameter, diameter_mat_angio$mat, method = "pearson")
  diameter_mat_gymno <- cor.test(diameter_mat_gymno$mean_diameter, diameter_mat_gymno$mat, method = "pearson")
  
  
  # diameter - min annual temperature
  diameter_tmin <- complete_file[!is.na(complete_file$mean_diameter & complete_file$tmin),] # 61 sp
  diameter_tmin_angio <- diameter_mat[diameter_tmin$new_gp == "angio",]
  diameter_tmin_gymno <- diameter_mat[diameter_tmin$new_gp == "gymno",]
  
  diameter_tmin_angio <- cor.test(diameter_tmin_angio$mean_diameter, diameter_tmin_angio$tmin, method = "pearson")
  diameter_tmin_gymno <- cor.test(diameter_tmin_gymno$mean_diameter, diameter_tmin_gymno$tmin, method = "pearson")
  
  
  # diameter - map annual temperature
  diameter_map <- complete_file[!is.na(complete_file$mean_diameter & complete_file$map),] # 61 sp
  diameter_map_angio <- diameter_mat[diameter_map$new_gp == "angio",]
  diameter_map_gymno <- diameter_mat[diameter_map$new_gp == "gymno",]
  
  diameter_map_angio <- cor.test(diameter_map_angio$mean_diameter, diameter_map_angio$map, method = "pearson")
  diameter_map_gymno <- cor.test(diameter_map_gymno$mean_diameter, diameter_map_gymno$map, method = "pearson")
  
  
  # diameter - leaf area
  diameter_la <- complete_file[!is.na(complete_file$mean_diameter & complete_file$la),] # 60 sp
  diameter_la_angio <- diameter_la[diameter_la$new_gp == "angio",]
  diameter_la_gymno <- diameter_la[diameter_la$new_gp == "gymno",]
  
  diameter_la_angio <- cor.test(diameter_la_angio$mean_diameter, diameter_la_angio$la, method = "pearson")
  diameter_la_gymno <- cor.test(diameter_la_gymno$mean_diameter, diameter_la_gymno$la, method = "pearson")
  
  
  # diameter - specific leaf area
  diameter_sla <- complete_file[!is.na(complete_file$mean_diameter & complete_file$sla),] # 60 sp
  diameter_sla_angio <- diameter_sla[diameter_sla$new_gp == "angio",]
  diameter_sla_gymno <- diameter_sla[diameter_sla$new_gp == "gymno",]
  
  diameter_sla_angio <- cor.test(diameter_sla_angio$mean_diameter, diameter_sla_angio$sla, method = "pearson")
  diameter_sla_gymno <- cor.test(diameter_sla_gymno$mean_diameter, diameter_sla_gymno$sla, method = "pearson")
  
  
  # diameter - seed mass
  diameter_sm <- complete_file[!is.na(complete_file$mean_diameter & complete_file$sm),] # 61 sp
  diameter_sm_angio <- diameter_sla[diameter_sm$new_gp == "angio",]
  diameter_sm_gymno <- diameter_sla[diameter_sm$new_gp == "gymno",]
  
  diameter_sm_angio <- cor.test(diameter_sm_angio$mean_diameter, diameter_sm_angio$sm, method = "pearson")
  diameter_sm_gymno <- cor.test(diameter_sm_gymno$mean_diameter, diameter_sm_gymno$sm, method = "pearson")
  
  
  # diameter - wood density
  diameter_wd <- complete_file[!is.na(complete_file$mean_diameter & complete_file$ssd),] # 61 sp
  diameter_wd_angio <- diameter_wd[diameter_wd$new_gp == "angio",]
  diameter_wd_gymno <- diameter_wd[diameter_wd$new_gp == "gymno",]
  
  diameter_wd_angio <- cor.test(diameter_wd_angio$mean_diameter, diameter_wd_angio$ssd, method = "pearson")
  diameter_wd_gymno <- cor.test(diameter_wd_gymno$mean_diameter, diameter_wd_gymno$ssd, method = "pearson")
  
  
  # diameter - shade tolerance
  diameter_sh <- complete_file[!is.na(complete_file$mean_diameter & complete_file$shade_tol_mean),] # 49 sp
  diameter_sh_angio <- diameter_sh[diameter_sh$new_gp == "angio",]
  diameter_sh_gymno <- diameter_sh[diameter_sh$new_gp == "gymno",]
  
  diameter_sh_angio <- cor.test(diameter_sh_angio$mean_diameter, diameter_sh_angio$shade_tol_mean, method = "pearson")
  diameter_sh_gymno <- cor.test(diameter_sh_gymno$mean_diameter, diameter_sh_gymno$shade_tol_mean, method = "pearson")
  
  
  
  
  # volume - mean annual temperature
  volume_mat <- complete_file[!is.na(complete_file$mean_volume & complete_file$mat),] # 47 sp
  volume_mat_angio <- volume_mat[volume_mat$new_gp == "angio",]
  volume_mat_gymno <- volume_mat[volume_mat$new_gp == "gymno",]
  
  volume_mat_angio <- cor.test(volume_mat_angio$mean_volume, volume_mat_angio$mat, method = "pearson")
  volume_mat_gymno <- cor.test(volume_mat_gymno$mean_volume, volume_mat_gymno$mat, method = "pearson")
  
  
  # volume - min annual temperature
  volume_tmin <- complete_file[!is.na(complete_file$mean_volume & complete_file$tmin),] # 130 sp
  volume_tmin_angio <- volume_mat[volume_tmin$new_gp == "angio",]
  volume_tmin_gymno <- volume_mat[volume_tmin$new_gp == "gymno",]
  
  volume_tmin_angio <- cor.test(volume_tmin_angio$mean_volume, volume_tmin_angio$tmin, method = "pearson")
  volume_tmin_gymno <- cor.test(volume_tmin_gymno$mean_volume, volume_tmin_gymno$tmin, method = "pearson")
  
  
  # volume - map annual temperature
  volume_map <- complete_file[!is.na(complete_file$mean_volume & complete_file$map),] # 130 sp
  volume_map_angio <- volume_mat[volume_map$new_gp == "angio",]
  volume_map_gymno <- volume_mat[volume_map$new_gp == "gymno",]
  
  volume_map_angio <- cor.test(volume_map_angio$mean_volume, volume_map_angio$map, method = "pearson")
  volume_map_gymno <- cor.test(volume_map_gymno$mean_volume, volume_map_gymno$map, method = "pearson")
  
  
  # volume - leaf area
  volume_la <- complete_file[!is.na(complete_file$mean_volume & complete_file$la),] # 118 sp
  volume_la_angio <- volume_la[volume_la$new_gp == "angio",]
  volume_la_gymno <- volume_la[volume_la$new_gp == "gymno",]
  
  volume_la_angio <- cor.test(volume_la_angio$mean_volume, volume_la_angio$la, method = "pearson")
  volume_la_gymno <- cor.test(volume_la_gymno$mean_volume, volume_la_gymno$la, method = "pearson")
  
  
  # volume - leaf area
  volume_sla <- complete_file[!is.na(complete_file$mean_volume & complete_file$sla),] # 118 sp
  volume_sla_angio <- volume_sla[volume_sla$new_gp == "angio",]
  volume_sla_gymno <- volume_sla[volume_sla$new_gp == "gymno",]
  
  volume_sla_angio <- cor.test(volume_sla_angio$mean_volume, volume_sla_angio$sla, method = "pearson")
  volume_sla_gymno <- cor.test(volume_sla_gymno$mean_volume, volume_sla_gymno$sla, method = "pearson")
  
  
  # volume - seed mass
  volume_sm <- complete_file[!is.na(complete_file$mean_volume & complete_file$sm),] # 130 sp
  volume_sm_angio <- volume_sla[volume_sm$new_gp == "angio",]
  volume_sm_gymno <- volume_sla[volume_sm$new_gp == "gymno",]
  
  volume_sm_angio <- cor.test(volume_sm_angio$mean_volume, volume_sm_angio$sm, method = "pearson")
  volume_sm_gymno <- cor.test(volume_sm_gymno$mean_volume, volume_sm_gymno$sm, method = "pearson")
  
  
  # volume - wood density
  volume_wd <- complete_file[!is.na(complete_file$mean_volume & complete_file$ssd),] # 127 sp
  volume_wd_angio <- volume_wd[volume_wd$new_gp == "angio",]
  volume_wd_gymno <- volume_wd[volume_wd$new_gp == "gymno",]
  
  volume_wd_angio <- cor.test(volume_wd_angio$mean_volume, volume_wd_angio$ssd, method = "pearson")
  volume_wd_gymno <- cor.test(volume_wd_gymno$mean_volume, volume_wd_gymno$ssd, method = "pearson")
  
  
  # volume - shade tolerance
  volume_sh <- complete_file[!is.na(complete_file$mean_volume & complete_file$shade_tol_mean),] # 119 sp
  volume_sh_angio <- volume_sh[volume_sh$new_gp == "angio",]
  volume_sh_gymno <- volume_sh[volume_sh$new_gp == "gymno",]
  
  volume_sh_angio <- cor.test(volume_sh_angio$mean_volume, volume_sh_angio$shade_tol_mean, method = "pearson")
  volume_sh_gymno <- cor.test(volume_sh_gymno$mean_volume, volume_sh_gymno$shade_tol_mean, method = "pearson")
  
  
  cor_angio <- matrix(nrow = 4, ncol = 8)
  cor_gymno <- matrix(nrow = 4, ncol = 8)
  
  p_angio <- matrix(nrow = 4, ncol = 8)
  p_gymno <- matrix(nrow = 4, ncol = 8)
  
  cor_angio[1,1] <- h_mat_angio$estimate
  cor_angio[1,2] <- h_tmin_angio$estimate
  cor_angio[1,3] <- h_map_angio$estimate
  cor_angio[1,4] <- h_la_angio$estimate
  cor_angio[1,5] <- h_sla_angio$estimate
  cor_angio[1,6] <- h_sm_angio$estimate
  cor_angio[1,7] <- h_wd_angio$estimate
  cor_angio[1,8] <- h_sh_angio$estimate
  
  cor_angio[2,1] <- ratio_mat_angio$estimate
  cor_angio[2,2] <- ratio_tmin_angio$estimate
  cor_angio[2,3] <- ratio_map_angio$estimate
  cor_angio[2,4] <- ratio_la_angio$estimate
  cor_angio[2,5] <- ratio_sla_angio$estimate
  cor_angio[2,6] <- ratio_sm_angio$estimate
  cor_angio[2,7] <- ratio_wd_angio$estimate
  cor_angio[2,8] <- ratio_sh_angio$estimate
  
  cor_angio[3,1] <- diameter_mat_angio$estimate
  cor_angio[3,2] <- diameter_tmin_angio$estimate
  cor_angio[3,3] <- diameter_map_angio$estimate
  cor_angio[3,4] <- diameter_la_angio$estimate
  cor_angio[3,5] <- diameter_sla_angio$estimate
  cor_angio[3,6] <- diameter_sm_angio$estimate
  cor_angio[3,7] <- diameter_wd_angio$estimate
  cor_angio[3,8] <- diameter_sh_angio$estimate
  
  cor_angio[4,1] <- volume_mat_angio$estimate
  cor_angio[4,2] <- volume_tmin_angio$estimate
  cor_angio[4,3] <- volume_map_angio$estimate
  cor_angio[4,4] <- volume_la_angio$estimate
  cor_angio[4,5] <- volume_sla_angio$estimate
  cor_angio[4,6] <- volume_sm_angio$estimate
  cor_angio[4,7] <- volume_wd_angio$estimate
  cor_angio[4,8] <- volume_sh_angio$estimate
  
  
  
  cor_gymno[1,1] <- h_mat_gymno$estimate
  cor_gymno[1,2] <- h_tmin_gymno$estimate
  cor_gymno[1,3] <- h_map_gymno$estimate
  cor_gymno[1,4] <- h_la_gymno$estimate
  cor_gymno[1,5] <- h_sla_gymno$estimate
  cor_gymno[1,6] <- h_sm_gymno$estimate
  cor_gymno[1,7] <- h_wd_gymno$estimate
  cor_gymno[1,8] <- h_sh_gymno$estimate
  
  cor_gymno[2,1] <- ratio_mat_gymno$estimate
  cor_gymno[2,2] <- ratio_tmin_gymno$estimate
  cor_gymno[2,3] <- ratio_map_gymno$estimate
  cor_gymno[2,4] <- ratio_la_gymno$estimate
  cor_gymno[2,5] <- ratio_sla_gymno$estimate
  cor_gymno[2,6] <- ratio_sm_gymno$estimate
  cor_gymno[2,7] <- ratio_wd_gymno$estimate
  cor_gymno[2,8] <- ratio_sh_gymno$estimate
  
  cor_gymno[3,1] <- diameter_mat_gymno$estimate
  cor_gymno[3,2] <- diameter_tmin_gymno$estimate
  cor_gymno[3,3] <- diameter_map_gymno$estimate
  cor_gymno[3,4] <- diameter_la_gymno$estimate
  cor_gymno[3,5] <- diameter_sla_gymno$estimate
  cor_gymno[3,6] <- diameter_sm_gymno$estimate
  cor_gymno[3,7] <- diameter_wd_gymno$estimate
  cor_gymno[3,8] <- diameter_sh_gymno$estimate
  
  cor_gymno[4,1] <- volume_mat_gymno$estimate
  cor_gymno[4,2] <- volume_tmin_gymno$estimate
  cor_gymno[4,3] <- volume_map_gymno$estimate
  cor_gymno[4,4] <- volume_la_gymno$estimate
  cor_gymno[4,5] <- volume_sla_gymno$estimate
  cor_gymno[4,6] <- volume_sm_gymno$estimate
  cor_gymno[4,7] <- volume_wd_gymno$estimate
  cor_gymno[4,8] <- volume_sh_gymno$estimate
  
  
  p_angio[1,1] <- h_mat_angio$p.value
  p_angio[1,2] <- h_tmin_angio$p.value
  p_angio[1,3] <- h_map_angio$p.value
  p_angio[1,4] <- h_la_angio$p.value
  p_angio[1,5] <- h_sla_angio$p.value
  p_angio[1,6] <- h_sm_angio$p.value
  p_angio[1,7] <- h_wd_angio$p.value
  p_angio[1,8] <- h_sh_angio$p.value
  
  p_angio[2,1] <- ratio_mat_angio$p.value
  p_angio[2,2] <- ratio_tmin_angio$p.value
  p_angio[2,3] <- ratio_map_angio$p.value
  p_angio[2,4] <- ratio_la_angio$p.value
  p_angio[2,5] <- ratio_sla_angio$p.value
  p_angio[2,6] <- ratio_sm_angio$p.value
  p_angio[2,7] <- ratio_wd_angio$p.value
  p_angio[2,8] <- ratio_sh_angio$p.value
  
  p_angio[3,1] <- diameter_mat_angio$p.value
  p_angio[3,2] <- diameter_tmin_angio$p.value
  p_angio[3,3] <- diameter_map_angio$p.value
  p_angio[3,4] <- diameter_la_angio$p.value
  p_angio[3,5] <- diameter_sla_angio$p.value
  p_angio[3,6] <- diameter_sm_angio$p.value
  p_angio[3,7] <- diameter_wd_angio$p.value
  p_angio[3,8] <- diameter_sh_angio$p.value
  
  p_angio[4,1] <- volume_mat_angio$p.value
  p_angio[4,2] <- volume_tmin_angio$p.value
  p_angio[4,3] <- volume_map_angio$p.value
  p_angio[4,4] <- volume_la_angio$p.value
  p_angio[4,5] <- volume_sla_angio$p.value
  p_angio[4,6] <- volume_sm_angio$p.value
  p_angio[4,7] <- volume_wd_angio$p.value
  p_angio[4,8] <- volume_sh_angio$p.value
  
  
  
  p_gymno[1,1] <- h_mat_gymno$p.value
  p_gymno[1,2] <- h_tmin_gymno$p.value
  p_gymno[1,3] <- h_map_gymno$p.value
  p_gymno[1,4] <- h_la_gymno$p.value
  p_gymno[1,5] <- h_sla_gymno$p.value
  p_gymno[1,6] <- h_sm_gymno$p.value
  p_gymno[1,7] <- h_wd_gymno$p.value
  p_gymno[1,8] <- h_sh_gymno$p.value
  
  p_gymno[2,1] <- ratio_mat_gymno$p.value
  p_gymno[2,2] <- ratio_tmin_gymno$p.value
  p_gymno[2,3] <- ratio_map_gymno$p.value
  p_gymno[2,4] <- ratio_la_gymno$p.value
  p_gymno[2,5] <- ratio_sla_gymno$p.value
  p_gymno[2,6] <- ratio_sm_gymno$p.value
  p_gymno[2,7] <- ratio_wd_gymno$p.value
  p_gymno[2,8] <- ratio_sh_gymno$p.value
  
  p_gymno[3,1] <- diameter_mat_gymno$p.value
  p_gymno[3,2] <- diameter_tmin_gymno$p.value
  p_gymno[3,3] <- diameter_map_gymno$p.value
  p_gymno[3,4] <- diameter_la_gymno$p.value
  p_gymno[3,5] <- diameter_sla_gymno$p.value
  p_gymno[3,6] <- diameter_sm_gymno$p.value
  p_gymno[3,7] <- diameter_wd_gymno$p.value
  p_gymno[3,8] <- diameter_sh_gymno$p.value
  
  p_gymno[4,1] <- volume_mat_gymno$p.value
  p_gymno[4,2] <- volume_tmin_gymno$p.value
  p_gymno[4,3] <- volume_map_gymno$p.value
  p_gymno[4,4] <- volume_la_gymno$p.value
  p_gymno[4,5] <- volume_sla_gymno$p.value
  p_gymno[4,6] <- volume_sm_gymno$p.value
  p_gymno[4,7] <- volume_wd_gymno$p.value
  p_gymno[4,8] <- volume_sh_gymno$p.value
  
  par(mfrow = c(1,1))
  corrplot(cor_angio, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Oranges"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  corrplot(cor_gymno, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
           col = brewer.pal(n = 10, name = "Greens"), addCoef.col = "black", cl.pos = "n",
           diag = TRUE)
  
  
  pca_file <- complete_file %>% dplyr::select(species, new_gp, 
                                              mean_volume, mean_hmax, mean_diameter, mean_ratio,
                                              map, mat, tmin,
                                              sla, la, sm, ssd,
                                              shade_tol_mean) 
  
  pca_file <- pca_file[!is.na(pca_file$mean_volume),]
  pca_file <- pca_file[!is.na(pca_file$mean_hmax),]
  pca_file <- pca_file[!is.na(pca_file$sla),]
  pca_file <- pca_file[!is.na(pca_file$shade_tol_mean),]
  
  row.names(pca_file) <- pca_file$species
  
  angio <- pca_file %>% dplyr::filter(new_gp == "angio") %>%
                        dplyr::select(-species, -new_gp)
  
  gymno <- pca_file %>% dplyr::filter(new_gp == "gymno") %>%
                        dplyr::select(-species, -new_gp)
  
  
  res_pca_full <- pca_file %>% dplyr::select(-species, -new_gp)
  res_pca_full <- prcomp(res_pca_full, scale = TRUE, center = TRUE)
  
  res_pca_angio <- prcomp(angio, scale = TRUE, center = TRUE)
  res_pca_gymno <- prcomp(gymno, scale = TRUE, center = TRUE)
  
  fviz_eig(res_pca_full, addlabels = TRUE) # axis 1: 29.3% / axis 2: 18.9%
  fviz_eig(res_pca_angio, addlabels = TRUE) # axis 1: 25.7% / axis 2: 24.5%
  fviz_eig(res_pca_gymno, addlabels = TRUE) # axis 1: 32.8% / axis 2: 21%
  
  par(mfrow = c(1,2))
  
  fviz_pca_var(res_pca_angio,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "Angiosperms",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "all")  
  
  fviz_pca_var(res_pca_gymno,
               col.var = "contrib", 
               gradient.cols = c("#006D2C", "#006D2C", "#006D2C"),
               title = "Angiosperms",
               ggtheme = theme_minimal(),
               repel = TRUE, 
               label = "all") 
  
}


response_to_c1 <- function () {
  
  # loading summaries from all model outputs
  depth_dbh_c1 <- read.csv(file = "output/depth_dbh_c1_summary.csv")
  diameter_dbh_c1 <- read.csv(file = "output/diameter_dbh_c1_summary.csv")
  ratio_dbh_c1 <- read.csv(file = "output/ratio_dbh_c1_summary.csv")
  
  dens_depth_20 <- density(depth_dbh_c1$mean_depth_20)
  plot(dens_depth_20, col = "lightgreen")
  dens_depth_5 <- density(depth_dbh_c1$mean_depth_5)
  lines(dens_depth_5, col = "darkgreen")
  
  depth_dbh_c1_b <- depth_dbh_c1[depth_dbh_c1$mean_depth_20 <= median(depth_dbh_c1$mean_depth_20),]
  dens_depth_20_b <- density(depth_dbh_c1_b$mean_depth_20)
  plot(dens_depth_20_b, frame = FALSE, col = "darkgreen", 
       main = "Density plot of depth response to competition",
       xlim = c(0,17000), 
       ylim = c(0,0.0005),
       las = 1,
       xlab = "Estimated depth (m) at a 15 cm dbh",
       ylab = "Density") 
  polygon(dens_depth_20_b, col = "lightgreen")
  
  depth_dbh_c1_b <- depth_dbh_c1[depth_dbh_c1$mean_depth_5 <= median(depth_dbh_c1$mean_depth_5),]
  dens_depth_5_b <- density(depth_dbh_c1_b$mean_depth_5)
  polygon(dens_depth_5_b, col = "darkgreen")
  
  
  dens_diameter_20 <- density(diameter_dbh_c1$mean_diameter_20)
  plot(dens_diameter_20, col = "lightgreen")
  dens_diameter_5 <- density(diameter_dbh_c1$mean_diameter_5)
  lines(dens_diameter_5, col = "darkgreen")
  
  diameter_dbh_c1_b <- diameter_dbh_c1[diameter_dbh_c1$mean_diameter_20 <= median(diameter_dbh_c1$mean_diameter_20),]
  dens_diameter_20_b <- density(diameter_dbh_c1_b$mean_diameter_20)
  plot(dens_diameter_20_b, frame = FALSE, col = "darkgreen", 
       main = "Density plot of diameter response to competition",
       xlim = c(0,500), 
       ylim = c(0,0.015),
       las = 1,
       xlab = "Estimated diameter (m) at a 15 cm dbh",
       ylab = "Density") 
  polygon(dens_diameter_20_b, col = "lightgreen")
  
  diameter_dbh_c1_b <- diameter_dbh_c1[diameter_dbh_c1$mean_diameter_5 <= median(diameter_dbh_c1$mean_diameter_5),]
  dens_diameter_5_b <- density(diameter_dbh_c1_b$mean_diameter_5)
  polygon(dens_diameter_5_b, col = "darkgreen")

  

  dens_ratio_20 <- density(ratio_dbh_c1$mean_ratio_20)
  plot(dens_ratio_20, col = "lightgreen")
  dens_ratio_5 <- density(ratio_dbh_c1$mean_ratio_5)
  lines(dens_ratio_5, col = "darkgreen")
  
  ratio_dbh_c1_b <- ratio_dbh_c1[ratio_dbh_c1$mean_ratio_20 <= median(ratio_dbh_c1$mean_ratio_20),]
  dens_ratio_20_b <- density(ratio_dbh_c1_b$mean_ratio_20)
  plot(dens_ratio_20_b, frame = FALSE, col = "darkgreen", 
       main = "Density plot of ratio response to competition",
       xlim = c(0.2,0.5), 
       ylim = c(0,15),
       las = 1,
       xlab = "Estimated ratio (m) at a 15 cm dbh",
       ylab = "Density") 
  polygon(dens_ratio_20_b, col = "lightgreen")
  
  ratio_dbh_c1_b <- ratio_dbh_c1[ratio_dbh_c1$mean_ratio_5 <= median(ratio_dbh_c1$mean_ratio_5),]
  dens_ratio_5_b <- density(ratio_dbh_c1_b$mean_ratio_5)
  polygon(dens_ratio_5_b, col = "darkgreen")
  
  
  depth_parameters_c1 <- read.csv(file = "output/depth_power_parameters_c1.csv")
  depth_parameters <- read.csv(file = "output/depth_power_parameters.csv")
  diameter_parameters_c1 <- read.csv(file = "output/diameter_power_parameters_c1.csv")

  
  dens_power_depth_comp <- density(depth_parameters_c1$mean_comp)
  dens_power_depth_a1 <- density(depth_parameters_c1$mean_a1)
  dens_power_depth_a2 <- density(depth_parameters_c1$mean_a2)
  dens_power_depth_a1_nocomp <- density(depth_parameters$mean_a1)
  dens_power_depth_a2_nocomp <- density(depth_parameters$mean_a2)
  
  dens_power_diam_comp <- density(diameter_parameters_c1$mean_comp)
  
  plot(dens_power_depth_comp, frame = FALSE, col = "green4", lwd = 4,
       xlim = c(0,1.0))
  lines(dens_power_diam_comp, col = "blue4", lwd = 4)
  
  
  plot(dens_power_depth_a1, frame = FALSE, col = "green4", lwd = 4,
       xlim = c(-1.0,5.0))
  lines(dens_power_depth_a1_nocomp, col = "green4", lwd = 4, lty = 2)
  
  plot(dens_power_depth_a2, frame = FALSE, col = "green4", lwd = 4,
       xlim = c(0,7),
       ylim = c(0,4))
  lines(dens_power_depth_a2_nocomp, col = "green4", lwd = 4, lty = 2)
       
  
  
  
  
  
  
  
}
  
  
  

  



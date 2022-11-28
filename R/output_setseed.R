height_parameters <-  function() {
  
  # compiling outputs from height models
  dd <- list.files(path = "output/", pattern = "asympt_height_nocomp_rs_")
  data_list <- lapply(paste0("output/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  height_resampling_asympt <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  height_resampling_asympt <- height_resampling_asympt %>% dplyr::select(-X)
  
  # removing all species for which 100 resamplings were not reached
  asymptot_summary <- height_resampling_asympt %>% dplyr::filter(weighted == "no") %>%
                                                   dplyr::group_by(species) %>%
                                                   dplyr::summarise(nobs = n()) 
  
  height_sp_list <- asymptot_summary[asymptot_summary$nobs >= 100,]$species
  
  height_resampling_asympt <- height_resampling_asympt[height_resampling_asympt$species %in% height_sp_list,]

  # height_parameters_natheo <- height_resampling_asympt %>% dplyr::filter(weighted == "yes")
  
  height_asympt <- height_resampling_asympt %>% dplyr::filter(weighted == "yes") %>%
                                                dplyr::group_by(species) %>%
                                                dplyr::slice_sample(n = 100)
  
  height_parameters <- height_asympt %>% dplyr::select(species, b1, b2, b3) %>%
                                         dplyr::group_by(species) %>%
                                         dplyr::mutate(id = c(1:100)) %>%
                                         dplyr::ungroup()
  
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea")
  height_parameters <- height_parameters[!height_parameters$species %in% to_remove,]
  
  # height_parameters_natheo <- height_parameters_natheo[!height_parameters_natheo$species %in% to_remove,] 
  # height_parameters_natheo <- height_parameters_natheo %>% dplyr::select(species, b1, b2, b3) 
  # write.csv(file = "output/bis_setseed/height_parameters_natheo.csv", height_parameters_natheo)
  
  write.csv(file = "output/bis_setseed/height_parameters.csv", height_parameters)
  return(height_parameters)  
  
}


diameter_parameters <- function() {
  
  # compiling outputs from diameter models
  dd <- list.files(path = "output/bis_setseed/", pattern = "diameter_")
  data_list <- lapply(paste0("output/bis_setseed/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  diameter_allmodels <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  diameter_allmodels <- diameter_allmodels[,c(1:23)]
  diameter_allmodels <- diameter_allmodels %>% dplyr::select(-X)
  id <- as.data.frame(rep(x = c(1:500), times = length(unique(diameter_allmodels$species)) * 6))
  names(id) <- "id"
  diameter_allmodels <- cbind(id, diameter_allmodels)
  
  diameter_nocomp <- diameter_allmodels %>% dplyr::filter(condition == "nocomp")
  diameter_nocomp <- diameter_nocomp %>% dplyr::filter(weighted == "yes")
  diameter_nocomp <- diameter_nocomp %>% dplyr::select(id, species, a1, a2, AIC) %>%
                                         dplyr::rename(a1_nocomp = a1, a2_nocomp = a2, AIC_nocomp = AIC)
  
  diameter_c1 <- diameter_allmodels %>% dplyr::filter(condition == "c1")
  diameter_c1 <- diameter_c1 %>% dplyr::filter(weighted == "yes")
  diameter_c1 <- diameter_c1 %>% dplyr::select(id, species, a1, a2, comp, AIC) %>%
                                 dplyr::rename(a1_c1 = a1, a2_c1 = a2, comp_c1 = comp, AIC_c1 = AIC)
  
  diameter_c2 <- diameter_allmodels %>% dplyr::filter(condition == "c2")
  diameter_c2 <- diameter_c2 %>% dplyr::filter(weighted == "yes")
  diameter_c2 <- diameter_c2 %>% dplyr::select(id, species, a1, a2, comp, AIC) %>%
                                 dplyr::rename(a1_c2 = a1, a2_c2 = a2, comp_c2 = comp, AIC_c2 = AIC)
  
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea")
  diameter_nocomp <- diameter_nocomp[!diameter_nocomp$species %in% to_remove,]
  diameter_c1 <- diameter_c1[!diameter_c1$species %in% to_remove,]
  diameter_c2 <- diameter_c2[!diameter_c2$species %in% to_remove,]
  
  diameter_parameters <- join_all(list(diameter_nocomp, diameter_c1, diameter_c2), by = c("id", "species"))
  diameter_parameters <- diameter_parameters %>% dplyr::group_by(species) %>%
                                                 dplyr::sample_n(110) %>%
                                                 dplyr::mutate(id = c(1:110)) %>%
                                                 dplyr::ungroup()
  
  AIC_summary <- diameter_parameters %>% dplyr::group_by(species) %>%
                                         dplyr::summarise(nocomp_AIC = sum(AIC_nocomp),
                                                          batot_AIC = sum(AIC_c1),
                                                          balarger_AIC = sum(AIC_c2)) 
  
  AIC_summary <- AIC_summary %>% dplyr::mutate(id = c(1:dim(AIC_summary)[1]))
  
  AIC_summary_b <- as.data.frame(t(AIC_summary))
  colnames(AIC_summary_b) <- AIC_summary_b[1,]
  AIC_summary_b <- AIC_summary_b[c(-1,-5),]
  
  best_AIC <- as.data.frame(matrix(nrow = dim(AIC_summary)[1], ncol = 1))
  names(best_AIC) <- "best_AIC"
  
  for(i in 1:dim(AIC_summary_b)[2]) {
    best_AIC[i,"best_AIC"] <- which.min(AIC_summary_b[,i])
  }
  
  id <- as.data.frame(c(1:dim(AIC_summary)[1]))
  names(id) <- "id"
  best_AIC <- cbind(best_AIC, id)
  
  nocomp <- best_AIC %>% dplyr::filter(best_AIC == 1) %>%
                         dplyr::mutate(AIC = "no_comp")
  
  c1 <- best_AIC %>% dplyr::filter(best_AIC == 2) %>%
                     dplyr::mutate(AIC = "ba_tot")
  
  c2 <- best_AIC %>% dplyr::filter(best_AIC == 3) %>%
                     dplyr::mutate(AIC = "ba_larger")
  
  AIC <- rbind(nocomp, c1, c2)
  AIC <- AIC %>% dplyr::select(-best_AIC)
  
  AIC_summary <- left_join(AIC_summary, AIC, by = "id")
  AIC_summary <- AIC_summary %>% dplyr::select(-id) %>%
                                 dplyr::rename(best_diam_AIC = AIC)

  
  write.csv(file = "output/bis_setseed/diameter_parameters.csv", diameter_parameters)
  write.csv(file = "output/bis_setseed/AIC_diameter.csv", AIC_summary)
  
  return(diameter_parameters)  
  
}
  
    
  

ratio_parameters <- function() {
  
  # compiling outputs from ratio models
  dd <- list.files(path = "output/bis_setseed/", pattern = "ratio_")
  data_list <- lapply(paste0("output/bis_setseed/", dd), utils::read.table,
                      header = TRUE, sep = ",", dec = ".",
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  
  ratio_allmodels <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  ratio_allmodels <- ratio_allmodels %>% dplyr::select(-X)
  id <- as.data.frame(rep(x = c(1:500), times = length(unique(ratio_allmodels$species)) * 6))
  names(id) <- "id"
  ratio_allmodels <- cbind(id, ratio_allmodels)
  
  ratio_nocomp <- ratio_allmodels %>% dplyr::filter(condition == "nocomp")
  ratio_nocomp <- ratio_nocomp %>% dplyr::filter(weighted == "yes")
  ratio_nocomp <- ratio_nocomp %>% dplyr::select(id, species, a1, AIC) %>%
                                   dplyr::rename(a1_nocomp = a1, AIC_nocomp = AIC)
  
  ratio_c1 <- ratio_allmodels %>% dplyr::filter(condition == "c1")
  ratio_c1 <- ratio_c1 %>% dplyr::filter(weighted == "yes")
  ratio_c1 <- ratio_c1 %>% dplyr::select(id, species, a1, comp, AIC) %>%
                           dplyr::rename(a1_c1 = a1, comp_c1 = comp, AIC_c1 = AIC)
  
  ratio_c2 <- ratio_allmodels %>% dplyr::filter(condition == "c2")
  ratio_c2 <- ratio_c2 %>% dplyr::filter(weighted == "yes")
  ratio_c2 <- ratio_c2 %>% dplyr::select(id, species, a1, comp, AIC) %>%
                           dplyr::rename(a1_c2 = a1, comp_c2 = comp, AIC_c2 = AIC)
  
  to_remove <- c("Cornus florida", "Crataegus monogyna", "Eucalyptus camaldulensis", "Olea europaea", "Picea")
  ratio_nocomp <- ratio_nocomp[!ratio_nocomp$species %in% to_remove,]
  ratio_c1 <- ratio_c1[!ratio_c1$species %in% to_remove,]
  ratio_c2 <- ratio_c2[!ratio_c2$species %in% to_remove,]

  ratio_parameters <- join_all(list(ratio_nocomp, ratio_c1, ratio_c2), by = c("id", "species"))
  ratio_parameters <- ratio_parameters[!is.na(ratio_parameters$comp_c2),]
  
  ratio_summary <- ratio_parameters %>% dplyr::group_by(species) %>%
                                        dplyr::summarise(nobs = n()) 
  
  sp_list <-ratio_summary[ratio_summary$nobs >= 100,]$species
  ratio_parameters <- ratio_parameters[ratio_parameters$species %in% sp_list,]
  ratio_parameters <- ratio_parameters[ratio_parameters$species != "Juglans nigra",]
  
  ratio_parameters <- ratio_parameters %>% dplyr::group_by(species) %>%
                                           dplyr::sample_n(110) %>%
                                           dplyr::mutate(id = c(1:110)) %>%
                                           dplyr::ungroup()
  
  AIC_summary <- ratio_parameters %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(nocomp_AIC = sum(AIC_nocomp),
                                                       batot_AIC = sum(AIC_c1),
                                                       balarger_AIC = sum(AIC_c2)) 
  
  AIC_summary <- AIC_summary %>% dplyr::mutate(id = c(1:dim(AIC_summary)[1]))
  
  AIC_summary_b <- as.data.frame(t(AIC_summary))
  colnames(AIC_summary_b) <- AIC_summary_b[1,]
  AIC_summary_b <- AIC_summary_b[c(-1,-5),]
  
  best_AIC <- as.data.frame(matrix(nrow = dim(AIC_summary)[1], ncol = 1))
  names(best_AIC) <- "best_AIC"
  
  for(i in 1:dim(AIC_summary_b)[2]) {
    best_AIC[i,"best_AIC"] <- which.min(AIC_summary_b[,i])
  }
  
  id <- as.data.frame(c(1:dim(AIC_summary)[1]))
  names(id) <- "id"
  best_AIC <- cbind(best_AIC, id)
  
  nocomp <- best_AIC %>% dplyr::filter(best_AIC == 1) %>%
                         dplyr::mutate(AIC = "no_comp")
  
  c1 <- best_AIC %>% dplyr::filter(best_AIC == 2) %>%
                     dplyr::mutate(AIC = "ba_tot")
  
  c2 <- best_AIC %>% dplyr::filter(best_AIC == 3) %>%
                     dplyr::mutate(AIC = "ba_larger")
  
  AIC <- rbind(nocomp, c1, c2)
  AIC <- AIC %>% dplyr::select(-best_AIC)
  
  AIC_summary <- left_join(AIC_summary, AIC, by = "id")
  AIC_summary <- AIC_summary %>% dplyr::select(-id) %>%
                                 dplyr::rename(best_ratio_AIC = AIC)
  
  write.csv(file = "output/bis_setseed/ratio_parameters.csv", ratio_parameters)
  write.csv(file = "output/bis_setseed/AIC_ratio.csv", AIC_summary)
  
  return(ratio_parameters)  
  
}

AIC_diameter <- read.csv(file = "output/bis_setseed/AIC_diameter.csv")
AIC_ratio <- read.csv(file = "output/bis_setseed/AIC_ratio.csv")

height <- read.csv(file = "output/bis_setseed/height_parameters.csv")
ratio <- read.csv("output/bis_setseed/ratio_parameters.csv")

  
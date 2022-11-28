# require(corrplot)
# library(ggplot2)
# library(grid)
# library(RColorBrewer)
# require(stringr)
# 
# 
# ## Analyzing models' outputs
# 
# get_height_outputs <- function(sp_complete_data) {
#   
#   sp_to_keep <- sp_complete_data$species
#   sp_to_keep <- sp_to_keep[!(sp_to_keep %in% c("Abies concolor", "Picea engelmannii", "Pinus halepensis",
#                                                "Pinus taeda","Thuja plicata", "Ulmus americana"))]
# 
#   # resampling - no competition
#   dd <- list.files(path = "output/", pattern = "asympt_height_nocomp_rs_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   height_resampling_asympt <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   # computing total AIC for each model
#   height_resampling_asympt <- height_resampling_asympt[height_resampling_asympt$species %in% sp_to_keep,]
#   height_asympt <- height_resampling_asympt %>% dplyr::filter(weighted == "no") %>%
#                                                 dplyr::group_by(species) %>%
#                                                 dplyr::filter(!is.na(b1)) %>% 
#                                                 dplyr::slice_sample(n = 100) %>%
#                                                 dplyr::mutate(ID_b = X + 300) %>%
#                                                 dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
#   
#   height_asympt_b <- height_resampling_asympt %>% dplyr::filter(weighted == "yes") %>%
#                                                   dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
#   height_asympt_b <- height_asympt_b[height_asympt_b$ID_tofilter %in% height_asympt$ID_species,]
#   
#   height_asympt_results_b <- height_asympt_b %>% dplyr::group_by(species) %>%
#                                                  dplyr::summarise(mean_b1 = mean(b1),
#                                                                   sd_b1 = sd(b1),
#                                                                   mean_b2 = mean(b2),
#                                                                   sd_b2 = sd(b2),
#                                                                   mean_b3 = mean(b3),
#                                                                   sd_b3 = sd(b3),
#                                                                   total_AIC = sum(AIC),
#                                                                   total_RMSE = sum(RMSE))
#   
#   # computing H max for all species
#   for (i in 1:dim(height_asympt_results_b)[1]) {
#     
#     n_repetition = 100
#     
#     height_max <- as.data.frame(matrix(nrow = n_repetition, ncol = 2)) 
#     height_max[,1] <- rep(height_asympt_results_b[i,"species"], n_repetition)
#     names(height_max) <- c("species", "h_max")
#     
#     mean_b1 = height_asympt_results_b$mean_b1[i]
#     sd_b1 = height_asympt_results_b$sd_b1[i]
#     
#     
#     for (j in 1:n_repetition) {
#       height_max[j,"h_max"] = rnorm(1, mean = mean_b1, sd = sd_b1) }
#     
#     write.csv(height_max, file =  paste0("output/height_max_", height_asympt_results_b[i,"species"], ".csv"))
#     
#   }
#     
#     
#   # computing height values from estimated parameters for different dbh (to allow estimating crown ratio then)
#   for (i in 1:dim(height_asympt_results_b)[1]) {
#     
#     n_repetition = 100
#     
#     height_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5)) 
#     height_dbh[,1] <- rep(height_asympt_results_b[i,"species"], n_repetition)
#     names(height_dbh) <- c("species", "dbh_10", "dbh_20", "dbh_30", "dbh_40")
#     
#     mean_b1 = height_asympt_results_b$mean_b1[i]
#     sd_b1 = height_asympt_results_b$sd_b1[i]
#     
#     mean_b2 = height_asympt_results_b$mean_b2[i]
#     sd_b2 = height_asympt_results_b$sd_b2[i]
#     
#     mean_b3 = height_asympt_results_b$mean_b3[i]
#     sd_b3 = height_asympt_results_b$sd_b3[i]
#     
#     
#     for (j in 1:n_repetition) {
#       
#       b1 <- rnorm(1, mean = mean_b1, sd = sd_b1)
#       b2 <- rnorm(1, mean = mean_b2, sd = sd_b2)
#       b3 <- rnorm(1, mean = mean_b3, sd = sd_b3)
#       
#       height_dbh[j,"dbh_10"] = 1.3 + b1 * (1-exp(-b2 * 10)) ^ b3
#       height_dbh[j,"dbh_20"] = 1.3 + b1 * (1-exp(-b2 * 20)) ^ b3
#       height_dbh[j,"dbh_30"] = 1.3 + b1 * (1-exp(-b2 * 30)) ^ b3
#       height_dbh[j,"dbh_40"] = 1.3 + b1 * (1-exp(-b2 * 40)) ^ b3
#       
#     }
#     
#     write.csv(height_dbh, file =  paste0("output/height_dbh_", height_asympt_results_b[i,"species"], ".csv"))
#     
#   }
#     
# }
# 
# 
# get_depth_outputs_nocomp <- function(sp_complete_data) {
#   
#   sp_to_keep <- sp_complete_data$species
#   sp_to_keep <- sp_to_keep[!(sp_to_keep %in% c("Abies concolor", "Picea engelmannii", "Pinus halepensis",
#                                                "Pinus taeda","Thuja plicata", "Ulmus americana"))]
#   
#   # resampling - no competition
#   dd <- list.files(path = "output/", pattern = "power_log_depth_nocomp_rs_") 
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   C_depth_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   depth_resampling_power <- C_depth_resampling_power_log[C_depth_resampling_power_log$species %in% sp_to_keep,]
#   depth_power <- depth_resampling_power %>% dplyr::filter(weighted == "no") %>%
#                                             dplyr::group_by(species) %>%
#                                             dplyr::filter(!is.na(a1)) %>% 
#                                             dplyr::slice_sample(n = 100) %>%
#                                             dplyr::mutate(ID_b = X + 300) %>%
#                                             dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
#   
#   depth_power_b <- C_depth_resampling_power_log %>% dplyr::filter(weighted == "yes") %>%
#                                                     dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
#   depth_power_b <- depth_power_b[depth_power_b$ID_tofilter %in% depth_power$ID_species,]
#   
#   depth_power_results_b <- depth_power_b %>% dplyr::group_by(species) %>%
#                                              dplyr::summarise(mean_a1 = mean(a1),
#                                                               sd_a1 = sd(a1),
#                                                               mean_a2 = mean(a2),
#                                                               sd_a2 = sd(a2),
#                                                               mean_sigma = mean(sigma),
#                                                               sd_sigma = sd(sigma),
#                                                               total_AIC = sum(AIC),
#                                                               total_RMSE = sum(RMSE))
#   
#   # computing depth values from estimated parameters for different dbh (to allow estimating crown ratio then)
#   for (i in 1:dim(depth_power_results_b)[1]) {
#     
#     n_repetition = 100
#     
#     depth_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5)) 
#     depth_dbh[,1] <- rep(depth_power_results_b[i,"species"], n_repetition)
#     names(depth_dbh) <- c("species", "dbh_10", "dbh_20", "dbh_30", "dbh_40")
#     
#     mean_a1 = depth_power_results_b$mean_a1[i]
#     sd_a1 = depth_power_results_b$sd_a1[i]
#     
#     mean_a2 = depth_power_results_b$mean_a2[i]
#     sd_a2 = depth_power_results_b$sd_a2[i]
#     
#     mean_sigma = depth_power_results_b$mean_sigma[i]
#     sd_sigma = depth_power_results_b$sd_sigma[i]
#     
#     
#     for (j in 1:n_repetition) {
#       
#       a1 <- rnorm(1, mean = mean_a1, sd = sd_a1)
#       a2 <- rnorm(1, mean = mean_a2, sd = sd_a2)
#       sigma <- rnorm(1, mean = mean_sigma, sd = sd_sigma)
#       
#       depth_dbh[j,"dbh_10"] = a1 * (10^a2) * ((1/2)*(exp(sigma^2)))
#       depth_dbh[j,"dbh_20"] = a1 * (20^a2) * ((1/2)*(exp(sigma^2)))
#       depth_dbh[j,"dbh_30"] = a1 * (30^a2) * ((1/2)*(exp(sigma^2)))
#       depth_dbh[j,"dbh_40"] = a1 * (40^a2) * ((1/2)*(exp(sigma^2)))
#       
#     }
#     
#     write.csv(depth_dbh, file =  paste0("output/depth_dbh_", depth_power_results_b[i,"species"], ".csv"))
#     
#   }
#   
# }
# 
# 
# get_crown_ratio <- function() {
#   
#   se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
#   
#   dd <- list.files(path = "output/", pattern = "height_dbh_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   height_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   height_dbh_file <- height_dbh_allsp %>% dplyr::select(species, dbh_20, dbh_30) %>%
#                                           dplyr::rename(dbh_20_height = dbh_20, dbh_30_height = dbh_30) %>%
#                                           dplyr::group_by(species) %>%
#                                           dplyr::mutate(ID = c(1:100))
# 
#   
#   dd <- list.files(path = "output/", pattern = "depth_dbh_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   depth_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   depth_dbh_file <- depth_dbh_allsp %>% dplyr::select(species, dbh_20, dbh_30) %>%
#                                         dplyr::rename(dbh_20_depth = dbh_20, dbh_30_depth = dbh_30) %>%
#                                         dplyr::group_by(species) %>%
#                                         dplyr::mutate(ID = c(1:100))
#   
#   crown_ratio <- left_join(height_dbh_file, depth_dbh_file, by = c("species", "ID"))
#   crown_ratio_file <- crown_ratio %>% dplyr::mutate(crown_ratio_2 = dbh_20_depth/dbh_20_height, 
#                                                     crown_ratio_3 = dbh_30_depth/dbh_30_height) %>%
#                                       dplyr::group_by(species) %>%
#                                       dplyr::summarise(mean_CR2 = mean(crown_ratio_2, na.rm = TRUE),
#                                                        sd_CR2 = sd(crown_ratio_2, na.rm = TRUE),
#                                                        se_CR2 = se(crown_ratio_2),
#                                                        mean_CR3 = mean(crown_ratio_3, na.rm = TRUE),
#                                                        sd_CR3 = sd(crown_ratio_3, na.rm = TRUE),
#                                                        se_CR3 = se(crown_ratio_3))
#   
#   write.csv(file = "output/crown_ratio.csv", crown_ratio_file)
# 
#                                       
# }
# 
# 
# get_parameters_final_sp <- function() {
#   
#   se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))
#   
#   crown_ratio_file <- read.csv(file =  "output/crown_ratio.csv")
#   
#   new_CR_file <- crown_ratio_file[crown_ratio_file$mean_CR2 < 1,]
#   new_CR_file <- new_CR_file[new_CR_file$mean_CR3 < 1,]
#   new_CR_file <- new_CR_file[!is.na(new_CR_file$mean_CR2),]
#   
#   crown_ratio_final <- new_CR_file
#   write.csv(file = "output/crown_ratio_final.csv", crown_ratio_final)
#   
#   new_sp_list <- new_CR_file$species
#   
#   
#   ## Maximal height
#   dd <- list.files(path = "output/", pattern = "height_max_") 
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   new_height_max <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   final_height_max <- new_height_max[new_height_max$species %in% new_sp_list,]
#   height_max_final <- final_height_max %>% dplyr::group_by(species) %>%
#                                            dplyr::summarise(mean_Hmax = mean(h_max, na.rm = TRUE),
#                                                             sd_Hmax = sd(h_max, na.rm = TRUE),
#                                                             se_Hmax = se(h_max))
#   
#   write.csv(file = "output/height_max_final.csv", height_max_final)
#   
#   
#   ## Diameter
#   dd <- list.files(path = "output/", pattern = "power_log_diameter_nocomp_rs_") 
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   C_diameter_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   diameter_resampling_power <- C_diameter_resampling_power_log[C_diameter_resampling_power_log$species %in% new_sp_list,]
#   diameter_power <- diameter_resampling_power %>% dplyr::filter(weighted == "no") %>%
#                                                   dplyr::group_by(species) %>%
#                                                   dplyr::filter(!is.na(a1)) %>% 
#                                                   dplyr::slice_sample(n = 100) %>%
#                                                   dplyr::mutate(ID_b = X + 300) %>%
#                                                   dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
#   
#   diameter_power_b <- C_diameter_resampling_power_log %>% dplyr::filter(weighted == "yes") %>%
#                                                           dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
#   diameter_power_b <- diameter_power_b[diameter_power_b$ID_tofilter %in% diameter_power$ID_species,]
#   
#   diameter_power_results_b <- diameter_power_b %>% dplyr::group_by(species) %>%
#                                                    dplyr::summarise(mean_a1 = mean(a1),
#                                                                     sd_a1 = sd(a1),
#                                                                     mean_a2 = mean(a2),
#                                                                     sd_a2 = sd(a2),
#                                                                     mean_sigma = mean(sigma),
#                                                                     sd_sigma = sd(sigma),
#                                                                     total_AIC = sum(AIC),
#                                                                     total_RMSE = sum(RMSE))
#   
#   # computing diameter values from estimated parameters for different dbh 
#   for (i in 1:dim(diameter_power_results_b)[1]) {
#     
#     n_repetition = 100
#     
#     diameter_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5)) 
#     diameter_dbh[,1] <- rep(diameter_power_results_b[i,"species"], n_repetition)
#     names(diameter_dbh) <- c("species", "dbh_10", "dbh_20", "dbh_30", "dbh_40")
#     
#     mean_a1 = diameter_power_results_b$mean_a1[i]
#     sd_a1 = diameter_power_results_b$sd_a1[i]
#     
#     mean_a2 = diameter_power_results_b$mean_a2[i]
#     sd_a2 = diameter_power_results_b$sd_a2[i]
#     
#     mean_sigma = diameter_power_results_b$mean_sigma[i]
#     sd_sigma = diameter_power_results_b$sd_sigma[i]
#     
#     
#     for (j in 1:n_repetition) {
#       
#       a1 <- rnorm(1, mean = mean_a1, sd = sd_a1)
#       a2 <- rnorm(1, mean = mean_a2, sd = sd_a2)
#       sigma <- rnorm(1, mean = mean_sigma, sd = sd_sigma)
#       
#       diameter_dbh[j,"dbh_10"] = a1 * (10^a2) * ((1/2)*(exp(sigma^2)))
#       diameter_dbh[j,"dbh_20"] = a1 * (20^a2) * ((1/2)*(exp(sigma^2)))
#       diameter_dbh[j,"dbh_30"] = a1 * (30^a2) * ((1/2)*(exp(sigma^2)))
#       diameter_dbh[j,"dbh_40"] = a1 * (40^a2) * ((1/2)*(exp(sigma^2)))
#       
#     }
#     
#     write.csv(diameter_dbh, file =  paste0("output/diameter_dbh_", diameter_power_results_b[i,"species"], ".csv"))
#     
#   }
#   
#   
#   dd <- list.files(path = "output/", pattern = "diameter_dbh_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   diameter_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   write.csv(file = "output/diameter_dbh_allsp.csv", diameter_dbh_allsp)
#   
#   diameter_dbh_20 <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
#                                             dplyr::summarise(mean = mean(dbh_20, na.rm = TRUE),
#                                                              sd = sd(dbh_20, na.rm = TRUE), 
#                                                              se = se(dbh_20))
#   
#   diameter_dbh_30 <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
#                                             dplyr::summarise(mean = mean(dbh_30, na.rm = TRUE),
#                                                              sd = sd(dbh_30, na.rm = TRUE), 
#                                                              se = se(dbh_30))
#   
#   
#   dd <- list.files(path = "output/", pattern = "depth_dbh_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   depth_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   depth_dbh_allsp_b <- depth_dbh_allsp[depth_dbh_allsp$species %in% new_sp_list,]
#   depth_dbh_allsp_b <- depth_dbh_allsp_b %>% dplyr::rename(depth_dbh_10 = dbh_10, 
#                                                            depth_dbh_20 = dbh_20,
#                                                            depth_dbh_30 = dbh_30,
#                                                            depth_dbh_40 = dbh_40)
#   
# 
#   sp_complete_file <- read.csv(file = "output/sp_complete_datafile.csv")
#   sp_complete_file <- sp_complete_file %>% dplyr::select(species, group)
#   
#   volume_file <- left_join(depth_dbh_allsp_b, diameter_dbh_allsp, by = c("species", "X"))
#   volume_file_b <- left_join(volume_file, sp_complete_file, by = "species")
#   
#   crown_volume <- as.data.frame(matrix(nrow = (dim(volume_file_b)[1]), ncol = 5))
#   colnames(crown_volume) <- c("species", "dbh_10", "dbh_20", "dbh_30", "dbh_40")
#   
#   for (i in 1:dim(volume_file_b)[1]) {
#     
#     crown_volume[i,"species"] = volume_file_b[i,"species"]
#     
#     if (volume_file_b[i,"group"] %in% c("A", "B")) {
#       
#       crown_volume[i,"dbh_10"] = (pi * (volume_file_b[i,"dbh_10"]/2)^2 * volume_file_b[i,"depth_dbh_10"])/(2 * 0.506 + 1)
#       crown_volume[i,"dbh_20"] = (pi * (volume_file_b[i,"dbh_20"]/2)^2 * volume_file_b[i,"depth_dbh_20"])/(2 * 0.506 + 1)
#       crown_volume[i,"dbh_30"] = (pi * (volume_file_b[i,"dbh_30"]/2)^2 * volume_file_b[i,"depth_dbh_30"])/(2 * 0.506 + 1)
#       crown_volume[i,"dbh_40"] = (pi * (volume_file_b[i,"dbh_40"]/2)^2 * volume_file_b[i,"depth_dbh_40"])/(2 * 0.506 + 1)
#       
#       
#     } else {
#       
#       crown_volume[i,"dbh_10"] = (pi * (volume_file_b[i,"dbh_10"]/2)^2 * volume_file_b[i,"depth_dbh_10"])/(2 * 0.326 + 1)
#       crown_volume[i,"dbh_20"] = (pi * (volume_file_b[i,"dbh_20"]/2)^2 * volume_file_b[i,"depth_dbh_20"])/(2 * 0.326 + 1)
#       crown_volume[i,"dbh_30"] = (pi * (volume_file_b[i,"dbh_30"]/2)^2 * volume_file_b[i,"depth_dbh_30"])/(2 * 0.326 + 1)
#       crown_volume[i,"dbh_40"] = (pi * (volume_file_b[i,"dbh_40"]/2)^2 * volume_file_b[i,"depth_dbh_40"])/(2 * 0.326 + 1)
#       
#     }
#     
#   }
#   
#   crown_volume <- crown_volume[crown_volume$dbh_10 > 0,]
#   crown_volume <- crown_volume[crown_volume$dbh_20 > 0,]
#   crown_volume <- crown_volume[crown_volume$dbh_30 > 0,]
#   crown_volume <- crown_volume[crown_volume$dbh_40 > 0,]
#   
#   crown_volume_f <- crown_volume %>% dplyr::group_by(species) %>% 
#                                      dplyr::filter(dbh_10 < quantile(dbh_10, probs = 0.999),
#                                                    dbh_20 < quantile(dbh_20, probs = 0.999),
#                                                    dbh_30 < quantile(dbh_30, probs = 0.999),
#                                                    dbh_40 < quantile(dbh_40, probs = 0.999)) 
#   
#   write.csv(file = "output/crown_volume_dbh_allsp", crown_volume_f) 
#   
#   crown_volume_dbh_20 <- crown_volume_f %>% dplyr::group_by(species) %>%
#                                             dplyr::summarise(mean = mean(dbh_20, na.rm = TRUE),
#                                                              sd = sd(dbh_20, na.rm = TRUE), 
#                                                              se = se(dbh_20))
#   
#   crown_volume_dbh_30 <- crown_volume_f %>% dplyr::group_by(species) %>%
#                                             dplyr::summarise(mean = mean(dbh_30, na.rm = TRUE),
#                                                              sd = sd(dbh_30, na.rm = TRUE), 
#                                                              se = se(dbh_30))
#   
#   
#   ### Plotting results #1 (forest plots)
#   
#   height_max_f <- left_join(height_max_final, sp_complete_file, by = "species")
#   height_max_f <- height_max_f[height_max_f$group %in% c("A", "D"),]
#   height_max_f <- arrange(height_max_f, height_max_f$mean_Hmax)
#   
#   par(mfrow = c(dim(height_max_f)[1],1))
#   par(mar = c(0,0,0,0))
#   par(oma = c(4,15,2,0)) 
#   
#   plotx_hmax <- c(10, 80) # x range for height max values
#   sepr <- 0.2
#   clrs <- c("#2E8B57","#FFC000") # defining colors
#   
#   for (i in 1:dim(height_max_f)[1]) {
#     
#     plot(range(plotx_hmax), c(1, 2), type = "n", bty = "n",
#          ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
#          ylab = "", yaxt = "n", xlab = "", xaxt = "n")
#     
#     if (height_max_f[i,"group"] == "D") {
#       
#       polygon(x = c((height_max_f[i, "mean_Hmax"] - height_max_f[i, "se_Hmax"]), (height_max_f[i, "mean_Hmax"] - height_max_f[i, "se_Hmax"]), 
#                     (height_max_f[i, "mean_Hmax"] + height_max_f[i, "se_Hmax"]), (height_max_f[i, "mean_Hmax"] + height_max_f[i, "se_Hmax"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[1], border = clrs[1])
#       
#       mean = height_max_f[i, "mean_Hmax"]
#       mean_0 <- mean$mean_Hmax
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(height_max_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     } else {
#       
#       polygon(x = c((height_max_f[i, "mean_Hmax"] - height_max_f[i, "se_Hmax"]), (height_max_f[i, "mean_Hmax"] - height_max_f[i, "se_Hmax"]), 
#                     (height_max_f[i, "mean_Hmax"] + height_max_f[i, "se_Hmax"]), (height_max_f[i, "mean_Hmax"] + height_max_f[i, "se_Hmax"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[2], border = clrs[2])
#       
#       mean = height_max_f[i, "mean_Hmax"]
#       mean_0 <- mean$mean_Hmax
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(height_max_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     }
#     
#   }
#   
#   axis(side = 1, at = c(10, 20, 30, 40, 50, 60, 70, 80), 
#        labels = c("10", "20", "30", "40", "50", "60", "70", "80"),
#        cex.axis = 0.8) # x-axis
#   
#   
#   
#   crown_ratio_f <- left_join(height_max_f, crown_ratio_final, by = "species")
#   crown_ratio_f <- crown_ratio_f %>% dplyr::select(-mean_Hmax, -se_Hmax, -sd_Hmax, -X)
#   
#   
#   par(mfrow = c(dim(crown_ratio_f)[1],1))
#   par(mar = c(0,0,0,0))
#   par(oma = c(4,15,2,0)) 
#   
#   plotx_cr <- c(0, 1) # x range for crown ratio values
#   sepr <- 0.2
#   clrs <- c("#2E8B57","#FFC000") # defining colors
#   
#   for (i in 1:dim(crown_ratio_f)[1]) {
#     
#     plot(range(plotx_cr), c(1, 2), type = "n", bty = "n",
#          ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
#          ylab = "", yaxt = "n", xlab = "", xaxt = "n")
#     
#     if (crown_ratio_f[i,"group"] == "D") {
#       
#       polygon(x = c((crown_ratio_f[i, "mean_CR3"] - crown_ratio_f[i, "se_CR3"]), (crown_ratio_f[i, "mean_CR3"] - crown_ratio_f[i, "se_CR3"]), 
#                     (crown_ratio_f[i, "mean_CR3"] + crown_ratio_f[i, "se_CR3"]), (crown_ratio_f[i, "mean_CR3"] + crown_ratio_f[i, "se_CR3"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[1], border = clrs[1])
#       
#       mean = crown_ratio_f[i, "mean_CR3"]
#       mean_0 <- mean$mean_CR3
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(crown_ratio_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     } else {
#       
#       polygon(x = c((crown_ratio_f[i, "mean_CR3"] - crown_ratio_f[i, "se_CR3"]), (crown_ratio_f[i, "mean_CR3"] - crown_ratio_f[i, "se_CR3"]), 
#                     (crown_ratio_f[i, "mean_CR3"] + crown_ratio_f[i, "se_CR3"]), (crown_ratio_f[i, "mean_CR3"] + crown_ratio_f[i, "se_CR3"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[2], border = clrs[2])
#       
#       mean = crown_ratio_f[i, "mean_CR3"]
#       mean_0 <- mean$mean_CR3
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(crown_ratio_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     }
#     
#   }  
#   
#   axis(side = 1, at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), 
#        labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0"),
#        cex.axis = 0.8) # x-axis
#   
#   
#   crown_diameter_f <- left_join(height_max_f, diameter_dbh_30, by = "species")
#   crown_diameter_f <- crown_diameter_f %>% dplyr::select(-mean_Hmax, -se_Hmax, -sd_Hmax)
#   
#   
#   par(mfrow = c(dim(crown_diameter_f)[1],1))
#   par(mar = c(0,0,0,0))
#   par(oma = c(4,15,2,0)) 
#   
#   plotx_cd <- c(2, 6) # x range for crown diameter values
#   sepr <- 0.2
#   clrs <- c("#2E8B57","#FFC000") # defining colors
#   
#   for (i in 1:dim(crown_diameter_f)[1]) {
#     
#     plot(range(plotx_cd), c(1, 2), type = "n", bty = "n",
#          ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
#          ylab = "", yaxt = "n", xlab = "", xaxt = "n")
#     
#     if (crown_diameter_f[i,"group"] == "D") {
#       
#       polygon(x = c((crown_diameter_f[i, "mean"] - crown_diameter_f[i, "se"]), (crown_diameter_f[i, "mean"] - crown_diameter_f[i, "se"]), 
#                     (crown_diameter_f[i, "mean"] + crown_diameter_f[i, "se"]), (crown_diameter_f[i, "mean"] + crown_diameter_f[i, "se"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[1], border = clrs[1])
#       
#       mean = crown_diameter_f[i, "mean"]
#       mean_0 <- mean$mean
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(crown_diameter_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     } else {
#       
#       polygon(x = c((crown_diameter_f[i, "mean"] - crown_diameter_f[i, "se"]), (crown_diameter_f[i, "mean"] - crown_diameter_f[i, "se"]), 
#                     (crown_diameter_f[i, "mean"] + crown_diameter_f[i, "se"]), (crown_diameter_f[i, "mean"] + crown_diameter_f[i, "se"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[2], border = clrs[2])
#       
#       mean = crown_diameter_f[i, "mean"]
#       mean_0 <- mean$mean
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(crown_diameter_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     }
#     
#   }  
#   
#   axis(side = 1, at = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6), 
#        labels = c("2", "2.5", "3", "3.5", "4", "4.5", "5", "5.5", "6"),
#        cex.axis = 0.8) # x-axis
#   
#   
#   
#   crown_volume_f <- left_join(height_max_f, crown_volume_dbh_30, by = "species")
#   crown_volume_f <- crown_volume_f %>% dplyr::select(-mean_Hmax, -se_Hmax, -sd_Hmax)
#   
#   
#   par(mfrow = c(dim(crown_volume_f)[1],1))
#   par(mar = c(0,0,0,0))
#   par(oma = c(4,15,2,0)) 
#   
#   plotx_cv <- c(0, 350) # x range for crown volume values
#   sepr <- 0.2
#   clrs <- c("#2E8B57","#FFC000") # defining colors
#   
#   for (i in 1:dim(crown_volume_f)[1]) {
#     
#     plot(range(plotx_cv), c(1, 2), type = "n", bty = "n",
#          ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
#          ylab = "", yaxt = "n", xlab = "", xaxt = "n")
#     
#     if (crown_volume_f[i,"group"] == "D") {
#       
#       polygon(x = c((crown_volume_f[i, "mean"] - crown_volume_f[i, "se"]), (crown_volume_f[i, "mean"] - crown_volume_f[i, "se"]), 
#                     (crown_volume_f[i, "mean"] + crown_volume_f[i, "se"]), (crown_volume_f[i, "mean"] + crown_volume_f[i, "se"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[1], border = clrs[1])
#       
#       mean = crown_volume_f[i, "mean"]
#       mean_0 <- mean$mean
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(crown_volume_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     } else {
#       
#       polygon(x = c((crown_volume_f[i, "mean"] - crown_volume_f[i, "se"]), (crown_volume_f[i, "mean"] - crown_volume_f[i, "se"]), 
#                     (crown_volume_f[i, "mean"] + crown_volume_f[i, "se"]), (crown_volume_f[i, "mean"] + crown_volume_f[i, "se"])), 
#               y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
#               col = clrs[2], border = clrs[2])
#       
#       mean = crown_volume_f[i, "mean"]
#       mean_0 <- mean$mean
#       segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
#       
#       mtext(crown_volume_f[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.6) 
#       
#     }
#     
#   }  
#   
#   axis(side = 1, at = c(0, 50, 100, 150, 200, 250, 300, 350), 
#        labels = c("0", "50", "100", "150", "200", "250", "300", "350"),
#        cex.axis = 0.8) # x-axis
#   
#   
#   
#   ### Plotting results #2 (correlograms)
#   
#   # correlogram: all species
#   crown_ratio_f <- crown_ratio_f %>% dplyr::select(-group, -mean_CR2, -sd_CR2, -se_CR2)
#   all_info_file <- left_join(height_max_f, crown_ratio_f, by = "species")
#   all_info_file <- all_info_file %>% dplyr::rename(mean_CR = mean_CR3, se_CR = se_CR3, sd_CR = sd_CR3)
#   
#   crown_diameter_f <- crown_diameter_f %>% dplyr::select(-group) %>%
#                                            dplyr::rename(mean_CD = mean, sd_CD = sd, se_CD = se)
#   all_info_file <- left_join(all_info_file, crown_diameter_f, by = "species")
#   
#   crown_volume_f <- crown_volume_f %>% dplyr::select(-group) %>%
#                                        dplyr::rename(mean_CV = mean, sd_CV = sd, se_CV = se)
#   all_info_file <- left_join(all_info_file, crown_volume_f, by = "species")
#   
#   all_species_file <- all_info_file %>% dplyr::select(-group)
#   write.csv(file = "output/all_parameters.csv", all_species_file)
#   
#   
#   deciduous_angiosperm <- all_info_file %>% dplyr::filter(group == "A") %>%
#                                             dplyr::select(-group)
#   
#   evergreen_gymnosperm <- all_info_file %>% dplyr::filter(group == "D") %>%
#                                             dplyr::select(-group)
#   
#   
#   # Function to compute significativity
#   cor.mtest <- function(mat, ...) {
#     mat <- as.matrix(mat)
#     n <- ncol(mat)
#     p.mat<- matrix(NA, n, n)
#     diag(p.mat) <- 0
#     for (i in 1:(n - 1)) {
#       for (j in (i + 1):n) {
#         tmp <- cor.test(mat[, i], mat[, j], ...)
#         p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#       }
#     }
#     colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#     p.mat
#   }
#   
#   ## Running correlations
#   rows = 4
#   cols = 4
#   n_rep = 100
#   cor_results_pearson <- array(0,c(rows, cols, n_rep))   
#   cor_results_kendall <- array(0,c(rows, cols, n_rep))
#   cor_sign <- array(0,c(rows, cols, n_rep))   
# 
#   
#   
#   for (j in 1:n_rep) {
#     
#     data <- as.data.frame(matrix(nrow = dim(all_species_file)[1], ncol = 4))
#     colnames(data) <- c("Hmax", "CR", "CD", "CV")
#     
#     for (i in 1:dim(all_species_file)[1]) {
#       
#       mean_Hmax = all_species_file[i,"mean_Hmax"]$mean_Hmax
#       sd_Hmax = all_species_file[i,"sd_Hmax"]$sd_Hmax
#       
#       mean_CR = all_species_file[i,"mean_CR"]$mean_CR
#       sd_CR = all_species_file[i,"sd_CR"]$sd_CR
#       
#       mean_CD = all_species_file[i,"mean_CD"]$mean_CD
#       sd_CD = all_species_file[i,"sd_CD"]$sd_CD
#       
#       mean_CV = all_species_file[i,"mean_CV"]$mean_CV
#       sd_CV = all_species_file[i,"sd_CV"]$sd_CV
#       
# 
#       data[i,"Hmax"] <- rnorm(1, mean = mean_Hmax, sd = sd_Hmax)
#       data[i,"CR"] <- rnorm(1, mean = mean_CR, sd = sd_CR)
#       data[i,"CD"] <- rnorm(1, mean = mean_CD, sd = sd_CD)
#       data[i,"CV"] <- rnorm(1, mean = mean_CV, sd = sd_CV)
#       
#     }
#     
#     cor_results_pearson[,,j] <- cor(data, method = "pearson")
#     cor_results_kendall[,,j] <- cor(data, method = "kendall")
#   
#   }
# 
# 
#   mean_pearson_matrix <- matrix(nrow = rows, ncol = cols)
#   mean_kendall_matrix <- matrix(nrow = rows, ncol = cols)
# 
#   
#   for (i in 1:4) {
#     for (j in 1:4) {
#       
#       mean_pearson_matrix[i,j] <- mean(cor_results_pearson[i,j,])
#       mean_kendall_matrix[i,j] <- mean(cor_results_kendall[i,j,])
#   
#     }
#   }
#   
#   
# 
#   par(mar = c(0,0,0,0))
#   par(mfrow = c(1,1))
#   par(oma = c(2,2,2,2))
#   
#   corrplot(mean_pearson_matrix, method = "circle", type = "upper", 
#            col = brewer.pal(n = 10, name = "RdYlBu"),
#            tl.col = "black", tl.srt = 45)
#   corrplot(mean_kendall_matrix, method = "circle", type = "upper", 
#            col = brewer.pal(n = 10, name = "RdYlBu"),
#            tl.col = "black", tl.srt = 45)
#   
#   
#   
#   ## Running correlations - Deciduous Angiosperm
#   rows = 4
#   cols = 4
#   n_rep = 100
#   cor_results_pearson_A <- array(0,c(rows, cols, n_rep))   
#   cor_results_kendall_A <- array(0,c(rows, cols, n_rep))
#   cor_sign <- array(0,c(rows, cols, n_rep))   
#   
#   
#   
#   for (j in 1:n_rep) {
#     
#     data <- as.data.frame(matrix(nrow = dim(deciduous_angiosperm)[1], ncol = 4))
#     colnames(data) <- c("Hmax", "CR", "CD", "CV")
#     
#     for (i in 1:dim(deciduous_angiosperm)[1]) {
#       
#       mean_Hmax = deciduous_angiosperm[i,"mean_Hmax"]$mean_Hmax
#       sd_Hmax = deciduous_angiosperm[i,"sd_Hmax"]$sd_Hmax
#       
#       mean_CR = deciduous_angiosperm[i,"mean_CR"]$mean_CR
#       sd_CR = deciduous_angiosperm[i,"sd_CR"]$sd_CR
#       
#       mean_CD = deciduous_angiosperm[i,"mean_CD"]$mean_CD
#       sd_CD = deciduous_angiosperm[i,"sd_CD"]$sd_CD
#       
#       mean_CV = deciduous_angiosperm[i,"mean_CV"]$mean_CV
#       sd_CV = deciduous_angiosperm[i,"sd_CV"]$sd_CV
#       
#       
#       data[i,"Hmax"] <- rnorm(1, mean = mean_Hmax, sd = sd_Hmax)
#       data[i,"CR"] <- rnorm(1, mean = mean_CR, sd = sd_CR)
#       data[i,"CD"] <- rnorm(1, mean = mean_CD, sd = sd_CD)
#       data[i,"CV"] <- rnorm(1, mean = mean_CV, sd = sd_CV)
#       
#     }
#     
#     cor_results_pearson_A[,,j] <- cor(data, method = "pearson")
#     cor_results_kendall_A[,,j] <- cor(data, method = "kendall")
#     
#   }
#   
#   
#   mean_pearson_matrix_A <- matrix(nrow = rows, ncol = cols)
#   mean_kendall_matrix_A <- matrix(nrow = rows, ncol = cols)
# 
#   
#   for (i in 1:4) {
#     for (j in 1:4) {
#       
#       mean_pearson_matrix_A[i,j] <- mean(cor_results_pearson_A[i,j,])
#       mean_kendall_matrix_A[i,j] <- mean(cor_results_kendall_A[i,j,])
#       
#     }
#   }
#   
#   
#   
#   par(mar = c(0,0,0,0))
#   par(mfrow = c(1,1))
#   par(oma = c(2,2,2,2))
#   
#   corrplot(mean_pearson_matrix_A, method = "circle", type = "upper", 
#            col = brewer.pal(n = 10, name = "RdYlBu"),
#            tl.col = "black", tl.srt = 45)
#   corrplot(mean_kendall_matrix_A, method = "circle", type = "upper", 
#            col = brewer.pal(n = 10, name = "RdYlBu"),
#            tl.col = "black", tl.srt = 45)
#   
#   
#   
#   ## Running correlations - Evergreen Gymnosperm
#   rows = 4
#   cols = 4
#   n_rep = 100
#   cor_results_pearson_D <- array(0,c(rows, cols, n_rep))   
#   cor_results_kendall_D <- array(0,c(rows, cols, n_rep))
#   cor_sign <- array(0,c(rows, cols, n_rep))   
#   
#   
#   
#   for (j in 1:n_rep) {
#     
#     data <- as.data.frame(matrix(nrow = dim(evergreen_gymnosperm)[1], ncol = 4))
#     colnames(data) <- c("Hmax", "CR", "CD", "CV")
#     
#     for (i in 1:dim(evergreen_gymnosperm)[1]) {
#       
#       mean_Hmax = evergreen_gymnosperm[i,"mean_Hmax"]$mean_Hmax
#       sd_Hmax = evergreen_gymnosperm[i,"sd_Hmax"]$sd_Hmax
#       
#       mean_CR = evergreen_gymnosperm[i,"mean_CR"]$mean_CR
#       sd_CR = evergreen_gymnosperm[i,"sd_CR"]$sd_CR
#       
#       mean_CD = evergreen_gymnosperm[i,"mean_CD"]$mean_CD
#       sd_CD = evergreen_gymnosperm[i,"sd_CD"]$sd_CD
#       
#       mean_CV = evergreen_gymnosperm[i,"mean_CV"]$mean_CV
#       sd_CV = evergreen_gymnosperm[i,"sd_CV"]$sd_CV
#       
#       
#       data[i,"Hmax"] <- rnorm(1, mean = mean_Hmax, sd = sd_Hmax)
#       data[i,"CR"] <- rnorm(1, mean = mean_CR, sd = sd_CR)
#       data[i,"CD"] <- rnorm(1, mean = mean_CD, sd = sd_CD)
#       data[i,"CV"] <- rnorm(1, mean = mean_CV, sd = sd_CV)
#       
#     }
#     
#     cor_results_pearson_D[,,j] <- cor(data, method = "pearson")
#     cor_results_kendall_D[,,j] <- cor(data, method = "kendall")
#     
#   }
#   
#   
#   mean_pearson_matrix_D <- matrix(nrow = rows, ncol = cols)
#   mean_kendall_matrix_D <- matrix(nrow = rows, ncol = cols)
# 
#   
#   for (i in 1:4) {
#     for (j in 1:4) {
#       
#       mean_pearson_matrix_D[i,j] <- mean(cor_results_pearson_D[i,j,])
#       mean_kendall_matrix_D[i,j] <- mean(cor_results_kendall_D[i,j,])
#       
#     }
#   }
#   
#   
#   
#   par(mar = c(0,0,0,0))
#   par(mfrow = c(1,1))
#   par(oma = c(2,2,2,2))
#   
#   corrplot(mean_pearson_matrix_D, method = "circle", type = "upper", 
#            col = brewer.pal(n = 10, name = "RdYlBu"),
#            tl.col = "black", tl.srt = 45)
#   corrplot(mean_kendall_matrix_D, method = "circle", type = "upper", 
#            col = brewer.pal(n = 10, name = "RdYlBu"),
#            tl.col = "black", tl.srt = 45)
#   
#   
#   
#  
#   
#   
#   
#   
#   dd <- list.files(path = "output/", pattern = "power_log_depth_c1_rs_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   F_depth_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   
#   dd <- list.files(path = "output/", pattern = "power_log_depth_c2_rs_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   I_depth_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   
#   dd <- list.files(path = "output/", pattern = "power_log_diameter_c1_rs_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   F_diameter_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   
#   dd <- list.files(path = "output/", pattern = "power_log_diameter_c2_rs_")
#   data_list <- lapply(paste0("output/", dd), utils::read.table,
#                       header = TRUE, sep = ",", dec = ".",
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#   
#   I_diameter_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   
#   depth_c1 <- F_depth_resampling_power_c1_log[F_depth_resampling_power_c1_log$species %in% new_sp_list,]
#   depth_c1_b <- depth_c1 %>% dplyr::filter(weighted == "no") %>%
#                              dplyr::group_by(species) %>%
#                              dplyr::filter(!is.na(a1)) %>% 
#                              dplyr::slice_sample(n = 100) %>%
#                              dplyr::mutate(ID_b = X + 300) %>%
#                              dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
#   
#   depth_c1_c <- F_depth_resampling_power_c1_log %>% dplyr::filter(weighted == "yes") %>%
#                                                     dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
#   depth_c1_c <- depth_c1_c[depth_c1_c$ID_tofilter %in% depth_c1_b$ID_species,]
#   
#   depth_c1_results <- depth_c1_c %>% dplyr::group_by(species) %>%
#                                      dplyr::summarise(mean_a1 = mean(a1),
#                                                       sd_a1 = sd(a1),
#                                                       mean_a2 = mean(a2),
#                                                       sd_a2 = sd(a2),
#                                                       mean_ba_plot = mean(comp),
#                                                       sd_ba_plot = sd(comp),
#                                                       mean_sigma = mean(sigma),
#                                                       sd_sigma = sd(sigma),
#                                                       total_AIC = sum(AIC),
#                                                       total_RMSE = sum(RMSE))
#   
#   
#   depth_c2 <- I_depth_resampling_power_c2_log[I_depth_resampling_power_c2_log$species %in% new_sp_list,]
#   depth_c2_b <- depth_c2 %>% dplyr::filter(weighted == "no") %>%
#                              dplyr::group_by(species) %>%
#                              dplyr::filter(!is.na(a1)) %>% 
#                              dplyr::slice_sample(n = 100) %>%
#                              dplyr::mutate(ID_b = X + 300) %>%
#                              dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
#   
#   depth_c2_c <- I_depth_resampling_power_c2_log %>% dplyr::filter(weighted == "yes") %>%
#                                                     dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
#   depth_c2_c <- depth_c2_c[depth_c2_c$ID_tofilter %in% depth_c2_b$ID_species,]
#   
#   depth_c2_results <- depth_c2_c %>% dplyr::group_by(species) %>%
#                                      dplyr::summarise(mean_a1 = mean(a1),
#                                                       sd_a1 = sd(a1),
#                                                       mean_a2 = mean(a2),
#                                                       sd_a2 = sd(a2),
#                                                       mean_ba_larger = mean(comp),
#                                                       sd_ba_larger = sd(comp),
#                                                       mean_sigma = mean(sigma),
#                                                       sd_sigma = sd(sigma),
#                                                       total_AIC = sum(AIC),
#                                                       total_RMSE = sum(RMSE))
#   
#   
#   diameter_c1 <- F_diameter_resampling_power_c1_log[F_diameter_resampling_power_c1_log$species %in% new_sp_list,]
#   diameter_c1_b <- diameter_c1 %>% dplyr::filter(weighted == "no") %>%
#                                    dplyr::group_by(species) %>%
#                                    dplyr::filter(!is.na(a1)) %>% 
#                                    dplyr::slice_sample(n = 100) %>%
#                                    dplyr::mutate(ID_b = X + 300) %>%
#                                    dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
#   
#   diameter_c1_c <- F_diameter_resampling_power_c1_log %>% dplyr::filter(weighted == "yes") %>%
#                                                           dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
#   diameter_c1_c <- diameter_c1_c[diameter_c1_c$ID_tofilter %in% diameter_c1_b$ID_species,]
#   
#   diameter_c1_results <- diameter_c1_c %>% dplyr::group_by(species) %>%
#                                            dplyr::summarise(mean_a1 = mean(a1),
#                                                            sd_a1 = sd(a1),
#                                                            mean_a2 = mean(a2),
#                                                            sd_a2 = sd(a2),
#                                                            mean_ba_plot = mean(comp),
#                                                            sd_ba_plot = sd(comp),
#                                                            mean_sigma = mean(sigma),
#                                                            sd_sigma = sd(sigma),
#                                                            total_AIC = sum(AIC),
#                                                            total_RMSE = sum(RMSE))
#   
#   
#   diameter_c2 <- I_diameter_resampling_power_c2_log[I_diameter_resampling_power_c2_log$species %in% new_sp_list,]
#   diameter_c2_b <- diameter_c2 %>% dplyr::filter(weighted == "no") %>%
#                                     dplyr::group_by(species) %>%
#                                     dplyr::filter(!is.na(a1)) %>% 
#                                     dplyr::slice_sample(n = 100) %>%
#                                     dplyr::mutate(ID_b = X + 300) %>%
#                                     dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))
#   
#   diameter_c2_c <- I_diameter_resampling_power_c2_log %>% dplyr::filter(weighted == "yes") %>%
#                                                           dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
#   diameter_c2_c <- diameter_c2_c[diameter_c2_c$ID_tofilter %in% diameter_c2_b$ID_species,]
#   
#   diameter_c2_results <- diameter_c2_c %>% dplyr::group_by(species) %>%
#                                           dplyr::summarise(mean_a1 = mean(a1),
#                                                            sd_a1 = sd(a1),
#                                                            mean_a2 = mean(a2),
#                                                            sd_a2 = sd(a2),
#                                                            mean_ba_larger = mean(comp),
#                                                            sd_ba_larger = sd(comp),
#                                                            mean_sigma = mean(sigma),
#                                                            sd_sigma = sd(sigma),
#                                                            total_AIC = sum(AIC),
#                                                            total_RMSE = sum(RMSE))
#   
#   
#   
#   depth_c1_f <- depth_c1_results %>% dplyr::select(species, mean_a1, mean_a2, mean_ba_plot, mean_sigma) %>%
#                                      dplyr::rename(a1_depth = mean_a1, a2_depth = mean_a2, comp_depth = mean_ba_plot, sigma_depth = mean_sigma)
#   
#   diameter_c1_f <- diameter_c1_results %>% dplyr::select(species, mean_a1, mean_a2, mean_ba_plot, mean_sigma) %>%
#                                            dplyr::rename(a1_diameter = mean_a1, a2_diameter = mean_a2, comp_diameter = mean_ba_plot, sigma_diameter = mean_sigma)
#   
#   volume_c1 <- left_join(depth_c1_f, diameter_c1_f, by = "species")
#   volume_c1 <- na.omit(volume_c1)
#   volume_c1 <- left_join(volume_c1, sp_complete_file, by = "species")
#   
#   c1_range <- c(15:30)
#   volume_c1_store <- as.data.frame(matrix(nrow = dim(volume_c1)[1], ncol = length(c1_range + 1)))
#   
#   for (j in 1:dim(volume_c1_store)[1]) {
#     
#     volume_c1_store[j,17] = volume_c1[j,"species"]
#     
#     for (i in 1:length(c1_range)) {
#       
#       if (volume_c1[j,"group"] %in% c("A", "B")) {
#       
#       diameter <- exp(volume_c1[j,"a1_diameter"] + (volume_c1[j,"comp_diameter"] * c1_range[i])) * (30 ^ volume_c1[j,"a2_diameter"]) *  ((1/2)*(exp(volume_c1[j,"sigma_diameter"]^2)))
#       diameter <- diameter$a1_diameter
#       
#       depth <- exp(volume_c1[j,"a1_depth"] + (volume_c1[j,"comp_depth"] * c1_range[i])) * (30 ^ volume_c1[j,"a2_depth"]) *  ((1/2)*(exp(volume_c1[j,"sigma_depth"]^2)))
#       depth <- depth$a1_depth
#       
#       volume_c1_store[j,i] <- (pi * (diameter/2)^2 * depth)/(2 * 0.506 + 1)
#       
#       } else {
#         
#         diameter <- exp(volume_c1[j,"a1_diameter"] + (volume_c1[j,"comp_diameter"] * c1_range[i])) * (30 ^ volume_c1[j,"a2_diameter"]) *  ((1/2)*(exp(volume_c1[j,"sigma_diameter"]^2)))
#         diameter <- diameter$a1_diameter
#         
#         depth <- exp(volume_c1[j,"a1_depth"] + (volume_c1[j,"comp_depth"] * c1_range[i])) * (30 ^ volume_c1[j,"a2_depth"]) *  ((1/2)*(exp(volume_c1[j,"sigma_depth"]^2)))
#         depth <- depth$a1_depth
#         
#         volume_c1_store[j,i] <- (pi * (diameter/2)^2 * depth)/(2 * 0.326 + 1)
#       
#       }
#       
#     }
#   }
#   
#   
#   volume_c1_store <- left_join(volume_c1_store, sp_complete_file, by = "species")
#   volume_c1_store <- volume_c1_store[volume_c1_store$species != "Fagus grandifolia",]
#   volume_c1_store_b <- volume_c1_store[,-c(17,18)]
# 
#   
#   
#   par(mar = c(4,4,0,0))
#   par(mfrow = c(1,1))
#   par(oma = c(2,2,2,2))
#   
#   clrs <- c("#2E8B57","#FFC000") # defining colors
#   
#   
#   plot(c1_range, volume_c1_store_b[1,], type = "l", lwd = 1.2, 
#        ylim = c(10, 120), xlim = c(15, 30),
#        ylab = "", yaxt = "n", xlab = "", xaxt = "n", bty = "n",
#        col = "white")
#   
#   for (i in 1:dim(volume_c1_store_b)[1]) {
#     
#     if (volume_c1_store[i,"group"] %in% "D") {
#       lines(c1_range, volume_c1_store_b[i,], type = "l", lwd = 1.5, col = clrs[1])
#     } else {
#       lines(c1_range, volume_c1_store_b[i,], type = "l", lwd = 1.5, col = clrs[2])
#     }
#     
#   }
#   
#   axis(side = 1, at = c(15, 20, 25, 30), labels = c("15", "20", "25", "30"), cex.axis = 1.2) # x-axis
#   axis(side = 2, at = c(0, 20, 40, 60, 80, 100, 120), labels = c("0", "20", "40", "60", "80", "100", "120"), cex.axis = 1.2) # x-axis
#   
#     
#     
#     
#   }
#     
#     
#         
#         
#         
#         for (i in 1:dim(volume_file_b)[1]) {
#           
#           crown_volume[i,"species"] = volume_file_b[i,"species"]
#           
#           if (volume_file_b[i,"group"] %in% c("A", "B")) {
#             
#             crown_volume[i,"dbh_10"] = (pi * (volume_file_b[i,"dbh_10"]/2)^2 * volume_file_b[i,"depth_dbh_10"])/(2 * 0.506 + 1)
#             crown_volume[i,"dbh_20"] = (pi * (volume_file_b[i,"dbh_20"]/2)^2 * volume_file_b[i,"depth_dbh_20"])/(2 * 0.506 + 1)
#             crown_volume[i,"dbh_30"] = (pi * (volume_file_b[i,"dbh_30"]/2)^2 * volume_file_b[i,"depth_dbh_30"])/(2 * 0.506 + 1)
#             crown_volume[i,"dbh_40"] = (pi * (volume_file_b[i,"dbh_40"]/2)^2 * volume_file_b[i,"depth_dbh_40"])/(2 * 0.506 + 1)
#             
#             
#           } else {
#             
#             crown_volume[i,"dbh_10"] = (pi * (volume_file_b[i,"dbh_10"]/2)^2 * volume_file_b[i,"depth_dbh_10"])/(2 * 0.326 + 1)
#             crown_volume[i,"dbh_20"] = (pi * (volume_file_b[i,"dbh_20"]/2)^2 * volume_file_b[i,"depth_dbh_20"])/(2 * 0.326 + 1)
#             crown_volume[i,"dbh_30"] = (pi * (volume_file_b[i,"dbh_30"]/2)^2 * volume_file_b[i,"depth_dbh_30"])/(2 * 0.326 + 1)
#             crown_volume[i,"dbh_40"] = (pi * (volume_file_b[i,"dbh_40"]/2)^2 * volume_file_b[i,"depth_dbh_40"])/(2 * 0.326 + 1)
#             
#           }
#           
#         }
# 
# 
# 
# ## Running correlations
# rows = 5
# cols = 5
# n_rep = 100
# cor_results_pearson <- array(0,c(rows, cols, n_rep))   
# cor_results_kendall <- array(0,c(rows, cols, n_rep))
# cor_sign <- array(0,c(rows, cols, n_rep))   
# 
# 
# 
# for (j in 1:n_rep) {
#   
#   data <- as.data.frame(matrix(nrow = dim(all_species_file)[1], ncol = 4))
#   colnames(data) <- c("Hmax", "CR", "CD", "CV")
#   
#   for (i in 1:dim(all_species_file)[1]) {
#     
#     mean_Hmax = all_species_file[i,"mean_Hmax"]$mean_Hmax
#     sd_Hmax = all_species_file[i,"sd_Hmax"]$sd_Hmax
#     
#     mean_CR = all_species_file[i,"mean_CR"]$mean_CR
#     sd_CR = all_species_file[i,"sd_CR"]$sd_CR
#     
#     mean_CD = all_species_file[i,"mean_CD"]$mean_CD
#     sd_CD = all_species_file[i,"sd_CD"]$sd_CD
#     
#     mean_CV = all_species_file[i,"mean_CV"]$mean_CV
#     sd_CV = all_species_file[i,"sd_CV"]$sd_CV
#     
#     
#     data[i,"Hmax"] <- rnorm(1, mean = mean_Hmax, sd = sd_Hmax)
#     data[i,"CR"] <- rnorm(1, mean = mean_CR, sd = sd_CR)
#     data[i,"CD"] <- rnorm(1, mean = mean_CD, sd = sd_CD)
#     data[i,"CV"] <- rnorm(1, mean = mean_CV, sd = sd_CV)
#     
#   }
#   
#   cor_results_pearson[,,j] <- cor(data, method = "pearson")
#   cor_results_kendall[,,j] <- cor(data, method = "kendall")
#   
# }
# 
# 
# mean_pearson_matrix <- matrix(nrow = rows, ncol = cols)
# mean_kendall_matrix <- matrix(nrow = rows, ncol = cols)
# 
# 
# for (i in 1:4) {
#   for (j in 1:4) {
#     
#     mean_pearson_matrix[i,j] <- mean(cor_results_pearson[i,j,])
#     mean_kendall_matrix[i,j] <- mean(cor_results_kendall[i,j,])
#     
#   }
# }
# 
# 
# 
# par(mar = c(0,0,0,0))
# par(mfrow = c(1,1))
# par(oma = c(2,2,2,2))
# 
# corrplot(mean_pearson_matrix, method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# corrplot(mean_kendall_matrix, method = "circle", type = "upper", 
#          col = brewer.pal(n = 10, name = "RdYlBu"),
#          tl.col = "black", tl.srt = 45)
# 
#       
#     
#   }
#   
#   
#   
#   
#   
#   
#   
#   
# 
#                                                               
#                                                                     
#   
#   
#   
#   
#   
#   
#   
#   
#   
# 
#   
#   
# }
#   
#   
#   
#   
#   
#   
#   
# 
# 
# 
# 
#   
#   
#   
#   
#   
#   
# 
#   

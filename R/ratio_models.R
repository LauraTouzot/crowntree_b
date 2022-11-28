ratio_resampling_nocomp <- function(ratio_data, ratio_species) {
  
  ## loading data and species list
  data_ok <- ratio_data
  species_list <- ratio_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == ratio_species]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  beta_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  beta_resampling_nocomp[,1] <- rep(ratio_species, n_repetition)
  names(beta_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "zvalue", "pr_z", "AIC", "RMSE", "weighted")
  
  beta_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  beta_resampling_nocomp_w[,1] <- rep(ratio_species, n_repetition)
  names(beta_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "zvalue", "pr_z", "AIC", "RMSE", "weighted")
  
  # beta_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 2)) 
  # names(beta_dbh) <- c("species", "dbh_15_w")
  # beta_dbh[,"species"] <- ratio_species
  
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == species_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x > 10 & y > 0 & y < 1) %>%
                      dplyr::select(x, y, location, protocol, id) %>%
                      dplyr:: mutate(x = as.numeric(x), y = as.numeric(y), 
                                     location = as.factor(droplevels.factor(location)), 
                                     protocol = as.factor(droplevels.factor(protocol)), id = as.numeric(id))
  
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_beta_ratio_nocomp_rs <- mod_beta_resampling_nocomp(ranged_data, nb_datasets_all, sample_size, beta_resampling_nocomp, beta_resampling_nocomp_w, beta_dbh, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_beta_ratio_nocomp_rs, file =  paste0("output/beta_ratio_nocomp_rs_", ratio_species, ".csv"))
  
}



ratio_resampling_nocomp_mean <- function(ratio_data, ratio_species) {
  
  ## loading data and species list
  data_ok <- ratio_data
  species_list <- ratio_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == ratio_species]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  beta_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 4)) 
  beta_resampling_nocomp[,1] <- rep(ratio_species[i], n_repetition)
  names(beta_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "RMSE", "weighted")
  
  beta_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 4)) 
  beta_resampling_nocomp_w[,1] <- rep(ratio_species[i], n_repetition)
  names(beta_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "RMSE", "weighted")
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == species_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x > 10 & y > 0 & y < 1) %>%
                      dplyr::select(x, y, location, protocol, id) %>%
                      dplyr:: mutate(x = as.numeric(x), y = as.numeric(y), 
                                     location = as.factor(droplevels.factor(location)), 
                                     protocol = as.factor(droplevels.factor(protocol)), id = as.numeric(id))
  
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_beta_ratio_nocomp_mean_rs <- mod_beta_resampling_nocomp_mean(ranged_data, nb_datasets_all, sample_size, beta_resampling_nocomp, beta_resampling_nocomp_w, beta_dbh, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_beta_ratio_nocomp_mean_rs, file =  paste0("output/beta_ratio_nocomp_mean_rs_", ratio_species, ".csv"))
  
}




ratio_resampling_c1 <- function(ratio_data, ratio_species_comp) {
  
  ## loading data and species list
  data_ok <- ratio_data
  new_sp_list <- ratio_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == ratio_species_comp]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  beta_resampling_c1 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 9)) 
  beta_resampling_c1[,1] <- rep(new_sp_list, n_repetition)
  names(beta_resampling_c1) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "a2", "zvalue", "pr_z", "AIC", "RMSE", "weighted")
  
  beta_resampling_c1_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 9)) 
  beta_resampling_c1_w[,1] <- rep(new_sp_list, n_repetition)
  names(beta_resampling_c1_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "a2", "zvalue", "pr_z", "AIC", "RMSE", "weighted")
  
  # beta_dbh_c1 <- as.data.frame(matrix(nrow = n_repetition, ncol = 3)) 
  # names(beta_dbh_c1) <- c("species", "dbh_15_w_a", "dbh_15_w_b")
  # beta_dbh_c1[,"species"] <- new_sp_list[i]
  
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == new_sp_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & y < 1 & !is.na(ba_plot) & !is.na(ba_larger)) %>%
                      dplyr::select(x, y, location, protocol, id, ba_plot, ba_larger) %>%
                      dplyr::mutate(x = as.numeric(x), y = as.numeric(y), 
                                    location = as.factor(droplevels.factor(location)), 
                                    protocol = as.factor(droplevels.factor(protocol)), 
                                    id = as.numeric(id), ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger))
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (competition - ba plot - resampling)
  output_beta_ratio_c1_rs <- mod_beta_resampling_c1(ranged_data, nb_datasets_all, sample_size, beta_resampling_c1, beta_resampling_c1_w, beta_dbh_c1, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_beta_ratio_c1_rs, file =  paste0("output/beta_ratio_c1_rs_", new_sp_list[i], ".csv"))
  
}



ratio_resampling_mean_c1 <- function(ratio_data, ratio_species_comp) {
  
  ## loading data and species list
  data_ok <- ratio_data
  new_sp_list <- ratio_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == ratio_species_comp]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  beta_resampling_c1 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
  beta_resampling_c1[,1] <- rep(new_sp_list[i], n_repetition)
  names(beta_resampling_c1) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "RMSE", "weighted")
  
  beta_resampling_c1_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
  beta_resampling_c1_w[,1] <- rep(new_sp_list[i], n_repetition)
  names(beta_resampling_c1_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "RMSE", "weighted")


  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == new_sp_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & y < 1 & !is.na(ba_plot) & !is.na(ba_larger)) %>%
                      dplyr::select(x, y, location, protocol, id, ba_plot, ba_larger) %>%
                      dplyr::mutate(x = as.numeric(x), y = as.numeric(y), 
                                    location = as.factor(droplevels.factor(location)), 
                                    protocol = as.factor(droplevels.factor(protocol)), 
                                    id = as.numeric(id), ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger))
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (competition - ba plot - resampling)
  output_beta_ratio_c1_mean_rs <- mod_beta_resampling_mean_c1(ranged_data, nb_datasets_all, sample_size, beta_resampling_c1, beta_resampling_c1_w, beta_dbh_c1, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_beta_ratio_c1_mean_rs, file =  paste0("output/beta_ratio_c1_mean_rs_", new_sp_list[i], ".csv"))
  
}



ratio_resampling_c2 <- function(ratio_data, ratio_species_comp) {
  
  ## loading data and species list
  data_ok <- ratio_data
  new_sp_list <- ratio_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == ratio_species_comp]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  beta_resampling_c2 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 9)) 
  beta_resampling_c2[,1] <- rep(new_sp_list, n_repetition)
  names(beta_resampling_c2) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "a2", "zvalue", "pr_z", "AIC", "RMSE", "weighted")
  
  beta_resampling_c2_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 9)) 
  beta_resampling_c2_w[,1] <- rep(new_sp_list, n_repetition)
  names(beta_resampling_c2_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "a2", "zvalue", "pr_z", "AIC", "RMSE", "weighted")
  
  # beta_dbh_c2 <- as.data.frame(matrix(nrow = n_repetition, ncol = 3)) 
  # names(beta_dbh_c2) <- c("species", "dbh_15_w_a", "dbh_15_w_b")
  # beta_dbh_c2[,"species"] <- new_sp_list[i]
  
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == new_sp_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & y < 1 & !is.na(ba_plot) & !is.na(ba_larger)) %>%
                      dplyr::select(x, y, location, protocol, id, ba_plot, ba_larger) %>%
                      dplyr::mutate(x = as.numeric(x), y = as.numeric(y), 
                                    location = as.factor(droplevels.factor(location)), 
                                    protocol = as.factor(droplevels.factor(protocol)), 
                                    id = as.numeric(id), ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger))
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (competition - ba plot - resampling)
  output_beta_ratio_c2_rs <- mod_beta_resampling_c2(ranged_data, nb_datasets_all, sample_size, beta_resampling_c2, beta_resampling_c2_w, beta_dbh_c2, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_beta_ratio_c2_rs, file =  paste0("output/beta_ratio_c2_rs_", new_sp_list[i], ".csv"))
  
}



ratio_resampling_mean_c2 <- function(ratio_data, ratio_species_comp) {
  
  ## loading data and species list
  data_ok <- ratio_data
  new_sp_list <- ratio_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == ratio_species_comp]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  beta_resampling_c2 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
  beta_resampling_c2[,1] <- rep(new_sp_list, n_repetition)
  names(beta_resampling_c2) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "RMSE", "weighted")
  
  beta_resampling_c2_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
  beta_resampling_c2_w[,1] <- rep(new_sp_list, n_repetition)
  names(beta_resampling_c2_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "RMSE", "weighted")
  
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == new_sp_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & y < 1 & !is.na(ba_larger) & !is.na(ba_larger)) %>%
                      dplyr::select(x, y, location, protocol, id, ba_larger, ba_larger) %>%
                      dplyr::mutate(x = as.numeric(x), y = as.numeric(y), 
                                    location = as.factor(droplevels.factor(location)), 
                                    protocol = as.factor(droplevels.factor(protocol)), 
                                    id = as.numeric(id), ba_larger = as.numeric(ba_larger), ba_larger = as.numeric(ba_larger))
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (competition - ba larger - resampling)
  output_beta_ratio_c2_mean_rs <- mod_beta_resampling_mean_c2(ranged_data, nb_datasets_all, sample_size, beta_resampling_c2, beta_resampling_c2_w, beta_dbh_c2, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_beta_ratio_c2_mean_rs, file =  paste0("output/beta_ratio_c2_mean_rs_", new_sp_list[i], ".csv"))
  
}








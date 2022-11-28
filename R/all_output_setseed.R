##### HEIGHT MODEL #####

height_models <- function(height_data, height_species) {
  
  ## loading data and species list
  data_ok <- height_data
  species_list <- height_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == height_species]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 400
  
  ## creating file to store model parameters (1 file per species)
  asymptot_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  asymptot_resampling_nocomp[,1] <- rep(height_species, n_repetition)
  names(asymptot_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "b1", "b2", "b3", "AIC", "RMSE")
  
  asymptot_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  asymptot_resampling_nocomp_w[,1] <- rep(height_species, n_repetition)
  names(asymptot_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)),"b1", "b2", "b3", "AIC", "RMSE")
  
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == species_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0) %>%
                      dplyr::select(x, y, location, protocol, id) %>%
                      dplyr::mutate(x = as.numeric(x), y = as.numeric(y), 
                                    location = as.factor(droplevels.factor(location)), 
                                    id = as.numeric(id), protocol = as.factor(droplevels.factor(protocol)))
  
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_asympt_height_nocomp_rs <- mod_height(ranged_data, nb_datasets_all, sample_size, asymptot_resampling_nocomp, asymptot_resampling_nocomp_w, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_asympt_height_nocomp_rs, file =  paste0("output/bis_setseed/height_", height_species, ".csv"))
  
}



##### DIAMETER MODEL #####

diameter_models <- function(diameter_data, diameter_species_comp) {
  
  data_ok <- diameter_data
  new_sp_list <- diameter_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == diameter_species_comp]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 500
  
  power_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_nocomp[,1] <- rep(new_sp_list, n_repetition)
  names(power_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted", "condition")
  
  power_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_nocomp_w[,1] <- rep(new_sp_list, n_repetition)
  names(power_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted", "condition")
  
  power_resampling_c1 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c1[,1] <- rep(new_sp_list, n_repetition)
  names(power_resampling_c1) <- c("species", paste0("a1.protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted", "condition")
  
  power_resampling_c1_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c1_w[,1] <- rep(new_sp_list, n_repetition)
  names(power_resampling_c1_w) <- c("species", paste0("a1.protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted", "condition")
  
  power_resampling_c2 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c2[,1] <- rep(new_sp_list, n_repetition)
  names(power_resampling_c2) <- c("species", paste0("a1.protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted", "condition")
  
  power_resampling_c2_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c2_w[,1] <- rep(new_sp_list, n_repetition)
  names(power_resampling_c2_w) <- c("species", paste0("a1.protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted", "condition")
  
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == new_sp_list[i]) %>%
                      dplyr::filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & !is.na(ba_plot) & !is.na(ba_larger) & ba_plot > 0 & ba_larger > 0) %>%
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
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_power_diameter <- mod_diameter(ranged_data, nb_datasets_all, sample_size, 
                                        power_resampling_nocomp, power_resampling_nocomp_w, 
                                        power_resampling_c1, power_resampling_c1_w, 
                                        power_resampling_c2, power_resampling_c2_w, 
                                        n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_power_diameter, file =  paste0("output/bis_setseed/diameter_", diameter_species_comp, ".csv"))
  
}



##### RATIO MODEL #####

ratio_models <- function(ratio_data, ratio_species_comp) {
  
  ## loading data and species list
  data_ok <- ratio_data
  species_list <- ratio_species_comp
  
  ## defining i
  i <- (1:length(species_list))[species_list == ratio_species_comp]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 500
  
  ## creating file to store model parameters (1 file per species)
  beta_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  beta_resampling_nocomp[,1] <- rep(species_list, n_repetition)
  names(beta_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "AIC", "RMSE", "weighted", "condition")
  
  beta_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  beta_resampling_nocomp_w[,1] <- rep(species_list, n_repetition)
  names(beta_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "AIC", "RMSE", "weighted", "condition")
  
  beta_resampling_c1 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  beta_resampling_c1[,1] <- rep(species_list, n_repetition)
  names(beta_resampling_c1) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "AIC", "RMSE", "weighted", "condition")
  
  beta_resampling_c1_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  beta_resampling_c1_w[,1] <- rep(species_list, n_repetition)
  names(beta_resampling_c1_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "AIC", "RMSE", "weighted", "condition")
  
  beta_resampling_c2 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  beta_resampling_c2[,1] <- rep(species_list, n_repetition)
  names(beta_resampling_c2) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "AIC", "RMSE", "weighted", "condition")
  
  beta_resampling_c2_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  beta_resampling_c2_w[,1] <- rep(species_list, n_repetition)
  names(beta_resampling_c2_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "comp", "AIC", "RMSE", "weighted", "condition")
  
  
  ## selecting data
  data <- data_ok %>% dplyr::filter(sp_name == species_list[i]) %>%
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
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_beta_ratio_mean <- mod_ratio(ranged_data, nb_datasets_all, sample_size, 
                                      beta_resampling_nocomp, beta_resampling_nocomp_w, 
                                      beta_resampling_c1, beta_resampling_c1_w,
                                      beta_resampling_c2, beta_resampling_c2_w,
                                      n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_beta_ratio_mean, paste0("output/bis_setseed/ratio_", ratio_species_comp, ".csv"))
  
}




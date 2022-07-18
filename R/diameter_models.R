diameter_alldata_nocomp <- function(diameter_data, diameter_species) {
  
  ## loading data and species list
  data_ok <- diameter_data
  species_list <- diameter_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == diameter_species]
  print(i)
  
  ## creating file to store model parameters (1 file per species) and protocol counts (i.e. nb of observations per protocol for each species)
  linear_alldata_nocomp <- as.data.frame(matrix(nrow = 2, ncol = length(unique(data_ok$protocol)) + 4)) 
  linear_alldata_nocomp[,1] <- species_list[i]
  names(linear_alldata_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "intercept", "slope", "AIC")
  
  power_alldata_nocomp <- as.data.frame(matrix(nrow = 2, ncol = length(unique(data_ok$protocol)) + 4)) 
  power_alldata_nocomp[,1] <- species_list[i]
  names(power_alldata_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "AIC")
  
  power_alldata_nocomp_log <- as.data.frame(matrix(nrow = 2, ncol = length(unique(data_ok$protocol)) + 5)) 
  power_alldata_nocomp_log[,1] <- species_list[i]
  names(power_alldata_nocomp_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "sigma", "AIC")
  
  
  ## selecting data
  data <- data_ok %>% filter(sp_name == species_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0) %>%
    select(x, y, location, protocol) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
           location = as.factor(droplevels.factor(location)), 
           protocol = as.factor(droplevels.factor(protocol)))
  
  
  ## running models (no competition - all data) and computing parameters (i.e. not and weighted)
  output_linear_diameter_nocomp <- mod_linear_alldata_nocomp(data, linear_alldata_nocomp)
  output_power_diameter_nocomp <- mod_power_alldata_nocomp(data, power_alldata_nocomp)
  output_power_diameter_nocomp_log <- mod_power_alldata_nocomp_log(data, power_alldata_nocomp_log)
  
  
  ## exporting results in .csv files
  write.csv(output_linear_diameter_nocomp, file =  paste0("output/linear_diameter_alldata_nocomp_", diameter_species, ".csv"))
  write.csv(output_power_diameter_nocomp, file =  paste0("output/power_diameter_alldata_nocomp_", diameter_species, ".csv"))
  write.csv(output_power_diameter_nocomp_log, file =  paste0("output/power_diameter_alldata_nocomp_log_", diameter_species, ".csv"))
  
}




diameter_resampling_nocomp <- function(diameter_data, diameter_species) {
  
  ## loading data and species list
  data_ok <- diameter_data
  species_list <- diameter_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == diameter_species]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  linear_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  linear_resampling_nocomp[,1] <- rep(diameter_species, n_repetition)
  names(linear_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "intercept", "slope", "AIC", "RMSE", "weighted")
  
  linear_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  linear_resampling_nocomp_w[,1] <- rep(diameter_species, n_repetition)
  names(linear_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "intercept", "slope", "AIC", "RMSE", "weighted")
  
  
  power_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  power_resampling_nocomp[,1] <- rep(diameter_species, n_repetition)
  names(power_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "AIC", "RMSE", "weighted")
  
  power_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  power_resampling_nocomp_w[,1] <- rep(diameter_species, n_repetition)
  names(power_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "AIC", "RMSE", "weighted")
  
  
  ## selecting data
  data <- data_ok %>% filter(sp_name == species_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0) %>%
    select(x, y, location, protocol, id) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
           location = as.factor(droplevels.factor(location)), 
           protocol = as.factor(droplevels.factor(protocol)), id = as.numeric(id))
  
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_linear_diameter_nocomp_rs <- mod_linear_resampling_nocomp(ranged_data, nb_datasets_all, sample_size, linear_resampling_nocomp, linear_resampling_nocomp_w, n_repetition)
  output_power_diameter_nocomp_rs <- mod_power_resampling_nocomp(ranged_data, nb_datasets_all, sample_size, power_resampling_nocomp, power_resampling_nocomp_w, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_linear_diameter_nocomp_rs, file =  paste0("output/linear_diameter_nocomp_rs_", diameter_species, ".csv"))
  write.csv(output_power_diameter_nocomp_rs, file =  paste0("output/power_diameter_nocomp_rs_", diameter_species, ".csv"))
  
}




diameter_resampling_nocomp_log <- function(diameter_data, diameter_species) {
  
  ## loading data and species list
  data_ok <- diameter_data
  species_list <- diameter_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == diameter_species]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  power_resampling_nocomp_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  power_resampling_nocomp_log[,1] <- rep(diameter_species, n_repetition)
  names(power_resampling_nocomp_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "sigma", "AIC", "RMSE", "weighted")
  
  power_resampling_nocomp_w_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  power_resampling_nocomp_w_log[,1] <- rep(diameter_species, n_repetition)
  names(power_resampling_nocomp_w_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "sigma", "AIC", "RMSE", "weighted")
  
  
  ## selecting data
  data <- data_ok %>% filter(sp_name == species_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x > 0 & y > 0) %>%
    select(x, y, location, protocol, id) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
           location = as.factor(droplevels.factor(location)), 
           protocol = as.factor(droplevels.factor(protocol)), id = as.numeric(id))
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_power_diameter_nocomp_rs_log <- mod_power_resampling_nocomp_log(ranged_data, nb_datasets_all, sample_size, power_resampling_nocomp_log, power_resampling_nocomp_w_log, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_power_diameter_nocomp_rs_log, file =  paste0("output/power_diameter_nocomp_rs_log_", diameter_species, ".csv"))
  
}






diameter_resampling_c1 <- function(diameter_data, diameter_species_comp) {
  
  ## loading data and species list
  data_ok <- diameter_data
  new_sp_list <- diameter_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == diameter_species_comp]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  linear_resampling_c1 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  linear_resampling_c1[,1] <- rep(new_sp_list[i], n_repetition)
  names(linear_resampling_c1) <- c("species", paste0("protocol", unique(data_ok$protocol)), "intercept", "comp", "slope", "AIC", "RMSE", "weighted")
  
  linear_resampling_c1_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  linear_resampling_c1_w[,1] <- rep(new_sp_list[i], n_repetition)
  names(linear_resampling_c1_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "intercept", "comp", "slope", "AIC", "RMSE", "weighted")
  
  
  power_resampling_c1 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  power_resampling_c1[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c1) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted")
  
  power_resampling_c1_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  power_resampling_c1_w[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c1_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted")
  
  
  ## selecting data
  data <- data_ok %>% filter(sp_name == new_sp_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & !is.na(ba_plot) & !is.na(ba_larger)) %>%
    select(x, y, location, protocol, id, ba_plot, ba_larger) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
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
  output_linear_diameter_c1_rs <- mod_linear_resampling_c1(ranged_data, nb_datasets_all, sample_size, linear_resampling_c1, linear_resampling_c1_w, n_repetition)
  output_power_diameter_c1_rs <- mod_power_resampling_c1(ranged_data, nb_datasets_all, sample_size, power_resampling_c1, power_resampling_c1_w, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_linear_diameter_c1_rs, file =  paste0("output/linear_diameter_c1_rs_", new_sp_list[i], ".csv"))
  write.csv(output_power_diameter_c1_rs, file =  paste0("output/power_diameter_c1_rs_", new_sp_list[i], ".csv"))
  
}




diameter_resampling_c1_log <- function(diameter_data, diameter_species_comp) {
  
  ## loading data and species list
  data_ok <- diameter_data
  new_sp_list <- diameter_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == diameter_species_comp]
  print(i)
  
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  power_resampling_c1_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c1_log[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c1_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "sigma", "AIC", "RMSE", "weighted")
  
  power_resampling_c1_w_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c1_w_log[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c1_w_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "sigma", "AIC", "RMSE", "weighted")
  
  
  ## selecting data
  data <- data_ok %>% filter(sp_name == new_sp_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & !is.na(ba_plot) & !is.na(ba_larger)) %>%
    select(x, y, location, protocol, id, ba_plot, ba_larger) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
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
  output_power_diameter_c1_rs_log <- mod_power_resampling_c1_log(ranged_data, nb_datasets_all, sample_size, power_resampling_c1_log, power_resampling_c1_w_log, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_power_diameter_c1_rs_log, file =  paste0("output/power_diameter_c1_rs_log_", new_sp_list[i], ".csv"))
  
}





diameter_resampling_c2 <- function(diameter_data, diameter_species_comp) {
  
  ## loading data and species list
  data_ok <- diameter_data
  new_sp_list <- diameter_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == diameter_species_comp]
  print(i)
  
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  linear_resampling_c2 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  linear_resampling_c2[,1] <- rep(new_sp_list[i], n_repetition)
  names(linear_resampling_c2) <- c("species", paste0("protocol", unique(data_ok$protocol)), "intercept", "comp", "slope", "AIC", "RMSE", "weighted")
  
  linear_resampling_c2_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  linear_resampling_c2_w[,1] <- rep(new_sp_list[i], n_repetition)
  names(linear_resampling_c2_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "intercept", "comp", "slope", "AIC", "RMSE", "weighted")
  
  
  power_resampling_c2 <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  power_resampling_c2[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c2) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted")
  
  power_resampling_c2_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 7)) 
  power_resampling_c2_w[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c2_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "AIC", "RMSE", "weighted")
  
  
  
  ## selecting data
  data <- data_ok %>% filter(sp_name == new_sp_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & !is.na(ba_plot) & !is.na(ba_larger)) %>%
    select(x, y, location, protocol, id, ba_plot, ba_larger) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
           location = as.factor(droplevels.factor(location)), 
           protocol = as.factor(droplevels.factor(protocol)), 
           id = as.numeric(id), ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger))
  
  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (competition - ba larger - resampling)
  output_linear_diameter_c2_rs <- mod_linear_resampling_c2(ranged_data, nb_datasets_all, sample_size, linear_resampling_c2, linear_resampling_c2_w, n_repetition)
  output_power_diameter_c2_rs <- mod_power_resampling_c2(ranged_data, nb_datasets_all, sample_size, power_resampling_c2, power_resampling_c2_w, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_linear_diameter_c2_rs, file =  paste0("output/linear_diameter_c2_rs_", new_sp_list[i], ".csv"))
  write.csv(output_power_diameter_c2_rs, file =  paste0("output/power_diameter_c2_rs_", new_sp_list[i], ".csv"))
  
}





diameter_resampling_c2_log <- function(diameter_data, diameter_species_comp) {
  
  ## loading data and species list
  data_ok <- diameter_data
  new_sp_list <- diameter_species_comp
  
  ## defining i
  i <- (1:length(new_sp_list))[new_sp_list == diameter_species_comp]
  print(i)
  
  
  ## defining number of repetitions
  n_repetition = 300
  
  ## creating file to store model parameters (1 file per species)
  power_resampling_c2_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c2_log[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c2_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "sigma", "AIC", "RMSE", "weighted")
  
  power_resampling_c2_w_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 8)) 
  power_resampling_c2_w_log[,1] <- rep(new_sp_list[i], n_repetition)
  names(power_resampling_c2_w_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "comp", "sigma", "AIC", "RMSE", "weighted")
  
  
  ## selecting data
  data <- data_ok %>% filter(sp_name == new_sp_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0 & !is.na(ba_plot) & !is.na(ba_larger)) %>%
    select(x, y, location, protocol, id, ba_plot, ba_larger) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
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
  output_power_diameter_c2_rs_log <- mod_power_resampling_c2_log(ranged_data, nb_datasets_all, sample_size, power_resampling_c2_log, power_resampling_c2_w_log, n_repetition)
  
  ## exporting results in .csv files
  write.csv(output_power_diameter_c2_rs_log, file =  paste0("output/power_diameter_c2_rs_log_", new_sp_list[i], ".csv"))
  
}


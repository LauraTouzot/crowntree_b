height_alldata_nocomp <- function(height_data, height_species) {
  
  ## loading data and species list
  data_ok <- height_data
  species_list <- height_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == height_species]
  print(i)
  
  ## creating file to store model parameters (1 file per species)
  power_alldata_nocomp <- as.data.frame(matrix(nrow = 2, ncol = length(unique(data_ok$protocol)) + 4)) 
  power_alldata_nocomp[,1] <- species_list[i]
  names(power_alldata_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "AIC")
  
  power_alldata_nocomp_log <- as.data.frame(matrix(nrow = 2, ncol = length(unique(data_ok$protocol)) + 5)) 
  power_alldata_nocomp_log[,1] <- species_list[i]
  names(power_alldata_nocomp_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "sigma", "AIC")
  
  
  asymptot_alldata_nocomp <- as.data.frame(matrix(nrow = 2, ncol = length(unique(data_ok$protocol)) + 5)) 
  asymptot_alldata_nocomp[,1] <- species_list[i]
  names(asymptot_alldata_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "b1", "b2", "b3", "AIC")

  
  ## selecting data
  data <- data_ok %>% filter(sp_name == species_list[i]) %>%
                      filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0) %>%
                      select(x, y, location, protocol) %>%
                      mutate(x = as.numeric(x), y = as.numeric(y), 
                             location = as.factor(droplevels.factor(location)), 
                             protocol = as.factor(droplevels.factor(protocol)))

  
  ## running models (no competition - all data) and computing parameters (i.e. not and weighted)
  output_power_height_nocomp <- mod_power_alldata_nocomp(data, power_alldata_nocomp)
  output_power_height_nocomp_log <- mod_power_alldata_nocomp_log(data, power_alldata_nocomp_log)
  output_asympt_height_nocomp <- mod_asympt_alldata_nocomp(data, asymptot_alldata_nocomp)

  
  ## exporting results in .csv files
  write.csv(output_power_height_nocomp, file =  paste0("output/power_height_alldata_nocomp_", height_species, ".csv"))
  write.csv(output_power_height_nocomp_log, file =  paste0("output/power_height_alldata_nocomp_log_", height_species, ".csv"))
  write.csv(output_asympt_height_nocomp, file =  paste0("output/asympt_height_alldata_nocomp_", height_species, ".csv"))
  
}




height_resampling_nocomp <- function(height_data, height_species) {
  
  ## loading data and species list
  data_ok <- height_data
  species_list <- height_species
  
  ## defining i
  i <- (1:length(species_list))[species_list == height_species]
  print(i)
  
  ## defining number of repetitions
  n_repetition = 5
  
  ## creating file to store model parameters (1 file per species)
  power_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
  power_resampling_nocomp[,1] <- rep(height_species, n_repetition)
  names(power_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "AIC", "RMSE")
  
  power_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
  power_resampling_nocomp_w[,1] <- rep(height_species, n_repetition)
  names(power_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a1", "a2", "AIC", "RMSE")
  
  asymptot_resampling_nocomp <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  asymptot_resampling_nocomp[,1] <- rep(height_species, n_repetition)
  names(asymptot_resampling_nocomp) <- c("species", paste0("protocol", unique(data_ok$protocol)), "b1", "b2", "b3", "AIC", "RMSE")
  
  asymptot_resampling_nocomp_w <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 6)) 
  asymptot_resampling_nocomp_w[,1] <- rep(height_species, n_repetition)
  names(asymptot_resampling_nocomp_w) <- c("species", paste0("protocol", unique(data_ok$protocol)),"b1", "b2", "b3", "AIC", "RMSE")

  
  ## selecting data
  data <- data_ok %>% filter(sp_name == species_list[i]) %>%
    filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0) %>%
    select(x, y, location, protocol, id) %>%
    mutate(x = as.numeric(x), y = as.numeric(y), 
           location = as.factor(droplevels.factor(location)), 
           id = as.numeric(id), protocol = as.factor(droplevels.factor(protocol)))

  
  ## classifying data based on dbh classes 
  ranged_data <- data_in_class(data)
  
  ## defining sample size for resampling
  sample_size <- what_sample_size(ranged_data)
  
  ## computing nb of datasets in which the species was surveyed
  nb_datasets_all <- length(unique(ranged_data$protocol))
  
  ## sampling data and running the models for each repetition (no competition - resampling)
  output_power_height_nocomp_rs <- mod_power_resampling_nocomp(ranged_data, nb_datasets_all, sample_size, power_resampling_nocomp, power_resampling_nocomp_w, n_repetition)
  output_asympt_height_nocomp_rs <- mod_asympt_resampling_nocomp(ranged_data, nb_datasets_all, sample_size, asymptot_resampling_nocomp, asymptot_resampling_nocomp_w, n_repetition)

  ## exporting results in .csv files
  write.csv(output_power_height_nocomp_rs, file =  paste0("output/power_height_nocomp_rs_", height_species, ".csv"))
  write.csv(output_asympt_height_nocomp_rs, file =  paste0("output/asympt_height_nocomp_rs_", height_species, ".csv"))
  
}




# height_resampling_nocomp_log <- function(height_data, height_species) {
#   
#   ## loading data and species list
#   data_ok <- height_data
#   species_list <- height_species
#   
#   ## defining number of repetitions
#   n_repetition = 250
#   
#   ## creating file to store model parameters (1 file per species)
#   power_resampling_nocomp_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
#   power_resampling_nocomp_log[,1] <- rep(height_species, n_repetition)
#   names(power_resampling_nocomp_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a2", "intercept", "AIC", "RMSE")
#   
#   power_resampling_nocomp_w_log <- as.data.frame(matrix(nrow = n_repetition, ncol = length(unique(data_ok$protocol)) + 5)) 
#   power_resampling_nocomp_w_log[,1] <- rep(height_species, n_repetition)
#   names(power_resampling_nocomp_w_log) <- c("species", paste0("protocol", unique(data_ok$protocol)), "a2", "intercept", "AIC", "RMSE")
#   
#   
#   
#   ## defining i
#   i <- (1:length(species_list))[species_list == height_species]
#   
#   print(i)
#   
#   ## selecting data
#   data <- data_ok %>% filter(sp_name == species_list[i]) %>%
#     filter(!is.na(x) & !is.na(y) & x > 0 & y > 0) %>%
#     select(x, y, location, protocol, id) %>%
#     mutate(x = as.numeric(x), y = as.numeric(y), 
#            location = as.factor(droplevels.factor(location)), 
#            protocol = as.factor(droplevels.factor(protocol)), id = as.numeric(id))

#   
#   ## classifying data based on dbh classes 
#   ranged_data <- data_in_class_bis(data)
#   
#   ## defining sample size for resampling
#   sample_size <- what_sample_size(ranged_data)
#   
#   ## computing nb of datasets in which the species was surveyed
#   nb_datasets_all <- length(unique(ranged_data$protocol))
#   
#   ## sampling data and running the models for each repetition (no competition - resampling)
#   output_power_height_nocomp_rs_log <- mod_power_resampling_nocomp_log(ranged_data, nb_datasets_all, power_resampling_nocomp_log, power_resampling_nocomp_w_log, n_repetition)
#   
#   ## exporting results in .csv files
#   write.csv(output_power_height_nocomp_rs_log, file =  paste0("output/power_height_alldata_nocomp_rs_log_", height_species, ".csv"))
#   
# }
# 





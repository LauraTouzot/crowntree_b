summarizing_allometry_dataset <- function(allometry_supp_variables) {

  data <- allometry_supp_variables
  data <- data %>% ungroup()
  dataset <- unique(data$data)
  data_summary <- as.data.frame(matrix(nrow = length(dataset), ncol = 8))
  colnames(data_summary) <- c("dataset", "nspecies", "nplot_crown", "ntree_crown", "nobs_HT", "nobs_Cdiam", "nobs_Cdepth", "nobs_CR")
  data_summary$dataset <- dataset
  
  for (i in 1:length(dataset)) {
    
    sample <- data[data$data == dataset[i],]
    data_summary[i,2] <- length(unique(sample$checked_name))
    data_summary[i,3] <- length(unique(sample$location_ID))
    data_summary[i,4] <- dim(sample)[1]
    data_summary[i,5] <- dim(sample[!is.na(sample$HT_m),])[1]
    data_summary[i,6] <- dim(sample[!is.na(sample$C_diam_m),])[1]
    data_summary[i,7] <- dim(sample[!is.na(sample$C_depth_m),])[1]
    data_summary[i,8] <- dim(sample[!is.na(sample$CR),])[1]
    
  }
  
  return(data_summary)
  
}
  


summarizing_allometry_species <- function(allometry_supp_variables) {

  require(dplyr)
  
  data <- allometry_supp_variables
  data <- data %>% ungroup()
  species_list <- unique(data$checked_name)
  data_summary <- as.data.frame(matrix(nrow = length(species_list), ncol = 7))
  colnames(data_summary) <- c("sp", "nplot_crown", "ntree_crown", "nobs_HT", "nobs_Cdiam", "nobs_Cdepth", "nobs_CR")
  data_summary$sp <- species_list
  
  for (i in 1:length(species_list)) {
    
    sample <- data[which(data$checked_name == species_list[i]),]
    data_summary[i,2] <- length(unique(sample$location_ID))
    data_summary[i,3] <- dim(sample)[1]
    data_summary[i,4] <- dim(sample[!is.na(sample$HT_m),])[1]
    data_summary[i,5] <- dim(sample[!is.na(sample$C_diam_m),])[1]
    data_summary[i,6] <- dim(sample[!is.na(sample$C_depth_m),])[1]
    data_summary[i,7] <- dim(sample[!is.na(sample$CR),])[1]
    
  }
  
  return(data_summary)
  
}
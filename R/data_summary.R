### Compiling database to identify species for which crown data are missing

compiling_data <- function(species_all_NFI, allometry_supp_variables) {
  
  df <- species_all_NFI %>% select(nplot, ntree, matched_name) %>% 
                            rename(nplot_nfi = nplot, ntree_nfi = ntree, checked_sp = matched_name)
  
  allometry_supp_variables <- allometry_supp_variables[allometry_supp_variables$HT_m > 0,]
  
  species_list <- unique(allometry_supp_variables$checked_sp)
  data_summary <- as.data.frame(matrix(nrow = length(species_list), ncol = 7))
  colnames(data_summary) <- c("checked_sp", "nplot_crown", "ntree_crown", "nobs_HT", "nobs_Cdiam", "nobs_Cdepth", "nobs_CR")
  data_summary$checked_sp <- species_list
  
  for (i in 1:length(species_list)) {
    
    sample <- allometry_supp_variables[allometry_supp_variables$checked_sp %in% species_list[i],]
    data_summary[i,2] <- length(unique(sample$location_ID))
    data_summary[i,3] <- dim(sample)[1]
    data_summary[i,4] <- dim(sample[!is.na(sample$HT_m),])[1]
    data_summary[i,5] <- dim(sample[!is.na(sample$C_diam_m),])[1]
    data_summary[i,6] <- dim(sample[!is.na(sample$C_depth_m),])[1]
    data_summary[i,7] <- dim(sample[!is.na(sample$CR),])[1]
    
  }
  
  
}
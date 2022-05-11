# numerous missing data for crown depth
# estimate crown depth data from CR and HT in order to compare observed vs. estimated

estimate_crown_depth <- function(allometry_database) {
  
  estimated_C_depth_m <- as.data.frame(allometry_database$CR * allometry_database$HT_m)
  colnames(estimated_C_depth_m) <- "estimated_C_depth_m"
    
  allometry_observed_estimated <- cbind(allometry_database, estimated_C_depth_m)
  
  return(allometry_observed_estimated)
  
}

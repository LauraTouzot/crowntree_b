### Compute supplementary variables for the allometry database
# basal area of each tree
# basal area of larger trees
# maximum height (i.e. 95th percentile total height of surveyed trees within each site)

compute_supplementary_variables <- function(allometry_database) {
  
  data <- allometry_database[allometry_database$DBH_cm > 10,]
  
  df <- data %>% mutate (ba_tree = (pi*((DBH_cm/100)/2)^2), DBH_square = DBH_cm ^2) %>% 
                               mutate(ba_tree_ha = ba_tree * W) 
  
  df_prep <- df %>% group_by(location_ID) %>% arrange(desc(DBH_cm), .by_group = TRUE)
 
  fun_ba_larger_trees <- function(location_ID, DBH_cm, ba_tree, W){
    bat <- ba_tree * W
    dd <- data.frame(location_ID, DBH_cm, bat)
    dd <- dd[order(-DBH_cm),]
    dd$csum <- ave(dd$bat, dd$location_ID, FUN = cumsum)
    dd$result <- (dd$csum - bat)
    return(dd$result)
    
  }
  
  database <- df_prep %>% 
    mutate(ba_larger_trees = fun_ba_larger_trees(location_ID, DBH_cm, ba_tree, W))
  
  database <- database %>% ungroup()  
  
  df_prep_bis <- database %>% group_by(location_ID) %>% arrange(desc(HT_m), .by_group = TRUE)
  
  fun_ba_higher_trees <- function(location_ID, DBH_cm, HT_m, ba_tree, W){
    bat <- ba_tree * W
    dd <- data.frame(location_ID, DBH_cm, HT_m, bat)
    dd <- dd[order(-HT_m),]
    dd$csum <- ave(dd$bat, dd$location_ID, FUN = cumsum)
    dd$result <- (dd$csum - bat)
    return(dd$result)
    
  }
  
  database_bis <- df_prep_bis %>% 
    mutate(ba_higher_trees = fun_ba_higher_trees(location_ID, DBH_cm, HT_m, ba_tree, W))

  return(database_bis)
    
}

extract_supplementary_variables <- function(allometry_supp_variables) {
  
  res_1 <- allometry_supp_variables %>% group_by(location_ID, W) %>% summarise(Ntree = length(DBH_cm),
                                                                         Ntree_ha = length(DBH_cm) * unique(W))
  
  res_2 <- res_1 %>% group_by(location_ID) %>% summarise(Ntree = sum(Ntree), Ntree_ha = sum(Ntree_ha))
  
  res_3 <- allometry_supp_variables %>% group_by(location_ID) %>% 
                                        summarise(ba_plot = sum(ba_tree_ha), Dg = sqrt(mean(DBH_square)))
                                            
  
  res <- left_join(res_3, res_2, by = "location_ID")
  res$Dg <- sqrt(res$Dg)/res$Ntree
  
  dataset <- allometry_supp_variables %>% group_by(location_ID) %>% summarise(dataset = unique(data))
  
  res <- left_join(res, dataset, by = "location_ID")
  
  return(res)
        
}





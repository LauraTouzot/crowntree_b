get_species_list <- function() {
  
  ### Loading data
  allometry_complete_database <- readRDS(file = "data/allometry_complete_database.RDS")
  NFI_data = readRDS(file = "data/NFI_TNRS_check.rds")
   
  ## extracting species list from NFI data (191 species)
  sampling <- NFI_data %>% 
    filter(continent == "E_U" & nplot >= 100 & ntree >= 1000 | continent == "N_A" & nplot >= 150 & ntree >= 3000)
  
  ## extracting species list in the allometry database (180 species)
  data <- allometry_complete_database
  data <- data %>% ungroup() # just to be sure :)
  species <- unique(data$checked_name)

  data_summary <- data %>% group_by(checked_name) %>% summarise(nplot_crown = length(unique(location_ID)),
                                                                ntree_crown = length(location_ID)) %>% ungroup()
  
  
  
  sampling <- left_join(sampling, data_summary, by = "checked_name")
  
  selected_sp <- sampling %>% 
    filter(continent == "E_U" & nplot_crown >= 100 & ntree_crown >= 1000 | continent == "N_A" & nplot_crown >= 150 & ntree_crown >= 3000)
  
  species_list <- unique(selected_sp$checked_name)
  species_list <- sort(species_list) # do not forget to order species list so that the rest of the code makes sense
  species_list <- species_list[-1]
  
  rm(allometry_complete_database)
  rm(data)
  rm(NFI_data)
  
  return(species_list)
  
}





get_data_allometry <- function(global_species_list) {
  
  ### Loading data
  allometry_complete_database <- readRDS(file = "data/allometry_complete_database.RDS")
  data_ok <- allometry_complete_database %>% filter(checked_name %in% global_species_list) 
  
  rm(allometry_complete_database)
  
  return (data_ok)
  
}





get_data_height <- function(data_allometry) {
  
  # selecting data within the global species list and filtering individual observations
  data_height_a <- data_allometry %>% filter(!is.na(DBH_cm) & !is.na(HT_m) & HT_m > 1.3) %>%
                                      select(checked_name, DBH_cm, HT_m, location_ID, data, ba_plot, ba_larger_trees) %>%
                                      rename(sp_name = checked_name, x = DBH_cm, y = HT_m, location = location_ID, protocol = data,
                                             ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
                                      mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                                             location = as.factor(location), protocol = as.factor(protocol),
                                             ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data from the data
  sel_location_height <- names(table(data_height_a$location))[table(data_height_a$location) > 2]
  sel_protocol_height <- names(table(data_height_a$protocol))[table(data_height_a$protocol) > 9]
  data_height_b <- data_height_a[data_height_a$location %in% sel_location_height & data_height_a$protocol %in% sel_protocol_height, ]

  
  # selecting species with more than 500 observations
  species_height <- data_height_b %>% group_by(sp_name) %>% summarise(nobs_HT = sum(!is.na(y))) %>% 
                                                                 filter(nobs_HT >= 500) %>%
                                                                 ungroup()
  
  # extracting height data and height species list
  species_height_list <- unique(species_height$sp_name)
  data_height <- data_height_b[data_height_b$sp_name %in% species_height_list,]
  
  # removing unused files
  rm(data_height_a, data_height_b, sel_location_height, species_height, species_height_list)
  gc()
  
  # returning data and species list
  return (data_height)
  
}


get_species_height <- function(height_data) {
  
  species_height_list <- unique(height_data$sp_name)
  return(species_height_list)
  
}






get_data_diameter <- function(data_allometry) {
  
  # selecting data within the global species list and filtering individual observations
  data_diameter_a <- data_allometry %>% filter(!is.na(DBH_cm) & !is.na(C_diam_m) & C_diam_m > 0) %>%
                                        select(checked_name, DBH_cm, C_diam_m, location_ID, data, ba_plot, ba_larger_trees) %>%
                                        rename(sp_name = checked_name, x = DBH_cm, y = C_diam_m, location = location_ID, protocol = data, 
                                               ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
                                        mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                                               location = as.factor(location), protocol = as.factor(protocol),
                                               ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data
  sel_location_diameter <- names(table(data_diameter_a$location))[table(data_diameter_a$location) > 2]
  sel_protocol_diameter <- names(table(data_diameter_a$protocol))[table(data_diameter_a$protocol) > 9]
  data_diameter_b <- data_diameter_a[data_diameter_a$location %in% sel_location_diameter & data_diameter_a$protocol %in% sel_protocol_diameter, ]
  
  # selecting species with more than 200 observations
  species_diameter <- data_diameter_b %>% group_by(sp_name) %>% summarise(nobs_diam = sum(!is.na(y))) %>% 
                                                           filter(nobs_diam >= 200) %>%
                                                           ungroup()
  
  # extracting diameter data and diameter species list
  species_diameter_list <- unique(species_diameter$sp_name)
  data_diameter <- data_diameter_b[data_diameter_b$sp_name %in% species_diameter_list,]
  
  # removing unused files
  rm(data_diameter_a, data_diameter_b, sel_location_diameter, species_diameter, species_diameter_list)
  gc()
  
  # returning data and species list
  return (data_diameter)
  
}


get_species_diameter <- function(diameter_data) {
  
  species_diameter_list <- unique(diameter_data$sp_name)
  return(species_diameter_list)
  
}


get_species_diameter_comp <- function(diameter_data) {
  
  data <- diameter_data
  
  summary <- data %>% filter(!is.na(ba_plot) & !is.na(ba_larger)) %>%
    group_by(sp_name) %>% 
    summarise(comp_count = n()) %>%
    filter(comp_count > 200)
  
  new_sp_list_diameter <- summary$sp_name
  
  return(new_sp_list_diameter)
  
  
}





get_data_depth <- function(data_allometry) {
  
  # selecting data within the global species list and filtering individual observations
  data_depth_a <- data_allometry %>% filter(!is.na(DBH_cm) & !is.na(C_depth_m) & C_depth_m > 0) %>%
                                     select(checked_name, DBH_cm, C_depth_m, location_ID, data, ba_plot, ba_larger_trees) %>%
                                     rename(sp_name = checked_name, x = DBH_cm, y = C_depth_m, location = location_ID, protocol = data, 
                                            ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
                                     mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                                            location = as.factor(location), protocol = as.factor(protocol),
                                            ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data
  sel_location_depth <- names(table(data_depth_a$location))[table(data_depth_a$location) > 2]
  sel_protocol_depth <- names(table(data_depth_a$protocol))[table(data_depth_a$protocol) > 9]
  data_depth_b <- data_depth_a[data_depth_a$location %in% sel_location_depth & data_depth_a$protocol %in% sel_protocol_depth, ]  
  
  # selecting species with more than 200 observations
  species_depth <- data_depth_b %>% group_by(sp_name) %>% summarise(nobs_depth = sum(!is.na(y))) %>% 
                                                     filter(nobs_depth >= 200) %>%
                                                     ungroup()
  
  # extracting depth data and depth species list
  species_depth_list <- unique(species_depth$sp_name)
  data_depth <- data_depth_b[data_depth_b$sp_name %in% species_depth_list,]
  
  # removing unused files
  rm(data_depth_a, data_depth_b, sel_location_depth, species_depth, species_depth_list)
  gc()
  
  # returning data and species list
  return (data_depth)
  
}


get_species_depth <- function(depth_data) {
  
  species_depth_list <- unique(depth_data$sp_name)
  return(species_depth_list)
  
}


get_species_depth_comp <- function(depth_data) {
  
  data <- depth_data
  
  summary <- data %>% filter(!is.na(ba_plot) & !is.na(ba_larger)) %>%
    group_by(sp_name) %>% 
    summarise(comp_count = n()) %>%
    filter(comp_count > 200)
  
  new_sp_list_depth <- summary$sp_name
  
  return(new_sp_list_depth)
  
  
}







get_data_ratio <- function(data_allometry) {
  
  # selecting data within the global species list and filtering individual observations
  data_ratio_a <- data_allometry %>% filter(!is.na(DBH_cm) & !is.na(CR) & CR > 0 & CR < 1) %>%
    select(checked_name, DBH_cm, CR, location_ID, data, ba_plot, ba_larger_trees) %>%
    rename(sp_name = checked_name, x = DBH_cm, y = CR, location = location_ID, protocol = data, 
           ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
    mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
           location = as.factor(location), protocol = as.factor(protocol),
           ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data
  sel_location_ratio <- names(table(data_ratio_a$location))[table(data_ratio_a$location) > 2]
  sel_protocol_ratio <- names(table(data_ratio_a$protocol))[table(data_ratio_a$protocol) > 9]
  data_ratio_b <- data_ratio_a[data_ratio_a$location %in% sel_location_ratio & data_ratio_a$protocol %in% sel_protocol_ratio, ]  
  
  # selecting species with more than 200 observations
  species_ratio <- data_ratio_b %>% group_by(sp_name) %>% summarise(nobs_ratio = sum(!is.na(y))) %>% 
    filter(nobs_ratio >= 200) %>%
    ungroup()
  
  # extracting ratio data and ratio species list
  species_ratio_list <- unique(species_ratio$sp_name)
  data_ratio <- data_ratio_b[data_ratio_b$sp_name %in% species_ratio_list,]
  
  # removing unused files
  rm(data_ratio_a, data_ratio_b, sel_location_ratio, species_ratio, species_ratio_list)
  gc()
  
  # returning data and species list
  return (data_ratio)
  
}


get_species_ratio <- function(ratio_data) {
  
  species_ratio_list <- unique(ratio_data$sp_name)
  return(species_ratio_list)
  
}


get_species_ratio_comp <- function(ratio_data) {
  
  data <- ratio_data
  
  summary <- data %>% filter(!is.na(ba_plot) & !is.na(ba_larger)) %>%
    group_by(sp_name) %>% 
    summarise(comp_count = n()) %>%
    filter(comp_count > 200)
  
  new_sp_list_ratio <- summary$sp_name
  
  return(new_sp_list_ratio)
  
  
}







get_data_heightdepth <- function(data_allometry) {
  
  # selecting data within the global species list and filtering individual observations
  data_heightdepth_a <- data_allometry %>% filter(!is.na(C_depth_m) & !is.na(HT_m) & C_depth_m > 0 & HT_m > 0) %>%
                                           select(checked_name, C_depth_m, HT_m, location_ID, data, 
                                                  ba_plot, ba_larger_trees) %>%
                                           rename(sp_name = checked_name, x = HT_m, y = C_depth_m, location = location_ID, protocol = data, 
                                                  ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
                                           mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                                                  location = as.factor(location), protocol = as.factor(protocol),
                                                  ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations from the data
  sel_location_heightdepth <- names(table(data_heightdepth_a$location))[table(data_heightdepth_a$location) > 2]
  data_heightdepth_b <- data_heightdepth_a[data_heightdepth_a$location %in% sel_location_heightdepth, ]
  
  # selecting species with more than 200 observations
  species_heightdepth <- data_heightdepth_b %>% group_by(sp_name) %>% summarise(nobs_depth = sum(!is.na(y))) %>% 
                                                                 filter(nobs_depth >= 200) %>%
                                                                 ungroup()
  
  # extracting heightdepth data and heightdepth species list
  species_heightdepth_list <- unique(species_heightdepth$sp_name)
  data_heightdepth <- data_heightdepth_b[data_heightdepth_b$sp_name %in% species_heightdepth_list,]
  
  # removing unused files
  rm(data_heightdepth_a, data_heightdepth_b, sel_location_heightdepth, species_heightdepth, species_heightdepth_list)
  gc()
  
  # returning data and species list
  return (data_heightdepth)
  
}


get_species_heightdepth <- function(heightdepth_data) {
  
  species_heightdepth_list <- unique(heightdepth_data$sp_name)
  return(species_heightdepth_list)
  
}



get_species_heightdepth_comp <- function(heightdepth_data) {
  
  data <- heightdepth_data
  
  summary <- data %>% filter(!is.na(ba_plot) & !is.na(ba_larger)) %>%
    group_by(sp_name) %>% 
    summarise(comp_count = n()) %>%
    filter(comp_count > 200)
  
  new_sp_list_heightdepth <- summary$sp_name
  
  return(new_sp_list_heightdepth)
  
  
}





## Extracting species list to analyse models' outputs

final_sp_per_relationship <- function(data_allometry) {
  
  #### NO COMPETITION
  
  ## 1. Height
  # selecting height data within the global species list and filtering individual observations
  data_height_a <- data_allometry %>% dplyr::filter(!is.na(DBH_cm) & !is.na(HT_m) & HT_m > 1.3) %>%
    dplyr::select(checked_name, DBH_cm, HT_m, location_ID, data, ba_plot, ba_larger_trees) %>%
    dplyr::rename(sp_name = checked_name, x = DBH_cm, y = HT_m, location = location_ID, protocol = data,
                  ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
    dplyr::mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                  location = as.factor(location), protocol = as.factor(protocol),
                  ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data from the data
  sel_location_height <- names(table(data_height_a$location))[table(data_height_a$location) > 2]
  sel_protocol_height <- names(table(data_height_a$protocol))[table(data_height_a$protocol) > 9]
  data_height_b <- data_height_a[data_height_a$location %in% sel_location_height & data_height_a$protocol %in% sel_protocol_height, ]
  
  
  # selecting species with more than 500 observations
  species_height <- data_height_b %>% dplyr::group_by(sp_name) %>% 
    dplyr::summarise(nobs_HT = sum(!is.na(y))) %>% 
    dplyr::filter(nobs_HT >= 500) %>%
    dplyr::ungroup()
  
  # extracting height data and height species list
  species_height_list <- unique(species_height$sp_name)
  data_height <- data_height_b[data_height_b$sp_name %in% species_height_list,]
  
  species_height_final <- unique(data_height$sp_name)
  
  # removing unused files
  rm(data_height_a, data_height_b, sel_location_height, species_height, species_height_list, data_height)
  gc()
  
  
  
  ## 2. Diameter    
  # selecting data within the global species list and filtering individual observations
  data_diameter_a <- data_allometry %>% dplyr::filter(!is.na(DBH_cm) & !is.na(C_diam_m) & C_diam_m > 0) %>%
    dplyr::select(checked_name, DBH_cm, C_diam_m, location_ID, data, ba_plot, ba_larger_trees) %>%
    dplyr::rename(sp_name = checked_name, x = DBH_cm, y = C_diam_m, location = location_ID, protocol = data, 
                  ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
    dplyr::mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                  location = as.factor(location), protocol = as.factor(protocol),
                  ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data
  sel_location_diameter <- names(table(data_diameter_a$location))[table(data_diameter_a$location) > 2]
  sel_protocol_diameter <- names(table(data_diameter_a$protocol))[table(data_diameter_a$protocol) > 9]
  data_diameter_b <- data_diameter_a[data_diameter_a$location %in% sel_location_diameter & data_diameter_a$protocol %in% sel_protocol_diameter, ]
  
  # selecting species with more than 200 observations
  species_diameter <- data_diameter_b %>% dplyr::group_by(sp_name) %>% 
    dplyr::summarise(nobs_diam = sum(!is.na(y))) %>% 
    dplyr::filter(nobs_diam >= 200) %>%
    dplyr::ungroup()
  
  # extracting diameter data and diameter species list
  species_diameter_list <- unique(species_diameter$sp_name)
  data_diameter <- data_diameter_b[data_diameter_b$sp_name %in% species_diameter_list,]
  
  species_diameter_final <- unique(data_diameter$sp_name)
  
  # removing unused files
  rm(data_diameter_a, data_diameter_b, sel_location_diameter, species_diameter, species_diameter_list, data_diameter)
  gc()
  
  
  
  ## 3. Depth  
  # selecting data within the global species list and filtering individual observations
  data_depth_a <- data_allometry %>% dplyr::filter(!is.na(DBH_cm) & !is.na(C_depth_m) & C_depth_m > 0) %>%
    dplyr::select(checked_name, DBH_cm, C_depth_m, location_ID, data, ba_plot, ba_larger_trees) %>%
    dplyr::rename(sp_name = checked_name, x = DBH_cm, y = C_depth_m, location = location_ID, protocol = data, 
                  ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
    dplyr::mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                  location = as.factor(location), protocol = as.factor(protocol),
                  ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data
  sel_location_depth <- names(table(data_depth_a$location))[table(data_depth_a$location) > 2]
  sel_protocol_depth <- names(table(data_depth_a$protocol))[table(data_depth_a$protocol) > 9]
  data_depth_b <- data_depth_a[data_depth_a$location %in% sel_location_depth & data_depth_a$protocol %in% sel_protocol_depth, ]  
  
  # selecting species with more than 200 observations
  species_depth <- data_depth_b %>% dplyr::group_by(sp_name) %>% 
    dplyr::summarise(nobs_depth = sum(!is.na(y))) %>% 
    dplyr::filter(nobs_depth >= 200) %>%
    dplyr::ungroup()
  
  # extracting depth data and depth species list
  species_depth_list <- unique(species_depth$sp_name)
  data_depth <- data_depth_b[data_depth_b$sp_name %in% species_depth_list,]
  
  species_depth_final <- unique(data_depth$sp_name)
  
  # removing unused files
  rm(data_depth_a, data_depth_b, sel_location_depth, species_depth, species_depth_list, data_depth)
  gc()
  
  
  
  ## 4. Crown ratio  
  # selecting data within the global species list and filtering individual observations
  data_ratio_a <- data_allometry %>% dplyr::filter(!is.na(DBH_cm) & !is.na(CR) & CR > 0 & CR < 1) %>%
    dplyr::select(checked_name, DBH_cm, CR, location_ID, data, ba_plot, ba_larger_trees) %>%
    dplyr::rename(sp_name = checked_name, x = DBH_cm, y = CR, location = location_ID, protocol = data, 
                  ba_plot = ba_plot, ba_larger = ba_larger_trees) %>% 
    dplyr::mutate(sp_name = as.character(sp_name), x = as.numeric(x), y = as.numeric(y), 
                  location = as.factor(location), protocol = as.factor(protocol),
                  ba_plot = as.numeric(ba_plot), ba_larger = as.numeric(ba_larger), id = as.numeric(1:n())) 
  
  
  # removing all plots with less than 2 observations and protocols with less than 9 observations from the data
  sel_location_ratio <- names(table(data_ratio_a$location))[table(data_ratio_a$location) > 2]
  sel_protocol_ratio <- names(table(data_ratio_a$protocol))[table(data_ratio_a$protocol) > 9]
  data_ratio_b <- data_ratio_a[data_ratio_a$location %in% sel_location_ratio & data_ratio_a$protocol %in% sel_protocol_ratio, ]  
  
  # selecting species with more than 200 observations
  species_ratio <- data_ratio_b %>% dplyr::group_by(sp_name) %>% 
    dplyr::summarise(nobs_ratio = sum(!is.na(y))) %>% 
    dplyr::filter(nobs_ratio >= 200) %>%
    dplyr::ungroup()
  
  # extracting depth data and depth species list
  species_ratio_list <- unique(species_ratio$sp_name)
  data_ratio <- data_ratio_b[data_ratio_b$sp_name %in% species_ratio_list,]
  
  species_ratio_final <- unique(data_ratio$sp_name)
  
  # removing unused files
  rm(data_ratio_a, data_ratio_b, sel_location_ratio, species_ratio, species_ratio_list, data_ratio)
  gc()
  
  
  
  ## 5. Extract all species
  all_sp <- as.data.frame(c(species_height_final, species_depth_final, species_diameter_final, species_ratio_final))
  colnames(all_sp) <- "sp"
  duplicate <- as.data.frame(duplicated(all_sp))
  colnames(duplicate) <- "to_keep"
  
  all_sp_list <- cbind(all_sp, duplicate) 
  all_sp_list <- all_sp_list[all_sp_list$to_keep == "FALSE",]
  
  ## 6. Add functional groups
  
  # loading source file
  
  d <- read.csv("data/Tolerance/niimentsvalladares.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
  d <- d %>% dplyr::select(Species, Evergreen, Gymnosperm) 
  
  # checking for taxonomy using TNRS
  species_names <- as.data.frame(unique(d$Species))
  row_number <- as.data.frame(c(1:dim(species_names)[1]))
  
  names_to_check <- cbind(row_number, species_names)
  colnames(names_to_check) <- c("row_number", "species")
  
  results <- TNRS(taxonomic_names = names_to_check, sources = "wfo", mode = "resolve") # World Flora
  results <- results %>% dplyr::select(Name_submitted, Accepted_name)
  
  d_checked <- left_join(d, results, by = c("Species" = "Name_submitted"))
  
  
  all_sp_list <- left_join(all_sp_list, d_checked, by = c("sp" = "Accepted_name"))
  all_sp_list <- all_sp_list %>% dplyr::select(sp, Evergreen, Gymnosperm) %>%
    dplyr::rename(evergreen = Evergreen, gymnosperm = Gymnosperm)
  
  all_sp_list <- all_sp_list[!(all_sp_list$sp %in% c("Salix", "Picea")),]
  
  all_sp_list$evergreen_b <- as.numeric(as.factor(all_sp_list$evergreen))
  all_sp_list$gymnosperm_b <- as.numeric(as.factor(all_sp_list$gymnosperm))
  all_sp_list$group <- "NA"
  
  all_sp_list[all_sp_list$evergreen_b == 2 & all_sp_list$gymnosperm_b == 2,]$group <- "A" # deciduous angiosperm
  all_sp_list[all_sp_list$evergreen_b == 2 & all_sp_list$gymnosperm_b == 4,]$group <- "B" # deciduous gymnosperm
  all_sp_list[all_sp_list$evergreen_b == 4 & all_sp_list$gymnosperm_b == 2,]$group <- "C" # evergreen angiosperm
  all_sp_list[all_sp_list$evergreen_b == 4 & all_sp_list$gymnosperm_b == 4,]$group <- "D" # evergreen gymnosperm
  
  # complete for all missing species (source: world of flora)
  all_sp_list[all_sp_list$sp == "Triadica sebifera",]$group = "C"
  all_sp_list[all_sp_list$sp == "Carya texana",]$group = "A"
  all_sp_list[all_sp_list$sp == "Pinus pinaster",]$group = "D"
  all_sp_list[all_sp_list$sp == "Quercus suber",]$group = "C"
  all_sp_list[all_sp_list$sp == "Quercus pyrenaica",]$group = "A"
  all_sp_list[all_sp_list$sp == "Quercus faginea",]$group = "A"
  all_sp_list[all_sp_list$sp == "Eucalyptus camaldulensis",]$group = "C" 
  all_sp_list[all_sp_list$sp == "Pinus pinea",]$group = "D" 
  all_sp_list[all_sp_list$sp == "Fraxinus angustifolia",]$group = "A" 
  all_sp_list[all_sp_list$sp == "Olea europaea",]$group = "C" 
  all_sp_list[all_sp_list$sp == "Eucalyptus globulus",]$group = "C" 
  all_sp_list[all_sp_list$sp == "Erica arborea",]$group = "C" 
  all_sp_list[all_sp_list$sp == "Myrica faya",]$group = "C" 
  all_sp_list[all_sp_list$sp == "Pinus canariensis",]$group = "D" 
  all_sp_list[all_sp_list$sp == "Prunus pensylvanica",]$group = "A" 
  all_sp_list[all_sp_list$sp == "Quercus ellipsoidalis",]$group = "A" 
  all_sp_list[all_sp_list$sp == "Juniperus thurifera",]$group = "D" 
  
  
  all_sp_list <- all_sp_list %>% dplyr::select(sp, group)
  
  species_height_final <- as.data.frame(species_height_final)
  colnames(species_height_final) <- "sp"
  species_height_final$height <- TRUE
  
  species_depth_final <- as.data.frame(species_depth_final)
  colnames(species_depth_final) <- "sp"
  species_depth_final$depth <- TRUE
  
  species_diameter_final <- as.data.frame(species_diameter_final)
  colnames(species_diameter_final) <- "sp"
  species_diameter_final$diameter <- TRUE
  
  species_ratio_final <- as.data.frame(species_ratio_final)
  colnames(species_ratio_final) <- "sp"
  species_ratio_final$ratio <- TRUE
  
  all_sp_list_groups <- left_join(all_sp_list, species_height_final, by = "sp")
  all_sp_list_groups <- left_join(all_sp_list_groups, species_diameter_final, by = "sp")
  all_sp_list_groups <- left_join(all_sp_list_groups, species_depth_final, by = "sp")
  all_sp_list_groups <- left_join(all_sp_list_groups, species_ratio_final, by = "sp")
  
  all_sp_list_groups[is.na(all_sp_list_groups$ratio),]$ratio <- FALSE
  all_sp_list_groups[is.na(all_sp_list_groups$diameter),]$diameter <- FALSE
  all_sp_list_groups[is.na(all_sp_list_groups$depth),]$depth <- FALSE
  
  all_sp_list_groups$Hmax_diam <- NA
  all_sp_list_groups$Hmax_crownratio <- NA
  all_sp_list_groups$crownratio_diam <- NA
  all_sp_list_groups$volume <- NA
  
  for (i in 1:dim(all_sp_list_groups)[1]) {
    
    aa <- all_sp_list_groups[i,]
    
    if (aa$height == TRUE & aa$diameter == TRUE) {
      all_sp_list_groups[i,"Hmax_diam"] <- TRUE
    } else { all_sp_list_groups[i,"Hmax_diam"] <- FALSE }
    
    
    if (aa$height == TRUE & aa$ratio == TRUE) {
      all_sp_list_groups[i,"Hmax_crownratio"] <- TRUE
    } else { all_sp_list_groups[i,"Hmax_crownratio"] <- FALSE }
    
    
    if (aa$ratio == TRUE & aa$diameter == TRUE) {
      all_sp_list_groups[i,"crownratio_diam"] <- TRUE
    } else { all_sp_list_groups[i,"crownratio_diam"] <- FALSE }
    
    
    if (aa$height == TRUE & aa$diameter == TRUE & aa$depth == TRUE) {
      all_sp_list_groups[i,"volume"] <- TRUE
    } else { all_sp_list_groups[i,"volume"] <- FALSE }
    
  }
  
  write.csv(file = "output/all_sp_groups.csv", all_sp_list_groups)
  
}









  

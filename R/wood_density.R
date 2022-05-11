# add wood density for each species of the database
# source of the data: Fine-root traits in the global spectrum of plant form and function (Carmona et al. 2021)
# repository link: https://doi.org/10.6084/m9.figshare.13140146

add_wood_density <- function(allometry_tolerance_database) {

  data <- read.delim("data/Above_traits.txt", sep = " ", header = FALSE)
  data <- data[-1,]
  
  # la: leaf area, ln: leaf nitrogen, ph: plant height, sla: specific leaf area, ssd: stem density, sm: seed mass
  colnames(data) <- c("species", "la", "ln", "ph", "sla", "ssd", "sm")
  
  # working on species present in the allometry database only
  data$species <- str_replace(data$species, "_", " ")
  
  species_list <- unique(allometry_database$sp)
  data <- data[data$species %in% species_list,] 
  
  # selecting variables and merging tables
  data <- data %>% dplyr::select(species, ssd)
  df <- left_join(allometry_database, data, by = c("sp" = "species"))
  
  return(df)
  
}

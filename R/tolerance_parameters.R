trim <- function (x) gsub("^\\s+|\\s+$", "", x)

strsplit_cbind <- function(x) {
    if(length(x)>1){
        return(as.numeric(c(x[1], x[2])))
      }else{return(as.numeric(c(x[1], NA)))}
}

extract_index <- function(df, var){
  res <- data.frame(t(sapply(strsplit(trim(df[[var]]), "±"), FUN = strsplit_cbind)))
  names(res) <- paste(var, c("mean", "sd"), sep ="_")
  return(res)
}

get_shade_tolerance_data <- function(allometry_database) {
  
  #function to read and select data
  d <- read.csv("data/niimentsvalladares.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
  names(d) <- gsub( "\\.", "_", trim(names(d)))
  d$Species <- trim(d$Species)
  
  d_S <- extract_index(d, "Shade_tolerance")
  d_D <- extract_index(d, "Drought_tolerance")
  d_W <- extract_index(d, "Waterlogging_tolerance")
  
  dd <- dplyr::select(data.frame(cbind(d, d_S, d_D, d_W)), -Shade_tolerance, -Drought_tolerance, -Waterlogging_tolerance) 
  dd <- dd %>% rename(sp = Species)
  
  # checking for taxonomy // need to be modified using TNRS (to do later)
  require(taxize)
  
  sources <- gnr_datasources()
  species <- unique(dd$sp) # cleaning species name before running the next step (au cas par cas)
  tolerance_species_list <- gnr_resolve(as.character(species), best_match_only = TRUE)
  
  matched_name <- as.data.frame(str_split_fixed(tolerance_species_list$matched_name, " ", 3))
  matched_name <- matched_name[,c(1,2)]
  colnames(matched_name) <- c("Genus", "Species")
  matched_name$Species <- str_replace_all(matched_name$Species, c("×" = "sp", "x" = "sp")) 
  
  tolerance_species <- cbind(tolerance_species_list, matched_name)
  tolerance_species <- tolerance_species[, c("user_supplied_name", "Genus", "Species")]
  checked_tolerance_species <- as.data.frame(paste(tolerance_species$Genus, tolerance_species$Species, sep = " "))
  checked_tolerance_species <- cbind(tolerance_species, checked_tolerance_species)
  colnames(checked_tolerance_species) <- c("user_supplied_name", "Genus", "Species", "checked_sp")
  
  df <- left_join(dd, checked_tolerance_species, by = c("sp" = "user_supplied_name"))
  df <- df %>% dplyr::select(Evergreen, Gymnosperm, Shade_tolerance_mean, Shade_tolerance_sd, 
                             Drought_tolerance_mean, Drought_tolerance_sd, 
                             Waterlogging_tolerance_mean, Waterlogging_tolerance_sd, checked_sp)
  
  df_allometry <- left_join(allometry_database, df, by = "checked_sp")
  
  return(df_allometry)
}

## Extracting species list to analyse models' outputs
get_mod_sp_list <- function() {
  
  # loading summaries from all model outputs
  height_max <- read.csv(file = "output/height_max_summary.csv")
  height_max <- height_max %>% dplyr::select(species, mean_hmax, sd_hmax, se_hmax)
  
  diameter_dbh <- read.csv(file = "output/diameter_dbh_summary.csv")
  diameter_dbh <- diameter_dbh %>% dplyr::select(-X)
  
  ratio_dbh <- read.csv(file = "output/ratio_beta_mean_estimates.csv")
  ratio_dbh <- ratio_dbh %>% dplyr::select(-X)

  # computing species list
  file <- left_join(height_max, diameter_dbh, by = "species")
  file <- left_join(file, ratio_dbh, by = "species")
  
  sp_list_final <- file$species
  
  return(sp_list_final)

    
}


## Adding climate variables from Gbif

add_climate_gbif <- function(sp_list_output) {
  
  climate_sumup <- read.csv("output/sp_gbif_climate.csv", sep = " ", header = TRUE)
  climate_sumup <- climate_sumup[-1,]
  
  sp_list_output <- as.data.frame(sp_list_output)
  colnames(sp_list_output) <- "species"
  
  sp_list_climate <- left_join(sp_list_output, climate_sumup, by = "species")
  sp_list_climate <- sp_list_climate[!(is.na(sp_list_climate$mat.low)),]
  
  rm(climate_sumup)
  gc()
  
  return(sp_list_climate)
  
}



## Adding traits and functional groups 
add_traits_funcgroup <- function(sp_climate) {

  ## Loading TRY data from article in Nature
  naturedata <- read.delim(file = "data/TRY/Above_traits.txt", sep = " ", header = F)
  colnames(naturedata) <- c("sp", "la", "ln", "ph", "sla", "ssd", "sm")
  naturedata$species <- str_replace_all(naturedata$sp, pattern = "_", replacement = " ")
  naturedata <- naturedata[-1,-c(1,3,4)]
  naturedata <- naturedata[naturedata$species %in% sp_climate$species,]

  ## Loading TRY data from TRY database to add LDMC
  TRYdata <- fread("data/TRY/22093.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
  TRYdata <- as.data.frame(TRYdata)
  TRYdata_b <- TRYdata %>% dplyr::select(SpeciesName, AccSpeciesName, TraitID, OrigValueStr, StdValue, UnitName) %>% 
                           dplyr::filter(TraitID == 47)
  TRYdata_b$OrigValueStr <- as.numeric(TRYdata_b$OrigValueStr)

  sp <- unique(TRYdata_b$AccSpeciesName)
  TRYdata_c <- as.data.frame(matrix(nrow = length(sp), ncol = 4))
  colnames(TRYdata_c) <- c("sp", "mean_ldmc", "min_ldmc", "max_ldmc")
  TRYdata_c$sp <- sp

  for (i in 1:length(sp)) {

    df <- TRYdata_b[TRYdata_b$AccSpeciesName %in% sp[i],]
    TRYdata_c[i,"mean_ldmc"] <- mean(df$StdValue, na.rm = TRUE)
    TRYdata_c[i,"min_ldmc"] <- quantile(df$StdValue, probs = 0.025, na.rm = TRUE)
    TRYdata_c[i,"max_ldmc"] <- quantile(df$StdValue, probs = 0.975, na.rm = TRUE)
  }

  sp_traits <- left_join(naturedata, TRYdata_c, by = c("species" = "sp"))



  ## Loading file with functional groups and shade tolerance

  # computing necessary functions
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  strsplit_cbind <- function(x) {

    if(length(x)>1){
      return(as.numeric(c(x[1], x[2])))
    } else { return(as.numeric(c(x[1], NA))) }

  }

  extract_index <- function(df, var) {

    res <- data.frame(t(sapply(strsplit(trim(df[[var]]), "±"), FUN = strsplit_cbind)))
    names(res) <- paste(var, c("mean", "sd"), sep ="_")
    return(res)

  }

  d <- read.csv("data/Tolerance/niimentsvalladares.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
  names(d) <- gsub( "\\.", "_", trim(names(d)))
  d$Species <- trim(d$Species)

  d_S <- extract_index(d, "Shade_tolerance")

  shade_tol <- d %>% dplyr::select(Species)
  shade_tol <- cbind(shade_tol, d_S)
  shade_tol <- shade_tol %>% dplyr::rename(species = Species, shade_tol_mean = Shade_tolerance_mean, shade_tol_sd = Shade_tolerance_sd)

  sp_traits <- left_join(sp_traits, shade_tol, by = "species")
 
  # A deciduous angiosperm
  # B deciduous gymnosperm
  # C evergreen angiosperm
  # D evergreen gymnosperm
  
  sp_traits_complete <- left_join(sp_climate, sp_traits, by = "species")
  
  fc_groups <- read.csv(file = "output/all_sp_groups.csv")
  fc_groups <- fc_groups %>% dplyr::select(sp, group)
  
  angio <- fc_groups %>% dplyr::filter(group %in% c("A", "C")) %>%
                         dplyr::mutate(new_gp = "angio")
  
  gymno <- fc_groups %>% dplyr::filter(group %in% c("B", "D")) %>%
                         dplyr::mutate(new_gp = "gymno")
  
  new_fc_groups <- rbind(angio, gymno)
  
  sp_traits_complete <- left_join(sp_traits_complete, new_fc_groups, by = c("species" = "sp"))
  
  write.csv(file = "output/sp_traits.csv", sp_traits_complete)
  return(sp_traits_complete)

}




  
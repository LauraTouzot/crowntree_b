## Analyzing model outputs (crown tree allometry)

### Loading packages (to be removed)
require(dplyr)
require(tidyverse)
require(data.table)
require(targets)
require(data.table)
require(TNRS)
require(plyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
require(stringr)


se <- function(x) sqrt(var(x, na.rm = TRUE) / length(x))

### Defining final species list
tar_load(height_species) # 163 sp
tar_load(depth_species) # 129 sp
tar_load(diameter_species) # 75 sp
tar_load(heightdepth_species) # 129 sp

sp_list <- height_species[height_species %in% diameter_species]
sp_list <- depth_species[depth_species %in% sp_list]
sp_list <- heightdepth_species[heightdepth_species %in% sp_list]


### Loading climate data (mean climate per species)

climate_sumup <- read.csv("output/sp_gbif_climate.csv", sep = " ", header = TRUE)
climate_sumup <- climate_sumup[-1,]
climate_sumup <- climate_sumup[climate_sumup$species %in% sp_list,]

sp_list <- sp_list[sp_list %in% climate_sumup$species]


### Completing sp list with trait, tree category (i.e. angiosperm vs. gymnosperm) and beta coefficient for crown shape

## Loading TRY data from article in Nature
naturedata <- read.delim(file = "data/TRY/Above_traits.txt", sep = " ", header = F)
colnames(naturedata) <- c("sp", "la", "ln", "ph", "sla", "ssd", "sm")
naturedata$species <- str_replace_all(naturedata$sp, pattern = "_", replacement = " ")
naturedata <- naturedata[-1,-c(1,3,4)]

# Checking for taxonomy using TNRS
species_names <- as.data.frame(unique(naturedata$sp))
row_number <- as.data.frame(c(1:dim(species_names)[1]))
names_to_check <- cbind(row_number, species_names)
colnames(names_to_check) <- c("row_number", "species")

results <- TNRS(taxonomic_names = names_to_check, sources = "wfo", mode = "resolve") # World Flora
results <- results %>% dplyr::select(Accepted_name) 
naturedata <- cbind(naturedata, results)
naturedata <- naturedata[naturedata$Accepted_name %in% sp_list,]

## Loading TRY data TRY database to add LDMC
TRYdata <- fread("data/TRY/22093.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
TRYdata <- as.data.frame(TRYdata) 
TRYdata_b <- TRYdata %>% select(SpeciesName, AccSpeciesName, TraitID, OrigValueStr, StdValue, UnitName) %>% filter(TraitID == 47)
TRYdata_b$OrigValueStr <- as.numeric(TRYdata_b$OrigValueStr)

sp <- unique(TRYdata_b$AccSpeciesName)
TRYdata_c <- as.data.frame(matrix(nrow = length(sp), ncol = 4))
colnames(TRYdata_c) <- c("sp", "mean_ldmc", "min_ldmc", "max_ldmc")
TRYdata_c$sp <- sp

for (i in 1:length(sp)) {
  
  df <- TRYdata_b[TRYdata_b$AccSpeciesName %in% sp[i],]
  TRYdata_c[i,"mean"] <- mean(df$StdValue, na.rm = TRUE)
  TRYdata_c[i,"min"] <- quantile(df$StdValue, probs = 0.025, na.rm = TRUE)
  TRYdata_c[i,"max"] <- quantile(df$StdValue, probs = 0.975, na.rm = TRUE)
}


# Checking for taxonomy using TNRS
species_names <- as.data.frame(unique(TRYdata_c$sp))
row_number <- as.data.frame(c(1:dim(species_names)[1]))
names_to_check <- cbind(row_number, species_names)
colnames(names_to_check) <- c("row_number", "species")

results <- TNRS(taxonomic_names = names_to_check, sources = "wfo", mode = "resolve") # World Flora
results <- results %>% dplyr::select(Name_submitted, Accepted_name) 

TRYdata_d <- left_join(TRYdata_c, results, by = c("sp" = "Name_submitted"))
TRYdata_e <- TRYdata_d[TRYdata_d$Accepted_name %in% sp_list,]


## Loading file with tree category (i.e. angiosperm vs. gymnosperm) and shade tolerance

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

shade_tol <- d %>% select(Species, Evergreen, Gymnosperm) 
shade_tol <- cbind(shade_tol, d_S)

# Checking for taxonomy using TNRS
species_names <- as.data.frame(unique(shade_tol$Species))
row_number <- as.data.frame(c(1:dim(species_names)[1]))

names_to_check <- cbind(row_number, species_names)
colnames(names_to_check) <- c("row_number", "species")

# using 3 different sources to compare the obtained results and increase our chances to find a match for each species name
results <- TNRS(taxonomic_names = names_to_check, sources = "wfo", mode = "resolve") # World Flora
results <- results %>% dplyr::select(Name_submitted, Accepted_name)

shade_tol <- left_join(shade_tol, results, by = c("Species" = "Name_submitted"))
shade_tol <- shade_tol[shade_tol$Accepted_name %in% sp_list,]

## Computing final species list with all required information
final_sp_list <- left_join(climate_sumup, TRYdata_e, by = c("species" = "Accepted_name"))
final_sp_list <- left_join(final_sp_list, naturedata, by = "species")
final_sp_list <- left_join(final_sp_list, shade_tol, by = c("species" = "Accepted_name"))


### SAVING FINAL SPECIES LIST 
write.csv(file = "final_sp_list.csv", final_sp_list)


### Loading model outputs for each explored allometric relationship

list_sp <- read.csv(file = "final_sp_list.csv")

## Relationship #1: total tree height ~ diameter at breast height

# resampling - no competition
dd <- list.files(path = "output/", pattern = "asympt_height_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

height_resampling_asympt <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_height_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

height_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


## Relationship #2: crown depth ~ diameter at breast height

# resampling - no competition
dd <- list.files(path = "output/", pattern = "linear_depth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

A_depth_resampling_linear <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# dd <- list.files(path = "output/", pattern = "power_depth_nocomp_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# B_depth_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_log_depth_nocomp_rs_") 
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

C_depth_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# resampling - competition ; ba plot
# dd <- list.files(path = "output/", pattern = "linear_depth_c1_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# D_depth_resampling_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
# 
# dd <- list.files(path = "output/", pattern = "power_depth_c1_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# E_depth_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_depth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

F_depth_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# resampling - competition : ba larger trees
# dd <- list.files(path = "output/", pattern = "linear_depth_c2_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# G_depth_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
# 
# dd <- list.files(path = "output/", pattern = "power_depth_c2_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# H_depth_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_depth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

I_depth_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


## Relationship #3: crown diameter ~ diameter at breast height

# resampling - no competition
dd <- list.files(path = "output/", pattern = "linear_diameter_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

A_diameter_resampling_linear <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# dd <- list.files(path = "output/", pattern = "power_diameter_nocomp_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# B_diameter_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_log_diameter_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

C_diameter_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# resampling - competition : ba plot
# dd <- list.files(path = "output/", pattern = "linear_diameter_c1_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# D_diameter_resampling_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
# 
# dd <- list.files(path = "output/", pattern = "power_diameter_c1_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# E_diameter_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_diameter_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

F_diameter_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

# resampling - competition : ba larger trees
# dd <- list.files(path = "output/", pattern = "linear_diameter_c2_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# G_diameter_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
# 
# dd <- list.files(path = "output/", pattern = "power_diameter_c2_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# H_diameter_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_diameter_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

I_diameter_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


## Relationship #4: crown depth ~ crown height

# resampling - no competition
dd <- list.files(path = "output/", pattern = "linear_heightdepth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

A_heightdepth_resampling_linear <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# dd <- list.files(path = "output/", pattern = "power_heightdepth_nocomp_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# B_heightdepth_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_log_heightdepth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

C_heightdepth_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

# resampling - competition : ba plot
# dd <- list.files(path = "output/", pattern = "linear_heightdepth_c1_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# D_heightdepth_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
# 
# dd <- list.files(path = "output/", pattern = "power_heightdepth_c1_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# E_heightdepth_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_heightdepth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

F_heightdepth_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# resampling - competition : ba larger trees
# dd <- list.files(path = "output/", pattern = "linear_heightdepth_c2_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# 
# G_heightdepth_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
# 
# dd <- list.files(path = "output/", pattern = "power_heightdepth_c2_rs_")
# data_list <- lapply(paste0("output/", dd), utils::read.table,
#                     header = TRUE, sep = ",", dec = ".",
#                     encoding = "UTF-8",
#                     stringsAsFactors = FALSE)
# 
# H_heightdepth_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_heightdepth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

I_heightdepth_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


### Complete summary of all models' outputs
final_sp_list <- read.csv(file = "final_sp_list.csv")
list_sp <- final_sp_list$species

height_power <- height_resampling_power %>% dplyr::group_by(species, weighted) %>% 
                                            dplyr::summarise(obs_height_power = n_distinct(na.omit(a1))) %>% 
                                            dplyr::filter(weighted == "no") %>%
                                            dplyr::select(-weighted)


height_asympt <- height_resampling_asympt %>% dplyr::group_by(species, weighted) %>% 
                                              dplyr::summarise(obs_height_asympt = n_distinct(na.omit(b1))) %>% 
                                              dplyr::filter(weighted == "no") %>%
                                              dplyr::select(-weighted)
                        


diameter_linear <- A_diameter_resampling_linear %>% dplyr::group_by(species, weighted) %>% 
                                                    dplyr::summarise(obs_diam_linear = n_distinct(na.omit(intercept))) %>% 
                                                    filter(weighted == "no") %>%
                                                    dplyr::select(-weighted)

diameter_power_log <- C_diameter_resampling_power_log %>% dplyr::group_by(species, weighted) %>% 
                                                          dplyr::summarise(obs_diam_power_log = n_distinct(na.omit(a1)))  %>% 
                                                          dplyr::filter(weighted == "no") %>%
                                                          dplyr::select(-weighted)

diameter_power_log_c1 <- F_diameter_resampling_power_c1_log %>% dplyr::group_by(species, weighted) %>% 
                                                                dplyr::summarise(obs_diam_power_log_c1 = n_distinct(na.omit(a1)))  %>% 
                                                                dplyr::filter(weighted == "no") %>%
                                                                dplyr::select(-weighted)

diameter_power_log_c2 <- I_diameter_resampling_power_c2_log %>% dplyr::group_by(species, weighted) %>% 
                                                                dplyr::summarise(obs_diam_power_log_c2 = n_distinct(na.omit(a1)))  %>% 
                                                                dplyr::filter(weighted == "no") %>%
                                                                dplyr::select(-weighted)


depth_linear <- A_depth_resampling_linear %>% dplyr::group_by(species, weighted) %>% 
                                              dplyr::summarise(obs_depth_linear = n_distinct(na.omit(intercept))) %>%
                                              dplyr::filter(weighted == "no") %>%
                                              dplyr::select(-weighted)

depth_power_log <- C_depth_resampling_power_log %>% dplyr::group_by(species, weighted) %>% 
                                                    dplyr::summarise(obs_depth_power_log = n_distinct(na.omit(a1)))  %>% 
                                                    dplyr::filter(weighted == "no") %>% 
                                                    dplyr::select(-weighted)

depth_power_log_c1 <- F_depth_resampling_power_c1_log %>% dplyr::group_by(species, weighted) %>% 
                                                          dplyr::summarise(obs_depth_power_log_c1 = n_distinct(na.omit(a1)))  %>% 
                                                          dplyr::filter(weighted == "no") %>% 
                                                          dplyr::select(-weighted)

depth_power_log_c2 <- I_depth_resampling_power_c2_log %>% dplyr::group_by(species, weighted) %>% 
                                                          dplyr::summarise(obs_depth_power_log_c2 = n_distinct(na.omit(a1)))  %>% 
                                                          dplyr::filter(weighted == "no") %>% 
                                                          dplyr::select(-weighted)

# heightdepth_linear <- A_heightdepth_resampling_linear %>% dplyr::group_by(species, weighted) %>% 
                                                            # dplyr::summarise(obs_heightdepth_linear = n_distinct(na.omit(intercept)))  %>% 
                                                            # dplyr::filter(weighted == "no") %>% 
                                                            # dplyr::select(-weighted)
                                                            
heightdepth_power_log <- C_heightdepth_resampling_power_log %>% dplyr::group_by(species, weighted) %>% 
                                                                dplyr::summarise(obs_heightdepth_power_log = n_distinct(na.omit(a1)))  %>% 
                                                                dplyr::filter(weighted == "no") %>% 
                                                                dplyr::select(-weighted)

heightdepth_power_log_c1 <- F_heightdepth_resampling_power_c1_log %>% dplyr::group_by(species, weighted) %>% 
                                                                      dplyr::summarise(obs_heightdepth_power_log_c1 = n_distinct(na.omit(a1)))  %>% 
                                                                      dplyr::filter(weighted == "no") %>% 
                                                                      dplyr::select(-weighted)

heightdepth_power_log_c2 <- I_heightdepth_resampling_power_c2_log %>% dplyr::group_by(species, weighted) %>% 
                                                                      dplyr::summarise(obs_heightdepth_power_log_c2 = n_distinct(na.omit(a1)))  %>% 
                                                                      dplyr::filter(weighted == "no") %>% 
                                                                      dplyr::select(-weighted)



sp_list <- sp_complete_file$species
sp_list <- as.data.frame(sp_list)
colnames(sp_list) <- "species"


model_outputs <- join_all(list(sp_list, 
                               height_power, height_asympt, 
                               diameter_linear, diameter_power_log, 
                               depth_linear, depth_power_log,
                               diameter_power_log_c1, diameter_power_log_c2,
                               depth_power_log_c1, depth_power_log_c2), by = "species", type = "left")

to_remove <- c("Abies concolor", "Picea engelmannii", "Pinus halepensis", "Pinus taeda", "Thuja plicata", "Carpinus betulus", "Pinus nigra")
model_outputs <- model_outputs[!(model_outputs$species %in% to_remove),]

sp_to_keep <- model_outputs$species

### Selecting base models

# Height model selection
height_resampling_power <- height_resampling_power[height_resampling_power$species %in% sp_to_keep,]
height_power <- height_resampling_power %>% dplyr::filter(weighted == "no") %>%
                                            dplyr::group_by(species) %>%
                                            dplyr::filter(!is.na(a1)) %>% 
                                            dplyr::slice_sample(n = 100) %>%
                                            dplyr::mutate(ID_b = X + 300) %>%
                                            dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

height_power_results <- height_power %>% dplyr::group_by(species) %>%
                                         dplyr::summarise(mean_a1 = mean(a1),
                                                          sd_a1 = sd(a1),
                                                          min_a1 = quantile(a1, probs = 0.025),
                                                          max_a1 = quantile(a1, probs = 0.975),
                                                          mean_a2 = mean(a2),
                                                          sd_a2 = sd(a2),
                                                          min_a2 = quantile(a2, probs = 0.025),
                                                          max_a2 = quantile(a2, probs = 0.975),
                                                          total_AIC = sum(AIC),
                                                          total_RMSE = sum(RMSE))


height_power_b <- height_resampling_power %>% dplyr::filter(weighted == "yes") %>%
                                              dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
height_power_b <- height_power_b[height_power_b$ID_tofilter %in% height_power$ID_species,]

height_power_results_b <- height_power_b %>% dplyr::group_by(species) %>%
                                              dplyr::summarise(mean_a1 = mean(a1),
                                                               sd_a1 = sd(a1),
                                                               min_a1 = quantile(a1, probs = 0.025),
                                                               max_a1 = quantile(a1, probs = 0.975),
                                                               mean_a2 = mean(a2),
                                                               sd_a2 = sd(a2),
                                                               min_a2 = quantile(a2, probs = 0.025),
                                                               max_a2 = quantile(a2, probs = 0.975),
                                                               total_AIC = sum(AIC),
                                                               total_RMSE = sum(RMSE))


height_resampling_asympt <- height_resampling_asympt[height_resampling_asympt$species %in% sp_to_keep,]
height_asympt <- height_resampling_asympt %>% dplyr::filter(weighted == "no") %>%
                                              dplyr::group_by(species) %>%
                                              dplyr::filter(!is.na(b1)) %>% 
                                              dplyr::slice_sample(n = 100) %>%
                                              dplyr::mutate(ID_b = X + 300) %>%
                                              dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

height_asympt_results <- height_asympt %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_b1 = mean(b1),
                                                            sd_b1 = sd(b1),
                                                            min_b1 = quantile(b1, probs = 0.025),
                                                            max_b1 = quantile(b1, probs = 0.975),
                                                            mean_b2 = mean(b2),
                                                            sd_b2 = sd(b2),
                                                            min_b2 = quantile(b2, probs = 0.025),
                                                            max_b2 = quantile(b2, probs = 0.975),
                                                            mean_b3 = mean(b3),
                                                            sd_b3 = sd(b3),
                                                            min_b3 = quantile(b3, probs = 0.025),
                                                            max_b3 = quantile(b3, probs = 0.975),
                                                            total_AIC = sum(AIC),
                                                            total_RMSE = sum(RMSE))
                                        

height_asympt_b <- height_resampling_asympt %>% dplyr::filter(weighted == "yes") %>%
                                                dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
height_asympt_b <- height_asympt_b[height_asympt_b$ID_tofilter %in% height_asympt$ID_species,]

height_asympt_results_b <- height_asympt_b %>% dplyr::group_by(species) %>%
                                              dplyr::summarise(mean_b1 = mean(b1),
                                                               sd_b1 = sd(b1),
                                                               min_b1 = quantile(b1, probs = 0.025),
                                                               max_b1 = quantile(b1, probs = 0.975),
                                                               mean_b2 = mean(b2),
                                                               sd_b2 = sd(b2),
                                                               min_b2 = quantile(b2, probs = 0.025),
                                                               max_b2 = quantile(b2, probs = 0.975),
                                                               mean_b3 = mean(b3),
                                                               sd_b3 = sd(b3),
                                                               min_b3 = quantile(b3, probs = 0.025),
                                                               max_b3 = quantile(b3, probs = 0.975),
                                                               total_AIC = sum(AIC),
                                                               total_RMSE = sum(RMSE))



# Depth model selection
depth_resampling_linear <- A_depth_resampling_linear[A_depth_resampling_linear$species %in% sp_to_keep,]
depth_linear <- depth_resampling_linear %>% dplyr::filter(weighted == "no") %>%
                                            dplyr::group_by(species) %>%
                                            dplyr::filter(!is.na(intercept)) %>% 
                                            dplyr::slice_sample(n = 80) %>%
                                            dplyr::mutate(ID_b = X + 300) %>%
                                            dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

depth_linear_results <- depth_linear %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean_intercept = mean(intercept),
                                                           sd_intercept = sd(intercept),
                                                           min_intercept = quantile(intercept, probs = 0.025),
                                                           max_intercept = quantile(intercept, probs = 0.975),
                                                           mean_slope = mean(slope),
                                                           sd_slope = sd(slope),
                                                           min_slope = quantile(slope, probs = 0.025),
                                                           max_slope = quantile(slope, probs = 0.975),
                                                           total_AIC = sum(AIC),
                                                           total_RMSE = sum(RMSE))


depth_linear_b <- A_depth_resampling_linear %>% dplyr::filter(weighted == "yes") %>%
                                                dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
depth_linear_b <- depth_linear_b[depth_linear_b$ID_tofilter %in% depth_linear$ID_species,]

depth_linear_results_b <- depth_linear_b %>% dplyr::group_by(species) %>%
                                             dplyr::summarise(mean_intercept = mean(intercept),
                                                              sd_intercept = sd(intercept),
                                                              min_intercept = quantile(intercept, probs = 0.025),
                                                              max_intercept = quantile(intercept, probs = 0.975),
                                                              mean_slope = mean(slope),
                                                              sd_slope = sd(slope),
                                                              min_slope = quantile(slope, probs = 0.025),
                                                              max_slope = quantile(slope, probs = 0.975),
                                                              total_AIC = sum(AIC),
                                                              total_RMSE = sum(RMSE))
                                                                                                                

depth_resampling_power <- C_depth_resampling_power_log[C_depth_resampling_power_log$species %in% sp_to_keep,]
depth_power <- depth_resampling_power %>% dplyr::filter(weighted == "no") %>%
                                          dplyr::group_by(species) %>%
                                          dplyr::filter(!is.na(a1)) %>% 
                                          dplyr::slice_sample(n = 80) %>%
                                          dplyr::mutate(ID_b = X + 300) %>%
                                          dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

depth_power_results <- depth_power %>% dplyr::group_by(species) %>%
                                       dplyr::summarise(mean_a1 = mean(a1),
                                                        sd_a1 = sd(a1),
                                                        min_a1 = quantile(a1, probs = 0.025),
                                                        max_a1 = quantile(a1, probs = 0.975),
                                                        mean_a2 = mean(a2),
                                                        sd_a2 = sd(a2),
                                                        min_a2 = quantile(a2, probs = 0.025),
                                                        max_a2 = quantile(a2, probs = 0.975),
                                                        mean_sigma = mean(sigma),
                                                        sd_sigma = sd(sigma),
                                                        min_sigma = quantile(sigma, probs = 0.025),
                                                        max_sigma = quantile(sigma, probs = 0.975),
                                                        total_AIC = sum(AIC),
                                                        total_RMSE = sum(RMSE))


depth_power_b <- C_depth_resampling_power_log %>% dplyr::filter(weighted == "yes") %>%
                                                  dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
depth_power_b <- depth_power_b[depth_power_b$ID_tofilter %in% depth_power$ID_species,]

depth_power_results_b <- depth_power_b %>% dplyr::group_by(species) %>%
                                           dplyr::summarise(mean_a1 = mean(a1),
                                                            sd_a1 = sd(a1),
                                                            min_a1 = quantile(a1, probs = 0.025),
                                                            max_a1 = quantile(a1, probs = 0.975),
                                                            mean_a2 = mean(a2),
                                                            sd_a2 = sd(a2),
                                                            min_a2 = quantile(a2, probs = 0.025),
                                                            max_a2 = quantile(a2, probs = 0.975),
                                                            mean_sigma = mean(sigma),
                                                            sd_sigma = sd(sigma),
                                                            min_sigma = quantile(sigma, probs = 0.025),
                                                            max_sigma = quantile(sigma, probs = 0.975),
                                                            total_AIC = sum(AIC),
                                                            total_RMSE = sum(RMSE))



# Diameter model selection
diameter_resampling_linear <- A_diameter_resampling_linear[A_diameter_resampling_linear$species %in% sp_to_keep,]
diameter_linear <- diameter_resampling_linear %>% dplyr::filter(weighted == "no") %>%
                                                  dplyr::group_by(species) %>%
                                                  dplyr::filter(!is.na(intercept)) %>% 
                                                  dplyr::slice_sample(n = 100) %>%
                                                  dplyr::mutate(ID_b = X + 300) %>%
                                                  dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

diameter_linear_results <- diameter_linear %>% dplyr::group_by(species) %>%
                                                dplyr::summarise(mean_intercept = mean(intercept),
                                                                 sd_intercept = sd(intercept),
                                                                 min_intercept = quantile(intercept, probs = 0.025),
                                                                 max_intercept = quantile(intercept, probs = 0.975),
                                                                 mean_slope = mean(slope),
                                                                 sd_slope = sd(slope),
                                                                 min_slope = quantile(slope, probs = 0.025),
                                                                 max_slope = quantile(slope, probs = 0.975),
                                                                 total_AIC = sum(AIC),
                                                                 total_RMSE = sum(RMSE))


diameter_linear_b <- A_diameter_resampling_linear %>% dplyr::filter(weighted == "yes") %>%
                                                      dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
diameter_linear_b <- diameter_linear_b[diameter_linear_b$ID_tofilter %in% diameter_linear$ID_species,]

diameter_linear_results_b <- diameter_linear_b %>% dplyr::group_by(species) %>%
                                                    dplyr::summarise(mean_intercept = mean(intercept),
                                                                     sd_intercept = sd(intercept),
                                                                     min_intercept = quantile(intercept, probs = 0.025),
                                                                     max_intercept = quantile(intercept, probs = 0.975),
                                                                     mean_slope = mean(slope),
                                                                     sd_slope = sd(slope),
                                                                     min_slope = quantile(slope, probs = 0.025),
                                                                     max_slope = quantile(slope, probs = 0.975),
                                                                     total_AIC = sum(AIC),
                                                                     total_RMSE = sum(RMSE))

diameter_resampling_power <- C_diameter_resampling_power_log[C_diameter_resampling_power_log$species %in% sp_to_keep,]
diameter_power <- diameter_resampling_power %>% dplyr::filter(weighted == "no") %>%
                                                dplyr::group_by(species) %>%
                                                dplyr::filter(!is.na(a1)) %>% 
                                                dplyr::slice_sample(n = 100) %>%
                                                dplyr::mutate(ID_b = X + 300) %>%
                                                dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

diameter_power_results <- diameter_power %>% dplyr::group_by(species) %>%
                                              dplyr::summarise(mean_a1 = mean(a1),
                                                               sd_a1 = sd(a1),
                                                               min_a1 = quantile(a1, probs = 0.025),
                                                               max_a1 = quantile(a1, probs = 0.975),
                                                               mean_a2 = mean(a2),
                                                               sd_a2 = sd(a2),
                                                               min_a2 = quantile(a2, probs = 0.025),
                                                               max_a2 = quantile(a2, probs = 0.975),
                                                               mean_sigma = mean(sigma),
                                                               sd_sigma = sd(sigma),
                                                               min_sigma = quantile(sigma, probs = 0.025),
                                                               max_sigma = quantile(sigma, probs = 0.975),
                                                               total_AIC = sum(AIC),
                                                               total_RMSE = sum(RMSE))


diameter_power_b <- C_diameter_resampling_power_log %>% dplyr::filter(weighted == "yes") %>%
                                                        dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
diameter_power_b <- diameter_power_b[diameter_power_b$ID_tofilter %in% diameter_power$ID_species,]

diameter_power_results_b <- diameter_power_b %>% dplyr::group_by(species) %>%
                                                  dplyr::summarise(mean_a1 = mean(a1),
                                                                   sd_a1 = sd(a1),
                                                                   min_a1 = quantile(a1, probs = 0.025),
                                                                   max_a1 = quantile(a1, probs = 0.975),
                                                                   mean_a2 = mean(a2),
                                                                   sd_a2 = sd(a2),
                                                                   min_a2 = quantile(a2, probs = 0.025),
                                                                   max_a2 = quantile(a2, probs = 0.975),
                                                                   mean_sigma = mean(sigma),
                                                                   sd_sigma = sd(sigma),
                                                                   min_sigma = quantile(sigma, probs = 0.025),
                                                                   max_sigma = quantile(sigma, probs = 0.975),
                                                                   total_AIC = sum(AIC),
                                                                   total_RMSE = sum(RMSE))


### Computing height values from estimated parameters
for (i in 1:dim(height_asympt_results_b)[1]) {
  
  n_repetition = 100
  
  height_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5)) 
  height_dbh[,1] <- rep(height_asympt_results_b[i,"species"], n_repetition)
  names(height_dbh) <- c("species", "dbh_20", "dbh_30", "dbh_40", "dbh_60")
  
  mean_b1 = height_asympt_results_b$mean_b1[i]
  sd_b1 = height_asympt_results_b$sd_b1[i]

  mean_b2 = height_asympt_results_b$mean_b2[i]
  sd_b2 = height_asympt_results_b$sd_b2[i]

  mean_b3 = height_asympt_results_b$mean_b3[i]
  sd_b3 = height_asympt_results_b$sd_b3[i]

  
  for (j in 1:n_repetition) {
    
  b1 <- rnorm(1, mean = mean_b1, sd = sd_b1)
  b2 <- rnorm(1, mean = mean_b2, sd = sd_b2)
  b3 <- rnorm(1, mean = mean_b3, sd = sd_b3)
  
  height_dbh[j,"dbh_20"] = 1.3 + b1 * (1-exp(-b2 * 20)) ^ b3
  height_dbh[j,"dbh_30"] = 1.3 + b1 * (1-exp(-b2 * 30)) ^ b3
  height_dbh[j,"dbh_40"] = 1.3 + b1 * (1-exp(-b2 * 40)) ^ b3
  height_dbh[j,"dbh_60"] = 1.3 + b1 * (1-exp(-b2 * 60)) ^ b3
  
  }
  
  write.csv(height_dbh, file =  paste0("output/height_dbh_", height_asympt_results_b[i,"species"], ".csv"))
  
}
  

### Computing depth values from estimated parameters

for (i in 1:dim(depth_power_results_b)[1]) {
  
  n_repetition = 100
  
  depth_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5)) 
  depth_dbh[,1] <- rep(height_asympt_results_b[i,"species"], n_repetition)
  names(depth_dbh) <- c("species", "dbh_20", "dbh_30", "dbh_40", "dbh_60")
  
  mean_a1 = depth_power_results_b$mean_a1[i]
  sd_a1 = depth_power_results_b$sd_a1[i]
   
  mean_a2 = depth_power_results_b$mean_a2[i]
  sd_a2 = depth_power_results_b$sd_a2[i]
  
  mean_sigma = depth_power_results_b$mean_sigma[i]
  sd_sigma = depth_power_results_b$sd_sigma[i]

  
  for (j in 1:n_repetition) {
    
  a1 <- rnorm(1, mean = mean_a1, sd = sd_a1)
  a2 <- rnorm(1, mean = mean_a2, sd = sd_a2)
  sigma <- rnorm(1, mean = mean_sigma, sd = sd_sigma)
  
  depth_dbh[j,"dbh_20"] = a1 * (20^a2) * ((1/2)*(exp(sigma^2)))
  depth_dbh[j,"dbh_30"] = a1 * (30^a2) * ((1/2)*(exp(sigma^2)))
  depth_dbh[j,"dbh_40"] = a1 * (40^a2) * ((1/2)*(exp(sigma^2)))
  depth_dbh[j,"dbh_60"] = a1 * (60^a2) * ((1/2)*(exp(sigma^2)))
  
  }
  
  write.csv(depth_dbh, file =  paste0("output/depth_dbh_", depth_power_results_b[i,"species"], ".csv"))
  
}



### Computing diameter values from estimated parameters

for (i in 1:dim(diameter_power_results_b)[1]) {
  
  n_repetition = 100
  
  diameter_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 5)) 
  diameter_dbh[,1] <- rep(height_asympt_results_b[i,"species"], n_repetition)
  names(diameter_dbh) <- c("species", "dbh_20", "dbh_30", "dbh_40", "dbh_60")

  mean_a1 = diameter_power_results_b$mean_a1[i]
  sd_a1 = diameter_power_results_b$sd_a1[i]
  
  mean_a2 = diameter_power_results_b$mean_a2[i]
  sd_a2 = diameter_power_results_b$sd_a2[i]
  
  mean_sigma = diameter_power_results_b$mean_sigma[i]
  sd_sigma = diameter_power_results_b$sd_sigma[i]
  
  for (j in 1:n_repetition) {
    
  a1 <- rnorm(1, mean = mean_a1, sd = sd_a1)
  a2 <- rnorm(1, mean = mean_a2, sd = sd_a2)
  sigma <- rnorm(1, mean = mean_sigma, sd = sd_sigma)
  
  diameter_dbh[j,"dbh_20"] = a1 * (20^a2) * ((1/2)*(exp(sigma^2)))
  diameter_dbh[j,"dbh_30"] = a1 * (30^a2) * ((1/2)*(exp(sigma^2)))
  diameter_dbh[j,"dbh_40"] = a1 * (40^a2) * ((1/2)*(exp(sigma^2)))
  diameter_dbh[j,"dbh_60"] = a1 * (60^a2) * ((1/2)*(exp(sigma^2)))
  
  }
  
  write.csv(diameter_dbh, file =  paste0("output/diameter_dbh_", diameter_power_results_b[i,"species"], ".csv"))
  
}


dd <- list.files(path = "output/", pattern = "height_dbh_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

height_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

height_dbh_20 <- height_dbh_allsp %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean = mean(dbh_20, na.rm = TRUE),
                                                       min = quantile(dbh_20, probs = 0.025, na.rm = TRUE),
                                                       max = quantile(dbh_20, probs = 0.975, na.rm = TRUE),
                                                       sd = sd(dbh_20, na.rm = TRUE), 
                                                       se = se(dbh_20))

height_dbh_30 <- height_dbh_allsp %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean = mean(dbh_30, na.rm = TRUE),
                                                       min = quantile(dbh_30, probs = 0.025, na.rm = TRUE),
                                                       max = quantile(dbh_30, probs = 0.975, na.rm = TRUE),
                                                       sd = sd(dbh_30, na.rm = TRUE), 
                                                       se = se(dbh_30))

height_dbh_40 <- height_dbh_allsp %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean = mean(dbh_40, na.rm = TRUE),
                                                       min = quantile(dbh_40, probs = 0.025, na.rm = TRUE),
                                                       max = quantile(dbh_40, probs = 0.975, na.rm = TRUE),
                                                       sd = sd(dbh_40, na.rm = TRUE), 
                                                       se = se(dbh_40))

height_dbh_60 <- height_dbh_allsp %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean = mean(dbh_60, na.rm = TRUE),
                                                       min = quantile(dbh_60, probs = 0.025, na.rm = TRUE),
                                                       max = quantile(dbh_60, probs = 0.975, na.rm = TRUE),
                                                       sd = sd(dbh_60, na.rm = TRUE), 
                                                       se = se(dbh_60))


dd <- list.files(path = "output/", pattern = "depth_dbh_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

depth_dbh_20 <- depth_dbh_allsp %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean = mean(dbh_20, na.rm = TRUE),
                                                       min = quantile(dbh_20, probs = 0.025, na.rm = TRUE),
                                                       max = quantile(dbh_20, probs = 0.975, na.rm = TRUE),
                                                       sd = sd(dbh_20, na.rm = TRUE), 
                                                       se = se(dbh_20))

depth_dbh_30 <- depth_dbh_allsp %>% dplyr::group_by(species) %>%
                                    dplyr::summarise(mean = mean(dbh_30, na.rm = TRUE),
                                                     min = quantile(dbh_30, probs = 0.025, na.rm = TRUE),
                                                     max = quantile(dbh_30, probs = 0.975, na.rm = TRUE),
                                                     sd = sd(dbh_30, na.rm = TRUE), 
                                                     se = se(dbh_30))


depth_dbh_40 <- depth_dbh_allsp %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean = mean(dbh_40, na.rm = TRUE),
                                                       min = quantile(dbh_40, probs = 0.025, na.rm = TRUE),
                                                       max = quantile(dbh_40, probs = 0.975, na.rm = TRUE),
                                                       sd = sd(dbh_40, na.rm = TRUE), 
                                                       se = se(dbh_40))

depth_dbh_60 <- depth_dbh_allsp %>% dplyr::group_by(species) %>%
                                      dplyr::summarise(mean = mean(dbh_60, na.rm = TRUE),
                                                       min = quantile(dbh_60, probs = 0.025, na.rm = TRUE),
                                                       max = quantile(dbh_60, probs = 0.975, na.rm = TRUE),
                                                       sd = sd(dbh_60, na.rm = TRUE), 
                                                       se = se(dbh_60))



dd <- list.files(path = "output/", pattern = "diameter_dbh_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

diameter_dbh_20 <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean = mean(dbh_20, na.rm = TRUE),
                                                           min = quantile(dbh_20, probs = 0.025, na.rm = TRUE),
                                                           max = quantile(dbh_20, probs = 0.975, na.rm = TRUE),
                                                           sd = sd(dbh_20, na.rm = TRUE), 
                                                           se = se(dbh_20))

diameter_dbh_30 <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean = mean(dbh_30, na.rm = TRUE),
                                                           min = quantile(dbh_30, probs = 0.025, na.rm = TRUE),
                                                           max = quantile(dbh_30, probs = 0.975, na.rm = TRUE),
                                                           sd = sd(dbh_30, na.rm = TRUE), 
                                                           se = se(dbh_30))

                                        
diameter_dbh_40 <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean = mean(dbh_40, na.rm = TRUE),
                                                           min = quantile(dbh_40, probs = 0.025, na.rm = TRUE),
                                                           max = quantile(dbh_40, probs = 0.975, na.rm = TRUE),
                                                           sd = sd(dbh_40, na.rm = TRUE), 
                                                           se = se(dbh_40))

diameter_dbh_60 <- diameter_dbh_allsp %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean = mean(dbh_60, na.rm = TRUE),
                                                           min = quantile(dbh_60, probs = 0.025, na.rm = TRUE),
                                                           max = quantile(dbh_60, probs = 0.975, na.rm = TRUE),
                                                           sd = sd(dbh_60, na.rm = TRUE), 
                                                           se = se(dbh_60))

## Plotting results using forest plots
sepr <- 0.2 # barre de separation

clrs <- c("#2E8B57","#FFC000") # defining colors

# Adding tree type within the files to separate colors

types <- final_sp_list %>% dplyr::select(species, Evergreen)
types[types$species == "Pinus pinea",]$Evergreen <- "Y"
types[types$species == "Pinus pinaster",]$Evergreen <- "Y"
types[types$species == "Quercus faginea",]$Evergreen <- "N"
types$evergreen <-  c("Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "No", 
                      "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",
                      "No", "No", "No", "Yes", "No", "No", "No", "Yes", "No", "No", 
                      "Yes" ,"Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                      "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "Yes",
                      "No", "Yes", "No", "No",  "No", "Yes", "No", "No", "No",
                      "No", "No", "No", "No", "No", "No", "Yes", "Yes", "No", "Yes", "Yes", "No")

height_dbh_20 <- left_join(height_dbh_20, types, by = "species")
height_dbh_30 <- left_join(height_dbh_30, types, by = "species")
height_dbh_40 <- left_join(height_dbh_40, types, by = "species")

depth_dbh_20 <- left_join(depth_dbh_20, types, by = "species")
depth_dbh_30 <- left_join(depth_dbh_30, types, by = "species")
depth_dbh_40 <- left_join(depth_dbh_40, types, by = "species")

diameter_dbh_20 <- left_join(diameter_dbh_20, types, by = "species")
diameter_dbh_30 <- left_join(diameter_dbh_30, types, by = "species")
diameter_dbh_40 <- left_join(diameter_dbh_40, types, by = "species")

height_dbh_20 <- arrange(height_dbh_20, mean)
species <- as.data.frame(height_dbh_20$species)
colnames(species) <- "species"

height_dbh_40 <- left_join(species, height_dbh_40, by = "species")

depth_dbh_20 <- left_join(species, depth_dbh_20, by = "species")
depth_dbh_40 <- left_join(species, depth_dbh_40, by = "species")

diameter_dbh_20 <- left_join(species, diameter_dbh_20, by = "species")
diameter_dbh_40 <- left_join(species, diameter_dbh_40, by = "species")

# Let's go with the plots!

par(mfrow = c(dim(height_dbh_20)[1],1))
par(mar = c(0,0,0,0))
par(oma = c(4,15,2,0)) 

plotx_height <- c(5, 40) # x range for height values

for (i in 1:dim(height_dbh_20)[1]) {
  
  plot(range(plotx_height), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (height_dbh_20[i,"evergreen"] =="Yes") {
  
  polygon(x = c((height_dbh_20[i, "mean"] - height_dbh_20[i, "se"]), (height_dbh_20[i, "mean"] - height_dbh_20[i, "se"]), 
                (height_dbh_20[i, "mean"] + height_dbh_20[i, "se"]), (height_dbh_20[i, "mean"] + height_dbh_20[i, "se"])), 
          y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
          col = clrs[1], border = clrs[1])
  
  mean = height_dbh_20[i, "mean"]
  mean_0 <- mean$mean
  segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
  
  mtext(height_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
  
  } else {

      polygon(x = c((height_dbh_20[i, "mean"] - height_dbh_20[i, "se"]), (height_dbh_20[i, "mean"] - height_dbh_20[i, "se"]), 
                    (height_dbh_20[i, "mean"] + height_dbh_20[i, "se"]), (height_dbh_20[i, "mean"] + height_dbh_20[i, "se"])), 
              y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
              col = clrs[2], border = clrs[2])
      
    mean = height_dbh_20[i, "mean"]
    mean_0 <- mean$mean
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(height_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }

}


axis(side = 1, at = c(5, 10, 15, 20, 25, 30, 35, 40), 
     labels = c("5", "10", "15", "20", "25", "30", "35", "40"),
     cex.axis = 1.2) # x-axis



for (i in 1:dim(height_dbh_40)[1]) {
  
  plot(range(plotx_height), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (height_dbh_40[i,"evergreen"] ==  "Yes") {
    
    polygon(x = c((height_dbh_40[i, "mean"] - height_dbh_40[i, "se"]), (height_dbh_40[i, "mean"] - height_dbh_40[i, "se"]), 
                  (height_dbh_40[i, "mean"] + height_dbh_40[i, "se"]), (height_dbh_40[i, "mean"] + height_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[1], border = clrs[1])
    
    mean_0 = height_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(height_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  } else {
    
    polygon(x = c((height_dbh_40[i, "mean"] - height_dbh_40[i, "se"]), (height_dbh_40[i, "mean"] - height_dbh_40[i, "se"]), 
                  (height_dbh_40[i, "mean"] + height_dbh_40[i, "se"]), (height_dbh_40[i, "mean"] + height_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[2], border = clrs[2])
    
    mean_0 = height_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(height_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }
  
}

axis(side = 1, at = c(5, 10, 15, 20, 25, 30, 35, 40), 
     labels = c("5", "10", "15", "20", "25", "30", "35", "40"),
     cex.axis = 1.2) # x-axis



par(mfrow = c(dim(depth_dbh_20)[1],1))
par(mar = c(0,0,0,0))
par(oma = c(4,15,2,0)) 

plotx_depth <- c(0, 30) # x range for depth values

for (i in 1:dim(depth_dbh_20)[1]) {
  
  plot(range(plotx_depth), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (depth_dbh_20[i,"evergreen"] =="Yes") {
    
    polygon(x = c((depth_dbh_20[i, "mean"] - depth_dbh_20[i, "se"]), (depth_dbh_20[i, "mean"] - depth_dbh_20[i, "se"]), 
                  (depth_dbh_20[i, "mean"] + depth_dbh_20[i, "se"]), (depth_dbh_20[i, "mean"] + depth_dbh_20[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[1], border = clrs[1])
    
    mean_0 = depth_dbh_20[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(depth_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  } else {
    
    polygon(x = c((depth_dbh_20[i, "mean"] - depth_dbh_20[i, "se"]), (depth_dbh_20[i, "mean"] - depth_dbh_20[i, "se"]), 
                  (depth_dbh_20[i, "mean"] + depth_dbh_20[i, "se"]), (depth_dbh_20[i, "mean"] + depth_dbh_20[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[2], border = clrs[2])
    
    mean_0 = depth_dbh_20[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(depth_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }
  
}


axis(side = 1, at = c(0, 5, 10, 15, 20, 25, 30), 
     labels = c("0", "5", "10", "15", "20", "25", "30"),
     cex.axis = 1.2) # x-axis



for (i in 1:dim(depth_dbh_40)[1]) {
  
  plot(range(plotx_depth), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (depth_dbh_40[i,"evergreen"] ==  "Yes") {
    
    polygon(x = c((depth_dbh_40[i, "mean"] - depth_dbh_40[i, "se"]), (depth_dbh_40[i, "mean"] - depth_dbh_40[i, "se"]), 
                  (depth_dbh_40[i, "mean"] + depth_dbh_40[i, "se"]), (depth_dbh_40[i, "mean"] + depth_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[1], border = clrs[1])
    
    mean_0 = depth_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(depth_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  } else {
    
    polygon(x = c((depth_dbh_40[i, "mean"] - depth_dbh_40[i, "se"]), (depth_dbh_40[i, "mean"] - depth_dbh_40[i, "se"]), 
                  (depth_dbh_40[i, "mean"] + depth_dbh_40[i, "se"]), (depth_dbh_40[i, "mean"] + depth_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[2], border = clrs[2])
    
    mean_0 = depth_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(depth_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }
  
}

axis(side = 1, at = c(0, 5, 10, 15, 20, 25, 30), 
     labels = c("0", "5", "10", "15", "20", "25", "30"),
     cex.axis = 1.2) # x-axis



par(mfrow = c(dim(diameter_dbh_20)[1],1))
par(mar = c(0,0,0,0))
par(oma = c(4,15,2,0)) 

plotx_diam <- c(0, 20) # x range for diameter values

for (i in 1:dim(diameter_dbh_20)[1]) {
  
  plot(range(plotx_diam), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (diameter_dbh_20[i,"evergreen"] =="Yes") {
    
    polygon(x = c((diameter_dbh_20[i, "mean"] - diameter_dbh_20[i, "se"]), (diameter_dbh_20[i, "mean"] - diameter_dbh_20[i, "se"]), 
                  (diameter_dbh_20[i, "mean"] + diameter_dbh_20[i, "se"]), (diameter_dbh_20[i, "mean"] + diameter_dbh_20[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[1], border = clrs[1])
    
    mean_0 = diameter_dbh_20[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(diameter_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  } else {
    
    polygon(x = c((diameter_dbh_20[i, "mean"] - diameter_dbh_20[i, "se"]), (diameter_dbh_20[i, "mean"] - diameter_dbh_20[i, "se"]), 
                  (diameter_dbh_20[i, "mean"] + diameter_dbh_20[i, "se"]), (diameter_dbh_20[i, "mean"] + diameter_dbh_20[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[2], border = clrs[2])
    
    mean_0 = diameter_dbh_20[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(diameter_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }
  
}


axis(side = 1, at = c(0, 5, 10, 15, 20), 
     labels = c("0", "5", "10", "15", "20"),
     cex.axis = 1.2) # x-axis



for (i in 1:dim(diameter_dbh_40)[1]) {
  
  plot(range(plotx_diam), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (diameter_dbh_40[i,"evergreen"] ==  "Yes") {
    
    polygon(x = c((diameter_dbh_40[i, "mean"] - diameter_dbh_40[i, "se"]), (diameter_dbh_40[i, "mean"] - diameter_dbh_40[i, "se"]), 
                  (diameter_dbh_40[i, "mean"] + diameter_dbh_40[i, "se"]), (diameter_dbh_40[i, "mean"] + diameter_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[1], border = clrs[1])
    
    mean_0 = diameter_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(diameter_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  } else {
    
    polygon(x = c((diameter_dbh_40[i, "mean"] - diameter_dbh_40[i, "se"]), (diameter_dbh_40[i, "mean"] - diameter_dbh_40[i, "se"]), 
                  (diameter_dbh_40[i, "mean"] + diameter_dbh_40[i, "se"]), (diameter_dbh_40[i, "mean"] + diameter_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[2], border = clrs[2])
    
    mean_0 = diameter_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(diameter_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }
  
}

axis(side = 1, at = c(0, 5, 10, 15, 20), 
     labels = c("0", "5", "10", "15", "20"),
     cex.axis = 1.2) # x-axis




### Estimation of crown volume for each species

depth_power_results_b <- left_join(depth_power_results_b, types, by = "species")

for (i in 1:dim(depth_power_results_b)[1]) {
  
  n_repetition = 100
  
  volume_dbh <- as.data.frame(matrix(nrow = n_repetition, ncol = 4)) 
  volume_dbh[,1] <- rep(height_asympt_results_b[i,"species"], n_repetition)
  names(volume_dbh) <- c("species", "dbh_20", "dbh_30", "dbh_40")
  
  
  # extracting depth parameters
  mean_depth_a1 = depth_power_results_b$mean_a1[i]
  sd_depth_a1 = depth_power_results_b$sd_a1[i]
  
  mean_depth_a2 = depth_power_results_b$mean_a2[i]
  sd_depth_a2 = depth_power_results_b$sd_a2[i]
  
  mean_depth_sigma = depth_power_results_b$mean_sigma[i]
  sd_depth_sigma = depth_power_results_b$sd_sigma[i]
  
  
  # extracting depth parameters
  diameter_data <- diameter_power_results_b[diameter_power_results_b$species %in% unique(depth_power_results_b[i,"species"]),]
  
  mean_diameter_a1 = diameter_data$mean_a1
  sd_diameter_a1 = diameter_data$sd_a1
  
  mean_diameter_a2 = diameter_data$mean_a2
  sd_diameter_a2 = diameter_data$sd_a2
  
  mean_diameter_sigma = diameter_data$mean_sigma
  sd_diameter_sigma = diameter_data$sd_sigma
  
  
  for (j in 1:n_repetition) {

    a1_depth <- rnorm(1, mean = mean_depth_a1, sd = sd_depth_a1)
    a2_depth <- rnorm(1, mean = mean_depth_a2, sd = sd_depth_a2)
    sigma_depth <- rnorm(1, mean = mean_depth_sigma, sd = sd_depth_sigma)
    
    depth_20 <- a1_depth * (20^a2_depth) * ((1/2)*(exp(sigma_depth^2)))
    depth_30 <- a1_depth * (30^a2_depth) * ((1/2)*(exp(sigma_depth^2)))
    depth_40 <- a1_depth * (40^a2_depth) * ((1/2)*(exp(sigma_depth^2)))
    
    a1_diameter <- rnorm(1, mean = mean_diameter_a1, sd = sd_diameter_a1)
    a2_diameter <- rnorm(1, mean = mean_diameter_a2, sd = sd_diameter_a2)
    sigma_diameter <- rnorm(1, mean = mean_diameter_sigma, sd = sd_diameter_sigma)
    
    diameter_20 <- a1_diameter * (20^a2_diameter) * ((1/2)*(exp(sigma_diameter^2)))
    diameter_30 <- a1_diameter * (30^a2_diameter) * ((1/2)*(exp(sigma_diameter^2)))
    diameter_40 <- a1_diameter * (40^a2_diameter) * ((1/2)*(exp(sigma_diameter^2)))
    
    
    if (height_asympt_results_b[i,"evergreen"] ==  "Yes") {
    
    volume_dbh[j,"dbh_20"] = (pi * (diameter_20/2)^2 * depth_20)/(2 * 0.26 + 1)
    volume_dbh[j,"dbh_30"] = (pi * (diameter_30/2)^2 * depth_30)/(2 * 0.26 + 1)
    volume_dbh[j,"dbh_40"] = (pi * (diameter_40/2)^2 * depth_40)/(2 * 0.26 + 1)
    
    } else {
    
      volume_dbh[j,"dbh_20"] = (pi * (diameter_20/2)^2 * depth_20)/(2 * 0.4 + 1)
      volume_dbh[j,"dbh_30"] = (pi * (diameter_30/2)^2 * depth_30)/(2 * 0.44 + 1)
      volume_dbh[j,"dbh_40"] = (pi * (diameter_40/2)^2 * depth_40)/(2 * 0.44 + 1)
    
  }
  
  }
  
  write.csv(volume_dbh, file =  paste0("output/volume_dbh_", depth_power_results_b[i,"species"], ".csv"))
  
}



dd <- list.files(path = "output/", pattern = "volume_dbh_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

volume_dbh_allsp <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

volume_dbh_20 <- volume_dbh_allsp %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean = mean(dbh_20, na.rm = TRUE),
                                                           min = quantile(dbh_20, probs = 0.025, na.rm = TRUE),
                                                           max = quantile(dbh_20, probs = 0.975, na.rm = TRUE),
                                                           sd = sd(dbh_20, na.rm = TRUE), 
                                                           se = se(dbh_20))

volume_dbh_30 <- volume_dbh_allsp %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean = mean(dbh_30, na.rm = TRUE),
                                                           min = quantile(dbh_30, probs = 0.025, na.rm = TRUE),
                                                           max = quantile(dbh_30, probs = 0.975, na.rm = TRUE),
                                                           sd = sd(dbh_30, na.rm = TRUE), 
                                                           se = se(dbh_30))


volume_dbh_40 <- volume_dbh_allsp %>% dplyr::group_by(species) %>%
                                          dplyr::summarise(mean = mean(dbh_40, na.rm = TRUE),
                                                           min = quantile(dbh_40, probs = 0.025, na.rm = TRUE),
                                                           max = quantile(dbh_40, probs = 0.975, na.rm = TRUE),
                                                           sd = sd(dbh_40, na.rm = TRUE), 
                                                           se = se(dbh_40))



height_dbh_20 <- arrange(height_dbh_20, mean)
species <- as.data.frame(height_dbh_20$species)
colnames(species) <- "species"

volume_dbh_20 <- left_join(species, volume_dbh_20, by = "species")
volume_dbh_40 <- left_join(species, volume_dbh_40, by = "species")

volume_dbh_20 <- left_join(volume_dbh_20, types, by = "species")
volume_dbh_40 <- left_join(volume_dbh_40, types, by = "species")

par(mfrow = c(dim(volume_dbh_20)[1],1))
par(mar = c(0,0,0,0))
par(oma = c(4,15,2,0)) 

plotx_volume <- c(0, 600) # x range for diameter values

for (i in 1:dim(volume_dbh_20)[1]) {
  
  plot(range(plotx_volume), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (volume_dbh_20[i,"evergreen"] =="Yes") {
    
    polygon(x = c((volume_dbh_20[i, "mean"] - volume_dbh_20[i, "se"]), (volume_dbh_20[i, "mean"] - volume_dbh_20[i, "se"]), 
                  (volume_dbh_20[i, "mean"] + volume_dbh_20[i, "se"]), (volume_dbh_20[i, "mean"] + volume_dbh_20[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[1], border = clrs[1])
    
    mean_0 = volume_dbh_20[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(volume_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  } else {
    
    polygon(x = c((volume_dbh_20[i, "mean"] - volume_dbh_20[i, "se"]), (volume_dbh_20[i, "mean"] - volume_dbh_20[i, "se"]), 
                  (volume_dbh_20[i, "mean"] + volume_dbh_20[i, "se"]), (volume_dbh_20[i, "mean"] + volume_dbh_20[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[2], border = clrs[2])
    
    mean_0 = volume_dbh_20[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(volume_dbh_20[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }
  
}

axis(side = 1, at = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600), 
     labels = c("0", "50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600"),
     cex.axis = 1.2) # x-axis


for (i in 1:dim(volume_dbh_40)[1]) {
  
  plot(range(plotx_volume), c(1, 2), type = "n", bty = "n",
       ylim = c(1-(1-sepr)/2-sepr, 2+(1-sepr)/2+sepr),
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  
  if (volume_dbh_40[i,"evergreen"] =="Yes") {
    
    polygon(x = c((volume_dbh_40[i, "mean"] - volume_dbh_40[i, "se"]), (volume_dbh_40[i, "mean"] - volume_dbh_40[i, "se"]), 
                  (volume_dbh_40[i, "mean"] + volume_dbh_40[i, "se"]), (volume_dbh_40[i, "mean"] + volume_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[1], border = clrs[1])
    
    mean_0 = volume_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(volume_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  } else {
    
    polygon(x = c((volume_dbh_40[i, "mean"] - volume_dbh_40[i, "se"]), (volume_dbh_40[i, "mean"] - volume_dbh_40[i, "se"]), 
                  (volume_dbh_40[i, "mean"] + volume_dbh_40[i, "se"]), (volume_dbh_40[i, "mean"] + volume_dbh_40[i, "se"])), 
            y = c(1-(1-sepr)/2, 1+(1-sepr)/2, 1+(1-sepr)/2, 1-(1-sepr)/2),
            col = clrs[2], border = clrs[2])
    
    mean_0 = volume_dbh_40[i, "mean"]
    segments(x0 = mean_0, y0 = 1-(1-sepr)/2, y1 = 1+(1-sepr)/2, col = "black", lwd = 2) 
    
    mtext(volume_dbh_40[i,"species"], side = 2, line = 1, las = 2, font = 1, cex = 0.8) 
    
  }
  
}

axis(side = 1, at = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600), 
     labels = c("0", "50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600"),
     cex.axis = 1.2) # x-axis






require(corrplot)

cor_file <- join_all(list(height_dbh_30, depth_dbh_30, diameter_dbh_30, volume_dbh_30), by = "species", type = "left")
colnames(cor_file) <- c("species", "mean_height", "min_height", "max_height", "sd_height", "se_height", "Evergreen", "evergreen_b", 
                        "mean_depth", "min_depth", "max_depth", "sd_depth", "se_depth", "Evergreen", "evergreen",
                        "mean_diameter", "min_diameter", "max_diameter", "sd_diameter", "se_diameter", "Evergreen", "evergreen",
                        "mean_volume", "min_volume", "max_volume", "sd_volume", "se_volume")

final_sp_list <- left_join(height_dbh_20, final_sp_list, by = "species")
final_sp_list <- final_sp_list <- dplyr::select()

cor_file <- left_join(cor_file, final_sp_list, by = "species")

cor_file <- cor_file %>% dplyr::select(species, mean_height, mean_depth, mean_diameter, mean_volume, evergreen_b)
cor_file_b <- cor_file %>% dplyr::select(-species, -evergreen_b)
cor_matrix <- cor(cor_file_b, method = "pearson")
cor_matrix_b <- cor(cor_file_b, method = "kendall")
head(round(cor_matrix, 2))


par(mar = c(0,0,0,0))
par(mfrow = c(3,2))
par(oma = c(0,0,0,0))
corrplot(cor_matrix, method = "circle", type = "upper", order="hclust", 
         col = brewer.pal(n = 10, name = "RdYlBu"),
         tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_b, method = "circle", type = "upper", order="hclust", 
         col = brewer.pal(n = 10, name = "RdYlBu"),
         tl.col = "black", tl.srt = 45)

cor_file_c <- cor_file %>% dplyr::filter(evergreen_b == "Yes") %>%
                           dplyr::select(-species, -evergreen_b)
cor_matrix <- cor(cor_file_c, method = "pearson")
cor_matrix_b <- cor(cor_file_c, method = "kendall")


corrplot(cor_matrix, method = "circle", type = "upper", order="hclust", 
         col = brewer.pal(n = 10, name = "RdYlBu"),
         tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_b, method = "circle", type = "upper", order="hclust", 
         col = brewer.pal(n = 10, name = "RdYlBu"),
         tl.col = "black", tl.srt = 45)

cor_file_d <- cor_file %>% dplyr::filter(evergreen_b == "No") %>%
                           dplyr::select(-species, -evergreen_b)
cor_matrix <- cor(cor_file_d, method = "pearson")
cor_matrix_b <- cor(cor_file_d, method = "kendall")


corrplot(cor_matrix, method = "circle", type = "upper", order="hclust", 
         col = brewer.pal(n = 10, name = "RdYlBu"),
         tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_b, method = "circle", type = "upper", order="hclust", 
         col = brewer.pal(n = 10, name = "RdYlBu"),
         tl.col = "black", tl.srt = 45)




# Heightdepth model selection
heightdepth_resampling_linear <- A_heightdepth_resampling_linear[A_heightdepth_resampling_linear$species %in% sp_to_keep,]
heightdepth_linear <- heightdepth_resampling_linear %>% dplyr::filter(weighted == "no") %>%
                                                        dplyr::group_by(species) %>%
                                                        dplyr::filter(!is.na(intercept)) %>% 
                                                        dplyr::slice_sample(n = 100) %>%
                                                        dplyr::mutate(ID_b = X + 300) %>%
                                                        dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

heightdepth_linear_results <- heightdepth_linear %>% dplyr::group_by(species) %>%
                                                      dplyr::summarise(mean_intercept = mean(intercept),
                                                                       min_intercept = quantile(intercept, probs = 0.025),
                                                                       max_intercept = quantile(intercept, probs = 0.975),
                                                                       mean_slope = mean(slope),
                                                                       min_slope = quantile(slope, probs = 0.025),
                                                                       max_slope = quantile(slope, probs = 0.975),
                                                                       total_AIC = sum(AIC),
                                                                       total_RMSE = sum(RMSE))


heightdepth_linear_b <- A_heightdepth_resampling_linear %>% dplyr::filter(weighted == "yes") %>%
                                                            dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
heightdepth_linear_b <- heightdepth_linear_b[heightdepth_linear_b$ID_tofilter %in% heightdepth_linear$ID_species,]

heightdepth_linear_results_b <- heightdepth_linear_b %>% dplyr::group_by(species) %>%
                                                          dplyr::summarise(mean_intercept = mean(intercept),
                                                                           min_intercept = quantile(intercept, probs = 0.025),
                                                                           max_intercept = quantile(intercept, probs = 0.975),
                                                                           mean_slope = mean(slope),
                                                                           min_slope = quantile(slope, probs = 0.025),
                                                                           max_slope = quantile(slope, probs = 0.975),
                                                                           total_AIC = sum(AIC),
                                                                           total_RMSE = sum(RMSE))

heightdepth_resampling_power <- C_heightdepth_resampling_power_log[C_heightdepth_resampling_power_log$species %in% sp_to_keep,]
heightdepth_power <- heightdepth_resampling_power %>% dplyr::filter(weighted == "no") %>%
                                                      dplyr::group_by(species) %>%
                                                      dplyr::filter(!is.na(a1)) %>% 
                                                      dplyr::slice_sample(n = 100) %>%
                                                      dplyr::mutate(ID_b = X + 300) %>%
                                                      dplyr::mutate(ID_species = paste(ID_b, species, sep = "_"))

heightdepth_power_results <- heightdepth_power %>% dplyr::group_by(species) %>%
                                                  dplyr::summarise(mean_a1 = mean(a1),
                                                                   min_a1 = quantile(a1, probs = 0.025),
                                                                   max_a1 = quantile(a1, probs = 0.975),
                                                                   mean_a2 = mean(a2),
                                                                   min_a2 = quantile(a2, probs = 0.025),
                                                                   max_a2 = quantile(a2, probs = 0.975),
                                                                   mean_sigma = mean(sigma),
                                                                   min_sigma = quantile(sigma, probs = 0.025),
                                                                   max_sigma = quantile(sigma, probs = 0.975),
                                                                   total_AIC = sum(AIC),
                                                                   total_RMSE = sum(RMSE))


heightdepth_power_b <- C_heightdepth_resampling_power_log %>% dplyr::filter(weighted == "yes") %>%
  dplyr::mutate(ID_tofilter = paste(X, species, sep = "_"))
heightdepth_power_b <- heightdepth_power_b[heightdepth_power_b$ID_tofilter %in% heightdepth_power$ID_species,]

heightdepth_power_results_b <- heightdepth_power_b %>% dplyr::group_by(species) %>%
  dplyr::summarise(mean_a1 = mean(a1),
                   min_a1 = quantile(a1, probs = 0.025),
                   max_a1 = quantile(a1, probs = 0.975),
                   mean_a2 = mean(a2),
                   min_a2 = quantile(a2, probs = 0.025),
                   max_a2 = quantile(a2, probs = 0.975),
                   mean_sigma = mean(sigma),
                   min_sigma = quantile(sigma, probs = 0.025),
                   max_sigma = quantile(sigma, probs = 0.975),
                   total_AIC = sum(AIC),
                   total_RMSE = sum(RMSE))


# Loading species list for each explored allometric relationship
height_species <- read.csv(file = "data/height_species_list.csv")
height_species <- height_species %>% select(x) %>% rename(species = x)

diameter_species <- read.csv(file = "data/diameter_species_list.csv")
diameter_species <- diameter_species %>% select(x) %>% rename(species = x)
diameter_species_comp <- read.csv(file = "data/diameter_comp_species_list.csv")
diameter_species_comp <- diameter_species_comp %>% select(x) %>% rename(species = x)

depth_species <- read.csv(file = "data/depth_species_list.csv")
depth_species <- depth_species %>% select(x) %>% rename(species = x)
depth_species_comp <- read.csv(file = "data/depth_comp_species_list.csv")
depth_species_comp <- depth_species_comp %>% select(x) %>% rename(species = x)

heightdepth_species <- read.csv(file = "data/heightdepth_species_list.csv")
heightdepth_species <- heightdepth_species %>% select(x) %>% rename(species = x)
heightdepth_species_comp <- read.csv(file = "data/heightdepth_comp_species_list.csv")
heightdepth_species_comp <- heightdepth_species_comp %>% select(x) %>% rename(species = x)














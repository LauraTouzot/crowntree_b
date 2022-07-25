## Analysing model outputs (crown tree allometry)

# Loading packages
require(dplyr)
require(tidyverse)
require(data.table)

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


# Relationship #1: total tree height ~ diameter at breast height

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



# Relationship #2: crown depth ~ diameter at breast height

dd <- list.files(path = "output/", pattern = "linear_depth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_linear <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_depth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_depth_alldata_nocomp_rs_log_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_depth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_depth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_depth_c1_rs_log_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_depth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_depth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_depth_c2_rs_log_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

depth_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)



# Relationship #3: crown diameter ~ diameter at breast height

dd <- list.files(path = "output/", pattern = "linear_diameter_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_linear <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_diameter_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_diameter_nocomp_rs_log_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_diameter_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_diameter_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_diameter_c1_rs_log_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_diameter_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_diameter_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_diameter_c2_rs_log_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

diameter_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)







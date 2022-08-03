## Analyzing model outputs (crown tree allometry)

### Loading packages (to be removed)
require(dplyr)
require(tidyverse)
require(data.table)

### Loading model outputs for each explored allometric relationship

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


dd <- list.files(path = "output/", pattern = "power_depth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

B_depth_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_log_depth_nocomp_rs_") 
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

C_depth_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_depth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)


# resampling - competition ; ba plot
D_depth_resampling_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_depth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

E_depth_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_depth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

F_depth_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_depth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

# resampling - competition : ba larger trees
G_depth_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_depth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

H_depth_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


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


dd <- list.files(path = "output/", pattern = "power_diameter_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

B_diameter_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_log_diameter_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

C_diameter_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_diameter_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

# resampling - competition : ba plot
D_diameter_resampling_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_diameter_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

E_diameter_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_diameter_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

F_diameter_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "linear_diameter_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

# resampling - competition : ba larger trees
G_diameter_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_diameter_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

H_diameter_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


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


dd <- list.files(path = "output/", pattern = "power_heightdepth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

B_heightdepth_resampling_power <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_log_heightdepth_nocomp_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

C_heightdepth_resampling_power_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

# resampling - competition : ba plot
dd <- list.files(path = "output/", pattern = "linear_heightdepth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

D_heightdepth_linear_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_heightdepth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

E_heightdepth_resampling_power_c1 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_heightdepth_c1_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

F_heightdepth_resampling_power_c1_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


# resampling - competition : ba larger trees
dd <- list.files(path = "output/", pattern = "linear_heightdepth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)


G_heightdepth_resampling_linear_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

dd <- list.files(path = "output/", pattern = "power_heightdepth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

H_heightdepth_resampling_power_c2 <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)


dd <- list.files(path = "output/", pattern = "power_log_heightdepth_c2_rs_")
data_list <- lapply(paste0("output/", dd), utils::read.table,
                    header = TRUE, sep = ",", dec = ".",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

I_heightdepth_resampling_power_c2_log <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)








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














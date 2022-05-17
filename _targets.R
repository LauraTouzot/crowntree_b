########################################    ALLOMETRY SCRIPT    ####################################################
#
# compile the allometry database
# extract species that are found in the NFI database based on determined number of plots and observations available
# run models to explore allometry relationships using nlme (frequentist)
# run models to explore allometry relationships using nimble (bayesian)
# compute figures of obtained results
#
#####################################################################################################################


######## Options and packages 

# Loading targets
library(targets)

# Loading functions
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))

# Specifying target options
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")
# tar_option_set(packages = packages.in)
tar_option_set(packages = c("dplyr", "nlme", "lme4", "clustermq", "tidyr"))

list(
  tar_target(species_list, get_species_list()),
  tar_target(outputs_height_nlme, height_models_nlme(species_list), pattern = map(species_list)),
  tar_target(outputs_diameter_nlme, diameter_models_nlme(species_list), pattern = map(species_list)),
  tar_target(outputs_depth_nlme, depth_models_nlme(species_list), pattern = map(species_list)),
  tar_target(outputs_depth_height_nlme, depth_height_models_nlme(species_list), pattern = map(species_list))
  )

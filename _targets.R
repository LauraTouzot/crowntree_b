########################################    ALLOMETRY SCRIPT    ####################################################
#
# compile the allometry database
# extract species that are found in the NFI database based on determined number of plots and observations available
# run models to explore allometry relationships 
# compute figures of obtained results
#
#####################################################################################################################


######## Options and packages 

# Loading targets
library(targets)
# require(baad.data)
# require(stringr)
# require(dplyr)
# require(sp)
# require(rworldmap)
# require(rgdal)
# require(measurements)
# require(sf)
# require(readxl)
# require(stringi)
# require(stringr)
# require(lubridate)
# require(tidyr)
# require(parzer)
# require(TNRS)
# require(ggplot2)
# require(lme4)
# require(nlme)
# require(Metrics)

# tar_make_clustermq(outputs_diameter_nlme, workers = 4)

# Loading functions
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))

# # Installing if needed and loading packages
packages.in <- c("stringr", "dplyr", "sp", "rworldmap", "rgdal", "measurements",
                 "sf", "readxl", "stringi", "lubridate", "tidyr", "parzer",
                 "TNRS", "ggplot2", "clustermq", "nlme", "lme4", "pals", "gdata", "Metrics")

for (i in 1:length(packages.in)) if(!(packages.in[i] %in% rownames(installed.packages()))) install.packages(packages.in[i])


# Specifying target options
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")
tar_option_set(packages = packages.in, 
               memory = "transient", garbage_collection = TRUE)


#debug = "depth_resampling_c2_output"
#library(tictoc)
#tic()
#tar_make_clustermq( workers = 40)
#toc()

list(
  
  # ### 1. Compiling all datasets
  # 
  # # baad database
  # tar_target(BAAD_crown_sp_file, "data/BAAD/res_crown_BAAD.csv", format = "file"),
  # tar_target(BAAD_crown_sp, read.csv(BAAD_crown_sp_file)),
  # tar_target(baad, baad_data()),
  # tar_target(baad_f, prepare_dataset_baad(baad)),
  # tar_target(baad_crown, data_formatted_baad(baad_f)),
  # tar_target(baad_crown_complete, extract_continents_baad(baad_crown)),
  # 
  # # FHM database
  # tar_target(FHM_data_tree, read_FHM_data()),
  # tar_target(FHM_data, get_FHM_weight(FHM_data_tree)),
  # 
  # # FIA database
  # tar_target(FIA_sp_file, "data/FIA/Crown_FIA_Tree_Plot_Coord.csv", format = "file"),
  # tar_target(FIA_data, read.csv(FIA_sp_file)),
  # 
  # # Spanish NFI data
  # tar_target(Spain_NFI, read_spanish_data()),
  # 
  # # FUNDIV data
  # tar_target(FUNDIV_data, read_FUNDIV_data()),
  # 
  # # FUNDIV crown data
  # tar_target(FUNDIV_crown_data, prepare_FUNDIV_explore_crown()),
  # tar_target(FUNDIV_crown, get_FUNDIV_coordinates(FUNDIV_crown_data)),
  # 
  # # MONTANE data (unpublished data from Belledonne)
  # tar_target(MONTANE_data, read_MONTANE()),
  # 
  # # data from Fuhr et al. (2017)
  # tar_target(data_paper_crown, crown_data_paper()),
  # 
  # # data from Evans et al. (2015)
  # tar_target(evan_crown_data, data_evans_crown()),
  # 
  # # data from Dettmann and MacFarlane (2018)
  # tar_target(dettmann_data, data_dettmann()),
  # 
  # # data from Heym et al. (2017)
  # tar_target(heym_2017, read_heym_crown_data()),
  # 
  # # GenTree database
  # tar_target(gen_tree_data, GenTree_data()),
  # 
  # # Legagy Tree database
  # tar_target(legacytree, Legacy_Tree_crown_data()),
  # 
  # # ICP database
  # tar_target(ICP_data, read_ICP()),
  # 
  # # data from Anderson-Teixeira et al. (2015)
  # tar_target(anderson2015, Anderson_2015_crown_data()),
  # 
  # # data from Dalponte and Coomes (2016)
  # tar_target(dalponte2016, Dalponte_2016_crown_data()),
  # 
  # # French NFI data
  # tar_target(FrenchNFI_data, french_NFI_crown_data()),
  # 
  # # Quebec NFI data
  # tar_target(Quebec, quebec_NFI_crown_data()),
  # 
  # # Canada NFI data
  # tar_target(Canada_data, canada_NFI_crown_data()),
  # 
  # # data from Usoltsev database
  # tar_target(Usoltsev, Usoltsev_data()),
  # 
  # # data from Sullivan et al. (2018)
  # tar_target(Sullivan_data, read_Sullivan()),
  # 
  # # merge all data collected on tree crowns in a single file
  # tar_target(all_crown_a, merge_crown_data(baad_crown_complete, FHM_data, FIA_data, Spain_NFI, FUNDIV_data, FUNDIV_crown,
  #                                MONTANE_data, data_paper_crown, evan_crown_data, dettmann_data, heym_2017, gen_tree_data,
  #                                legacytree, ICP_data, anderson2015, dalponte2016, FrenchNFI_data, Quebec, Canada_data,
  #                                Usoltsev, Sullivan_data)),
  # 
  # # cleaning the database
  # tar_target(all_crown, remove_duplicated_ref(all_crown_a)),
  # tar_target(all_crown_clean, last_cleaning(all_crown)),
  # tar_target(all_crown_checked, checking_tree_data(all_crown_clean)),
  # tar_target(all_crown_checked_bis, cleaning_after_checking(all_crown_checked)),
  # 
  # 
  # 
  # ### 2. Checking for taxonomy and computing supplementary variables
  # 
  # tar_target(allometry_database, check_for_taxonomy_allometry(all_crown_checked_bis)),
  # tar_target(allometry_supp_variables, compute_supplementary_variables(allometry_database)),
  # tar_target(location_variables, extract_supplementary_variables(allometry_supp_variables)),
  # tar_target(location_variables_checked, checking_plot_data(location_variables)),
  # tar_target(allometry_complete_database, complete_allometry(allometry_supp_variables, location_variables_checked)),
  # 
  # 
  # ### 3. Summary of the allometry database for exploration and threshold definitions
  # 
  # tar_target(summary_dataset, summarizing_allometry_dataset(allometry_supp_variables)),
  # tar_target(summary_species, summarizing_allometry_species(allometry_supp_variables)),
  # 
  # 
  # ### 4. Comparing data availability: allometry database vs. NFI
  # 
  # tar_target(species_all_NFI, read.csv("data/species_NFI_all_climate_exo.csv")),
  # tar_target(NFI_data, check_for_taxonomy_NFI(species_all_NFI)),
  # tar_target(data_availability_comparison, binding_databases(NFI_data, summary_species))
  
  
  ### 5. Extracting data for all explored allometric relationships
  
  tar_target(global_species_list, get_species_list()),
  tar_target(data_allometry, get_data_allometry(global_species_list)),

  tar_target(height_data, get_data_height(data_allometry)),
  tar_target(height_species, get_species_height(height_data)),

  tar_target(diameter_data, get_data_diameter(data_allometry)),
  tar_target(diameter_species, get_species_diameter(diameter_data)),
  tar_target(diameter_species_comp, get_species_diameter_comp(diameter_data)),

  tar_target(depth_data, get_data_depth(data_allometry)),
  tar_target(depth_species, get_species_depth(depth_data)),
  tar_target(depth_species_comp, get_species_depth_comp(depth_data)),

  tar_target(heightdepth_data, get_data_heightdepth(data_allometry)),
  tar_target(heightdepth_species, get_species_heightdepth(heightdepth_data)),
  tar_target(heightdepth_species_comp, get_species_heightdepth_comp(heightdepth_data)))


  # ### 6. Fitting allometric relationships on all data and without competition
 ,# tar_target(height_alldata_nocomp_output, height_alldata_nocomp(height_data, height_species), pattern = map(height_species)),
  # tar_target(depth_alldata_nocomp_output, depth_alldata_nocomp(depth_data, depth_species), pattern = map(depth_species)),
  # tar_target(diameter_alldata_nocomp_output, diameter_alldata_nocomp(diameter_data, diameter_species), pattern = map(diameter_species)),
  # tar_target(heightdepth_alldata_nocomp_output, heightdepth_alldata_nocomp(heightdepth_data, heightdepth_species), pattern = map(heightdepth_species)),
  # 
  # 
  # ### 7. Fitting allometric relationships on resampled data and without competition
  # tar_target(height_resampling_nocomp_output, height_resampling_nocomp(height_data, height_species), pattern = map(height_species)),
  # tar_target(depth_resampling_nocomp_output, depth_resampling_nocomp(depth_data, depth_species), pattern = map(depth_species)),
  # tar_target(diameter_resampling_nocomp_output, diameter_resampling_nocomp(diameter_data, diameter_species), pattern = map(diameter_species)),
  # tar_target(heightdepth_resampling_nocomp_output, heightdepth_resampling_nocomp(heightdepth_data, heightdepth_species), pattern = map(heightdepth_species)),
  # 
  # 
  # ### 8. Fitting allometric relationships on resampled data and with competition
  # tar_target(depth_resampling_c1_output, depth_resampling_c1(depth_data, depth_species_comp), pattern = map(depth_species_comp)),
  # tar_target(diameter_resampling_c1_output, diameter_resampling_c1(diameter_data, diameter_species_comp), pattern = map(diameter_species_comp)),
  # tar_target(heightdepth_resampling_c1_output, heightdepth_resampling_c1(heightdepth_data, heightdepth_species_comp), pattern = map(heightdepth_species_comp)),
  # 
  # tar_target(depth_resampling_c2_output, depth_resampling_c2(depth_data, depth_species_comp), pattern = map(depth_species_comp)),
  # tar_target(diameter_resampling_c2_output, diameter_resampling_c2(diameter_data, diameter_species_comp), pattern = map(diameter_species_comp)),
  # tar_target(heightdepth_resampling_c2_output, heightdepth_resampling_c2(heightdepth_data, heightdepth_species_comp), pattern = map(heightdepth_species_comp)),
  # 
  ### 9. Fitting allometric relationships on resampled data and without competition - log log models
  tar_target(depth_resampling_nocomp_output_log, depth_resampling_nocomp_log(depth_data, depth_species), pattern = map(depth_species)),
  tar_target(diameter_resampling_nocomp_output_log, diameter_resampling_nocomp_log(diameter_data, diameter_species), pattern = map(diameter_species)),
  tar_target(heightdepth_resampling_nocomp_output_log, heightdepth_resampling_nocomp_log(heightdepth_data, heightdepth_species), pattern = map(heightdepth_species)),
  # 
  # ### 10. Fitting allometric relationships on resampled data and with competition - log log models
  # tar_target(depth_resampling_c1_output_log, depth_resampling_c1_log(depth_data, depth_species_comp), pattern = map(depth_species_comp)),
  # tar_target(diameter_resampling_c1_output_log, diameter_resampling_c1_log(diameter_data, diameter_species_comp), pattern = map(diameter_species_comp)),
  # tar_target(heightdepth_resampling_c1_output_log, heightdepth_resampling_c1_log(heightdepth_data, heightdepth_species_comp), pattern = map(heightdepth_species_comp)),
  # 
  # tar_target(depth_resampling_c2_output_log, depth_resampling_c2_log(depth_data, depth_species_comp), pattern = map(depth_species_comp)),
  # tar_target(diameter_resampling_c2_output_log, diameter_resampling_c2_log(diameter_data, diameter_species_comp), pattern = map(diameter_species_comp)),
  # tar_target(heightdepth_resampling_c2_output_log, heightdepth_resampling_c2_log(heightdepth_data, heightdepth_species_comp), pattern = map(heightdepth_species_comp))
  # 
  # )



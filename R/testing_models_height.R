get_species_list <- function () {
  
  ### Loading data
  allometry_complete_database <- data.table::fread("data/allometry_complete_database.csv", sep = ",")
  NFI_data = readRDS(file = "data/NFI_TNRS_check.rds")
  allometry_complete_database <- as.data.frame(allometry_complete_database) 
  # extracting species list from NFI data (191 species)
  sampling <- NFI_data %>% 
    filter(continent == "E_U" & nplot >= 100 & ntree >= 1000 | continent == "N_A" & nplot >= 150 & ntree >= 3000)
  
  # extracting species list in the allometry database (180 species)
  data <- allometry_complete_database
  data <- data %>% ungroup()
  species <- unique(data$checked_name)
  data_summary <- data %>% group_by(checked_name) %>% summarise(nplot_crown = length(unique(location_ID)),
                                                                ntree_crown = length(location_ID),
                                                                nobs_HT = sum(!is.na(HT_m)),
                                                                nobs_Cdiam = sum(!is.na(C_diam_m)),
                                                                nobs_Cdepth = sum(!is.na(C_depth_m)),
                                                                nobs_CR = sum(!is.na(CR))) %>% ungroup()
  # data_summary <- as.data.frame(matrix(nrow = length(species), ncol = 7))
  # colnames(data_summary) <- c("checked_name", "nplot_crown", "ntree_crown", "nobs_HT", "nobs_Cdiam", "nobs_Cdepth", "nobs_CR")
  # data_summary$checked_name <- species
  # 
  # for (i in 1:length(species)) {
  #   
  #   sample <- data[data$checked_name == species[i],]
  #   data_summary[i,2] <- length(unique(sample$location_ID))
  #   data_summary[i,3] <- dim(sample)[1]
  #   data_summary[i,4] <- dim(sample[!is.na(sample$HT_m),])[1]
  #   data_summary[i,5] <- dim(sample[!is.na(sample$C_diam_m),])[1]
  #   data_summary[i,6] <- dim(sample[!is.na(sample$C_depth_m),])[1]
  #   data_summary[i,7] <- dim(sample[!is.na(sample$CR),])[1]
  #   
  # } # Need to do that in dplyr super slow
  
  
  sampling <- left_join(sampling, data_summary, by = "checked_name")
  
  selected_sp <- sampling %>% 
    filter(continent == "E_U" & nplot_crown >= 100 & ntree_crown >= 1000 | continent == "N_A" & nplot_crown >= 150 & ntree_crown >= 3000)
  

  species_list <- unique(selected_sp$checked_name)
  species_list <- sort(species_list) # do not forget to order species list so that the rest of the code makes sense
  species_list <- species_list[-1]
  return(species_list)
  
}


height_models_nlme <- function (sp) {

  ### Loading data
  allometry_complete_database <- data.table::fread("data/allometry_complete_database.csv", sep = ",")
  NFI_data = readRDS(file = "data/NFI_TNRS_check.rds")
  allometry_complete_database <- as.data.frame(allometry_complete_database) 
  
  # extracting species list from NFI data (191 species)
  sampling <- NFI_data %>% 
  filter(continent == "E_U" & nplot >= 100 & ntree >= 1000 | continent == "N_A" & nplot >= 150 & ntree >= 3000)

  # extracting species list in the allometry database (180 species)
  data <- allometry_complete_database
  data <- data %>% ungroup()
  species <- unique(data$checked_name)
  data_summary <- data %>% group_by(checked_name) %>% summarise(nplot_crown = length(unique(location_ID)),
                                                                ntree_crown = length(location_ID),
                                                                nobs_HT = sum(!is.na(HT_m)),
                                                                nobs_Cdiam = sum(!is.na(C_diam_m)),
                                                                nobs_Cdepth = sum(!is.na(C_depth_m)),
                                                                nobs_CR = sum(!is.na(CR))) %>% ungroup()
  

  sampling <- left_join(sampling, data_summary, by = "checked_name")

  selected_sp <- sampling %>% 
    filter(continent == "E_U" & nplot_crown >= 100 & ntree_crown >= 1000 | continent == "N_A" & nplot_crown >= 150 & ntree_crown >= 3000)



  ## 1. Running models with 
  # random effect location_ID on a1 and b1
  # fixed protocol effect on a1 and b1
  data_ok <- allometry_complete_database
  data_ok <- data_ok[data_ok$checked_name %in% selected_sp$checked_name,]

  nrep = 500
  species_list <- unique(selected_sp$checked_name)
  species_list <- sort(species_list) # do not forget to order species list so that the rest of the code makes sense
  species_list <- species_list[-1]

  parameters_power_1 <- as.data.frame(matrix(nrow = length(species_list), ncol = length(unique(data_ok$data)) + 3))
  parameters_power_1[,1] <- species_list

  parameters_power_2 <- as.data.frame(matrix(nrow = (length(species_list) * nrep), ncol = length(unique(data_ok$data)) + 3))
  parameters_power_2[,1] <- sort(rep(species_list, nrep))


  parameters_asympt_1 <- as.data.frame(matrix(nrow = length(species_list), ncol = length(unique(data_ok$data)) + 4))
  parameters_asympt_1[,1] <- species_list

  parameters_asympt_2 <- as.data.frame(matrix(nrow = (length(species_list) * nrep), ncol = length(unique(data_ok$data)) + 4))
  parameters_asympt_2[,1] <- sort(rep(species_list, nrep))


  ## 2. Testing different models

  pdf(file = paste0("figures/alldata_heightmodels_1", sp, ".pdf"), width = 7, height = 5.5)

  
  i <- (1:length(species_list))[species_list == sp]
  
    par(mfrow = c(1,1))
  
    # compiling selected data and variables
    df <- data_ok[data_ok$checked_name %in% species_list[i],]
    df <- df %>% filter(!is.na(DBH_cm) & !is.na(HT_m))
  
    data <- as.data.frame(cbind(df$DBH_cm, df$HT_m, df$location_ID, df$data))
    colnames(data) <- c("x", "y", "location", "protocol")
    data <- data %>% mutate(x = as.numeric(x), y = as.numeric(y), protocol = as.factor(protocol))
    sel_loc <- names(table(data$location))[table(data$location) > 2]
    data <- data[data$location %in% sel_loc, ]
    data$location <- factor(data$location)  

    if (dim(data)[1] >= 1000) { # running the models only if more than 1000 observations are left in the sampled data

    print(i)
  
    ### STEP 1: FITTING MODELS ON ALL DATA  
    # tested model
    mod_power <- y ~ a1 * (x ^ a2)
    mod_asympt <- y ~ 1.3 + b1 * (1-exp(-b2 * x)) ^ b3
  

    tryCatch({  
    
      # plotting data
      plot(data$x, data$y, xlab = "diameter at breast height (cm)", ylab = "tree height (m)", main = species_list[i], las = 1, pch = 16, cex = 0.5, col = densCols(data$x, data$y))
      fun.boxplot.breaks(data$x, data$y)
    
      dbh <- 10:max(data$x)
    

      # fitting power relationships
      init <- fixef(lmer(log(y) ~ log(x) + (1|location), data)) # initializing values for power models
      
   
      m2 <- nlme(mod_power,
                 data = data,
                 fixed = list(a1 ~ 1, a2 ~ 1),
                 random = a1 ~ 1|location,
                 start = c(a1 = exp(init[1]), a2 = init[2]),
                 weights = varPower(form = ~fitted(.)),
                 method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      m3 <- nlme(mod_power,
                 data = data,
                 fixed = list(a1 ~ protocol, a2 ~ 1),
                 random = a1 ~ 1|location,
                 start = c(a1 = c(rep(exp(init[1]), length(unique(data$protocol)))), a2 = init[2]),
                 weights = varPower(form = ~fitted(.)),
                 method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      
      lines(dbh, fixed.effects(m3)[1]*dbh^(fixed.effects(m3)[length(unique(data$protocol))+1]), type = "l", col = "firebrick4", lwd = 1) # predict of power model with protocol effect
      
      parameters_power_1[i,2] <- fixed.effects(m3)[1]
      parameters_power_1[i,22] <- fixed.effects(m3)[length(unique(data$protocol))+1]
      parameters_power_1[i,23] <- AIC(m3)
      
      
      for (k in 2:length(unique(data$protocol))) {
        lines(dbh, (fixed.effects(m3)[1]+fixed.effects(m3)[k]) * dbh^(fixed.effects(m3)[length(unique(data$protocol))+1]), type = "l", col = "firebrick4", lwd = 1)
        parameters_power_1[i,k+1] <- fixed.effects(m3)[1]+fixed.effects(m3)[k]
      }
      
      lines(dbh, fixef(m2)["a1"]*dbh^fixef(m2)["a2"], type = "l", col ="forestgreen", lwd = 3.5) # predict of power model without protocol effect
      
      },
  
    error = function(e) {
        
        print(paste("error power model", species_list[i], sep = " "))
        
      }) 
  
  
    tryCatch({  
    
      # plotting data
      plot(data$x, data$y, xlab = "diameter at breast height (cm)", ylab = "tree height (m)", main = species_list[i], las = 1, pch = 16, cex = 0.5, col = densCols(data$x, data$y))
      fun.boxplot.breaks(data$x, data$y)
    
    
      # fitting asymptotic relationships  
      vars <- data.frame(var = c("b1", "b2", "b3"), start = c(quantile(data$y, probs = 0.97)*0.8, 0.07, 0.9))
    
      m5 <- nls(mod_asympt,
              data = data,
              start = setNames(as.list(vars$start), vars$var),
              lower = c(0.0001, 0.0001, 0.0001), algorithm = "port",
              nls.control(maxiter = 800))
    
      m6 <- nlme(mod_asympt,
               data = data,
               fixed = b1 + b2 + b3 ~ 1,
               random = b1 ~ 1|location,
               start = c(b1 = coefficients(m5)["b1"], b2 = coefficients(m5)["b2"], b3 = coefficients(m5)["b3"]),
               method = "ML",
               control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
    
      m7 <- nlme(mod_asympt,
               data = data,
               fixed = list(b1 ~ 1, b2 ~ 1, b3 ~ 1),
               random = b1 ~ 1|location,
               start = c(b1 = fixef(m6)["b1"], b2 = fixef(m6)["b2"], b3 = fixef(m6)["b3"]),
               method = "ML", 
               weights = varPower(form = ~fitted(.)),
               control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
    
      m8 <- nlme(mod_asympt,
               data = data,
               fixed = list(b1 ~ protocol, b2 ~ 1, b3 ~ 1),
               random = b1 ~ 1|location,
               start = c(b1 = c(rep(fixef(m7)["b1"], length(unique(data$protocol))), b2 = fixef(m7)["b2"], b3 = fixef(m7)["b3"])),
               method = "ML", 
               weights = varPower(form = ~fitted(.)),
               control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
    
      lines(dbh, 1.3+ fixed.effects(m8)[1]*(1-exp(-(fixed.effects(m8)[length(unique(data$protocol))+1])*dbh))^(fixed.effects(m8)[length(unique(data$protocol))+2]), type = "l", col ="firebrick4", lwd = 1) # predict of asymptot model with protocol effect
      
      parameters_asympt_1[i,2] <- fixed.effects(m8)[1]
      parameters_asympt_1[i,22] <- fixed.effects(m8)[length(unique(data$protocol))+1]
      parameters_asympt_1[i,23] <- fixed.effects(m8)[length(unique(data$protocol))+2]
      parameters_asympt_1[i,24] <- AIC(m8)
      
      for (k in 2:length(unique(data$protocol))) {
        lines(dbh, 1.3+ (fixed.effects(m8)[1]+fixed.effects(m8)[k])*(1-exp(-(fixed.effects(m8)[length(unique(data$protocol))+1])*dbh))^(fixed.effects(m8)[length(unique(data$protocol))+2]), type = "l", col ="firebrick4", lwd = 1)
        parameters_asympt_1[i,k+1] <- fixed.effects(m8)[1]+fixed.effects(m8)[k]
      }
      
      lines(dbh, 1.3+ fixef(m7)["b1"]*(1-exp(-fixef(m7)["b2"]*dbh))^fixef(m7)["b3"], type = "l", col ="forestgreen", lwd = 3.5) # predict of asymptot model without protocol effect
      
      
    },
    
    error = function(e) {
    
      print(paste("error asymptotic model", species_list[i], sep = " "))
    
      }) 
  
  
    ### STEP 2: FITTING MODELS ON SUBSAMPLES  
    # computing nb of datasets in which the species was surveyed
    nb_datasets_all <- length(unique(data$protocol))
  
    # creating classes of dbh
    range_dbh <- max(data$x) - 10
    class_dbh <- range_dbh/4
    
    size_a <- floor(dim(data[data$x < (class_dbh + 10),])[1] * 0.9)
    size_b <- floor(dim(data[data$x >= (class_dbh + 10) & data$x < (class_dbh * 2 + 10),])[1] * 0.9)
    size_c <- floor(dim(data[data$x >= (class_dbh * 2 + 10) & data$x < (class_dbh * 3 + 10),])[1] * 0.9)
    size_d <- floor(dim(data[data$x >= (class_dbh * 3 + 10),])[1] * 0.9)
    
    size <- min(size_a, size_b, size_c, size_d)
    
    if (size >=  50) { 
      sample_size <- size
    } else {
      data <- data[data$x <= quantile(data$x, 0.99),] # removing extreme values of dbh if no data in the last dbh class defined
      range_dbh <- max(data$x) - 10
      class_dbh <- range_dbh/4
      
      size_a <- floor(dim(data[data$x < (class_dbh + 10),])[1] * 0.9)
      size_b <- floor(dim(data[data$x >= (class_dbh + 10) & data$x < (class_dbh * 2 + 10),])[1] * 0.9)
      size_c <- floor(dim(data[data$x >= (class_dbh * 2 + 10) & data$x < (class_dbh * 3 + 10),])[1] * 0.9)
      size_d <- floor(dim(data[data$x >= (class_dbh * 3 + 10),])[1] * 0.9)
      
      sample_size <- min(size_a, size_b, size_c, size_d)
    }
    
    
    j <- 1
    while(j < nrep) {
    
    # tested models
    mod_power <- y ~ a1 * (x ^ a2)
    mod_asympt <- y ~ 1.3 + b1 * (1-exp(-b2 * x)) ^ b3
    
    class_1 <- data[data$x < (class_dbh + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
    class_2 <- data[data$x >= (class_dbh + 10) & data$x < (class_dbh * 2 + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
    class_3 <- data[data$x >= (class_dbh * 2 + 10) & data$x < (class_dbh * 3 + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
    class_4 <- data[data$x >= (class_dbh * 3 + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
    
    new_data <- bind_rows(class_1, class_2, class_3, class_4)
    nb_datasets_sample <- length(unique(new_data$protocol))
    
    while (nb_datasets_sample < (floor(nb_datasets_all * 0.33))) {
      
      class_1 <- data[data$x < (class_dbh + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
      class_2 <- data[data$x >= (class_dbh + 10) & data$x < (class_dbh * 2 + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
      class_3 <- data[data$x >= (class_dbh * 2 + 10) & data$x < (class_dbh * 3 + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
      class_4 <- data[data$x >= (class_dbh * 3 + 10),] %>% sample_n(sample_size, replace = FALSE, prob = NULL)
      
      new_data <- bind_rows(class_1, class_2, class_3, class_4)
      
      nb_datasets_sample <- length(unique(new_data$protocol)) }
    
    
    tryCatch({  
      
      
      # fitting power relationships
      init_s <- fixef(lmer(log(y) ~ log(x) + (1|location), new_data)) # initializing values for power models
      
   
      m2_s <- nlme(mod_power,
                   data = new_data,
                   fixed = list(a1 ~ 1, a2 ~ 1),
                   random = a1 ~ 1|location,
                   start = c(a1 = exp(init_s[1]), a2 = init_s[2]),
                   weights = varPower(form = ~fitted(.)),
                   method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      m3_s <- nlme(mod_power,
                   data = data,
                   fixed = list(a1 ~ protocol, a2 ~ 1),
                   random = a1 ~ 1|location,
                   start = c(a1 = c(rep(exp(init_s[1]), length(unique(new_data$protocol)))), a2 = init_s[2]),
                   weights = varPower(form = ~fitted(.)),
                   method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      parameters_power_2[(((nrep * i) - nrep) + j),2] <- fixed.effects(m3_s)[1]
      parameters_power_2[(((nrep * i) - nrep) + j),22] <- fixed.effects(m3_s)[length(unique(new_data$protocol))+1]
      parameters_power_2[(((nrep * i) - nrep) + j),23] <- AIC(m3_s)
      
      
      for (k in 2:length(unique(new_data$protocol))) {
        parameters_power_2[(((nrep * i) - nrep) + j),k+1] <- fixed.effects(m3_s)[1]+fixed.effects(m3_s)[k]
      }
      
      # fitting asymptotic relationships  
      vars_s <- data.frame(var = c("b1", "b2", "b3"), start = c(quantile(new_data$y, probs = 0.97)*0.8, 0.07, 0.9))
      
      m5_s <- nls(mod_asympt,
                  data = new_data,
                  start = setNames(as.list(vars_s$start), vars_s$var),
                  lower = c(0.0001, 0.0001, 0.0001), algorithm = "port",
                  nls.control(maxiter = 800))
      
      m6_s <- nlme(mod_asympt,
                   data = new_data,
                   fixed = b1 + b2 + b3 ~ 1,
                   random = b1 ~ 1|location,
                   start = c(b1 = coefficients(m5_s)["b1"], b2 = coefficients(m5_s)["b2"], b3 = coefficients(m5_s)["b3"]),
                   method = "ML",
                   control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      m7_s <- nlme(mod_asympt,
                   data = new_data,
                   fixed = list(b1 ~ 1, b2 ~ 1, b3 ~ 1),
                   random = b1 ~ 1|location,
                   start = c(b1 = fixef(m6_s)["b1"], b2 = fixef(m6_s)["b2"], b3 = fixef(m6_s)["b3"]),
                   method = "ML", 
                   weights = varPower(form = ~fitted(.)),
                   control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      m8_s <- nlme(mod_asympt,
                   data = new_data,
                   fixed = list(b1 ~ protocol, b2 ~ 1, b3 ~ 1),
                   random = b1 ~ 1|location,
                   start = c(b1 = c(rep(fixef(m7_s)["b1"], length(unique(new_data$protocol))), b2 = fixef(m7_s)["b2"], b3 = fixef(m7_s)["b3"])),
                   method = "ML", 
                   weights = varPower(form = ~fitted(.)),
                   control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      
      parameters_asympt_2[(((nrep * i) - nrep) + j),2] <- fixed.effects(m8_s)[1]
      parameters_asympt_2[(((nrep * i) - nrep) + j),22] <- fixed.effects(m8_s)[length(unique(new_data$protocol))+1]
      parameters_asympt_2[(((nrep * i) - nrep) + j),23] <- fixed.effects(m8_s)[length(unique(new_data$protocol))+2]
      parameters_asympt_2[(((nrep * i) - nrep) + j),24] <- AIC(m8_s)
      
      for (k in 2:length(unique(new_data$protocol))) {
        parameters_asympt_2[(((nrep * i) - nrep) + j),k+1] <- fixed.effects(m8_s)[1] + fixed.effects(m8_s)[k]
      }
      
    },
    
    error = function(e) {
      
      print(paste("error asymptotic model", species_list[i], " sampling", j, sep = " "))
      
    }) 
    
    j <- j + 1
    
        }
      }
    

  dev.off()  
  
  write.csv(parameters_power_1, file = paste0("output/height_power_alldata__nlme.",sp, ".csv") )
  write.csv(parameters_power_2, file =  paste0("output/height_power_resampling__nlme.",sp, ".csv"))
  write.csv(parameters_asympt_1, file =  paste0("output/height_asympt_alldata__nlme.",sp, ".csv"))
  write.csv( parameters_asympt_2, file =  paste0("output/height_asympt_resampling__nlme.",sp, ".csv"))

  return(list(parameters_power_1, parameters_power_2, parameters_asympt_1, parameters_asympt_2))
  
}
  
  

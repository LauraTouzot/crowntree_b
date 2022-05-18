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

  nrep = 200
  species_list <- unique(selected_sp$checked_name)
  species_list <- sort(species_list) # do not forget to order species list so that the rest of the code makes sense
  species_list <- species_list[-1]

  parameters_power_1 <- as.data.frame(matrix(nrow = 1, ncol = length(unique(data_ok$data)) + 4))
  parameters_power_1[,1] <- sp

  parameters_power_2 <- as.data.frame(matrix(nrow = nrep, ncol = length(unique(data_ok$data)) + 4))
  parameters_power_2[,1] <- rep(sp, nrep)


  parameters_asympt_1 <- as.data.frame(matrix(nrow = 1, ncol = length(unique(data_ok$data)) + 5))
  parameters_asympt_1[,1] <- sp

  parameters_asympt_2 <- as.data.frame(matrix(nrow = nrep, ncol = length(unique(data_ok$data)) + 5))
  parameters_asympt_2[,1] <- rep(sp, nrep)

  names(parameters_power_1) <- names(parameters_power_2) <-
    c("species", "inter",paste0("protocol", unique(data_ok$data)), "slope", "AIC")
  
  names(parameters_asympt_1) <- names(parameters_asympt_2) <-
    c("species", "b1",paste0("protocol", unique(data_ok$data)), "b2", "b3", "AIC")
  
  ## 2. Testing different models

  pdf(file = paste0("figures/alldata_heightmodels_1", sp, ".pdf"), width = 7, height = 5.5)

  
  i <- (1:length(species_list))[species_list == sp]
  
    par(mfrow = c(1,1))
  
    # compiling selected data and variables
    df <- data_ok[data_ok$checked_name %in% species_list[i],]
    df <- df %>% filter(!is.na(DBH_cm) & !is.na(HT_m))
  
    data <- data.frame(df$DBH_cm, df$HT_m, df$location_ID, df$data)
    names(data) <- c("x", "y", "location", "protocol")
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
      
      if(length(data$protocol)>1){
        
      m3 <- nlme(mod_power,
                 data = data,
                 fixed = list(a1 ~ protocol, a2 ~ 1),
                 random = a1 ~ 1|location,
                 start = c(a1 = c(rep(exp(init[1]), length(unique(data$protocol)))), a2 = init[2]),
                 weights = varPower(form = ~fitted(.)),
                 method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      
      lines(dbh, fixed.effects(m3)[1]*dbh^(fixed.effects(m3)[length(unique(data$protocol))+1]), type = "l", col = "firebrick4", lwd = 1) # predict of power model with protocol effect
      
      parameters_power_1[1,"inter"] <- fixed.effects(m3)[1]
      parameters_power_1[1,"slope"] <- fixed.effects(m3)["a2"]
      parameters_power_1[1,"AIC"] <- AIC(m3)
   
      
      for (k in paste0("protocol", levels(data$protocol)[-1])) {
        lines(dbh, (fixed.effects(m3)[1]+fixed.effects(m3)[paste0("a1.", k)]) * dbh^(fixed.effects(m3)["a2"]), type = "l", col = "firebrick4", lwd = 1)
        parameters_power_1[1,k] <- fixed.effects(m3)[1]+fixed.effects(m3)[paste0("a1.", k)]
      }
      parameters_power_1[1,paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m3)[1]
      
      }else{
        parameters_power_1[1,"inter"] <- fixed.effects(m2)[1]
        parameters_power_1[1,"slope"] <- fixed.effects(m2)["a2"]
        parameters_power_1[1,"AIC"] <- AIC(m2)
        
        
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
  
          if(length(data$protocol)>1){
        
      m8 <- nlme(mod_asympt,
               data = data,
               fixed = list(b1 ~ protocol, b2 ~ 1, b3 ~ 1),
               random = b1 ~ 1|location,
               start = c(b1 = c(rep(fixef(m7)["b1"], length(unique(data$protocol))), b2 = fixef(m7)["b2"], b3 = fixef(m7)["b3"])),
               method = "ML", 
               weights = varPower(form = ~fitted(.)),
               control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
    
      lines(dbh, 1.3+ fixed.effects(m8)[1]*(1-exp(-(fixed.effects(m8)[length(unique(data$protocol))+1])*dbh))^(fixed.effects(m8)[length(unique(data$protocol))+2]), type = "l", col ="firebrick4", lwd = 1) # predict of asymptot model with protocol effect
      
      parameters_asympt_1[1,"b1"] <- fixed.effects(m8)[1]
      parameters_asympt_1[1,"b2"] <- fixed.effects(m8)["b2"]
      parameters_asympt_1[1,"b3"] <- fixed.effects(m8)["b3"]
      parameters_asympt_1[1,"AIC"] <- AIC(m8)
      

      for (k in paste0("protocol", levels(data$protocol)[-1])) {
        lines(dbh, 1.3+ (fixed.effects(m8)["b1"]+fixed.effects(m8)[paste0("b1.", k)])*(1-exp(-(fixed.effects(m8)["b2"])*dbh))^(fixed.effects(m8)["b3"]), type = "l", col ="firebrick4", lwd = 1)
        parameters_asympt_1[1,k] <- fixed.effects(m8)[1]+fixed.effects(m8)[paste0("b1.", k)]
      }
      parameters_asympt_1[1,paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m8)[1]
          }else{
            
            parameters_asympt_1[1,"b1"] <- fixed.effects(m7)[1]
            parameters_asympt_1[1,"b2"] <- fixed.effects(m7)["b2"]
            parameters_asympt_1[1,"b3"] <- fixed.effects(m7)["b3"]
            parameters_asympt_1[1,"AIC"] <- AIC(m7)
            
          }
      lines(dbh, 1.3+ fixef(m7)["b1"]*(1-exp(-fixef(m7)["b2"]*dbh))^fixef(m7)["b3"], type = "l", col ="forestgreen", lwd = 3.5) # predict of asymptot model without protocol effect
      
      
    },
    
    error = function(e) {
    
      print(paste("error asymptotic model", species_list[i], sep = " "))
    
      }) 
  
  
    ### STEP 2: FITTING MODELS ON SUBSAMPLES  
  
    # creating classes of dbh
    range_dbh <- max(data$x) - 10
    class_dbh <- range_dbh/4
    s1 <- class_dbh + 10
    s2 <- class_dbh * 2 + 10
    s3 <- class_dbh * 3 + 10
    d1 <- data[data$x < s1,]
    d1 <- d1[d1$location %in% names(table(d1$location))[table(d1$location)>2], ]
    d2 <- data[data$x >= s1 & data$x < s2,]
    d2 <- d2[d2$location %in% names(table(d2$location))[table(d2$location)>2], ]
    d3 <- data[data$x >= s2 & data$x < s3,]
    d3 <- d3[d3$location %in% names(table(d3$location))[table(d3$location)>2], ]
    d4 <- data[data$x >= s3,]
    d4 <- d4[d4$location %in% names(table(d4$location))[table(d4$location)>2], ]
    
    size_a <- floor(length(unique(d1$location))* 0.9)*3
    size_b <- floor(length(unique(d2$location))* 0.9)*3
    size_c <- floor(length(unique(d3$location))* 0.9)*3
    size_d <- floor(length(unique(d4$location))* 0.9)*3
    
    size <- min(size_a, size_b, size_c, size_d)
    
    if (size >=  50) { 
      sample_size <- size
    } else {
      data <- data[data$x <= quantile(data$x, 0.99),] # removing extreme values of dbh if no data in the last dbh class defined
      range_dbh <- max(data$x) - 10
      class_dbh <- range_dbh/4
      
      s1 <- class_dbh + 10
      s2 <- class_dbh * 2 + 10
      s3 <- class_dbh * 3 + 10
      d1 <- data[data$x < s1,]
      d1 <- d1[d1$location %in% names(table(d1$location))[table(d1$location)>2], ]
      d2 <- data[data$x >= s1 & data$x < s2,]
      d2 <- d2[d2$location %in% names(table(d2$location))[table(d2$location)>2], ]
      d3 <- data[data$x >= s2 & data$x < s3,]
      d3 <- d3[d3$location %in% names(table(d3$location))[table(d3$location)>2], ]
      d4 <- data[data$x >= s3,]
      d4 <- d4[d4$location %in% names(table(d4$location))[table(d4$location)>2], ]
      
      size_a <- floor(length(unique(d1$location))* 0.9)*3
      size_b <- floor(length(unique(d2$location))* 0.9)*3
      size_c <- floor(length(unique(d3$location))* 0.9)*3
      size_d <- floor(length(unique(d4$location))* 0.9)*3
      
      sample_size <- min(size_a, size_b, size_c, size_d)
    }
    
    # computing nb of datasets in which the species was surveyed
    nb_datasets_all <- length(unique(c(d1$protocol, d2$protocol, d3$protocol,d4$protocol)))
    
    
    for(j in 1:nrep) {
    
    # tested models
    mod_power <- y ~ a1 * (x ^ a2)
    mod_asympt <- y ~ 1.3 + b1 * (1-exp(-b2 * x)) ^ b3
    
    loc1 <- sample(unique(d1$location), floor(sample_size/3))
    class_1 <- d1 %>% filter(location %in% loc1) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
    loc2 <- sample(unique(d2$location), floor(sample_size/3))
    class_2 <- d2 %>% filter(location %in% loc2) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
    loc3 <- sample(unique(d3$location), floor(sample_size/3))
    class_3 <- d3 %>% filter(location %in% loc3) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
    loc4 <- sample(unique(d4$location), floor(sample_size/3))
    class_4 <- d4 %>% filter(location %in% loc4) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
    
    new_data <- bind_rows(class_1, class_2, class_3, class_4)
    nb_datasets_sample <- length(unique(new_data$protocol))
    
    while (nb_datasets_sample < (floor(nb_datasets_all * 0.33))) {
      
      loc1 <- sample(unique(d1$location), floor(sample_size/3))
      class_1 <- d1 %>% filter(location %in% loc1) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
      loc2 <- sample(unique(d2$location), floor(sample_size/3))
      class_2 <- d2 %>% filter(location %in% loc2) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
      loc3 <- sample(unique(d3$location), floor(sample_size/3))
      class_3 <- d3 %>% filter(location %in% loc3) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
      loc4 <- sample(unique(d4$location), floor(sample_size/3))
      class_4 <- d4 %>% filter(location %in% loc4) %>% group_by(location) %>% slice_sample(n = 3) %>% ungroup()
      
      new_data <- bind_rows(class_1, class_2, class_3, class_4)
      
      nb_datasets_sample <- length(unique(new_data$protocol)) }
    
    
    tryCatch({  
      
      
      # fitting power relationships
      init_s <- fixef(lmer(log(y) ~ log(x) + (1|location), new_data)) # initializing values for power models
      
   
      m2_sb <- nlme(mod_power,
                   data = new_data,
                   fixed = list(a1 ~ 1, a2 ~ 1),
                   random = a1 ~ 1|location,
                   start = c(a1 = exp(init_s[1]), a2 = init_s[2]),
                   method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))

      m2_s <- nlme(mod_power,
                    data = new_data,
                    fixed = list(a1 ~ 1, a2 ~ 1),
                    random = a1 ~ 1|location,
                    start = c(a1 = fixed.effects(m2_sb)["a1"], a2 = fixed.effects(m2_sb)["a2"]),
                    weights = varPower(form = ~fitted(.)),
                    method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      if(length(new_data$protocol)>1){
        
      m3_s <- nlme(mod_power,
                   data = new_data,
                   fixed = list(a1 ~ protocol, a2 ~ 1),
                   random = a1 ~ 1|location,
                   start = c(a1 = c(rep(fixed.effects(m2_s)["a1"], length(unique(new_data$protocol)))), a2 = fixed.effects(m2_s)["a2"]),
                   weights = varPower(form = ~fitted(.)),
                   method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
      
      parameters_power_2[ j,"a1"] <- fixed.effects(m3_s)[1]
      parameters_power_2[ j,"a2"] <- fixed.effects(m3_s)["a2"]
      parameters_power_2[j,"AIC"] <- AIC(m3_s)
      
      
      
      
      for (k in paste0("protocol", levels(data$protocol)[-1])) {
         parameters_power_2[j,k] <- fixed.effects(m3_s)[1]+fixed.effects(m3_s)[paste0("a1.", k)]
      }
      parameters_power_2[j,paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m3_s)[1]
      }else{
        parameters_power_2[ j,"a1"] <- fixed.effects(m2_s)[1]
        parameters_power_2[ j,"a2"] <- fixed.effects(m2_s)["a2"]
        parameters_power_2[j,"AIC"] <- AIC(m2_s)
        
        
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
      
      if(length(new_data$protocol)>1){
        
      m8_s <- nlme(mod_asympt,
                   data = new_data,
                   fixed = list(b1 ~ protocol, b2 ~ 1, b3 ~ 1),
                   random = b1 ~ 1|location,
                   start = c(b1 = c(rep(fixef(m7_s)["b1"], length(unique(new_data$protocol))), b2 = fixef(m7_s)["b2"], b3 = fixef(m7_s)["b3"])),
                   method = "ML", 
                   weights = varPower(form = ~fitted(.)),
                   control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
     
      
      parameters_asympt_2[j,"b1"] <- fixed.effects(m8_s)[1]
      parameters_asympt_2[j,"b2"] <- fixed.effects(m8_s)["b2"]
      parameters_asympt_2[ j,"b3"] <- fixed.effects(m8_s)["b3"]
      parameters_asympt_2[ j,"AIC"] <- AIC(m8_s)
      

      for (k in paste0("protocol", levels(data$protocol)[-1])) {
        parameters_asympt_2[j,k] <- fixed.effects(m8_s)[1] + fixed.effects(m8_s)[paste0("b1.", k)]
      }
      parameters_asympt_2[j,paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m8_s)[1]
      }else{
        parameters_asympt_2[j,"b1"] <- fixed.effects(m7_s)[1]
        parameters_asympt_2[j,"b2"] <- fixed.effects(m7_s)["b2"]
        parameters_asympt_2[ j,"b3"] <- fixed.effects(m7_s)["b3"]
        parameters_asympt_2[ j,"AIC"] <- AIC(m7_s)
        
        
      }
    },
    
    error = function(e) {
      
      print(paste("error asymptotic model", species_list[i], " sampling", j, sep = " "))
      
    }) 
    
    
        }
      
    }
    

  dev.off()  
  
  write.csv(parameters_power_1, file = paste0("output/height_power_alldata__nlme.",sp, ".csv") )
  write.csv(parameters_power_2, file =  paste0("output/height_power_resampling__nlme.",sp, ".csv"))
  write.csv(parameters_asympt_1, file =  paste0("output/height_asympt_alldata__nlme.",sp, ".csv"))
  write.csv( parameters_asympt_2, file =  paste0("output/height_asympt_resampling__nlme.",sp, ".csv"))

  return(list(parameters_power_1, parameters_power_2, parameters_asympt_1, parameters_asympt_2))
  
}
  
  

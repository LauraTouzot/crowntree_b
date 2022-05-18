depth_models_nlme <- function (sp) {
  
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


  parameters_linear_1 <- as.data.frame(matrix(nrow = length(species_list), ncol = length(unique(data_ok$data)) + 3))
  parameters_linear_1[,1] <- species_list

  parameters_power_1 <- as.data.frame(matrix(nrow = length(species_list), ncol = length(unique(data_ok$data)) + 3))
  parameters_power_1[,1] <- species_list


  parameters_linear_2 <- as.data.frame(matrix(nrow = (length(species_list) * nrep), ncol = length(unique(data_ok$data)) + 3))
  parameters_linear_2[,1] <- species_list

  parameters_power_2 <- as.data.frame(matrix(nrow = (length(species_list) * nrep), ncol = length(unique(data_ok$data)) + 3))
  parameters_power_2[,1] <- species_list


  ## 2. Testing different models
  pdf(file = paste0("figures/alldata_depthmodels_1.", gsub(" ", "_", sp), ".pdf"), width = 7, height = 5.5)
  
  
  i <- (1:length(species_list))[species_list == sp]
  

  
  par(mfrow = c(1,1))
  
  # compiling selected data and variables
  df <- data_ok[data_ok$checked_name %in% species_list[i],]
  df <- df %>% filter(!is.na(DBH_cm) & !is.na(C_depth_m))
  
  data <- as.data.frame(cbind(df$DBH_cm, df$C_depth_m, df$location_ID, df$data))
  colnames(data) <- c("x", "y", "location", "protocol")
  data <- data %>% mutate(x = as.numeric(x), y = as.numeric(y), protocol = as.factor(protocol))
  sel_loc <- names(table(data$location))[table(data$location) > 2]
  data <- data[data$location %in% sel_loc, ]
  data$location <- factor(data$location)  
  
  if (dim(data)[1] < 1000) { # running the models only if more than 1000 observations are left in the sampled data
    next 
  } else
    {    
    
    print(i)
    
    ### STEP 1: FITTING MODELS ON ALL DATA  
    # tested models
    mod_power <- y ~ a1 * (x ^ a2)
    
    tryCatch({  
      
      # plotting data
      plot(data$x, data$y, xlab = "diameter at breast height (cm)", ylab = "crown depth (m)", main = species_list[i], las = 1, pch = 16, cex = 0.5, col = densCols(data$x, data$y))
      fun.boxplot.breaks(data$x, data$y)
      
      dbh <- 10:max(data$x)
      
      # fitting linear relationships
      m1_l <- lme(y ~ x,
                  data = data,
                  random = ~ 1|location,
                  weights = varPower(form = ~fitted(.)),
                  method = "ML",  control = lmeControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))
      
      m2_l <- lme(y ~ x + protocol,
                  data = data,
                  random = ~ 1|location,
                  weights = varPower(form = ~fitted(.)),
                  method = "ML",  control = lmeControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))
      
      lines(dbh, fixed.effects(m2_l)[1] + fixed.effects(m2_l)[2] * dbh, type = "l", col = "firebrick4", lwd = 1)
      
      parameters_linear_1[i,2] <- fixed.effects(m2_l)[1]
      parameters_linear_1[i,22] <- fixed.effects(m2_l)[2]
      parameters_linear_1[i,23] <- AIC(m2_l)
      
      
      for (k in 3:(length(unique(data$protocol)) + 1)) {
        lines(dbh, (fixed.effects(m2_l)[1] + fixed.effects(m2_l)[k]) + fixed.effects(m2_l)[2] * dbh, type = "l", col = "firebrick4", lwd = 1)
        parameters_linear_1[i,k] <- fixed.effects(m2_l)[1]+fixed.effects(m2_l)[k]
      }
      
      lines(dbh, fixef(m1_l)[1] +  fixef(m1_l)[2]*dbh, type = "l", col ="forestgreen", lwd = 3.5) # predict of power model without protocol effect
      
      
    },
    
    error = function(e) {
      
      print(paste("error linear model", species_list[i], sep = " "))
      
    }) 
    
    
    tryCatch({  
      
      # plotting data
      plot(data$x, data$y, xlab = "diameter at breast height (cm)", ylab = "crown depth (m)", main = species_list[i], las = 1, pch = 16, cex = 0.5, col = densCols(data$x, data$y))
      fun.boxplot.breaks(data$x, data$y)
      
      # fitting power relationships
      init <- fixef(lmer(log(y) ~ log(x) + (1|location), data)) # initializing values for power models
      
      # m1 <- nlme(mod_power,
      #            data = data,
      #            fixed = list(a1 ~ 1, a2 ~ 1),
      #            random = a1 ~ 1|location,
      #            start = c(a1 = exp(init[1]), a2 = init[2]),
      #            method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
      
      m2 <- nlme(mod_power,
                 data = data,
                 fixed = list(a1 ~ 1, a2 ~ 1),
                 random = a1 ~ 1|location,
                 start = c(a1 = exp(init[1]), a2 = init[2]),
                 weights = varPower(form = ~fitted(.)),
                 method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
      
      m3 <- nlme(mod_power,
                 data = data,
                 fixed = list(a1 ~ protocol, a2 ~ 1),
                 random = a1 ~ 1|location,
                 start = c(a1 = c(rep(exp(init[1]), length(unique(data$protocol)))), a2 = init[2]),
                 weights = varPower(form = ~fitted(.)),
                 method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
      
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
    while (j < nrep) {
      
      # tested models
      mod_power <- y ~ a1 * (x ^ a2)
      
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
        
        # fitting linear relationshios
        
        m1_ls <- lme(y ~ x,
                     data = new_data,
                     random = ~ 1|location,
                     weights = varPower(form = ~fitted(.)),
                     method = "ML", control = lmeControl(maxIter = 1500, tolerance = 1e-3, msTol = 1e-2))
        
        m2_ls <- lme(y ~ x + protocol,
                     data = new_data,
                     random = ~ 1|location,
                     weights = varPower(form = ~fitted(.)),
                     method = "ML", control = lmeControl(maxIter = 1500, tolerance = 1e-3, msTol = 1e-2))
        
        parameters_linear_2[(((nrep * i) - nrep) + j),2] <- fixed.effects(m2_ls)[1]
        parameters_linear_2[(((nrep * i) - nrep) + j),22] <- fixed.effects(m2_ls)[2]
        parameters_linear_2[(((nrep * i) - nrep) + j),23] <- AIC(m2_ls)
        
        
        for (k in 3:length(unique(new_data$protocol))) {
          parameters_linear_2[(((nrep * i) - nrep) + j),k] <- fixed.effects(m2_ls)[1] + fixed.effects(m2_ls)[k]
        }
        
        
        # fitting power relationships
        init_s <- fixef(lmer(log(y) ~ log(x) + (1|location), new_data)) # initializing values for power models
        
        # m1 <- nlme(mod_power,
        #            data = data,
        #            fixed = list(a1 ~ 1, a2 ~ 1),
        #            random = a1 ~ 1|location,
        #            start = c(a1 = exp(init[1]), a2 = init[2]),
        #            method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
        
        m2_s <- nlme(mod_power,
                     data = new_data,
                     fixed = list(a1 ~ 1, a2 ~ 1),
                     random = a1 ~ 1|location,
                     start = c(a1 = exp(init_s[1]), a2 = init_s[2]),
                     weights = varPower(form = ~fitted(.)),
                     method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
        
        m3_s <- nlme(mod_power,
                     data = new_data,
                     fixed = list(a1 ~ protocol, a2 ~ 1),
                     random = a1 ~ 1|location,
                     start = c(a1 = c(rep(exp(init_s[1]), length(unique(new_data$protocol)))), a2 = init_s[2]),
                     weights = varPower(form = ~fitted(.)),
                     method = "ML",  control = nlmeControl(maxIter = 1500, tolerance = 1e-2, pnlsTol = 1e-1))
        
        parameters_power_2[(((nrep * i) - nrep) + j),2] <- fixed.effects(m3_s)[1]
        parameters_power_2[(((nrep * i) - nrep) + j),22] <- fixed.effects(m3_s)[length(unique(new_data$protocol))+1]
        parameters_power_2[(((nrep * i) - nrep) + j),23] <-AIC(m3_s)
        
        
        for (k in 2:length(unique(new_data$protocol))) {
          parameters_power_2[(((nrep * i) - nrep) + j),k+1] <- fixed.effects(m3_s)[1]+fixed.effects(m3_s)[k]
        }
  
        
      },
      
      error = function(e) {
        
        print(paste("error model", species_list[i], " sampling", j, sep = " "))
        
      }) 
      
      j <- j + 1
      
      }
    }   
  

  dev.off()  
  
  write.csv(file = paste0("output/depth_linear_alldata__nlme.", sp,".csv"), parameters_linear_1)
  write.csv(file =  paste0("output/depth_linear_resampling__nlme.", sp,".csv"), parameters_linear_2)
  write.csv(file =  paste0("output/depth_power_alldata__nlme.", sp,".csv"), parameters_power_1)
  write.csv(file = paste0( "output/depth_power_resampling__nlme.", sp,".csv"), parameters_power_2)
  
  return(parameters_linear_1, parameters_linear_2, parameters_power_1, parameters_power_2)
  
}
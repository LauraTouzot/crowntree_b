depth_models_nlme <- function (sp) {
  
  ### 0. Loading data
  ### Loading data
  allometry_complete_database <- data.table::fread("data/allometry_complete_database.csv", sep = ",")
  NFI_data = readRDS(file = "data/NFI_TNRS_check.rds")
  allometry_complete_database <- as.data.frame(allometry_complete_database) 
  
  # extracting species list from NFI data (191 species)
  sampling <- NFI_data %>% 
    filter(continent == "E_U" & nplot >= 100 & ntree >= 1000 | continent == "N_A" & nplot >= 150 & ntree >= 3000)
  rm(NFI_data)
  # extracting species list in the allometry database (180 species)
  allometry_complete_database <- allometry_complete_database %>% ungroup()
  species <- unique(allometry_complete_database$checked_name)
  data_summary <- allometry_complete_database %>% group_by(checked_name) %>% summarise(nplot_crown = length(unique(location_ID)),
                                                                                       ntree_crown = length(location_ID),
                                                                                       nobs_HT = sum(!is.na(HT_m)),
                                                                                       nobs_Cdiam = sum(!is.na(C_diam_m)),
                                                                                       nobs_Cdepth = sum(!is.na(C_depth_m)),
                                                                                       nobs_CR = sum(!is.na(CR))) %>% ungroup()
  
  
  sampling <- left_join(sampling, data_summary, by = "checked_name")
  rm(data_summary)
  selected_sp <- sampling %>% 
    filter(continent == "E_U" & nplot_crown >= 100 & ntree_crown >= 1000 | continent == "N_A" & nplot_crown >= 150 & ntree_crown >= 3000)
  
  species_list <- unique(selected_sp$checked_name)
  species_list <- sort(species_list) # do not forget to order species list so that the rest of the code makes sense
  species_list <- species_list[-1]
  
  data_ok <- allometry_complete_database %>% filter(checked_name %in% species_list) %>% 
    filter(!duplicated(data))
  nrep = 20
  
  ## 1. Preparing storage before running the models
  parameters_linear_1 <- as.data.frame(matrix(nrow = 1, ncol = length(unique(data_ok$data)) + 4))
  parameters_linear_1[,1] <- sp
  
  parameters_power_1 <- as.data.frame(matrix(nrow = 1, ncol = length(unique(data_ok$data)) + 5))
  parameters_power_1[,1] <- sp
  
  
  parameters_linear_2 <- as.data.frame(matrix(nrow =  nrep, ncol = length(unique(data_ok$data)) + 4))
  parameters_linear_2[,1] <- rep(sp, nrep)
  
  parameters_power_2 <- as.data.frame(matrix(nrow =  nrep, ncol = length(unique(data_ok$data)) + 5))
  parameters_power_2[,1] <- rep(sp, nrep)
  
  names(parameters_linear_1) <- names(parameters_linear_2) <-
    c("species", "inter", paste0("protocol", unique(data_ok$data)), "slope", "AIC")
  
  names(parameters_power_1) <- names(parameters_power_2) <-
    c("species", "inter", paste0("protocol", unique(data_ok$data)), "slope", "std_residuals", "AIC")
  
  ## 2. Running different models on all data 
  
  i <- (1:length(species_list))[species_list == sp]
  
  # compiling selected data and variables
  data <- allometry_complete_database %>% filter(checked_name == species_list[i],
                                                 !is.na(DBH_cm) & !is.na(C_depth_m) & C_depth_m >0) %>%
    select(DBH_cm, C_diam_m, location_ID, data) %>%
    rename(x = DBH_cm, y = C_depth_m, location = location_ID, protocol = data) %>% 
    mutate(x = as.numeric(x), y = as.numeric(y), location = as.character(location), 
           protocol = as.character(protocol))
  
  sel_loc <- names(table(data$location))[table(data$location) > 2]
  data_2 <- data[data$location %in% sel_loc, ]
  data_2$location <- factor(data_2$location)  
  rm(allometry_complete_database, data_ok, sampling,selected_sp, sel_loc, species)
  

  if (dim(data_2)[1] >= 200) { # running the models only if more than 200 observations are left in the sampled data
    
    print(i)
    
    ### STEP 1: FITTING MODELS ON ALL DATA  
    # tested models
    
    tryCatch({  

      
      # fitting linear relationships
      m1_l <- lme(y ~ x,
                  data = data_2,
                  random = ~ 1|location,
                  weights = varPower(form = ~fitted(.)),
                  method = "ML",  control = lmeControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))
      
      if (length(unique(data_2$protocol))>1) {
        
        m2_l <- lme(y ~ x + protocol,
                    data = data_2,
                    random = ~ 1|location,
                    weights = varPower(form = ~fitted(.)),
                    method = "ML",  control = lmeControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))

        parameters_linear_1[1,"inter"] <- fixed.effects(m2_l)[1]
        parameters_linear_1[1, "slope"] <- fixed.effects(m2_l)[2]
        parameters_linear_1[1, "AIC"] <- AIC(m2_l)
        
        for (k in paste0("protocol", levels(data_2$protocol)[-1])) {
          
          parameters_linear_1[1,k] <- fixed.effects(m2_l)[1]+fixed.effects(m2_l)[k]
        }
        
        parameters_linear_1[1,paste0("protocol", levels(data_2$protocol)[1])] <- fixed.effects(m2_l)[1]
        
      } else {
        
        parameters_linear_1[1,"inter"] <- fixed.effects(m1_l)[1]
        parameters_linear_1[1, "slope"] <- fixed.effects(m1_l)[2]
        parameters_linear_1[1, "AIC"] <- AIC(m1_l)
        parameters_linear_1[1,paste0("protocol", unique(data$protocol))] <- fixed.effects(m1_l)[1]
        
      }
      
    }, 
    
    error = function(e) {
      
      print(paste("error linear model", species_list[i], sep = " "))
      
    }) 
    
    
    tryCatch({
      
      # fitting power relationships
      m2 <- lmer(log(y) ~ log(x) + (1|location), data_2)
      
      if(length(unique(data_2$protocol))>1){
        
        m3 <- lmer(log(y) ~ protocol + log(x) + (1|location), data_2)
        
        parameters_power_1[1,"inter"] <- fixed.effects(m3)[1]
        parameters_power_1[1,"slope"] <- fixed.effects(m3)[length(unique(data_2$protocol))+1]
        parameters_power_1[1,"AIC"] <- AIC(m3)
        parameters_power_1[1, "std_residuals"] <- sigma(m3)
        
        
        for (k in paste0("protocol", levels(data_2$protocol)[-1])) {
          parameters_power_1[1,k] <- fixed.effects(m3)[1]+fixed.effects(m3)[k]
        }
        
        parameters_power_1[1,paste0("protocol", levels(data_2$protocol)[1])] <- fixed.effects(m3)[1]
        
      } else {
        
        parameters_power_1[1,"inter"] <- fixed.effects(m2)[1]
        parameters_power_1[1,"slope"] <- fixed.effects(m2)[length(unique(data_2$protocol))+1]
        parameters_power_1[1,"AIC"] <- AIC(m2)
        parameters_power_1[1, "std_residuals"] <- sigma(m2)
        parameters_power_1[1,paste0("protocol", unique(data$protocol))] <- fixed.effects(m2)[1]
        
      }
     
    },
    
    error = function(e) {
      
      print(paste("error power model", species_list[i], sep = " "))
      
    }) 
    
    
    ### STEP 2: FITTING MODELS ON SUBSAMPLES  
    
    # creating classes of dbh
    range_dbh <- max(data$x) - 10
    class_dbh <- range_dbh/4
    s1 <- class_dbh + 10
    s2 <- class_dbh * 2 + 10
    s3 <- class_dbh * 3 + 10
    d1 <- data[data$x < s1,]
    d2 <- data[data$x >= s1 & data$x < s2,]
    d3 <- data[data$x >= s2 & data$x < s3,]
    d4 <- data[data$x >= s3,]
    
    size_a <- ceiling(length(unique(d1$location))* 0.7)
    size_b <- ceiling(length(unique(d2$location))* 0.7)
    size_c <- ceiling(length(unique(d3$location))* 0.7)
    size_d <- ceiling(length(unique(d4$location))* 0.7)
    
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
      d2 <- data[data$x >= s1 & data$x < s2,]
      d3 <- data[data$x >= s2 & data$x < s3,]
      d4 <- data[data$x >= s3,]
      
      size_a <- ceiling(length(unique(d1$location))* 0.7)
      size_b <- ceiling(length(unique(d2$location))* 0.7)
      size_c <- ceiling(length(unique(d3$location))* 0.7)
      size_d <- ceiling(length(unique(d4$location))* 0.7)
      
      sample_size <- min(size_a, size_b, size_c, size_d)
    }
    
    # computing nb of datasets in which the species was surveyed
    nb_datasets_all <- length(unique(c(d1$protocol, d2$protocol, d3$protocol, d4$protocol)))

    
    for (j in 1:nrep) {
      
      # tested models
      
      loc1 <- sample(unique(d1$location), sample_size)
      class_1 <- d1 %>% filter(location %in% loc1) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
      loc2 <- sample(unique(d2$location), sample_size)
      class_2 <- d2 %>% filter(location %in% loc2) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
      loc3 <- sample(unique(d3$location), sample_size)
      class_3 <- d3 %>% filter(location %in% loc3) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
      loc4 <- sample(unique(d4$location), sample_size)
      class_4 <- d4 %>% filter(location %in% loc4) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
      
      new_data <- bind_rows(class_1, class_2, class_3, class_4)
      nb_datasets_sample <- length(unique(new_data$protocol))
      
      maxit <- 0
      while (nb_datasets_sample < ceiling(nb_datasets_all * 0.33) & maxit < 6) { # ceiling
        
        loc1 <- sample(unique(d1$location), sample_size)
        class_1 <- d1 %>% filter(location %in% loc1) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
        loc2 <- sample(unique(d2$location), sample_size)
        class_2 <- d2 %>% filter(location %in% loc2) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
        loc3 <- sample(unique(d3$location), sample_size)
        class_3 <- d3 %>% filter(location %in% loc3) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
        loc4 <- sample(unique(d4$location), sample_size)
        class_4 <- d4 %>% filter(location %in% loc4) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
        
        new_data <- bind_rows(class_1, class_2, class_3, class_4)
        
        nb_datasets_sample <- length(unique(new_data$protocol)) 
        maxit <- maxit + 1
        
        }
      
      
      
      tryCatch({
        
        # fitting linear relationshios
        
        m1_ls <- gls(y ~ x,
                     data = new_data,
                     weights = varPower(form = ~fitted(.)),
                     method = "ML", control = lmeControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))
        
        if(length(unique(new_data$protocol))>1){
          
          m2_ls <- gls(y ~ x + protocol,
                       data = new_data,
                       weights = varPower(form = ~fitted(.)),
                       method = "ML", control = glsControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))
          
          parameters_linear_2[ j,"inter"] <- coefficients(m2_ls)[1]
          parameters_linear_2[j,"slope"] <- coefficients(m2_ls)[2]
          parameters_linear_2[j,"AIC"] <- AIC(m2_ls)
          
          for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
            parameters_linear_2[j,k] <- coefficients(m2_ls)[1] + coefficients(m2_ls)[k]
          }
          
          parameters_linear_2[j,paste0("protocol", levels(factor(new_data$protocol))[1])] <- coefficients(m2_ls)[1]
          
        } else {
          
          parameters_linear_2[ j,"inter"] <- coefficients(m1_ls)[1]
          parameters_linear_2[j,"slope"] <- coefficients(m1_ls)[2]
          parameters_linear_2[j,"AIC"] <- AIC(m1_ls)
          parameters_linear_2[j,paste0("protocol", unique(new_data$protocol))] <- coefficients(m1_ls)[1]
          
        }
        
        
        # fitting power relationships
        m2_s <- gls(log(y) ~ log(x),
                    data = new_data,
                    weights = varPower(form = ~fitted(.)),
                    method = "ML",  control = glsControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))
        
        if(length(unique(new_data$protocol))>1){
          
          m3_s <- gls(log(y) ~ log(x) + protocol,
                      data = new_data,
                      weights = varPower(form = ~fitted(.)),
                      method = "ML",  control = glsControl(maxIter = 1500, tolerance = 1e-2, msTol = 1e-1))
          
          parameters_power_2[j,"inter"] <- coefficients(m3_s)[1]
          parameters_power_2[j,"slope"] <- coefficients(m3_s)[2]
          parameters_power_2[j, "std_residuals"] <- sigma(m3_s)
          parameters_power_2[j,"AIC"] <- AIC(m3_s)
          
          for (k in paste0("protocol", levels(factor(new_data$protocol))[-1])) {
            
            parameters_power_2[j,k] <- coefficients(m2_ls)[k]
          }
          
          parameters_power_2[j,paste0("protocol", levels(factor(new_data$protocol))[1])] <- coef(m3_s)[1]
          
        } else {
          
          parameters_power_2[j,"inter"] <- coefficients(m2_s)[1]
          parameters_power_2[j,"slope"] <- coefficients(m2_s)(m2_s)[length(unique(new_data$protocol))+1]
          parameters_power_2[j,"AIC"] <-AIC(m2_s)
          parameters_power_2[j, "std_residuals"] <- sigma(m2_s)
          parameters_power_2[j,paste0("protocol", unique(new_data$protocol))] <- coefficients(m2_s)(m2_s)[1]
          
        }
        
      },
      
      error = function(e) {
        
        print(paste("error model", species_list[i], " sampling", j, sep = " "))
        
      }) 
      
      
      
    }
    
  }   
 
  
  write.csv(file = paste0("output/depth_linear_alldata__nlme.",sp,".csv"), parameters_linear_1)
  write.csv(file = paste0("output/depth_linear_resampling__nlme.",sp,".csv"), parameters_linear_2)
  write.csv(file = paste0("output/depth_power_alldata__nlme.",sp,".csv"), parameters_power_1)
  write.csv(file = paste0("output/depth_power_resampling__nlme.",sp,".csv"), parameters_power_2)
  
  return(list(parameters_linear_1, parameters_linear_2, parameters_power_1, parameters_power_2))
  rm(list = ls())
  gc()
  
}

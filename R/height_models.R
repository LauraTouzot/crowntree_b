asymptot_alldata_nocomp <- function(data, sp) {
  
  ## loading data and species list
  data_ok <- height_files[[1]]
  species_list <- height_files[[2]]
  
  ## creating file to store model parameters (1 file per species)
  asymptot_alldata_nocomp <- as.data.frame(matrix(nrow = 1, ncol = length(unique(data_ok$protocol)) + 5)) 
  asymptot_alldata_nocomp[,1] <- sp
  
  names(asymptot_alldata_nocomp) <- c("species", "b1", paste0("protocol", unique(data_ok$protocol)), "b2", "b3", "AIC")
  
  ## running the model for all species in the list
  i <- (1:length(species_list))[species_list == sp]

  print(i)
  
  ## defining tested asymptotic model and selecting data
  mod_asympt <- y ~ 1.3 + b1 * (1-exp(-b2 * x)) ^ b3
  data <- data_ok %>% filter(sp == species_list[i]) %>%
                      filter(!is.na(x) & !is.na(y) & x >= 10 & y > 0) %>%
                      select(x, y, location, protocol) %>%
                      mutate(x = as.numeric(x), y = as.numeric(y), location = as.factor(location), protocol = as.factor(protocol))
  
  rm(data_ok)
  gc()
  
  tryCatch({
    
    vars <- data.frame(var = c("b1", "b2", "b3"), start = c(quantile(data$y, probs = 0.97)*0.8, 0.07, 0.9))
    
    m0_a <- nls(mod_asympt,
              data = data,
              start = setNames(as.list(vars$start), vars$var),
              lower = c(0.0001, 0.0001, 0.0001), algorithm = "port",
              nls.control(maxiter = 800))
    
    m1_a <- nlme(mod_asympt,
               data = data,
               fixed = list(b1 ~ 1, b2 ~ 1, b3 ~ 1),
               random = b1 ~ 1|location,
               start = c(b1 = coefficients(m0_a)["b1"], b2 = coefficients(m0_a)["b2"], b3 = coefficients(m0_a)["b3"]),
               method = "ML", 
               weights = varPower(form = ~fitted(.)),
               control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
    
    if (length(unique(data$protocol)) > 1) {
      
      m2_a <- nlme(mod_asympt,
                 data = data,
                 fixed = list(b1 ~ protocol, b2 ~ 1, b3 ~ 1),
                 random = b1 ~ 1|location,
                 start = c(b1 = c(rep(fixef(m1_a)["b1"], length(unique(data$protocol))), b2 = fixef(m1_a)["b2"], b3 = fixef(m1_a)["b3"])),
                 method = "ML", 
                 weights = varPower(form = ~fitted(.)),
                 control = nlmeControl(maxIter = 1500, tolerance = 1e-3, pnlsTol = 1e-2))
      
      asymptot_alldata_nocomp[1,"b1"] <- fixed.effects(m2_a)[1]
      asymptot_alldata_nocomp[1,"b2"] <- fixed.effects(m2_a)["b2"]
      asymptot_alldata_nocomp[1,"b3"] <- fixed.effects(m2_a)["b3"]
      asymptot_alldata_nocomp[1,"AIC"] <- AIC(m2_a)
      
      asymptot_alldata_nocomp[1,paste0("protocol", levels(data$protocol)[1])] <- fixed.effects(m2_a)[1]
      
      for (k in paste0("protocol", levels(data$protocol)[-1])) {
        asymptot_alldata_nocomp[1,k] <- fixed.effects(m2_a)[1] + fixed.effects(m2_a)[paste0("b1.", k)]
      }
      
      
      
    } else {
      
      asymptot_alldata_nocomp[1,"b1"] <- fixed.effects(m1_a)["b1"]
      asymptot_alldata_nocomp[1,"b2"] <- fixed.effects(m1_a)["b2"]
      asymptot_alldata_nocomp[1,"b3"] <- fixed.effects(m1_a)["b3"]
      asymptot_alldata_nocomp[1,"AIC"] <- AIC(m1_a)
      asymptot_alldata_nocomp[1,paste0("protocol", unique(data$protocol))] <- fixed.effects(m1_a)[1]
      
    }
    
  },
  
  error = function(e) {
    print(species_list[i])
  })
  
  write.csv(asymptot_alldata_nocomp, file =  paste0("output/height_alldata_nocomp_", sp, ".csv"))
  
}





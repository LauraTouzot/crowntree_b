data_in_class <- function(data) {
  
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
    
  }
  
  d1 <- d1 %>% mutate(dbh_class = "d1")
  d2 <- d2 %>% mutate(dbh_class = "d2")
  d3 <- d3 %>% mutate(dbh_class = "d3")
  d4 <- d4 %>% mutate(dbh_class = "d4")
  
  ranged_data <- bind_rows(d1, d2, d3, d4)
  
  return(ranged_data)
    
}


data_in_class_bis <- function(data) {
  
  range_dbh <- max(data$x)
  class_dbh <- range_dbh/4
  s1 <- class_dbh
  s2 <- class_dbh * 2
  s3 <- class_dbh * 3
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
    range_dbh <- max(data$x)
    class_dbh <- range_dbh/4
    
    s1 <- class_dbh 
    s2 <- class_dbh * 2 
    s3 <- class_dbh * 3 
    d1 <- data[data$x < s1,]
    d2 <- data[data$x >= s1 & data$x < s2,]
    d3 <- data[data$x >= s2 & data$x < s3,]
    d4 <- data[data$x >= s3,]
    
  }
  
  d1 <- d1 %>% mutate(dbh_class = "d1")
  d2 <- d2 %>% mutate(dbh_class = "d2")
  d3 <- d3 %>% mutate(dbh_class = "d3")
  d4 <- d4 %>% mutate(dbh_class = "d4")
  
  ranged_data <- bind_rows(d1, d2, d3, d4)
  
  return(ranged_data)
  
}




what_sample_size <- function(ranged_data) {
  
  df <- ranged_data %>% select(dbh_class, location) %>%
                            group_by(dbh_class) %>% summarise(count_distinct = n_distinct(location)) %>%
                            mutate(size = ceiling(count_distinct * 0.7)) %>% ungroup()
  
  sample_size <- min(df$size)
  
  return(sample_size)
  
}




sampling_protocol <- function(ranged_data, nb_datasets_all, sample_size) {
  
  # sampling data within each dbh class
  loc1 <- ranged_data %>% filter(dbh_class == "d1") 
  sampled_loc1 <- sample(unique(loc1$location), sample_size)
  class_1 <- loc1 %>% filter(location %in% sampled_loc1) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()

  loc2 <- ranged_data %>% filter(dbh_class == "d1") 
  sampled_loc2 <- sample(unique(loc2$location), sample_size)
  class_2 <- loc2 %>% filter(location %in% sampled_loc2) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()

  loc3 <- ranged_data %>% filter(dbh_class == "d1") 
  sampled_loc3 <- sample(unique(loc3$location), sample_size)
  class_3 <- loc3 %>% filter(location %in% sampled_loc3) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()

  loc4 <- ranged_data %>% filter(dbh_class == "d1") 
  sampled_loc4 <- sample(unique(loc4$location), sample_size)
  class_4 <- loc4 %>% filter(location %in% sampled_loc4) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()

  # computing new dataset and extracting nb of protocols presnt within the new dataset
  new_data <- bind_rows(class_1, class_2, class_3, class_4)
  new_data <- new_data %>% mutate(protocol = droplevels.factor(protocol))
  nb_datasets_sample <- length(unique(new_data$protocol))
  
  maxit <- 0
  while (nb_datasets_sample < ceiling(nb_datasets_all * 0.33) & maxit < 6) { # ceiling
    
    # sampling data within each dbh class
    loc1 <- ranged_data %>% filter(dbh_class == "d1") 
    sampled_loc1 <- sample(unique(loc1$location), sample_size)
    class_1 <- loc1 %>% filter(location %in% sampled_loc1) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
    
    loc2 <- ranged_data %>% filter(dbh_class == "d1") 
    sampled_loc2 <- sample(unique(loc2$location), sample_size)
    class_2 <- loc2 %>% filter(location %in% sampled_loc2) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
    
    loc3 <- ranged_data %>% filter(dbh_class == "d1") 
    sampled_loc3 <- sample(unique(loc3$location), sample_size)
    class_3 <- loc3 %>% filter(location %in% sampled_loc3) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
    
    loc4 <- ranged_data %>% filter(dbh_class == "d1") 
    sampled_loc4 <- sample(unique(loc4$location), sample_size)
    class_4 <- loc4 %>% filter(location %in% sampled_loc4) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
    
    # computing new dataset and extracting nb of protocols presnt within the new dataset
    new_data <- bind_rows(class_1, class_2, class_3, class_4)
    new_data <- new_data %>% mutate(protocol = droplevels.factor(protocol))
    nb_datasets_sample <- length(unique(new_data$protocol))
    
    maxit <- maxit + 1
  }
  
  # removing protocols with less than 10 observations
  sel_pro <- names(table(new_data$protocol))[table(new_data$protocol) > 9]
  new_data <- new_data[as.character(new_data$protocol) %in% sel_pro,]
  new_data$protocol <- as.factor(new_data$protocol)
  
  return(new_data)
  
}




testing_data <- function(ranged_data, new_data, sample_size) {
  
  loc1 <- ranged_data %>% filter(dbh_class == "d1") 
  data1 <- new_data %>% filter(dbh_class == "d1")
  sampled_loc1b <- sample(unique(loc1$location), ceiling((sample_size*0.3)/0.7))
  loc1b <- loc1 %>% filter(!(id %in% data1$id))
  class_1b <- loc1b %>% filter(location %in% sampled_loc1b) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
  
  loc2 <- ranged_data %>% filter(dbh_class == "d1") 
  data1 <- new_data %>% filter(dbh_class == "d1")
  sampled_loc2b <- sample(unique(loc2$location), ceiling((sample_size*0.3)/0.7))
  loc2b <- loc2 %>% filter(!(id %in% data1$id))
  class_2b <- loc2b %>% filter(location %in% sampled_loc2b) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
  
  loc3 <- ranged_data %>% filter(dbh_class == "d1") 
  data1 <- new_data %>% filter(dbh_class == "d1")
  sampled_loc3b <- sample(unique(loc3$location), ceiling((sample_size*0.3)/0.7))
  loc3b <- loc3 %>% filter(!(id %in% data1$id))
  class_3b <- loc3b %>% filter(location %in% sampled_loc3b) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
  
  loc4 <- ranged_data %>% filter(dbh_class == "d1") 
  data1 <- new_data %>% filter(dbh_class == "d1")
  sampled_loc4b <- sample(unique(loc4$location), ceiling((sample_size*0.3)/0.7))
  loc4b <- loc4 %>% filter(!(id %in% data1$id))
  class_4b <- loc4b %>% filter(location %in% sampled_loc4b) %>% group_by(location) %>% slice_sample(n = 1) %>% ungroup()
  
  test_data <- bind_rows(class_1b, class_2b, class_3b, class_4b)
  
  return(test_data)
  
  
}





# Read Usoltsev data

Usoltsev_data <- function(){
  
  data <- read_excel("data/BDengl2-1.xlsx",
                     sheet = "Database", skip = 1)
  
  ID_unique <- as.data.frame(1:dim(data)[1])
  colnames(ID_unique) <- "ID_unique"
  data <- cbind(ID_unique, data)
  
  long <- as.data.frame(str_split_fixed(data$`Longitude, East`, "-", 2))
  colnames(long) <- c("long_1", "long_2")
  long <- cbind(ID_unique, long)
  long$long_1 <- str_replace_all(long$long_1, c("59.07′" = "59.07", "60.00′" = "60.00", "101.1043\"" = "101.1043", "93.10′" = "93.10", "59.18′" = "59.18", "77.52′" = "77.52", "79.18′" = "79.18"))
  long_1 <- long[,c("ID_unique", "long_1")]
  long[long$long_2 == "",] <- NA
  long_2 <- long$long_2
  longitude <- as.data.frame(cbind(long_1, long_2))
  longitude_b <- longitude[!is.na(longitude$long_2),]
  longitude_b$long_1 <- as.numeric(longitude_b$long_1)
  longitude_b$long_2 <- as.numeric(longitude_b$long_2)
  longitude_b$long_1 <- (longitude_b$long_1 + longitude_b$long_2)/2
  longitude <- as.data.frame(longitude[is.na(longitude$long_2),])
  longitude <- rbind(longitude, longitude_b)
  longitude <- longitude[,c("ID_unique", "long_1")]
  
  lat <- as.data.frame(str_split_fixed(data$Latitude, "-", 2))
  colnames(lat) <- c("lat_1", "lat_2")
  lat <- cbind(ID_unique, lat)
  lat$lat_1 <- str_replace_all(lat$lat_1, c("64.0336\"" = "64.0336", "56.54′" = "56.54", "52.00′" = "52.00", "52.25′" = "52.25"))
  lat_1 <- lat[,c("ID_unique", "lat_1")]
  lat[lat$lat_2 == "",] <- NA
  lat_2 <- lat$lat_2
  latitude <- as.data.frame(cbind(lat_1, lat_2))
  latitude_b <- latitude[!is.na(latitude$lat_2),]
  latitude_b$lat_1 <- as.numeric(latitude_b$lat_1)
  latitude_b$lat_2 <- as.numeric(latitude_b$lat_2)
  latitude_b$lat_1 <- (latitude_b$lat_1 + latitude_b$lat_2)/2
  latitude <- as.data.frame(latitude[is.na(latitude$lat_2),])
  latitude <- rbind(latitude, latitude_b)
  latitude <- latitude[,c("ID_unique", "lat_1")]
  
  data <- left_join(data, longitude, by = "ID_unique")
  data <- left_join(data, latitude, by = "ID_unique")
  
  data$sp <- word(data$Species, 1,2, sep=" ")
  names(data) <- gsub(",", ".", gsub(" ", "_", names(data)))
  data$H._m <- as.numeric(data$H._m)
  df <- data %>% filter(!is.na(D._cm)) %>%
    rename(DBH_cm = D._cm,
           HT_m = H._m)
  df <- df %>% dplyr::select(sp, DBH_cm, HT_m, Location, long_1, lat_1)
  
  # add continents based on plot coordinates
  states_map <- readOGR(dsn="data/ne_10m_admin_1_states_provinces/", 
                        layer="ne_10m_admin_1_states_provinces")
  
  countriesSP <- getMap(resolution = 'low')
  
  df$longitude <- as.numeric(df$long_1)
  df$latitude <- as.numeric(df$lat_1)
  
  ID_unique <- as.data.frame(c(1:dim(df)[1]))
  colnames(ID_unique) <- "ID_unique"
  df <- cbind(ID_unique, df)
  
  no_coordinates <- df[is.na(df$longitude),]
  no_coordinates$continent <- "E_U"
  no_coordinates$cont <- NA
  
  df_coordinates <- df[!is.na(df$longitude),]
  coordinates <- df_coordinates[,c("longitude", "latitude")]
  
  pointsSP = SpatialPoints(coordinates, proj4string = CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  indices = over(pointsSP, countriesSP)
  
  locations <- as.data.frame(indices$REGION)
  colnames(locations) <- "cont"
  locations <- cbind(df_coordinates$ID_unique, locations)
  colnames(locations) <- c("ID_unique", "cont")
  
  continent <- as.data.frame(c("E_U", "N_A", "A_S", "S_A", "A_F", "A_U"))
  cont <- as.data.frame(c("Europe", "North America", "Asia", "South America", "Africa", "Australia"))
  continents <- cbind(continent, cont)
  colnames(continents) <- c("continent", "cont")
  
  locations <- left_join(locations, continents, by = "cont")
  no_coordinates <- no_coordinates[,c("ID_unique", "cont", "continent")]
  locations <- rbind(locations, no_coordinates)
  
  Usoltsev_crown_data <- left_join(locations, df, by = "ID_unique")
  
  Usoltsev_crown_data$latitude_plot <- Usoltsev_crown_data$latitude
  Usoltsev_crown_data$longitude_plot <- Usoltsev_crown_data$longitude
  Usoltsev_crown_data$latitude_tree <- NA
  Usoltsev_crown_data$longitude_tree <- NA
  Usoltsev_crown_data$location_ID <- paste(Usoltsev_crown_data$Location, "Usoltev", sep = "_")
  Usoltsev_crown_data$W <- NA
  
  Usoltsev_crown_data <- Usoltsev_crown_data %>% ungroup()
  
  # plots to check unit within the dataset
  p1 <-  ggplot(Usoltsev_crown_data[!is.na(Usoltsev_crown_data$HT_m), ], aes(x = DBH_cm, y = HT_m)) + geom_point(alpha = 0.1) 
  png("figures/unit_check/unit_check_Usoltev.png", width = 340, height = 340)
  p1
  dev.off()
  
  return(Usoltsev_crown_data)
  
}

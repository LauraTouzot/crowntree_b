### Cleaning data based on the first filter before the taxonomy check

cleaning_after_checking <- function(all_crown_checked) {
  
  df <- all_crown_checked[all_crown_checked$location_ID != "TN_127_NA_1_1_FHM_1999",] # 3 extremely high measures of crown diameter (> 30m) for small dbh (around 25 cm) in this plot 
  df$HT_m[df$location_ID == "Middle_Ural,_Nizhnie_Sergi_Usoltev" & df$DBH_cm == 12.5 & df$sp == "Pinus sibirica"] <- NA
  
  return(df)
  
}
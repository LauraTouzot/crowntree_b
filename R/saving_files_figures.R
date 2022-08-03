#### Saving files and figures

# Function to get the path of a file, and create directories if they don't exist
# @param file.in character: path of the file, filename included (ex: "plot/plot.png")
# @author Julien BARRERE

create_dir_if_needed <- function(file.in){
  
  path.in <- strsplit(file.in, "/")[[1]]
  
  if(length(path.in) > 1) {
    for(i in 1:(length(path.in)-1)) {
      if(i == 1) path.in_i <- path.in[i]
      else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
      if(!dir.exists(path.in_i)) dir.create(path.in_i)
    }
  }
}

# Get file from its url and write it on disk, at a specified location. 
# @param dir.name.in Directory where the file should be written (ex: "data/BWI")
# @param url.in URL where to download the file.
# @author Julien BARRERE

get_and_write <- function(dir.name.in, url.in){
  
  # Write directories if they do not exist
  path.in <- strsplit(dir.name.in, "/")[[1]]
  
  for(i in 1:length(path.in)) {
    
    if(i == 1) path.in_i <- path.in[i]
    else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
    if(!dir.exists(path.in_i)) dir.create(path.in_i)
    
  }
  
  # Write file on the disk
  url.in_split <- strsplit(url.in, "/")[[1]]
  file.in <- paste(dir.name.in, url.in_split[length(url.in_split)], sep = "/")
  
  if(!file.exists(file.in)) {
    
    try(GET(url.in, authenticate('guest', ""), write_disk(file.in, overwrite = TRUE)))
    # Specific case of zip file: unzip and delete zip file
    
    if("zip" %in% strsplit(file.in, split = "\\.")[[1]]){
      unzip(file.in, exdir = dir.name.in, overwrite = T)
      print(paste0("---Getting and unzipping ", file.in))
      unlink(file.in)
    } else { print(paste0("---Getting ", file.in)) }
  } 
}


# Write a table on disk
# @param table.in dataframe to write on the disk
# @param file.in Name (and path) of the file on the disk
# @author Julien BARRERE

write_on_disk <- function(table.in, file.in) {
  
  create_dir_if_needed(file.in)
  write.table(table.in, file = file.in, row.names = F)
  return(file.in)
  
}

#### Saving files and figures

# function to identify path of a file/figure and create directories if they don't already exist

create_dir_if_needed <- function(file.in){
  
  path.in <- strsplit(file.in, "/")[[1]]
 
   if(length(path.in) > 1){
    for(i in 1:(length(path.in)-1)){
      if(i == 1) path.in_i <- path.in[i]
      else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
      if(!dir.exists(path.in_i)) dir.create(path.in_i)
      
    }
  }
}

# function to save files and figures where they belong

write_somewhere <- function(table.in, file.in){
  
  create_dir_if_needed(file.in)
  write.table(table.in, file = file.in, row.names = F)
  return(file.in)
  
}

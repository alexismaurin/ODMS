create_csv <- function(outliers, folder_name){
  
  destination_folder = paste0('Results/', str_replace_all(str_extract(Sys.time(), '.*(?=\\s)'), '-', '_'), '_',  folder_name)
  if(!dir.exists(destination_folder)) dir.create(destination_folder)
  
  write.csv(outliers, paste0(destination_folder, '/outliers_list.csv'), row.names = F)
  message(paste0("List of outliers available in ", destination_folder,"."))
  
}
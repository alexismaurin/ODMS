plot_datapoints_outliers_per_cluster <- function(outliers, data, folder_name, save){

  tick_label <- unique(substr(colnames(data[[1]]), 1, 4))
  tick_pos <- seq(1, length(colnames(data[[1]])),   ceiling(length(colnames(data[[1]]))/length(tick_label)))
  
  destination_folder = paste0('Results/', str_replace_all(str_extract(Sys.time(), '.*(?=\\s)'), '-', '_'), '_',  folder_name)
  if(!dir.exists(destination_folder)) dir.create(destination_folder)

  for(i in 1:length(data)){
    
    outlying_series <- outliers %>% 
      filter(cluster == i) %>% 
      select(series) %>% 
      pull()
    
    data_cluster <- data[[i]]
    
    if(save == T){
      
      Cairo(file = , paste0(destination_folder,'/cluster_', i,'.png'),
            type = 'png',
            units ="cm", 
            width = 15, 
            height = 15, 
            pointsize = 10, 
            dpi = 200)
      
      
    }
    
    matplot(data.frame(t(data_cluster)), 
            type = 'l',
            lty = 1,
            col = rgb(0, 0, 0, 0.1),
            xlab = '', 
            ylab = '', 
            xaxt = "n", 
            las = 1 ,
            main = paste('cluster', i))
    
    axis(side = 1, at = tick_pos, labels = F)
    text(x = tick_pos,  y = par("usr")[3], labels = tick_label, srt = 90, pos = 1, xpd = TRUE, cex = 0.8, offset = c(1.5), adj = 0.5)
    
    for(j in 1:length(outlying_series)){
      
      lines(c(unlist(data_cluster[outlying_series[j],])),
            col =  rgb(1,0,0),
            lwd = 1)
      
      periods <- outliers %>% 
        filter(series == outlying_series[j]) %>% 
        select(period) %>% 
        pull() %>% 
        as.character()
    
      values <- data_cluster[outlying_series[j], which(colnames(data_cluster)%in%periods)]
      
      points(which(colnames(data_cluster) %in% periods), values, col = 'blue', pch = 4)
      
    }
    
    if(save == T) dev.off()
    
  } 
  
  if(save == T) message(paste0("Plots available in ", destination_folder,"."))

}
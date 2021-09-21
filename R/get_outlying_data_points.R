get_outlying_data_points <- function(data, window_percentage, from='All'){
  
  #Order the data in case of monthly data.
  if(str_detect(colnames(data[[1]])[1], "[:digit:]{4}[:alpha:]{3}"))  data <- order_months_periods(data)
  
  #Gives information on the period over which the outlier detection will be applied
  if(from %in% colnames(data[[1]])){
    
    data <- lapply(data, function(x) x[,which(colnames(x)==from):length(x)])
    message(paste0('Outliers detection will be carried out from ', colnames(data[[1]])[1], ' to ', colnames(data[[1]])[length(data[[1]])], "."))
    
  }else if(from == 'All'){
    
    message(paste0('The whole period range will be used: from ', colnames(data[[1]])[1], ' to ', colnames(data[[1]])[length(data[[1]])], "."))
    
  }else{
    
    message("Set 'All' to use the whole period range.")
    message(paste0('Or set period between ', colnames(data[[1]])[1], ' to ', colnames(data[[1]])[length(data[[1]])], "."))
    
    stop('Non-valid period set up.', call. = F)
    
  }
  
  message('Detecting outlying data points on each cluster over the selected periods...')

  nb_period <-  ncol(data[[1]])
  
  data <- lapply(data, function(x){
    
    x %>% 
      tibble::rownames_to_column('series')
  
  })
  
  nb_clusters <- length(data)

  message('Clusters processed: ')
  
  outlying_series <- list()
  index = 1
  
  for(CLSTR in 1:nb_clusters){
    
    message(glue('{CLSTR}'))
    
    nb_series <- nrow(data[[CLSTR]])
    
    data_cluster_with_series_name <- data[[CLSTR]] %>% 
      gather(period, value, 2:(all_of(nb_period)+1))
     
    data2cluster <- data_cluster_with_series_name %>% 
      select(period, value) %>% 
      mutate(period = as.factor(period))
    
    gower_dist_list <- list()
    sliding_window_approach <- T
    
    #Try to compute the gower distance over the whole dataset of points 
    
    tryCatch({ 
      gower_dist_list[[1]] <- daisy(data2cluster, metric = 'gower')
      sliding_window_approach <-  F
      window_index = 1} , 
      error = function(e) {      
        message('Cannot compute the Gower distance for the whole cluster.')
        message(paste0('The sliding window approach will be used with a window of ',window_percentage ,'%.'))})
    
    gc()
    
    #Case when dissimilarity matrix too large
    #Computes the sliding window gower distance
    
    if(sliding_window_approach == T){
      
      nb_obs <- nrow(data_cluster)
      window_size = window_percentage*nb_obs /100
      step = ceiling(window_size/2)
      
      size_decider = T
      side_decider_iterator = F
      
      while(size_decider == T){
        
        tryCatch({ 
          
          test <- daisy(data_cluster[1:window_size,], metric = 'gower')
          size_decider <- F}, 
          
          error = function(e){})
        
        if(size_decider == T){
          
          window_percentage <- floor(window_percentage * 0.9)
          window_size <- window_percentage*nb_obs /100
          step <- ceiling(window_size/2)
          side_decider_iterator <-  T
          
        }
        
      } 
      
      if(side_decider_iterator == T) message(paste0('Too large window set up. ',window_percentage ,'% window used instead.'))
      
      rm(test)
      gc()
      
      window_index <- list()
      
      for(i in 0:(floor(nb_obs/step)-1)){
        
        window_index[[(i+1)]] <- (1:window_size)+step*i
      } 
      
      window_index[[length(window_index)]] <- window_index[[length(window_index)]][1:which(window_index[[length(window_index)]]==nb_obs)]
      
    }
    
    for(w in 1:length(window_index)){
      
      if(length(gower_dist_list) == 1){
        
        data_cluster_with_series_name_tmp <- data_cluster_with_series_name
        gower_dist <- gower_dist_list[[w]]
        
      }else {
        
        data_tmp <- data2cluster[window_index[[w]],]
        period_to_del <- names(table(data_tmp$period))[which(!(table(data_tmp$period) %in% c(0, nb_obs/nb_period)))]
        data_tmp <- data_tmp %>% 
          filter(!(period %in% period_to_del))
        
        gower_dist <- daisy(data_tmp , metric = 'gower')
        
        data_cluster_with_series_name_tmp <- data_cluster_with_series_name[window_index[[w]],] %>% 
          filter(!(period %in% period_to_del))
        
      }
      
      tmp <- as.matrix(gower_dist)
      tmp[tmp==0] <- 1
      epsilon <- max(apply(tmp, 2, min))
      rm(tmp)
      gc()
      
      dbscan.res <- dbscan(gower_dist, minPts = 1, eps = epsilon)
      clusters <- as.vector(table(dbscan.res$cluster))
      outliers <- which(clusters!= nb_series)
      outliers_indicator <- length(outliers)
      
      iter <-  1
      
      while(outliers_indicator==0 & iter != 6){
        
        epsilon <- epsilon*0.97
        iter <- iter + 1
        dbscan.res <- dbscan(gower_dist, minPts = 1, eps = epsilon)
        clusters <- as.vector(table(dbscan.res$cluster))
        outliers <- which(clusters!= nb_series)
        outliers_indicator <- length(outliers)

      }    
      
      if(length(outliers)!=0){
        
        distance_outlying_series <- data.frame()
        
        data_cluster_with_series_name_tmp$cluster <- dbscan.res$cluster
        
        tmp <- data_cluster_with_series_name_tmp %>%
          filter(cluster %in% outliers) %>%
          group_by(period) %>%
          group_split()
        
        outlying_cluster <- c()
        
        for(y in 1:length(tmp)){
          
          outlying_cluster[y] <- as.numeric(names(which.min(table(tmp[[y]]$cluster))))
          
        }
        
        data_cluster_with_series_name_tmp$outlier <- ifelse(data_cluster_with_series_name_tmp$cluster %in% outlying_cluster, 1, 0)
        
        data_outliers <- data_cluster_with_series_name_tmp %>%
          filter(outlier == 1) %>%
          select(series, period)
        
        data_outliers <- cbind(data_outliers, CLSTR)
        
        
      }else{
        
        distance_outlying <- NULL
        
      }   
      
      suppressWarnings(rm(gower_dist, data_tmp, data_cluster_meta))
      gc()
      
      outlying_series[[index]] <- data_outliers
      index <- index+1
      
    }
    
    suppressWarnings(rm(gower_dist_list, data_cluster_meta_all, data_cluster))
    gc()
    
  }
  
  outlying_series <-   do.call(rbind, Filter(Negate(is.null), outlying_series))
  names(outlying_series) <- c('series', 'period', 'cluster')
  
  outlying_series <- outlying_series %>% 
    group_by(series, period) %>% 
    group_split()
  
  outlying_series <- do.call(rbind, (lapply(outlying_series, function(x) x[1,]))) %>% 
    arrange(cluster)
  
  
  message('Done!')
  
  return(outlying_series)
  
}

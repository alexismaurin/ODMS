get_clusters <- function(data_raw, data_scaled, data_scaled_smoothed, q_parameter, lamda_parameter){
  
  message('Finding optimal number of clusters using the Affinity Propagation algorithm...')
  
  AP_results <- apcluster(negDistMat(r=2), as.matrix(data_scaled_smoothed), q = q_parameter, lam = lamda_parameter)
  
  nb_clusters <- length(AP_results@clusters)
  message('Done! ', nb_clusters, ' clusters identified.')
  
  data_series_cluster <- data.frame()
  
  for(c in 1:nb_clusters){
    
    tmp <- as.data.frame(AP_results@clusters[[c]]) %>% 
      tibble::rownames_to_column('series') %>% 
      select(-2) %>% 
      mutate(cluster = c)
    
    data_series_cluster <- rbind(data_series_cluster, tmp)
      
    }
    
  
  data_raw <- left_join(data_raw %>% tibble::rownames_to_column('series'), data_series_cluster, by = 'series') %>% 
    group_by(cluster) %>% 
    group_split() %>% 
    lapply(function(x){
      
      x %>% 
        select(-cluster) %>% 
        tibble::column_to_rownames('series')
    })
  
  data_scaled <- merge(data_scaled %>% tibble::rownames_to_column('series'), data_series_cluster, by = 'series') %>% 
    group_by(cluster) %>% 
    group_split() %>% 
    lapply(function(x){
      
      x %>% 
        select(-cluster) %>% 
        tibble::column_to_rownames('series')
    })
  
  
  results <- list(data_raw = data_raw,
                  data_scaled = data_scaled,
                  centre = AP_results@exemplars)
  
  message('Data with clusters created!')
  
  gc()
  
  return(results)
  
}




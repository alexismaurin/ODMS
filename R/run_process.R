run_process <- function(data, lowess_smoothing_parameter, ap_q_parameter, ap_lambda_parameter, window_percentage, from, save_plots, folder_name){
  
  #Standardise data
  data_scaled <- data.frame(t(scale(t(data))))
  data_scaled[is.na(data_scaled)] <- 0
  names(data_scaled) <- names(data)
  
  #Smooth data
  data_scaled_smoothed <- data.frame(t(apply(data_scaled, 1, function(x) lowess(x, f = lowess_smoothing_parameter)$y)))
  data_scaled_smoothed[is.na(data_scaled_smoothed)] <- 0
  names(data_scaled_smoothed) <- names(data) 
  
  #Find clusters
  data_with_clusters <- get_clusters(data, data_scaled, data_scaled_smoothed, ap_q_parameter, ap_lambda_parameter)
  
  #Identify outliers
  outliers <- get_outlying_data_points(data = data_with_clusters[['data_scaled']], window_percentage = window_percentage, from = from)
  
  #Create outputs
  create_csv(outliers, folder_name)
  plot_datapoints_outliers_per_cluster(outliers = outliers, data = data_with_clusters[['data_scaled']], folder_name = folder_name, save = save_plots)
  
}

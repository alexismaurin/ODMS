#import the data and run the function

run_process(data = data, 
            lowess_smoothing_parameter = .2, 
            ap_q_parameter = 0, 
            ap_lambda_parameter = .5, 
            window_percentage = 25, 
            from = 'starting_period', 
            save_plots = T, 
            folder_name = 'data')



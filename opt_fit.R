####################################################################################
### Kadir Bice, University of Georgia, 2020
### This script creates a mean annual pattern of the series, fits that pattern to each years existing points
### by scaling and shifting to minimize total error of the fit and creates a gapfilled time series

### Notes: 
# Useful if most of the expected characteristics of the series are missing which would make statistical fitting harder
# Not good enough if there is a strong trend

### Inputs:
# gapped_ts = gapped time series data in (year, month, value) format 
# mean_pattern = mean values for each month of each site (month, value) format - for 12 months

### Output:
# A list including: artificial time series and properties of the best fit

opt_fit <- function(gapped_ts){
  
  # calculate means and standard deviations come up with an annual curve
  mean_pattern <- cbind(month = 1:12, aggregate(gapped_ts[,c(-1,-2)], by = list(gapped_ts$month),function(x) c(mean(x,na.rm = T)))[,-1],
                            aggregate(gapped_ts[,c(-1,-2)], by = list(gapped_ts$month),function(x) c(sd(x,na.rm = T)))[,-1])
  
  # fit the annual curve to the existing points per year to preserve annual pattern
  obj_func_list <- list() # list of different fittings
  min_obj_list <- list() # list of best alternatives
  month_range <- 1:12 # range to focus while fitting the curve
  min_obj <- c() # fitting that minimizes objective value
  
  # loop over years
  for(k in 1:length(as.numeric(unique(gapped_ts[,1])))){
    # objective value for each year
    obj_func <- c()
    # shifting parameter
    for(s in seq(0,max(gapped_ts[,3], na.rm = T),by = 50)){
      # scaling parameter
      for(o in seq(0.5,max(gapped_ts[,3], na.rm = T)/min(gapped_ts[,3], na.rm = T),by = 0.1)){
        observed <- gapped_ts %>% 
          dplyr::filter(year == unique(gapped_ts[,1])[k]) %>%
          select(year, month, 3)
        
        # loop over months to fit
        error_term <- c() # differences between fitted and measured values where available under fitting variations
        for (m in month_range){
          observed_month <- observed[observed[,2] == m,3] # month filter for observations
          fit_month <- mean_pattern[mean_pattern[,1] == m,2] - min(mean_pattern[,2]) # month filter for fit (subtract minimum value to shift to zero)
          if(sum(observed_month, na.rm = T) > 0){ # eliminate missing months
            error_term <- rbind(error_term, cbind(m,mean(observed_month),fit_month, o,s, # take mean of the same month data
                                                  mean(observed_month) - fit_month * o - s,
                                                  (mean(observed_month) - fit_month * o - s)^2))
          }
        }
        
        colnames(error_term) <- c('month','obs','fit','scale','shift','gap','error')
        obj_func <- rbind(obj_func, cbind(as.numeric(unique(gapped_ts[,1]))[k],o,s, mean(error_term[,7]))) # calculate obj func (mean to normalize, otherwise use sum)
      }
    }
    colnames(obj_func) <- c('year','scale','shift', 'obj')
    
    # plot(obj_func[,2], obj_func[,3], type = 'l', xlab = 'shift', ylab = 'objective')
    obj_func_list[[k]] <- obj_func # store objective value for each year 
    
    # find error minimizing combination
    min_obj <- rbind(min_obj, cbind(as.numeric(unique(gapped_ts[,1]))[k], obj_func[obj_func[,4] == min(obj_func[,4]),2][1], 
                                    obj_func[obj_func[,4] == min(obj_func[,4]),3][1],min(obj_func[,4])))
    
  }
  
  # create an artificial timeseries with gaps filled by fitted curves above
  artificial_ts <- c()
  for(k in 1:length(as.numeric(unique(gapped_ts[,1])))){
    artificial_ts <- data.frame(rbind(artificial_ts, cbind(year = as.numeric(unique(gapped_ts[,1]))[k],
                                               month = 1:12, value = (mean_pattern[,2] - min(mean_pattern[,2]))* min_obj[k,2] + min_obj[k,3])))
    }

  return(list(artificial_ts, min_obj))
}
############ optimized fit (this is an alternative to cover the gaps) 
# minimizes total error of the fit
# multi_biom = biomass data in (year, month, site) format 
# mean_shape_curve = mean values for each month of each site (month, site) format - for 12 months
# this is only for Wassaw now
opt_fit <- function(multi_biom, mean_shape_curve){
  obj_func_list <- list()
  min_obj_list <- list() # list of best alternatives
  par(mfrow=c(2,2))
  month_range <- 1:12 # focus on high variability part to decrease impact of low biomass parts
  min_obj <- c()
  
  for(k in 1:34){#length(as.numeric(unique(multi_biom[,1])))){
    obj_func <- c()
    # shifting parameter
    for(s in seq(0,800,by = 50)){
      # scaling parameter
      for(o in seq(0.5,2,by = 0.05)){
        observed_biomass <- multi_biom[multi_biom[,1] == unique(multi_biom[,1])[k],c(1,2,3)] # for Wassaw now
        head(observed_biomass)
        # loop over months
        error_term <- c()
        for (m in month_range){
          observed_biomass_month <- observed_biomass[observed_biomass[,2] == m,3] # month filter for observations
          fit_month <- mean_shape_curve[mean_shape_curve[,1] == m,2] - min(mean_shape_curve[,2]) # month filter for fit (subtract minimum value to shift to zero)
          if(sum(observed_biomass_month) > 0){ # eliminate missing months
            error_term <- rbind(error_term, cbind(m,mean(observed_biomass_month),fit_month, o,s, # take mean of the same month data
                                                  mean(observed_biomass_month) - fit_month * o - s,
                                                  (mean(observed_biomass_month) - fit_month * o - s)^2))
          }
        }
        
        colnames(error_term) <- c('month','obs','fit','scale','shift','gap','error')
        # print(error_term)
        obj_func <- rbind(obj_func, cbind(as.numeric(unique(multi_biom[,1]))[k],o,s, mean(error_term[,7]))) # calculate obj func (mean to normalize, otherwise use sum)
      }
    }
    colnames(obj_func) <- c('year','scale','shift', 'obj')
    
    # plot(obj_func[,2], obj_func[,3], type = 'l', xlab = 'shift', ylab = 'objective')
    obj_func_list[[k]] <- obj_func # store obj func for each year 
    
    # find error minimizing combination
    min_obj <- rbind(min_obj, cbind(as.numeric(unique(multi_biom[,1]))[k], obj_func[obj_func[,4] == min(obj_func[,4]),2][1], 
                                    obj_func[obj_func[,4] == min(obj_func[,4]),3][1],min(obj_func[,4])))
    
    ## plot the best fit
    # par(mfrow = c(3,3))
    # plot(observed_biomass[,2],observed_biomass[,3],xlim = c(1,12), ylim = c(0,2500),
    #      main = paste('year', as.numeric(unique(multi_biom[,1]))[k], 'scale = ',
    #                   min_obj[k,2],'shift = ', min_obj[k,3]), xlab = 'month', ylab = 'biomass')
    # lines(mean_shape_curve[,1],
    #       (mean_shape_curve[,2] - min(mean_shape_curve[,2]))* min_obj[k,2] + min_obj[k,3])
  }
  
  # create an artificial timeseries with gaps filled by fitted curves above
  artificial_ts <- c()
  for(k in 1:34){artificial_ts <-rbind(artificial_ts,
                                       cbind(as.numeric(unique(multi_biom[,1]))[k],1:12,(mean_shape_curve[,2] - min(mean_shape_curve[,2]))* min_obj[k,2] + min_obj[k,3]))}
  artificial_ts
  # compare with real data
  par(mfrow=c(2,1))
  plot(main = 'Wassaw with mean shape', multi_biom[,2]+12*(multi_biom[,1]-1984),multi_biom[,3], xlim = c(1,408), 
       ylim = c(0,2200), pch = 16, xlab = 'Months(1984-2018)', ylab = 'Biomass(g/m2)')
  lines(artificial_ts[,2]+12*(artificial_ts[,1]-1984),artificial_ts[,3], col = 'red')
  abline(v = c(337,348), lty = 2)
  text(342,2000, 'no data')
  legend('topright', lty = c(NA,1), pch = c(16,NA), col = 1:2, legend = c('data','fit'))
  
  return(artificial_ts)
  ## can be prepared for each site (instead of class that I did in previous dataset, currently focus on statistical methods)
  # min_obj <- cbind(cls,min_obj)
  # colnames(min_obj) <- c('class','year','scale','shift','minimum_error')
}
####################################################################################
### Kadir Bice, University of Georgia, 2020
### This script demonstrates alternative time series gap filling approaches (mostly seasonal series) that fits different needs
### Included approaches are:
# Annual shape fitting (opt_fit.R): Basic approach to fit annual shapes by minimizing error for each year to fill the gaps

library(tidyverse)
library(astsa)
library(ggfortify)
library(lubridate)
library(zoo)
library(reshape2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# options(noaakey = "ksqzZJuiacOtnGaZJTLIXjdOcypMeTmb")
# 
# ncdc(datasetid = 'GSOM', stationid = 'GSOM:USW00014895', startdate = '1990-01-01',
#      enddate = '2000-01-01')
# lawn_bbox_polygon(c(-122.2047, 47.5204, -122.1065, 47.6139)) %>% view
# stations <- ncdc_stations(extent = c(47.5204, -122.2047, 47.6139, -122.1065))
# 
# dummy <- ncdc(datasetid='GSOM', startdate = '2013-10-01', enddate = '2013-12-01',
#               stationid = "GHCND:AE000041196")
# 
# dummy$data %>%
#   filter(datatype == 'PRCP') %>%
#   select(date, value) %>%
#   mutate(time = as.POSIXct(date)) %>%
#   ggplot(aes(time, value)) 

data(list = c('salmon','gas','oil','cardox'))
autoplot(ts.union(oil,gas), facets = F)

data(list = c('unemp','birth'))
birth <- birth[-length(birth)]

# as.yearmon(time(birth))
# as.Date.yearmon(time(birth))

# autoplot(birth)
salmon
input_ts <- unemp
unemp_data <- data.frame(year = year(as.Date(time(input_ts))), month = month(as.Date(time(input_ts))),
       value = as.numeric(input_ts))
ggplot(unemp_data, aes(1:length(input_ts), value)) + geom_line()
############ optimized fit (this is an alternative to cover the gaps)
## introduce some missing points
unemp_data$value[as.integer(runif(length(input_ts)/2, min = 1, max = length(input_ts)))] <- NA
ggplot(unemp_data, aes(1:length(input_ts), value)) + geom_line()

source('opt_fit.R')
artificial_ts <- opt_fit(unemp_data)

# checkups
length(as.numeric(unique(unemp_data[,1])))
max(unemp_data[,3], na.rm = T)/min(unemp_data[,3], na.rm = T)

artificial_ts[[1]] %>%
  mutate(index = 1:nrow(artificial_ts[[1]]), 
         gapped = c(unemp_data$value, rep(NA,(nrow(artificial_ts[[1]])-length(unemp_data$value)))),
         artificial = value) %>%
  select(index, artificial, gapped) %>%
  gather(key = 'metric', value = 'value', -index) %>%
  ggplot(aes(x = index, y = value)) + geom_line() + 
    geom_point(data = data.frame(x = 1:(length(input_ts)), y = input_ts), aes(x = x, y = y), col = 2, size = 0.7) +
    facet_wrap(~metric, nrow = 2) 
    
length(unemp_data$value)
nrow(artificial_ts[[1]])
length(c(unemp_data$value, rep(NA,(nrow(artificial_ts[[1]])-length(unemp_data$value)))))

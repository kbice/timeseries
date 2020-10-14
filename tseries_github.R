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

data(list = c('unemp','birth'))

input_ts <- birth
unemp_data <- data.frame(year = year(as.Date(time(input_ts))), month = month(as.Date(time(input_ts))),
       value = as.numeric(input_ts))
ggplot(unemp_data, aes(1:length(input_ts), value)) + geom_line()

## introduce some missing points
unemp_data$value[as.integer(runif(length(input_ts)/2, min = 1, max = length(input_ts)))] <- NA
ggplot(unemp_data, aes(1:length(input_ts), value)) + geom_line()

## source and run fitting script
source('opt_fit.R')
artificial_ts <- opt_fit(unemp_data)

# # checkups
# length(as.numeric(unique(unemp_data[,1])))
# max(unemp_data[,3], na.rm = T)/min(unemp_data[,3], na.rm = T)

## plot the resulting artificial time series
artificial_ts[[1]] %>%
  mutate(index = 1:nrow(artificial_ts[[1]]), 
         gapped = c(unemp_data$value, rep(NA,(nrow(artificial_ts[[1]])-length(unemp_data$value)))),
         artificial = value) %>%
  select(index, artificial, gapped) %>%
  gather(key = 'metric', value = 'value', -index) %>%
  ggplot(aes(x = index, y = value)) + geom_line() + 
    geom_point(data = data.frame(x = 1:(length(input_ts)), y = input_ts), aes(x = x, y = y), col = 2, size = 0.7) +
    facet_wrap(~metric, nrow = 2) 
    
# length(unemp_data$value)
# nrow(artificial_ts[[1]])
# length(c(unemp_data$value, rep(NA,(nrow(artificial_ts[[1]])-length(unemp_data$value)))))

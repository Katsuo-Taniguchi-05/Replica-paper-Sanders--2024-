###############################################
#
#      Generate Synthetic Choice Data for
#        an arbitrary store--product.
#
#        by Robert Evan Sanders, July 12, 2023
#
###############################################

####################################
### SET PATH and Open utilities ####
####################################

library(data.table)
library(zoo)
library(purrr)
library(tidyverse)

###################################
#Choose parameters for demand model
#and simulated dataset.
###################################

set.seed(3)

#Demand parameters
K = 3                                                #number of day-parts, must be greater than 1. 
beta = -2                                           #price sensitivity
alpha = rnorm(K,3,1)                                 #generate day-part varying intercepts to capture time-varying prefs
arrival_lambda_weekday = runif(K,min = 3, max = 8)/K   #mean arrivals per daypart on weekday
arrival_lambda_weekend = runif(K,min = 6, max = 14)/K   #mean arrivals per daypart on weekend

#Dataset parameters and simulation
N_days = 1000                                        #how many days do we see in the data?
productname = 'Bread 1'                              #name of bread product, only use one product. 
storename = 1                                        #store number, only use store. 
weekday_realization = sample(c('Monday','Tuesday','Wednesday','Thursday',
                               'Friday','Saturday','Sunday'),N_days,replace=T)  #simulate days of weeks uniformly
weekday_realzn_daypart = rep(weekday_realization,each=K)                        #setup simulation for arrivals
arrival_rates = rep(0,N_days*K)
arrival_rates[weekday_realzn_daypart%in%c('Saturday','Sunday')] <- arrival_lambda_weekend  #set corresponding arrival rates
arrival_rates[!weekday_realzn_daypart%in%c('Saturday','Sunday')] <- arrival_lambda_weekday #set corresponding arrival rates
actual_arrivals = rpois(length(arrival_rates),arrival_rates)                    #simulate arrivals per day-part for each day


price = round(runif(N_days, min = 1, max = 2),2)        # generate prices of the inside good.
arrivalsperday <- rollapply(actual_arrivals, width = K,
                            by = K, FUN = sum,
                            align = "left")             #get total trips per day, summed over dayparts

price = rep(price,arrivalsperday)                       #Let the price be the same for each day
new_price = rep(0, length(price) * 2)                   #add price of zero for the outside option.
new_price[seq(1, by = 2,
              length.out = length(price))] = price


weekday_realization_trip = rep(weekday_realization,arrivalsperday)  #Days of week for each trip
daypart_realization_trip = rep(rep(1:K,N_days),actual_arrivals)     #daypart for each trip
date_sim = rep(1:N_days,arrivalsperday)                             #day of arrival tracker

###################################
#Simulate Choice data.
###################################

#Setup the master panel.
syntheticdat = data.table('store'=storename,'date'=rep(date_sim,each=2),
                          'productN'=rep(1:2),'price'=new_price,
                          'productc'=rep(c(productname,'Outside')),
                          'weekday' = rep(weekday_realization_trip,each=2),
                          'daypart' = rep(daypart_realization_trip,each=2))

syntheticdat[,TripNumber:=rep(1:(nrow(syntheticdat)/2),each=2)]

# Create the columns dynamically
for(i in 1:K){
  syntheticdat[, paste0("dpt", i) := as.numeric(daypart == i)]
}

#Now simulate choices.
syntheticdat[,alpha_trip:=0]
syntheticdat[,beta_trip:=beta]
for(i in 1:K){
  syntheticdat[daypart==i, alpha_trip:= alpha[i]]
}

syntheticdat[,choice_probability:=exp(alpha_trip+beta_trip*price)/(1+exp(alpha_trip+beta_trip*price))]
syntheticdat[,choice:=as.numeric(rbernoulli(nrow(syntheticdat),choice_probability))]
syntheticdat[productc=='Outside',choice:=NA_real_]
syntheticdat[,choice:=mean(choice,na.rm=T),by=.(TripNumber)]
syntheticdat[,choice:=as.character(choice)]
syntheticdat[choice=="1",choice:=productname]
syntheticdat[choice=="0",choice:='Outside']


syntheticdat[,alpha_trip:=NULL]
syntheticdat[,beta_trip:=NULL]
syntheticdat[,choice_probability:=NULL]

######################################
#Save main synthetic data
######################################

setwd('../data')
write_rds(syntheticdat,file=paste0('synthetic_data.rds'))



#Save this because it's needed for the arrival estimate data.
#Need to know day of week for each trip. 
write_rds(weekday_realzn_daypart,file=paste0('synthetic_data_days.rds'))





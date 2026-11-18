#######################################################
#
#
#
#   Merge the demand preference and arrival
#   parameter draws into a single dataset
#   to be used by the supply side for optimization.
#
#   by Robert Evan Sanders, July 12, 2023
#
#######################################################

####################################
### SET PATH and Open utilities ####
####################################

library("data.table")
library("tidyverse")


####################################
### 
###    Read in demand estimates from 
###    demand_estimate.R
###    and from 
###    arrivals_estimate.R
###  
####################################

setwd('../output')

fulldat = read_rds(file='arrival_estimates_before_merge.rds')
thetadat = read_rds(file='demand_estimates_before_merge.RData')
load(file='demand_estimates_raw_objects.RData')

K = ncol(z)-1 #number of dayparts. 

thetadat=thetadat %>% slice(rep(row_number(), K))
thetadat=data.table(thetadat)
thetadat=thetadat[order(Product,store,draw)]
thetadat[,Period:=rep(1:K,nrow(thetadat)/K)]

thetadat=thetadat %>% slice(rep(row_number(), 2))
thetadat=data.table(thetadat)
thetadat=thetadat[order(Product,store,draw,Period)]
thetadat[,Weekend:=rep(0:1,nrow(thetadat)/2)]


setnames(fulldat,'r','draw')

masterprefs = merge(thetadat,fulldat,by=c('store','Product','draw','Period','Weekend'))
masterprefs=masterprefs[order(Product,store,draw,Weekend,Period)]


masterprefs[,alpha:=NULL]
masterprefs[,beta:=NULL]

setnames(masterprefs,'draw','r')
setnames(masterprefs,'Period','d')
setnames(masterprefs,'postdraw','lambda')
setnames(masterprefs,'store','location_name')
masterprefs[,location_id:=.GRP,by=.(location_name)]



####################################
### 
###    Save the final demand 
###    output to be used in
###    the supply side. 
###  
####################################

write_rds(masterprefs,file='masterprefs.rds')










#######################################################
#
#
#
#   Arrivals Estimation from Synthetic
#   Choice Model Data
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
#Read in synthetic data.
####################################

setwd('../data/')
testdata = read_rds('synthetic_data.rds')
weekday_realzn_daypart = read_rds(file=paste0('synthetic_data_days.rds'))


productname = testdata[productc!='Outside']$productc[1]

testdata[,Weekend:=as.numeric(weekday%in%c('Saturday','Sunday'))]
setnames(testdata,'daypart','Period')

#Aggregate to daypart-weekday-store
testdata=testdata[,.(Arrivals=uniqueN(TripNumber)),by=.(date,Weekend,Period,store)][order(date,Period)]

#Create a balanced grid
temp = data.table(CJ(date=min((testdata$date)):max((testdata$date)),Period=unique(testdata$Period),store=unique(testdata$store)))



#Merge the arrivals in
testdata2=merge(testdata,temp,by=c('date','Period','store'),all=T)
testdata2[,newday:=weekday_realzn_daypart]
testdata2[,Weekend:=as.numeric(newday%in%c('Saturday','Sunday'))]
testdata2[,newday:=NULL]
testdata2 = testdata2[order(date,Period,store)]
testdata2[is.na(Arrivals),Arrivals:=0]

testdata2[Weekend==1,j:=.GRP,by=.(date)]
testdata2[Weekend==0,j:=.GRP,by=.(date)]

#####################################
#Setup the Bayesian Arrival Estimator
#####################################

#priors.
alpha = 0
beta = 0


load(file='../output/demand_estimates_raw_objects.RData')

#implied posteriors.
dat0 = testdata2[,.(alpha=sum(Arrivals),beta=max(j)),by=.(Weekend,Period)]
dat0=dat0[rep(1:.N,each=floor(end))]                   #Setup posterior draws from this distribution!
dat0[,r:=rep(1:floor(end),dim(dat0)[1]/floor(end))]
dat0[,store:=unique(testdata$store)]
dat0[,'Product':=productname]

set.seed(1)
dat0[,postdraw:=rgamma(1,alpha,beta),by=.(Weekend,Period,r)]


setwd('../output/')
write_rds(dat0,file='arrival_estimates_before_merge.rds')










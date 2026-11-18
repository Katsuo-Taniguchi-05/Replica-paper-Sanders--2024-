#######################################################
#
#
#
#   Demand Choice Parameter Estimation from Synthetic
#   Choice Model Data.
#
#   by Robert Evan Sanders, July 12, 2023
#
#######################################################

####################################
### SET PATH and Open utilities ####
####################################

library("data.table")
library("bayesm")

####################################
#Read in synthetic data.
####################################

setwd('../data/')
testdata = read_rds('synthetic_data.rds')

### SET UP ESTIMATION SAMPLE FOR ESTIMATION
productlist = unique(testdata$productc)
m = max(testdata$productN)
brands = m-1
obs = nrow(testdata)/m
prodind = rbind( diag(c(rep(1,m-1))) , c(rep(0,m-1)) )
X = cbind(testdata$price)
X = cbind( kronecker( matrix(c(1),obs,1) , prodind ) , X)		# add the product dummy variables
label = c( rep(c(""),brands) , "price")
for (rr in 1:brands) {
  label[rr] = as.vector(testdata$productc[rr])
}
testdata[,choicedummy:=0]
testdata[productc==choice,choicedummy:=1]

colnames(X) = label
choice = kronecker(matrix(1,obs,1), as.matrix(1:(m)))[ testdata$choicedummy==1]
trip = testdata$TripNumber


#Add in the daypart part of the design matrix.
for(i in 1:max(testdata$daypart)){
  testdata[, paste0("dpt", i,"I") := get(paste0("dpt", i))]
}
for(i in 1:max(testdata$daypart)){
  testdata[productN==m, paste0("dpt", i,"I") := 0]
}

if(max(testdata$daypart)>1){
prefix = 'dpt'
range = 2:max(testdata$daypart)
cols_to_bind <- lapply(range, function(i) testdata[[paste0(prefix, i, "I")]])
names(cols_to_bind) <- paste0(prefix, range)
X2 <- do.call(cbind, cols_to_bind)
X = cbind(X,X2)
}

###################
#### SETTINGS #####
###################

ncoef = ncol(X)
nlgt = length(unique(testdata$TripNumber))		# number trips

############################################
##### ESTIMATE HOMOGENEOUS LOGIT
##### This is just your standard
#####	Logit Model,  but with time-varying
#####	intercepts
############################################


##Controls
R = 800000       # number MCMC draws
keep = 600       # interval at which to retain MCMC draws for posterior calculation
burnin = 200000  #the burn in
Mcmc1 = list(R=R,keep=keep)


### SET UP THE DATA ####
##Homogeneous, so just pool everyone.
yy=testdata[testdata$choicedummy==1,]$productN
xx=X


Data1 = list(m=m,p=m,y=yy,X=xx)
out1 = rmnlIndepMetrop(Data=Data1,Mcmc=Mcmc1)

############################################
##### Save the output 
##### and use objects for the
##### dynamic supply-side estimation. 
############################################

begin = (Mcmc1$R-burnin)/Mcmc1$keep
end = Mcmc1$R/Mcmc1$keep
ll =out1$loglike[begin:floor(end)]
logML = logMargDenNR( ll )
q2 = quantile(ll,prob=.05)
q98 = quantile(ll,prob=.95)
logMLtrim = logMargDenNR( ll[ll>q2 & ll<q98] )
z = head(X,m)
coefnames = colnames(z)
prices = testdata[,.(price=median(price)),by=.(productc)]
z[,m]= prices$price



############################################################
#
# Post-Process the Demand Parameters
# for homogeneous logit.
# 
# First, Make the intercept estimates
# in levels (not differences
# relative to the base intercept
# of daypart 1),
#
# Second, save the highest estimated
# choice share of the inside good 
# so that we can calibrate our supply
# side state-space well without 
# wasting computational resources. 
# This share tells us the most we can expect
# to sell in a single day if prices are
# toward their minimum. 
#
############################################################
thetadat = data.table()
obsdata=data.table()
R=dim(out1$betadraw)[1]
drawtheta = data.table(out1$betadraw[1:R,])

if(max(testdata$daypart)>1){
	for(i in 2:max(testdata$daypart)){
		drawtheta[,paste0('V',1+i):=V1+get(paste0('V',1+i))]
		colnames(drawtheta)[1+i]=paste0('Intercept_',i)
	}
}

colnames(drawtheta)[1]='Intercept_1'
colnames(drawtheta)[2]= 'Price'
drawtheta[,draw:=1:R]
drawtheta[Price>0,Price:=0]

proddds = testdata[productN==1]$productc[1]
loc = testdata[productN==1]$store[1]
drawtheta[,store:=loc]
drawtheta[,Product:=proddds]

#Calculate Observed Share at observed Prices
obs = testdata[productc!='Outside',.(ob=.N),by=.(price,choice,daypart)]
obs[,tot:=sum(ob),by=.(price,daypart)][,s:=round(ob/tot,3)]
obs = obs[choice==proddds][order(price,daypart)]
obs[,store:=loc]
setnames(obs,'choice','Product')
setnames(obs,'price','price')

#Calculate Predicted Share at observed Prices, will use this in supply-side for initializing
#a big-enough state space grid. 
obsHAT=data.table()
for(kkk in unique(obs$price)){
  
  tempoary = copy(drawtheta)
  tempoary[,price:=kkk]
  tempoary[,s1hat:=exp(Intercept_1+price*Price)/(1+exp(Intercept_1+price*Price))]

  if(max(testdata$daypart)>1){
		  for(dd in 2:max(testdata$daypart)){
			  tempoary[,paste0('s',dd,'hat'):=exp(get(paste0('Intercept_',dd))+price*Price)/(1+exp(get(paste0('Intercept_',dd))+price*Price))]
		 }
		
  }
  
	expressions <- lapply(1:max(testdata$daypart), function(i) {
			parse(text=paste0("`", i, "`=round(mean(s", i, "hat),3)"))
			})
			
	# Collapse the expressions into a single call
	expressions <- as.call(c(list(as.name("list")), unlist(expressions, recursive=FALSE)))
	
	# Evaluate the expressions in the data.table
	tempoary <- tempoary[, eval(expressions), by=price]

	# Set the column names
	setnames(tempoary, paste0("V", 1:max(testdata$daypart)), as.character(1:max(testdata$daypart)))
		
  tempoary=melt(tempoary,id.vars='price', measure.vars=as.character(1:max(testdata$daypart)),
                variable.name = "daypart", value.name = "s_hat")
  tempoary[,daypart:=as.numeric(daypart)]
  obsHAT = rbind(obsHAT,tempoary)
  
  
}
 
obs = merge(obs,obsHAT,by=c('daypart','price'))
thetadat = drawtheta
obsdata = copy(obs)
setcolorder(thetadat, c("draw",colnames(thetadat)[!(colnames(thetadat) %in% c("draw"))]))
setcolorder(thetadat, c("store",colnames(thetadat)[!(colnames(thetadat) %in% c("store"))]))
setcolorder(thetadat, c("Product",colnames(thetadat)[!(colnames(thetadat) %in% c("Product"))]))
obsdata[,error:=abs(s-s_hat)]
obsdata[,MinP:=min(price),by=.(Product,store)]
bounds = obsdata[price==MinP,.(s_hat=mean(s_hat)),by=.(Product,store)]
 
thetadat = merge(thetadat,bounds,by=c('Product','store'))

setwd('../output/')
save(out1,logMLtrim,z,prices,sample,m,nlgt,begin,end,testdata,file='demand_estimates_raw_objects.RData')
write_rds(thetadat,file='demand_estimates_before_merge.RData')




















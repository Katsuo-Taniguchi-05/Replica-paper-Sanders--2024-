###########################################################################################
#
#                             consumersurplusfunction.R 
#
#
#
#  inputs: 
#             1. Expected Probability-action space for all Q and for all locations. 
#                 (output from expecteddemandDist.R)
#
#             2. locationid: The above item is for all stores. 
#                   This location refers to the store id (the field called 'location_id').  
#                   This subsets the above data table and returns the expected reward
#                    data for only one store (we allow store specific coefficients 
#                    which is why this is important.)  
#
#             3. parameters of DP  (parms) 
#                   (including) 
#                   
#                    a. statecols: list a character vector of the state names that affect preferences, 
#                                 for example, if preferences vary by weekday and daypart, input c('w','d'). 
#                
#                    b. tolerance: drop all probability vectors less than this tolerance, the idea being 
#                                  that they probably don't affect the expectation.  Keep it =0 if you 
#                                  want to keep all vectors to integrate over.    
#                
#                
#  Output:    
#             1. S-A pair, r(s,a) (expectation) 
#
#
#        Written by: Robert Evan Sanders
#        Date: July 12, 2023
#
#
#
############################################################################################


consumersurplus = function(prefparms,locid,parms,policycandidate){
	
	modeltype = parms$modeltype
  mem=parms$mem
  J = parms$J
  modeltype = parms$modeltype
  statecols = parms$statecols
  maxq = parms$maxq
  econometricunc = parms$econometricunc
  Tp = parms$Tp
  
	#Calculate for each draw and each day-weekday 
	
	#lambda, s, delta
  temp1 = prefparms[location_id==locid]
  nam = names(temp1)
  print(mem_used())

  #need to merge prefs with policy
  temp1=merge(temp1,policycandidate,by=c('d','w'))
  nam = names(temp1)
  
  #Create inventory for each state
  if(Tp==1){for(j in 1:J){temp1[,paste0('q_inv',j):=get(paste0('prod',j,'vint1'))+get(paste0('order',j))]}}
  if(Tp==2){for(j in 1:J){temp1[,paste0('q_inv',j):=get(paste0('prod',j,'vint1'))+get(paste0('prod',j,'vint2'))+get(paste0('order',j))]}}
  if(Tp==3){for(j in 1:J){temp1[,paste0('q_inv',j):=get(paste0('prod',j,'vint1'))+get(paste0('prod',j,'vint2'))+get(paste0('prod',j,'vint3'))+get(paste0('order',j))]}}
  if(Tp==4){for(j in 1:J){temp1[,paste0('q_inv',j):=get(paste0('prod',j,'vint1'))+get(paste0('prod',j,'vint2'))+get(paste0('prod',j,'vint3'))+get(paste0('prod',j,'vint4'))+get(paste0('order',j))]}}


	if(modeltype=='multinomiallogitpoisson'){
	
		temp1_prune = copy(temp1)
    nam = c("d","w","lambda",'beta1','pricecoef',paste0('q_inv',1:J),paste0('p',1:J,'s'),"state")
    				
    temp1_prune=temp1_prune[,..nam]
    emc = -digamma(1)

   	temp1_prune[,paste0('share',1):=exp(beta1 + pricecoef*p1s)/(1+exp(beta1 + pricecoef*p1s))]
		temp1_prune[,paste0('s0_',1):=emc-log(1+exp(beta1 + pricecoef*p1s))/exp(beta1 + pricecoef*p1s)]
		temp1_prune[is.nan(get(paste0('s0_',1))),paste0('s0_',1):=emc]
		temp1_prune[,paste0('s_',1):=emc+log(1+exp(beta1 + pricecoef*p1s))]
		temp1_prune[,paste0('delta_',1):=get(paste0('s_',1))-get(paste0('s0_',1))]

	
		#replicate the table up to get correction term probabilities (see gumbelsim.R)
		temp1_prune[,index:=1:.N,by=state]	
		temp1_prune <- temp1_prune[rep(1:.N,(q_inv1+1))]
    temp1_prune[,j:=0:(.N-1),by=.(state,index)]
		

		temp1_prune[,paste0('prbuy_',1):=exp(-lambda*get(paste0('share',1)))*((lambda*get(paste0('share',1)))^(j))/factorial(j)]
		temp1_prune[,paste0('term_',1):=(q_inv1-j)*get(paste0('prbuy_',1))]
		temp1_prune[,paste0('correction_',1):=sum(get(paste0('term_',1))),by=.(state,index)]
		temp1_prune[,paste0('prbuy_',1):=NULL]
		temp1_prune[,paste0('term_',1):=NULL]
		
		temp1_prune=temp1_prune[, head(.SD, 1),by=.(state,index)]

		temp1_prune[,paste0('cs_',1):=-1*(1/pricecoef)*(lambda*get(paste0('s_',1)) + 
																										q_inv1*get(paste0('delta_',1)) - 
																										get(paste0('delta_',1))*lambda*(get(paste0('share',1))) - 
																										get(paste0('delta_',1))*get(paste0('correction_',1)))]
		
		eval=copy(temp1_prune)
    
    eval = eval[,.(state,cs_1)] 
    setnames(eval,'cs_1','cs')  
     
    }
    

	return(eval)
  
  
}
	
	












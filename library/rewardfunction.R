###########################################################################################
#
#                             rewardfunction.R 
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




returnmat = function(prHat,locationid,parms){
	
			
		#Keep only \Hat{Pr(Q|a) for a particular store.
		PrQmat=copy(prHat)[location_id==locationid]
		
		  minprices=parms$minprices
		  maxprices=parms$maxprices
		  a=parms$a
		  J=parms$J
		  Tp=parms$Tp
		  categoryvector=parms$categoryvector
		  K=parms$K
		  wmax=parms$wmax
		  pricerule=parms$pricerule
		  planningperiods=parms$planningperiods
		  ordercap=parms$ordercap
		  c=parms$c
		  gammaD = parms$gammaD                
		  gammaS = parms$gammaS            
		  Drawdown=parms$Drawdown
		  mem=parms$mem
		  modeltype=parms$modeltype  
		  statecols=parms$statecols             
		  maxq = parms$maxq                 
		  tolerance=parms$tolerance              
		  econometricunc = parms$econometricunc  
		  orderpolicy = parms$orderpolicy 
		  
		q = Vstatecreator(parms)
		SS = stateactioncreator(parms,q)
		As = SS$SA
		A = SS$AA
		
		stateaction=matrix(0,dim(As)[1],1)
		
		#Calculate Total Inventory For Each State for Each Product.
		
		if(orderpolicy=='endogenous'){
		  for(j in 1:J){
		
			cols = paste0('prod',j,'vint',1:parms$Tp)
			cols = c(paste0('prod',j,'vint',1:parms$Tp),paste0('order',j))
			As[, paste0('Itot',j) := Reduce(`+`, .SD), .SDcols=cols][]
				
			}
		}
	
		##########################################################
		#
		#
		#   Drop sufficiently low probabiity Q vectors.
		#
		#   e.g. Pr(Q|a) < 10^-8
		#    
		#   
		#
		#########################################################
		
		PrQmat=PrQmat[pQ>=tolerance]
		
		#Dont need the negative 1 for this part. Always has 0 mass. (Needed in calculating probability
		#transition matrix later on..
		
		for(j in 1:J){
		  PrQmat = PrQmat[get(paste0('Q',j))>=0]
		}
		
		x = colnames(As)
		y=colnames(PrQmat)
		ncols = length(unique(c(x,y)))
		nrows = as.numeric(max(As$anum))*as.numeric(max(As$state))*as.numeric(max(PrQmat$Q1+1)^J)
		totsizepred = as.numeric(nrows)*as.numeric(ncols)
		totmemorypred = totsizepred*4.7/1e+9
		if(mem<totmemorypred){
		  
		  cat(paste0("You are making very large demands upon your machine. You said that you have ", mem,
		             "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
		             round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
		             ,"or find a higher memory machine."))
		  
		  return()
		}
		
		As=merge(As,PrQmat,by=c(paste0('p',1:J,'s'),statecols),allow.cartesian = T,all.x=T)
		
		###############################################
		#
		#                 Revenue - prod cost
		#
		#
		#
		#
		###############################################
		
		for(j in 1:J){
			As[,paste0('rev',j):=	get(paste0('p',j,'s'))*pmin(get(paste0('Q',j)),get(paste0('Itot',j)))]
			As[,paste0('cost',j):=	as.numeric(d==1)*-c[j]*get(paste0('order',j))]
		}
		
		
		###############################################
		#
		#                 Stockout Cost
		#
		#
		#
		#
		###############################################
		
		
		for(j in 1:J){
		As[,paste0('stockoutcost',j):=	
				-gammaS*(pmax(get(paste0('Q',j))-get(paste0('Itot',j)),0))]
		}
		
		
		
		###############################################
		#
		#                 Disposal Cost
		#
		#
		#
		#
		###############################################
		
		##First get POST demand units of tau = 1 for each product
		
		for(j in 1:J){
		
		#If shelf-life is more than 1 day. 
		
		  if(Tp>1){
		    #sum up total inventory above tau=1	
		    nameabove = c(paste0('prod',j,'vint',Tp:2),paste0('order',j))
		    As[,paste0('totabove',j):= Reduce(`+`, .SD), .SDcols=nameabove]
		  
		    #waste cost
		    if(Drawdown=='LIFO'){
		      As[,paste0('waste',j):=0]
		      As[get(paste0('totabove',j))>=get(paste0('Q',j)),paste0('waste',j):=
		  		
		  			-as.numeric(d==K)*gammaD*
		  		(get(paste0('prod',j,'vint',1)))
		  		
		  		
		  		] 
		    As[get(paste0('totabove',j))<get(paste0('Q',j)),paste0('waste',j):=
		  		
		  		-as.numeric(d==K)*gammaD*
		  		pmax(0,get(paste0('totabove',j))+get(paste0('prod',j,'vint',1))-get(paste0('Q',j)))
		  		
		  	] 		
		  
		  }
		    if(Drawdown=='FIFO'){
		  	
		  	
		    As[,paste0('waste',j):=0] 	
		    As[get(paste0('prod',j,'vint',1)) >= get(paste0('Q',j)),paste0('waste',j):=
		  		
		  		-as.numeric(d==K)*gammaD*
		  		(get(paste0('prod',j,'vint',1))-get(paste0('Q',j)))
		  		
		  		]
		  	
		  }
		    
		  }
		
		#######################################################
		#If shelf-life is only 1 day, then it's only 1 vintage.
		  if(Tp==1){
		    
		    As[,paste0('waste',j):=0
		        
		        
		        ] 
		    
		    
		    As[(get(paste0('prod',j,'vint1'))>=get(paste0('Q',j))),paste0('waste',j):=
		          
		          -as.numeric(d==K)*gammaD*
		          ((get(paste0('Itot',j)))-get(paste0('Q',j)))
		        
		        
		        ]
		    
		  }
		#######################################################
		  
		  
		  	
		}	
		
		
		#########################################################
		#
		#                   Total Reward
		#
		#
		#
		#
		#########################################################
		
		names=c(paste0('stockoutcost',1:J),paste0('cost',1:J),
						paste0('rev',1:J),paste0('waste',1:J))
			
		As[,Rtotal:= Reduce(`+`, .SD), .SDcols=names]
		
		
		#########################################################
		#
		#                Infeasible actions get -infinity payoff
		#
		#
		#
		#
		#########################################################
		
		for(j in 1:J){
			
			As[get(paste0('Itot',j))>categoryvector[j],Rtotal:=-Inf]
		  As[get(paste0('order',j))>0 & d!=1,Rtotal:=-Inf]
			
			if(pricerule=='static'){
			
			  As[get(paste0('p',j,'s'))!=get(paste0('p',j,'sLAST')),Rtotal:=-Inf]	
			
			}
			
		}
		
		
		
		#########################################################
		#
		#                    Sum _q r(s,a,q)*Pr(q|a)
		#
		#
		#
		#
		#########################################################
		
		for(j in 1:J){
			
			As[,paste0('rev',j):=NULL]
			As[,paste0('cost',j):=NULL]
			As[,paste0('stockoutcost',j):=NULL]
			As[,paste0('waste',j):=NULL]
			if(Tp>1){As[,paste0('totabove',j):=NULL]}
		
		}
		
		
		
		As[,Rsa := pQ*Rtotal]
		As00 =copy(data.table(SS$SA))
		namelat = names(copy(As00))
		newmat=As[,.(rsa=sum(Rsa)),by=namelat]
		
		
		for(j in 1:J){
		  
		  newmat[,paste0('Itot',j):=NULL]
		  
		}
		
		
		newmat=newmat[order(state,anum)]
		
		return(list('Rsa'=newmat))

}




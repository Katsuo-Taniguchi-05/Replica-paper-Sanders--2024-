####################################################################################################
#
#                      This function gives you the expected transition probability matrix 
#                      from state to state, for each action. 
#                      
#                          expectation.R  
#         
#          Inputs: 
#
#
#               1.prHat - Expected Probability-action space for all Q and for all locations. 
#                 (output from expecteddemandDist.R)
#                 
#               2. locationid: The above item is for all stores. 
#                   This location refers to the store id (the field called 'location_id').  
#                   This subsets the above data table and returns the expected reward
#                    data for only one store (we allow store specific coefficients 
#                    which is why this is important.)
#
#               3. parameters of DP  (parms)  (including statecols) 
# 
#           
# 
#          Output:
#
#               1. p(s'|s,a)
#              
#               2. state-data.table
# 
#               3. action data.table
#
#
#         
#                                By Robert Evan Sanders, July 12, 2023 
# 
###################################################################################################


PAmats =function(prHat,locationid,parms){

		wmax=parms$wmax
		K = parms$K    
		J = parms$J                      
		Tp = parms$Tp                      
		a = parms$a                     
		minprices = parms$minprices       
		maxprices = parms$maxprices      
		categoryvector = parms$categoryvector
		pricerule = parms$pricerule       
		planningperiods = parms$planningperiods                 
		gammaD = parms$gammaD                
		gammaS = parms$gammaS          
		c=parms$c                  
		Drawdown=parms$Drawdown
		ordercap=parms$ordercap
    mem=parms$mem
    modeltype=parms$modeltype  
    statecols=parms$statecols             
    maxq = parms$maxq                 
    tolerance=parms$tolerance              
    econometricunc = parms$econometricunc  
    orderpolicy = parms$orderpolicy 
    if(orderpolicy=='exogenous'){
      basestock = parms$basestock}
    
		q = Vstatecreator(parms)
		As = stateactioncreator(parms,q)
		
		prHat=prHat[location_id==locationid]

		q[,V:=NULL]
		q2 = copy(q)
		names(q2) = paste0(names(q),'prm')
		
		
		q = As$SA
		
		totsizepred = (as.numeric(dim(q)[1])*as.numeric(dim(q2)[1]))*as.numeric(dim(q)[2] + dim(q2)[2])
		totmemorypred = totsizepred*4.7/1e+9
		if(mem<totmemorypred){
		  
		  cat(paste0("You are making very large demands upon your machine. You said that you have ", mem,
		             "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
		             round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
		             ,"or find a higher memory machine."))
		  
		  return()
		}
		
		
	if(pricerule=='unrestricted'){ # & categoryvec>25

		#not beginning of day
		qnoend=copy(q)[d!=1 & order1==0]
		qnoend2=copy(q2)[dprm!=2]
		pfill_a=qnoend[, k:=t+1][qnoend2[, k:=tprm], on=.(k), nomatch=0L,allow.cartesian=TRUE][,k:=NULL]

		#beginning of day
		qend=copy(q)[d==1]
		qend2=copy(q2)[dprm==2]
		pfill_b=qend[, k:=t+1][qend2[, k:=tprm], on=.(k), nomatch=0L,allow.cartesian=TRUE][,k:=NULL]
		pfill=rbind(pfill_a,pfill_b)
 }
		
		
		###############################################
		# For restricted pricing (static or decreasing), 	
		# only certain price transitions are impossible:
		###############################################
		print(mem_used())
		############
		# Static
		############
		if(pricerule=='static'){
			
			
		#not beginning of day
		qnoend=copy(q)[d!=1 & order1==0]
		qnoend2=copy(q2)[dprm!=2]
		pfill_a=qnoend[, k:=t+1][qnoend2[, k:=tprm], on=.(k), nomatch=0L,allow.cartesian=TRUE][,k:=NULL]

		#beginning of day
		qend=copy(q)[d==1]
		qend2=copy(q2)[dprm==2]
		pfill_b=qend[, k:=t+1][qend2[, k:=tprm], on=.(k), nomatch=0L,allow.cartesian=TRUE][,k:=NULL]
		pfill=rbind(pfill_a,pfill_b)
			
		}
		print(mem_used())
		

		pfill[,ssp:=1:dim(pfill)[1]] #ssp is a state-action to state 
		
		pcopy=copy(pfill)
		
		
#####################################################		
#      
#      
#      
#      
#      
#      Endogenous Capacity State Adjustments 
#      
#     
#     
#
#####################################################
		
if(orderpolicy=='endogenous'){		
				
		##############################
		#Broad Inventory Restrictions,
		#         by Tp.
		##############################
		
		#Part 1. if it is anything except the last period, 
		#inventory must elementwise weakly decrease in the next period (relative to post order
		#current inventory state)
		#
		#
		#Part 2. if it is the last period, all inventory for the newest vintages
		#must transition to 0 and inventory for other vintages must be 
		#weakly less than the vintage above it, or if K==1, the order that period. 
		#
		#zero out all others! 

		##########################################
		#     
		#     define post ordering states 
		#
		##########################################
		for(j in 1:J){
		  print(mem_used())
		  pfill[,paste0('prod',j,'vint',Tp,'post'):=get(paste0('prod',j,'vint',Tp))+get(paste0('order',j))]
		  
		}
		if(Tp>1){
		for(j in 1:J){
		  for(tau in 1:(Tp-1)){
		  pfill[,paste0('prod',j,'vint',tau,'post'):=get(paste0('prod',j,'vint',tau))]
		  }
		}
		}
		
		
		#Part 1
		for(j in 1:J){
		  for(tau in 1:Tp){
		    
		    pfill=pfill[(d==K) |
		                  
		                  (d!=K & get(paste0('prod',j,'vint',tau,'prm'))<=get(paste0('prod',j,'vint',tau,'post')))]
		    
		  }
		}

	  #Part 2.
		if(K==1){
		  
		  for(j in 1:J){
		    
		    if(Tp==1){
		      #New inventry = 0 next period. 
		      pfill=pfill[get(paste0('prod',j,'vint',Tp,'prm'))==0]
		    }
		    print(mem_used())
		    if(Tp>1){
		      
		      #New inventry = 0 next period. 
		      pfill=pfill[get(paste0('prod',j,'vint',Tp,'prm'))==0]
		  
		      for(tau in 1:(Tp-1)){
		             
		        pfill=pfill[get(paste0('prod',j,'vint',tau,'prm'))<=get(paste0('prod',j,'vint',tau+1,'post'))]
		            
		       }
		       
		     }
		      
		      
		    }

		  }
		if(K>1){
		
		  for(j in 1:J){
  		  if(Tp==1){
  		  
  		         
  		     pfill=pfill[((d==1) | (d!=1 & d!=K) |
  		                    
  		                  #New inventory next period = 0
  		                 (d==K & get(paste0('prod',j,'vint',Tp,'prm'))==0))]
  		  
  		  }
  		  if(Tp>1){
  		    for(tau in 1:(Tp-1)){
  		      
  		              pfill=pfill[((d==1) | (d!=1 & d!=K) |
  		                   
  		                   #New inventory next period = 0 
  		                   (d==K & get(paste0('prod',j,'vint',Tp,'prm'))==0
  		                    
  		                    #all other products have to have less than vintage above. 
  		                    &  get(paste0('prod',j,'vint',tau,'prm'))<=get(paste0('prod',j,'vint',tau+1,'post')))
  		                 
  		    )]
  		    }
  		  }
		  }

		  print(mem_used())
		}
		

		#########################################
		#Capacity Inventory Restrictions
		#########################################
		
		#the probability of ILLEGAL actions does not actually matter,
		#because the payoff is negative infinity so they'll never be chosen. 
		#so focus only on the subset of legal. 
		
		#cannot order outside beginning of day
		#and you cant order more than the total capacity
		
		for(j in 1:J){
		  
		  cols = paste0('prod',j,'vint',1:Tp)
		  cols = c(paste0('prod',j,'vint',1:Tp),paste0('order',j))
		  pfill[, paste0('Itot',j) := Reduce(`+`, .SD), .SDcols=cols]
		  
		}
		
		for(j in 1:J){
		  pfill=pfill[(get(paste0('order',j))==0 & d!=1) | (d==1)]
		  pfill=pfill[(get(paste0('Itot',j))<=categoryvector[j])]
		}
		

		#########################################
		#Drawdown-Specific Inventory Restrictions
		#########################################
		
		#get inventory in the next period, pre-order
		for(j in 1:J){
		  
		  cols = paste0('prod',j,'vint',1:Tp,'prm')
		  pfill[, paste0('Itot',j,'prm') := Reduce(`+`, .SD), .SDcols=cols]
		  
		}
		
		#Next period you cant have more total inventory than the capacity 
		#before ordering.
		for(j in 1:J){
		  print(mem_used())
		  pfill=pfill[(get(paste0('Itot',j,'prm'))<=categoryvector[j])]
		}
		
		#you can never have more pre-order total inventory in the next period 
		#than you do in the postorder inventory of this period
		for(j in 1:J){
		  
		  pfill=pfill[get(paste0('Itot',j)) >= get(paste0('Itot',j,'prm'))]	
		  
		  
		}
		
		#####################################################
		#
		#for LIFO (FIFO), post-ordering, if d!=K, then each new
		#inventory must be a valid LIFO (FIFO) transformation of the
		#inventory in the current period. 
		#
		#if d==K, then: 
		#
		#     InvPrm[1:Tp-1] must be a valid LIFO (FIFO) transform of
		#    
		#     Inv[2:Tp]
		#
		#
		#These must hold true product-wise forall products. 
		#
		#####################################################
		
		#how-to-check-whether-a-vector-is-lifo-decreasing
		
		#to later deal with the d==K case, it's useful to define new columns
		#that line up the post-decay comparisons. That is, given 
		#   A B C  ---> D E F 
		#    
		# We want to make sure B C to D E is LIFO (FIFO).  
		
		if(Tp>1){
		  for(j in 1:J){
	  	for(tau in 1:(Tp-1)){
		
	  	    pfill[,paste0('prod',j,'vint',tau,'postDK'):=get(paste0('prod',j,'vint',tau+1,'post'))]
	  	    pfill[,paste0('prod',j,'vint',tau,'DKprm'):=get(paste0('prod',j,'vint',tau,'prm'))]
	  	 }
		  }
		
		
		  print(mem_used())
		
		for(j in 1:J){
		  
		  #d!=K
		  cols = c('ssp',paste0('prod',j,'vint',1:Tp,'post'),paste0('prod',j,'vint',1:Tp,'prm'))
		  temp = pfill[d!=K,..cols]
		  tbl <- melt((temp), meas=patterns(paste0("^prod",j,"vint[1-9]post$"), "prm$"), 
		              value.name=c("bef","aft"))
		  
		  if(Drawdown=='FIFO'){
		    setorder(tbl, ssp, variable) 
		    }
		  if(Drawdown=='LIFO'){
		    setorder(tbl, ssp, -variable) 
		  }
		  print(mem_used())
		  tbl = data.table(tbl)
		  tbl[,dif:=(sum(bef) - sum(aft)),by=ssp]
		  tbl[,flifo:=(pmin(pmax(cumsum(bef) - dif, 0), bef)),by=ssp]
		  tbl[,keep:=as.numeric(identical(as.numeric(flifo), as.numeric(aft))),by=ssp]
		  keep0=unique(tbl[keep==1,ssp])
		  temp=data.table(temp)
		  temp[ , paste0('keepit',j) := 0]
		  temp[ssp%in%keep0,paste0('keepit',j):=1]
		  temp0 = copy(temp)
		  
		  
		  #d==K
		  
		  cols = c('ssp',paste0('prod',j,'vint',1:(Tp-1),'postDK'),paste0('prod',j,'vint',1:(Tp-1),'DKprm'))
		  temp = pfill[d==K,..cols]
		  tbl <- melt((temp), meas=patterns(paste0("^prod",j,"vint[1-9]postDK$"), "DKprm$"), 
		              value.name=c("bef","aft"))
		  if(Drawdown=='FIFO'){
		    setorder(tbl, ssp, variable) 
		  }
		  if(Drawdown=='LIFO'){
		    setorder(tbl, ssp, -variable) 
		  }
		  tbl = data.table(tbl)
		  tbl[,dif:=(sum(bef) - sum(aft)),by=ssp]
	
	    tbl[,flifo:=(pmin(pmax(cumsum(bef) - dif, 0), bef)),by=ssp]
	    tbl[,keep:=as.numeric(identical(as.numeric(flifo), as.numeric(aft))),by=ssp]

		  keep0=unique(tbl[keep==1,ssp])
		  temp=data.table(temp)
		  temp[ , paste0('keepit',j) := 0]
		  temp[ssp%in%keep0,paste0('keepit',j):=1]
		  temp1=copy(temp) 
		  print(mem_used())
		  cols = c('ssp',paste0('keepit',j))
		  temp2 = rbind(temp0[,..cols],temp1[,..cols])
		  
		  
		  pfill = merge(pfill,temp2,by='ssp',all.x=T)
		}
		
		  print(mem_used())
  	for(j in 1:J){
  	  
  	pfill = pfill[get(paste0('keepit',j))==1]  
  	  
  	}
		}
}		


####################################################
#
#
#
#
#		
#    Get the implied demand for each action state
#
#
#
#
#
#
###################################################

	###########################################
	#
	#
	#      For each transition, there is a SET
	#      of implied Q's.
	#
	#      
	#
	###########################################
		
	for(j in 1:J){
	  pfill[,
	        paste0('Q',j,'impliedlb'):=0.00001
	        ]
	  print(mem_used())
	  pfill[,
	        paste0('Q',j,'impliedub'):=0.00001
	        ]
	  
	  pfill[,
	       paste0('Itot',j):=as.numeric(get(paste0('Itot',j)))
	        ]
	  pfill[,
	        paste0('Itot',j,'prm'):=as.numeric(get(paste0('Itot',j,'prm')))
	        ]
	
	}  
		
		#################	
		#d<K
		#################
		#Case 1: Total inventory is the same in the next period, and not 0. 
		#        then demand must have been [0,0]. 
		 
		for(j in 1:J){
		      
		pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))!=0),
		      paste0('Q',j,'impliedlb'):=0
		      ]
		pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))!=0),
		      paste0('Q',j,'impliedub'):=0
		      ]
		print(mem_used())

		#Case 2: Inventory now and next period is 0.  Demand must be: [0,Inf]
		     
		pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))==0),
		      paste0('Q',j,'impliedlb'):=0
		      ]
		pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))==0),
		      paste0('Q',j,'impliedub'):=Inf
		      ]
		
		
		 #Case 3: Inventory now is positive. 
		 #        Next period, inventory is less than what it is now, but still not zero. 
		 #        Demand must have been [Itot-Itotprm, Itot-Itotprm]
	
		
		pfill[(d<K) & (get(paste0('Itot',j))>(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))) & 
		        ((get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))>0),
		      paste0('Q',j,'impliedlb'):=(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))
		      ]
		      
		pfill[(d<K) & (get(paste0('Itot',j))>(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))) & 
		        ((get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))>0),
		      paste0('Q',j,'impliedub'):=(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))
		      ]
		
		 #Case 4:  Inventory is positive currently, and 0 the next period.
		 #         This means demand was [Itot,Inf]
		
		
		pfill[(d<K) & (get(paste0('Itot',j))>(0)) & ((get(paste0('Itot',j,'prm')))==0),
		      
		      paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))
		      
		      ]
		
		pfill[(d<K) & (get(paste0('Itot',j))>(0)) & ((get(paste0('Itot',j,'prm')))==0),
		      
		      paste0('Q',j,'impliedub'):=Inf
		      
		      ]
}
		
		##################
		#d==K
		##################
		#
		#This part must be broken up by LIFO and FIFO.
		#(Note that next period always considered BEFORE 
		#order inventory.)
		#
		############
		#LIFO
		############
		#
		#Case 1. If all vintages shift one down
		#        and Itot-prodjvint1 !=0
		#        then this means demand was:
		#        Demand: [0,0]
		#        Or in other words, Itotprm (before order) = Itot-prodjvint1
    #
		#Case 2. If Itot-prodjvint1 !=0
		#        and Itotprm (before order) = 0 next period 
		#        then this means demand was:
		#        Demand: [Itot-prodjvint1,Inf]
		#
		#Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
		#        This means that some demand was drawn down.
		#        Demand: [Itot-prodjvint1 - Itotprm,Itot-prodjvint1 - Itotprm]
		#
    #
		#Case 4. If Itot-prodjvint1 = 0
		#       Demand: [0,Inf]
		#
		############
		#FIFO
		############
		#
		#Case 1. If all vintages shift one down
		#        and Itot-prodjvint1 !=0
		#        then this means demand was:
		#        Demand: [0,prodjvint1]
		#        Or in other words, Itotprm (before order) = Itot-prodjvint1
		#
		#Case 2. If Itot-prodjvint1 !=0
		#        and Itotprm (before order) = 0 next period 
		#        then this means demand was:
		#        Demand: [Itot,Inf]
		#
		#Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
		#        This means that some demand was drawn down.
		#        Demand: [Itot - Itotprm, Itot - Itotprm]
		#
		#
		#Case 4. If Itot-prodjvint1 = 0
		#        Demand: [0,Inf]
		#
		
		for(j in 1:J){		
		  if(Tp==1){
		  
		  for(j in 1:J){
		      
		    if(K==1){
		    pfill[,paste0('Q',j,'impliedlb'):=0]
		    pfill[,paste0('Q',j,'impliedub'):=Inf]
		    }
		    if(K>1){
		      print(mem_used())
		      pfill[(d==K), paste0('Q',j,'impliedlb'):=0]  
		      pfill[(d==K), paste0('Q',j,'impliedub'):=Inf] 
		      
		    }
		  }
		 }
		  if(Tp>1){
		    
		    
		    if(Drawdown=='LIFO'){
		      

		      
		      #Case 1. If all vintages shift one down
		      #        and Itot-prodjvint1 !=0
		      #        then this means demand was:
		      #        Demand: [0,0]
		      #        Or in other words, Itotprm (before order) = Itot-prodjvint1
		      ###########################
		      # CASE 1
		      ###########################
		      ## set some variable names
		      pre <- paste0("prod",j,'vint', 2:(Tp),'post')
		      pst <- paste0("prod",j,'vint', 1:(Tp-1),'prm')
		     
		      pfill[,paste0('nonexpInv',j):=(get(paste0('Itot',j))-get(paste0('prod',j,'vint',1)))]
		      ## select where they match using `==` and Reduce to combine with a `&` (logical AND)
		      pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
		             & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedlb'):=0]
		      pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
		             & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedub'):=0]
		      
		      #Case 2. If Itot-prodjvint1 !=0
		      #        and Itotprm (before order) = 0 next period 
		      #        then this means demand was:
		      #        Demand: [Itot-prodjvint1,Inf]
		      ###########################
		      # CASE 2
		      ###########################
		      ## set some variable names
		    
		      pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
		              (get(paste0('Itot',j,'prm')))==0), 
		               paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))-get(paste0('prod',j,'vint',1))]
		      
		      pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
		              (get(paste0('Itot',j,'prm')))==0), paste0('Q',j,'impliedub'):=Inf]
		      
		     
		      #Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
		      #        This means that some demand was drawn down.
		      #        Demand: [Itot-prodjvint1 - Itotprm,Itot-prodjvint1 - Itotprm]
		      ###########################
		      # CASE 3
		      ###########################
		      pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
		               
		               (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
		            
		            paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))-get(paste0('prod',j,'vint',1))-get(paste0('Itot',j,'prm'))]
		      
		      pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
		               
		               (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
		            
		            paste0('Q',j,'impliedub'):=get(paste0('Itot',j))-get(paste0('prod',j,'vint',1))-get(paste0('Itot',j,'prm'))]
		      
		      print(mem_used())
		      #Case 4. If Itot-prodjvint1 = 0
		      #       Demand: [0,Inf]
		      ###########################
		      # CASE 4
		      ###########################
		      pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
		            
		            paste0('Q',j,'impliedlb'):=0]
		      
		      pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
		             
		             paste0('Q',j,'impliedub'):=Inf]
		  
		  
		  
		    }
		    if(Drawdown=='FIFO'){
		  		  
		  		  
		  		  #Case 1. If all vintages shift one down
		  		  #        and Itot-prodjvint1 !=0
		  		  #        then this means demand was:
		  		  #        Demand: [0,prodjvint1]
		  		  #        Or in other words, Itotprm (before order) = Itot-prodjvint1
		  		  ###########################
		  		  # CASE 1
		  		  ###########################
		  		  ## set some variable names
		  		  pre <- paste0("prod",j,'vint', 2:(Tp),'post')
		  		  pst <- paste0("prod",j,'vint', 1:(Tp-1),'prm')
		  		  
		  		  pfill[,paste0('nonexpInv',j):=(get(paste0('Itot',j))-get(paste0('prod',j,'vint',1)))]
		  		  ## select where they match using `==` and Reduce to combine with a `&` (logical AND)
		  		  pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
		  		         & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedlb'):=0]
		  		  pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
		  		         & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedub'):=as.numeric(get(paste0('prod',j,'vint',1)))]
		  		  
		  		  print(mem_used())
		  		  
		  		  #Case 2. If Itot-prodjvint1 !=0
		  		  #        and Itotprm (before order) = 0 next period 
		  		  #        then this means demand was:
		  		  #        Demand: [Itot,Inf]
		  		  ###########################
		  		  # CASE 2
		  		  ###########################
		  		  pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
		  		           (get(paste0('Itot',j,'prm')))==0), 
		  		        paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))]
		  		  
		  		  pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
		  		           (get(paste0('Itot',j,'prm')))==0), paste0('Q',j,'impliedub'):=Inf]
		  		  print(mem_used())
		  		  #Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
		  		  #        This means that some demand was drawn down.
		  		  #        Demand: [Itot - Itotprm, Itot - Itotprm]
		  		  #
		  		  ###########################
		  		  # CASE 3
		  		  ###########################
		  		  pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
		  		           
		  		           (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
		  		        
		  		        paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))-get(paste0('Itot',j,'prm'))]
		  		  
		  		  pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
		  		           
		  		           (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
		  		        
		  		        paste0('Q',j,'impliedub'):=get(paste0('Itot',j))-get(paste0('Itot',j,'prm'))]
		  		  
		  		  #Case 4. If Itot-prodjvint1 = 0
		  		  #        Demand: [0,Inf]
		  		  #
		  		  ###########################
		  		  # CASE 4
		  		  ###########################
		  		  pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
		  		        
		  		        paste0('Q',j,'impliedlb'):=0]
		  		  
		  		  pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
		  		        
		  		        paste0('Q',j,'impliedub'):=Inf]
		  		  
		  		  print(mem_used())

		  		}
		  	
		 }
		}	
			
		
		
    		
    ################################################
    #
    #       Now, given the probability 
    #       of distribution as a function 
    #       of actions (or action state, in theory)
    #       , calculate the probability of transitioning
    #       from one state to the next
    #       
    #       
    ###############################################
    
    #Change the output into a CDF, because ultimately this is what 
    #we use because we have demand ranges		
    cols = paste0('p',1:J,'s')
    cols = c(statecols,cols)		
    
    if(J==1){prHat[,CDF := cumsum(pQ),by=cols]}
    
    

    cols=  c( paste0("p",1:J,'s'),statecols)
    
    print(mem_used())
    CDF1 = unlist(prHat$CDF)
    prHat[,CDF:=CDF1]
    
    #if your max probability is not close to 1, throw back an error.
    pre <- paste0("p",1:J,'s')
    pst <- minprices
    
    ## select where they match using `==` and Reduce to combine with a `&` (logical AND)
    maxpr=max(prHat[(Reduce(`&`, Map(`==`, mget(pre), pst))),CDF])
    if(maxpr<.999){
      print(maxpr)
      print("Warning: You need to add more possible Q's to the distribution creator.  You
            are not approximating the distribution very well.")
     
    }
    
    print(mem_used())
    #Because we cannot store infinite entires in this database., we 
    #set the maximum Q as the infinity in the pfill database. 
    maxQ = max(prHat$Q1)
    
    for(j in 1:J){
    pfill[get(paste0('Q',j,'impliedlb'))==Inf,paste0('Q',j,'impliedlb'):=maxQ]
    pfill[get(paste0('Q',j,'impliedub'))==Inf,paste0('Q',j,'impliedub'):=maxQ]
    }
    
    prHat[,pQ:=NULL]
    print(mem_used())
   
    
    ############################################
    #To calculate Pr(a <= X <= b), we need:
    #F(b) - F(a-1) because it is discrete
    #distribution. 
    #  
    #
    ############################################
    
 
    print(mem_used())
    priceA = copy(As$AA)
    priceA[,anum:=NULL]
    
    
    for(j in 1:J){
      
      
    pfill[,paste0(letters[J*(j-1)+1],'0'):=get(paste0('Q',j,'impliedlb'))]
    pfill[,paste0(letters[J*(j-1)+2],'0'):=get(paste0('Q',j,'impliedub'))]  
    if(orderpolicy=='endogenous'){priceA[,paste0('order',j):=NULL]}
    
    pfill[,paste0(letters[J*(j-1)+1],'0','minus1'):=get(paste0('Q',j,'impliedlb'))-1]
 
    pfill[,paste0('Q',j,'impliedlb'):=NULL]
    pfill[,paste0('Q',j,'impliedub'):=NULL]
    print(mem_used())
    
    }
    
    
    
    if(J==1){
      # F(b)     = 1 
      # F(a-1)   = 2
     
      colA = c('b0','a0minus1')
      nam = c('one','two')
      
      
      for(l in 1:2){ #four because there are 2 terms in a 1-d CDF with bounds.  
        
        assign(paste0("prHat",nam[l]),copy(prHat))
        get(paste0("prHat",nam[l]))[,colA[l]:=Q1]

        get(paste0("prHat",nam[l]))[,Q1:=NULL]

        get(paste0("prHat",nam[l]))[,paste0('CDF',nam[l]):=CDF] 
        get(paste0("prHat",nam[l]))[,CDF:=NULL] 
        
        get(paste0("prHat",nam[l]))[,location_name:=NULL]
        get(paste0("prHat",nam[l]))[,location_id:=NULL]
        
        
      }
      
      
      #MERGE AND CALCULATE STATE TRANITIONS:
      for(l in 1:2){
        mrgnam = colnames(get(paste0("prHat",nam[l])))
        mrgnam = mrgnam[-length(mrgnam)]
        pfill=merge(pfill,get(paste0("prHat",nam[l])),
                    by=mrgnam,all.x=T)
        
      }
      
      
      pfill[,prss:=CDFone - CDFtwo]
    
    }
    
  
    priceA = unique(priceA)
    
    
    #Now, the task is to build SxS for each A knowing that everything is sparse except the below!
		
		
		##########################################
		#   
		#   
		#   the aboves are all the nonzeroes!
		#
		#
		#
		##########################################
		
		
    pfill=pfill[,.(anum,state,stateprm,prss)]
	
    q = Vstatecreator(parms)
    As = stateactioncreator(parms,q)
    
return(list('datatablePA'=pfill,'q'=q,'A'=As$AA))
    

}
PAmats_with_policy =function(prHat,locationid,parms,policy){

		wmax=parms$wmax
		K = parms$K      
		J = parms$J                      
		Tp = parms$Tp                       
		a = parms$a                     
		minprices = parms$minprices       
		maxprices = parms$maxprices      
		categoryvector = parms$categoryvector
		pricerule = parms$pricerule       
		planningperiods = parms$planningperiods
		gammaD = parms$gammaD                
		gammaS = parms$gammaS          
		c=parms$c                  
		Drawdown=parms$Drawdown
		ordercap=parms$ordercap
    mem=parms$mem
    modeltype=parms$modeltype  
    statecols=parms$statecols             
    maxq = parms$maxq                 
    tolerance=parms$tolerance              
    econometricunc = parms$econometricunc  
    orderpolicy = parms$orderpolicy 
    if(orderpolicy=='exogenous'){
      basestock = parms$basestock}
    
		q = Vstatecreator(parms)
		As = stateactioncreator(parms,q)
		
		prHat=prHat[location_id==locationid]

		q[,V:=NULL]
		q2 = copy(q)
		names(q2) = paste0(names(q),'prm')
		
		
		q = copy(policy)
		q[,V:=NULL]
		
		totsizepred = (as.numeric(dim(q)[1])*as.numeric(dim(q2)[1]))*as.numeric(dim(q)[2] + dim(q2)[2])
		totmemorypred = totsizepred*4.7/1e+9
		if(mem<totmemorypred){
		  
		  cat(paste0("You are making very large demands upon your machine. You said that you have ", mem,
		             "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
		             round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
		             ,"or find a higher memory machine."))
		  
		  return()
		}
		
		

		if(pricerule=='unrestricted'){
	     pfill=q[, k:=t+1][q2[, k:=tprm], on=.(k), nomatch=0L,allow.cartesian=TRUE][,k:=NULL]
		}
		print(mem_used())
		###############################################
		# For restricted pricing (static or decreasing), 	
		# only certain price transitions are impossible:
		###############################################
		print(mem_used())
		############
		# Static
		############
		if(pricerule=='static'){
		  
		  q[, k0:=t+1]
		  q2[, k0:=tprm]
		  
		  for(j in 1:J){
		    
    	  q[, paste0('k',j):=get(paste0('p',j,'s'))]
    	  q2[, paste0('k',j):=get(paste0('p',j,'sLASTprm'))]

    	  q[, paste0('l',j):=get(paste0('p',j,'sLAST'))]
    	  q2[, paste0('l',j):=get(paste0('p',j,'sLASTprm'))]
    	  
		  }
		  print(mem_used())
		  cols = c('k0',paste0('k',1:J),paste0('l',1:J))
		  
		  pfill=q[q2, on=cols, nomatch=0L,allow.cartesian=TRUE][,k0:=NULL]
		  
  		  for(j in 1:J){
  		    pfill[,paste0('l',j):=NULL]
  		     pfill[,paste0('k',j):=NULL]
  		  }
    	  
		}
		print(mem_used())
		
		#the next period last price must be the same as 
		#the current price in this period, unless it's 
		#the end of the day in which case it resets.
		
		pfill[,ssp:=1:dim(pfill)[1]] #ssp is a state-action to state 
		
		pcopy=copy(pfill)
		
				
		#####################################################		
		#      
		#      
		#      
		#      
		#      
		#      Endogenous Capacity State Adjustments 
		#      
		#     
		#     
		#
		#####################################################
				
		if(orderpolicy=='endogenous'){		
						
				##############################
				#Broad Inventory Restrictions,
				#         by Tp.
				##############################
				
				#Part 1. if it is anything except the last period, 
				#inventory must elementwise weakly decrease in the next period (relative to post order
				#current inventory state)
				#
				#
				#Part 2. if it is the last period, all inventory for the newest vintages
				#must transition to 0 and inventory for other vintages must be 
				#weakly less than the vintage above it, or if K==1, the order that period. 
				#
				#zero out all others! 
		
				##########################################
				#     
				#     define post ordering states 
				#
				##########################################
				for(j in 1:J){
				  print(mem_used())
				  pfill[,paste0('prod',j,'vint',Tp,'post'):=get(paste0('prod',j,'vint',Tp))+get(paste0('order',j))]
				  
				}
				if(Tp>1){
				for(j in 1:J){
				  for(tau in 1:(Tp-1)){
				  pfill[,paste0('prod',j,'vint',tau,'post'):=get(paste0('prod',j,'vint',tau))]
				  }
				}
				}
				
				
				#Part 1
				for(j in 1:J){
				  for(tau in 1:Tp){
				    
				    pfill=pfill[(d==K) |
				                  
				                  (d!=K & get(paste0('prod',j,'vint',tau,'prm'))<=get(paste0('prod',j,'vint',tau,'post')))]
				    
				  }
				}
		
			  #Part 2.
				if(K==1){
				  
				  for(j in 1:J){
				    
				    if(Tp==1){
				      #New inventry = 0 next period. 
				      pfill=pfill[get(paste0('prod',j,'vint',Tp,'prm'))==0]
				    }
				    print(mem_used())
				    if(Tp>1){
				      
				      #New inventry = 0 next period. 
				      pfill=pfill[get(paste0('prod',j,'vint',Tp,'prm'))==0]
				  
				      for(tau in 1:(Tp-1)){
				             
				        pfill=pfill[get(paste0('prod',j,'vint',tau,'prm'))<=get(paste0('prod',j,'vint',tau+1,'post'))]
				            
				       }
				       
				     }
				      
				      
				    }
		
				  }
				if(K>1){
				
				  for(j in 1:J){
		  		  if(Tp==1){
		  		  
		  		         
		  		     pfill=pfill[((d==1) | (d!=1 & d!=K) |
		  		                    
		  		                  #New inventory next period = 0
		  		                 (d==K & get(paste0('prod',j,'vint',Tp,'prm'))==0))]
		  		  
		  		  }
		  		  if(Tp>1){
		  		    for(tau in 1:(Tp-1)){
		  		      
		  		              pfill=pfill[((d==1) | (d!=1 & d!=K) |
		  		                   
		  		                   #New inventory next period = 0 
		  		                   (d==K & get(paste0('prod',j,'vint',Tp,'prm'))==0
		  		                    
		  		                    #all other products have to have less than vintage above. 
		  		                    &  get(paste0('prod',j,'vint',tau,'prm'))<=get(paste0('prod',j,'vint',tau+1,'post')))
		  		                 
		  		    )]
		  		    }
		  		  }
				  }
		
				  print(mem_used())
				}
				
		
				#########################################
				#Capacity Inventory Restrictions
				#########################################
				
				#the probability of ILLEGAL actions does not actually matter,
				#because the payoff is negative infinity so they'll never be chosen. 
				#so focus only on the subset of legal. 
				
				#cannot order outside beginning of day
				#and you cant order more than the total capacity
				
				for(j in 1:J){
				  
				  cols = paste0('prod',j,'vint',1:Tp)
				  cols = c(paste0('prod',j,'vint',1:Tp),paste0('order',j))
				  pfill[, paste0('Itot',j) := Reduce(`+`, .SD), .SDcols=cols]
				  
				}
				
				for(j in 1:J){
				  pfill=pfill[(get(paste0('order',j))==0 & d!=1) | (d==1)]
				  pfill=pfill[(get(paste0('Itot',j))<=categoryvector[j])]
				}
				
		
				#########################################
				#Drawdown-Specific Inventory Restrictions
				#########################################
				
				#get inventory in the next period, pre-order
				for(j in 1:J){
				  
				  cols = paste0('prod',j,'vint',1:Tp,'prm')
				  pfill[, paste0('Itot',j,'prm') := Reduce(`+`, .SD), .SDcols=cols]
				  
				}
				
				#Next period you cant have more total inventory than the capacity 
				#before ordering.
				for(j in 1:J){
				  print(mem_used())
				  pfill=pfill[(get(paste0('Itot',j,'prm'))<=categoryvector[j])]
				}
				
				#you can never have more pre-order total inventory in the next period 
				#than you do in the postorder inventory of this period
				for(j in 1:J){
				  
				  pfill=pfill[get(paste0('Itot',j)) >= get(paste0('Itot',j,'prm'))]	
				  
				  
				}
				
				#####################################################
				#
				#for LIFO (FIFO), post-ordering, if d!=K, then each new
				#inventory must be a valid LIFO (FIFO) transformation of the
				#inventory in the current period. 
				#
				#if d==K, then: 
				#
				#     InvPrm[1:Tp-1] must be a valid LIFO (FIFO) transform of
				#    
				#     Inv[2:Tp]
				#
				#
				#These must hold true product-wise forall products. 
				#
				#####################################################
				
				
				#to later deal with the d==K case, it's useful to define new columns
				#that line up the post-decay comparisons. That is, given 
				#   A B C  ---> D E F 
				#    
				# We want to make sure B C to D E is LIFO (FIFO).  
				
				if(Tp>1){
				  for(j in 1:J){
			  	for(tau in 1:(Tp-1)){
				
			  	    pfill[,paste0('prod',j,'vint',tau,'postDK'):=get(paste0('prod',j,'vint',tau+1,'post'))]
			  	    pfill[,paste0('prod',j,'vint',tau,'DKprm'):=get(paste0('prod',j,'vint',tau,'prm'))]
			  	 }
				  }
				
				
				  print(mem_used())
				
				for(j in 1:J){
				  
				  #d!=K
				  cols = c('ssp',paste0('prod',j,'vint',1:Tp,'post'),paste0('prod',j,'vint',1:Tp,'prm'))
				  temp = pfill[d!=K,..cols]
				  tbl <- melt((temp), meas=patterns(paste0("^prod",j,"vint[1-9]post$"), "prm$"), 
				              value.name=c("bef","aft"))
				  
				  if(Drawdown=='FIFO'){
				    setorder(tbl, ssp, variable) 
				    }
				  if(Drawdown=='LIFO'){
				    setorder(tbl, ssp, -variable) 
				  }
				  print(mem_used())
				  tbl = data.table(tbl)
				  tbl[,dif:=(sum(bef) - sum(aft)),by=ssp]
				  tbl[,flifo:=(pmin(pmax(cumsum(bef) - dif, 0), bef)),by=ssp]
				  tbl[,keep:=as.numeric(identical(as.numeric(flifo), as.numeric(aft))),by=ssp]
				  keep0=unique(tbl[keep==1,ssp])
				  temp=data.table(temp)
				  temp[ , paste0('keepit',j) := 0]
				  temp[ssp%in%keep0,paste0('keepit',j):=1]
				  temp0 = copy(temp)
				  
				  
				  #d==K
				  
				  cols = c('ssp',paste0('prod',j,'vint',1:(Tp-1),'postDK'),paste0('prod',j,'vint',1:(Tp-1),'DKprm'))
				  temp = pfill[d==K,..cols]
				  tbl <- melt((temp), meas=patterns(paste0("^prod",j,"vint[1-9]postDK$"), "DKprm$"), 
				              value.name=c("bef","aft"))
				  if(Drawdown=='FIFO'){
				    setorder(tbl, ssp, variable) 
				  }
				  if(Drawdown=='LIFO'){
				    setorder(tbl, ssp, -variable) 
				  }
				  tbl = data.table(tbl)
				  tbl[,dif:=(sum(bef) - sum(aft)),by=ssp]
			
			    tbl[,flifo:=(pmin(pmax(cumsum(bef) - dif, 0), bef)),by=ssp]
			    tbl[,keep:=as.numeric(identical(as.numeric(flifo), as.numeric(aft))),by=ssp]
		
				  keep0=unique(tbl[keep==1,ssp])
				  temp=data.table(temp)
				  temp[ , paste0('keepit',j) := 0]
				  temp[ssp%in%keep0,paste0('keepit',j):=1]
				  temp1=copy(temp) 
				  print(mem_used())
				  cols = c('ssp',paste0('keepit',j))
				  temp2 = rbind(temp0[,..cols],temp1[,..cols])
				  
				  
				  pfill = merge(pfill,temp2,by='ssp',all.x=T)
				}
				
				  print(mem_used())
		  	for(j in 1:J){
		  	  
		  	pfill = pfill[get(paste0('keepit',j))==1]  
		  	  
		  	}
				}
		}		
		
	
				
		####################################################
		#
		#
		#
		#
		#		
		#    Get the implied demand for each action state
		#
		#
		#
		#
		#
		#
		###################################################
	
		###########################################
		#
		#
		#      For each transition, there is a SET
		#      of implied Q's.
		#
		#      
		#
		###########################################
			
		for(j in 1:J){
		  pfill[,
		        paste0('Q',j,'impliedlb'):=0.00001
		        ]
		  print(mem_used())
		  pfill[,
		        paste0('Q',j,'impliedub'):=0.00001
		        ]
		  
		  pfill[,
		       paste0('Itot',j):=as.numeric(get(paste0('Itot',j)))
		        ]
		  pfill[,
		        paste0('Itot',j,'prm'):=as.numeric(get(paste0('Itot',j,'prm')))
		        ]
		
		}  
			
	
		#################	
		#d<K
		#################
		#Case 1: Total inventory is the same in the next period, and not 0. 
		#        then demand must have been [0,0]. 
		 
		for(j in 1:J){
				      
				pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))!=0),
				      paste0('Q',j,'impliedlb'):=0
				      ]
				pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))!=0),
				      paste0('Q',j,'impliedub'):=0
				      ]
				print(mem_used())
		
				#Case 2: Inventory now and next period is 0.  Demand must be: [0,Inf]
				     
				pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))==0),
				      paste0('Q',j,'impliedlb'):=0
				      ]
				pfill[(d<K) & (get(paste0('Itot',j))==get(paste0('Itot',j,'prm')) & get(paste0('Itot',j))==0),
				      paste0('Q',j,'impliedub'):=Inf
				      ]
				
				
				 #Case 3: Inventory now is positive. 
				 #        Next period, inventory is less than what it is now, but still not zero. 
				 #        Demand must have been [Itot-Itotprm, Itot-Itotprm]
			
				
				pfill[(d<K) & (get(paste0('Itot',j))>(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))) & 
				        ((get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))>0),
				      paste0('Q',j,'impliedlb'):=(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))
				      ]
				      
				pfill[(d<K) & (get(paste0('Itot',j))>(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))) & 
				        ((get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))>0),
				      paste0('Q',j,'impliedub'):=(get(paste0('Itot',j)) - get(paste0('Itot',j,'prm')))
				      ]
				
				 #Case 4:  Inventory is positive currently, and 0 the next period.
				 #         This means demand was [Itot,Inf]
				
				
				pfill[(d<K) & (get(paste0('Itot',j))>(0)) & ((get(paste0('Itot',j,'prm')))==0),
				      
				      paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))
				      
				      ]
				
				pfill[(d<K) & (get(paste0('Itot',j))>(0)) & ((get(paste0('Itot',j,'prm')))==0),
				      
				      paste0('Q',j,'impliedub'):=Inf
				      
				      ]
		}
		
		##################
		#d==K
		##################
		#
		#This part must be broken up by LIFO and FIFO.
		#(Note that next period always considered BEFORE 
		#order inventory.)
		#
		############
		#LIFO
		############
		#
		#Case 1. If all vintages shift one down
		#        and Itot-prodjvint1 !=0
		#        then this means demand was:
		#        Demand: [0,0]
		#        Or in other words, Itotprm (before order) = Itot-prodjvint1
    #
		#Case 2. If Itot-prodjvint1 !=0
		#        and Itotprm (before order) = 0 next period 
		#        then this means demand was:
		#        Demand: [Itot-prodjvint1,Inf]
		#
		#Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
		#        This means that some demand was drawn down.
		#        Demand: [Itot-prodjvint1 - Itotprm,Itot-prodjvint1 - Itotprm]
		#
    #
		#Case 4. If Itot-prodjvint1 = 0
		#       Demand: [0,Inf]
		#
		############
		#FIFO
		############
		#
		#Case 1. If all vintages shift one down
		#        and Itot-prodjvint1 !=0
		#        then this means demand was:
		#        Demand: [0,prodjvint1]
		#        Or in other words, Itotprm (before order) = Itot-prodjvint1
		#
		#Case 2. If Itot-prodjvint1 !=0
		#        and Itotprm (before order) = 0 next period 
		#        then this means demand was:
		#        Demand: [Itot,Inf]
		#
		#Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
		#        This means that some demand was drawn down.
		#        Demand: [Itot - Itotprm, Itot - Itotprm]
		#
		#
		#Case 4. If Itot-prodjvint1 = 0
		#        Demand: [0,Inf]
		#
		
		for(j in 1:J){		
  if(Tp==1){
  
  for(j in 1:J){
      
    if(K==1){
    pfill[,paste0('Q',j,'impliedlb'):=0]
    pfill[,paste0('Q',j,'impliedub'):=Inf]
    }
    if(K>1){
      print(mem_used())
      pfill[(d==K), paste0('Q',j,'impliedlb'):=0]  
      pfill[(d==K), paste0('Q',j,'impliedub'):=Inf] 
      
    }
  }
 }
  if(Tp>1){
    
    
    if(Drawdown=='LIFO'){
      
      
      #for details on what code does.
      
      #Case 1. If all vintages shift one down
      #        and Itot-prodjvint1 !=0
      #        then this means demand was:
      #        Demand: [0,0]
      #        Or in other words, Itotprm (before order) = Itot-prodjvint1
      ###########################
      # CASE 1
      ###########################
      ## set some variable names
      pre <- paste0("prod",j,'vint', 2:(Tp),'post')
      pst <- paste0("prod",j,'vint', 1:(Tp-1),'prm')
     
      pfill[,paste0('nonexpInv',j):=(get(paste0('Itot',j))-get(paste0('prod',j,'vint',1)))]
      ## select where they match using `==` and Reduce to combine with a `&` (logical AND)
      pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
             & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedlb'):=0]
      pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
             & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedub'):=0]
      
      #Case 2. If Itot-prodjvint1 !=0
      #        and Itotprm (before order) = 0 next period 
      #        then this means demand was:
      #        Demand: [Itot-prodjvint1,Inf]
      ###########################
      # CASE 2
      ###########################
      ## set some variable names
    
      pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
              (get(paste0('Itot',j,'prm')))==0), 
               paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))-get(paste0('prod',j,'vint',1))]
      
      pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
              (get(paste0('Itot',j,'prm')))==0), paste0('Q',j,'impliedub'):=Inf]
      
     
      #Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
      #        This means that some demand was drawn down.
      #        Demand: [Itot-prodjvint1 - Itotprm,Itot-prodjvint1 - Itotprm]
      ###########################
      # CASE 3
      ###########################
      pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
               
               (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
            
            paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))-get(paste0('prod',j,'vint',1))-get(paste0('Itot',j,'prm'))]
      
      pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
               
               (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
            
            paste0('Q',j,'impliedub'):=get(paste0('Itot',j))-get(paste0('prod',j,'vint',1))-get(paste0('Itot',j,'prm'))]
      
      print(mem_used())
      #Case 4. If Itot-prodjvint1 = 0
      #       Demand: [0,Inf]
      ###########################
      # CASE 4
      ###########################
      pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
            
            paste0('Q',j,'impliedlb'):=0]
      
      pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
             
             paste0('Q',j,'impliedub'):=Inf]
  
  
  
    }
    if(Drawdown=='FIFO'){
  		  
  		  
  		  #Case 1. If all vintages shift one down
  		  #        and Itot-prodjvint1 !=0
  		  #        then this means demand was:
  		  #        Demand: [0,prodjvint1]
  		  #        Or in other words, Itotprm (before order) = Itot-prodjvint1
  		  ###########################
  		  # CASE 1
  		  ###########################
  		  ## set some variable names
  		  pre <- paste0("prod",j,'vint', 2:(Tp),'post')
  		  pst <- paste0("prod",j,'vint', 1:(Tp-1),'prm')
  		  
  		  pfill[,paste0('nonexpInv',j):=(get(paste0('Itot',j))-get(paste0('prod',j,'vint',1)))]
  		  ## select where they match using `==` and Reduce to combine with a `&` (logical AND)
  		  pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
  		         & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedlb'):=0]
  		  pfill[(d==K & Reduce(`&`, Map(`==`, mget(pre), mget(pst))) 
  		         & get(paste0('nonexpInv',j))!=0),paste0('Q',j,'impliedub'):=as.numeric(get(paste0('prod',j,'vint',1)))]
  		  
  		  print(mem_used())
  		  
  		  #Case 2. If Itot-prodjvint1 !=0
  		  #        and Itotprm (before order) = 0 next period 
  		  #        then this means demand was:
  		  #        Demand: [Itot,Inf]
  		  ###########################
  		  # CASE 2
  		  ###########################
  		  pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
  		           (get(paste0('Itot',j,'prm')))==0), 
  		        paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))]
  		  
  		  pfill[(d==K &  get(paste0('nonexpInv',j))!=0 & 
  		           (get(paste0('Itot',j,'prm')))==0), paste0('Q',j,'impliedub'):=Inf]
  		  print(mem_used())
  		  #Case 3. If 0 < Itotprm (before order) < Itot-prodjvint1
  		  #        This means that some demand was drawn down.
  		  #        Demand: [Itot - Itotprm, Itot - Itotprm]
  		  #
  		  ###########################
  		  # CASE 3
  		  ###########################
  		  pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
  		           
  		           (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
  		        
  		        paste0('Q',j,'impliedlb'):=get(paste0('Itot',j))-get(paste0('Itot',j,'prm'))]
  		  
  		  pfill[(d==K &  (0 < (get(paste0('Itot',j,'prm')))) & 
  		           
  		           (get(paste0('Itot',j,'prm'))) < (get(paste0('Itot',j))) - get(paste0('prod',j,'vint',1))), 
  		        
  		        paste0('Q',j,'impliedub'):=get(paste0('Itot',j))-get(paste0('Itot',j,'prm'))]
  		  
  		  #Case 4. If Itot-prodjvint1 = 0
  		  #        Demand: [0,Inf]
  		  #
  		  ###########################
  		  # CASE 4
  		  ###########################
  		  pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
  		        
  		        paste0('Q',j,'impliedlb'):=0]
  		  
  		  pfill[(d==K &  (get(paste0('Itot',j)) - get(paste0('prod',j,'vint',1))==0)), 
  		        
  		        paste0('Q',j,'impliedub'):=Inf]
  		  
  		  
  		  print(mem_used())
  		  
  		  
  		}
  	
 }
}	
	
		
		
    		
    ################################################
    #
    #       Now, given the probability 
    #       of distribution as a function 
    #       of actions (or action state, in theory)
    #       , calculate the probability of transitioning
    #       from one state to the next
    #       
    #       
    ###############################################
    
    #Change the output into a CDF, because ultimately this is what 
    #we use because we have demand ranges		
    cols = paste0('p',1:J,'s')
    cols = c(statecols,cols)		
    
    if(J==1){prHat[,CDF := cumsum(pQ),by=cols]}
    
    cols=  c( paste0("p",1:J,'s'),statecols)
    
   
    print(mem_used())
    CDF1 = unlist(prHat$CDF)
    prHat[,CDF:=CDF1]
    
    #if your max probability is not close to 1, throw back an error.
    pre <- paste0("p",1:J,'s')
    pst <- minprices
    
    ## select where they match using `==` and Reduce to combine with a `&` (logical AND)
    maxpr=max(prHat[(Reduce(`&`, Map(`==`, mget(pre), pst))),CDF])
    if(maxpr<.999){
      print(maxpr)
      print("Warning: You need to add more possible Q's to the distribution creator.  You
            are not approximating the distribution very well.")
     
    }
    
    print(mem_used())
    #Because we cannot store infinite entires in this database., we 
    #set the maximum Q as the infinity in the pfill database. 
    maxQ = max(prHat$Q1)
    
    for(j in 1:J){
    pfill[get(paste0('Q',j,'impliedlb'))==Inf,paste0('Q',j,'impliedlb'):=maxQ]
    pfill[get(paste0('Q',j,'impliedub'))==Inf,paste0('Q',j,'impliedub'):=maxQ]
    }
    
    prHat[,pQ:=NULL]
    print(mem_used())
   
    
    ############################################
    #To calculate Pr(a <= X <= b), we need:
    #F(b) - F(a-1) because it is discrete
    #distribution. 
    #
    ############################################
    
    print(mem_used())
    priceA = copy(As$AA)
    priceA[,anum:=NULL]
    
    
    for(j in 1:J){
      
      
    pfill[,paste0(letters[J*(j-1)+1],'0'):=get(paste0('Q',j,'impliedlb'))]
    pfill[,paste0(letters[J*(j-1)+2],'0'):=get(paste0('Q',j,'impliedub'))]  
    if(orderpolicy=='endogenous'){priceA[,paste0('order',j):=NULL]}
    
    pfill[,paste0(letters[J*(j-1)+1],'0','minus1'):=get(paste0('Q',j,'impliedlb'))-1]
 
    pfill[,paste0('Q',j,'impliedlb'):=NULL]
    pfill[,paste0('Q',j,'impliedub'):=NULL]
    print(mem_used())

    
    }
    
    if(J==1){
      # F(b)     = 1 
      # F(a-1)   = 2
     
      colA = c('b0','a0minus1')
      nam = c('one','two')
      
      
      for(l in 1:2){ #four because there are 2 terms in a 1-d CDF with bounds.  
        
        assign(paste0("prHat",nam[l]),copy(prHat))
        get(paste0("prHat",nam[l]))[,colA[l]:=Q1]

        get(paste0("prHat",nam[l]))[,Q1:=NULL]

        get(paste0("prHat",nam[l]))[,paste0('CDF',nam[l]):=CDF] 
        get(paste0("prHat",nam[l]))[,CDF:=NULL] 
        
        get(paste0("prHat",nam[l]))[,location_name:=NULL]
        get(paste0("prHat",nam[l]))[,location_id:=NULL]
        
        
      }
      
      
      #MERGE AND CALCULATE STATE TRANITIONS:
      for(l in 1:2){
        mrgnam = colnames(get(paste0("prHat",nam[l])))
        mrgnam = mrgnam[-length(mrgnam)]
        pfill=merge(pfill,get(paste0("prHat",nam[l])),
                    by=mrgnam,all.x=T)
        
      }
      
      pfill[,prss:=CDFone - CDFtwo]
    
    }
    
    priceA = unique(priceA)
    
    #Now, the task is to build SxS for each A knowing that everything is sparse except the below!
		
		##########################################
		#   
		#   the aboves are all the nonzeroes!
		#
		##########################################
		
		
    pfill=pfill[,.(anum,state,stateprm,prss)]
	
    q = Vstatecreator(parms)
    As =  copy(policy)
		
    
		return(list('datatablePA'=pfill,'q'=q,'A'=As$AA))

}


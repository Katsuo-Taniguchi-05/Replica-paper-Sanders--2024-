###############################################################################
#
#
#
#          Vstatecreator.R
# 
#    Creates TWO functions:
#    
#    1. Vstatecreator
#    
#      Inputs: 
#
#          1.   parms: the parameters of the dynamic programming problem. 
#      
#      Outputs: 
#      
#          1.  q: a data table of every state and the value function value (initialized at arbitrary 555.)   
#     
#    2. stateactioncreator
#    
#
#      Inputs: 
#
#          1.   parms: the parameters of the dynamic programming problem. 
#          
#          2.   q: The output from the above function. 
#      
#      Outputs: 
#      
#          1.  As$SA: This is a data table of every state-action pair
#          
#          2.  As$AA: This is a data table of every action. 
#
#
#
#
#                 By Robert Sanders, July 12, 2023
#

##############################################################################


Vstatecreator = function(parms){

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
    mem=parms$mem
    modeltype=parms$modeltype  
    statecols=parms$statecols             
    maxq = parms$maxq                 
    tolerance=parms$tolerance              
    econometricunc = parms$econometricunc  
    orderpolicy = parms$orderpolicy 
    Drawdown = parms$Drawdown
    if(orderpolicy=='exogenous'){basestock = parms$basestock}
		obspricekeep = parms$obspricekeep
    
  if(Tp>3){
		print('Please select Tp<=3.')
		return()
	}     
  if(K==1){
		print('Please select K>=2.')
		return()
	}
	if(orderpolicy!='endogenous'){
		print('Please select and endogenous ordering policy.')
		return()
	}
	if(orderpolicy!='endogenous'){
		print('Please select and endogenous ordering policy.')
		return()
	}
	if(J!=1){
		print('There is only support for J=1. Please set J=1.')
		return()
	}
	if(!Drawdown%in%c('LIFO','FIFO')){
		print('Please select only LIFO or FIFO drawdown.')
		return()
	}	
	if(!pricerule%in%c('static','unrestricted')){
		print('Please select only static or unrestricted price-policy rules.')
		return()
	}		
  
  if(!modeltype%in%c('multinomiallogitpoisson')){
		print('Please select only multinomiallogitpoisson demand model.')
		return()
	}		
		
  #############################################
  ###Setup the sets of actions and price states
  #############################################
  
	 #list of price and ordering possibilities, and states for prices
   listprices=list()
   listpriceslast = list()
   listorders = list()

    for(j in 1:J){
    		if(minprices[1]!= maxprices[1]){
	        listprices[[paste0('p',j,'s')]]=sort(c(round(seq(from = minprices[1], to = maxprices[1],
	        	                             by=a),3),obspricekeep[j]))
	        listpriceslast[[paste0('p',j,'sLAST')]]=sort(c(round(seq(from = minprices[1], to = maxprices[1],
	        	                             by=a),3),obspricekeep[j]))
	       
    		}else{
    			  listprices[[paste0('p',j,'s')]]=sort(c(round(seq(from = minprices[1], to = maxprices[1],
	        	                             by=a),3)))
	        listpriceslast[[paste0('p',j,'sLAST')]]=sort(c(round(seq(from = minprices[1], to = maxprices[1],
	        	                             by=a),3)))
    			
    		}
    	 listorders[[paste0('order',j)]]=0:ordercap[j]
     }
   
  #############################################
  ###Setup the sets of inventory states
  #############################################
   listinventory=list()
    k=1
    for(j in 1:J){
      for(t in 1:Tp){
        listinventory[[paste0('prod',j,'vint',t)]]=0:categoryvector[j]
        k=k+1
      }}
    

  #numberofperiods = K*wmax+1 #add to VT+1 = 0 
  	
    if(pricerule!='unrestricted'){
    
      
      x = length(listinventory)
      x2 = length(listinventory[[1]])
      
      px = length(listpriceslast)
      px2 = length(listpriceslast[[1]])
      
      totsizepred = length(1:K)*length(1:(wmax+1))*(x2^x)*(px2^px)*(3+J*Tp+J)
      totmemorypred = totsizepred*4.7/1e+9
      #print(totmemorypred)
      if(mem<totmemorypred){
        
        cat(paste0("You are making very large demands upon your machine. You said that you have ", mem, 
                   "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
                   round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
                   ,"or find a higher memory machine.")) 
        
        
        return()
      }else{
        
      
      Vmat = data.table(expand.grid(c(list(d = 1:K, w = 1:(wmax+1)),
                                        listinventory,listpriceslast)))
    
      }
    
    
    }                   
    if(pricerule=='unrestricted'){
   
      
      x = length(listinventory)
      x2 = length(listinventory[[1]])
      
      
      totsizepred = length(1:K)*length(1:(wmax+1))*(x2^x)*(3+J*Tp+J)
      totmemorypred = totsizepred*4.7/1e+9
      #print(totmemorypred)
      if(mem<totmemorypred){
        
        cat(paste0("You are making very large demands upon your machine. You said that you have ", mem, 
                   "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
                   round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
                   ,"or find a higher memory machine.")) 
        
        return()
      }else{
      
    	 Vmat = data.table(expand.grid(c(list(d = 1:K, w = 1:(wmax+1)),
                                        listinventory)))
      }
    }      
  	###########################
    #Pare capacity constraints
    #in state and action
    ############################
    for(j in 1:J){
  		 
    	Vmat[,paste0('Sum',j) := rowSums(.SD, na.rm = TRUE), 
  		 	                  .SDcols = names(listinventory)[(j*Tp-(Tp-1)):(j*Tp)]] 
    	
    	}
    for(j in 1:J){
      
      Vmat=Vmat[get(paste0('Sum',j))<=categoryvector[j]]
      
    }
  		
		
  ################################
  #
  #
  #  New Inventory has to be 0 at beginning 
  #  of the day, except for that which was ordered 
  #
  ################################
 
  for(j in 1:J){
  	
  	Vmat=Vmat[d!=1 | (d==1 & get(paste0('prod',j,'vint',Tp))==0),]
  	
  	}
  

  ##Since w=wmax+1 is the death period, drop 
  ##all d != 1 in this state, because this is 
  Vmat = Vmat[w<(wmax+1) | (w==(wmax+1) & d==1),]

  
  Vmat = cbind(Vmat,'V'=555)  #555 is a fake number to initialize V
  Vmat = data.table(Vmat)
    
  #Add in the time 'states'
  #add in appropriate number of planning periods
  

	Vmat[,planningperiod:=1]
	Vend = Vmat[w==wmax+1]
	
	  new = data.table()
  	for(i in 1:planningperiods){
  	
  		Vmatcopy = copy(Vmat[w!=wmax+1])
  	  Vmatcopy[,planningperiod:=i]
  		new=rbind(new,Vmatcopy)
  	}
	  
	  #bind the vend on
	  Vmat = rbind(new,Vend[,planningperiod:=planningperiods])
	  
  	Vmat[,t:=(d+K*(w-1))+(K+K*(wmax-1))*(planningperiod-1)]
  	
  	setcolorder(Vmat, c("t", setdiff(names(Vmat), "t")))
  	
    Vmat[t==max(t),w:=1]
  	Vmat[,planningperiod:=NULL]
  
  
  ###Add in terminal value = 0 for backward induction
  Vmat[t==max(t),V:=0]  #terminal value is 0 
  	

  #drop Sum columns, they aren't states
  for(j in 1:J){
  	
  	Vmat[,paste0('Sum',j):=NULL]
  }
  
  
  Vmat = Vmat[order(t,w,d),]


  Vmat=data.table(Vmat[,state:=1:dim(Vmat)[1]])
  	
  return(Vmat) 
  
  
	
}
  
stateactioncreator = function(parms,Vmat){
  
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
  mem=parms$mem
  modeltype=parms$modeltype  
  statecols=parms$statecols             
  maxq = parms$maxq                 
  tolerance=parms$tolerance              
  econometricunc = parms$econometricunc  
  orderpolicy = parms$orderpolicy 
  if(orderpolicy=='exogenous'){basestock = parms$basestock}
  obspricekeep = parms$obspricekeep
  Drawdown = parms$Drawdown
  
  
  if(Tp>3){
		print('Please select Tp<=3.')
		return()
	}    
  if(K==1){
		print('Please select K>=2.')
		return()
	}
	if(orderpolicy!='endogenous'){
		print('Please select and endogenous ordering policy.')
		return()
	}
	if(orderpolicy!='endogenous'){
		print('Please select and endogenous ordering policy.')
		return()
	}
	if(J!=1){
		print('There is only support for J=1. Please set J=1.')
		return()
	}
	if(!Drawdown%in%c('LIFO','FIFO')){
		print('Please select only LIFO or FIFO drawdown.')
		return()
	}	
	if(!pricerule%in%c('static','unrestricted')){
		print('Please select only static or unrestricted price-policy rules.')
		return()
	}		
  
  if(!modeltype%in%c('multinomiallogitpoisson')){
		print('Please select only multinomiallogitpoisson demand model.')
		return()
	}			
  
	  
	  #############################################
	  ###Setup the sets of actions and price states
	  #############################################
	  
		 #list of price and ordering possibilities, and states for prices
	   listprices=list()
	   listorders = list()
	   print(print(mem_used()))
	    for(j in 1:J){
	            		if(minprices[1]!= maxprices[1]){
		        listprices[[paste0('p',j,'s')]]=sort(c(round(seq(from = minprices[1], to = maxprices[1],
		        	                             by=a),3),obspricekeep[j]))
		       
		       
	    		}else{
	    			  listprices[[paste0('p',j,'s')]]=sort(c(round(seq(from = minprices[1], to = maxprices[1],
		        	                             by=a),3)))
		       
	    			
	    		}
	        listorders[[paste0('order',j)]]=0:ordercap[j]
	     }
	   
	
	    #Set up unconstrained action space. 
	   
	    	 Amat = data.table(expand.grid(c(listprices,listorders)))
	 
	    	 #Make it so the total order can never exceed 
	    Amat[,anum:=1:dim(Amat)[1]]	 
	    AA=copy(Amat)
	    
	    nam = names(AA)
	    print(mem_used())
	    totsizepred = (as.numeric(dim(Vmat)[1])*as.numeric(dim(Amat)[1]))*((dim(Vmat)[2]+dim(Amat)[2]))
	    totmemorypred = totsizepred*4.7/1e+9
	    #print(totmemorypred)
	    if(mem<totmemorypred){
	      
	      cat(paste0("You are making very large demands upon your machine. You said that you have ", mem, 
	                 "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
	                 round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
	                 ,"or find a higher memory machine.")) 
	      
	      return()
	      
	    }else{
	    
	      
	    #Efficient Cross Join
	    CJ.table.1 <- function(X,Y)
	     setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
	    Amat=CJ.table.1(Amat,Vmat)
	    Amat = Amat[order(anum,state)]
	    print(mem_used())
	    setcolorder(Amat, c(colnames(Vmat), nam))
	    Amat[,V:=NULL]
	    print(mem_used())
	   
	  	return(list('SA'=data.table(Amat),'AA'=AA)) 
	    
	    }
	  
	
	
	  
  
}



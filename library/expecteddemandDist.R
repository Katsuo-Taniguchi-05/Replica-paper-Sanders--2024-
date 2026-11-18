###############################################################################
#
#
#
#         Expected Demand Distribution Creation Script
# 
#
#      Inputs: 
#      
#           1 drawlocationvalue:  is a data table consisting of location name (location_name), location number (location_id),
#                                draw number (r), state, 
#                                [parameters] (one column for each if vector)
#
#          2.   parms: the parameters of the dynamic programming problem. 
#                
#                Including....
#          
#                      a.  maxq:   this script creates a grid over quantities, this is the maximum of the grid, which runs from {0,1,2,...Q}^J
#          
#                      b.  statecols: the names of the state variables that shift demand. 
#                      
#                      c.  modeltype: the demand model used
#                      
#                               
#      Outputs: 
#      
#          1.  (Hat)Pr(Q|a,s), which is a large data table for each possible Q-action-state.
#               (The max Q)   
#
#
#                 By Robert Evan Sanders, July 12, 2023
#
#
#
#
##############################################################################


prgen = function(drawlocationvalue,parms){
  
  
  mem=parms$mem
  J = parms$J
  modeltype = parms$modeltype
  statecols = parms$statecols
  maxq = parms$maxq
  econometricunc = parms$econometricunc
  
  ################################
  #CREATE PRICE MATRIX WITH parms
  ################################
  q = Vstatecreator(parms)
  As = stateactioncreator(parms,q)
  
  pricematrix=As$AA
  
  #Create simple price cartesian set from full action space
  #this is the only thing that changes probabilities of demand.
  prods = paste0('p',1:J,'s')
  
  #Create grid of all possible quantites demanded
  #let this grid be "too large" so no likely quantities are missed. 
  
  Qs = do.call(CJ, replicate(J, -1:maxq, simplify = FALSE))
  setnames(Qs, paste0('Q', 1:J))
  
  
  pricematrix = pricematrix[,..prods]
  pricematrix = unique(pricematrix)
  
  #If we are ignoring econometric uncertainty and calculating
  #the optimal policy at a plugin estimator theta_hat:
  if(econometricunc==F){
    
    names = colnames(drawlocationvalue)
    names = names[which(names!='r')]
    drawlocationvalue=drawlocationvalue[,lapply(.SD, mean),by=names]
  
    }
  
  #Do this analysis store by store in a loop because it is 
  #too costly to do this all at once.
  
  list0 = sort(unique(drawlocationvalue$location_id))
  
  
  fullprobs = data.table()
  
  for(l in list0){
        #print(paste0('store ',l))
        temp1 = drawlocationvalue[location_id==l]
        nam = names(temp1)
        print(mem_used())
      
        totsizepred = (as.numeric(dim(temp1)[1])*as.numeric(dim(pricematrix)[1]))*((dim(temp1)[2]+dim(pricematrix)[2]))
        totmemorypred = totsizepred*4.7/1e+9
        #print(totmemorypred)
        if(mem<totmemorypred){
      
          cat(paste0("You are making very large demands upon your machine. You said that you have ", mem,
                     "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
                     round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
                     ,"or find a higher memory machine."))
      
          return()
        }
      
        
        
        temp1=temp1[, pricematrix[],by=nam]	 
        nam = names(temp1)
        
        
        
        totsizepred = (as.numeric(dim(temp1)[1])*as.numeric(dim(Qs)[1]))*((dim(temp1)[2]+dim(Qs)[2]))
        totmemorypred = totsizepred*4.7/1e+9
        if(mem<totmemorypred){
          
          cat(paste0("You are making very large demands upon your machine. You said that you have ", mem,
                     "GB of RAM. The parameters you entered will initialize a state-space matrix of size ",
                     round(totmemorypred,1), "GB of RAM.  Terminating now, please select a smaller problem,"
                     ,"or find a higher memory machine."))
          
          return()
        }
        temp1=temp1[, Qs[],by=nam]	
        print(mem_used())
        

        if(modeltype=='multinomiallogitpoisson'){
        
          ##Creates logit probabilities based on prices. 
          temp1[,
                (paste0("pi",seq(J))) := Map(
                  function(b,pc,p) exp(b + pc * p),
                  mget(paste0("beta", seq(J))),
                  .(pricecoef),
                  mget(paste0("p", seq(J),'s'))
                )
                ]
          
          #This section is not super readable because it is for an arbitrary number of products
          #See https://stackoverflow.com/questions/53843186/dynamic-column-calling-and-transformation-in-data-table-part-by-number-part-by-n/53843408?noredirect=1#comment94535795_53843408
          #For explanation of code
          
          temp1[, denom := 1 + Reduce(`+`, mget(paste0("pi", seq(J))) ) ]
          temp1[, (paste0("pi",seq(J))) := lapply(mget(paste0("pi", seq(J))), `/`, denom) ]
          
          temp1[, (paste0("lambdabuy",seq(J))) := lapply(mget(paste0("pi", seq(J))), `*`, lambda) ]
      
          temp1[,
                (paste0("prQ",seq(J))) := Map(
                  function(q,lam) dpois(q,lam),
                  mget(paste0("Q", seq(J))),
                  
                  mget(paste0("lambdabuy", seq(J)))
                )
                ]
          
         
          cols = c('location_name','location_id',paste0('p',seq(J),'s'),statecols,paste0('Q',seq(J)))
          
          temp2 = temp1[, lapply(.SD, mean), by=cols]
          
          cols2= c('location_name','location_id',paste0('p',seq(J),'s'),statecols,paste0('Q',seq(J)),paste0('prQ',seq(J)))
          temp2 = temp2[,cols2,with=F]
           
          fullprobs = rbind(fullprobs,temp2)
           
          
     
    }
    

  }  
  
  
  #calculate cumulative probabilities
  
  names=c(paste0('prQ',1:J))
  fullprobs[,pQ:= Reduce(`*`, .SD), .SDcols=names]
  
  
  for(j in 1:J){
    
    fullprobs[,paste0('prQ',j):=NULL]
 
    
  }
  
  return(data.table(fullprobs))
  
  
  }
  
  






























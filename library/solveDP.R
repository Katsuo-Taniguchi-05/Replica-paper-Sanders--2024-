#####################################################################
#
#
#  solveDP.R - 
#  
#            Inputs:
#            
#                 1.  parameters of DP  (parms) 
#
#                 2. locationid: The above item is for all stores. 
#                    This location refers to the store id (the field called 'location_id').  
#                    This subsets the above data table and returns the expected reward
#                    data for only one store (we allow store specific coefficients 
#                    which is why this is important.)
# 
#                 3. precomputed PrHat
#            
#            Outputs: 
#  
#                1. Optimal policy function. 
#  
#           sets up the necessary objects for the DP
# 
#           then solves the policy function DP 
# 
# 
# 
#    By Robert Evan Sanders, July 12, 2023. 
# 
####################################################################

################################################################
#
#
#                 Necessary Components constructed within
#
#   
#        R(s,a)      - reward at state s if                          
#                       you take action a
#                       
#        P(s,s',a)   - probability of transitioning                  
#                      s to s' given action a 
#
#        V(s)     - value function of every state                   
#
#        
#        Pi(s,a*) - policy function that, for every state,           
#                   suggests an action (start it as blank)          
#                                                                    
#
###################################################################


solveDP =function(parms,locationid,prHat){

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
  modeltype = parms$modeltype
  statecols = parms$statecols
  orderpolicy = parms$orderpolicy 
  if(orderpolicy=='exogenous'){
    basestock = parms$basestock}
  econometricunc=parms$econometricunc
  
  
  if(J>2){
    
   cat('You must code the J>2 case for the expected transition matrix.  This involves finding the density F(b,c,d), etc. See stackoverflow question.')
     
  }

q = Vstatecreator(parms)

As = stateactioncreator(parms,q)


####################################################
#
#
#          Execute Dynamic Programming 
#
#          Backward Induction Solution 
#          
#          Guess infinite horizon
#           
#          with Finite horizon solution. 
#
#
####################################################

#Create R
RAf = returnmat(prHat,locationid=locationid,parms)$Rsa
RA=RAf[,.(state,anum,rsa)]

#Create P
PA0=PAmats(prHat,locationid=locationid,parms)
PA = PA0$datatablePA
PA = PA[prss!=0]
#Create V, pi
Vmat = copy(q)
Vmat[,anum:=0]


#Action mat
Amat = As$AA

anum = max(As$AA$anum)

#############################################################
#
#
#  For each time period
#   Calculate EV(s,a) 
#
#############################################################

#state action data table for merges. 
stateaction = As$SA[,.(state,anum)]


#time is T

#find continuation value
for(tcp1 in max(Vmat$t):2){
  
print(tcp1)
tc   = tcp1-1    #t

states = copy(Vmat)[t==tc,state]

Vprime = copy(Vmat)[t==tcp1]
Vprime[,stateprm:=state]
Vprime[,state:=NULL]
Vprime=Vprime[,.(V,stateprm)]

stats0 = Vprime$stateprm
PAtemp = PA[stateprm%in%stats0]
pa0 = data.table(CJ(stateprm=stats0,state=states,anum=1:anum))
PAtemp2 = merge(pa0,PAtemp,by=c('state','stateprm', 'anum'),all.x=T)
PAV = merge(PAtemp2,Vprime,by='stateprm')
PAV[is.na(prss),prss:=0]

EVA = PAV[,.(EV=sum(prss*V)),by=.(state,anum)]

tempRV = merge(RA[state%in%states],EVA,by=c('state','anum'))

tempRV[,V:=rsa+EV]

pitemp=tempRV[,.(anum=anum[which.max(V)]),by=state]

Vupdate = merge(tempRV[,.(state,anum,V)],pitemp,by=c('state','anum'))

Vmat[state%in%states, V:= Vupdate[,V]]
Vmat[state%in%states, anum:= pitemp[,as.numeric(anum)]]



}




polfinal = merge(Vmat, Amat, by = c('anum'),all.x=T)
polfinal=polfinal[order(t,w,d)]
setcolorder(polfinal, c(names(Vmat), names(Amat)[-length(names(Amat))]))
polfinal=polfinal[order(state)]



######################################
# 
#  for static policy you gotta optimize
#  over which initial p you pick
#
#####################################
if(pricerule=='static'){
	
	
  pre=rep(paste0('prod',1:J,'vint'),Tp)
  pre =  paste0(pre,rep(1:Tp,each=J))
  
  temp = polfinal[(t==1 & Reduce(`&`, Map(`==`, mget(pre), 0))) ]
  temp=temp[V==V[which.max(V)]]
  
  cols = paste0('p',1:J,'s')
  pstar = as.numeric(temp[,..cols])
  
  pre=paste0('p',1:J,'sLAST')
  polfinal=	polfinal[Reduce(`&`, Map(`==`, mget(pre), pstar))]
	


}

return(list('polfinal'=polfinal,'q'=q,'Amat'=Amat))


}



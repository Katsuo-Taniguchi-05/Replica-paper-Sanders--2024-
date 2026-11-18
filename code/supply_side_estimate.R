###################################################################################
#
#
#         
#         This is a script that computes the posterior distribution of outcomes
#         under optimal static and dynamic pricing for:
#         
#         1. given set of demand-side parameters
#         2. given set of supply-side parameters. 
#         
#
#         For Organic Waste Bans and Dynamic Pricing: A Supply-Side Model
#         of Grocery Retailers
#
#         Robert Evan Sanders July 12, 2023
#
#
#################################################################################

library("data.table")
library("dplyr")
library("lobstr")

####################################
### SET PATH and Open utilities ####
####################################

source("../library/expectation.R")
source("../library/LIFOFIFOdrawdown.R")
source("../library/Vstatecreator.R")
source("../library/rewardfunction.R")
source("../library/expecteddemandDist.R")
source("../library/solveDP.R")
source("../library/wastefunction.R")
source("../library/consumersurplusfunction.R")


masterprefs = read_rds(file='../output/masterprefs.rds')
syntheticdata = read_rds(file='../data/synthetic_data.rds')

####################################
### Choose supply-side primitives ##
####################################

mc = .2                                  #The marginal cost of creating a new unit of inventory. 
gammaDxxx = .3                            #The disposal cost in dollars per unit wasted.
gammaSxxx = 0                             #This would be an explicit long-run stockout cost, paid per unit demanded in excess of inventory, if you want one.  Set to zero in paper. 
lll = masterprefs$location_name[1]        #Store name
prod = masterprefs$Product[1]             #Product name 
K0 = max(masterprefs$d)                   #Maximum day-parts, determined by demand side. 
checks = T                                #Do you want to spend the extra time to check that
																		      #the optimal policy satisfies the ACOE? If so,
																		      #put checks = T, otherwise, put checks = F. 
drawN = 50                                #Number of posterior draws you want to calculate the 
																		      #distribution of outcomes at.  This can be costly so 
																		      #don't make it too high.
J = 1                                     #This is 1 because the number of products in the demand system is 1, 
	                                        #and I optimize prices and inventories product-by-product.  Do not set to
																	      	 #J > 1. 
Tp0 = 2                                   #Shelf life of product. 
drawdown0 = 'FIFO'                        #'FIFO' or 'LIFO', depending on your desired rule for inventory drawdown when customers demand it. 
pricerule0 = 'unrestricted'               #"static" = static price, "unrestricted" = price can increase or decrease dynamically upon beginning of each daypart
                                          #we will use both as we solve for each type of policy below. 
modelofdemand0 = 'multinomiallogitpoisson' #Demand model is the binary logit with poisson arrival. Must keep this.  
planningperiods0 = 2                      #Number of weeks we solve the finite horizon policy 
                                          #for to "guess" the infinite horizon policy. 
                                          #This typically does not need to be > 2 unless 
                                          #shelf life gets longer.                                      
  


####################################
### Setup for solving the dynamic
### problems
####################################

#To save the outputs here 
UNPlist = list()  #Dynamic Pricing Policy  
SNPlist = list()  #Static Pricing Policy 


#The min allowable price is the max of the marginal cost, OR 50% of the minimum observed price, 
#The max allowable price is 75% higher than the maximum observed price. 
maxp1 =  1.75*max(syntheticdata[productc!='Outside']$price)
minp1 =  max(.5*min(syntheticdata[productc!='Outside']$price),mc)


#Work with a copy of the preference data to figure out a
#good guess for how many units should be stocked.  We will
#make sure the ordering amounts aren't ever corner solutions later
#on, but just to start we go here. 

dat3 = copy(masterprefs)
#find how many units should be stocked
maxprob = max(dat3$s_hat)
maxmean = mean(dat3$lambda)*3*maxprob*2  #Average daily orders at highest observed purchase price, times two. 


categoryvec = max(min(35,round(min(maxmean*1.7))),5) #This rule of thumb seems
																										 #to work reasonably well for guessing max state space needed for 
																										 #a given set of supply and demand primitives. 
                                                     #Take the max initial state space is the min of 35 and 70% above maxmean. Then
                                                     #if this value is below 5, just increase to 5.
wmax0 = 7                                            #Seven days in the week                       

basestock = data.table(w=1:wmax0,d=1)                 #Set up the ordering matrix. 

for(j in 1:J){
  basestock[,paste0('fillorder',j):=c(rep(categoryvec,wmax0))]
}

#Let the fineness of price grid be determined by 
#how many units there are, or else the state-action
#space becomes too big.  Of course, your own computational
#limits might dictate that you change this!
#You can select your own aaa1 = the increment of prices in
#the action space, just know that the smaller the value,
#the most costly it will be to solve the DPs.

if(categoryvec<=17){
  aaa1=.05}                             #Five cent increments if less than or equal to 17 units. 
if(categoryvec>17){
  aaa1=.15 
}                            #Fifteen cent increments if > than 17 units < 25 units. 
if(categoryvec>=25){
  aaa1=.35
}                            #Thirty-five cent increments >=25 units. 

  
medianpriceobs = median(syntheticdata[productc!='Outside']$price)  #Median observed price

##############################
#Setup the Master Supply-side
#Input Parameter List. 
##############################

parms = list(
  J = J,                                          #Number of unique products to maximize prices over
  Tp = Tp0,                                       #Shelf life of product
  a = aaa1,                                       #a = increment in prices
  minprices = rep(minp1,J),                       #Min price
  maxprices = rep(maxp1,J),                       #Max price										 
  categoryvector = rep(round(categoryvec),J),     #Maximum allowable inventory. 
  K = K0,                                         #number of unique prices per day
  wmax=wmax0,                                     #number of unique weekdays
  pricerule = pricerule0,                         #"static" = static price, "unrestricted" = price can increase or decrease dynamically upon beginning of each daypart
  planningperiods = planningperiods0,             #Number of weeks we solve the finite horizon policy for to "guess" the infinite horizon policy. This typically does not need to be > 2 unless shelf life gets longer. 
  gammaD = gammaDxxx,                             #Disposal cost
  gammaS = gammaSxxx,                             #long-run reduce form stockout cost
  c=rep(mc,J),                                    #marginal cost 
  Drawdown=drawdown0,                             #LIFO or FIFO
  ordercap = rep(round(categoryvec),J),           #Max order amount in action space, because of max allowable inventory
  mem = Inf,                                      #GB of working ram you have. The code will try to warn you 
  modeltype= modelofdemand0,                      #Demand model. 
  statecols=c('w','d'),                           #state variables that affect demand, do not change. 
  maxq = round(categoryvec)*2,                    #maxq for probability distribution of possible demand realizations. Need this to be sufficiently large but the larger it is, the most costly it is computationally. 
  tolerance=10^-8,                                #set vectors of demand realizations that have probability less than this tolerance equal to zero, reduces computational burden. 
  econometricunc = T,                             #Keep as true, want to get posterior distribution of outcomes. 
  orderpolicy = 'endogenous',                     #"endogeneous", "exogenous". Keep as "endogenous" so orders and prices are jointly optimized. 
  basestock = basestock,                          #Keep for data construction in DP solver.
  obspricekeep = medianpriceobs                   #Median observed price. useful to know profitability here as a benchmark. 
)

parmso = copy(parms)

#Example state and state-action space. 
q = Vstatecreator(parms)
As = stateactioncreator(parms,q)


###############################
#Setup how preference
#parameters will be used by
#the DP for optimization.
###############################

statecols = c('w','d')

##############################################################################
###Melt this and make preference parameters instead a function of 
###the state, weekday vs. weekend.
masterprefs[,Product:=NULL]
weekends = sort(c(parms$wmax,parms$wmax-1))
weekdays = 1:parms$wmax
weekdays = weekdays[which(!weekdays%in%weekends)]

days = unique(q[,.(w)])
days[w%in%weekdays,Weekend:=0]
days[w%in%weekends,Weekend:=1]

DT.m2 = merge(masterprefs,days,by='Weekend',all.x=T,all.y=T,allow.cartesian = T)
DT.m2 = DT.m2[complete.cases(DT.m2[, statecols, with=FALSE])]
drawlocationvalue=copy(DT.m2)


#Some transformations if homogeneous logit to make
#pref data easier to work with.
drawlocationvalue=drawlocationvalue[order(location_name,r,w,d)]  
  

#Make single intercept col
drawlocationvalue[,beta1:=Intercept_1]

if(K0>1){
	for(kk in 2:K0){
drawlocationvalue[d==kk,beta1:=get(paste0('Intercept_',kk))]
}
	for(kk in 2:K0){
drawlocationvalue[, paste0('Intercept_',kk):=NULL]
}
}

setnames(drawlocationvalue,'Price','pricecoef')


lll1 = unique(drawlocationvalue$location_id)

#Draws to keep for posterior outcome distribution. 
draws = floor(seq(1,max(drawlocationvalue$r), length.out=drawN))

#unrestricted price
prHat = prgen(drawlocationvalue[r%in%draws],parms)
prHat_fullstatic = copy(prHat)  

#Static price, faster to loop
prices=unique(As$AA$p1s)
listofstatics=list()
listofprofits = list()

######################################
#
#
#    Find the Optimal 
#    
#    Static Policy Function 
#
#
######################################

#Loop over set of feasible prices. 
for(sp in 1:length(prices)){
  print(paste0('Now trying the ',sp,'th price of ',length(prices),' prices.'))
  parms$pricerule='static'
  parms$minprices=prices[sp]
  parms$maxprices=prices[sp]
  prHat2 = prHat_fullstatic[p1s==prices[sp]]
  SN = solveDP(parms,lll1,prHat2)$polfinal
  listofstatics[[sp]]=SN
  listofprofits[[sp]]=SN[1]$V
  #Once we have two decreases in a row, stop looking for optimum price. 
  if(sp>2){
    if(listofprofits[[sp]]<listofprofits[[sp-1]] & listofprofits[[sp-1]]<listofprofits[[sp-2]]){break}
  }
}

#find best static price:
profits = unlist(listofprofits)
keeper = which(profits==max(profits))
if(length(keeper)==1){
  SN = listofstatics[[keeper]]
}else{
  SN = listofstatics[[1]]
}
print(SN)

Order = max(SN[d==1]$order1,na.rm=T)
#get the most possibly EVER ordered and had on hand
SNo = max(SN[d==1][prod1vint1==Order & t<max(t)]$order1+Order)
#Get the max state space quantity
SNmaxC = max(SN$prod1vint1)
converge = 999
#Increase the grid state until inventory is not binding        
#or Value function doesnt improve more than half a cent. 
while(SNo==SNmaxC & converge>.01){
      print(converge)
      SNold=copy(SN)
      print(SNo-SNmaxC)
      categoryvec = categoryvec+1   
      for(j in 1:J){
        basestock[,paste0('fillorder',j):=c(rep(categoryvec,wmax0))]
      }  
        
      parms$basestock=basestock
      parms$categoryvector=rep(round(categoryvec),J)
      parms$ordercap = rep(round(categoryvec),J)
      parms$maxq = round(categoryvec)*2
      parms$minprices=SN$p1s[1]
      parms$maxprices=SN$p1s[1]
      
      prHat = prgen(drawlocationvalue[r%in%draws],parms)
      prHat_fullstatic = copy(prHat)  

      SN = solveDP(parms,lll1,prHat)$polfinal

      Order = max(SN[d==1]$order1,na.rm=T)
      #get the most possibly EVER ordered and had on hand
      SNo = max(SN[d==1][prod1vint1==Order & t<max(t)]$order1+Order)
      #Get the max state space quantity
      SNmaxC = max(SN$prod1vint1)
      converge = SN[1]$V-SNold[1]$V
  
}

####################################################
# 
# 
# 
# 
# 
#         Calculate Distribution of outcomes
#           For optimal Static pricing (SN) 
# 
#     Corresponding steps described in the paper. 
# 
# 
# 
# 
# 
####################################################
q = Vstatecreator(parms)
As = stateactioncreator(parms,q)
#####################################  
#1. Save policy for first week
#####################################  
SNcandidate = SN[t<=(parms$wmax*parms$K)]

#Get the optimal static price
SN_P_opt = SNcandidate$p1s[1]

#Find the action-state transition matrix. 
parms_optimumSN = copy(parms)
parms_optimumSN$pricerule='static'
parms_optimumSN$minprices=SN_P_opt
parms_optimumSN$maxprices=SN_P_opt

prHat_list_SN = list()
transitionmat_list_SN = list()
stationary_list_SN = list()
reward_list_SN = data.table('r' = draws,'profit_per_period'=0)
CS_list_SN = data.table('r' = draws,'CS_per_period'=0)
waste_list_SN = data.table('r' = draws,'waste_per_period'=0)

for(rr in draws){
  print(rr)
  #get demand probabilities for each state action
  prHat_list_SN[[rr]]=  prgen(drawlocationvalue[r==rr],parms_optimumSN)
  
  #Find the action-state transition matrix. 
  transitionmat_list_SN[[rr]] = PAmats_with_policy(prHat_list_SN[[rr]],lll1,parms_optimumSN,SN)
  
  #find the stationary distribution of states for each draw
  #reshape to an SXS given optimal action at each step
  
  statetransmat = CJ('state'=transitionmat_list_SN[[rr]]$q[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]$state,
                     'stateprm'=transitionmat_list_SN[[rr]]$q[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]$state)
  
  statetransmat_action_opt = merge(statetransmat,SN[,.(state,anum)],by='state')
  
  #now get the nonzero state transition probs
  non_zero_probs = transitionmat_list_SN[[rr]]$datatablePA

  #For each state, keep only the optimal action
  non_zero_probs = merge(non_zero_probs,SN[,.(state,anum)],by=c('state','anum'))
  
  #get the problem states
  probstates = unique(transitionmat_list_SN[[rr]]$q[t==(parms_optimumSN$wmax*parms_optimumSN$K)]$state)
  
  
  non_zero_probs_correct = non_zero_probs[state<max(transitionmat_list_SN[[rr]]$q[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]$state) & (!state%in%probstates)]
  non_zero_probs_fix = non_zero_probs[state%in%probstates]
  
  #Fix the last state transition since it loops back into itself.
  renamelaststate = transitionmat_list_SN[[rr]]$q[t==(parms_optimumSN$wmax*parms_optimumSN$K+1)]
  renamelaststate[,t:=NULL]
  renamelaststate[,V:=NULL]
  setnames(renamelaststate,'state','old_state')
 
  looperstate=copy(transitionmat_list_SN[[rr]]$q)
  looperstate=looperstate[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]
  looperstate[,t:=NULL]
  looperstate[,V:=NULL]
  
  #find the correct state for the second week, 
  #basically wrapping the policy back into itself.
  
  fixend = merge(looperstate,renamelaststate,by=c('d','w',paste0(rep('prod',parms_optimumSN$Tp*parms_optimumSN$J),
                                                         rep(1:parms_optimumSN$J,each=parms_optimumSN$Tp),
                                                         rep('vint',parms_optimumSN$Tp*parms_optimumSN$J),
                                                         rep(1:parms_optimumSN$Tp,parms_optimumSN$J)),
                                                  paste0('p',1:parms_optimumSN$J,'sLAST'))) #need to add in for SP. 
  
  fixend_short = copy(fixend)[,.(state,old_state)]
  fixend_short_prm = copy(fixend_short)
  setnames(fixend_short_prm,'state','stateprm_fix')
  setnames(fixend_short_prm,'old_state','stateprm')
  
  non_zero_probs_fix = merge(non_zero_probs_fix,fixend_short_prm,by='stateprm')
  non_zero_probs_fix[,stateprm:=stateprm_fix]
  non_zero_probs_fix[,stateprm_fix:=NULL]
  setcolorder(non_zero_probs_fix, colnames(non_zero_probs_correct))
  
  non_zero_probs_fix_complete = rbind(non_zero_probs_correct,non_zero_probs_fix)
  
  #Merge in the nonzero probs
  statetransmat_withprobs = merge(statetransmat,non_zero_probs_fix_complete,by=c("state", "stateprm"),all=T)
  statetransmat_withprobs[is.na(prss),prss:=0]
  
  #Turn this into an SxS matrix
  statetransmat_withprobs_mat_SN = matrix(statetransmat_withprobs$prss,nrow=max(statetransmat_withprobs$state),ncol=max(statetransmat_withprobs$state),byrow = T)

  # Because it is a markov, let's take stationary distribution of states
  #by raising state transition matrix to a high power.   

  #Because the markov chain is periodic, the power you raise to tells you exactly where
  #you will be in terms of day,week.  So we need to get all K*wmax dayparts since you spend
  #an even amount of time in each of these. Then we take the stationary distributions 
  #and average over them. 
  # stackedmats = list()
  # 

  
  vec = t(matrix(c(1,rep(0,nrow(statetransmat_withprobs_mat_SN)-1)),
                 nrow(statetransmat_withprobs_mat_SN)))
    
  veclist = list()
  
  for(i in 1:40){
    
    vec = vec%*%statetransmat_withprobs_mat_SN
    
  }
  tempvec = copy(vec)
  cummat = copy(tempvec)
  
  for(i in 1:(parms_optimumSN$K*parms_optimumSN$wmax-1)){
        print(i)
        tempvec = tempvec%*%statetransmat_withprobs_mat_SN
        cummat = cummat + tempvec
   }
  addedmats = t(cummat/(parms_optimumSN$K*parms_optimumSN$wmax))
  
  
  #Hit it with an arbitrary state
  #
  # 
  Stationary_Dist=copy(addedmats)
  Stationary_Dist_DT = data.table(Stationary_Dist)
  setnames(Stationary_Dist_DT,'V1','Probability')
  Stationary_Dist_DT[,state:=1:.N]
  
  #Do some checks. 
  statereader = transitionmat_list_SN[[rr]]$q[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]
  statereader[,V:=NULL]
  Stationary_Dist_DT_check = merge(Stationary_Dist_DT,statereader,by='state')
  Stationary_Dist_DT[,r:=rr]
  
  #save the stationary dist       
  stationary_list_SN[[rr]] = Stationary_Dist_DT

  #now that we have stationary distribution for a draw, we need to
  #find the expected Profit, CS, and Waste 
  
  #Profit
  #Find the current period reward in each state
  reward_all = returnmat(prHat_list_SN[[rr]],lll1,parms_optimumSN)$Rsa
  reward=merge(reward_all[t<=(parms_optimumSN$wmax*parms_optimumSN$K)],SN[,.(state,anum)],by=c('state','anum'))
  reward = reward[,.(state,rsa)]
  
  reward = merge(Stationary_Dist_DT,reward,by='state')
  reward[,e_reward:=rsa*Probability]
  reward_exp = sum(reward$e_reward)
  reward_list_SN[r==rr,profit_per_period:=reward_exp]
  
  #Waste 
  waste_all = wastefunction(prHat_list_SN[[rr]],lll1,parms_optimumSN)$Wsa
  waste=merge(waste_all[t<=(parms_optimumSN$wmax*parms_optimumSN$K)],SN[,.(state,anum)],by=c('state','anum'))
  waste = waste[,.(state,Wsa)]
  
  waste = merge(Stationary_Dist_DT,waste,by='state')
  waste[,e_reward:=Wsa*Probability]
  waste_exp = sum(waste$e_reward)
  waste_list_SN[r==rr,waste_per_period:=waste_exp]
  
  #####################################  
  #1. Save policy for first week
  #####################################  
  SNcandidate = SN[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]

  #Consumer surplus (this is slow because of the integral over normals)
  cs = consumersurplus(drawlocationvalue[r==rr],lll1,parms_optimumSN,SNcandidate)
  cs = merge(Stationary_Dist_DT,cs,by='state')
  cs[,e_cs:=cs*Probability]
  cs_exp = sum(cs$e_cs)
  CS_list_SN[r==rr,CS_per_period:=cs_exp]

}


############################
# 
# Stationary Distribution
# 
#   ACOE check 
#
#  Static Policy 
# 
############################
  
#####################################  
# Save policy for first week
#####################################  
SNcandidate = SN[t<=(parms$wmax*parms$K)]

#####################################  
#  Get SXS transition matrix given the policy. 
#####################################  

#Get the optimal static price
SN_P_opt = SNcandidate$p1s[1]

#Find the demand at every state-action 
prHat_SN = prHat[p1s==SN_P_opt]

#Find the action-state transition matrix. 
parms_optimumSN = copy(parms)
parms_optimumSN$pricerule='static'
parms_optimumSN$minprices=SN_P_opt
parms_optimumSN$maxprices=SN_P_opt

transitionmat = PAmats(prHat_SN,lll1,parms_optimumSN)

#reshape to an SXS given optimal action at each step

statetransmat = CJ('state'=transitionmat$q[t<=(parms$wmax*parms_optimumSN$K)]$state,
                 'stateprm'=transitionmat$q[t<=(parms$wmax*parms_optimumSN$K)]$state)

statetransmat_action_opt = merge(statetransmat,SN[,.(state,anum)],by='state')

#now get the nonzero state transition probs
non_zero_probs = transitionmat$datatablePA

#For each state, keep only the optimal action
non_zero_probs = merge(non_zero_probs,SN[,.(state,anum)],by=c('state','anum'))

#get the problem states
probstates = unique(transitionmat$q[t==(parms$wmax*parms_optimumSN$K)]$state)


non_zero_probs_correct = non_zero_probs[state<max(transitionmat$q[t<=(parms$wmax*parms_optimumSN$K)]$state) & (!state%in%probstates)]
non_zero_probs_fix = non_zero_probs[state%in%probstates]

#Fix the last state transition since it loops back into itself.
renamelaststate = transitionmat$q[t==(parms$wmax*parms_optimumSN$K+1)]
renamelaststate[,t:=NULL]
renamelaststate[,V:=NULL]
setnames(renamelaststate,'state','old_state')

looperstate=copy(transitionmat$q)
looperstate=looperstate[t<=(parms$wmax*parms_optimumSN$K)]
looperstate[,t:=NULL]
looperstate[,V:=NULL]

#find the correct state for the second week, 
#basically wrapping the policy back into itself.

fixend = merge(looperstate,renamelaststate,by=c('d','w',paste0(rep('prod',parms_optimumSN$Tp*parms_optimumSN$J),
                                                     rep(1:parms_optimumSN$J,each=parms_optimumSN$Tp),
                                                     rep('vint',parms_optimumSN$Tp*parms_optimumSN$J),
                                                     rep(1:parms_optimumSN$Tp,parms_optimumSN$J)
                                                     ),paste0('p',1:parms_optimumSN$J,'sLAST')
                                              
                                              ))

fixend_short = copy(fixend)[,.(state,old_state)]
fixend_short_prm = copy(fixend_short)
setnames(fixend_short_prm,'state','stateprm_fix')
setnames(fixend_short_prm,'old_state','stateprm')

non_zero_probs_fix = merge(non_zero_probs_fix,fixend_short_prm,by='stateprm')
non_zero_probs_fix[,stateprm:=stateprm_fix]
non_zero_probs_fix[,stateprm_fix:=NULL]
setcolorder(non_zero_probs_fix, colnames(non_zero_probs_correct))

non_zero_probs_fix_complete = rbind(non_zero_probs_correct,non_zero_probs_fix)

# statetransmat[,i:=1:.N]
# non_zero_probs_fix_complete[,i2:=1:.N]
# 
#Merge in the nonzero probs
statetransmat_withprobs = merge(statetransmat,non_zero_probs_fix_complete,by=c("state", "stateprm"),all=T)
statetransmat_withprobs[is.na(prss),prss:=0]

#Turn this into an SxS matrix
statetransmat_withprobs_mat_SN = matrix(statetransmat_withprobs$prss,nrow=max(statetransmat_withprobs$state),ncol=max(statetransmat_withprobs$state),byrow = T)

#####################################
# Verify the solution solves for rho. 
#####################################

#Because the markov chain is periodic, the power you raise to tells you exactly where
#you will be in terms of day,week.  So we need to get all K*wmax dayparts since you spend
#an even amount of time in each of these. Then we take the stationary distributions 
#and average over them. 
stackedmats = list()

vec = t(matrix(c(1,rep(0,nrow(statetransmat_withprobs_mat_SN)-1)),
   nrow(statetransmat_withprobs_mat_SN)))

veclist = list()

for(i in 1:40){
  
  vec = vec%*%statetransmat_withprobs_mat_SN
  
}
tempvec = copy(vec)
cummat = copy(tempvec)

for(i in 1:(parms_optimumSN$K*parms_optimumSN$wmax-1)){
      print(i)
      tempvec = tempvec%*%statetransmat_withprobs_mat_SN
      cummat = cummat + tempvec
 }
addedmats = t(cummat/(parms_optimumSN$K*parms_optimumSN$wmax))

Stationary_Dist_SP = copy(addedmats)
Stationary_Dist_DT = data.table(Stationary_Dist_SP)
setnames(Stationary_Dist_DT,'V1','Probability')
Stationary_Dist_DT[,state:=1:.N]

#Do some checks. 
statereader = transitionmat$q[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]
statereader[,V:=NULL]
Stationary_Dist_DT_check = merge(Stationary_Dist_DT,statereader,by='state')

#Save stationary dist and transition matrix
Stationary_Dist_DT_check_SN = copy(Stationary_Dist_DT_check)
statetransmat_withprobs_SN_output = copy(statetransmat_withprobs)
statetransmat_withprobs_SN_output = statetransmat_withprobs_SN_output[prss!=0]

if(checks==T){
	
		#####################################  
		# solve linear equation for rho and h
		#####################################  
		#Calculate reward function by state. 
		reward_all = returnmat(prHat_SN,lll1,parms_optimumSN)$Rsa
		reward=merge(reward_all[t<=(parms$wmax*parms_optimumSN$K)],SN[,.(state,anum)],by=c('state','anum'))
		reward = reward[,.(state,rsa)]
		reward_mat = matrix(-reward$rsa,nrow=dim(reward)[1])
		
		#Create Invertible Matrix to Solve the Linear System of Equations Verifying the ACOE Condition
		P_mat_new = copy(statetransmat_withprobs_mat_SN)
		P_mat_new = P_mat_new - diag(nrow(P_mat_new))
		P_mat_new=P_mat_new[,-1]
		P_mat_new=cbind(P_mat_new,-1)


    #Calculate stationary rho(scalar) = dist * reward, which is the expected reward weighted across
    #all states, first calculate the expected reward at each state, given the policy.  
    P_mat_new_inv = solve(P_mat_new) 
	  
	  #Solution
	  Solution = P_mat_new_inv%*%reward_mat
	  

    reward_all = returnmat(prHat_SN,lll1,parms_optimumSN)$Rsa
    reward=merge(reward_all,SN[,.(state,anum)],by=c('state','anum'))
    reward = merge(reward,Stationary_Dist_DT,by='state')
    
    #rho
    reward[,expreward:=rsa*Probability]
    rho = sum(reward$expreward)
   
    #SAVE THIS IN OUTPUT
    SP_rho_difference = abs(Solution[nrow(Solution),1]-rho)
      
  #####################################
  # Verify the solution solves ACOE
  #####################################        
  #Now we have rho and h, we need to check rho + h = max_a (r(a) + P(a)*h(a)
  h_candidate = cbind(c(0,Solution[-nrow(Solution),]))
  h_candidate_datatable = data.table('state'=1:dim(h_candidate)[1],'h'=h_candidate[,1])
  rho_cand = Solution[nrow(Solution),1]          
          
  #Now, create r(s,a)(SxA matrix) and stacked Probability transition matrix so we can calc 
  #RHS of ACOE
  reward_all = reward_all[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]
  reward_mat_creator = reward_all[,.(state,anum,rsa)]
  
  #Turn this into an SxA matrix
  reward_mat = matrix(reward_mat_creator$rsa,nrow=max(reward_mat_creator$state),ncol=max(reward_mat_creator$anum),byrow = T)
 
  #lets get expected h hit by transition prob matrix. 
  #now get the nonzero state transition probs
  non_zero_probs = transitionmat$datatablePA
  
  #############################
  #Need to remake the P mat
  #but for every A this time
  #############################
  #get the problem states
  probstates = unique(transitionmat$q[t==parms$wmax*parms$K]$state)
  
  non_zero_probs_correct = non_zero_probs[state<max(transitionmat$q[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]$state) & (!state%in%probstates)]
  non_zero_probs_fix = non_zero_probs[state%in%probstates]
  
  #Fix the last state transition since it loops back into itself.
  renamelaststate = transitionmat$q[t==(parms_optimumSN$wmax*parms_optimumSN$K+1)]
  renamelaststate[,t:=NULL]
  renamelaststate[,V:=NULL]
  setnames(renamelaststate,'state','old_state')
 
  looperstate=copy(transitionmat$q)
  looperstate=looperstate[t<=(parms_optimumSN$wmax*parms_optimumSN$K)]
  looperstate[,t:=NULL]
  looperstate[,V:=NULL]
  
  #find the correct state for the second week, 
  #basically wrapping the policy back into itself.
  
  fixend = merge(looperstate,renamelaststate,by=c('d','w',paste0(rep('prod',parms_optimumSN$Tp*parms_optimumSN$J),
                                                         rep(1:parms_optimumSN$J,each=parms_optimumSN$Tp),
                                                         rep('vint',parms_optimumSN$Tp*parms_optimumSN$J),
                                                         rep(1:parms_optimumSN$Tp,parms_optimumSN$J)
                                                         ),paste0('p',1:parms_optimumSN$J,'sLAST')
                                                  
                                                  ))
  
  fixend_short = copy(fixend)[,.(state,old_state)]
  fixend_short_prm = copy(fixend_short)
  setnames(fixend_short_prm,'state','stateprm_fix')
  setnames(fixend_short_prm,'old_state','stateprm')
  
  non_zero_probs_fix = merge(non_zero_probs_fix,fixend_short_prm,by='stateprm')
  non_zero_probs_fix[,stateprm:=stateprm_fix]
  non_zero_probs_fix[,stateprm_fix:=NULL]
  setcolorder(non_zero_probs_fix, colnames(non_zero_probs_correct))
  
  non_zero_probs_fix_complete = rbind(non_zero_probs_correct,non_zero_probs_fix)
	
	#Lets calculate the RHS = max R + Eh
	
	R = reward_all[,.(state,anum,rsa)]
	
	H_prime = copy(h_candidate_datatable)[,.(state,h)]   #V is h(s) fyi. 
	setnames(H_prime,'state','stateprm')  
	        
	pmat = copy(non_zero_probs_fix_complete)
	 
	#So lets calculate Pmat*H
	PH = merge(pmat,H_prime,by=c('stateprm'))
	 
	PHs = PH[,.(Hprime = sum(prss*h)),by=.(state,anum)]
	PHs = PHs[order(state,anum)]
	 
	RHS = merge(R,PHs,by=c('state','anum'),all=T)
	RHS[is.na(Hprime),Hprime:=0]
	 
	RHS[,Total:=rsa+Hprime]
	RHS=RHS[,.(Total=max(Total)),by=state]
	
	#check RHS and LHS. Hope diff is small.
	LHS = copy(h_candidate_datatable)
	setnames(LHS,'h','rho_plus_h')
	LHS[,rho_plus_h:=rho_plus_h+rho_cand]
	 
	bothsides = merge(LHS,RHS, by='state')
	bothsides[,diff:=abs(Total-rho_plus_h)]
	
	#####################################
	# check max diff between LHS 
	# and RHS
	#####################################      
	     
	#This should be real close to zero if indeed the solution is correct. 
	SP_maxdifference = max(bothsides$diff)
	
	
	}



#############################################################################
#
#
#
#
#  Solve for the optimal dynamic pricing policy now. 
#
#
#
#
############################################################################


################################################################
#Make 
#sure that you don't cut off the inventory state space too much
#or you will end up having UN<SN.
#
#The below snippet just makes sure we explore a bit more of the state 
#space if its close to binding in the static policy. 
#
#
#
################################################################


if(is.data.table(SN)==FALSE){
  dregs = copy(SN[[1]][t==1 & order1>0])
  dregs[,bigone:=prod1vint1+order1]
  categoryvec = round(max(7,dregs[t==1]$bigone*1.3))
  pricestatical = copy(SN[[1]][t==1 & order1>0,p1s])[1]
  
  if(categoryvec>39){categoryvec=max(dregs[t==1]$bigone+1)} #39 is arbitrary, just where my computer runs out of memory
  
}else{
  
  
  dregs = copy(SN[t==1 & order1>0])
  dregs[,bigone:=prod1vint1+order1]
  categoryvec = round(max(7,dregs[t==1]$bigone*1.3))
  pricestatical = copy(SN[t==1 & order1>0,p1s])[1]
  
  if(categoryvec>39){categoryvec=max(dregs[t==1]$bigone+1)} #39 is arbitrary, just where my computer runs out of memory
  
}


pricerule0='unrestricted'


#####################################
#We can't have it so that
#the same set of feasible prices
#is not present in both 
#DP's, otherwise DP could do worse
#since it can't simply pick the 
#SP price!
#####################################

for(j in 1:J){
  basestock[,paste0('fillorder',j):=c(rep(categoryvec,wmax0))]
}



parms = list(
  J = J,                                          #Number of unique products to maximize prices over
  Tp = Tp0,                                       #Shelf life of product
  a = aaa1,                                       #a = increment in prices
  minprices = rep(minp1,J),                       #Min price
  maxprices = rep(maxp1,J),                       #Max price										 
  categoryvector = rep(round(categoryvec),J),     #Maximum allowable inventory. 
  K = K0,                                         #number of unique prices per day
  wmax=wmax0,                                     #number of unique weekdays
  pricerule = pricerule0,                         #"static" = static price, "unrestricted" = price can increase or decrease dynamically upon beginning of each daypart
  planningperiods = planningperiods0,             #Number of weeks we solve the finite horizon policy for to "guess" the infinite horizon policy. This typically does not need to be > 2 unless shelf life gets longer. 
  gammaD = gammaDxxx,                             #Disposal cost
  gammaS = gammaSxxx,                             #long-run reduce form stockout cost
  c=rep(mc,J),                                    #marginal cost 
  Drawdown=drawdown0,                             #LIFO or FIFO
  ordercap = rep(round(categoryvec),J),           #Max order amount in action space, because of max allowable inventory
  mem = Inf,                                      #GB of working ram you have. The code will try to warn you 
  modeltype= modelofdemand0,                      #Demand model. 
  statecols=c('w','d'),                           #state variables that affect demand, do not change. 
  maxq = round(categoryvec)*2,                    #maxq for probability distribution of possible demand realizations. Need this to be sufficiently large but the larger it is, the most costly it is computationally. 
  tolerance=10^-8,                                #set vectors of demand realizations that have probability less than this tolerance equal to zero, reduces computational burden. 
  econometricunc = T,                             #Keep as true, want to get posterior distribution of outcomes. 
  orderpolicy = 'endogenous',                     #"endogeneous", "exogenous". Keep as "endogenous" so orders and prices are jointly optimized. 
  basestock = basestock,                          #Keep for data construction in DP solver.
  obspricekeep = medianpriceobs                   #Median observed price. useful to know profitability here as a benchmark. 
)


#If we have massive state spaces, limit the size of the action space.  
#You can comment these out, but beware unless you have a lot of memory. 

if(categoryvec>25){
  
  parms$minprices=max(pricestatical-2,minp1)
  parms$maxprices=min(pricestatical+2,maxp1)
  if(pricestatical==max(prices)){parms$maxprices=pricestatical}
  
}
if(categoryvec>35){
  
  parms$minprices=max(pricestatical-1,minp1)
  parms$maxprices=min(pricestatical+1,maxp1)
  if(pricestatical==max(prices)){parms$maxprices=pricestatical}
  
}



#unrestricted price
prHat = prgen(drawlocationvalue[r%in%draws],parms)
prHat_dynamic = copy(prHat)

q = Vstatecreator(parms)
As = stateactioncreator(parms,q)

UN = solveDP(parms,lll1,prHat_dynamic)$polfinal

print(UN)

#Make sure the max inventory is not binding 
Order = max(UN[d==1]$order1,na.rm=T)
#get the most possibly EVER ordered and had on hand
UNo = max(UN[d==1][prod1vint1==Order & t<max(t)]$order1+Order)
#Get the max state space quantity
UNmaxC = max(UN$prod1vint1)
converge = 999
#Increase the grid state until inventory is not binding        
#or Value function doesnt improve more than half a cent. 
while(UNo==UNmaxC & converge>.01){
      print(converge)
      UNold=copy(UN)
      print(UNo-UNmaxC)
      categoryvec = categoryvec+1   
      for(j in 1:J){
        basestock[,paste0('fillorder',j):=c(rep(categoryvec,wmax0))]
      }  
        
      parms$basestock=basestock
      parms$categoryvector=rep(round(categoryvec),J)
      parms$ordercap = rep(round(categoryvec),J)
      parms$maxq = round(categoryvec)*2
 
      prHat = prgen(drawlocationvalue[r%in%draws],parms)
      prHat_dynamic = copy(prHat)  

      UN = solveDP(parms,lll1,prHat)$polfinal

      Order = max(UN[d==1]$order1,na.rm=T)
      #get the most possibly EVER ordered and had on hand
      UNo = max(UN[d==1][prod1vint1==Order & t<max(t)]$order1+Order)
      #Get the max state space quantity
      UNmaxC = max(UN$prod1vint1)
      converge = UN[1]$V-UNold[1]$V
  
}


parmso = copy(parms)
####################################################
# 
# 
# 
# 
# 
#         Calculate Distribution of outcomes
#           For Dynamic pricing (UN)
# 
# 
# 
# 
####################################################

prHat_list_UN = list()
transitionmat_list_UN = list()
stationary_list_UN = list()
reward_list_UN = data.table('r' = draws,'profit_per_period'=0)
CS_list_UN = data.table('r' = draws,'CS_per_period'=0)
waste_list_UN = data.table('r' = draws,'waste_per_period'=0)

for(rr in draws){
  print(rr)
  #get demand probabilities for each state action
  prHat_list_UN[[rr]]=  prgen(drawlocationvalue[r==rr],parms)
  
  #Find the action-state transition matrix. 
  transitionmat_list_UN[[rr]] = PAmats_with_policy(prHat_list_UN[[rr]],lll1,parms,UN)
  
  #find the stationary distribution of states for each draw
  #reshape to an SXS given optimal action at each step
  
  statetransmat = CJ('state'=transitionmat_list_UN[[rr]]$q[t<=(parms$wmax*parms$K)]$state,
                     'stateprm'=transitionmat_list_UN[[rr]]$q[t<=(parms$wmax*parms$K)]$state)
  
  statetransmat_action_opt = merge(statetransmat,UN[,.(state,anum)],by='state')
  
  #now get the nonzero state transition probs
  non_zero_probs = transitionmat_list_UN[[rr]]$datatablePA

  #For each state, keep only the optimal action
  non_zero_probs = merge(non_zero_probs,UN[,.(state,anum)],by=c('state','anum'))
  
  #get the problem states
  probstates = unique(transitionmat_list_UN[[rr]]$q[t==(parms$wmax*parms$K)]$state)
  
  
  non_zero_probs_correct = non_zero_probs[state<max(transitionmat_list_UN[[rr]]$q[t<=(parms$wmax*parms$K)]$state) & (!state%in%probstates)]
  non_zero_probs_fix = non_zero_probs[state%in%probstates]
  
  #Fix the last state transition since it loops back into itself.
  renamelaststate = transitionmat_list_UN[[rr]]$q[t==(parms$wmax*parms$K+1)]
  renamelaststate[,t:=NULL]
  renamelaststate[,V:=NULL]
  setnames(renamelaststate,'state','old_state')
 
  looperstate=copy(transitionmat_list_UN[[rr]]$q)
  looperstate=looperstate[t<=(parms$wmax*parms$K)]
  looperstate[,t:=NULL]
  looperstate[,V:=NULL]
  
  #find the correct state for the second week, 
  #basically wrapping the policy back into itself.
  
  fixend = merge(looperstate,renamelaststate,by=c('d','w',paste0(rep('prod',parms$Tp*parms$J),
                                                         rep(1:parms$J,each=parms$Tp),
                                                         rep('vint',parms$Tp*parms$J),
                                                         rep(1:parms$Tp,parms$J)
                                                         ))) #need to add in for SP. 
  
  fixend_short = copy(fixend)[,.(state,old_state)]
  fixend_short_prm = copy(fixend_short)
  setnames(fixend_short_prm,'state','stateprm_fix')
  setnames(fixend_short_prm,'old_state','stateprm')
  
  non_zero_probs_fix = merge(non_zero_probs_fix,fixend_short_prm,by='stateprm')
  non_zero_probs_fix[,stateprm:=stateprm_fix]
  non_zero_probs_fix[,stateprm_fix:=NULL]
  setcolorder(non_zero_probs_fix, colnames(non_zero_probs_correct))
  
  non_zero_probs_fix_complete = rbind(non_zero_probs_correct,non_zero_probs_fix)
  
 
  #Merge in the nonzero probs
  statetransmat_withprobs = merge(statetransmat,non_zero_probs_fix_complete,by=c("state", "stateprm"),all=T)
  statetransmat_withprobs[is.na(prss),prss:=0]
  
  #Turn this into an SxS matrix
  statetransmat_withprobs_mat_UN = matrix(statetransmat_withprobs$prss,nrow=max(statetransmat_withprobs$state),ncol=max(statetransmat_withprobs$state),byrow = T)

  vec = t(matrix(c(1,rep(0,nrow(statetransmat_withprobs_mat_UN)-1)),
           nrow(statetransmat_withprobs_mat_UN)))

  veclist = list()
  
  for(i in 1:40){
    
    vec = vec%*%statetransmat_withprobs_mat_UN
    
  }
  tempvec = copy(vec)
  cummat = copy(tempvec)
  
  for(i in 1:(parms$K*parms$wmax-1)){
        print(i)
        tempvec = tempvec%*%statetransmat_withprobs_mat_UN
        cummat = cummat + tempvec
   }
  addedmats = t(cummat/(parms$K*parms$wmax))


  Stationary_Dist = copy(addedmats)
  
  Stationary_Dist_DT = data.table(Stationary_Dist)
  setnames(Stationary_Dist_DT,'V1','Probability')
  Stationary_Dist_DT[,state:=1:.N]
  
  #Do some checks. 
  statereader = transitionmat_list_UN[[rr]]$q[t<=(parms$wmax*parms$K)]
  statereader[,V:=NULL]
  Stationary_Dist_DT_check = merge(Stationary_Dist_DT,statereader,by='state')
  Stationary_Dist_DT[,r:=rr]
  
  #save the stationary dist       
  stationary_list_UN[[rr]] = Stationary_Dist_DT

  #now that we have stationary distribution for a draw, we need to
  #find the expected Profit, CS, and Waste 
  
  #Profit
  #Find the current period reward in each state
  reward_all = returnmat(prHat_list_UN[[rr]],lll1,parms)$Rsa
  reward=merge(reward_all[t<=(parms$wmax*parms$K)],UN[,.(state,anum)],by=c('state','anum'))
  reward = reward[,.(state,rsa)]
  
  reward = merge(Stationary_Dist_DT,reward,by='state')
  reward[,e_reward:=rsa*Probability]
  reward_exp = sum(reward$e_reward)
  reward_list_UN[r==rr,profit_per_period:=reward_exp]
  
  #Waste 
  waste_all = wastefunction(prHat_list_UN[[rr]],lll1,parms)$Wsa
  waste=merge(waste_all[t<=(parms$wmax*parms$K)],UN[,.(state,anum)],by=c('state','anum'))
  waste = waste[,.(state,Wsa)]
  
  waste = merge(Stationary_Dist_DT,waste,by='state')
  waste[,e_reward:=Wsa*Probability]
  waste_exp = sum(waste$e_reward)
  waste_list_UN[r==rr,waste_per_period:=waste_exp]
  
  #####################################  
  #1. Save policy for first week
  #####################################  
  UNcandidate = UN[t<=(parms$wmax*parms$K)]

  #Consumer surplus (this is slow because of the integral over normals)
  cs = consumersurplus(drawlocationvalue[r==rr],lll1,parms,UNcandidate)

  cs = merge(Stationary_Dist_DT,cs,by='state')
  cs[,e_cs:=cs*Probability]
  cs_exp = sum(cs$e_cs)
  CS_list_UN[r==rr,CS_per_period:=cs_exp]


  
}


########################################################
# 
# Stationary Distribution
# 
#   ACOE check 
#
#  Dynamic Pricing Policy 
# 
########################################################

#####################################  
# Save policy for first week
#####################################  
UNcandidate = UN[t<=(parms$wmax*parms$K)]

#####################################  
# Get SXS transition matrix given the policy. 
#####################################  

#Find the demand at every state-action 
prHat_UN = copy(prHat)

#Find the action-state transition matrix. 
transitionmat = PAmats(prHat_UN,lll1,parms) 

#reshape to an SXS given optimal action at each step

statetransmat = CJ('state'=transitionmat$q[t<=(parms$wmax*parms$K)]$state,
                   'stateprm'=transitionmat$q[t<=(parms$wmax*parms$K)]$state)

statetransmat_action_opt = merge(statetransmat,UN[,.(state,anum)],by='state')

#now get the nonzero state transition probs
non_zero_probs = transitionmat$datatablePA

#For each state, keep only the optimal action
non_zero_probs = merge(non_zero_probs,UN[,.(state,anum)],by=c('state','anum'))

#get the problem states
probstates = unique(transitionmat$q[t==(parms$wmax*parms$K)]$state)


non_zero_probs_correct = non_zero_probs[state<max(transitionmat$q[t<=(parms$wmax*parms$K)]$state) & (!state%in%probstates)]
non_zero_probs_fix = non_zero_probs[state%in%probstates]

#Fix the last state transition since it loops back into itself.
renamelaststate = transitionmat$q[t==(parms$wmax*parms$K+1)]
renamelaststate[,t:=NULL]
renamelaststate[,V:=NULL]
setnames(renamelaststate,'state','old_state')

looperstate=copy(transitionmat$q)
looperstate=looperstate[t<=(parms$wmax*parms$K)]
looperstate[,t:=NULL]
looperstate[,V:=NULL]

#find the correct state for the second week, 
#basically wrapping the policy back into itself.

fixend = merge(looperstate,renamelaststate,by=c('d','w',paste0(rep('prod',parms$Tp*parms$J),
                                                       rep(1:parms$J,each=parms$Tp),
                                                       rep('vint',parms$Tp*parms$J),
                                                       rep(1:parms$Tp,parms$J)
                                                       )))

fixend_short = copy(fixend)[,.(state,old_state)]
fixend_short_prm = copy(fixend_short)
setnames(fixend_short_prm,'state','stateprm_fix')
setnames(fixend_short_prm,'old_state','stateprm')

non_zero_probs_fix = merge(non_zero_probs_fix,fixend_short_prm,by='stateprm')
non_zero_probs_fix[,stateprm:=stateprm_fix]
non_zero_probs_fix[,stateprm_fix:=NULL]
setcolorder(non_zero_probs_fix, colnames(non_zero_probs_correct))

non_zero_probs_fix_complete = rbind(non_zero_probs_correct,non_zero_probs_fix)


#Merge in the nonzero probs
statetransmat_withprobs = merge(statetransmat,non_zero_probs_fix_complete,by=c("state", "stateprm"),all=T)
statetransmat_withprobs[is.na(prss),prss:=0]

#Turn this into an SxS matrix
statetransmat_withprobs_mat_UN = matrix(statetransmat_withprobs$prss,nrow=max(statetransmat_withprobs$state),ncol=max(statetransmat_withprobs$state),byrow = T)

   
#####################################
#Verify the solution solves for rho. 
#####################################

#Because it is a markov, let's take stationary distribution of states
#by raising state transition matrix to a high power.   

#Because the markov chain is periodic, the power you raise to tells you exactly where
#you will be in terms of day,week.  So we need to get all K*wmax dayparts since you spend
#an even amount of time in each of these. Then we take the stationary distributions 
#and average over them. 
vec = t(matrix(c(1,rep(0,nrow(statetransmat_withprobs_mat_UN)-1)),
 nrow(statetransmat_withprobs_mat_UN)))

veclist = list()
for(i in 1:40){

vec = vec%*%statetransmat_withprobs_mat_UN

}
tempvec = copy(vec)
cummat = copy(tempvec)

for(i in 1:(parms$K*parms$wmax-1)){
    print(i)
    tempvec = tempvec%*%statetransmat_withprobs_mat_UN
    cummat = cummat + tempvec
}
addedmats = t(cummat/(parms$K*parms$wmax))

Stationary_Dist_UN = addedmats
Stationary_Dist_DT = data.table(Stationary_Dist_UN)
setnames(Stationary_Dist_DT,'V1','Probability')
Stationary_Dist_DT[,state:=1:.N]

#Do some checks. 
statereader = transitionmat$q[t<=(parms$wmax*parms$K)]
statereader[,V:=NULL]
Stationary_Dist_DT_check = merge(Stationary_Dist_DT,statereader,by='state')


#Save stationary dist and transition matrix
Stationary_Dist_DT_check_UN = copy(Stationary_Dist_DT_check)
statetransmat_withprobs_UN_output = copy(statetransmat_withprobs)
statetransmat_withprobs_UN_output = statetransmat_withprobs_UN_output[prss!=0]

if(checks==T){  

			#####################################  
			# solve linear equation for rho and h
			#####################################  
			#Calculate reward function by state. 
			reward_all = returnmat(prHat_UN,lll1,parms)$Rsa
			reward=merge(reward_all[t<=(parms$wmax*parms$K)],UN[,.(state,anum)],by=c('state','anum'))
			reward = reward[,.(state,rsa)]
			reward_mat = matrix(-reward$rsa,nrow=dim(reward)[1])
			
			#Create Invertible Matrix to Solve the Linear System of Equations Verifying the ACOE Condition
			P_mat_new = copy(statetransmat_withprobs_mat_UN)
			P_mat_new = P_mat_new - diag(nrow(P_mat_new))
			P_mat_new=P_mat_new[,-1]
			P_mat_new=cbind(P_mat_new,-1)
				
    	#Calculate stationary rho(scalar) = dist * reward, which is the expected reward weighted across
    	#all states, first calculate the expected reward at each state, given the policy.  
      P_mat_new_inv = solve(P_mat_new) 

		  #Solution
		  Solution = P_mat_new_inv%*%reward_mat

      reward_all = returnmat(prHat_UN,lll1,parms)$Rsa
      reward=merge(reward_all,UN[,.(state,anum)],by=c('state','anum'))
      reward = merge(reward,Stationary_Dist_DT,by='state')
      
      #rho
      reward[,expreward:=rsa*Probability]
      rho = sum(reward$expreward)
     
      #SAVE THIS IN OUTPUT
      UN_rho_difference = abs(Solution[nrow(Solution),1]-rho)
      
		  #####################################
		  #Verify the solution solves ACOE
		  #####################################        
		  #Now we have rho and h, we need to check rho + h = max_a (r(a) + P(a)*h(a)
		
		  h_candidate = cbind(c(0,Solution[-nrow(Solution),]))
		  h_candidate_datatable = data.table('state'=1:dim(h_candidate)[1],'h'=h_candidate[,1])
		  rho_cand = Solution[nrow(Solution),1]          
		          
		  #Now, create r(s,a)(SxA matrix) and stacked Probability transition matrix so we can calc 
		  #RHS of ACOE
		  reward_all = reward_all[t<=(parms$wmax*parms$K)]
		  reward_mat_creator = reward_all[,.(state,anum,rsa)]
		  
		  #Turn this into an SxA matrix
		  reward_mat = matrix(reward_mat_creator$rsa,nrow=max(reward_mat_creator$state),ncol=max(reward_mat_creator$anum),byrow = T)
		 
		  #We can actually do this whole thing with data table groupbys as I do in solveDP2.R.
		  #Rather than the full matrix multiplication.  So let's do it that way. 
		  
		  #lets get expected h hit by transition prob matrix. 
		  #now get the nonzero state transition probs
		  non_zero_probs = transitionmat$datatablePA
		  
		  #############################
		  #Need to remake the P mat
		  #but for every A this time
		  #############################
      #get the problem states
      probstates = unique(transitionmat$q[t==parms$wmax*parms$K]$state)
      
      non_zero_probs_correct = non_zero_probs[state<max(transitionmat$q[t<=(parms$wmax*parms$K)]$state) & (!state%in%probstates)]
      non_zero_probs_fix = non_zero_probs[state%in%probstates]
      
      #Fix the last state transition since it loops back into itself.
      renamelaststate = transitionmat$q[t==(parms$wmax*parms$K+1)]
      renamelaststate[,t:=NULL]
      renamelaststate[,V:=NULL]
      setnames(renamelaststate,'state','old_state')
     
      looperstate=copy(transitionmat$q)
      looperstate=looperstate[t<=(parms$wmax*parms$K)]
      looperstate[,t:=NULL]
      looperstate[,V:=NULL]
      
      #find the correct state for the second week, 
      #basically wrapping the policy back into itself.
      
      fixend = merge(looperstate,renamelaststate,by=c('d','w',paste0(rep('prod',parms$Tp*parms$J),
                                                             rep(1:parms$J,each=parms$Tp),
                                                             rep('vint',parms$Tp*parms$J),
                                                             rep(1:parms$Tp,parms$J)
                                                      )))
      
      fixend_short = copy(fixend)[,.(state,old_state)]
      fixend_short_prm = copy(fixend_short)
      setnames(fixend_short_prm,'state','stateprm_fix')
      setnames(fixend_short_prm,'old_state','stateprm')
      
      non_zero_probs_fix = merge(non_zero_probs_fix,fixend_short_prm,by='stateprm')
      non_zero_probs_fix[,stateprm:=stateprm_fix]
      non_zero_probs_fix[,stateprm_fix:=NULL]
      setcolorder(non_zero_probs_fix, colnames(non_zero_probs_correct))
      
      non_zero_probs_fix_complete = rbind(non_zero_probs_correct,non_zero_probs_fix)
      
     # Lets calculate the RHS = max R + Eh
    
     R = reward_all[,.(state,anum,rsa)]
    
     H_prime = copy(h_candidate_datatable)[,.(state,h)]   #V is h(s) fyi. 
     setnames(H_prime,'state','stateprm')  
            
     pmat = copy(non_zero_probs_fix_complete)
     
     #So lets calculate Pmat*H
     PH = merge(pmat,H_prime,by=c('stateprm'))
     
     PHs = PH[,.(Hprime = sum(prss*h)),by=.(state,anum)]
     PHs = PHs[order(state,anum)]
     
     RHS = merge(R,PHs,by=c('state','anum'),all=T)
     RHS[is.na(Hprime),Hprime:=0]
     
     RHS[,Total:=rsa+Hprime]
     RHS=RHS[,.(Total=max(Total)),by=state]

	   #check RHS and LHS. Hope diff is small.
  
     LHS = copy(h_candidate_datatable)
     setnames(LHS,'h','rho_plus_h')
     LHS[,rho_plus_h:=rho_plus_h+rho_cand]
     
     bothsides = merge(LHS,RHS, by='state')
     bothsides[,diff:=abs(Total-rho_plus_h)]
 
		 #####################################
		 # check max diff between LHS 
		 # and RHS
		 #####################################      
		     
     UN_maxdifference = max(bothsides$diff)
     
}








#######################################
#
#   Primary Outputs and Analysis
#   of Results. 
#
#
#######################################

UNcandidate[,t:=NULL]
UNcandidate[,V:=NULL]
SNcandidate[,V:=NULL]
SNcandidate[,t:=NULL]
SNcandidate[,p1sLAST:=NULL]



print(head(UNcandidate)) #optimal Dynamic pricing policy. Until column "state", these are state variables. 
                         # d = daypart
                         # w = weekday
                         # prod1vint[tau] = units of vintage expiring in tau days. 
                         # p1s = optimal price
                         # order1 = optimal ordering quantity  

print(head(SNcandidate)) #Same variable meanings as above, but for the optimal Static pricing policy. 


print(head(reward_list_UN)) #The ex ante net profit per period for each draw we decided to keep under optimal Dynamic pricing. 
print(head(CS_list_UN))     #The ex ante consumer welfare per period for each draw we decided to keep under optimal Dynamic pricing. 
print(head(waste_list_UN))  #The ex ante wasted units per period for each draw we decided to keep under optimal Dynamic pricing. 


print(head(reward_list_SN)) #The ex ante net profit per period for each draw we decided to keep under optimal Static pricing. 
print(head(CS_list_SN))     #The ex ante consumer welfare per period for each draw we decided to keep under optimal Static pricing. 
print(head(waste_list_SN))  #The ex ante wasted units per period for each draw we decided to keep under optimal Static pricing. 


#What is the percent change in profit? 
change_prof= round(100*(reward_list_UN$profit_per_period/reward_list_SN$profit_per_period-1),1)

#What is the percent change in cs? 
change_cs = round(100*(CS_list_UN$CS_per_period/CS_list_SN$CS_per_period-1),1)

#What is the percent change in waste? 
change_waste = round(100*(waste_list_UN$waste_per_period/waste_list_SN$waste_per_period-1),1)

#Average percent changes in outcomes. 
print(round(median(change_prof),1))
print(round(median(change_cs),1))
print(round(median(change_waste),1))

#################################
#
#   Save important information
#
#################################

setwd('../output')

if(checks==T){
save(UNcandidate,             #Save the Dynamic pricing policy function
		 SNcandidate,             #Save the Static pricing policy function
     SP_maxdifference,        #ACOE max difference between LHS and RHS for Static pricing policy
		 Stationary_Dist_SP,      #Stationary distribution under Static pricing policy 
     UN_maxdifference,        #ACOE max difference between LHS and RHS for Dynamic pricing policy
		 Stationary_Dist_UN,      #Stationary distribution under Dynamic pricing policy 
     prHat_list_SN,           #Demand distribution under Static pricing for every draw (that we get posterior for)
     transitionmat_list_SN,   #Trasition matrix under Static pricing for every draw (that we get posterior for)
     stationary_list_SN,      #Stationary distribution under Static pricing policy for every draw (that we get posterior for)  
     reward_list_SN,          #Posterior Distribution of Ex ante Profit Per Period under Static Pricing
     CS_list_SN,              #Posterior Distribution of Ex ante Consumer welfare Per Period under Static Pricing
     waste_list_SN,           #Posterior Distribution of Ex ante Waste Per Period under Static Pricing
     prHat_list_UN,           #Demand distribution under Dynamic pricing for every draw (that we get posterior for)
     transitionmat_list_UN,   #Trasition matrix under Dynamic pricing for every draw (that we get posterior for)
     stationary_list_UN,      #Stationary distribution under Dynamic pricing policy for every draw (that we get posterior for)
     reward_list_UN,          #Posterior Distribution of Ex ante Profit Per Period under Dynamic Pricing
     CS_list_UN,              #Posterior Distribution of Ex ante Consumer welfare Per Period under Dynamic Pricing
     waste_list_UN,           #Posterior Distribution of Ex ante Waste Per Period under Dynamic Pricing
     prHat_dynamic,           #Expected Demand Distribution, by action-state, for Dynamic pricing. 
     prHat_fullstatic,        #Expected Demand Distribution, by action-state, for Static pricing. 
		 statetransmat_withprobs_UN_output,  #Expected Transition Matrix, for Dynamic pricing, only nonzero entries. 
		 statetransmat_withprobs_SN_output,  #Expected Transition Matrix, for Static pricing, only nonzero entries. 
		 file=paste0('DP_Result.RData'))
}
if(checks==F){
save(UNcandidate,             #Save the Dynamic pricing policy function
		 SNcandidate,             #Save the Static pricing policy function
		 Stationary_Dist_SP,      #Stationary distribution under Static pricing policy 
		 Stationary_Dist_UN,      #Stationary distribution under Dynamic pricing policy 
     prHat_list_SN,           #Demand distribution under Static pricing for every draw (that we get posterior for)
     transitionmat_list_SN,   #Trasition matrix under Static pricing for every draw (that we get posterior for)
     stationary_list_SN,      #Stationary distribution under Static pricing policy for every draw (that we get posterior for)  
     reward_list_SN,          #Posterior Distribution of Ex ante Profit Per Period under Static Pricing
     CS_list_SN,              #Posterior Distribution of Ex ante Consumer welfare Per Period under Static Pricing
     waste_list_SN,           #Posterior Distribution of Ex ante Waste Per Period under Static Pricing
     prHat_list_UN,           #Demand distribution under Dynamic pricing for every draw (that we get posterior for)
     transitionmat_list_UN,   #Trasition matrix under Dynamic pricing for every draw (that we get posterior for)
     stationary_list_UN,      #Stationary distribution under Dynamic pricing policy for every draw (that we get posterior for)
     reward_list_UN,          #Posterior Distribution of Ex ante Profit Per Period under Dynamic Pricing
     CS_list_UN,              #Posterior Distribution of Ex ante Consumer welfare Per Period under Dynamic Pricing
     waste_list_UN,           #Posterior Distribution of Ex ante Waste Per Period under Dynamic Pricing
     prHat_dynamic,           #Expected Demand Distribution, by state, for Dynamic pricing. 
     prHat_fullstatic,        #Expected Demand Distribution, by state, for Static pricing. 
		 statetransmat_withprobs_UN_output,  #Expected Transition Matrix, for Dynamic pricing, only nonzero entries. 
		 statetransmat_withprobs_SN_output,  #Expected Transition Matrix, for Static pricing, only nonzero entries. 
		 file=paste0('DP_Result.RData'))
}
    



#######################################################################################
#
#
#   LIFOFIFOdrawdown.R - This is a function to drawdown inventory
#                     given a particular method and demand vector. 
#                     
#                     
#              Inputs:
#
#                      dat  - a data table of all product-vintages and 
#                             a column "num" ordering them.  
#
#                      Q    -  a vector of demand for product.
#
#                      drawdown - "LIFO" or "FIFO" indicating how 
#                                  inventory should be drawn down. 
#
#
#              Output: 
#  
#                      dat - a data table that is the remaining inventory post demand. 
#
#
#
#  by Robert Evan Sanders
#  July 12, 2023.
#
#######################################################################################


drawdownfunc = function(dat,Q,Drawdown,j){
  
    tbl <- melt((dat), meas=patterns(paste0("^prod",j,'vint[1-9]')), 
                value.name=c("bef"))
      
    if(Drawdown=='FIFO'){
      setorder(tbl, num, variable) 
    }
    if(Drawdown=='LIFO'){
      setorder(tbl, num, -variable) 
    }
    
    tbl = data.table(tbl)
    tbl[,flifo:=(pmin(pmax(cumsum(bef) - Q, 0), bef)),by=num]
    tbl[,bef:=NULL]    
  
    dat2 = data.table::dcast(tbl, num ~ variable,value.var='flifo')
    
    return(dat2)
  }
  
  

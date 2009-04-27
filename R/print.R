# ==============================================================================
# S4 print function for mi object
# ==============================================================================
setMethod("print", signature( x = "mi" ),
  function ( x, ... ) {
    print.mi( x, ...)
  }
)

print.mi <- function ( x, ... ) {
    n <- nrow(x@data)
    cat ( "\nMultiply imputed data set" )
    cat ( "\n\nCall:\n " )
    print( call.mi(x) )
    cat ( "\nNumber of multiple imputations: ", m(x),"\n")
    tab <- mi.info.table( info.mi(x) )[,c("names","type","number.mis")]
    tab <- data.frame(tab, proportion=tab[,"number.mis"]/n )
    cat ( "\nNumber and proportion of missing data per column:\n" )
    print ( tab )
    cat ( "\nTotal Cases:", n )
    r    <- 1 * is.na ( x@data )
    cat ( "\nMissing at least one item:", sum ( colSums(r)!= 0 ) )
    cat ( "\nComplete cases:", sum ( rowSums(r) == 0 ), "\n" )
    invisible( tab )
  }
  
  
    
print.mi.pooled <- function(x, ...){
  display(x, ...)
}


  
  
  
  
  
  
# ========================================================================
# S4 print function for mi.info object
# ========================================================================
setMethod("print", signature( x = "mi.info" ),
  function ( x, ... ) {
    print.mi.info( x, ...)
  }
)

print.mi.info <- function ( x, ... ){
  print( mi.info.table( x ) )
}

mi.info.table <- function ( info ) {
  result<-data.frame(matrix(NA,nrow=length(info),ncol=7))
  dimnames(result)<-list(c(1:length(info)),c("names","include","order","number.mis","all.mis","type","collinear"))
  result[,1]<-sapply(info,function(inf){inf$name})
  result[,2]<-sapply(info,function(inf){if(inf$include){"Yes"}else{"No"}}) 
  result[,3]<-sapply(info,function(inf){inf$imp.order})
  result[,4]<-sapply(info,function(inf){inf$nmis})
  result[,5]<-sapply(info,function(inf){if(inf$all.missing){"Yes"}else{"No"}}) 
  result[,6]<-sapply(info,function(inf){inf$type})
  result[,7]<-sapply(info,function(inf){if(is.na(inf$collinear[1])){"No"}else{paste(inf$collinear,collapse=", ")}})  
  invisible(result)
}
# ==============================================================================
# S4 print function for mi.method object
# ==============================================================================
setMethod("print", signature( x = "mi.method" ),
  function ( x, ... ) {
    print.mi.method(x,...)
  }
)

print.mi.method <- function ( x, ... ) {
    cat("model:\n ")
    print(x@model$call)
    cat("\ncoefficients:\n")
    print(x@model$coefficient)
    cat("\nimputed values:\n")
    print(x@random)
}

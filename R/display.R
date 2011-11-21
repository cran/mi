setMethod("display", signature(object = "mi.pooled"),     
    function ( object, digits=2 ) {
  cat( "=======================================\n" )  
  cat( "Separate Estimates for each Imputation\n" )
  cat( "=======================================\n" )
  for( i in 1:length( object@mi.fit ) ){
    cat( "\n** Chain", i, "**\n" )
    display( object@mi.fit[[i]], digits=digits )
  }
  cat( "\n=======================================\n" )
  cat( "Pooled Estimates\n" )
  cat( "=======================================\n" )
  print( object@call, digits=digits )
  tab <- cbind( object@mi.pooled[[1]], object@mi.pooled[[2]] )
  dimnames( tab )[[2]] <- c( "coef.est", "coef.se" )
  pfround( tab,  digits=digits)
  cat( "---\n\n" )
}
)

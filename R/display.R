setMethod("display", signature(object = "mi.lm"),     
    function ( object, digits=2 ) {
  cat( "=======================================\n" )
  cat( "Pooled Estimate\n" )
  cat( "=======================================\n" )
  print( object@call, digits=digits )
  tab <- cbind( object@mi.pooled[[1]], object@mi.pooled[[2]] )
  dimnames( tab )[[2]] <- c( "coef.est", "coef.se" )
  pfround( tab,  digits=digits)
  cat( "\n=======================================\n" )
  cat( "Separate Estimate for each Imputation\n" )
  cat( "=======================================\n" )
  for( i in 1:length( object@mi.fit ) ){
    cat( "\n** Imputation", i, "**\n" )
    display( object@mi.fit[[i]], digits=digits )
  }
}
)

setMethod("display", signature(object = "mi.glm"),     
    function ( object, digits=2 ) {
  cat( "=======================================\n" )
  cat( "Pooled Estimate\n" )
  cat( "=======================================\n" )
  print( object@call, digits=digits )
  tab <- cbind( object@mi.pooled[[1]], object@mi.pooled[[2]] )
  dimnames( tab )[[2]] <- c( "coef.est", "coef.se" )
  pfround( tab,  digits=digits)
  cat( "\n=======================================\n" )
  cat( "Separate Estimate for each Imputation\n" )
  cat( "=======================================\n" )
  for( i in 1:length( object@mi.fit ) ){
    cat( "\n** Imputation", i, "**\n" )
    display( object@mi.fit[[i]], digits=digits )
  }
}
)


setMethod("display", signature(object = "mi.mer"),     
    function ( object, digits=2 ) {
  cat( "=======================================\n" )
  cat( "Pooled Estimate\n" )
  cat( "=======================================\n" )
  print( object@call, digits=digits )
  tab <- cbind( object@mer.mi.pooled[[1]], object@mer.mi.pooled[[2]] )
  dimnames( tab )[[2]] <- c( "coef.est", "coef.se" )
  pfround( tab,  digits=digits)
  cat( "\n=======================================\n" )
  cat( "Separate Estimate for each Imputation\n" )
  cat( "=======================================\n" )
  for( i in 1:length( object@mer.mi.fit ) ){
    cat( "\n** Imputation", i, "**\n" )
    display( object@mer.mi.fit[[i]], digits=digits )
  }
}
)

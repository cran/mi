setMethod("display", signature(object = "mi.lm"),     
    function ( object, digits=2 ) {
  cat( "=======================================\n" );
  cat( "Pooled Estimate\n" );
  cat( "=======================================\n" );
  print( object$call, digits=digits );
  tab <- cbind( object$lm.mi.pooled[[1]], object$lm.mi.pooled[[2]] );
  dimnames( tab )[[2]] <- c( "coef.est", "coef.se" );
  pfround( tab,  digits=digits);
  cat( "\n=======================================\n" );
  cat( "Separate Estimate for each Imputation\n" );
  cat( "=======================================\n" );
  for( i in 1:length( object$lm.mi.fit ) ){
    cat( "\n** Imputation", i, "**\n" );
    display( object$lm.mi.fit[[i]], digits=digits );
  }
}
)

setMethod("display", signature(object = "mi.glm"),     
    function ( object, digits=2 ) {
  cat( "=======================================\n" );
  cat( "Pooled Estimate\n" );
  cat( "=======================================\n" );
  print( object$call, digits=digits );
  tab <- cbind( object$glm.mi.pooled[[1]], object$glm.mi.pooled[[2]] );
  dimnames( tab )[[2]] <- c( "coef.est", "coef.se" );
  pfround( tab,  digits=digits);
  cat( "\n=======================================\n" );
  cat( "Separate Estimate for each Imputation\n" );
  cat( "=======================================\n" );
  for( i in 1:length( object$glm.mi.fit ) ){
    cat( "\n** Imputation", i, "**\n" );
    display( object$glm.mi.fit[[i]], digits=digits );
  }
}
)

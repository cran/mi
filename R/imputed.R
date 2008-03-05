# ==============================================================================
# extract imputed values for mi.method class object
# ==============================================================================
setMethod( "imputed", signature( object = "mi.method" ),     
  function ( object, y ) {
    y[is.na( y )] <- object@random
    return( y )
  }
)


setMethod( "imputed", signature( object = "mi.categorical" ),     
  function ( object, y ) {
    #y.level <- levels(y)
    #y <- as.numeric(y)  
    y[is.na( y )] <- object@random
    return( y )
  }
)


setMethod( "imputed", signature( object = "mi.polr" ),     
  function ( object, y ) {
    #y.level <- levels(y)
    #y <- as.numeric(y)  
    y[is.na( y )] <- object@random
    return( y )
  }
)

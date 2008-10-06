# ==============================================================================
# extract imputed values for mi.method class object
# ==============================================================================
setMethod( "imputed", signature( object = "mi.method" ),     
  function ( object, y ) {
    y[is.na( y )] <- object$random
    return( y )
  }
)
# ==============================================================================
# extract imputed values for mi.mixed class object
# ==============================================================================
setMethod("imputed", signature(object = "mi.mixed"),     
  function ( object, y ) {
    y[ is.na( y ) ] <- object$random
    return( y )
  }
)

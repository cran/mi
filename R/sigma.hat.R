# ==============================================================================
# extract sigma.hat values for mi.method class object
# ==============================================================================
setMethod( "sigma.hat", signature( object = "mi.method" ),     
  function ( object, ... ) {
    return( object$model$sigma )
  }
)

# ==============================================================================
# extract sigma.hat values for mi.mixed class object
# ==============================================================================
setMethod("sigma.hat", signature(object = "mi.mixed"),     
  function ( object,... ) {
    return( object$model$model.2$sigma )
  }
)

# ==============================================================================
# extract coefficient values for mi.method class object
# ==============================================================================

coef.mi.method <- function(object, ...){
  object$model$coefficient
}
  
#
#setMethod( "coef", signature( object = "mi.method" ),     
#  function ( object, ... ) {
#    return( object$model$coefficient )
#  }
#) 



# ==============================================================================
# extract coefficient values for mi.mixed class object
# ==============================================================================


coef.mi.mixed <- function(object, ...){
  return( list( object$model$model.1$coefficient, 
                object$model$model.2$coefficient ) )
}


#setMethod("coef", signature(object = "mi.mixed"),     
#  function ( object ) {
#    return( list( object$model$model.1$coefficient, 
#                    object$model$model.2$coefficient ) )
#  }
#)

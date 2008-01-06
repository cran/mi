# ==============================================================================
# extracting starting values for mi.method class object
# ==============================================================================
setMethod( "mi.start", signature( object = "mi.method" ), 
  function( object ) {     
    return( object$model$coefficient )
  }
) 


#setMethod("mi.start", signature(object = "mi.logcontinuous"),  
#  function ( object ) {   
#    result <-  list(object$model$model.1$coefficient,
#                      object$model$model.2$coefficient)
#    return ( result )
#  }
#) 



# ==============================================================================
# extracting starting values for mi.mixed class object
# ==============================================================================
setMethod("mi.start", signature(object = "mi.mixed"),  
  function ( object ) {   
    result <-  list(object$model$model.1$coefficient,
                      object$model$model.2$coefficient)
    return ( result )
  }
) 

# ==============================================================================
# extract residual values for mi.method class object
# ==============================================================================


residuals.mi.method <- function ( object, y, ...) {
    return( y - fitted( object ) )
}

#setMethod( "resid", signature( object = "mi.method" ),     
#  function ( object, y ) {
#    return( y - fitted( object ) )
#  }
#)

# ==============================================================================
# extract residual values for mi.dichotomous class object
# ==============================================================================

residuals.mi.dichotomous <-   function (object, y, ...) {
    return( dicot( y ) - fitted ( object ) )
  }


#setMethod( "resid", signature( object = "mi.dichotomous" ),     
#  function ( object, y ) {
#    return( dicot( y ) - fitted ( object ) )
#  }
#)
# ==============================================================================
# extract residual values for mi.logcontinuous class object
# ==============================================================================

residuals.mi.logcontinuous <- function ( object, y, ...) {
    return( log( y ) - log( fitted ( object ) ) )
  }

# ==============================================================================
# extract residual values for mi.sqrtcontinuous class object
# ==============================================================================

residuals.mi.sqrtcontinuous <-   function (object, y, ...) {
    return( sqrt(y) - sqrt(fitted ( object )) )
    #return( y - fitted ( object ) )
  }


#setMethod( "resid", signature( object = "mi.sqrtcontinuous" ),     
#  function ( object, y ) {
#    return( sqrt(y) - sqrt(fitted ( object )) )
#    #return( y - fitted ( object ) )
#  }
#)
# ==============================================================================
# extract residual values for mi.mixed class object
# ==============================================================================

residuals.mi.mixed <-   function (object, y, ...) {
   return( list(residual.values.1 = 1*( y > 0 ) - fitted( object )[[1]], 
                residual.values.2 = y[as.double( names( fitted( object )[[2]]))]-fitted( object )[[2]]) )
}


#setMethod( "resid", signature( object = "mi.mixed" ),     
#  function ( object, y ) {
#   return( list(residual.values.1 = 1*( y > 0 ) - fitted( object )[[1]], 
#                residual.values.2 = y[as.double( names( fitted( object )[[2]]))]-fitted( object )[[2]]) )
#  }
#)

# ==============================================================================
# extract residual values for mi.sqrtcontinuous class object
# ==============================================================================

residuals.mi.categorical <-   function (object, y, ...) {
    return( object$residual )
    #return( y - fitted ( object ) )
}


#setMethod( "resid", signature( object = "mi.categorical" ),     
#  function ( object, y ) {
#    return( object$residual )
#    #return( y - fitted ( object ) )
#  }
#)

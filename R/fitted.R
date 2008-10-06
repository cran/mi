# ==============================================================================
# extract fitted values for mi.method class object
# ==============================================================================
fitted.mi.method <-   function ( object, ... ) {
    return( object$expected )
}



#setMethod( "fitted", signature( object = "mi.method" ),     
#  function ( object, ... ) {
#    return( object$expected )
#  }
#)
# ==============================================================================
# extract fitted values for mi.mixed class object
# ==============================================================================
fitted.mi.mixed <-   function ( object, ... ) {
    return( object$expected)
  }



#setMethod("fitted", signature(object = "mi.mixed"),     
#  function ( object ) {
#    return( object$expected)
#  }
#)

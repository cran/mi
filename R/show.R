# ==============================================================================
# S4 show function for mi object
# ==============================================================================
setMethod( "show", signature( object = "mi" ),
  function ( object ) {
    print( object )
  }
) 

# ==============================================================================
# S4 show function for mi.info object
# ==============================================================================
setMethod( "show", signature( object = "mi.info" ), 
  function ( object ) {
    print( object )
  }
)

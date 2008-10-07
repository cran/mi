# ==============================================================================
# generic method definition 
# ==============================================================================

if ( !isGeneric( "call.mi" )) {
  setGeneric( "call.mi",
              function( object ){
                standardGeneric( "call.mi" )
              } 
  )
}



if ( !isGeneric( "converged" )) {
  setGeneric( "converged",
              function( object ){
                standardGeneric( "converged" )
              } 
  )
}



if ( !isGeneric( "data.mi" )) {
  setGeneric( "data.mi",
              function( object ){
                standardGeneric( "data.mi" )
              } 
  )
}



if ( !isGeneric( "m" )) {
  setGeneric( "m",
              function( object ){
                standardGeneric( "m" )
              } 
  )
}


if ( !isGeneric( "bugs.mi" )) {
  setGeneric( "bugs.mi",
              function( object ){
                standardGeneric( "bugs.mi" )
              } 
  )
}


if ( !isGeneric( "info.mi" )) {
  setGeneric( "info.mi",
              function( object ){
                standardGeneric( "info.mi" )
              } 
  )
}


if ( !isGeneric( "imp" )) {
  setGeneric( "imp",
              function( object,... ){
                standardGeneric( "imp" )
              } 
  )
}

if ( !isGeneric( "is.mi" )) {
  setGeneric( "is.mi",
              function( object ){
                standardGeneric( "is.mi" )
              } 
  )
}


if ( !isGeneric( "mi.completed" )) {
  setGeneric( "mi.completed",
              function( object, ... ){
                standardGeneric( "mi.completed" )
              } 
  )
}
if ( !isGeneric( "mi.matrix" )) {
  setGeneric( "mi.matrix",
              function( object,... ){
                standardGeneric( "mi.matrix" )
              } 
  )
}
if ( !isGeneric( "mi.start" ) ) {
    setGeneric( "mi.start", 
              function( object ) {
                standardGeneric( "mi.start" ) 
              }
  )
}
if ( !isGeneric( "mi.data.frame" )) {
  setGeneric( "mi.data.frame",
              function( object,... ){
                standardGeneric( "mi.data.frame" )
              } 
  )
}

if ( !isGeneric( "imputed" ) ) {
    setGeneric( "imputed",
               function( object, ... ){
                  standardGeneric( "imputed" )
               } 
    )
}
if ( !isGeneric("mi.hist")) {
    setGeneric( "mi.hist",
               function( object, Yobs,... ){
               standardGeneric( "mi.hist" ) 
               }
    )
}



if ( !isGeneric( "typecast" ) ) {
    setGeneric( "typecast",
               function( object ){
               standardGeneric( "typecast" )
               } 
    )
}

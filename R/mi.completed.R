setMethod( "mi.completed", signature( object = "mi" ),
  function ( object, m = 1, outcome = c("data.frame","matrix") ) {
    outcome<- match.arg(outcome)
    if( m(object) < m )  { stop( message = "Index of imputation is not within the range." ) }
    info <- info.mi(object)
    mis.name <- names( nmis(info)[ nmis( info ) > 0 & !all.missing( info ) ] )
    mimatrix <- data.mi(object)
    for ( i in 1:length(mis.name) ){
      nm <- mis.name[i]
      mimatrix[ ,nm] <- imputed( imp(object,m)[[nm]],mimatrix[ ,nm] )
    }  
    if(outcome=="data.frame"){
      return( data.frame( mimatrix ) )
    } else {
      return( as.matrix( mapply(.factor2num, mimatrix) ) )
    }
  }
)
setMethod( "mi.matrix", signature( object = "mi" ),
  function ( object, m = 1 ) {
    mi.completed( object, m = 1, outcome = "matrix" ) 
  }
)
setMethod( "mi.data.frame", signature( object = "mi" ),
  function ( object, m = 1 ) {
    mi.completed( object, m = 1, outcome = "data.frame" ) 
  }
)

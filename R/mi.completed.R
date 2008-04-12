mi.completed.default <- function(object, m = 1){
  if( m(object) < m )  { stop( message = "Index of imputation is not within the range." ) }
  if(object@preprocess){
    info <- object@mi.info.preprocessed
    mimatrix <- data.mi(object)
    mimatrix <- mi.preprocess(mimatrix, info=object@mi.info)$data
  }
  else{
    info <- info.mi(object)
    mimatrix <- data.mi(object)
  }
  mis.name <- names(.nmis(info)[.nmis(info) > 0 & !.all.missing( info ) ] )

  for ( i in 1:length(mis.name) ){
    nm <- mis.name[i]
    mimatrix[ ,nm] <- imputed(imp(object,m)[[nm]], mimatrix[ ,nm] )
  }
  return(as.data.frame(mimatrix))
}  
  



setMethod("mi.completed", signature( object = "mi" ),
  function (object) {
    n.chains <- m(object)
    data <- vector("list", n.chains)
    for(i in 1:m(object)){
      data[[i]] <- mi.completed.default(object, m = i) 
    }
    if(object@preprocess){
      info <- object@mi.info.preprocessed
      data <- mi.postprocess(data, info)
    }
    return(data)
  }
)




setMethod( "mi.data.frame", signature( object = "mi" ),
  function ( object, m = 1) {
    if(object@preprocess){
      data <- mi.completed.default(object, m=m)
      aa <<- data
      data <- mi.postprocess(data, info=object@mi.info.preprocessed)
    }
    else{
      data <- mi.completed.default(object, m = m) 
    }
    return(data)
  }
)

mi.completed.default <- function(object, m = 1){
  if( m(object) < m ) { 
    stop( message = "Index of imputation is not within the range." ) 
  }
  if(object@preprocess){
    info <- object@mi.info.preprocessed
    miMatrix <- data.mi(object)
    miMatrix <- mi.preprocess(miMatrix, info=object@mi.info)$data
  }
  else{
    info <- info.mi(object)
    miMatrix <- data.mi(object)
  }
  
  # changed to check if var was included (DS)
  mis.name <- names(info)[.nmis(info) > 0 & !.all.missing(info)& .include(info)]

  for ( i in 1:length(mis.name) ){
    nm <- mis.name[i]
    miMatrix[ ,nm] <- imputed(imp(object,m)[[nm]], miMatrix[ ,nm] )
  }
  return(as.data.frame(miMatrix))
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




setMethod("mi.data.frame", signature( object = "mi" ),
  function (object, m = 1) {
    if(object@preprocess){
      data <- mi.completed.default(object, m=m)
      data <- mi.postprocess(data, info=object@mi.info.preprocessed)
    }
    else{
      data <- mi.completed.default(object, m = m) 
    }
    return(data)
  }
)

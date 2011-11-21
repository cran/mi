# ==============================================================================
# extract residual values for mi.method class object
# ==============================================================================

setMethod("residuals", signature(object = "mi.method"), 
  function(object, y){
  y - fitted(object)
}
)



setMethod("residuals", signature(object = "mi.binary"), 
  function(object, y){
  .dichot(y) - fitted(object)
}
)

setMethod("residuals", signature(object = "mi.categorical"), 
  function(object, y){
  as.numeric(y) - as.numeric(fitted(object))
}
)




setMethod("residuals", signature(object = "mi.polr"), 
  function(object, y){
  as.numeric(y) - as.numeric(fitted(object))
}
)


setMethod("residuals", signature(object = "mi.count"), 
  function(object, y){
  res <- y - fitted(object)
  stud.res <- res/sd(fitted(object))
  return(stud.res)
}
)


setMethod("resid", signature(object = "mi.method"), 
  function(object, y){
  y - fitted(object)
}
)

setMethod("resid", signature(object = "mi.binary"), 
  function(object, y){
  .dichot(y) - fitted(object)
}
)

setMethod("resid", signature(object = "mi.categorical"), 
  function(object, y){
  as.numeric(y) - as.numeric(fitted(object))
}
)




setMethod("resid", signature(object = "mi.polr"), 
  function(object, y){
  as.numeric(y) - as.numeric(fitted(object))
}
)


setMethod("resid", signature(object = "mi.count"), 
  function(object, y){
  res <- y - fitted(object)
  stud.res <- res/sd(fitted(object))
  return(stud.res)
}
)

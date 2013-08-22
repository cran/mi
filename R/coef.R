# ==============================================================================
# extract coefficient values
# ==============================================================================

setMethod("coef", signature(object = "mi.method"), 
  function(object){
  object@model$coefficients
}
)


setMethod("coefficients", signature(object = "mi.method"), 
  function(object){
  object@model$coefficients
}
)


setMethod("coef", signature(object = "mi.pooled"), 
  function(object){
  object@mi.pooled$coefficients
}
)

setMethod("coefficients", signature(object = "mi.pooled"), 
  function(object){
  object@mi.pooled$coefficients
}
)

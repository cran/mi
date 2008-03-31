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


setMethod("coef", signature(object = "mi.lm"), 
  function(object){
  object@mi.pooled$coefficients
}
)

setMethod("coefficients", signature(object = "mi.lm"), 
  function(object){
  object@mi.pooled$coefficients
}
)


setMethod("coef", signature(object = "mi.glm"), 
  function(object){
  object@mi.pooled$coefficients
}
)

setMethod("coefficients", signature(object = "mi.glm"), 
  function(object){
  object@mi.pooled$coefficients
}
)


setMethod("coef", signature(object = "mi.mer"), 
  function(object){
  object@mi.pooled$coefficients
}
)

setMethod("coefficients", signature(object = "mi.mer"), 
  function(object){
  object@mi.pooled$coefficients
}
)

setMethod("se.coef", signature(object = "mi.lm"), 
  function(object){
  object@mi.pooled$se
}
)

setMethod("se.coef", signature(object = "mi.glm"), 
  function(object){
  object@mi.pooled$se
}
)

setMethod("se.coef", signature(object = "mi.mer"), 
  function(object){
  object@mi.pooled$se
}
)

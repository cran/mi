setMethod("se.coef", signature(object = "mi.lm"), 
  function(object){
  object@lm.mi.pooled$se
}
)

setMethod("se.coef", signature(object = "mi.glm"), 
  function(object){
  object@glm.mi.pooled$se
}
)

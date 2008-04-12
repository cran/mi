setMethod("se.coef", signature(object = "mi.pooled"), 
  function(object){
  object@mi.pooled$se
}
)

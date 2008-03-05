# ==============================================================================
# extract fitted values 
# ==============================================================================
setMethod("fitted", signature(object = "mi.method"), 
  function(object){
  object@expected
}
)

# ==============================================================================
# extract sigma.hat values 
# ==============================================================================

setMethod("sigma.hat", signature( object = "mi.method"),     
  function (object) {
    object@model$sigma
  }
)

# ==============================================================================
# extract coefficient values
# ==============================================================================

coef.mi.method <- function(object,...){
    object@model$coefficients
}

coefficients <- function (object, ...){
    UseMethod("coef")
}

#
#setMethod("coefficients", signature(object = "mi.method"),
#  function(object){
#  object@model$coefficients
#}
#)

coef.mi.pooled <- function(object,...){
     object@mi.pooled$coefficients
}



#setMethod("coefficients", signature(object = "mi.pooled"),
#  function(object){
#  object@mi.pooled$coefficients
#}
#)

# ==============================================================================
# The function to select the model based on the variable type
# ==============================================================================
#match.type <- function ( type.list , type = mi.types() ) {  
#  return( as.vector( sapply( type.list, match.arg, type ) ) )
#}

mi.types <- function(){
  type = c( "fixed",
            "continuous",
            "log-continuous",
            "count",
            "ordered-categorical",
            "unordered-categorical",
            "binary",
            "nonnegative",
            "proportion",
            "predictive-mean-matching",
            "positive-continuous")
  return( type )
}

mi.types.add <- function ( type.name ) {
  len <- length(body(mi.types)[[2]][[3]]) + 1
  body(mi.types)[[2]][[3]][[len]] <- type.name
}

## To select the model 
type.models <- function (type) {
    type <- match.arg (type, mi.types( ) )
    imputation.method <- list (
        "continuous"               = "mi.continuous",
        "log-continuous"           = "mi.continuous",
        "count"                    = "mi.count",
        "ordered-categorical"      = "mi.polr",
        "unordered-categorical"    = "mi.categorical",
        "binary"              = "mi.binary",
        "positive-continuous"      = "mi.continuous",
        "proportion"               = "mi.continuous",
        "predictive-mean-matching" = "mi.pmm",
        "fixed"                    = "mi.fixed", 
        "nonnegative"                    = "mi.continuous"
    )
    return(imputation.method[type]) # Will be NULL if the method is undefined
}

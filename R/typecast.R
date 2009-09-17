# ==============================================================================
#  function to select the variable type
# ==============================================================================
setMethod("typecast", signature( object = "ANY" ),     
  function (object){
  if (is.null(object)){ 
    "NULL" 
  } 
  else{
    values <- unique (object)[!is.na(unique(object))]
    len    <- length (values)
    if (len == 1){      # 1 category variable
      "fixed"
    } 
    else if (len == 2){ # 2 category variable
      "binary"
    }
    else if (is.numeric(object)){     # if the variable is numeric
      if (all(values>0) & all(values < 1)){
        "proportion"
      } 
      else if (len > 2 & len <= 5){   # 3~5 category variable
        "ordered-categorical"
      } 
      else if (len > 5 & all(values > 0)){ # more than 5 category and positive 
        "positive-continuous" 
      } 
      else if (len > 5 & "0" %in% values & all(values >= 0)){# more than 5 category with 0 and all positive numbers
        "nonnegative" 
      } 
      else { # everything else
        "continuous" 
      }
    }
    else if (is.ordered(object)){
      "ordered-categorical"
    }
    else if (is.factor(object)){ 
      "unordered-categorical" 
    }
    else if (is.character(object)){ 
      "unordered-categorical" 
    }
    else{
      "unordered-categorical" 
    }
  }
}
)

setMethod("typecast", signature( object = "matrix" ), 
  function ( object ) {    
    return( sapply( object, typecast ) )
  }
)

setMethod("typecast", signature( object = "data.frame" ), 
  function ( object ) {    
    return( sapply( object, typecast ) )
  }
)

setMethod("typecast", signature( object = "list" ), 
  function ( object ) {    
    return( lapply( object, typecast ) )
  }
)


#is.less1.matrix <- function ( mat, check ) {
#    return( sapply( mat, function(lst){ any( lst < 1, na.rm = TRUE ) }))
#}
#
#is.int.matrix <- function ( mat ) {
#    return(apply( mat, 2, function(lst){ all( (lst - floor(lst))==0, na.rm = TRUE)}))
#}
#
#is.num.matrix <- function ( mat ) {
#    return(sapply( mat, function(lst){ is.numeric(lst)}))
#}
#
#is.pos.matrix <- function ( mat ) {
#    return(apply( mat, 2, function(lst){ all( lst >= 0, na.rm = TRUE)}))
#}
#
#is.complete.matrix <- function (mat) {
#    return(sapply( mat, function(lst){ all(!is.na( lst ))}))
#}
#
#is.categorical <- function ( mat ) {
#    return(apply( mat, 2, function(lst){ length ( names ( table ( lst ) ) ) <= 5 }))
#}
#
#recode.vector <- function ( vec ) {
#    unique(vec)
#}

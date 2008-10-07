
vrb.typ <- NULL


factor2num <- function( a ) {
  if(is.factor( a ) ) {
    as.double( levels( a ) )[ as.double( a ) ]
  } else {
    a
  }
}


factor2char <- function( a ) {
  levels( a )[ as.numeric( a ) ]
}



# ========================================================================
# Extracts the imputation order vector(integer)
# ========================================================================

imp.order <- function(info){
  return(sapply(info,function(inf){inf$imp.order}))
}

# ========================================================================
# Extracts the include or not vector (logical)
# ========================================================================

include<-function(info){
  return(sapply(info,function(inf){inf$include}))
}

# ========================================================================
# Extracts the number of missing vector(integer)
# ========================================================================

nmis<-function(info){
  return(sapply(info,function(inf){inf$nmis}))
}

# ========================================================================
# Extracts the all missing or not (logical) as vector
# ========================================================================

all.missing <-function(info){
  return(sapply(info,function(inf){inf$all.missing}))
}

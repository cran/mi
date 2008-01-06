
vrb.typ <- NULL # to pass R CMD check
data.tmp <<- NULL # to pass R CMD check


# ========================================================================
# Random draw from the obs. data
# ========================================================================

.randdraw <- function(data, n = 1){
  foo <- function(x) sample(na.exclude(x), size = n, replace = FALSE)
  added.rows <- apply(data, 2, FUN = foo)
  return(added.rows)
}


# ========================================================================
# Convert factor to numeric value
# ========================================================================


.factor2num <- function( a ) {
  if(is.factor( a ) ) {
    as.double( levels( a ) )[ as.double( a ) ]
  } else {
    a
  }
}

# ========================================================================
# Convert character value
# ========================================================================

.factor2char <- function( a ) {
  levels( a )[ as.numeric( a ) ]
}



# ========================================================================
# Extracts the imputation order vector(integer)
# ========================================================================

imp.order <- function(info){
  foo <- function(x) x$imp.order
  return(sapply(info, FUN=foo))
}

# ========================================================================
# Extracts the include or not vector (logical)
# ========================================================================

include <- function(info){
  foo <- function(x) x$include
  return(sapply(info, FUN=foo))
}

# ========================================================================
# Extracts the number of missing vector(integer)
# ========================================================================

nmis <- function(info){
  foo <- function(x) x$nmis
  return(sapply(info, FUN=foo))
}

# ========================================================================
# Extracts the all missing or not (logical) as vector
# ========================================================================

all.missing <-function(info){
  foo <- function(x) x$all.missing
  return(sapply(info, FUN=foo))
}

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

# ==============================================================================
# random imputation function
# ==============================================================================
random.imp <- function ( data , imp.method = c( "bootstrap", "pca" ) , ... ) {
  imp.method <- match.arg ( imp.method )
  if(imp.method=="bootstrap"){
    if( is.vector( data ) ) {
      mis       <- is.na ( data )
      imputed   <- data
      imputed[ mis ] <- sample( data[ !mis ], sum( mis ), replace = TRUE )
    } 
    else if( is.matrix( data ) || is.data.frame( data )  ){
      imputed <- data
      for( j in 1:ncol ( data ) ) {
          mis  <- is.na ( data[,j] )
          if( sum(mis) == length(data[,j])){
            warning(message = paste( "variable", names(data)[j], "has no observation"))
          } 
          else {
            imputed[mis,j] <- sample( data[!mis, j], sum( mis ), replace = TRUE )
          }
      }
    } 
    else{
        stop ( message = "Unexpected data type: data must be vector, matrix, or data frame." )
    }
  } 
  else if (imp.method=="pca"){
     stop ( message = "pca imputation is not implemente in current version." )
  }
  
  #else{
  #    imputed <- pca( data, nPcs = 3, method = "bpca" )@completeObs
  #}
  return( as.data.frame( imputed ) )
}

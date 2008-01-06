# ==============================================================================
# imputation function for categorical variable
# ==============================================================================
mi.categorical <- function( formula, data = NULL, n.iter = 100, 
                              MaxNWts = 1500, ...  ) 
{
  call <- match.call()
  mf   <- match.call(expand.dots = FALSE)
  m    <- match(c("formula", "data"), names(mf), 0)
  mf   <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  Y  <- model.response(mf, "any")
  if (length(dim(Y)) == 1) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if (!is.null(nm)) 
      names(Y) <- nm
  }
  X <- mf[,-1,drop=FALSE]
  namesD <- if( is.null( data ) ) { NULL } else { deparse( substitute( data ) ) }
  mis    <- is.na( Y )
  n.mis  <- sum( mis )
  if(is.null(data)){ data<- mf }
  # main program

  lm.cat.imp  <- multinom( formula = formula, data = data, maxit = n.iter, 
                            trace = FALSE , MaxNWts = MaxNWts, ...)
  
  deter.prob  <- predict( lm.cat.imp, newdata = data, type = "p" )
  y.cat       <- as.double( levels ( factor ( Y ) ) )
  if(length(y.cat)<=2){stop(message="number of category must be bigger than 2")}
  determ.pred <- as.vector( deter.prob %*% y.cat )
  names( determ.pred ) <- 1:length( determ.pred )
  random.pred <- Rmultnm( n.mis, deter.prob[mis,], y.cat )
  names( random.pred ) <- names( determ.pred[mis] )
  # return the result
  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), 
                    expected = NULL, random = NULL )         
  result$model$call        <- lm.cat.imp$call
  result$model$call$formula<- formula
  result$model$call$maxit  <- n.iter
  result$model$call$MaxNWts<- MaxNWts
  result$model$coefficient <- coefficients( lm.cat.imp )
  result$model$sigma       <- NULL 
  result$expected <- determ.pred
  result$random   <- random.pred
  result$residual <- Y[ !is.na( Y ) ] - determ.pred[ !is.na( Y ) ] 
  class ( result ) <- c( "mi.categorical", "mi.method","list" )
  
  return( result )
  on.exit( rm( lm.cat.imp ) )
}


## The random Multinomial function (for the categorical variable)
Rmultnm <- function( n, prob.mat, category  ) {
  y.imp <- NULL
  prob  <- prob.mat * NA
  if( is.null( dim( prob.mat ) ) ) {
    prob <- t( rmultinom( 1, 1, prob.mat ) )
  } else {
    for( i in 1:n ){
       prob[i,] <- rmultinom( 1, 1, prob.mat[i, ] )
    }
  }
  y.imp <- as.double( prob %*% category )
  return( y.imp )
}


#
#mi.categorical <- function( Y, X, data = NULL, type = NULL, n.iter = 100, check = TRUE, ...  ) {
#  namesX <- if( is.null( colnames( X ) ) ) { deparse( substitute( X ) ) } else { colnames( X ) }
#  namesY <- deparse( substitute( Y ) )
#  namesD <- if( is.null( data ) ) { NULL } else { deparse( substitute( data ) ) }
#  mis    <- is.na( Y )
#  n.mis  <- sum( mis )
#  if( is.null( data ) ) { data <- data.frame ( cbind ( Y, X ) ) }
#  if ( check ) {
#    if ( is.null( Y ) ) { stop ( message = "Inconplete variable must be specified." ) }
#    if ( is.null( X ) ) { stop ( message = "Predictor matrix must be specified." ) }
#    if( nlevels( factor( Y ) ) > 5 ) { stop ( message = "Incomplete variable must have category of 5 or less." ) }
#    if( !is.vector( Y ) ) { stop ( message = "Incomplete variable must be a vector." ) }
#    if( is.data.frame( X ) ) { X <- as.matrix ( X ) }
#    if( !is.numeric( X ) ) { stop ( message = "Predictor Matrix must be numeric." ) }
#    if( n.mis==0 ) { warning( message = "There is no missing value to impute." ) }
#  }
#  # main program
#  lm.cat.imp  <- multinom( formula = Y ~ X, data = data, maxit = n.iter, trace = FALSE , ...)
#  deter.prob  <- predict( lm.cat.imp, newdata = data, type = "p", )
#  y.cat       <- as.numeric( levels ( factor ( Y ) ) )
#  determ.pred <- as.vector( deter.prob %*% y.cat )
#  names( determ.pred ) <- 1:length( determ.pred )
#  random.pred <- Rmultnm( n.mis, deter.prob[mis,], y.cat )
#  names( random.pred ) <- names( determ.pred[mis] )
#  # return the result
#  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), expected = NULL, random = NULL )
#  result$model$call        <- paste( "multinom( formula = ", namesY, " ~ ",
#                                      paste( namesX, collapse = " + " ),
#                                      ", trace = FALSE, ",
#                                      if( !is.null( namesD ) ){ paste( "data = ", namesD, ", ", sep="" ) }, 
#                                      "maxit = ", n.iter, ")", sep="" )
#  result$model$coefficient <- coefficients( lm.cat.imp )
#  result$model$sigma       <- NULL 
#  #result$model$terms       <- terms(lm.cat.imp)
#  colnames(result$model$coefficient )<- c( "(Intercept)", namesX )
#  result$expected <- determ.pred
#  result$random   <- random.pred
#  result$residual <- Y[!is.na(Y)] - determ.pred[!is.na( Y )] 
#  class ( result ) <- c( "mi.categorical", "mi.method","list" )
#  return( result )
#  on.exit( rm( lm.cat.imp ) )
#}
### The random Multinomial function (for the categorical variable)
#Rmultnm <- function( n, prob.mat, category  ) {
#    y.imp <- NULL
#    prob  <- prob.mat * NA
#    for( i in 1:n ){
#       prob[i,] <- rmultinom( 1, 1, prob.mat[ i, ] )
#    }
#    y.imp <- as.double( prob %*% category )
#  return( y.imp )
#}
#
#
#setMethod("mi.plot", signature(object = "mi.categorical"), 
#function ( object, Yobs, main=deparse( substitute( Yobs ) ),gray.scale = FALSE ) {
#  #par(mfrow=c(1,4))
#  fit     <- mi.expected( object )
#  res     <- mi.resid( object, Yobs )
#  sigma   <- mi.sigma(object)
#  vrb.obs <- Yobs
#  vrb.imp <- mi.imputed( object, Yobs )
#  mi.hist( vrb.obs, object, type = vrb.typ, xlab = main, main = main, gray.scale = gray.scale )
#  binnedplot( fit[ !is.na(Yobs) ], res[ !is.na(Yobs) ], nclass = sqrt( length( fit[ !is.na(Yobs)] ) ), main = main)
#  mtext( "Binned Residual", 3, cex = 0.7, adj = NA ) 
#  mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale )
#  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#
#} 
#)
#setMethod("mi.hist", signature(object = "mi.categorical"),  
# function ( object, Yobs, main = paste("Histogram of ", deparse( substitute( Yobs ) )),  
#                gray.scale = FALSE, xlab = deparse( substitute( Yobs ) ), ylab = "Frequency", 
#                b = NULL, binwidth = NULL, col = c( "black", "blue", "red" ), lty = c( 1, 1, 1 ), lwd = c( 1, 1, 1 ), mlt = 0.1, ... )
#{
#  Yimp <-mi.imputed(object,Yobs)
#  mis  <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
#  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
#  if( is.null( binwidth ) ) { binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) )}
#  if( is.null( b )) { b <- seq( min( Yimp ), max( Yimp ), length.out = sqrt( length( Yimp ) ) )}
#  if( gray.scale == TRUE ) { 
#    col <- c( gray( 0.8 ), gray( 0.6 ), gray( 0 ) ) 
#    lty <- c( 3, 1, 1 )
#  }
#  b <- seq( 0, ceiling( max( Yimp ) ), 0.2 )
#  h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
#  h.mis <- hist( mis, plot = FALSE, breaks = b )
#  h.imp <- hist( Yimp, plot = FALSE, breaks = b )
#  plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
#  xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
#  lab <- as.double( names( table( obs.nomis ) ) )
#  if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
#  histlineplot ( h.mis, shift=-mlt*binwidth, col=col[3], lty = lty[3], lwd = lwd[3] )
#  histlineplot ( h.obs, shift=mlt*binwidth, col=col[2], lty = lty[2], lwd = lwd[2] ) 
#  histlineplot ( h.imp, col=col[1] , lty = lty[1], lwd = lwd[1] )  
#  axis( 1, lab, tick = TRUE, col.axis = 'black' )
#}
#)

# ==============================================================================
# S4 plot function for mi object
# ==============================================================================

#setMethod( "plot", signature( x = "mi", y="missing" ),
#  function ( x, ... ) {
#    plot.mi( x, ... )
#  }
#)

plot.mi <- function ( x, m = 1, vrb = NULL, vrb.name = "Variable Score",
                        gray.scale = FALSE, mfrow=c( 1, 4 ), ... ) {
    if ( m(x) < m )  { 
      stop( message = paste( "Index of imputation 'm' must be within the range of 1 to", m(x) ) ) 
    } else{
      mids <- imp(x,m)
      Y    <- as.data.frame( x@data[ , names( mids ) ] )
      names( Y ) <- names( mids )
      par( mfrow = mfrow )
      for( i in 1:dim( Y )[2] ) {
        par( ask = TRUE )
        if( !is.null( mids[[i]] ) ) {
          plot( x = mids[[i]], y = Y[ ,names( mids )[i]], main = names( Y )[ i ] )
        }
      }
    }
  }
# ==============================================================================
# S4 plot function for mi.method object
# ==============================================================================

plot.mi.method <- function( x, y, 
  main = deparse( substitute( y ) ), gray.scale = FALSE, ... ){      
    fit   <- fitted( x )
    res   <- residuals( x, y )
    sigma <- sigma.hat( x )
    vrb.obs <- y
    vrb.imp <- imputed( x, y )
    mi.hist(x, vrb.obs, xlab=main, main = main, gray.scale = gray.scale )
    residual.plot( fit, res, sigma, main = main, gray.scale = gray.scale )
    binnedplot ( fit[ !is.na( y )], res[ !is.na( y )], 
              nclass = sqrt( length( fit[  !is.na( y )] ) ), main = main )
    mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, 
                      main = main, gray.scale = gray.scale )
    #plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
}


#setMethod( "plot", signature( x = "mi.method", y ="ANY"), 
#  function( x, y, main = deparse( substitute( y ) ), gray.scale = FALSE ){      
#    fit   <- fitted( x )
#    res   <- resid( x, y )
#    sigma <- sigma.hat( x )
#    vrb.obs <- y
#    vrb.imp <- imputed( x, y )
#    mi.hist(  object, vrb.obs, xlab=main, main = main, gray.scale = gray.scale )
#    residual.plot( fit, res, sigma, main = main, gray.scale = gray.scale )
#    binnedplot ( fit[ !is.na( y )], res[ !is.na( y )], 
#              nclass = sqrt( length( fit[  !is.na( y )] ) ), main = main )
#    mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, 
#                      main = main, gray.scale = gray.scale )
#    #plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#  }
#)

# ==============================================================================
# S4 plot function for mi.mixed object
# =======================================================================

plot.mi.mixed <-  function ( x, y, 
  main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
  #par(mfrow=c(1,4))
  fit     <- fitted( x )
  res     <- residuals(x, y)
  sigma   <- sigma.hat( x )
  vrb.obs <- y
  vrb.imp <- imputed( x, y )
  fit1 <- fit[[1]]
  fit2 <- fit[[2]]
  res1 <- res[[1]]
  res2 <- res[[2]]    
  mi.hist ( x, vrb.obs, xlab = main, main = main, gray.scale = gray.scale )
  binnedplot( fit1[ !is.na( y ) ], res1[ !is.na( y ) ], nclass=sqrt(length(fit1[ !is.na( y ) ])), main = main)
  residual.plot ( fit2[y>0], res2[y>0], sigma, main = main, gray.scale = gray.scale )
  mtext( "sqrt", 2, cex = 0.7, adj = 1 )
  mi.scatterplot ( vrb.obs, vrb.imp, fit1*fit2,xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale, display.zero=FALSE )
}



#setMethod("plot", signature(x = "mi.mixed",y="ANY"), 
# function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE ) {
#  #par(mfrow=c(1,4))
#  fit     <- fitted( x )
#  res     <- resid( x, y )
#  sigma   <- sigma.hat( x )
#  vrb.obs <- y
#  vrb.imp <- imputed( x, y )
#  fit1 <- fit[[1]]
#  fit2 <- fit[[2]]
#  res1 <- res[[1]]
#  res2 <- res[[2]]    
#  mi.hist ( x, vrb.obs, xlab = main, main = main, gray.scale = gray.scale )
#  binnedplot( fit1[ !is.na( y ) ], res1[ !is.na( y ) ], nclass=sqrt(length(fit1[ !is.na( y ) ])), main = main)
#  residual.plot ( fit2[y>0], res2[y>0], sigma, main = main, gray.scale = gray.scale )
#  #mtext( "sqrt", 2, cex = 0.7, adj = 1 )
#  mi.scatterplot ( vrb.obs, vrb.imp, fit1*fit2,xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale, display.zero=FALSE )
#}
#)

# ==============================================================================
# S4 plot function for mi.sqrtcontinuous object
# ==============================================================================


plot.mi.sqrtcontinuous <- function ( x, y, 
  main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
  #par(mfrow=c(1,4))
  fit     <- fitted( x )
  res     <- residuals( x, y )
  sigma   <- sigma.hat( x )
  vrb.obs <- y
  vrb.imp <- imputed( x, y )
  sqrta <- vrb.obs
  sqrta[!is.na(vrb.obs)] <- sqrt(vrb.obs[!is.na(vrb.obs)])
  mi.hist(x, vrb.obs, main = main, xlab=main, gray.scale = gray.scale )
  #mtext( "sqrt", 1, cex = 0.7, adj = 1 )
  residual.plot( fit, res, sigma, main = main, xlab= "Predicted",  ylab = paste( "Residual" ), gray.scale = gray.scale )
  mtext( "sqrt", 2, cex = 0.7, adj = 1 )
  mi.scatterplot( vrb.obs, vrb.imp, X=fit, xlab= "Predicted",  ylab =  main , main = main, gray.scale = gray.scale )
  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 



#setMethod("plot", signature(x = "mi.sqrtcontinuous",y ="ANY"), 
#function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
#  #par(mfrow=c(1,4))
#  fit     <- fitted( x )
#  res     <- resid( x, y )
#  sigma   <- sigma.hat( x )
#  vrb.obs <- y
#  vrb.imp <- imputed( x, y )
#  sqrta <- vrb.obs
#  sqrta[!is.na(vrb.obs)] <-sqrt(vrb.obs[!is.na(vrb.obs)])
#  mi.hist( x, vrb.obs,main = main, xlab=main, gray.scale = gray.scale )
#  #mtext( "sqrt", 1, cex = 0.7, adj = 1 )
#  residual.plot( fit, res, sigma, main = main, xlab= "Predicted",  ylab = paste( "Residual" ), gray.scale = gray.scale )
#  mtext( "sqrt", 2, cex = 0.7, adj = 1 )
#  mi.scatterplot( vrb.obs, vrb.imp, X=fit, xlab= "Predicted",  ylab =  main , main = main, gray.scale = gray.scale )
#  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#} 
#)

# ==============================================================================
# S4 plot function for mi.polr object
# ==============================================================================


plot.mi.polr <- function ( x, y, 
  main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
  #par(mfrow=c(1,4))
  y       <- .factor2num( y )
  fit     <- .factor2num( fitted( x ))
  res     <- .factor2num( residuals( x, y ))
  sigma   <- .factor2num( sigma.hat( x ) )
  vrb.obs <- .factor2num( y )
  vrb.imp <- .factor2num( imputed( x, y ) )
  mi.hist(  x, vrb.obs, xlab = main, main = main, gray.scale = gray.scale ) 
  binnedplot( fit[  !is.na( y ) ], res[  !is.na( y ) ], nclass = sqrt( length( fit[ !is.na( y ) ] ) ), main = main )
  mtext( "Binned Residual", 3, cex = 0.7, adj = NA ) 
  mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale )
  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 


#setMethod("plot", signature(x = "mi.polr",y="ANY"), 
#function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE ) {
#  #par(mfrow=c(1,4))
#  y       <- factor2num( y )
#  fit     <- factor2num( fitted( x ))
#  res     <- factor2num( resid( x, y ))
#  sigma   <- factor2num( sigma.hat( x ) )
#  vrb.obs <- factor2num( y )
#  vrb.imp <- factor2num( imputed( x, y ) )
#  mi.hist(  x, vrb.obs,xlab = main, main = main, gray.scale = gray.scale ) 
#  binnedplot( fit[  !is.na( y ) ], res[  !is.na( y ) ], nclass = sqrt( length( fit[ !is.na( y ) ] ) ), main = main )
#  mtext( "Binned Residual", 3, cex = 0.7, adj = NA ) 
#  mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale )
#  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#} 
#)

# ==============================================================================
# S4 plot function for mi.categorical object
# ==============================================================================

plot.mi.categorical <- function ( x, y, 
  main=deparse( substitute( y ) ),gray.scale = FALSE, ... ) {
  #par(mfrow=c(1,4))
  fit     <- fitted( x )
  res     <- residuals( x, y )
  sigma   <- sigma.hat( x )
  vrb.obs <- y
  vrb.imp <- imputed( x, y )
  mi.hist(  x, Yobs=vrb.obs, type = vrb.typ, xlab = main, main = main, gray.scale = gray.scale )
  binnedplot( fit[ !is.na(y) ], res[ !is.na(y) ], nclass = sqrt( length( fit[ !is.na(y)] ) ), main = main)
  mtext( "Binned Residual", 3, cex = 0.7, adj = NA ) 
  mi.scatterplot( Yobs=vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale )
  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 

#setMethod("plot", signature(x = "mi.categorical", y="ANY"), 
#function ( x, y, main=deparse( substitute( y ) ),gray.scale = FALSE ) {
#  #par(mfrow=c(1,4))
#  fit     <- fitted( x )
#  res     <- resid( x, y )
#  sigma   <- sigma.hat( x )
#  vrb.obs <- y
#  vrb.imp <- imputed( x, y )
#  mi.hist(  x, vrb.obs, type = vrb.typ, xlab = main, main = main, gray.scale = gray.scale )
#  binnedplot( fit[ !is.na(y) ], res[ !is.na(y) ], nclass = sqrt( length( fit[ !is.na(y)] ) ), main = main)
#  mtext( "Binned Residual", 3, cex = 0.7, adj = NA ) 
#  mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale )
#  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#
#} 
#)

# ==============================================================================
# S4 plot function for mi.dichotomous object
# ==============================================================================

plot.mi.dichotomous <- function ( x, y, 
  main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
          #par(mfrow=c(1,4))
          fit     <- fitted( x )
          res     <- residuals( x, y )
          sigma   <- sigma.hat( x )
          vrb.obs <- y
          vrb.imp <- imputed( x, y )
          mi.hist ( x, Yobs=vrb.obs, xlab = main, main = main, gray.scale = gray.scale )
          binnedplot ( fit[ !is.na( y )], res[ !is.na( y )], nclass = sqrt( length( fit[  !is.na( y )] ) ), main = main )
          mtext( "Binned Residual", 3, cex = 0.7, adj = NA )
          mi.scatterplot( Yobs=vrb.obs, vrb.imp, fit, xlab = "Predicted", ylab = main, main = main, gray.scale = gray.scale )
          plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 


#
#setMethod("plot", signature(x = "mi.dichotomous",y="ANY"), 
#function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE ) {
#          #par(mfrow=c(1,4))
#          fit     <- fitted( x )
#          res     <- resid( x, y )
#          sigma   <- sigma.hat( x )
#          vrb.obs <- y
#          vrb.imp <- imputed( x, y )
#          mi.hist ( x, vrb.obs, xlab = main, main = main, gray.scale = gray.scale )
#          binnedplot ( fit[ !is.na( y )], res[ !is.na( y )], nclass = sqrt( length( fit[  !is.na( y )] ) ), main = main )
#          mtext( "Binned Residual", 3, cex = 0.7, adj = NA )
#          mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "Predicted", ylab = main, main = main, gray.scale = gray.scale )
#          plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#} 
#)
#

# ==============================================================================
# S4 plot function for mi.logcontinuous object
# ==============================================================================

plot.mi.logcontinuous <- function ( x, y, 
  main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
    par(mfrow=c(1,4))
    fit     <- fitted( x )
    res     <- residuals( x, y )
    sigma   <- sigma.hat( x )
    vrb.obs <- y
    vrb.imp <- imputed( x, y ) 
    loga  <- vrb.obs
    loga[!is.na(vrb.obs)] <- log(vrb.obs[!is.na(vrb.obs)])
    mi.hist(x, loga, type = vrb.typ, main = main, xlab=paste("log(",main,")"), gray.scale = gray.scale )
    mtext( "log", 1, cex = 0.7, adj = 1 )
    residual.plot( log(fit), res, sigma, main = main, xlab= "log(Predicted)",  ylab = paste( "log(Residual)" ), gray.scale = gray.scale )
    mtext( "log", 1, cex = 0.7, adj = 1 )
    mtext( "log", 2, cex = 0.7, adj = 1 )
    mi.scatterplot( loga, log(vrb.imp), fit, xlab= "predicted",  ylab = paste( "log(", main, ")" ) , main = main, gray.scale = gray.scale )
    mtext( "log", 2, cex = 0.7, adj = 1 )
    plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 


#setMethod("plot", signature(x = "mi.logcontinuous",y="ANY"), 
#  function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
#    par(mfrow=c(1,4))
#    fit     <- fitted( x )
#    res     <- resid( x, y )
#    sigma   <- sigma.hat( x )
#    vrb.obs <- y
#    vrb.imp <- imputed( x, y ) 
#    loga  <- vrb.obs
#    loga[!is.na(vrb.obs)] <-log(vrb.obs[!is.na(vrb.obs)])
#    mi.hist(  x, loga, type = vrb.typ, main = main, xlab=paste("log(",main,")"), gray.scale = gray.scale )
#    mtext( "log", 1, cex = 0.7, adj = 1 )
#    residual.plot( log(fit), res, sigma, main = main, xlab= "log(Predicted)",  ylab = paste( "log(Residual)" ), gray.scale = gray.scale )
#    mtext( "log", 1, cex = 0.7, adj = 1 )
#    mtext( "log", 2, cex = 0.7, adj = 1 )
#    mi.scatterplot( loga, log(vrb.imp), fit, xlab= "predicted",  ylab = paste( "log(", main, ")" ) , main = main, gray.scale = gray.scale )
#    mtext( "log", 2, cex = 0.7, adj = 1 )
#    plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#  } 
#)

# ==============================================================================
# S4 plot function for mi.copy object
# ==============================================================================

plot.mi.copy <- function ( x, y, 
  main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
}


#setMethod("plot", signature(x = "mi.copy",y="ANY"), 
# function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE ) {
#  par(mfrow=c(1,4))
#  #  fit     <- mi.expected( object )
#  #  res     <- mi.resid( object, Yobs )
#  #  sigma   <- mi.sigma(object)
#  #  vrb.obs <- Yobs
#  #  vrb.imp <- mi.imputed( object, Yobs )
#  #  fit1 <- fit[[1]]
#  #  fit2 <- fit[[2]]
#  #  res1 <- res[[1]]
#  #  res2 <- res[[2]]    
#  #  mi.hist ( vrb.obs, object, xlab = main, main = main, gray.scale = gray.scale )
#  #  binnedplot( fit1[ !is.na( Yobs ) ], res1[ !is.na( Yobs ) ], nclass=sqrt(length(fit1[ !is.na( Yobs ) ])), main = main)
#  #  residual.plot ( fit2, res2, sigma, main = main, gray.scale = gray.scale )
#  #  #mtext( "sqrt", 2, cex = 0.7, adj = 1 )
#  #  mi.scatterplot ( vrb.obs, vrb.imp, xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale, display.zero=FALSE )
#  }
#)

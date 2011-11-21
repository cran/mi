# ==============================================================================
# S4 plot function for mi object
# ==============================================================================

setMethod( "plot", signature( x = "mi", y="ANY" ),
  function(x, y, ...) {
    plot.mi(x, y, ...)
  }
)

plot.mi <- function ( x, y, m = 1, vrb = NULL, vrb.name = "Variable Score",
                        gray.scale = FALSE, mfrow=c(1, 3), ... ) 
{
  if (m(x) < m) { 
    stop(message = paste("Index of imputation 'm' must be within the range of 1 to", m(x))) 
  } 
  else{
    mids <- imp(x, m)
#    if(x@preprocess){
#      x@data <- mi.preprocess(x@data, info=x@mi.info)$data
#    }z
    Y    <- as.data.frame(x@data[ , names(mids)])
    names(Y) <- names(mids)
    par(mfrow = mfrow)
    for(i in 1:dim(Y)[2]){
      par( ask = TRUE )
      old.par <- par(no.readonly = TRUE)  
      par(mar=c(3,3,3,1), mgp=c(1.5,.25,0), tcl=-0.2)
      if(!is.null(mids[[i]])) {
        plot(x = mids[[i]], y = Y[ ,names(mids)[i]], main = names(Y)[i], gray.scale=gray.scale)
      }
    }
  }
  par(old.par)
}

# ==============================================================================
# S4 plot function for mi.method object
# ==============================================================================


setMethod( "plot", signature( x = "mi.method", y ="ANY"), 
  function( x, y, main = deparse( substitute( y ) ), gray.scale = FALSE, ...){      
    fit   <- fitted( x )
    res   <- residuals( x, y )
    sigma <- sigma.hat( x )
    vrb.obs <- y
    vrb.imp <- imputed( x, y )
    fit.imp <- x@model$startY
    res.imp <- fit.imp - fit[is.na(y)]
    mi.hist(x, vrb.obs, xlab=main, main = main, gray.scale = gray.scale )
    
    #residual.plot( fit, res, sigma, main = main, gray.scale = gray.scale)
    #points(fit.imp, res.imp, col = ifelse(gray.scale, "black", "red"), pch=19, cex=0.5)
    
    binnedplot ( fit[ !is.na( y )], res[ !is.na( y )], col.pts = ifelse(gray.scale, "black", "blue"), 
              nclass = sqrt( length( fit[  !is.na( y )] ) ), main = main )
    aa <- data.frame(binned.resids (fit.imp, res.imp, nclass=sqrt(length(fit[!is.na(y)])))$binned)
    points (aa$xbar, aa$ybar, pch=19, cex=0.8, col=ifelse(gray.scale, "black", "red"))
    
    mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, 
                      main = main, gray.scale = gray.scale )
    #plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
  }
)

# ==============================================================================
# S4 plot function for mi.polr object
# ==============================================================================

setMethod("plot", signature(x = "mi.polr", y="ANY"), 
function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, ...) {
  #par(mfrow=c(1,4))
  fit     <- .factor2num( fitted( x ))
  res     <- .factor2num( residuals( x, y ))
  sigma   <- .factor2num( sigma.hat( x ) )
  vrb.obs <- .factor2num( y )
  vrb.imp <- .factor2num( imputed( x, y ) )
  mi.hist(  x, y, xlab = main, main = main, gray.scale = gray.scale ) 
#  binnedplot( fit[  !is.na( y ) ], res[  !is.na( y ) ], nclass = sqrt( length( fit[ !is.na( y ) ] ) ), main = main )
#  mtext( "Binned Residual", 3, cex = 0.7, adj = NA ) 
  mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "expected", ylab = main, main = main, gray.scale = gray.scale )
  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 
)

# ==============================================================================
# S4 plot function for mi.categorical object
# ==============================================================================

setMethod("plot", signature(x = "mi.categorical", y="ANY"), 
function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, ...) {
  #par(mfrow=c(1,4))
  fit     <- as.numeric(fitted( x ))
  res     <- residuals(x, y)
  sigma   <- sigma.hat( x )
  vrb.imp <- as.numeric(imputed(x, y))
  vrb.obs <- as.numeric(y)
  mi.hist(  x, y, type = vrb.typ, xlab = main, main = main, gray.scale = gray.scale )
#  binnedplot( fit[ !is.na(y) ], res[!is.na(y)], nclass = sqrt( length( fit[ !is.na(y)] ) ), main = main)
#  mtext( "Binned Residual", 3, cex = 0.7, adj = NA ) 
  mi.scatterplot( Yobs=vrb.obs, vrb.imp, fit, xlab = "expected", ylab = main, main = main, gray.scale = gray.scale )
  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
#  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 
)

# ==============================================================================
# S4 plot function for mi.binary object
# ==============================================================================


setMethod("plot", signature(x = "mi.binary",y="ANY"), 
function ( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, ...) {
   #par(mfrow=c(1,4))
   fit     <- fitted( x )
   res     <- residuals( x, y )
   sigma   <- sigma.hat( x )
   vrb.obs <- y
   vrb.imp <- imputed( x, y ) 
   fit.imp <- fit[is.na(y)]
   res.imp <- x@model$startY - fit[is.na(y)]

   mi.hist ( x, Yobs=vrb.obs, xlab = main, main = main, gray.scale = gray.scale )

   binnedplot ( fit[ !is.na( y )], res[ !is.na( y )], col.pts = ifelse(gray.scale, "black", "blue"), 
    nclass = sqrt( length( fit[  !is.na( y )] ) ), main = main )
   aa <- data.frame(binned.resids (fit.imp, res.imp, nclass=sqrt(length(fit[!is.na(y)])))$binned)
   points (aa$xbar, aa$ybar, pch=19, cex=0.8, col=ifelse(gray.scale, "black", "red"))
   
   mtext( "Binned Residual", 3, cex = 0.7, adj = NA )
   mi.scatterplot( Yobs=vrb.obs, vrb.imp, fit, xlab = "Predicted", ylab = main, main = main, gray.scale = gray.scale )
#   plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
}
)





# ==============================================================================
# S4 plot function for mi.copy object
# ==============================================================================

#plot.mi.copy <- function ( x, y, 
#  main=deparse( substitute( y ) ), gray.scale = FALSE, ... ) {
#}


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

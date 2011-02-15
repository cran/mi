# ==============================================================================
# scatter plot for the completed, observed and imputed values
# ==============================================================================

#setMethod("mi.scatterplot", signature( Yobs = "ANY", Yimp = "ANY"),  
mi.scatterplot <- function ( Yobs, Yimp, X = NULL, xlab = NULL, ylab = NULL, 
                            main = "Imputed Variable Scatter Plot", 
                             display.zero = TRUE, gray.scale = FALSE, 
                              obs.col = rgb( 0, 0, 1 ), 
                              imp.col = rgb( 1, 0, 0 ), 
                              obs.pch = 20 , imp.pch = 20, 
                              obs.cex = 0.3, imp.cex = 0.3, 
                              obs.lty = 1  , imp.lty = 1, 
                              obs.lwd = 2.5, imp.lwd = 2.5, ... ) {
  call <- match.call()
  if ( is.null( X ) ) { 
    X    <- seq( length( Yobs ) )
    xlab <- "Index"
  } else {
    if ( is.null( xlab ) ) { xlab <- deparse( substitute( X ) ) }
  }
  if ( is.null ( Yobs ) || is.null ( Yimp ) ) { 
    stop ( message = "Both observed and imputed data must be specified." ) 
  } else {
    if ( is.null( ylab ) ) { 
      ylab <- paste( deparse( substitute( Yobs ) ), " & ", deparse( substitute( Yimp ) ) ) 
    }
  }
  if ( gray.scale ) {
    obs.col <- gray( 0.8 )
    imp.col <- gray( 0.3 )
  }
  if( display.zero ) {         
    X.obs <- X[ !is.na( Yobs ) ]
    X.imp <- X[ is.na( Yobs ) ]
    Y.obs <- Yobs[ !is.na( Yobs ) ]
    Y.imp <- Yimp[ is.na( Yobs ) ]
  } else {
    X.obs <- X[ !is.na( Yobs ) & Yimp > 0 ]    
    X.imp <- X[ is.na( Yobs ) & Yimp > 0  ]     
    Y.obs <- Yobs[ !is.na( Yobs ) & Yimp > 0 ] 
    Y.imp <- Yimp[ is.na( Yobs ) & Yimp > 0 ]  
  }
  j.X.obs <- jitter( X.obs )
  j.Y.obs <- jitter( Y.obs )
  j.X.imp <- jitter( X.obs )
  j.Y.imp <- jitter( Y.obs )
  if ( length(  X.imp )  >   0  ) {
    j.X.imp <- jitter( X.imp )
    j.Y.imp <- jitter( Y.imp )
  }
  btx <- c( j.X.obs, j.X.imp )
  bty <- c( j.Y.obs, j.Y.imp )
  #pt.range <- range(btx, bty)
  #browser()
  plot( range( btx ), range( bty ), frame.plot = TRUE, type = "n", 
          xlab = xlab, ylab = ylab, main = main, ... )
  abline(a=0, b=1, lty=2)
  #observed 
  points( jitter( X.obs ), jitter( Y.obs ), col = obs.col, pch = obs.pch, 
            cex = obs.cex ) 
  lines( lowess( X.obs, Y.obs ), col = obs.col, lwd = obs.lwd, lty = obs.lty )
  #imputed
  if (length(  X.imp )  >   0  ) {
    points( jitter( X.imp ), jitter( Y.imp ), col = imp.col, pch = imp.pch, 
              cex = imp.cex )        
#    lines( lowess( X.imp, Y.imp ), col = imp.col, lwd = imp.lwd, lty = imp.lty )
  }
}
#)

marginal.scatterplot<-function ( data, object, use.imputed.X = FALSE, ...  ){
  info  <- info.mi( object )
  nmis  <- .nmis(info)
  dName <- dimnames( data )[[2]]
  all.missing <- .all.missing(info)
  name.list   <- names(nmis)[nmis >0 & !all.missing]
  J       <-  length( name.list )  
  impData <- mi.data.frame( object )
  par( ask = TRUE )
  for ( j in 1:J ) {
    yobs  <- data[,dName == name.list[j]]
    yimp  <- impData[,dName == name.list[j]]
    X     <- if( use.imputed.X ){
                impData[ ,dName != name.list[j]]
              }else{
                data[ ,dName != name.list[j]]
              }
    tName <- dName[dName != name.list[j]]
    K     <- dim( X )[2]
      for( k in 1:K ) {
        mi.scatterplot( Yobs = yobs[ !is.na( X[ , k] ) ], 
                        Yimp = yimp[ !is.na( X[ , k] ) ],
                         X = X[!is.na( X[ ,k] ), k], 
                         xlab = tName[k], ylab = name.list[j],
                         main =  paste(tName[k], "vs",name.list[j] ), ... )
      }
  }
}

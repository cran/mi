# ==============================================================================
# missing pattern plot
# ==============================================================================
mp.plot <- missing.pattern.plot <- function ( data, y.order = FALSE, x.order = FALSE, 
                                    clustered = TRUE, 
                                    xlab = "Index", ylab = "Variable", 
                                    main = NULL, gray.scale = FALSE,
                                    obs.col = "blue", mis.col = "red", ... ) 
{

  if (is.null(main)) {
    main <- deparse( substitute( data ) )
  }
  index <- seq(nrow(data))
  x.at =  1:nrow( data )
  x.lab = index
  if( y.order ) { 
    data <- data[ ,order( colSums( is.na( data ) ), decreasing = TRUE ) ] 
    ylab = "Ordered by number of missing items" 
  }
  if( x.order ) { 
    data <- data[order( rowSums( is.na( data ) ), decreasing = FALSE ), ] 
    index<- row.names( data )
    xlab = "Ordered by number of missing items" 
    x.at = NULL
    x.lab= FALSE
  }
  missingIndex <- as.matrix(is.na(data))*1
  if(clustered){
    orderIndex <-  order.dendrogram(as.dendrogram(hclust(dist(missingIndex), method="mcquitty")))
    missingIndex <- missingIndex[orderIndex,]
  }
  col <- if( gray.scale ){ 
           gray(c(0, 1)) 
         } 
         else { 
           c(obs.col, mis.col) 
         }
#  par( mar = c( 4.5, 11, 3, 1 ) )
#  par( mgp = c( 1, .3, 0 ) )
#  par( cex.lab = 0.7 )
  image(x = 1:nrow(data), y = 1:ncol(data), z = missingIndex, 
        ylab = "", xlab = xlab, main = main, col = col ,yaxt = "n",
        tck = -0.05, xaxt="n", ...)
  box( "plot" )
  axis( side = 2, at = 1:ncol( data ), labels = names( data ), las = 1, 
         tick = FALSE, yaxs = "r", tcl = 0.3, xaxs ="i", yaxs = "i" )
  mtext( ylab, side = 3 , line = 10, cex=0.7)
  if( x.order ) { 
    axis( side = 1, at =x.at, labels = x.lab, tick = FALSE, 
          xaxs = "i", las = 1 )   
  } 
}

# ==============================================================================
# imputation function for ordered categorical variable
# ==============================================================================
mi.polr <- function ( formula, data = NULL, drop.unused.levels = TRUE, 
                       start = NULL, n.iter = 100, ... ) {
  call <- match.call();
  mf   <- match.call(expand.dots = FALSE);
  m    <- match(c("formula", "data"), names(mf), 0);
  mf   <- mf[c(1, m)];
  mf$drop.unused.levels <- TRUE;
  mf$na.action <- na.pass;
  mf[[1]] <- as.name( "model.frame" );
  mf <- eval( mf, parent.frame( ) );
  mt <- attr( mf, "terms" );
  Y  <- model.response( mf, "any" );
  if ( length( dim( Y ) ) == 1 ) {
    nm <- rownames( Y );
    dim( Y ) <- NULL;
    if ( !is.null( nm ) ) 
      names ( Y ) <- nm;
  }
  X <- as.matrix( mf[ , -1, drop = FALSE ] );
  namesD <- if( is.null( data ) ) { 
              NULL;
            } else { 
              deparse( substitute( data ) );
            }
  mis    <- is.na( Y );
  n.mis  <- sum( mis );
  if(is.null(data)){
    data<- mf;
  }
  # convert the levels
  Y.levels <- levels( factor( Y ) );
  Y.nlevel <- nlevels( factor( Y ) );
  if( is.numeric( Y ) ) { Y.levels <- as.double( Y.levels ) }
  Y.org <- Y
#    for ( i in 1:Y.nlevel ) {
#        Y <- replace ( Y, Y.org == Y.levels[i], i )
#    }
    levels( Y ) <- c(1:Y.nlevel)
  Y  <- factor( as.double( Y ) )
  #if( is.null( data ) ) { data <- data.frame( cbind( factor( Y ), X ) ) }
  #if(is.null(data)){ data<- data.frame(factor(mf[,1]),mf[,-1]) }
  if( is.null( data ) ){ data <- data.frame( mf )}

  # main program
  #if( !is.null( start ) ){ n.iter <- 1 } 
  bplr.imp    <- bayespolr( formula = formula, data = data, start = 0, 
                              method = c( "logistic" ), 
                              drop.unused.levels = FALSE, n.iter = n.iter );
#    bplr.imp    <- polr( formula = formula, data = data,  
#                              method = c( "logistic" ) );

#  bplr.imp    <- bayespolr2( formula = formula, data = data, method = c( "logistic" ), 
#                  drop.unused.levels = FALSE, n.iter = n.iter,start=0) #, start = start );
#  #bplr.imp    <- bayespolr( formula = formula, data = data, method = c( "logistic" ), drop.unused.levels = FALSE, n.iter = n.iter )
  expect.prob <- predict( bplr.imp, newdata = data, type = "probs" )
  determ.pred <- as.vector( expect.prob %*% as.double( Y.levels ) )
  names( determ.pred ) <- 1:length( determ.pred )
  random.pred <- Rmultnm( n.mis, expect.prob[mis,],  c( 1:Y.nlevel ) )    
  #names( determ.pred ) <- NULL
  # reconvert the levels
  #imputed.vctr <- imputed.tmp

    random.pred <-  recode( random.pred, paste(1:Y.nlevel,"='",Y.levels,"'",sep="",collapse=";") )        
    #imputed.vctr <- replace( imputed.vctr, imputed.tmp == i, Y.levels[i] )
  # return the result
  result <- list( model = list( call = NULL, 
                                coefficient = NULL, 
                                sigma = NULL ), 
                  expected = NULL, 
                  random = NULL )
  result$model$call        <- bplr.imp$call
  result$model$call$formula<- as.formula(formula)
  result$model$call$start  <- round(as.double(start),2)
  result$model$call$n.iter <- n.iter
  result$model$coefficient <- bplr.imp$coefficient
  result$model$sigma       <- NULL  #sigma.hat( bplr.imp ) 
  result$expected          <- as.double(determ.pred)
  result$random            <- as.double(random.pred)
  names(result$random)     <- names( determ.pred[mis] )
  class( result )          <- c( "mi.polr", "mi.method", "list" )
#  result <-new("mi.polr",
#            model    = list( call = bplr.imp$call,
#                             call$formula = as.formula( formula ),
#                             call$start   = round(as.double( start ), 2 ),
#                             call$n.iter  = n.iter,
#                             coefficient  = bplr.imp$coefficient,
#                             sigma        = NULL)
#            expected = determ.pred,
#            random   = random.pred);
#  names(result@random)     <- names( determ.pred[mis] );
  return( result )
  on.exit( rm( bplr.imp ) )
}

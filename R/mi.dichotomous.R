# ==============================================================================
# imputation function for dichotomous variable
# ==============================================================================
mi.dichotomous <- function( formula, data = NULL, start = NULL, n.iter = 100,
                             draw.from.beta=FALSE,... ) {
  call <- match.call( );
  mf   <- match.call( expand.dots = FALSE );
  m    <- match( c( "formula", "data" ), names( mf ), 0 );
  mf   <- mf[c( 1, m )];
  mf$drop.unused.levels <- TRUE;
  mf$na.action <- na.pass;
  mf[[1]] <- as.name( "model.frame" );
  mf <- eval( mf, parent.frame( ) );
  mt <- attr( mf, "terms" );
  Y  <- model.response( mf, "any" );
  if ( length( dim( Y ) ) == 1 ) {
    nm <- rownames( Y );
    dim( Y ) <- NULL;
    if ( !is.null( nm ) ) { names( Y ) <- nm; }
  }
  X <- as.matrix( mf[, -1, drop = FALSE] );
  namesD <- if( is.null( data ) ) { NULL; } else { deparse( substitute( data ) ); }
  mis    <- is.na( Y );
  n.mis  <- sum( mis );
  if(is.null(data)){ data <- mf; }
    # 0/1 conversion
    y.levels <- if ( is.numeric( Y ) ) { sort( unique ( Y ));} 
                else if ( is.logical( Y ) ) { c( FALSE, TRUE ); }
                else { levels( factor( Y ) ); }
  # Y <-replace( Y, Y==y.levels[1], 0 )
  Y <- recode( Y, paste ( "'", y.levels, "'=", c(0,1), sep="", collapse="; " ) );
  data <- mf;
  data[,1] <- Y;
  # main program
  if( !is.null( start ) ){ 
    n.iter <- 1;
    start[is.na(start)]<-0;
    } 
    
   bglm.imp    <- bayesglm( formula = formula, data = data, 
                              family = binomial ( link = "logit" ), 
                              n.iter = n.iter, start = start, 
                               drop.unused.levels=FALSE, Warning=FALSE, ... )

    #determ.pred <- predict ( bglm.imp, newdata = data, type = "response" )
    determ.pred <- predict ( bglm.imp, newdata = data, type = "response" );
    if( draw.from.beta ) {
      sim.bglm.imp<- sim( bglm.imp,1 );
      prob.pred   <- invlogit( tcrossprod(cbind( ( X[mis,1,drop=FALSE]*0 + 1 ), X[mis,,drop=FALSE]), sim.bglm.imp$beta ) );
      random.temp <- rbinom  ( n.mis, 1, prob.pred );
    } else{
      random.temp <- rbinom  ( n.mis, 1, determ.pred[mis] ); 
    }
    
    # reverse 0/1 conversion
    random.pred <- random.temp;
    random.pred <- replace ( random.pred, random.temp == 0, y.levels[1] );
    random.pred <- replace ( random.pred, random.temp == 1, y.levels[2] ); 
    if ( is.logical( y.levels ) ){ random.pred <- as.logical( random.pred ); }
    names( random.pred ) <- names( determ.pred[mis] );
    
    # calculate residual
    # residual.val<- bglm.imp$residuals #Y - determ.pred
    # return the result
    result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), expected = NULL, random = NULL );
    #result$model$call        <- gettextf("bayesglm( formula = %s, data = %s, family = binomial ( link = 'logit' ), n.iter = %d, start = start, drop.unused.levels=FALSE, ... )",formula, namesD, n.iter)
    result$model$call        <- bglm.imp$call
    result$model$call$formula<- as.formula( formula );
    result$model$call$start  <- round(as.double( start ), 2 );
    result$model$call$n.iter <- n.iter;
    result$model$coefficient <- bglm.imp$coefficients;
    result$model$sigma       <- sigma.hat( bglm.imp );
    result$expected <- determ.pred;
    result$random   <- random.pred;
    #result$residual <- residual.val
    class ( result ) <- c( "mi.dichotomous", "mi.method", "list" );
    return( result );
    on.exit( rm( bglm.imp ) );
}

dicot<-function(Y){
    y.levels <- if ( is.double( Y ) ) sort( unique ( Y ) ) else levels( factor( Y ) );
    Y <-replace( Y, Y==y.levels[1], 0 );
    Y <-replace( Y, Y==y.levels[2], 1 );
    return(Y);
}

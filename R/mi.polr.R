# ==============================================================================
# imputation function for ordered categorical variable
# ==============================================================================
mi.polr <- function ( formula, data = NULL, drop.unused.levels = TRUE, 
                       start = NULL, n.iter = 100, ... ) {
  call <- match.call()
  mf   <- match.call(expand.dots = FALSE)
  m    <- match(c("formula", "data"), names(mf), 0)
  mf   <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name( "model.frame" )
  mf <- eval( mf, parent.frame( ) )
  mt <- attr( mf, "terms" )
  Y  <- model.response( mf, "any" )
  if ( length( dim( Y ) ) == 1 ) {
    nm <- rownames( Y )
    dim( Y ) <- NULL
    if ( !is.null( nm)) 
      names ( Y ) <- nm
  }
#  X <- as.matrix( mf[ , -1, drop = FALSE ] )
  namesD <- if( is.null( data ) ) { 
              NULL
            } 
            else { 
              deparse( substitute( data ) )
            }
  mis    <- is.na( Y )
  n.mis  <- sum( mis )
  if(is.null(data)){
    data <- mf
  }

  # convert the levels
  Y.levels <- levels(ordered(Y))
  Y.nlevel <- nlevels(ordered(Y))
  response.name <- names(mf)[1]  
  form <- formula
  form <- gsub(response.name, paste("ordered(", response.name, ")", sep=""), form)
  if(!is.na(form[2])){
    form <- as.formula(paste(form[2], form[1], form[3]))
  }


#
#  if( is.numeric(Y)){ 
#    Y.levels <- as.double(Y.levels) 
#  }
#
#  Y.org <- Y
#  levels( Y ) <- 1:Y.nlevel
#  Y  <- factor( as.double( Y ) )

  bplr.imp    <- bayespolr( formula = form, data = data, start = 0, 
                              method = c( "logistic" ), 
                              drop.unused.levels = FALSE, n.iter = n.iter )
  expect.prob <- predict( bplr.imp, newdata = data, type = "probs" )
  determ.pred <- predict(bplr.imp, newdata=data, type="class")#as.vector( expect.prob %*% as.double( Y.levels ) )
  names( determ.pred ) <- 1:length( determ.pred )

  if(n.mis>0){
    random.pred <- Rmultnm(n.mis, expect.prob[mis,], 1:Y.nlevel)    
    random.pred <-  recode(random.pred, paste(1:Y.nlevel,"='",Y.levels,"'",sep="",collapse=";") )        
    names(random.pred) <- names(determ.pred[mis])
  #random.pred <- Y.levels[random.pred]
  }
  else{
    random.pred <- NULL
  }
#  resids <- as.numeric(Y)[!is.na(Y)] - as.numeric(determ.pred)[!is.na(Y)] 

  # return the result
  result <- new(c("mi.polr", "mi.method"),
              model = vector("list", 0),
              expected = NULL, 
              random = NULL)
  result@model$call        <- bplr.imp$call
  result@model$call$formula<- formula
  result@model$call$start  <- round(as.double(start),2)
  result@model$call$n.iter <- n.iter
  result@model$coefficients <- coef(bplr.imp)
  result@model$sigma       <- NULL  
  result@expected          <- determ.pred
  result@random            <- random.pred
  #result@residuals         <- resids 
  return(result)
  on.exit(rm(bplr.imp))
}

# ==============================================================================
# imputation function for categorical variable
# ==============================================================================
mi.categorical <- function( formula, data = NULL, n.iter = 100, 
                            MaxNWts = 1500, missing.index = NULL, ...  ) 
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
    if (!is.null(nm)){
      names(Y) <- nm
    }
  }
  X <- mf[,-1,drop=FALSE]
#  namesD <- if( is.null(data)){ 
#              NULL 
#            } 
#            else{ 
#              deparse(substitute(data))
#            }
  mis <- is.na(Y)
  n.mis <- if(is.null(missing.index)){
             sum(mis)
           } else{
             length(missing.index)
           }
  if(is.null(missing.index)& any(mis)){
    missing.index <- mis
  }
  
  if(is.null(data)){ 
    data <- mf 
  }
  
  # main program
  lm.cat.imp  <- multinom( formula = formula, data = data, maxit = n.iter, 
                            trace = FALSE , MaxNWts = MaxNWts, ...)
  
  deter.prob  <- predict( lm.cat.imp, newdata = data, type = "p" )
  y.cat       <- levels(factor(Y))
  y.ncat <- length(y.cat)

  if(length(y.cat)<=2){
    stop(message="number of category must be bigger than 2")
  }
  #determ.pred <- as.vector( deter.prob %*% y.cat )
  determ.pred <- predict(lm.cat.imp, newdata=data, type="class")
  names( determ.pred ) <- 1:length( determ.pred )
  if(n.mis>0){
    random.pred <-  Rmultnm( n.mis, deter.prob[missing.index,], 1:y.ncat)
    random.pred <-  recode(random.pred, paste(1:y.ncat,"='",y.cat,"'",sep="",collapse=";") )        
    names( random.pred ) <- names( determ.pred[missing.index] )
  } else{
    random.pred <- NULL
  }
  #resids <- as.numeric(Y)[!is.na(Y)] - as.numeric(determ.pred)[!is.na(Y)] 
  # return the result
  result <- new(c("mi.categorical", "mi.method"),
            model = vector("list", 0),
              expected = NULL, 
              random = NULL)
  result@model$call         <- lm.cat.imp$call
  result@model$call$formula <- formula
  result@model$call$maxit   <- n.iter
  result@model$call$MaxNWts <- MaxNWts
  result@model$coefficients <- coef( lm.cat.imp )
  result@model$sigma        <- NULL 
  result@expected <- determ.pred
  result@random   <- random.pred
  #result@residuals <- resids
  return(result)
  on.exit(rm(lm.cat.imp))
  on.exit(rm(Y))
  on.exit(rm(X))
}


## The random Multinomial function (for the categorical variable)
Rmultnm <- function( n, prob.mat, category  ) {
  y.imp <- NULL
  prob  <- prob.mat * NA
  if( is.null( dim( prob.mat ) ) ) {
    prob <- t( rmultinom( 1, 1, prob.mat ) )
  } 
  else {
    for( i in 1:n ){
      prob[i,] <- rmultinom( 1, 1, prob.mat[i, ] )
    }
  }
  y.imp <- as.double(prob %*% category)
  return( y.imp )
}

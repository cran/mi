# ==============================================================================
# imputation function using predictive mean matching
# ==============================================================================
mi.pmm <- function(formula, data = NULL, start = NULL, n.iter = 100, missing.index = NULL, ... )
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

#  X <- mf[,-1,drop=FALSE]
#  namesD <- if(is.null(data)) {
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
  if(!is.null(start)){
    n.iter <- 10
  }
  
  bglm.imp  <- bayesglm(formula, data = data, start = start, n.iter = n.iter )
  yhat <- predict(bglm.imp)#, newdata = data.frame( Y, X ) ) 
  result <- new(c("mi.pmm", "mi.method"),
              model = vector("list", 0),
              expected = numeric(0), 
              random = numeric(0))
  result@model$call <- bglm.imp$call
  result@model$coefficients <- bglm.imp$coefficients
  result@model$sigma <- sigma.hat( bglm.imp )
  result@model$startY <- Y[missing.index]
  result@expected <- yhat
  result@random   <- apply( as.array( yhat[missing.index] ), 1, 
                              mi.pmm.match, yhat=yhat[-missing.index], Y=Y[-missing.index] ) 
  result@residuals <- bglm.imp$residuals
  return(result)
}

mi.pmm.match<-function(z, yhat=yhat, Y=Y){
  d <- abs( yhat - z )
  m <- Y[ d == min( d )]
  if ( length( m ) > 1 ){
    m <- sample( m, 1 )
  }
  return( m )
}

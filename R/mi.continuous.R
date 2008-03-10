# ==============================================================================
# imputation function for continuous variable
# ==============================================================================

mi.continuous <- function ( formula, data = NULL, start = NULL, 
                            n.iter = 100, draw.from.beta = FALSE, ...  ) {
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
  namesD <- if(is.null(data)){ 
              NULL 
            }  
            else{ 
              deparse(substitute(data)) 
            }
  mis    <- is.na(Y)
  n.mis  <- sum(mis)
  if(is.null(data)){
    data <- mf
  }

  # main program
  if( !is.null( start ) ){ 
    #n.iter <- 1
    start[is.na(start)] <- 0
  }
  bglm.imp    <- bayesglm( formula = formula, data = data, family = gaussian, 
                            n.iter = n.iter, start = start, 
                            drop.unused.levels = FALSE, Warning=FALSE,... )
  determ.pred <- predict( bglm.imp, newdata = data, type = "response" )
  if(n.mis>0){
    if(draw.from.beta){
      sim.bglm.imp    <- sim(bglm.imp,1)
      random.pred     <- rnorm(n.mis, 
                              tcrossprod(cbind(X[mis,1,drop=FALSE]*0+1,X[mis,,drop=FALSE]),sim.bglm.imp$beta), 
                              sim.bglm.imp$sigma)
    }
    else{
      random.pred <- rnorm(n.mis, determ.pred[mis], sigma.hat(bglm.imp))
    }
    names(random.pred) <- names(determ.pred[mis])
  }
  else{
    random.pred <- numeric(0)
  }

  # return
  result <- new(c("mi.continuous", "mi.method"),
              model = vector("list", 0),
              expected = numeric(0), 
              random = numeric(0))
  result@model$call        <- bglm.imp$call
  result@model$call$formula<- as.formula(formula)
  result@model$call$start  <- round(as.double(start), 2)
  result@model$call$n.iter <- n.iter
  result@model$coefficients <- coef(bglm.imp)
  result@model$sigma       <- sigma.hat(bglm.imp)
  result@model$dispersion  <- bglm.imp$dispersion
  result@expected <- determ.pred
  result@random   <- random.pred
  return(result)
  on.exit(rm(Y))
  on.exit(rm(X))
  on.exit(rm(bglm.imp))
}

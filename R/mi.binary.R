# ==============================================================================
# imputation function for binary variable
# ==============================================================================
mi.binary <- function( formula, data = NULL, start = NULL, n.iter = 100,
                             draw.from.beta=TRUE, missing.index = NULL, ... ) {
  call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  Y <- model.response(mf, "any")
  if (length(dim(Y)) == 1) {
      nm <- rownames(Y)
      dim(Y) <- NULL
      if (!is.null(nm)) {
          names(Y) <- nm
      }
  }
#  X <- as.matrix(mf[, -1, drop = FALSE])
#  namesD <- if (is.null(data)) {
#                NULL
#            }
#            else {
#                deparse(substitute(data))
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
  
  
  y.levels <- if (is.numeric(Y)) {
                sort(unique(Y))
              } else if (is.logical(Y)) {
                  c(FALSE, TRUE)
              } else {
                levels(factor(Y))
              }
  
  Y <- recode(Y, paste("'", y.levels, "'=", c(0, 1), sep = "", collapse = "; "))

  if (!is.null(start)) {
      n.iter <- 50
      start[is.na(start)] <- 0
  }  
  if (is.null(data)) {
      data <- mf
      data[,names(mf)[1]] <- Y
  }
  else{
     data[,names(mf)[1]] <- Y
  }
  bglm.imp <- bayesglm(formula = formula, data = data, family = binomial(link = "logit"), 
      n.iter = n.iter, start = start, drop.unused.levels = FALSE, 
      Warning = FALSE, ...)
  determ.pred <- predict(bglm.imp, newdata = data, type = "response")

  if(n.mis>0){
    if (draw.from.beta) {
      ####get right design matrix#
        tt <- terms(bglm.imp)
        Terms <- delete.response(tt)
        mf <- model.frame(Terms, data=data[missing.index,,drop=FALSE],  xlev = bglm.imp$xlevels)
        mf <- Matrix(model.matrix(Terms, mf, contrasts.arg = bglm.imp$contrasts), sparse=TRUE)
      ############################
        sim.coef  <- sim(bglm.imp,1)@coef
        prob.pred <- invlogit(as.matrix(tcrossprod(mf, sim.coef)))
        random.temp <- rbinom(n.mis, 1, prob.pred)
    }
    else {
        random.temp <- rbinom(n.mis, 1, determ.pred[missing.index])
    }
    random.pred <- random.temp 
    random.pred <- replace(random.pred, random.temp == 0, y.levels[1])
    random.pred <- replace(random.pred, random.temp == 1, y.levels[2])
    names(random.pred) <- names(determ.pred[missing.index])                      
  }
  else{
    random.pred <- numeric(0)
  }

  if (is.logical(y.levels)) {                                      
    random.pred <- as.logical(random.pred)                         
  }                                                                 

  result <- new(c("mi.binary", "mi.method"),
              model = vector("list", 0), 
              expected = numeric(0), 
              random = numeric(0))
  result@model$call <- bglm.imp$call
  result@model$call$formula <- as.formula(formula)
  result@model$call$start <- round(as.double(start), 2)
  result@model$call$n.iter <- n.iter
  result@model$coefficients <- coef(bglm.imp)
  result@model$sigma <- sigma.hat(bglm.imp)
  result@model$startY <- Y[missing.index]
  result@expected <- determ.pred
  result@random <- random.pred
  return(result)
  on.exit(rm(bglm.imp))
}

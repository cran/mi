# ==============================================================================
# imputation function for fixed variable
# ==============================================================================
mi.fixed <- function( formula, data = NULL, missing.index = NULL,  ... ) {
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
#  namesD <- if( is.null( data ) ) { 
#              NULL 
#            } 
#            else { 
#              deparse( substitute( data ) )
#            }
#  nameY <- deparse( substitute( Y ) )

  mis <- is.na(Y)
  n.mis <- if(is.null(missing.index)){
             sum(mis)
           } else{
             length(missing.index)
           }
  if(is.null(missing.index)& any(mis)){
    missing.index <- mis
  }

  y.level <-  if ( is.numeric( Y ) ) {
                sort( unique ( Y ) )
              } else {
                levels( factor( Y ) )
              }
  # main program
  fixd.imp    <- y.level
  determ.pred <- rep( y.level, length( Y ) )
  names( determ.pred ) <- 1:length( determ.pred )
  random.pred <- determ.pred[missing.index]
  result <- new(c("mi.fixed", "mi.method"),
              model = vector("list", 0),
              expected = numeric(0), 
              random = numeric(0))
  result@model$call    <- ""
  result@expected <- determ.pred
  result@random   <- random.pred
  return(result)
  on.exit(rm(fixd.imp))
}

mi.copy <- function(Y, X, missing.index = NULL, ... ) {
#  fit <- lm(Y ~ unlist(X))
#  rho <- coef(fit)[2]
#  nameY <- deparse(substitute(Y))
#  nameX <- deparse(substitute(X))
  mis <- is.na(Y)
  n.mis <- if(is.null(missing.index)){
             sum(mis)
           } else{
             length(missing.index)
           }
  # main program
  #fixd.imp    <- nameX
  determ.pred <- unlist(X)
  names( determ.pred ) <- 1:length( determ.pred )
  random.pred <- determ.pred[missing.index] #* rho
  # return the result
  result <- new(c("mi.fixed", "mi.method"),
              model = vector("list", 0),
              expected = numeric(0), 
              random = numeric(0))
  result@expected <- determ.pred #* rho
  result@random   <- random.pred
  return( result )
#  on.exit(rm(fixd.imp))
}

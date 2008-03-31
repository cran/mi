lmer.mi <- function(formula, mi.object, rescale=FALSE, ...){
  call <- match.call()
  m <- m(mi.object)
  result <- vector("list", m)
  names(result) <- as.character(paste("Imputation", seq(m), sep = ""))
  mi.data <- mi.completed(mi.object)
  mi.data <- mi.postprocess(mi.data)
  if(rescale){
    tmp <- vector("list", m)
    for(i in 1:m){
      tmp[[i]] <- as.data.frame(apply(mi.data[[i]], 2, rescale, ...))
    }
    mi.data <- tmp
  }
  for (i in 1:m) {
    result[[i]] <- lmer(formula, data = mi.data[[i]], ...)
  }
  coef <- vector("list", m)
  se <- vector("list", m)
  for (j in 1:m) {
    coef[[j]] <- lapply(result, summary)[[j]]@coefs[,1]
    se[[j]] <- lapply(result, summary)[[j]]@coefs[,2]
  }
  W <- colSums(do.call(rbind, se)^2)/m
  Bhat <- colSums(do.call(rbind, coef))/m
  Bhat_rep <- t(matrix(rep(Bhat, m), length(Bhat), m))
  B <- colSums(do.call(rbind, coef) - Bhat_rep)^2/(m - 1)
  pooled <- list(coefficients = NULL, se = NULL)
  pooled$coefficients <- Bhat
  pooled$se <- sqrt(W + (1 + 1/m) * B)
  mi.mer.object <- new("mi.mer",
                      call = call, 
                      mi.pooled = pooled, 
                      mi.fit = result
                    )    
  return(mi.mer.object)
}



glmer.mi <- function(formula, mi.object, family=gaussian, rescale=FALSE, ...){
  call <- match.call()
  m <- m(mi.object)
  result <- vector("list", m)
  names(result) <- as.character(paste("Imputation", seq(m), sep = ""))
  mi.data <- mi.completed(mi.object)
  mi.data <- mi.postprocess(mi.data)
  if(rescale){
    tmp <- vector("list", m)
    for(i in 1:m){
      tmp[[i]] <- as.data.frame(apply(mi.data[[i]], 2, rescale, ...))
    }
    mi.data <- tmp
  }
  for (i in 1:m) {
    result[[i]] <- glmer(formula, data = mi.data[[i]], family=family,...)
  }
  coef <- vector("list", m)
  se <- vector("list", m)
  for (j in 1:m) {
    coef[[j]] <- lapply(result, summary)[[j]]@coefs[,1]
    se[[j]] <- lapply(result, summary)[[j]]@coefs[,2]
  }
  W <- colSums(do.call(rbind, se)^2)/m
  Bhat <- colSums(do.call(rbind, coef))/m
  Bhat_rep <- t(matrix(rep(Bhat, m), length(Bhat), m))
  B <- colSums(do.call(rbind, coef) - Bhat_rep)^2/(m - 1)
  pooled <- list(coefficients = NULL, se = NULL)
  pooled$coefficients <- Bhat
  pooled$se <- sqrt(W + (1 + 1/m) * B)
  mi.mer.object <- new("mi.mer",
                      call = call, 
                      mi.pooled = pooled, 
                      mi.fit = result
                    )    
  return(mi.mer.object)
}

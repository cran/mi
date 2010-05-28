lmer.mi <- function(formula, mi.object, rescale=FALSE, ...){
  call <- match.call()
  m <- m(mi.object)
  result <- vector("list", m)
  names(result) <- as.character(paste("Chain", seq(m), sep = ""))
  mi.data <- mi.completed(mi.object)
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
    coef[[j]] <- lapply(result, fixef)[[j]]
    se[[j]] <- lapply(result, se.coef)[[j]]$fixef
  }
  pooled <- mi.pooled(coef, se)
  mi.pooled.object <- new("mi.pooled",
                          call = call, 
                          mi.pooled = pooled,
                          mi.fit = result)
  return( mi.pooled.object )
}



glmer.mi <- function(formula, mi.object, family=gaussian, rescale=FALSE, ...){
  call <- match.call()
  m <- m(mi.object)
  result <- vector("list", m)
  names(result) <- as.character(paste("Chain", seq(m), sep = ""))
  mi.data <- mi.completed(mi.object)
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
    coef[[j]] <- lapply(result, fixef)[[j]]
    se[[j]] <- lapply(result, se.coef)[[j]]$fixef
  }
  pooled <- mi.pooled(coef, se)
  mi.pooled.object <- new("mi.pooled",
                          call = call, 
                          mi.pooled = pooled,
                          mi.fit = result)
  return( mi.pooled.object )
}

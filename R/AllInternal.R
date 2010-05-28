# ========================================================================
# Get missing index
# ========================================================================

.getMissingIndex <- function(x){
  x <- is.na(x)
  x <- x*(1:length(x))
  return(x[x>0])
}

#.getMissingIndex <- function(dataframe){
#  N <- dim(dataframe)[1]
#  idx <- lapply(dataframe, .MissingIndex)
#  return(idx)
#}


# ========================================================================
# factorize the categorical data
# ========================================================================

.update.data <- function(data, info){
  K <- dim(data)[2]
  for(i in 1:K){
    if(info$type[i]=="unordered-categorical"){
      data[,i] <-  as.factor(as.character(data[,i]))
    }
  }
  return(data)
}



# ========================================================================
# create missingness
# ========================================================================

.create.missing <- function(data, pct.mis=10){
  n <- nrow(data)
  J <- ncol(data)
  if(length(pct.mis)==1){
    n.mis <- rep((n*(pct.mis/100)), J)
  }
  else{
    if(length(pct.mis) < J) stop("The length of missing does not equal to the column of the data")
    n.mis <- n*(pct.mis/100)
  }
  for(i in 1:ncol(data)){
    if(n.mis[i]==0){
      data[,i] <- data[,i]
    }
    else{
      data[sample(1:n, n.mis[i], replace=FALSE),i] <- NA
    }
  }
  return(as.data.frame(data))
}
  



#.check.log.var <- function(x){
#  check1 <- min(x, na.rm=TRUE) < 0
#  if(check1) stop("log cannot take on negative values")
#  check2 <- min(x, na.rm=TRUE) == 0 
#  if(check2){
#    k <- round((min(x[x>0], na.rm=TRUE) + 0)/2,2)
#    return(k)
#  }
#  else return(0)
#}
#





vrb.typ <- NULL # to pass R CMD check


# ========================================================================
# Random draw from the obs. data
# ========================================================================

.randdraw <- function(data, n = 1){
  added.rows <- rep(0, n)
  varnames <- names(data)
  for(i in 1:ncol(data)){
    tmp <- sample(na.exclude(data[,i]), n, replace=TRUE)
    added.rows <- cbind.data.frame(added.rows, tmp)
  }
  added.rows <- added.rows[,-1]
  names(added.rows) <- varnames
  return(added.rows)
}


# ========================================================================
# Convert factor to numeric value
# ========================================================================


.factor2num <- function( a ) {
  if(is.factor( a ) ) {
    as.double( levels( a ) )[ as.double( a ) ]
  } 
  else {
    a
  }
}

# ========================================================================
# Convert character value
# ========================================================================

.factor2char <- function( a ) {
  levels( a )[ as.numeric( a ) ]
}


# ========================================================================
# Extracts the type (character) as vector
# ========================================================================

.type <-function(info){
  foo <- function(x){
    x$type
  }
  type <- sapply(info, FUN = foo)
  return(type)
}

# ========================================================================
# Extracts the level (vector) as list
# ========================================================================

.level <- function(info){
  foo <- function(x){
    x$level
  }
  level <- sapply(info, FUN = foo)
  return(level)
}

.n.level <- function(info){
  foo <- function(x){
    x$level
  }
  level <- sapply(info, FUN = foo)
  return(level)
}


# ========================================================================
# Extract imputation formula (character) as list
# ========================================================================

.imp.formula <-function(info){
  foo <- function(x){
    x$imp.formula
  }
  form <- sapply(info, FUN = foo)
  return(form)
}

# ========================================================================
# Extracts the imputation order vector(integer)
# ========================================================================

.imp.order <- function(info){
  foo <- function(x){
    x$imp.order
  }
  imp.order <- sapply(info, FUN = foo)
  return(imp.order)
}

# ========================================================================
# Extracts the include or not vector (logical)
# ========================================================================

.include <- function(info){
  foo <- function(x){
    x$include
  }
  include <- sapply(info, FUN = foo)
  return(include)
}

# ========================================================================
# Extracts the number of missing vector(integer)
# ========================================================================

.nmis <- function(info){
  foo <- function(x){
    x$nmis
  }
  nmis <- sapply(info, FUN=foo)
  return(nmis)
}

# ========================================================================
# Extracts the all missing or not (logical) as vector
# ========================================================================

.all.missing <- function(info){
  foo <- function(x){
    x$all.missing
  }
  all.missing <- sapply(info, FUN=foo)
  return(all.missing)
}

# ========================================================================
# dichotomize a variable
# ========================================================================


.dichot <- function(Y){
    y.levels <- if (is.double(Y)){
                  sort(unique(Y)) 
                }
                else{
                  levels(factor(Y))
                }
    Y <- replace(Y, Y==y.levels[1], 0)
    Y <- replace(Y, Y==y.levels[2], 1)
    return(Y)
}

# ========================================================================
# decomposing a unordered categorical variable into a binary
# ========================================================================


.cat2binary <- function(x){
    x.levels <- if (is.double(x)){
                  sort(unique(x))
                }
                else{
                  levels(factor(x))
                }
    n.levels <- length(x.levels)
    new.x <- array(NA, c(length(x), n.levels))
    for(i in 1:n.levels){
      new.x[,i] <- ifelse(x==x.levels[i], 1, 0)
    }
    return(new.x)
}


.catvarnames <- function(varname, level){
  new.varnames <-  paste(varname, "(", level, ")", sep="")
  return(new.varnames)
}

#=================================
# internal used for AveVar in mi
#==================================
.getmean <- function(v, type){
  if(type=="unordered-categorical"){
    new.v <- .cat2binary(v)
    return(apply(new.v, 2, mean))
  }
  else if(type=="ordered-categorical"){
    return(mean(as.numeric(factor(na.exclude(v)))))
  }
  else {
    return(mean(unclass(na.exclude(v))))
  }
}

.getsd <- function(v, type){
  if(type=="unordered-categorical"){
    new.v <- .cat2binary(v)
    return(apply(new.v, 2, sd))
  }
  else if(type=="ordered-categorical"){
    return(sd(as.numeric(factor(na.exclude(v)))))
  }
  else {
    return(sd(unclass(v), na.rm=TRUE))
  }
}
#  if(is.numeric(v)){
#    mean(unclass(v), na.rm=TRUE)
#  }
#  else if(is.ordered(v)){
#    mean(as.numeric(factor(v)), na.rm=TRUE)
#  }
#  else if(is.factor(v)){
#    new.v <- .cat2binary(v)
#    apply(new.v, 2, mean)
#  }
#  else if(is.character(v)){
#    new.v <- .cat2binary(v)
#    apply(new.v, 2, mean)
#  }
#  else{
#    new.v <- .cat2binary(v)
#    apply(new.v, 2, mean)
#  }
#}

#.foo2 <- function(v, type){
#  if(type=="unordered-categorical"){
#    new.v <- .cat2binary(v)
#    apply(new.v, 2, sd)
#  }
#  else if(type=="ordered-categorical"){
#    sd(as.numeric(factor(na.exclude(v))))
#  }
#  else {
#    sd(unclass(v), na.rm=TRUE)
#  }
#}




#
## ===============================
##  Internal use in update.mi.info
##=================================
#.change.formula.ordered <- function(x, varnames){
#  x <- gsub(paste("factor(", varnames, ")", sep=""), varnames, x, fix=TRUE)
#  return(x)
#}
#
#
#
#.change.formula.unordered <- function(x, varnames){
#  x <- gsub(varnames, paste("factor(", varnames, ")", sep=""), x)
#  return(x)
#}

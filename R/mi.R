#==============================================================================
# mi default function
#==============================================================================

mi.default <- function(data, info, n.imp = 3, n.iter = 30, R.hat = 1.1,
                  max.minutes = 20, rand.imp.method = "bootstrap", 
                  run.past.convergence = FALSE,
                  seed = NA, check.coef.convergence = FALSE, 
                  add.noise = noise.control())
{
  call <- match.call()
  
  org.data <- data
  
  # set random seed
  if(!is.na(seed)){
    set.seed(seed) 
  }    
  
  if(n.iter <= 5){ 
    stop(message="number of iterations must be more than 5")
  }
  
  # starting time
  ProcStart     <- proc.time()                  
  
  # pick a add.noise method
  if(is.logical(add.noise)){
    add.noise.method <- "other"
  }
  else{
    add.noise.method <- add.noise$method
  }
  
  # variable initialization
  time.out.flg  <- FALSE
  converged.flg <- FALSE
  coef.converged.flg <- FALSE
  max.iter.flg  <- FALSE
  Time.Elapsed  <- 0
  conv.check     <- NULL
  coef.mcmc <- NULL
  
  # this makes sure categorical data is factorized
  data <- .update.data(data, info)        

  # store level info in mi.info for unordered cat vars
  info <- .catVarLevelCheck(data, info)
  
  # initialize starting and end point of mcmc list
  s_start <- 1
  s_end <- n.iter
  
  # creating misc info for further usage
  missingVar.idx <- .nmis(info) > 0
  includeVar.idx <- .include(info)#& missingVar.idx
  unorderedCatVar.idx <- .type(info)=="unordered-categorical"
  includeCatVar.idx <- (includeVar.idx & missingVar.idx & unorderedCatVar.idx)
  ncol.mis <- sum(missingVar.idx)
  start.val.length <- sum(includeVar.idx & missingVar.idx)
  varNames <- names(info)[includeVar.idx & missingVar.idx]
  varNames <- varNames[order(.imp.order(info)[includeVar.idx & missingVar.idx])]
  # convergence array initialization
  
  aveVar <- .initializeConvCheckArray(data, info, n.iter, n.imp, 
    missingVar.idx, includeVar.idx, includeCatVar.idx, unorderedCatVar.idx, ncol.mis)
  
  #dim.mcmc <- dim(aveVar)
  data <- data[ , includeVar.idx, drop = FALSE]
  
  
  # mi list initialization
  mi.data <- .initializeMiList(data, info, start.val.length, varNames, n.imp, ncol.mis, missingVar.idx, rand.imp.method)
  start.val <- mi.data$start.val
  mi.object <- mi.data$mi.object
  coef.val <- mi.data$coef.val
  mi.data <- mi.data$mi.data
  

  cat("Beginning Multiple Imputation (", date(), "):\n")
  #===============================================================
  # begin of iteration loop
  for(s in s_start:s_end){
    cat("Iteration", s,"\n" )
    #===============================================================
    # begin of imputation loop
    for( i in 1:n.imp ){
      cat(" Chain", i,  ": " )
      #===============================================================
      # begin of variable loop
      #===============================================================
      for(jj in 1:length(varNames)){
        CurrentVar <- varNames[jj]        
        #===============================================================
        # begin of add noises
        if(add.noise.method=="reshuffling"){
          prob.add.noise <- add.noise$K/s
          prob.add.noise <- ifelse(prob.add.noise > 1, 1, prob.add.noise)
          q <- rbinom(1, 1, prob=prob.add.noise)           
          if(q){
            cat(paste(CurrentVar, "*", sep=""), " ")
          }
          else{
            cat(CurrentVar, "  ")
          }
        }
        else if(add.noise.method=="fading"){
          cat(paste(CurrentVar, "*", sep=""), " ")
        }
        else{
          cat(CurrentVar, "  ")
        }

        CurVarFlg <- (names(data) == CurrentVar)
        dat <- mi.data[[i]]
        missing.index <- info[[CurrentVar]]$missing.index
        #   Deside which model to use
        model.type <- as.character(type.models( info[[CurrentVar]]$type))

        if(add.noise.method=="reshuffling"){
          if(q){
            dat <- random.imp(data, method = rand.imp.method)
          }
        }
        if(add.noise.method=="fading"){
          n.aug <- trunc(nrow(dat)*(add.noise$pct.aug/100))
          dat <- rbind(dat, .randdraw(data, n=n.aug))
        }  
        #=end of add noise=====================================================

        #===============================================================        
        # Error Handling
        .Internal(seterrmessage(""))
        errormessage <- paste("\nError while imputing variable:", CurrentVar, ", model:",model.type,"\n")
        on.exit(cat(errormessage,geterrmessage()))
        on.exit(options(show.error.messages = TRUE),add = TRUE)
        options(show.error.messages = FALSE)
        #===============================================================
        
        mi.object[[i]][[CurrentVar]] <- with(data = dat, 
                                          do.call(model.type,
                                            args = c(list(formula = info[[CurrentVar]]$imp.formula, 
                                            data = dat,
                                            start = if(!is.null(start.val[[i]][[jj]])){
                                                      start.val[[i]][[jj]]
                                                    } else{
                                                    NULL
                                                    },
                                            missing.index = missing.index
                                            ),
                                          info[[CurrentVar]]$params$n.iter)
                                          ))
        #===============================================================
        # Error Handling
        on.exit(options(show.error.messages = TRUE))
        #===============================================================

        if(add.noise.method=="reshuffling"){        
          if(q){
            mi.object[[i]][[CurrentVar]]@random <- dat[missing.index,CurrentVar]
          }
        }
        mi.data[[i]][missing.index, CurrentVar] <- mi.object[[i]][[CurrentVar]]@random

        if(info[[CurrentVar]]$type=="unordered-categorical"){
          n.level <- length(info[[CurrentVar]]$level)
          if(!is.null(coef(mi.object[[i]][[CurrentVar]]))){
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],
              unlist(as.list(coef(mi.object[[i]][[CurrentVar]]))))
          }
          else{
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],0)
          }
        }
        else{
          if(!is.null(coef(mi.object[[i]][[CurrentVar]]))){
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],coef(mi.object[[i]][[CurrentVar]]))
          }
          else{
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],0)
          }
        }
        if(!is.null(coef(mi.object[[i]][[CurrentVar]]))){
          start.val[[i]][[jj]] <- coef(mi.object[[i]][[CurrentVar]])
        }
        else{
          start.val[[i]][[jj]] <- 0
        }
      } 
      #= end of variable loop ===============================================================


      cat("\n" )
      avevar.mean <- NULL
      avevar.sd <- NULL
      for (mm in 1:length(varNames)){
        avevar.mean <- c(avevar.mean, .getmean(mi.data[[i]][,varNames[mm]], type=info$type[varNames[mm]]))
        avevar.sd <- c(avevar.sd, .getsd(mi.data[[i]][,varNames[mm]], type=info$type[varNames[mm]]))
      }

      aveVar[s,i,] <- c(avevar.mean, avevar.sd)
    } 
    #= end of imputation loop ===============================================================
       
    # Check for convergence
    Time.Elapsed <- proc.time() - ProcStart
    if (s > 5 || ((((Time.Elapsed)/60)[3] > 0.5) && s > 2)){
      conv.check <- as.bugs.array(aveVar[1:s, , ])$summary[,"Rhat"]
      if(all(conv.check < R.hat)) { 
        converged.flg <- TRUE
        if(!run.past.convergence & !check.coef.convergence){ 
          break
        }
      }
      if(((Time.Elapsed)/60)[3] > max.minutes){ 
        time.out.flg <- TRUE
        break
      }
    }
    if(s==s_end){ 
      max.iter.flg <- TRUE 
    }
  } # iteration loop
  
  options(show.error.messages = TRUE)

  if(check.coef.convergence){
    coef.mcmc <- .checkCoefConvergence(coef.mcmc, coef.val, n.imp)
    if(all(as.bugs.array(coef.mcmc)$summary[,"Rhat"] < R.hat)){
      coef.converged.flg <- TRUE
    }
    cat(if(converged.flg & coef.converged.flg){
        converged.flg <- TRUE
        "mi converged (" 
      } 
      else if(time.out.flg){
        "Time out, mi did not converge ("
      } 
      else if(max.iter.flg){
        "Reached the maximum iteration, mi did not converge ("
      } 
      else{ 
        "Unknown termination ("
      }
      , date(), ")\n"
      ) 
  }
  else{
  # Print out reason for termination
  cat(if(converged.flg){
        "mi converged (" 
      } 
      else if(time.out.flg){
        "Time out, mi did not converge ("
      } 
      else if(max.iter.flg){
        "Reached the maximum iteration, mi did not converge ("
      } 
      else{ 
        "Unknown termination ("
      }
      , date(), ")\n"
      ) 
  }
  # impute correlated variables
  for( cor.idx in 1:length(info)) {
    if( !is.na(info[[cor.idx]]$collinear) 
         && info[[cor.idx]]$nmis > 0 
          && info[[cor.idx]]$include==FALSE ) {
      rho <- coef(lm(data[[names(info)[cor.idx]]] ~ data[[info[[cor.idx]]$determ.pred]]))[2]
      for ( ii in 1:n.imp ){
        mi.object[[ii]][[names(info)[[cor.idx]]]] <- do.call( mi.copy, 
                                                              args=list(
                                                                Y=data[[names(info)[cor.idx]]],
                                                                X=(mi.data[[ii]][info[[cor.idx]]$collinear])*rho))
      }
    }
  }
  
  aveVar <- aveVar[1:s,,]
  total.iters <- s
  
  if(!is.logical(add.noise)){
    add.noise.flg <- TRUE
  }
  else{
    add.noise.flg <- FALSE
    if(s > 30){
      n.thin = max(1, floor(s/30))
      keep <- c(rep(FALSE, n.thin-1), TRUE)
      aveVar <- aveVar[((1:s)[keep]),,]
    } else{
    aveVar <- aveVar[1:s,,]
    }
  }
  
  object <- new("mi", 
            call      = call,
            data      = org.data,
            m         = n.imp,
            mi.info   = info,
            imp       = mi.object,
            mcmc = aveVar,
            converged = converged.flg,
            coef.mcmc = coef.mcmc,
            coef.converged = coef.converged.flg,
            add.noise = add.noise.flg,
            total.iters = total.iters)
  return(object)
}

# ======================================================================
# mi for the data.frame method
# ======================================================================


setMethod("mi", signature(object = "data.frame"), 
        function (object, info, n.imp = 3, n.iter = 30, R.hat = 1.1,
                  max.minutes = 20, rand.imp.method = "bootstrap", 
                  run.past.convergence = FALSE,
                  seed = NA, check.coef.convergence = FALSE, 
                  add.noise = noise.control())
{ 

  if(missing(info)){
    info <- mi.info(object)
  }
  
  object <- mi.default(object, info, n.imp, n.iter, R.hat,
            max.minutes, rand.imp.method, run.past.convergence,
            seed, check.coef.convergence, add.noise)

  add.noise.flg <- object@add.noise

  if(add.noise.flg){
    if(add.noise$post.run.iter > 0){
      cat("Run", add.noise$post.run.iter, "more iterations to mitigate the influence of the noise...\n")
      object <- mi(object, run.past.convergence = TRUE, max.minutes = max.minutes, n.iter = add.noise$post.run.iter, R.hat = R.hat)
    }
    else{
      warning("Run additional iterations is suggested to mitigate the influence of the noise\n")
    }
  }
  return(object)
}
)

# ======================================================================
# mi for the mi.preprocessed method
# ======================================================================



setMethod("mi", signature(object = "mi.preprocessed"), 
        function (object, n.imp = 3, n.iter = 30, R.hat = 1.1,
                  max.minutes = 20, rand.imp.method = "bootstrap", 
                  run.past.convergence = FALSE,
                  seed = NA, check.coef.convergence = FALSE, 
                  add.noise = noise.control())
{ 

  info <- object@mi.info
  object <- object@data
  
  object <- mi.default(object, info, n.imp, n.iter, R.hat,
            max.minutes, rand.imp.method, run.past.convergence,
            seed, check.coef.convergence, add.noise)

  add.noise.flg <- object@add.noise
  if(add.noise.flg){
    if(add.noise$post.run.iter > 0){
      cat("Run", add.noise$post.run.iter, "more iterations to mitigate the influence of the noise...\n")
      object <- mi(object, run.past.convergence = TRUE, max.minutes = max.minutes, n.iter = add.noise$post.run.iter, R.hat = R.hat)
    }
    else{
      warning("Run additional iterations is suggested to mitigate the influence of the noise\n")
    }
  }
  return(object)
}
)


# ======================================================================
# mi for the mi method
# ======================================================================


setMethod("mi", signature(object = "mi"), 
        function (object, n.iter = 30, R.hat = 1.1,
                  max.minutes = 20, rand.imp.method = "bootstrap", 
                  run.past.convergence = FALSE,
                  seed = NA) 
{ 
  call <- match.call()                         # call
  if(!is.na(seed)){
    set.seed(seed) 
  }    # set random seed
  
  if(n.iter <= 5){ 
    stop(message="number of iterations must be more than 5")
  }
  ProcStart     <- proc.time()                  # starting time

  # variable initialization
  time.out.flg  <- FALSE
  converged.flg <- FALSE
  coef.converged.flg <- FALSE
  max.iter.flg  <- FALSE
  Time.Elapsed  <- 0
  conv.check     <- NULL
  coef.mcmc <- NULL

  # for mi object
  data  <- data.mi(object)
  info <- info.mi(object)

  # this makes sure categorical data is factorized
  data <- .update.data(data, info)
  
  
  if(is.null(object@coef.mcmc)){
    check.coef.convergence <- FALSE
  }
  else{
    check.coef.convergence <- TRUE
  }
    
  #ncol.mis <- sum(.nmis(info)>0)
  n.imp     <- m(object)
  prev.iter <- dim(object@mcmc)[1]
  prevIterNoThin <- object@total.iters
  
  # creating misc info for further usage
  missingVar.idx <- .nmis(info) > 0
  includeVar.idx <- .include(info) #& missingVar.idx
  unorderedCatVar.idx <- .type(info)=="unordered-categorical"
  includeCatVar.idx <- (includeVar.idx & missingVar.idx & unorderedCatVar.idx)
  ncol.mis <- sum(missingVar.idx)
  start.val.length <- sum(includeVar.idx & missingVar.idx)
  varNames <- names(info)[includeVar.idx & missingVar.idx]
  varNames <- varNames[order(.imp.order(info)[includeVar.idx & missingVar.idx])]

  # convergence array initialization
  if(object@add.noise){
    prev.iter <- 0
    prevIterNoThin <- 0
    object@coef.mcmc <- NULL
  }

  aveVar <- .initializeConvCheckArray(data, info, n.iter = n.iter + prev.iter, n.imp, 
      missingVar.idx, includeVar.idx, includeCatVar.idx, unorderedCatVar.idx, ncol.mis)  
  data <- data[ , includeVar.idx, drop = FALSE]
    
  if(prev.iter > 0){
    aveVar[1:prev.iter,,] <- object@mcmc
  }
  s_start <- 1 + prev.iter
  s_end <- n.iter + prev.iter
  
  
  # mi list initialization
  mi.data <- .initializeMiList(data, info, start.val.length, varNames, n.imp, ncol.mis, missingVar.idx, rand.imp.method)
  start.val <- mi.data$start.val
  mi.object <- mi.data$mi.object
  coef.val <- mi.data$coef.val
  mi.data <- mi.data$mi.data

  
  cat( "Beginning Multiple Imputation (", date(), "):\n" )
  # iteration loop
  for ( s in s_start:s_end ) {
    cat( "Iteration", (s + prevIterNoThin),"\n" )
    # imputation loop
    for ( i in 1:n.imp ){
      cat( " Chain", i,  ": " )
      # variable loop
      for( jj in 1:length(varNames) ) {
        CurrentVar <- varNames[jj]
        cat(CurrentVar, "  ")      
        CurVarFlg <- ( names ( data ) == CurrentVar )

        #missing.index <- info$missing.index[[CurrentVar]]
        missing.index <- info[[CurrentVar]]$missing.index
        #########which model to use#########################################
        model.type <- as.character(type.models( info[[CurrentVar]]$type))
        ####################################################################
        # Error Handling
        .Internal(seterrmessage(""))
        errormessage <- paste("\nError while imputing variable:", CurrentVar, ", model:",model.type,"\n")
        on.exit(cat(errormessage,geterrmessage()))
        on.exit(options(show.error.messages = TRUE),add = TRUE)
        options(show.error.messages = FALSE)
        # Error Handling
        mi.object[[i]][[CurrentVar]] <- with(data = mi.data[[i]], 
                                          do.call(model.type,
                                            args = c(list(formula = info[[CurrentVar]]$imp.formula, 
                                            data = mi.data[[i]],
                                            start = if(!is.null(start.val[[i]][[jj]])){
                                                      start.val[[i]][[jj]]
                                                    } else{
                                                    NULL
                                                    },
                                            missing.index = missing.index
                                            ),
                                          info[[CurrentVar]]$params$n.iter)
                                          ))
        # Error Handling
        on.exit(options(show.error.messages = TRUE))

        # Error Handling        
        mi.data[[i]][missing.index, CurrentVar] <- mi.object[[i]][[CurrentVar]]@random
        if(info[[CurrentVar]]$type=="unordered-categorical"){
          n.level <- length(info[[CurrentVar]]$level)
          if(!is.null(coef(mi.object[[i]][[CurrentVar]]))){
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],
              unlist(as.list(coef(mi.object[[i]][[CurrentVar]]))))
          }        
          else{
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],0)
          }
        }
        else{
          if(!is.null(coef(mi.object[[i]][[CurrentVar]]))){
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],coef(mi.object[[i]][[CurrentVar]]))
          }
          else{
            coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],0)          
          }
        }
        if(!is.null(coef(mi.object[[i]][[CurrentVar]]))){
          start.val[[i]][[jj]] <- coef(mi.object[[i]][[CurrentVar]])
        }
        else{
          start.val[[i]][[jj]] <- 0
        }
      } ## variable loop 
      cat("\n" )      
      avevar.mean <- NULL
      avevar.sd <- NULL
      for (mm in 1:length(varNames)){
        avevar.mean <- c(avevar.mean, .getmean(mi.data[[i]][,varNames[mm]], type=info$type[varNames[mm]]))
        avevar.sd <- c(avevar.sd, .getsd(mi.data[[i]][,varNames[mm]], type=info$type[varNames[mm]]))
      }

      aveVar[s,i,] <- c(avevar.mean, avevar.sd)
     
    
    } # imputation loop
    # Check for convergence
    Time.Elapsed <- proc.time() - ProcStart
    if (s > 5 || ((((Time.Elapsed)/60)[3] > 0.5) && s > 2)){
      conv.check <- as.bugs.array(aveVar[1:s, , ])$summary[,"Rhat"]
      if(all(conv.check < R.hat)) { 
        converged.flg <- TRUE
        if(!run.past.convergence & !check.coef.convergence){ 
          break
        }
      }
      if(((Time.Elapsed)/60)[3] > max.minutes){ 
        time.out.flg <- TRUE
        break
      }
    }
    if(s==s_end){ 
      max.iter.flg <- TRUE 
    }
  } # iteration loop
 
 
  if(check.coef.convergence){
    coef.mcmc <- object@coef.mcmc
    coef.mcmc <- .checkCoefConvergence(coef.mcmc, coef.val, n.imp)
    if(all(as.bugs.array(coef.mcmc)$summary[,"Rhat"] < R.hat)){
      coef.converged.flg <- TRUE
    }
   # Print out reason for termination
    cat(if(converged.flg & coef.converged.flg){
        converged.flg <- TRUE
        "mi converged (" 
      } 
      else if(time.out.flg ){
        "Time out, mi did not converge ("
      } 
      else if( max.iter.flg ){
        "Reached the maximum iteration, mi did not converge ("
      } 
      else{ 
        "Unknown termination ("
      }
      , date(), ")\n")
  }
  else{
  # Print out reason for termination
  cat(if(converged.flg){
        "mi converged (" 
      } 
      else if(time.out.flg ){
        "Time out, mi did not converge ("
      } 
      else if( max.iter.flg ){
        "Reached the maximum iteration, mi did not converge ("
      } 
      else{ 
        "Unknown termination ("
      }
      , date(), ")\n")
  }


  # impute collinear variables
  for(cor.idx in 1:length(info)) {
    if(!is.na(info[[cor.idx]]$collinear) 
         && info[[cor.idx]]$nmis > 0 
          && !info[[cor.idx]]$include ) {
      rho <- coef(lm(data[[names(info)[cor.idx]]] ~ data[[info[[cor.idx]]$determ.pred]]))[2]
      for ( ii in 1:n.imp ){
        mi.object[[ii]][[names(info)[[cor.idx]]]] <- do.call( mi.copy, 
                                                              args=list(
                                                                Y=data[[names(info)[cor.idx]]],
                                                                X=mi.data[[ii]][info[[cor.idx]]$determ.pred]))
      }
    }
  }

  aveVar <- aveVar[1:s,,]
  total.iters <- s + prevIterNoThin
  
  if(s > 30){
    n.thin = max(1, floor(s/30))
    keep <- c(rep(FALSE, n.thin-1), TRUE)
    aveVar <- aveVar[((1:s)[keep]),,]
  } else{
    aveVar <- aveVar[1:s,,]
  }

  
  object <- new("mi", 
            call      = call,
            data      = data.mi(object),
            m         = n.imp,
            mi.info   = info.mi(object),
            imp       = mi.object,
            mcmc      = aveVar,
            converged = converged.flg,
            coef.mcmc = coef.mcmc,
            coef.converged = coef.converged.flg,
            add.noise = FALSE,
            total.iters = total.iters)
  return(object)
}
)





##The simple imputation function
impute <- function (a, a.impute) { 
  out <- ifelse (is.na(a), a.impute, a)
  return (out) 
}


setMethod("is.mi", signature( object = "mi" ),
  function (object){ 
    return(inherits ( object, "mi" )) 
  }
)

setMethod("call.mi", signature( object = "mi" ),
  function (object) { 
    return(object@call) 
  }
)

setMethod("data.mi", signature( object = "mi" ),
  function (object) { 
    return( object@data ) 
  }
)

setMethod("converged", signature( object = "mi" ),
  function (object, check = c("data", "coefs") ) { 
    if(check=="coefs"){
      out <- object@coef.converged
    }
    if(check=="data"){
      out <- object@converged
    }
    return(out)     
  }
)

setMethod("m", signature( object = "mi" ),
  function (object) { 
    return( object@m ) 
  }
)

setMethod("bugs.mi", signature( object = "mi" ),
  function (object, check = c("data", "coefs")){
    check <- match.arg(check)
    if(check=="coefs"){
      out <- as.bugs.array(object@coef.mcmc)
    }
    if(check=="data"){
      out <- as.bugs.array(object@mcmc)
    }
    return(out) 
  }
)

setMethod("info.mi", signature(object = "mi" ),
   function(object){
    return(object@mi.info ) 
  }
)

setMethod("imp", signature( object = "mi" ),
  function(object, m=1){
      return(object@imp[[m]])
  }
)

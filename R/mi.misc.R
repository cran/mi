## many internal functions are stored in mi.misc.R


noise.control <- function(method=c("reshuffling", "fading"), pct.aug=10, K = 1, post.run.iter=20){
  method <- match.arg(method)
  if(method=="reshuffling"){
    return(list(method=method, K=K, post.run.iter=post.run.iter))
  }
  if(method=="fading"){
    return(list(method=method, pct.aug=pct.aug, post.run.iter=post.run.iter))  
  }
}


.preprocessMiInfo <- function(data, info){
  proc.tmp <- mi.preprocess(data, info)
  data <- as.data.frame(proc.tmp$data)
  info.org <- info
  info <- tmp <- mi.info(data)
  for(i in 1:length(info.org)){
    info[[i]] <- info.org[[i]]    
    info[[i]]$missing.index <- tmp[[i]]$missing.index
  }
  for(i in 1:ncol(data)){
    info[[i]]$type <- proc.tmp$type[[i]]
  }
  info <- mi.info.formula.default(info)
  info$imp.formula[1:length(info.org)] <- info.org$imp.formula
  return(list(data=data, info=info, info.org=info.org))
}


.catVarLevelCheck <- function(data, info){
  for(i in 1:ncol(data)){
    if (info$type[[i]]=="unordered-categorical"){
      if(is.null(info$level[[i]])){
        info$level[[i]] <- sort(na.exclude(unique(data[,i])))
      }
    }
  }
  return(info)
}



.initializeConvCheckArray <- function(data, info, n.iter, n.imp, 
  missingVar.idx, includeVar.idx, includeCatVar.idx, unorderedCatVar.idx, ncol.mis)
{

  if(all(includeCatVar.idx==0)){
    tot.nlevel <- 0
    tot.n.unord.cat.var <- 0
  }
  else{
    tot.nlevel <- sum(sapply(info$level[includeCatVar.idx], length))
    tot.n.unord.cat.var <- sum(includeCatVar.idx)
  }
  
  if(sum(includeVar.idx & missingVar.idx)==1){
    n.col.sims.array <- 1 + tot.nlevel - tot.n.unord.cat.var
  }
  else{
    n.col.sims.array <- dim(data[, includeVar.idx & missingVar.idx])[2] + tot.nlevel - tot.n.unord.cat.var
  }
  
  aveVar  <- array(NA, c(n.iter, n.imp, n.col.sims.array*2))
  varNames <- as.list(info$name)

  if(!all(includeCatVar.idx==0)){
    includeCatVarNames <- info$name[includeCatVar.idx]
    catVar.idx <- charmatch(includeCatVarNames, info$name)
    for(i in catVar.idx){
      varNames[[i]] <- .catvarnames(varNames[[i]], info$level[[i]])
    }
  }
  varNames <- varNames[includeVar.idx & missingVar.idx]
  sim.varnames <- unlist(varNames)
  #varNames <- names(info)[includeVar.idx & missingVar.idx]
  #varNames <- varNames[order(.imp.order( info )[includeVar.idx & missingVar.idx])]
  dimnames( aveVar ) <- list(NULL, NULL, 
                             c(paste("mean(", sim.varnames,")",sep=""), 
                               paste("sd(", sim.varnames, ")", sep="")))
  convArray <- aveVar#list(aveVar=aveVar)#, varNames=varNames)
  return(convArray)
}


.initializeMiList <- function(data, info, start.val.length, varNames, n.imp, ncol.mis, missingVar.idx, rand.imp.method)
{
  mi.data       <- vector("list", n.imp)
  start.val     <- vector("list", n.imp)
  mi.object     <- vector("list", n.imp)
  mi.object.name <- varNames#names(info)[missingVar.idx]
  for (j in 1:n.imp){ 
    mi.data[[j]]  <-  random.imp(data, method = rand.imp.method)
    start.val[[j]]<- vector( "list", start.val.length)
    mi.object[[j]]<- vector( "list", ncol.mis)
    names(mi.object[[j]]) <- mi.object.name
  }

  coef.val <- vector("list", ncol.mis)
  names(coef.val) <- mi.object.name

  for (jjj in 1:ncol.mis){
    coef.val[[jjj]] <- vector("list", n.imp)
  }
  names(mi.object)<- paste( "Chain", 1:n.imp, sep="" )
  miList <- list(mi.data=mi.data, start.val=start.val, mi.object=mi.object, coef.val=coef.val)
  return(miList)
}

#.checkConvergence <- function(s, s_end, Time.Elapsed, aveVar, R.hat, max.minutes, 
#    converged.flg, time.out.flg, max.iter.flg, run.past.convergence){
#  if (s > 5 || ((((Time.Elapsed)/60)[3] > 0.5) && s > 2)){
#    con.check <- as.bugs.array(aveVar[1:s, , ])
#    if(all(con.check$summary[,8] < R.hat)) { 
#      converged.flg <- TRUE
#      if(!run.past.convergence){ 
#        break
#      }
#    }
#    if(((Time.Elapsed)/60)[3] > max.minutes){ 
#      time.out.flg <- TRUE
#      break
#    }
#  }
#  if(s==s_end){ 
#    max.iter.flg <- TRUE 
#  }
#  conv.check$sims.matrix <- NULL
#  convCheck <- list(conv.check=conv.check, converged.flg=converged.flg, 
#    time.out.flg=time.out.flg, max.iter.flg=max.iter.flg)
#  return(convCheck)
#}

.checkCoefConvergence <- function(coef.mcmc, coef.val, n.imp){
  mcmc <- if(is.null(coef.mcmc)){
        .strict.check(coef.val,dim(coef.val[[1]][[1]])[1],n.imp)
      }
      else{
        abind(coef.mcmc,.strict.check(coef.val,dim(coef.val[[1]][[1]])[1],n.imp),along=1)
      }
  return(mcmc)
}



.strict.check <- function(coefficient, n.iter, n.imp){
  res <- array(NA,c(n.iter,n.imp,0))
#  j <- length(coefficient)
#  while(j > 1){
#    if(is.null(coefficient[[j]][[1]])){
#      coefficient[[j]] <- NULL
#      j <- length(coefficient)
#      #print(j)
#    }
#    else{
#      j <- length(coefficient) - 1
#    }
#  }
  for(i in 1:length(coefficient)){
    for(j in 1:NCOL(coefficient[[i]][[1]])){      
     res <- abind(res,
      matrix(unlist(lapply(coefficient[[i]], "[", , j)),,n.imp),
      along=3)
    }
  }
  return(res)
}

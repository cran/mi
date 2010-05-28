# ==============================================================================
# Creates information matrix
# ==============================================================================
mi.info <- function(data, threshold = 0.99999){
  if(is.matrix(data)) { 
    data <- data.frame(data) 
  }
  info <- vector("list", dim(data)[2])
  names(info) <- dimnames(data)[[2]]
  #data.original.name <- deparse(substitute(data))
  collinear <- mi.check.correlation(data, threshold)
#  foo <- function(lst){
#    lst[-1]
#  }
#  lapply(collinear, FUN=foo)

  ord <- 1
  for( i in 1:dim( data )[2] ) {
    info[[i]] <- vector( "list", 15)
    names( info[[i]] ) <- c( "name","imp.order", "nmis", "type", "var.class",
                            "level", 
                            "include", "is.ID", "all.missing",
                            "collinear", "determ.pred", "imp.formula", 
                            "missing.index",
                            "params", "other" )
                            
    info[[i]]$name <- dimnames(data)[[2]][i]
    # nmis
    info[[i]]$nmis <- sum(is.na(data[,i]))
    
    # type
    info[[i]]$type <- typecast(data[,i])
    
    # missing.index 
    info[[i]]$missing.index <- as.numeric(.getMissingIndex(data[,i]))
 
    info[[i]]$var.class <- class(data[,i])
    # level
    if(info[[i]]$var.class[1] == "character" ) {
      lev <- sort(unique(as.character(data[,i]))[!is.na(unique(as.character(data[,i])))])
      #lev <- lev[order(lev)]
      if(length(lev) == 2 ) {
        info[[i]]$level <- c(0, 1)
      } else{
        info[[i]]$level <- 1:length(lev)
      }
      names(info[[i]]$level) <- lev
    } else if( info[[i]]$var.class[1] == "factor" ) {
      lev <- levels( data[ ,i] )[ !is.na( levels( data[ ,i] ) )]
      lev <- lev[!( lev %in% c( "NA", "RF", "DK" ) )]
      if( length( lev ) == 2 ) {
          info[[i]]$level <- c( 0, 1 )
      } else {
        info[[i]]$level <- 1:length( lev )
      }
      names( info[[i]]$level ) <- lev
    }

    # is.ID then exclude
    info[[i]]$is.ID <- if(length(unique(data[!is.na(data[,i]),i]))==length(data[,i]) &&
                        all(data[,i]==sort(data[,i]))){ 
                          TRUE
                        } else { 
                          FALSE
                        }   
    info[[i]]$include <- if( info[[i]]$is.ID ){ 
                            FALSE 
                          } else { 
                            TRUE 
                          }

    # all missing then exclude
    info[[i]]$all.missing <- if(sum(is.na(data[ ,i])) == dim(data)[1]){
                               TRUE
                             } else { 
                               FALSE
                             }    
    if(info[[i]]$include){
      info[[i]]$include <- if(info[[i]]$all.missing) { 
                             FALSE
                           } else { 
                             TRUE
                           }
    }
    
    # order
    if(info[[i]]$include && info[[i]]$nmis>0){
      info[[i]]$imp.order <- ord
      ord <- ord + 1
    } else{
      info[[i]]$imp.order <- NA
    }
    
    # collinear 
    info[[i]]$collinear <- collinear[[i]]
    
    # params
    formal.args <- formals(as.character(type.models(info[[i]]$type)))
    info[[i]]$params <- formal.args[!names( formal.args ) %in% c( "formula", "data", "start", "..." )]    
  }
 


  # all missing
  allmis <- .all.missing(info)
  if(any(allmis)){
    cat("\avariable(s)", paste( names(info)[allmis],collapse=", ",sep=""),
         "has(have) no observed value, and will be omitted.\n\n" )
  }
  # collinear then exclude  
  data.collinear <- mi.correlated.list(data)
  if(length(data.collinear) > 0) {
    for(c.idx in 1:length(data.collinear)) {
      c.nm <- data.collinear[[c.idx]][-1]
      for( nm.idx in 1:length(c.nm)){
        info[[c.nm[nm.idx]]]$include     <- FALSE
        info[[c.nm[nm.idx]]]$imp.order   <- NA
        info[[c.nm[nm.idx]]]$determ.pred <- paste(data.collinear[[c.idx]][1] )
      }
    }
  }
 
  ord.temp <- .imp.order(info)
  re.ord   <- ord.temp[ !is.na( ord.temp ) ]
  ord.temp[!is.na( ord.temp )] <- rank( re.ord, ties.method = "first" )
  for( ord.index in 1:length( ord.temp ) ) {
    info[[ord.index]]$imp.order <- ord.temp[ord.index]
  }
  # default formmula
  info <- mi.info.formula.default(info)
  if( length( mi.correlated.list( data ) ) > 0 ) {
    cat( "\afollowing variables are collinear\n" )
    print( data.collinear )
  }
  # Rank checks
#  dtch<-data[complete.cases(data[,include(info)]),include(info)]
#  if(dim(dtch)[2] > qr(dtch)$rank)){  #rankMat(dtch)){
#    warning(message=paste("after accounting for simple correlation",
#                          "there seems to be some variable that are",
#                          "linear combination of others.\n",
#                          "This may cause problem in your imputation.",
#                          "If you are aware of this variable, please specify them later."))
#  }
  
  class( info ) <- c( "mi.info", "list" )
  return( info )
}

mi.info.formula.default <-function(info){
  varnames <- names(info)
#  for(i in 1:length(varnames)){
#    type <- info[[i]]$type
#    if(type=="ordered-categorical"){
#      varnames[i] <- paste("ordered(",varnames[i],")",sep="")
#    }
#    if(info[[i]]$type=="unordered-categorical"){
#      varnames[i] <- paste("factor(",varnames[i],")",sep="")
#    }
#    else{
#      varnames[i] <- varnames[i]
#    }
#  }
  for(i in 1:length(info)){
    # default formula
    inc <- .include(info)
    inc[i] <- FALSE
    response <- varnames[i]
    predvarn <- varnames[inc]
    form <- paste(response,"~",paste(predvarn,collapse=" + "))
    info[[i]]$imp.formula <- form
  }
  return(info)
}

mi.info.params.default <-function( info ){
# params
  for(i in 1:length(info)){
    if(info[[i]]$include){
      formal.args <- formals(as.character(type.models(info[[i]]$type)))
      info[[i]]$params <-  formal.args[!names( formal.args )%in%c("formula","data","start","...")]  
    }
  }
}

# ========================================================================
# Default formula for the type
# ========================================================================
type.default.formula <- function(response.name, predictor.name, type) {
#  if (type=="ordered-categorical"){
#    form <- paste( paste( "ordered(", response.name, ") ~",sep=""),paste(predictor.name,collapse=" + "))
#  } 
#  else if (type=="unordered-categorical"){
#    form <- paste( paste( "factor(", response.name, ") ~",sep=""),paste(predictor.name,collapse=" + "))
#  } 
  if (type=="fixed"){
    form <- paste( response.name, " ~",response.name)
  } 
  else{
  form <- paste( response.name,"~",paste(predictor.name,collapse=" + "))
  }
  return(form)
}



# ========================================================================
# Fix information matrix
# ========================================================================
mi.info.fix <- function( info ) {
  cat("----------------------------------- \n")
  cat("checking mi.info\n")
  cat("----------------------------------- \n")
  res0<-menu(c("interactive variable specification"
              ,"edit mi.info directly (for advanced user only)")
              ,title="How would you like to proceed?")
  if( res0 == 1 ) {
    # interactive specification
    #--------------------
    # include variable
    #--------------------
    inc.flg<-TRUE
    while(inc.flg){
      # change variables to include
      res.ord<-menu(c("Yes","No","look at current setting"),
                    title=.make.title("change variable to include?" ))
      # change
      if(res.ord == 1){
        inc.change <- data.frame( include = .include( info ) )
        inc.change <- fix( inc.change )
        # when something is wrong
        if( any( is.na( inc.change[,1] ) ) ) {
          # loop until things are OK
          while( any( is.na(inc.change[,1] ) ) ) {
            inc.change<-fix(inc.change)
          }
        }
        # update the data
        for( inc.index in 1:length( inc.change[,1] ) ) {
          info[[inc.index]]$include <- inc.change[inc.index,]
          if( !inc.change[inc.index,] ) {
            info[[inc.index]]$imp.order <- NA
          }
        }
        # reorganize the order
        ord.temp <- .imp.order(info)
        re.ord <- ord.temp[!is.na(ord.temp)]
        ord.temp[!is.na(ord.temp)]<-rank(re.ord, ties.method ="first")
        for(ord.index in 1:length(ord.temp)){
          info[[ord.index]]$imp.ord <- ord.temp[ord.index]
        }
      } else if( res.ord == 2 ) {
        # no change
        inc.flg <- FALSE
      } else if( res.ord == 3 ) {
        # view setting
        print( info )
      }
    }
    #--------------------
    # imputation order
    #--------------------
    ord.flg <- TRUE
    while(ord.flg){
      res.ord<-menu(c("Yes","No","look at current setting")
                    , title=.make.title("change imputation order?" ))
      #change order
      if(res.ord == 1){
        ord.change.temp <- as.data.frame(.imp.order(info))
        ord.change <- data.frame(imp.ord=ord.change.temp[!is.na(ord.change.temp)])
        dimnames(ord.change)[[1]]<-dimnames(ord.change.temp)[[1]][!is.na(ord.change.temp)]
        ord.change <- fix(ord.change)
        if(any(is.na(ord.change[,1]))){
          while(any(is.na(ord.change[,1]))){
            ord.change<-fix(ord.change)
          }
        }
        ord.change.temp[!is.na(ord.change.temp),1]<-rank(ord.change[,1], ties.method ="first")
        for(ord.index in 1:length(ord.change.temp[,1])){
          info[[ord.index]]$imp.order <- ord.change.temp[ord.index,]
        }
      } 
      else if(res.ord == 2){
        #no change
        ord.flg <- FALSE
      } 
      else if(res.ord == 3){
        #view setting
        print(info)
      }
    }
    #--------------------
    # change variable
    #--------------------
    var.flg<-TRUE
    while( var.flg ) {
      res.var  <-menu( c( "Yes","No","look at current setting" ), 
                        title = .make.title("change variable specification?" ) )
      #change var
      if( res.var == 1 ) {
        each.var.flg<-TRUE
        while( each.var.flg ) {
          cat(.make.title("enter name of variable to fix"))
          cat("\n(type 'list' to see the list of variable names, type 'end' to exit)\n")
          Var.to.fix <- scan(what = character(), sep = "\n", strip.white = TRUE, nlines=1, quiet=TRUE)
          if( Var.to.fix=="list"){
            cat("----------------------------------- \n")
            print(names(info))
            cat("----------------------------------- \n\n")
          }
          else if( Var.to.fix=="end"){
            each.var.flg <- FALSE
          }
          else if(length(which( names(info)==Var.to.fix) ) > 0 ){
            print.variable.setting(info[[Var.to.fix]])
            res.var2<-menu(c("Yes","No"),title="would you like to change this variable")
            if( res.var == 1 ) {
            #change
              change.var.flg<-TRUE
              while( change.var.flg ) {
                res.var3<-menu(c("type","formula (always update when you change type)","parameter","levels","view setting","different variable")
                                  ,title=.make.title("What would you like to change?" ))
                if(res.var3 ==1){
                  #type
                  res.var3.type <- menu(mi.types(),title="choose a type to change to")
                  print(res.var3.type)
                  change.list <- list(Var.to.fix=mi.types()[res.var3.type])
                  names(change.list) <- Var.to.fix
                  info <- update(info, target="type", change.list)
                  
                  #type.default.formula(,info[[Var.to.fix]]$type)
                }
                else if(res.var3 ==2){
                  #formula
                  info <- mi.fix.formula (info, Var.to.fix )
                }
                else if(res.var3 ==3){
                  #param
                  info <- mi.fix.params (info, Var.to.fix )
                }
                else if(res.var3 ==4){
                  #level
                  info[[Var.to.fix]]$level
                }
                else if(res.var3 ==5){
                  #view setting
                  print.variable.setting(info[[Var.to.fix]])
                }
                else if(res.var3 ==6){
                  change.var.flg <- FALSE
                }
              }
            }
            else if( res.var == 2 ) {
            #no change
            }
          }
        }
      }
      #no change
      else if( res.var == 2 ) {
        var.flg <- FALSE
      }
      #
      else if( res.var == 3 ) {
        print(info)
      }
    }
    
  }
  else if ( res0 == 2 ){
    # edit mi.info directly
    info <- fix( info )
  }
  return( info )
}

.make.title <- function ( title ){
  cat.title <- "----------------------------------- \n"
  cat.title <- paste(cat.title, title)
  cat.title <- paste(cat.title, "\n-----------------------------------")
  return(cat.title)
}

# ========================================================================
# print variable setting
# ========================================================================
print.variable.setting <- function(info.variable){
      cat("\nName:", info.variable$name,"\n\n")
      cat("Type:", info.variable$type)
      cat(", Class:", info.variable$var.class,"\n\n")
      cat("Levels: \n")
      print(info.variable$level)
      cat("\nFormula: \n")
      if(length(info.variable$imp.formula)==1){
        print(as.formula(info.variable$imp.formula))
      } else{
        for(i in 1:length(info.variable$imp.formula)){
          print(as.formula(info.variable$imp.formula[[i]]))
          cat("\n")
        }
      }
      cat("\nParameters:\n")
      print(paste(names(info.variable$params),"=",info.variable$params,collapse=", "))
      cat("\n")
}

# ========================================================================
# fix mi.info
# ========================================================================

mi.fix.formula <- function( info, name ) {
  formula.to.edit <- info[[ name ]]$imp.formula
  fixed.formula   <- fix( formula.to.edit )
  info[[ name ]]$imp.formula <- fixed.formula
  return( info )
}
# ========================================================================
# fix mi.info.params
# ========================================================================

mi.fix.params <- function( info, name ) {
  params.to.edit <- info[[ name ]]$params
  fixed.params   <- fix( params.to.edit )
  info[[ name ]]$params <- fixed.params
  return( info )
}

# ========================================================================
# check correlation
# ========================================================================

mi.check.correlation <- function (data, threshold = 0.99999 ){
  options(warn = -1)
  data <- as.data.frame(apply(data, 2, as.numeric))
  cor.data <- cor(data, use = "pairwise.complete.obs")
  diag( cor.data ) <- 1
  index  <- abs( cor.data - diag( dim( cor.data )[1] ) ) >= threshold 
  result <- vector( "list", dim( index )[1] )
  for( i in 1:dim(index)[1] ){
    if( length( names( which( index[i,]==1 ) ) ) > 0 ) {
     # result[[i]]<-c(names(data)[i],names(which(index[i,]==1)))
      result[[i]] <- c( names( which( index[i,]==1 ) ) )
      result[[i]] <- result[[i]][ order( result[[i]] )]
    }
    else{
      result[[i]] <- NA
    }
  }
#  unique.cor<-unique(result)
#  unique.cor<- unique.cor[!sapply(unique.cor,is.null)] 
#  return(unique.cor)
  on.exit()
  options(warn = 0)
  return(result)
}

# ========================================================================
# return correlation list
# ========================================================================

mi.correlated.list <- function ( data, threshold = 0.99999 ){
  options(warn = -1)
  data <- as.data.frame(apply(data, 2, as.numeric))
  cor.data <- cor( data, use="pairwise.complete.obs" )
  diag(cor.data) <- 1
  index <- abs( cor.data - diag(dim(cor.data)[1])) >= threshold 
  result <- vector("list",dim(index)[1])
  for( i in 1:dim(index)[1] ){
    if(length(names(which(index[i,]==1)))>0){
      result[[i] ]<- c(names(data)[i], names(which(index[i,]==1)))
      #result[[i]]<-c(names(which(index[i,]==1)))
      result[[i]] <- result[[i]][order(result[[i]])]
    }
    else{
      result[[i]]<-NA
    }
  }
  unique.cor <- unique(result)
  unique.cor <- unique.cor[!sapply(unique.cor,is.null)] 
  unique.cor <- unique.cor[!sapply(sapply(unique.cor,is.na),any)]
  if(length(unique.cor)>0){
    for(k in 1:length(unique.cor)){
      chk <- is.na(data[,unique.cor[[k]][1]]) -  is.na(data[,unique.cor[[k]][2]])
      if(!all(chk==0)){
        unique.cor[[k]] <- NA
      }
    }
    unique.cor <- unique.cor[!sapply(sapply(unique.cor,is.na),any)]  
    unique.cor <- unique.cor[!sapply(unique.cor,is.null)] 
  }
  options(warn = 0)
  return(unique.cor)
}

# ========================================================================
# interactive wrapper for mi
# ========================================================================

mi.interactive <- function ( data ){
  cat("----------------------------------- \n")
  cat("creating information matrix:\n")
  cat("----------------------------------- \n\n")
  info <- mi.info( data )
  cat("done \n\n")
  loop <- TRUE
  while( loop ){
  resp1<-menu(c("look at current setting","proceed to mi with current setting","change current setting"),title="----------------------------------- \n Would you like to:\n-----------------------------------")
    if(resp1==1){
      cat("\n----------------------------------- \n")
      cat("current setting: \n")
      cat("----------------------------------- \n\n")
      print(info)
      cat("\n\n")
    }
    else if(resp1==2){
      loop <- FALSE
    }
    else if(resp1==3){
      info <- mi.info.fix( info )
    }
    else{
      cat("invalid selection")
    }
  }
  cat("\n----------------------------------- \n")
  cat("parameter setting for mi: \n")
  cat("----------------------------------- \n\n")
  default<-mi.default.check()
  cat("\n----------------------------------- \n")
  cat("starting mi: \n")
  cat("----------------------------------- \n\n")
  # run mi
  data.mi <- mi( data, info, n.iter=default$n.iter, n.imp = default$m, max.minutes=default$max.minutes, run.past.convergence=default$run.past.convergence )
  stop.mi <- FALSE
  if(!converged(data.mi)){
    resp.rerun<-menu(c("yes","no"),title="mi did not converge, would you like to keep it running?")
    if(resp.rerun==2){
      stop.mi <-TRUE
    }
  } else {
    resp.rerun <-menu(c("yes","no"),title="mi converged, would you like to stop?")
    if(resp.rerun==1){
      stop.mi <- TRUE
    }
  }
  while( !stop.mi ){
    deflt.chng<-menu(c("yes","no"),title="do you want to change the parameter setting?")
    if(deflt.chng==1){
      default<-mi.default.check()
    }
    data.mi <- mi( data.mi, info,n.iter=default$n.iter, max.minutes=default$max.minutes, run.past.convergence=default$run.past.convergence )
    if(!converged(data.mi)){
      resp.rerun<-menu(c("yes","no"),title="mi did not converge, would you like to keep it running?")
      if(resp.rerun==2){
        stop.mi <- TRUE
      }
    }else{
      resp.rerun <-menu(c("yes","no"),title="mi converged, would you like to stop?")
      if(resp.rerun==1){
        stop.mi <- TRUE
      }
    }
  }
  return(data.mi)
}


# ========================================================================
# recodeing variable to numeric value
# ========================================================================
mi.info.recode <- function( data, info ){
  for(i in 1:dim(data)[2]){
    if(!is.null(info[[i]]$level)){
      # treatment since recode can't handle "=" as variable name
      if(length(grep("=",names(info[[i]]$level)))>0){
        names(info[[i]]$level) <- gsub("=","@@@@@",names(info[[i]]$level))
        data[[i]]<- gsub("=","@@@@@",data[[i]])
      }
      data[[i]]<-recode(data[[i]],paste("'",names(info[[i]]$level),"'=",info[[i]]$level,sep="",collapse="; "))
    }
  }
  return(data)
}

# ========================================================================
# codeing back the variable to original value
# ========================================================================
mi.info.uncode <-function( data, info ){
  for(i in 1:dim(data)[2]){
    if(!is.null(info[[i]]$level)){
      recode.equal <- FALSE
      if(length(grep("=",names(info[[i]]$level)))>0){
        names(info[[i]]$level) <- gsub("=","@@",names(info[[i]]$level))
        recode.equal <- TRUE
      }
      if(info[[i]]$var.class=="factor"){
        data[[i]] <- factor(data[[i]])
      }
      else if(info[[i]]$var.class=="character"){
        data[[i]] <- as.character(data[[i]])
      }
      data[[i]] <- recode(data[[i]],paste(info[[i]]$level,"=","'",names(info[[i]]$level),"'",sep="",collapse="; "))
      if(recode.equal){
        data[[i]] <- gsub("@@","=",data[[i]])
      }
    }
  }
  return(data)
}
# ========================================================================
# check default setting
# ========================================================================

mi.default.check<- function( n.iter=30, m=3, max.minutes =20, run.past.convergence=FALSE ){
  # number of iteration
  cat( "mi will run for 30 iteration by default.\n" )
  resp.iter<-menu( c( "yes","no" ), title="would you like change it?" )
  if( resp.iter==1 ) {
    cat( "enter number of iteration\n" )
    n.iter <- scan( what=double(), nmax=1 )
  }
  # number of chains
  cat( "mi will create 3 chains by default.\n" )
  resp.chain <- menu( c( "yes","no" ),title="would you like change it?" )
  if( resp.chain == 1 ) {
    cat( "enter number of chains\n" )
    m <- scan( what = double( ), nmax = 1 )
  }
  # time 
  cat("mi will run for 20 minutes by default \n")
  resp.time<-menu(c("yes", "no" ),title="would you like to change it?")
  if(resp.time==1){
    cat("enter number of minutes for mi to run\n")
    max.minutes<-scan(what=double(),nmax=1)
  }
  # convergence
  cat( "mi will run until convergence but you can make it run longer \n" )
  resp.conv <- menu( c( "yes", "no" ), title = "would you like it to run after convergence?" )
  if( resp.conv == 1 ){
    run.past.convergence <- TRUE
  }
  return( list( n.iter = n.iter, m = m, max.minutes = max.minutes, run.past.convergence = run.past.convergence ) )
}

center <- function( x ) {
  return( x - mean( x, na.rm = TRUE ) )
}


#=================================================================
#     update mi.info functions
#=================================================================

update.mi.info <- function(object, target, list, ...){
  nam <- names(list)
  if (is.null(nam)) {
    if (length(list) != length(object)){
      stop ( message = "type for all the variable must be specified " )
    } 
    else {
      nam <- 1:length(list)
    }
  }
  #======================================================================
  # this fixes the include slot when "is.ID", and "all.missing" are changed
  #======================================================================
  for (i in 1:length(list)) {
    object[[nam[i]]][[target]] <- list[[nam[i]]]
    if(target=="type"){
      formal.args <- formals(as.character(type.models(object[[nam[i]]]$type)))
      object[[nam[i]]]$params <-  formal.args[!names( formal.args )%in%c("formula","data","start","...")]  
    }
    
    # is.ID
    if(target=="is.ID"){
      if(object[[nam[i]]][["include"]]){
        list[[nam[i]]] <- ifelse(list[[nam[i]]], FALSE, TRUE)
        object[[nam[i]]][["include"]] <- list[[nam[i]]]
      }
    }
    
    # all.missing
    if(target=="all.missing"){
      if(object[[nam[i]]][["include"]]){
        list[[nam[i]]] <- ifelse(list[[nam[i]]], FALSE, TRUE)
        object[[nam[i]]][["include"]] <- list[[nam[i]]]
      }
    }
  }
    
  #======================================================================
  # this fixes imp.order when "include", "is.ID", "all.missing" are changed
  #======================================================================
  # imp.order
  if(target %in% c("include", "is.ID", "all.missing")){
    ord <- 1
    for(i in 1:length(object)){
      if(object[[i]][["include"]] && object[[i]][["nmis"]]>0){
        object[[i]][["imp.order"]] <- ord
        ord <- ord + 1
      } else{
        object[[i]][["imp.order"]] <- NA
        ord <- ord
      }
    }
  }
  #======================================================================
  # this fixes imp.formula
  #======================================================================  
  #object <- mi.info.formula.default(object)
  if(target=="imp.formula"){
    for(i in 1:length(list)){
      object[[nam[i]]][["imp.formula"]] <- list[[nam[i]]]
    }
  }
  # fix the formula to get rid of the exclude variable
  excludeVar <- names(object)[!object$include] 
  for(jj in 1:length(object)){
    for(kk in 1:length(excludeVar)){
      object[[jj]]$imp.formula <- .gsubFormula(object[[jj]]$imp.formula, excludeVar[kk])
    }
  }


 
  class(object) <- "mi.info"
  return(object)
}

.gsubFormula <- function(object, pattern){
  form <- gsub(paste ("\\+[[:blank:]]*[^[:alnum:]]", pattern, "[^[:alnum:]]", sep=""), "", object)
  form <- gsub(paste ("~[[:blank:]]*[^[:alnum:]]", pattern, "[^[:alnum:]][[:blank:]]*\\+", sep=""), "~", form)
  return(form)
}

                          
mi.info.update.type <- function (object, list ) {
  info <- update(object, target="type", list )
  return(info)
}

mi.info.update.level <- function (object, list ) {
  info <- update(object, target="level", list )
  return(info)
}

mi.info.update.include <- function (object, list ) {
  info <- update(object, target="include", list )
  return(info)
}

mi.info.update.is.ID <- function (object, list ) {
  object <- update(object, target="is.ID", list) 
  return(object)  
}

mi.info.update.collinear <- function (object, list ) {
  info <- update(object, target="collinear", list ) 
  return(info)
}

#mi.info.update.transform <- function (object, list ) {
#  info <- update (object, target="transform", list )
#  return(info)
#}

mi.info.update.imp.order <- function ( object, list ) {
  info <- update (object, target="imp.order", list )
  return(info)
}

mi.info.update.determ.pred <- function ( object, list ) {
  info <- update (object, target="determ.pred", list ) 
  return(info)
}

mi.info.update.params <- function ( object, list ) {
  info <- update (object, target="params", list ) 
  return(info)
}

mi.info.update.imp.formula <- function ( object, list ) {
  info <- update (object, target="imp.formula", list )
  return(info)
}

mi.info.update.other <- function ( object, list ) {
  info <- update (object, target="other", list )
  return(info)
}



#=============================================================
# mi.info.utils
#=============================================================

is.mi.info <- function( object ) {
  return(inherits(object, "mi.info")) 
}



"$.mi.info" <- function (x, ..., drop = TRUE) {
  if (missing(x)) {
      if (drop) 
          return(drop(x))
      else return(x)
  }
  else{
    foo <- function(x){
      x[[...]]
    }
    result <- sapply(x, FUN = foo)
    return(result)
  }
}

"[.mi.info" <- function (x, ..., drop = TRUE) {
  if (missing(x)) {
    if (drop){
      return(drop(x))
    }
    else{
      return(x)
    }
  }
  else{
    foo <- function(x){
      x[[...]]
    }    
    result <- sapply(x, FUN = foo)
    return(result)
  }
}


"[<-.mi.info" <- function (x, ..., value = NULL) {
  if (missing(x)) {
    if (drop){ 
      return(drop(x))
    }
    else{
      return(x)
    }
  }
  else{
    for(i in 1:length(x)){
      x[[i]][[...]] <- value[[i]]
    }
    return(x)
  }
}


"$<-.mi.info" <- function (x, ..., value = NULL) {
  if (missing(x)) {
      if (drop) 
          return(drop(x))
      else return(x)
  }
  else{
    for(i in 1:length(x)){
      x[[i]][[...]] <- value[[i]]
    }
    return(x)
  }
}























#rankMat <- function(A, tol = NULL, singValA = svd(A, 0,0)$d)
#{
#    ## Purpose: rank of a matrix ``as Matlab''
#    ## ----------------------------------------------------------------------
#    ## Arguments: A: a numerical matrix, maybe non-square
#    ##          tol: numerical tolerance (compared to singular values)
#    ##     singValA: vector of non-increasing singular values of A
#    ##               (pass as argument if already known)
#    ## ----------------------------------------------------------------------
#    ## Author: Martin Maechler, Date:  7 Apr 2007, 16:16
#    d <- dim(A)
#    stopifnot(length(d) == 2, length(singValA) == min(d),
#              diff(singValA) < 0)       # must be sorted decreasingly
#    if(is.null(tol))
#        tol <- max(d) * .Machine$double.eps * abs(singValA[1])
#    else stopifnot(is.numeric(tol), tol >= 0)
#    return( sum(singValA >= tol) )
#}

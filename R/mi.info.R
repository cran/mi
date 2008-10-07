# ==============================================================================
# Creates information matrix
# ==============================================================================
mi.info <- function( data, threshold  = 0.99999 )
{
  if( is.matrix( data ) ) { 
    data <- data.frame( data ) 
  }
  info <- vector("list", dim( data )[2] )
  names( info ) <- dimnames( data )[[2]]
  data.original.name <- deparse( substitute( data ) )
  correlated <- mi.check.correlation( data, threshold )
  unlist( lapply( correlated, function( lst ) { lst[-1]} ) )
  ord <- 1
  for( i in 1:dim( data )[2] ) {
    info[[i]] <- vector( "list", 15 );
    names( info[[i]] ) <- c( "name","imp.order", "nmis", "type", "var.class",
                            "level", "include", "is.ID", "all.missing",
                            "correlated", "determ.pred", "imp.formula", 
                            "transform","params", "other" );
    info[[i]]$name <- dimnames(data)[[2]][i];
    # nmis
    info[[i]]$nmis <- sum( is.na( data[ ,i] ) );
    # type
    info[[i]]$type <- typecast( data[,i] );
    info[[i]]$var.class <- class( data[,i] );
    # level
    if( info[[i]]$var.class == "character" ) {
      lev <- unique( as.character( data[,i] ) )[!is.na( unique( as.character( data[,i] ) ) )];
      lev <- lev[order( lev )];
      if( length( lev ) == 2 ) {
        info[[i]]$level <- c( 0, 1 );
      } else{
        info[[i]]$level <- 1:length( lev );
      }
      names( info[[i]]$level ) <- lev;
    } else if( info[[i]]$var.class == "factor" ) {
      lev <-levels( data[ ,i] )[ !is.na( levels( data[ ,i] ) )];
      lev <- lev[!( lev %in% c( "NA", "RF", "DK" ) )];
      #ulev<-unique(as.character(data[,i]))[!is.na(unique(as.character(data[,i])))]     
      if( length( lev ) == 2 ) {
        info[[i]]$level <- c( 0, 1 );
      } else {
        info[[i]]$level <- 1:length( lev );
      }
      names( info[[i]]$level ) <- lev;
    }

    info[[i]]$is.ID <- if( length(unique( data[ !is.na(data[,i]),i] )) == length( data[,i] ) 
                        && is.integer( data[,i] ) && all(data[,i]==data[order( data[,i] ),i]) ){ 
                          TRUE;
                        } else { 
                          FALSE;
                        }   
    info[[i]]$include <- if( info[[i]]$is.ID ){ FALSE } else { TRUE }
    # all missing then exclude
    info[[i]]$all.missing <- if( sum( is.na( data[ ,i] ) ) == dim( data )[1] ){
                               TRUE;
                             } else { 
                               FALSE;
                             }    
    if(info[[i]]$include){
      info[[i]]$include <- if( info[[i]]$all.missing ) { 
                             FALSE;
                           } else { 
                             TRUE;
                           }
    }
    # order
    if(info[[i]]$include && info[[i]]$nmis>0){
      info[[i]]$imp.order <- ord;
      ord <- ord + 1;
    } else{
      info[[i]]$imp.order <- NA;
    }
    # correlated 
    info[[i]]$correlated <- correlated[[i]];
    # params
    formal.args <- formals( as.character( type.models( info[[i]]$type ) ) );
    info[[i]]$params <- formal.args[!names( formal.args ) %in% c( "formula", "data", "start", "..." )];    
    # transform
    #info[[i]]$transform <- if(info[[i]]$type%in%c("unordered-categorical","dichotomous")){}
  }
 
  # all missing
  allmis <- sapply( info, function( inf ){ inf$all.missing } );
  if( any( allmis ) ) {
    cat("\avariable(s)", paste( names(info)[allmis],collapse=", ",sep=""),
         "has(have) no observed value, and will be omitted.\n\n" );
  }
  # correlated then exclude  
  data.correlated <- mi.correlated.list( data );
  if( length( data.correlated ) > 0 ) {
    for( c.idx in 1:length( data.correlated ) ) {
      c.nm <- data.correlated[[c.idx]][-1];
      for( nm.idx in 1:length(c.nm)){
        info[[c.nm[nm.idx]]]$include     <- FALSE;
        info[[c.nm[nm.idx]]]$imp.order   <- NA;
        info[[c.nm[nm.idx]]]$determ.pred <- paste(data.correlated[[c.idx]][1] );
      }
    }
  }
  ord.temp <- imp.order(info);
  re.ord   <- ord.temp[ !is.na( ord.temp ) ];
  ord.temp[!is.na( ord.temp )] <- rank( re.ord, ties.method = "first" );
  for( ord.index in 1:length( ord.temp ) ) {
    info[[ord.index]]$imp.order <- ord.temp[ord.index];
  }
  # default formula
  info <- mi.info.formula.default( data, info );
  if( length( mi.correlated.list( data ) ) > 0 ) {
    cat( "\afollowing variables are correlated\n" );
    print( data.correlated );
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
  
  class( info ) <- c( "mi.info", "list" );
  return( info );
}

mi.info.formula.default <-function( data, info ){
  for( i in 1:dim(data)[2]){
    # default formula
    inc <- sapply(info, function(inf){inf$include} )
    inc[i]<-FALSE
    dimnames(data)[[2]][i]
    info[[i]]$imp.formula <- type.default.formula(dimnames(data)[[2]][i],dimnames(data[,inc,drop=FALSE])[[2]],info[[i]]$type)
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
type.default.formula <- function( response.name, predictor.name, type ) {
  if (type=="mixed"){
    form <- list(paste( paste( "1*(", response.name, "!=0) ~",sep=""),paste(predictor.name,collapse=" + ")),
                 paste( response.name,"~",paste(predictor.name,collapse=" + ")))
  } else if (type=="squareroot-continuous"){
    form <- paste( paste( "sqrt(", response.name, ") ~",sep=""),paste(predictor.name,collapse=" + "))
  } else if (type=="logscale-continuous"){
    form <- paste( paste( "log(", response.name, ") ~",sep=""),paste(predictor.name,collapse=" + "))
  } else if (type=="ordered-categorical"){
    form <- paste( paste( "factor(", response.name, ") ~",sep=""),paste(predictor.name,collapse=" + "))
  } else if (type=="fixed"){
    form <- paste( response.name, " ~",response.name)
  } else{
    form <- paste( response.name,"~",paste(predictor.name,collapse=" + "))
  }
  return(form) 
}


# ========================================================================
# Extracts the type (character) as vector
# ========================================================================

type <-function(info){
  return(sapply(info,function(inf){inf$type}))
}

# ========================================================================
# Extracts the level (vector) as list
# ========================================================================

level <-function(info){
  return(sapply(info,function(inf){inf$level}))
}

# ========================================================================
# Extract imputation formula (character) as list
# ========================================================================

imp.formula <-function(info){
  return(sapply(info,function(inf){inf$level}))
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
        inc.change <- data.frame( include=include( info ) )
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
        ord.temp <- imp.order(info)
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
    ord.flg<-TRUE
    while(ord.flg){
      res.ord<-menu(c("Yes","No","look at current setting")
                    , title=.make.title("change imputation order?" ))
      #change order
      if(res.ord == 1){
        ord.change.temp <- as.data.frame(imp.order(info))
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
      } else if(res.ord == 2){
        #no change
        ord.flg<-FALSE
      } else if(res.ord == 3){
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
                  info[[Var.to.fix]]$type <- mi.types()[res.var3.type]
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

mi.check.correlation <- function ( data, threshhold = 0.99999 ){
  cor.data <- cor( data, use = "pairwise.complete.obs" )
  diag( cor.data ) <- 1
  index  <- abs( cor.data - diag( dim( cor.data )[1] ) ) >= threshhold 
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
return(result)
}

# ========================================================================
# return correlation list
# ========================================================================

mi.correlated.list <- function ( data, threshhold = 0.99999 ){
  cor.data<-cor( data, use="pairwise.complete.obs" )
  diag(cor.data)<-1
  index<-abs( cor.data - diag(dim(cor.data)[1])) >= threshhold 
  result<-vector("list",dim(index)[1])
  for( i in 1:dim(index)[1] ){
    if(length(names(which(index[i,]==1)))>0){
      result[[i]]<-c(names(data)[i],names(which(index[i,]==1)))
      #result[[i]]<-c(names(which(index[i,]==1)))
      result[[i]]<-result[[i]][order(result[[i]])]
    }
    else{
      result[[i]]<-NA
    }
  }
  unique.cor<-unique(result)
  unique.cor<- unique.cor[!sapply(unique.cor,is.null)] 
  unique.cor<- unique.cor[!sapply(sapply(unique.cor,is.na),any)]
  return(unique.cor)
return(result)
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
  data.mi <- mi( data, info, n.iter=default$n.iter, n.imp = default$m, max.minutes=default$max.minutes, continue.on.convergence=default$continue.on.convergence )
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
    data.mi <- mi( data.mi, info,n.iter=default$n.iter, max.minutes=default$max.minutes, continue.on.convergence=default$continue.on.convergence )
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
mi.info.recode <-function( data, info ){
  for(i in 1:dim(data)[2]){
    if(!is.null(info[[i]]$level)){
      # treatment since recode can't handle "=" as variable name
      if(length(grep("=",names(info[[i]]$level)))>0){
        names(info[[i]]$level) <- gsub("=","@@@@@",names(info[[i]]$level))
        data[[i]]<-gsub("=","@@@@@",data[[i]])
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
        data[[i]]<-factor(data[[i]])
      }
      else if(info[[i]]$var.class=="character"){
        data[[i]]<-as.character(data[[i]])
      }
      data[[i]]<-recode(data[[i]],paste(info[[i]]$level,"=","'",names(info[[i]]$level),"'",sep="",collapse="; "))
      if(recode.equal){
        data[[i]]<-gsub("@@","=",data[[i]])
      }
    }
  }
  return(data)
}
# ========================================================================
# check default setting
# ========================================================================

mi.default.check<- function( n.iter=30, m=3, max.minutes =20, continue.on.convergence=FALSE ){
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
  cat("mi will run for 20 minutes by default")
  resp.time<-menu(c("yes", "no" ),title="would you like to change it?")
  if(resp.time==1){
    cat("enter number of minutes for mi to run\n")
    max.minutes<-scan(what=double(),nmax=1)
  }
  # convergence
  cat( "mi will run until convergence but you can make it run longer \n" )
  resp.conv <- menu( c( "yes", "no" ), title = "would you like it to run after convergence?" )
  if( resp.conv == 1 ){
    continue.on.convergence <- TRUE
  }
  return( list( n.iter = n.iter, m = m, max.minutes = max.minutes, continue.on.convergence = continue.on.convergence ) )
}

center <- function( x ) {
  return( x - mean( x, na.rm = TRUE ) )
}


mi.info.update <-function( info, target, list ) {
  if( !is.mi.info( info ) ) {
    stop ( message = "this function is available only for mi.info objects" )
  } else {
    nam <- names( list )
    if ( is.null( nam ) ) {
      if ( length( list ) != length ( info ) ){
        stop ( message = "type for all the variable must be specified " )
      } else {
        nam <- 1:length( list )
      }
    } 
    for ( i in 1:length( list ) ) {
      info[[nam[i]]][[target]] <- list[[nam[i]]]
    }
  }
  return( info )
}
                            
mi.info.update.type <- function ( info, list ) {
  return( mi.info.update ( info, "type", list ) )
}
mi.info.update.level <- function ( info, list ) {
  return( mi.info.update ( info, "level", list ) )
}
mi.info.update.include <- function ( info, list ) {
  return( mi.info.update ( info, "include", list ) )
}
mi.info.update.is.ID <- function ( info, list ) {
  return( mi.info.update ( info, "is.ID", list ) )
}
mi.info.update.correlated <- function ( info, list ) {
  return( mi.info.update ( info, "correlated", list ) )
}
mi.info.update.transform <- function ( info, list ) {
  return( mi.info.update ( info, "transform", list ) )
}
mi.info.update.imp.order <- function ( info, list ) {
  return( mi.info.update ( info, "imp.order", list ) )
}
mi.info.update.determ.pred <- function ( info, list ) {
  return( mi.info.update ( info, "determ.pred", list ) )
}
mi.info.update.params <- function ( info, list ) {
  return( mi.info.update ( info, "params", list ) )
}
mi.info.update.imp.formula <- function ( info, list ) {
  return( mi.info.update ( info, "imp.formula", list ) )
}
mi.info.update.other <- function ( info, list ) {
  return( mi.info.update ( info, "other", list ) )
}

is.mi.info <- function( object ) {
  return( inherits( object, "mi.info" ) ) 
}


"$.mi.info" <- function (x, ..., drop = TRUE) {
  if (missing(x)) {
      if (drop) 
          return(drop(x))
      else return(x)
  }else{
    result <- NULL
    result<-sapply(x,function(xs){xs[[...]]})
    return(result)
  }
}
"[.mi.info" <- function (x, ..., drop = TRUE) {
  if (missing(x)) {
      if (drop) 
          return(drop(x))
      else return(x)
  }else{
    result <- NULL
    result<-sapply(x,function(xs){xs[[...]]})
    return(result)
  }
}

"[<-.mi.info" <- function (x, ..., value = NULL) {
  if (missing(x)) {
      if (drop) 
          return(drop(x))
      else return(x)
  }else{
    result <- NULL
    for(i in 1:length(x)){
      x[[i]][[...]]<-value[[i]]
    }
    return(x)
  }
}
"$<-.mi.info" <- function (x, ..., value = NULL) {
  if (missing(x)) {
      if (drop) 
          return(drop(x))
      else return(x)
  }else{
    result <- NULL
    for(i in 1:length(x)){
      x[[i]][[...]]<-value[[i]]
    }
    return(x)
  }
}

rankMat <- function(A, tol = NULL, singValA = svd(A, 0,0)$d)
{
    ## Purpose: rank of a matrix ``as Matlab''
    ## ----------------------------------------------------------------------
    ## Arguments: A: a numerical matrix, maybe non-square
    ##          tol: numerical tolerance (compared to singular values)
    ##     singValA: vector of non-increasing singular values of A
    ##               (pass as argument if already known)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  7 Apr 2007, 16:16
    d <- dim(A)
    stopifnot(length(d) == 2, length(singValA) == min(d),
              diff(singValA) < 0)       # must be sorted decreasingly
    if(is.null(tol))
        tol <- max(d) * .Machine$double.eps * abs(singValA[1])
    else stopifnot(is.numeric(tol), tol >= 0)
    return( sum(singValA >= tol) )
}

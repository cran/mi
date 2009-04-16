#mi.check.correlation <- function (data, threshhold = 1){
#  #cat("Checking for correlation")
#  options(show.error.messages = FALSE)
#  cor.data <- cor(data, use="pairwise.complete.obs")
#  diag(cor.data) <- 1
#  #index<-abs( cor.data*upper.tri(cor.data)) >= threshhold  
#  index <- abs(cor.data - diag(dim(cor.data)[1])) >= threshhold 
#  result <- vector("list", dim(index)[1])
#  for(i in 1:dim(index)[1]){
#    result[[i]] <- c(names(which(index[i,]==1)))
#  }
#  options(show.error.messages = TRUE)
#  return(result)
#}

# preprocess: this is ugly..but working!..need to improve it
mi.preprocess <- function(data, info){
  n.col <- ncol(data)
  n.row <- nrow(data)
  var.name <- info$name
  type <- info$type
  for (i in 1:n.col){
    typ <- type[i]
    if(typ == "mixed"){
      tmp <- ifelse(data[,i] > 0, 1, ifelse(data[,i]==0, 0, NA))
      data <- cbind(data, tmp)
      Ind.lab <- paste(var.name[i], "ind", sep=".")
      names(data)[ncol(data)] <- Ind.lab
      type[ncol(data)] <- "dichotomous"
      names(type)[ncol(data)] <- Ind.lab
      data[,i] <- log(ifelse(data[,i]>0, data[,i], NA))
      type[i] <- "log-continuous"
    }
    if (typ == "positive-continuous"){
      data[,i] <- log(data[,i])
      type[i] <- "log-continuous"
    }
    if (typ == "proportion"){
      data[,i] <- logit(data[,i])
      type[i] <- "proportion"
    }
    else{
      data[,i] <- data[,i]
      type[i] <- type[i]
    }
  }
  return(list(data=data, type=type))
}

mi.postprocess <- function(mi.data, info){
  if(class(mi.data)=="list"){
    n.chains <- length(mi.data)
  }
  if(class(mi.data)=="data.frame"){
    n.chains <- 1
  }
  if(class(mi.data)=="matrix"){
    n.chains <- 1
  }
  n.col <- length(info)
  varnames <- info$name
  type <- info$type
  idx <- grep(".ind", varnames)
  if(n.chains==1){
    for (i in 1:n.col){
      typ <- type[i]
      if(typ == "log-continuous"){
        mi.data[,i] <- exp(mi.data[,i])
      }
      if(typ == "proportion"){
        mi.data[,i] <- invlogit(mi.data[,i])
      }
      if(sum(grep(".ind", varnames[i]))){
        mixed.name <- gsub(".ind", "", varnames[i])
        mi.data[,mixed.name] <- mi.data[,mixed.name] * mi.data[,varnames[i]]
      }
    }
    if(sum(idx)==0){
      mi.data <- mi.data
    }
    else{
      mi.data <- mi.data[,-idx]
    }
  }
  else{
    for(s in 1:n.chains){
      for (i in 1:n.col){
        typ <- type[i]
        if(typ == "log-continuous"){
          mi.data[[s]][,i] <- exp(mi.data[[s]][,i])
        }
        if(typ == "proportion"){
          mi.data[[s]][,i] <- invlogit(mi.data[[s]][,i])
        }
        if(sum(grep(".ind", varnames[i]))){
          mixed.name <- gsub(".ind", "", varnames[i])
          mi.data[[s]][,mixed.name] <- mi.data[[s]][,mixed.name] * mi.data[[s]][,varnames[i]]
        }
      }
      if(sum(idx)==0){
        mi.data[[s]] <- mi.data[[s]]
      }
      else{
        mi.data[[s]] <- mi.data[[s]][,-idx]
      }
    } 
  }
  return(mi.data)
}

#
#mi.preprocess <- function( data, info=NULL, trans = trans.func, name = trans.name ){
#  if(is.null(info)){ 
#    info <- mi.info( data ) 
#  }
#  inVar     <- .include( info )
#  type.list <- type( info )
#  processed <-data[ ,inVar,drop=FALSE]
#  
#  
#  for ( i in 1:dim( processed )[2]){
#    processed[,i] <- trans( type.list[i])( data[,i] )
#    if( !is.null( name( type.list[i] ) ) ) {
#      dimnames( processed )[[2]][i] <- paste( name( type.list[i] ),dimnames( data )[[2]][i], sep="" )
#    }
#  }
#  return( processed )
#}
#
#
#
#mi.postprocess<-function(data, type.list, trans = inverse.func, name = trans.name  ){
#  processed <-data
#  for (i in 1:dim(data)[2]){
#    processed[,i] <- trans(type.list[i])(data[,i])
#
#    if(!is.null(name(type.list[i]))){
#      dimnames(processed)[[2]][i]<-sub(name(type.list[i]),"",dimnames(data)[[2]][i])
#    }
#  }
#  return(processed)
#}

#mi.recode <- function(data, info, undo=FALSE){
#  for (i in 1:dim(data)[2]){
#    if(is.character(data[,i])){
#      if(undo){
#        rec<- paste("'", .level(info)[[i]],"' =",names(.level(info)[[i]]),sep="")
#      }
#      else{
#        rec<-paste("'",names(.level(info)[[i]]),"' =",.level(info)[[i]],sep="")
#      }
#      for(j in 1:length(rec)){
#        data[,i]<-recode(data[,i],rec[i])
#      }
#    }
#    else if (is.factor(data[,i])){
#      if(undo){
#        levels(data[,i]) <- names(.level(info)[[i]])
#      }
#      else{
#        levels(data[,i]) <- .level(info)[[i]]
#      }
#    }
#  }
#  return(data)
#}
#
#trans.func<-function(type){
#    fun <-if     (type == "squareroot-continuous" ) {sqrt}
#          else if(type == "logscale-continuous" ) {log}
#          else if(type == "mixed" ) {function ( x ) {
#                                        x[x>0&!is.na(x)]<-sqrt(x[x>0&!is.na(x)])  
#                                        return(x)
#                                      } }
#          else { function ( x ) { x } }
#    return( fun )
#}
#trans.name<-function(type){
#    name <-if     (type == "squareroot-continuous" ) {"sqrt."}
#          else if(type == "logscale-continuous" ) {"log."}
#          else if(type == "mixed" ) {"msqrt."}
#          else { NULL }
#    return( name )
#}
#
#inverse.func<-function(type){
#    fun <-if     (type == "squareroot-continuous" ) {function ( x ) { x^2 }}
#          else if(type == "logscale-continuous" ) {function ( x ) { exp( x ) }}
#          else if(type == "mixed" ) {function ( x ) {
#                                        x[ x>0 & !is.na( x ) ] <- ( x[ x>0 & !is.na( x ) ] )^2 
#                                        return( x )
#                                      } }
#          else { function ( x ) { x } }
#    return( fun )
#}
#
#


#plot.outcheck <- function( data , standardize = FALSE ) {
#  par( mar = c( 4.5, 11, 3, 1 ) )
#  nm <- names( data )
#  if( standardize ) {
#    data <- apply( data, 2,function(x){(x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))})
#  }
#  mn <- colMeans(data, na.rm=T)
#  se <- apply( data, 2, sd, na.rm=TRUE)
#  ot <- apply( data, 2, function(x){boxplot(x,plot=FALSE)$out})
#  #co <- apply( data, 2, function(x){boxplot(x,plot=FALSE)$conf})
#  plot(range(data,na.rm=T),c(0,dim(data)[2]+1),type="n",yaxt = "n",ylab="",xlab="",yaxs = "i")
#  axis(side=2,at=1:dim(data)[2],labels=nm,las=2)
#  for(i in 1:dim(data)[2]){
#    points(mn[i], i)
#    lines( c( mn[i]+se[i], mn[i]-se[i] ), c( i, i ) )
#    #lines( c( co[1,i],co[2,i] ), c( i, i ) )
#    points( ot[[i]], ot[[i]]*0+i, pch=20 )
#  }
#  return(ot)
#}


#mi.check.discrete <- function (data, threshhold = 0.5){
#  discrete<-length(unique(data)) < length(data)*threshhold
#  return(discrete)
#}


#mi.remove.all.missing <- function ( data ){
#  cat("following variables have no observed values, thus will be omitted from the imputation procedure\n")
#  print(dimnames(data)[[2]][colSums(is.na(data))==dim(data)[[1]]])
#  return(data[colSums(is.na(data))!=dim(data)[[1]]])
#}






#mi.check <- function ( data , filename = "model.txt" ) {
#  n.col <- ncol(data)
#  varnames <- names(data)
#  result.type <- vector("character", n.col)
#  names(result.type) <- varnames
#  default.type <- typecast(data)
#  cat("-----starting the check (select 0 to quit)-------\n")
#  res0 <- menu(
#    c("go through all of the variables.",
#      "choose specific variable to change."),
#      title="Would you like to"
#      )
#  # Go through all the variables
#  if(res0 == 1 ) {
#    for(i in 1:dim(data)[2]){
#      y <- data[,i]
#      y.name <- varnames[i]
#      cat("checking variable:", y.name,"\n")
#      type <- default.type[i]
#      cat("default type for variable:", y.name, "is", type, "\n")
#      
#      res <- menu(c("Yes","No"), title="would you like to change it?")
#      if(res==1){
#        new.type <- menu(mi.types(),title="select the type:")
#        result.type[i] <- if(new.type==0){ 
#                            break 
#                          } 
#                          else{ 
#                            new.type 
#                          }
#      }
#      else if(res==2){
#        result.type[i] <- type
#      }
#      else if(res==0){
#        break
#      }
#      else{
#      }    
#    }
#  }
#  # Choose variable
#  else if (res0 == 2){
#    esc<-0
#    while( esc == 0 ){
#      cat("Please enter a name of variable you wish to edit\n")
#      print(paste(varnames, sep=","))
#      varname <- scan( what = character(), sep = "\n", strip.white = TRUE, nlines=1, quiet=TRUE )
#      if( varname %in% dimnames(data)[[2]]){
#                
#      }
#      else if ( varname == "list"){
#       
#      }
#      else if ( varname == "end"){
#        esc <- "1"
#      }
#      else{}
#    }
#  }
#  
#  else if (res0 == 0){
#   break
#  }
#  return(result.type)
#}

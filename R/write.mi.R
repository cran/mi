write.mi <- function (object, format = c("csv", "dta", "table"), ...)
{
  n.chains <- m(object)
  format <- match.arg(format)
  if (!(format %in% c("csv", "table", "dta"))) {
    stop("The data format is not supported")
  }
  
  if(format == "csv"){
    ext <- ".csv"
  }
  else if(format == "dta"){
    ext <- ".dta"
  }
  else{
    ext <- ".dat"
  }
    
  datalist <- mi.completed(object)
    
  for (i in 1:n.chains) {
    if (format == "csv") {
      write.csv(datalist[[i]], paste("midata", i, ext, sep = ""), ...)
    }
    else if (format == "dta") {
      write.dta(datalist[[i]], paste("midata", i, ext, sep = ""), ...)
    }
    else{
      write.table(datalist[[i]], paste("midata", i, ext, sep = ""), ...)
    }
  }
  invisible()
}

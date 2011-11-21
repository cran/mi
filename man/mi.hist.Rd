\name{mi.hist}
%\docType{genericFunction}
\alias{mi.hist}
\alias{mi.hist,ANY,ANY-method}
\alias{mi.hist,mi.method,ANY-method}
\alias{mi.hist,mi.categorical,ANY-method}
\alias{mi.hist,mi.binary,ANY-method}
\alias{mi.hist,mi.polr,ANY-method}
\alias{mi.hist,mi.pmm,ANY-method}
\alias{mi.hist,ANY-method}
\alias{mi.hist,mi.method-method}
\alias{mi.hist,mi.categorical-method}
\alias{mi.hist,mi.binary-method}
\alias{mi.hist,mi.polr-method}
\alias{mi.hist,mi.pmm-method}




\title{ 
    Multiple Imputation Histogram 
}
\description{
    A function for plotting the histogram of each variable and of its observed and imputed values. 
}
\usage{
  mi.hist( object, Yobs, \dots)
\S4method{mi.hist}{mi.method,ANY}( object, Yobs, \dots)
\S4method{mi.hist}{mi.categorical,ANY}( object, Yobs, \dots)
\S4method{mi.hist}{mi.binary,ANY}( object, Yobs,\dots)
\S4method{mi.hist}{mi.polr,ANY}( object, Yobs, \dots)
\S4method{mi.hist}{mi.pmm,ANY}( object, Yobs, \dots)
}
\arguments{
    \item{Yobs}{ observed values.}
    \item{object}{ imputed values or member object of mi.method object family. }
    \item{\dots}{ Other options for plot function. }  
}
\value{
    The histogram (in black) of the complete variable, the histogram (in blue) of the observed values
    and the histogram (in red) of the imputed values. 
}
\references{ 
    Kobi Abayomi, Andrew Gelman and Marc Levy. (2008).
    \dQuote{Diagnostics for multivariate imputations}.
    \emph{Applied Statistics} 57, Part 3: 273--291.

  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
   
    Andrew Gelman and Jennifer Hill. (2007).
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}.
    Cambridge University Press.
}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\note{
    The histogram of the completed values (observed plus imputed) is in black, 
    the histogram of the imputed values in red, 
    while the one of the observed values in blue.}
\seealso{
    \code{\link{plot.mi}},
    \code{\link{hist}}
}
\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  # create artificial missingness on y
  y[seq(2,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  dat.cont.mi <- mi.continuous(y~x, data = dat.xy)
  mi.hist( dat.cont.mi, y)

  # imputation
  dat.mi <- mi(dat.xy)
  mi.hist( imp(dat.mi,1)[["y"]], y)
}
\keyword{ models }

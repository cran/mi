\name{lm.mi}
\docType{class}
% Classes
\alias{mi.lm-class}
% Functions
\alias{lm.mi}
% Methods
\alias{print.mi.lm}
\alias{display,mi.lm-method}
\alias{coef,mi.lm-method}
\alias{coefficients,mi.lm-method}
\alias{se.coef,mi.lm-method}

\title{ Linear Regression Function for Multiply Imputed Dataset }
\description{
  Linear Regression Function that pulls together the estimates from multiply imputed dataset.
}
\usage{
lm.mi(formula, mi.object, ...)
\S3method{print}{mi.lm}(x, \dots)
\S4method{coef}{mi.lm}(object)
\S4method{se.coef}{mi.lm}(object)
\S4method{display}{mi.lm}(object, digits=2)
}
\arguments{
  \item{formula}{ View \code{lm} for detail. }
  \item{mi.object}{ \code{mi} object. }
  \item{x}{\code{mi.lm} object.}
  \item{object}{\code{mi.lm} object.}
  \item{digits}{number of significant digits to display, default=2.}
  \item{\dots}{ Any option to pass on to \code{lm} function. }
}
\details{
  Extracs the starting values from \code{mi.method} object family.
}
\value{
  \item{call}{the matched call.}
  \item{mi.pooled}{pulled estimates from the multiple dataset.}
  \item{mi.fit}{estimates from each dataset. See \code{lm} for details.}

}
\references{
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}

\author{
  Masanao Yajima  \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{ys463@columbia.edu}
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman   \email{gelman@stat.columbia.edu}
}
\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
 
  # create artificial missingness
  x[seq(1,100,10)] <- NA
  y[seq(2,98,10)] <- NA
  dat.xy <- data.frame(x,y)
 
  # imputation 
  dat.mi <- mi(dat.xy)
 
  # fitting linear regression 
  fit <- lm.mi(y ~ x, dat.mi)
  display(fit)
  coef(fit)
  se.coef(fit)
}
\seealso{
  \code{\link{mi}}, 
  \code{\link{lm.mi}}
}
\keyword{ models }

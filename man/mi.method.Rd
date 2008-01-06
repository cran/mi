\name{mi.method}
\docType{class}
% Classes
\alias{mi.method-class}
\alias{mi.method}
% Function
\alias{imputed}
\alias{print,mi.method-method}
\alias{plot.mi.method}
\alias{fitted.mi.method}
\alias{coef.mi.method}
\alias{imputed,mi.method-method}
\alias{residuals.mi.method}
\alias{sigma.hat,mi.method-method}

\title{
    Virtual class for all mi classes.
}
\description{
    Imputes univariate missing data using linear regression.
}
\usage{
  \S4method{imputed}{mi.method}(object,y)
  \S3method{coef}{mi.method}(object, \dots)
  \S4method{sigma.hat}{mi.method}(object, \dots)
  \S3method{fitted}{mi.method}(object, \dots)
  \S3method{residuals}{mi.method}(object, y, \dots)
  \S4method{print}{mi.method}(x, \dots)
  \S3method{plot}{mi.method}(x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{mi.method} object.}
  \item{\dots}{ Currently not used. }
  \item{x}{ \code{mi.method} object.}
  \item{y}{ Observed values.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }

}
\details{
  mi.method is a virtual class for all the \code{mi} classes.  
  Basically all the necessary functions are defined under \code{mi.method} class, thus
  most of the \code{mi} classes that do not have specific method defined for them inherits their methods from this class.
  For some special class as \code{mi.mixed} these methods are extended to tailor to the needs.
}
%\value{
%    \item{model   }{A summary of the fitted model.}
%    \item{expected}{The expected values estimated by the model.}
%    \item{random  }{Vector of length n.mis of random predicted values predicted by using the normal distribution.} 
%}
\references{
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2006.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{
    \code{\link{mi}}
}

\keyword{ models }

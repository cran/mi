\name{mi.method}
\docType{class}
% Classes
\alias{mi.method-class}
\alias{mi.method}
% Function
\alias{imputed}
\alias{print,mi.method-method}
\alias{plot,mi.method-method}
\alias{plot,mi.method,ANY-method}
\alias{fitted,mi.method-method}
\alias{coef,mi.method-method}
\alias{coefficients,mi.method-method}
\alias{imputed,mi.method-method}
\alias{imputed,mi.categorical-method}
\alias{imputed,mi.polr-method}
\alias{residuals,mi.method-method}
\alias{resid,mi.method-method}
\alias{sigma.hat,mi.method-method}

\title{
    Virtual class for all mi classes.
}
\description{
    Imputes univariate missing data using linear regression.
}
\usage{
  \S4method{imputed}{mi.method}(object,y)
  \S4method{imputed}{mi.categorical}(object,y)
  \S4method{imputed}{mi.polr}(object,y)
  \S4method{coef}{mi.method}(object)
  \S4method{coefficients}{mi.method}(object)
  \S4method{sigma.hat}{mi.method}(object)
  \S4method{fitted}{mi.method}(object)
  \S4method{resid}{mi.method}(object, y)
  \S4method{residuals}{mi.method}(object, y)
  \S4method{print}{mi.method}(x, \dots)
  \S4method{plot}{mi.method,ANY}(x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots)
}

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
  For some special class as \code{mi.nonnegative} these methods are extended to tailor to the needs.
}


\references{
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{
    \code{\link{mi}}
}

\keyword{models}
\keyword{aplot}
\keyword{dplot}
\keyword{hplot}

\name{glm.mi}
\docType{class}
% Classes
\alias{mi.glm-class}
% Functions
\alias{glm.mi}
% Methods
\alias{print.mi.glm}
\alias{display,mi.glm-method}
\alias{coef,mi.glm-method}
\alias{coefficients,mi.glm-method}
\alias{se.coef,mi.glm-method}


\title{ Generalized Linear Modeling Function for Multiply Imputed Dataset }

\description{
  Generalized Linear Modeling Function that pulls together the estimates from multiple imputed dataset.
}

\usage{
glm.mi(formula, mi.object, family = gaussian, ...)
\S3method{print}{mi.glm}(x, \dots)
\S4method{coef}{mi.glm}(object)
\S4method{se.coef}{mi.glm}(object)
\S4method{display}{mi.glm}(object, digits=2)
}

\arguments{
  \item{formula}{ See \code{glm} for detail. }
  \item{mi.object}{ mi object. }
  \item{family}{See \code{glm} for detail.}
  \item{x}{\code{mi.glm} object.}
  \item{object}{\code{mi.glm} object.}
  \item{digits}{number of significant digits to display, default=2.}
  \item{\dots}{ Any option to pass on to glm function. }
}
\details{
  See \code{\link[stats]{glm}}.
}
\value{
  \item{call}{the matched call.}
  \item{glm.mi.pooled}{pulled estimates from the multiple dataset.}
  \item{glm.mi.fit}{estimates from each dataset. See \code{glm} for details.}

}
\references{
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}

\author{
  Masanao Yajima  \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{ys463@columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman   \email{gelman@stat.columbia.edu}
}
\examples{
  # true data
  x <- rnorm(100,0,1) # N(0,1)
  y <- rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)

  # create artificial missingness
  x[seq(1,100,10)]<-NA
  y[seq(2,98,10)]<-NA
  dat.xy <- data.frame(x,y)

  # imputation 
  dat.mi <- mi(dat.xy)

  # fitting linear regression 
  fit <- glm.mi(y ~ x, dat.mi, family = gaussian)
  display(fit)
  coef(fit)
  se.coef(fit)
}
\seealso{
  \code{\link{mi}}, 
  \code{\link{lm.mi}}
}
\keyword{ models }

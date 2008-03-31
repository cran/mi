\name{lmer.mi}
\docType{class}
% Classes
\alias{mi.mer-class}
% Functions
\alias{lmer.mi}
\alias{glmer.mi}
% Methods
\alias{print.mi.mer}
\alias{display,mi.mer-method}
\alias{coef,mi.mer-method}
\alias{coefficients,mi.mer-method}
\alias{se.coef,mi.mer-method}


\title{Fit A Linear Mixed Model or A Generalized Linear Mixed Model for Multiply Imputed Dataset }

\description{
  Fit A Linear Mixed Model or A Generalized Linear Mixed Model that pulls together the estimates from multiple imputed dataset.
}

\usage{
lmer.mi(formula, mi.object, rescale=FALSE, ...)
glmer.mi(formula, mi.object, family = gaussian, rescale=FALSE, ...)
\S3method{print}{mi.mer}(x, \dots)
\S4method{coef}{mi.mer}(object)
\S4method{se.coef}{mi.mer}(object)
\S4method{display}{mi.mer}(object, digits=2)
}

\arguments{
  \item{formula}{See \code{\link[lme4]{lmer}} for detail. }
  \item{mi.object}{mi object.}
  \item{family}{See \code{\link[lme4]{lmer}} for detail.}
  \item{rescale}{default is FALSE, see \code{\link[arm]{rescale}} for detail.}
  \item{x}{\code{mi.mer} object.}
  \item{object}{\code{mi.mer} object.}
  \item{digits}{number of significant digits to display, default=2.}
  \item{\dots}{ Any option to pass on to \code{\link[lme4]{lmer}} function. }
}
\details{
  See \code{\link[lme4]{lmer}}.
}
\value{
  \item{call}{the matched call.}
  \item{mi.pooled}{pulled estimates from the multiple dataset.}
  \item{mi.fit}{estimates from each dataset. See \code{\link[lme4]{lmer}} for details.}

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
}

\seealso{
  \code{\link[lme4]{lmer}}
}

\keyword{models}

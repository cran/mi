\name{mi.pooled}
\docType{class}
% Classes
\alias{mi.pooled}
\alias{mi.pooled-class}
% Functions
\alias{lm.mi}
\alias{glm.mi}
\alias{bayesglm.mi}
\alias{polr.mi}
\alias{bayespolr.mi}
\alias{lmer.mi}
\alias{glmer.mi}
% Methods
\alias{print.mi.pooled}
\alias{display,mi.pooled-method}
\alias{coef,mi.pooled-method}
\alias{coefficients,mi.pooled-method}
\alias{se.coef,mi.pooled-method}


\title{Modeling Functions for Multiply Imputed Dataset }

\description{
  Modeling Function that pulls together the estimates from multiply imputed dataset.
}

\usage{
mi.pooled(coef, se)
lm.mi(formula, mi.object, ...)
glm.mi(formula, mi.object, family = gaussian, ...)
bayesglm.mi(formula, mi.object, family = gaussian, ...)
polr.mi(formula, mi.object, ...)
bayespolr.mi(formula, mi.object, ...)
lmer.mi(formula, mi.object, rescale=FALSE, ...)
glmer.mi(formula, mi.object, family = gaussian, rescale=FALSE, ...)
\S3method{print}{mi.pooled}(x, \dots)
\S4method{coef}{mi.pooled}(object)
\S4method{se.coef}{mi.pooled}(object)
\S4method{display}{mi.pooled}(object, digits=2)
}

\arguments{
  \item{coef}{list of coefficients}
  \item{se}{list of standard errors}
%  \item{m}{number of chains for the imputation}
  \item{formula}{See \code{\link[stats]{lm}}, 
    \code{\link[stats]{glm}}, \code{\link[MASS]{polr}}, 
    \code{\link[lme4]{lmer}}for detail.}
  \item{mi.object}{\code{mi} object}
  \item{family}{See \code{\link[stats]{glm}}, \code{\link[MASS]{polr}}, 
    \code{\link[lme4]{lmer}}for detail.}
  \item{rescale}{default is \code{FALSE}, see \code{\link[arm]{rescale}} for detail.}
  \item{x}{\code{mi.pooled} object.}
  \item{object}{\code{mi.pooled} object.}
  \item{digits}{number of significant digits to display, default=2.}
  \item{\dots}{ Any option to pass on to \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, 
    \code{\link[arm]{bayesglm}}, \code{\link[arm]{bayespolr}}, \code{\link[MASS]{polr}}, 
    and \code{\link[lme4]{lmer}} functions}
}

\value{
  \item{call}{the matched call.}
  \item{mi.pooled}{pulled estimates from the multiple dataset.}
  \item{mi.fit}{estimates from each dataset.}

}
\references{
    Andrew Gelman and Jennifer Hill, 
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}, 
    Cambridge University Press, 2007.
}

\seealso{
    \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, 
    \code{\link[arm]{bayesglm}}, \code{\link[arm]{bayespolr}}, \code{\link[MASS]{polr}}, 
    and \code{\link[lme4]{lmer}} 
}


\author{
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
}

\examples{
  # true data
  n <- 100
  x <- rbinom(n,1,.45) 
  z <- ordered(rep(seq(1, 5),n)[sample(1:n, n)])
  y <- rnorm(n)
  group <- rep(1:10, 10)

  # create artificial missingness
  dat.xy <- data.frame(x, z, y)
  dat.xy <- mi:::.create.missing(dat.xy, pct.mis=10)  

  # imputation 
  IMP <- mi(dat.xy, n.iter=6, add.noise=FALSE)

  # fit models
  M1 <- lm.mi(y ~ x + z, IMP)
  display(M1)
  coef(M1)
  se.coef(M1)

  M2 <- glm.mi(x ~ y , IMP, family = binomial(link="logit"))
  display(M2)
  coef(M2)
  se.coef(M2)

  M3 <- bayesglm.mi(x ~ y , IMP, family = binomial(link="logit"))
  display(M3)
  coef(M3)
  se.coef(M3)

  M4 <- polr.mi(ordered(z) ~  y, IMP)
  display(M4)
  coef(M4)
  se.coef(M4)

  M5 <- bayespolr.mi(ordered(z) ~  y, IMP)
  display(M5)
  coef(M5)
  se.coef(M5)

  M6 <- lmer.mi(y ~ x  + (1|group), IMP)
  display(M6)
  coef(M6)
  se.coef(M6)

  M7 <- glmer.mi(x ~ y  + (1|group), IMP, family = binomial(link="logit"))
  display(M7)
  coef(M7)
  se.coef(M7)  
}

\keyword{models}
\keyword{manip}

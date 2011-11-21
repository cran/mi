\name{mi.count}
\docType{class}
% Classes
\alias{mi.count-class}
% Function
\alias{mi.count}
% methods
\alias{resid,mi.count-method}
\alias{residuals,mi.count-method}
\title{
    Elementary function: Bayesian overdispersed poisson regression to impute a count variable.
}
\description{
    Imputes univariate missing data using bayesglm, an 
    R functions for generalized linear modeling with independent normal, t, or
    Cauchy prior distribution for the coefficients.
}
\usage{
mi.count(formula, data = NULL, start = NULL, n.iter = 100, 
  draw.from.beta = TRUE, missing.index = NULL, \dots)
\S4method{resid}{mi.count}(object, y)
\S4method{residuals}{mi.count}(object, y)
%\S4method{plot}{mi.count,ANY}( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots)
}

\arguments{
  \item{formula}{  an object of class \code{formula} (or one that can be coerced to that class): 
    a symbolic description of the model to be fitted.  See bayesglm \code{formula} for details. }
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
  \item{missing.index}{The index of missing units of the outcome variable}
  \item{\dots}{ Currently not used. }
  \item{object}{\code{mi.countr} object.}
%  \item{x}{ \code{mi.countr} object.}
  \item{y}{ Observed values.}
%  \item{main}{ main title of the plot. }
%  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }

}
\details{
    In bayesglm default the prior distribution is Cauchy with center 0 and scale 2.5 
    for all coefficients (except for the intercept, which has a prior scale of 10). 
    See also glm for other details.
}
\value{
    \item{model    }{A summary of the bayesian fitted model.}
    \item{expected }{The expected values estimated by the model.}
    \item{random   }{Vector of length n.mis of random predicted values predicted by using the binomial distribution.}
}
\references{
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).

    Andrew Gelman and Jennifer Hill, 
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}, 
    Cambridge University Press, 2007.
}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\note{ see also http://www.stat.columbia.edu/~gelman/standardize/ }

\examples{
  # true data
  x <- rnorm(100,0,1) 
  y <- rpois(100,40)  
  # create artificial missingness on y
  y [seq(1,100,10)] <- NA
  dat.xy <- data.frame(x,y)
  # imputation
  mi.count(y ~ x, data = dat.xy)
}
\seealso{
    \code{\link{mi.info}}, 
    \code{\link{mi.method}},
    \code{\link{mi}}
}
\keyword{models}
\keyword{aplot}
\keyword{dplot}
\keyword{hplot}

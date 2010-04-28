\name{mi.pmm}
\docType{class}
% Classes
\alias{mi.pmm-class}
% Function
\alias{mi.pmm}
\title{
    Elementary function: Predictive Mean Matching for imputation.
}
\description{
    Imputes univariate missing data using bayesglm and predictive mean matching.
}
\usage{
mi.pmm(formula, data = NULL, start = NULL, n.iter = 100, missing.index = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{  an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details. }
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{missing.index}{The index of missing units of the outcome variable}
  \item{\dots}{ Currently not used. }
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
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.

   Van Buuren, S. and Oudshoorn, C.G.M. (2000). Multivariate Imputation
     by Chained Equations:  MICE V1.0 User's manual. Report
     PG/VGZ/00.038, TNO Prevention and Health, Leiden.

     Rubin, D.B. (1987). Multiple imputation for nonresponse in
     surveys. New York: Wiley.

}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\note{ see also http://www.stat.columbia.edu/~gelman/standardize/ }
\seealso{
    \code{ \link{mi.info}}, 
    \code{\link{mi.method}},
    \code{ \link{mi}}
}
\keyword{ models }

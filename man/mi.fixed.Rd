\name{mi.fixed}
\docType{class}
% Classes
\alias{mi.fixed-class}
\alias{mi.copy-class}
% Function
\alias{mi.fixed}
\alias{mi.copy}
%\alias{plot.mi.copy}
\title{
    Elementary function: imputation of constant variable.
}
\description{
    Imputes univariate constant missing data.
}

\usage{
 mi.fixed( formula, data = NULL, missing.index = NULL, ... )
 mi.copy(Y, X, missing.index = NULL, ...)
% \S3method{plot}{mi.copy}( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots)
}

\arguments{
  \item{formula}{an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{missing.index}{The index of missing units of the outcome variable}
  \item{Y}{A variable that is collinear with X}.
  \item{X}{A variable that is colliear with Y}.
  \item{\dots}{ Currently not used}
%  \item{x}{ An object of class '"mi.copy"'.}
%  \item{y}{ Observed values.}
%  \item{main}{ main title of the plot. }
%  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }

}

\value{
    \item{model   }{A summary of the fitted model.}
    \item{expected}{The expected values estimated by the model.}
    \item{random  }{Vector of length n.mis of random predicted values predicted by using the normal distribution.} 
}
\references{
    Andrew Gelman and Jennifer Hill, 
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}, 
    Cambridge University Press, 2006.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{
    \code{ \link{mi.info}}, 
    \code{\link{mi.method}},
    \code{ \link{mi}}
}
\examples{
# fake data
n <- 100
x1 <- rbinom(n, 1, .45)
x2 <- 2*x1
x1[c(1, 3, 5, 20, 26)] <- NA

# impute data
mi.copy(x1, x2)

}
\keyword{models}
\keyword{aplot}
\keyword{dplot}
\keyword{hplot}

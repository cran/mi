\name{mi.sqrtcontinuous}
\docType{class}
% Classes
\alias{mi.sqrtcontinuous-class}
% Function
\alias{mi.sqrtcontinuous}
\alias{residuals.mi.sqrtcontinuous}
\alias{plot.mi.sqrtcontinuous}
\title{
    Elementary function: linear regression to impute a continuous positive variable.
}
\description{
    Imputes univariate missing data using linear regression on the square-root values.
}
\usage{
mi.sqrtcontinuous(formula, data = NULL, start = NULL, n.iter = 100, draw.from.beta = FALSE, ...)

\S3method{residuals}{mi.sqrtcontinuous}(object, y, \dots)
\S3method{plot}{mi.sqrtcontinuous}( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots)
}

\arguments{
  \item{formula}{an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
  \item{\dots}{ Currently not used. }
  \item{object}{\code{mi.sqrtcontinuous} object.}
  \item{x}{ \code{mi.sqrtcontinuous} object.}
  \item{y}{ Observed values.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }

}
\details{
    see lm
}
\value{
  
  \item{model   }{A summary of the fitted model}
  \item{expected}{The expected values estimated by the model}
  \item{random  }{Vector of length n.mis of random predicted values predicted by using the normal distribution} 
}

\references{
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
        
\note{ 
    The incomplete data vector has to be positive.
}
\seealso{
    \code{\link{mi.info}}, 
    \code{\link{mi.method}},
    \code{\link{mi}}
}
\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(4+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  # create artificial missingness on y
  y[seq(1,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  mi.sqrtcontinuous(y~x, data = dat.xy)
}
\keyword{ models }

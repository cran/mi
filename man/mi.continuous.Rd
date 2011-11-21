\name{mi.continuous}
\docType{class}
% Classes
\alias{mi.continuous-class}
% Function
\alias{mi.continuous}
\title{
    Elementary function: linear regression to impute a continuous variable.
}
\description{
    Imputes univariate missing data using linear regression.
}
\usage{
mi.continuous(formula, data = NULL, start = NULL, n.iter = 100, 
    draw.from.beta = TRUE, missing.index = NULL, \dots)
}

\arguments{
  \item{formula}{an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
  \item{missing.index}{The index of missing units of the outcome variable}
  \item{\dots}{ Currently not used. }
}

\details{
    see bayesglm
}

\value{
    \item{model   }{A summary of the fitted model.}
    \item{expected}{The expected values estimated by the model.}
    \item{random  }{Vector of length n.mis of random predicted values predicted by using the normal distribution.} 
}

\references{
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).


    Andrew Gelman and Jennifer Hill, 
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}, 
    Cambridge University Press, 2006.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  # create artificial missingness on y
  y[seq(1,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  mi.continuous(y~x, data = dat.xy)
}

\seealso{
    \code{\link{mi.info}}, 
    \code{\link{mi.method}},
    \code{\link{mi}}
}


\keyword{ models }

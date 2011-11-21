\name{mi.categorical}
\docType{class}
% Classes
\alias{mi.categorical-class}
% Function
\alias{mi.categorical}
\alias{resid,mi.categorical-method}
\alias{residuals,mi.categorical-method}
\alias{plot,mi.categorical-method}
\alias{plot,mi.categorical,ANY-method}

\title{ 
    Elementary function: multinomial log-linear models to impute a categorical variable.
}
\description{
    Imputes missing data in a categorical variable using multinomial Log-linear Models. 
}
\usage{
  mi.categorical( formula, data = NULL, n.iter = 100, MaxNWts = 1500, missing.index = NULL, \dots)
  \S4method{residuals}{mi.categorical}(object, y)
  \S4method{plot}{mi.categorical,ANY}( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots)
}
\arguments{
  \item{formula}{ a formula expression as for regression models, of the form
          \code{response ~ predictors}. The response should be a factor or a
          matrix with K columns, which will be interpreted as counts
          for each of K classes. A log-linear model is fitted, with
          coefficients zero for the first class. An offset can be
          included: it should be a numeric matrix with K columns if the
          response is either a matrix with K columns or a factor with K
          > 2 classes, or a numeric vector for a response factor with 2
          levels. See the documentation of \code{formula()} for other
          details.  }
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{n.iter}{ Maximum number of iteration. }
  \item{MaxNWts}{ The maximum allowable number of weights. See nnet for detail. }
  \item{missing.index}{The index of missing units of the outcome variable}
  \item{object}{ \code{mi.categorical} object.}
  \item{x}{ \code{mi.categorical} object.}
  \item{y}{ Observed values.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }
  \item{\dots}{Currently not used.}

}
\details{
    \code{multinom} calls the library \pkg{nnet}. See \code{multinom} for other details.   
}
\value{
    \item{model   }{A summary of the multinomial fitted model.}
    \item{expected}{The expected values estimated by the model.}
    \item{random  }{Vector of length n.mis of random predicted values predicted by using the multinomial distribution.}
}
\references{
    Andrew Gelman and Jennifer Hill, 
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}, 
    Cambridge University Press, 2007.

  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).

}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\seealso{
    \code{\link{mi.info}}, 
    \code{\link{mi.method}},
    \code{\link{mi}}
}
\examples{
  x <-rnorm(100,0,1)
  y <- x+4
  y <- round(y)
  y[y<0] <- 0
  # create artificial missingness on y
  y[seq(1,100,10)] <- NA
  dat.xy <- data.frame(x,y)
  mi.categorical(formula = y ~ x, data = dat.xy)
}
\keyword{models}
\keyword{aplot}
\keyword{dplot}
\keyword{hplot}

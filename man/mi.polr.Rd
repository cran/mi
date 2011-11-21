\name{mi.polr}
\docType{class}
% Classes
\alias{mi.polr-class}
% Function
\alias{mi.polr}
\alias{resid,mi.polr-method}
\alias{residuals,mi.polr-method}
\alias{plot,mi.polr-method}
\alias{plot,mi.polr,ANY-method}
\title{
    Elementary function: multinomial log-linear models to impute a ordered categorical variable.
}
\description{
    Imputes missing data in a categorical variable using multinomial Log-linear Models. 
}
\usage{
  mi.polr(formula, data = NULL, drop.unused.levels = TRUE, start = NULL, 
    n.iter = 100, missing.index = NULL, \dots)
  \S4method{residuals}{mi.polr}(object, y)
  \S4method{plot}{mi.polr,ANY}( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots )
}

\arguments{
  \item{formula}{ a formula expression as for regression models, of the form
          \code{response ~ predictors}. The response should be a factor
          (preferably an ordered factor), which will be interpreted as
          an ordinal response, with levels ordered as in the factor.  
          The model must have an intercept: attempts to remove one will
          lead to a warning and be ignored.  An offset may be used. 
          See the documentation of 'formula' for other details. }
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{drop.unused.levels}{ Drops unused levels. }
  \item{start}{ Starting value for bayespolr. }
  \item{n.iter}{ Maximum number of iteration for bayespolr. The default is 100.}
  \item{missing.index}{The index of missing units of the outcome variable.}
  \item{\dots}{ Currently not used. }
  \item{object}{ \code{mi.polr} object.}
  \item{x}{\code{mi.polr} object.}
  \item{y}{ Observed values.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }

}
\details{
    \code{multinom} calls the library \pkg{nnet}. See multinom for other details.   
}
\value{
    \item{model    }{A summary of the multinomial fitted model}
    \item{expected }{The expected values estimated by the model}
    \item{random   }{Vector of length n.mis of random predicted values predicted by using the multinomial distribution}
    \item{residual }{The residual vector of length same as y}
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
  Yu-Sung Su \email{ys463@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\seealso{
    \code{\link{mi.info}},
    \code{\link{mi.method}},
    \code{\link{mi}}
    
}
\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-(1+2*x)+rnorm(100,0,1)
  y<-round(y)
  y[y<0]<-0
  # create artificial missingness on y
  y[seq(1,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  polr.imp <- mi.polr(y~x, data = dat.xy)
}

\keyword{ models }

\name{mi.mixed}
\docType{class}
% Classes
\alias{mi.mixed-class}
% Function
\alias{mi.mixed}
\alias{coef.mi.mixed}
\alias{imputed,mi.mixed-method}
\alias{sigma.hat,mi.mixed-method}
\alias{fitted.mi.mixed}
\alias{residuals.mi.mixed}
\alias{plot.mi.mixed}
\alias{print,mi.mixed-method}
\title{
    Two-stage elementary function: linear regression to impute a variable that can be positive or zero. }
\description{
    Imputes univariate missing data using a two stage imputation process. 
    The first regression model is fit to all the data for which the variable is observed, 
    and the second model is fit to all the data for which the variable is observed and positive.
}
\usage{
  mi.mixed(formula, data = NULL, start = NULL, n.iter = 100, draw.from.beta = FALSE, \dots)

  \S3method{coef}{mi.mixed}(object, \dots)
  \S4method{sigma.hat}{mi.mixed}(object, \dots)
  \S3method{fitted}{mi.mixed}(object, \dots)
  \S3method{residuals}{mi.mixed}(object, y, \dots)
  \S4method{imputed}{mi.mixed}(object, y)
  \S3method{plot}{mi.mixed}(x, y, main=deparse( substitute( y ) ), gray.scale = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
  \item{\dots}{ Currently not used. }
  \item{object}{\code{mi.mixed} object.}
  \item{x}{\code{mi.mixed} object.}
  \item{y}{ Observed values.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }

}
\details{
    Imputation variable that can be positive or zero. 
    See Gelman and Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, (2206), Chapter 25. 
}
\value{
    \item{model$model.1}{A summary of the fitted model for the all the data for which y is observed.}
    \item{model$model.2}{A summary of the fitted model for the all the data for which y is observed and positive.}
    \item{expected}{The expected values estimated by the model}
    \item{random }{Vector of length n.mis of random predicted values to impute the missing data. 
                    Whether missing values are positive random is positive, otherwise is equal to zero.}
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
\seealso{
    \code{\link{mi.info}},  
    \code{\link{mi.method}},
    \code{\link{mi}}
}
\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  y[y<0]<-0
  # create artificial missingness on y
  y[seq(1,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  mi.mixed(list("1*(y>0)~x","y~x"), data = dat.xy)
}
\keyword{ models }

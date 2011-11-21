\name{mi.scatterplot}
\alias{mi.scatterplot}
\alias{marginal.scatterplot}
\title{ Multiple Imputation Scatterplot }
\description{
  A function for plotting observed and imputed values for a variable . 
}
\usage{
mi.scatterplot( Yobs, Yimp, X = NULL, xlab = NULL, ylab = NULL, 
                            main = "Imputed Variable Scatter Plot", 
                             display.zero = TRUE, gray.scale = FALSE, 
                              obs.col = rgb( 0, 0, 1 ), 
                              imp.col = rgb( 1, 0, 0 ), 
                              obs.pch = 20 , imp.pch = 20, 
                              obs.cex = 0.3, imp.cex = 0.3, 
                              obs.lty = 1  , imp.lty = 1, 
                              obs.lwd = 2.5, imp.lwd = 2.5, ... )
marginal.scatterplot ( data, object, use.imputed.X = FALSE, ...  )
} 
\arguments{ 
  \item{Yobs}{ observed values. }
  \item{Yimp}{ imputed values. }
  \item{X}{ variable to plot on the x axis. }
  \item{xlab}{ label on the x axis. }
  \item{ylab}{ label on the y axis. }
  \item{display.zero}{ if set to FALSE zeros will not be displayed. Default is TRUE.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }
  \item{obs.col}{ color for the observed variable.  Default is "blue". }
  \item{imp.col}{ color for the imputed variable.  Default is "red". }
  \item{obs.pch}{ data symbol for observed variable.  Default is 20. }
  \item{imp.pch}{ data symbol for imputed variable.  Default is 20. }
  \item{obs.cex}{ text size for observed variable. Default is 0.3. }
  \item{imp.cex}{ text size for imputed variable. Default is 0.3. }
  \item{obs.lty}{ line type for observed variable. Default is 1. }
  \item{imp.lty}{ line type for imputed variable. Default is 1. }
  \item{obs.lwd}{ line width for observed variable. Default is 2.5. }
  \item{imp.lwd}{ line width for imputed variable. Default is 2.5. }
  \item{\dots}{ Other options for 'plot' function. }
  \item{data}{ missing data. }
  \item{object}{ \code{mi} object. }
  \item{use.imputed.X}{ If you want to use the imputed X.  Default is FALSE. }
}
\details{
 Since several data points can have the same data values, especially in discrete variables,
 small random number is added to each value so that points do not fall on top of each other. 
 See help on \code{jitter} for more details.  
 Lowess line is fitted to both imputed and observed data.
}
\value{
  A scatterplot with the observed and the imputed values plotted against a chosen variable. 
}
\references{
   Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).

    Kobi Abayomi, Andrew Gelman and Marc Levy. (2008).
    \dQuote{Diagnostics for multivariate imputations}.
    \emph{Applied Statistics} 57, Part 3: 273--291.

    Andrew Gelman and Jennifer Hill. (2007).
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}.
    Cambridge University Press.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}}
\note{ 
  By default imputed values are in red, while the observed values are in blue.
} 
\seealso{ 
  \code{\link{mi}},
  \code{\link{plot}} 
} 
\examples{ 
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  # create artificial missingness on y
  y[seq(1,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  imp.cont<-mi.continuous(y~x, data = dat.xy)
  mi.scatterplot(y,imputed(imp.cont,y))
}
\keyword{ hplot }

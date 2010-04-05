\name{missing.pattern.plot}
\alias{missing.pattern.plot}
\alias{mp.plot}

\title{ Missing Pattern Plot }
\description{
  Function to plot a missing pattern plot.
}
\usage{
missing.pattern.plot ( data, y.order = FALSE, x.order = FALSE, clustered = TRUE, 
                                    xlab = "Index", ylab = "Variable", 
                                    main = NULL, gray.scale = FALSE,
                                    obs.col = "blue", mis.col = "red", ... )
}
\arguments{
  \item{data}{ data.frame or matrix of data with missing data coded as "NA". }
  \item{y.order}{ if TRUE, orders the variable by number of missing value.  Default is FALSE.}
  \item{x.order}{ if TRUE, orders the data by number of missing value.  Default is FALSE. }
  \item{clustered}{if TRUE, data are grouped together with similiar missingness patterns.}
  \item{xlab}{ a title for the x axis: see 'title'. }
  \item{ylab}{ a title for the y axis: see 'title'. }
  \item{main}{ an overall title for the plot: see 'title'. }
  \item{gray.scale}{ if TRUE, makes the plot into black and white.  
                     This option overwrites the color specification. }
  \item{obs.col}{ color used for observed values.  Default is "blue". }
  \item{mis.col}{ color used for missing values.  Default is "red".  }
  \item{\dots}{ additional parameters passed to 'image' function. }
}
\details{
  Color image with different color for missing and observed value in the dataset is plotted.  
  By default the observed is in "blue" and missing is in "red".
}
\value{
  Plot to visualize pattern of missingness in the data.  
}
\references{
    Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. \emph{Forthcoming}.
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software}.

    Kobi Abayomi, Andrew Gelman and Marc Levy. (2008).
    \dQuote{Diagnostics for multivariate imputations}.
    \emph{Applied Statistics} 57, Part 3: 273--291.

    Andrew Gelman and Jennifer Hill. (2007).
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}.
    Cambridge University Press.
}
\seealso{
  \code{\link{mi}}, 
  \code{\link{plot}}
}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\examples{
  data(CHAIN)
  missing.pattern.plot(CHAIN)
}
\keyword{ hplot }

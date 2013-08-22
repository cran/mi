\name{mi.info}
\docType{class}
\alias{mi.info-class}
\alias{mi.info}
\alias{mi.info.fix}
\alias{is.mi.info}
\alias{mi.interactive}
% methods
\alias{show,mi.info-method}
\alias{print,mi.info-method}

\title{ Function to create information matrix for missing data imputation }
\description{
  Produces matrix of information needed to impute the missing data.  
  After the information is extracted user has the option of changing the default.
}
\usage{
  mi.info(data, threshold  = 0.99999)
  \S4method{print}{mi.info}(x, \dots)
  \S4method{show}{mi.info}(object)
}
\arguments{
  \item{data}{ dataframe or matrix of dataset with missing data coded as NAs. }
  \item{threshold}{Threshold value for correlation to be considered a problem.}
  \item{x}{ An object of a class \code{mi.info}.}
  \item{object}{ An object of a class \code{mi.info}.}
  \item{\dots}{ Currently not used.}
}
\value{
  \item{info}{information matrix
   \tabular{llll}{
     \tab \tab -name: \tab Name of variable\cr
     \tab \tab -imp.order: \tab Imputation Order\cr
     \tab \tab -nmis: \tab Number of missing\cr
     \tab \tab -type: \tab Type of variable\cr
     \tab \tab -var.class: \tab Class of input variable\cr
     \tab \tab -level: \tab Levels in the input varialbe\cr
     \tab \tab -include: \tab Include in the imputation process or not\cr
     \tab \tab -is.ID: \tab Is ID variable or not\cr
     \tab \tab -all.missing: \tab All observation missing or not\cr
     \tab \tab -collinear: \tab Collineared variables\cr
     \tab \tab -determ.pred: \tab Deterministic predictor\cr
     \tab \tab -imp.formula: \tab Imputation formula\cr
     \tab \tab -params: \tab Parameters for the imputation model\cr
     \tab \tab -other: \tab Currently not used\cr
   }
  }
}

\references{
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{ 
  \code{\link{mi}} 
}
\examples{
  data(CHAIN)
  info.CHAIN <- mi.info(CHAIN)
  
  info.CHAIN$imp.order # imputation order
  
  info.CHAIN$imp.formula # imputation formula
  info.CHAIN[["age.W1"]]$imp.formula  #imputation formula for specific variable
}
\keyword{ models }

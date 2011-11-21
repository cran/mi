\name{write.mi}
\alias{write.mi}
\title{
Writes mi impuations to file
}

\description{
Writes the imputed datasets to file for the \code{mi} object 
into the \code{csv}, \code{dta}, and \code{table} format.
}

\usage{
write.mi(object, format = c("csv", "dta", "table"), \dots)
}

\arguments{
  \item{object}{\code{mi} object}
  \item{format}{output format, only \dQuote{csv}, \dQuote{dta} and \dQuote{table} format are supported.}
  \item{\dots}{further arguments for \code{write} functions}
}
\details{
\code{write.mi} write each imputed dataset to a file in one of the three formats: \code{csv}, 
\code{dta} and \code{table}, using \code{write.csv}, \code{write.dta} and \code{write.table}
repectively.

The output files should be

 \preformatted{
    midata1.csv
    midata2.csv
    omidata3.csv
    ...
  }
}


\author{
  Yu-Sung Su      \email{suyusung@tsinghua.edu.cn},
}

\references{ 
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
}


\seealso{
\code{\link{write.csv}}, \code{\link{write.table}}, \code{\link{write.dta}}
}


\examples{
# data(CHAIN)
# IMP <- mi(CHAIN)
# write.mi(IMP)
}
\keyword{manip}

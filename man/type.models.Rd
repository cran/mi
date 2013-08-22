\name{type.models}
\alias{type.models}
\alias{mi.types}

\title{Functions to identify types of the models of the mi object}
\description{
The function to select the model based on the variable type
}
\usage{
type.models(type)
mi.types()

}

\arguments{
  \item{type}{Nine types: \code{continuous}, 
        \code{log-continuous},
        \code{count},
        \code{ordered-categorical}, 
        \code{unordered-categorical}, 
        \code{binary},
        \code{positive-continuous},
        \code{proportion},
        \code{predictive-mean-matching},
        \code{nonnegative},
        \code{fixed}}
}

\author{
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\references{
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
}


\seealso{\code{\link{mi.info}}}

\keyword{manip}

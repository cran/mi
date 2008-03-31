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
        \code{count},
        \code{ordered-categorical}, 
        \code{unordered-categorical}, 
        \code{dichotomous},
        \code{positive-continuous},
        \code{proportion},
        \code{predictive-mean-matching},
        \code{mixed},
        \code{fixed}}
}

\author{
  Yu-Sung Su \email{ys463@columbia.edu},
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}


\seealso{\code{\link{mi.info}}}

\keyword{manip}

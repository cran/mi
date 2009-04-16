\name{prior.control}
\alias{prior.control}

\title{Auxiliary for Adding Priors to Missing Data Imputation}
\description{
  Auxiliary function as user interface for adding priors for \code{mi} procedure.
  Typically only used when calling \code{mi}.
}

\usage{
prior.control(method=c("reshuffling", "fading"), pct.aug=10, K=1)

}
\arguments{
  \item{method}{two methods are implemented: \code{reshuffling} and \code{fading}.}  
  \item{pct.aug}{percent of N being add into the exisitng data, where N is the number of 
    observation of the completed data set, default is 10.}  
  \item{K}{the cooling parameter, default is 1.}
}

\details{
If \code{reshuffling} method is used, the imputation will randomly switch between 
randomly imputing data from marginal (imputing data from the observed values)
and drawing from the modeled based values. And with each iteration (s), the probability of cooling (q) decreases by number of iterations, 
such that \code{q = K/s}.

If \code{fading} method is used, the imputation will augment 
}


\author{
  Yu-Sung Su \email{yajima@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi}} 
}

\examples{
### NOT RUN
#===========================================================
# add fadding empirical priors by augmenting 10% of the data
#===========================================================
if(0){
 data(CHAIN)
 IMP <- mi(CHAIN, add.priors=prior.control(method="fading", pct.aug=10))
#=================================================
# add priors by randomly drawing from the marginal
#=================================================
## This is the default setting
 IMP <- mi(CHAIN, add.priors=prior.control(method="reshuffling", K=1))
#=================================================
# use no prior
#=================================================
 IMP <- mi(CHAIN, add.priors=FALSE)
}
}

\keyword{manip}

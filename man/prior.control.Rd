\name{prior.control}
\alias{prior.control}

\title{Auxiliary for Adding Priors to Missing Data Imputation}
\description{
  Auxiliary function as user interface for adding priors for \code{mi} procedure.
  Typically only used when calling \code{mi}.
}

\usage{
prior.control(augment.data=FALSE, pct.aug=10, K=1)

}
\arguments{
  \item{augment.data}{add the extra rows of data with \code{pct.aug} of the 
    random draws from the observed data, default= \code{FALSE}}  
  \item{pct.aug}{percent of N being add into the exisitng data, where N is the number of 
    observation of the completed data set, default is 10.}  
  \item{K}{the cooling parameter, default is 1. 
        If K > 0, the imputation will randomly switch between 
        randomly imputing data from marginal (imputing data from the observed values)
        and drawing from the modeled based values. 
        And with each iteration (s), the probability of cooling (q) decreases by number of iterations, 
        such that q = K/s.}
}

\details{
If both \code{augment.data=TRUE} and \code{K>0} are specified, it will switch
\code{augment.data=FALSE} and use only \code{K>0}.

The default \code{\link{mi}} process uses \code{K=1}.  In genereal, add priors
to the \code{\link{mi}} process will speed the convergence of the imputation.
}


\author{
  Yu-Sung Su \email{yajima@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi}} 
}

\examples{
### NOT RUN
#=================================================
# add priors by augmenting 10% of the data
#=================================================
if(0){
 data(CHAIN)
 IMP <- mi(CHAIN, add.priors=prior.control(augment.data=TRUE, pct.aug=10))
#=================================================
# add priors by randomly drawing from the marginal
#=================================================
## This is the default setting
 IMP <- mi(CHAIN, add.priors=prior.control(K=1))
#=================================================
# use no prior
#=================================================
 IMP <- mi(CHAIN, add.priors=prior.control(K=0))
}
}

\keyword{manip}

\name{mi.info.update}
\alias{mi.info.update}
\alias{mi.info.update.type}
\alias{mi.info.update.level}
\alias{mi.info.update.include}
\alias{mi.info.update.is.ID}
\alias{mi.info.update.correlated}
\alias{mi.info.update.transform}
\alias{mi.info.update.imp.order}
\alias{mi.info.update.determ.pred}
\alias{mi.info.update.params}
\alias{mi.info.update.imp.formula}
\alias{mi.info.update.other}
\title{ function to update mi.info object to use for multiple imputation }
\description{
  This function is internal function to update the mi.info object.
}
\usage{
mi.info.update(info, target, list)
mi.info.update.type(info, list)
mi.info.update.level(info, list)
mi.info.update.include(info, list)
mi.info.update.is.ID(info, list)
mi.info.update.correlated(info, list)
mi.info.update.transform(info, list)
mi.info.update.imp.order(info, list)
mi.info.update.determ.pred(info, list)
mi.info.update.params(info, list)
mi.info.update.imp.formula(info, list)
mi.info.update.other(info, list)
}
\arguments{
  \item{info}{ mi.info object that is result of mi.info function. }
  \item{target}{ which part of mi.info object to modify. }
  \item{list}{ list that has same length as the number of variables in the mi.info object.  Element which are left NULL will not be updated .}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{info }{updated mi.info object.}
}
\references{
    Andrew Gelman and M. Grazia Pittau,  
    A flexible program for missing-data imputation and model checking, 
    Technical report, Columbia University, New York; Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi.matrix}} 
}
\examples{
data(CHAIN)
CHAIN.info<-mi.info(CHAIN)
CHAIN.info<-mi.info.update.type(CHAIN.info, list(age.W1="continuous", b05.W1="unordered-categorical"))
}
\keyword{ models }

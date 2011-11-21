\name{mi.info.update}
\alias{update.mi.info}
\alias{mi.info.update}
\alias{mi.info.update.type}
\alias{mi.info.update.level}
\alias{mi.info.update.include}
\alias{mi.info.update.is.ID}
\alias{mi.info.update.collinear}
%\alias{mi.info.update.transform}
\alias{mi.info.update.imp.order}
\alias{mi.info.update.determ.pred}
\alias{mi.info.update.params}
\alias{mi.info.update.imp.formula}
\alias{mi.info.update.other}
\title{ function to update mi.info object to use for multiple imputation }
\description{
  This function is internal function to update the \code{mi.info} object.
}
\usage{
\method{update}{mi.info}(object, target, list, \dots)
mi.info.update.type(object, list)
mi.info.update.level(object, list)
mi.info.update.include(object, list)
mi.info.update.is.ID(object, list)
mi.info.update.collinear(object, list)
%mi.info.update.transform(object, list)
mi.info.update.imp.order(object, list)
mi.info.update.determ.pred(object, list)
mi.info.update.params(object, list)
mi.info.update.imp.formula(object, list)
mi.info.update.other(object, list)
}

\arguments{
  \item{object}{\code{mi.info} object that is result of \code{mi.info} function. }
  \item{target}{which part of \code{mi.info} object to modify. }
  \item{list}{list that has same length as the number of variables in the mi.info object.  Element which are left NULL will not be updated .}
  \item{\dots}{currently no function.}
}

\value{
  \item{info}{updated \code{mi.info} object.}
}

\references{ 
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
  
    Andrew Gelman and Jennifer Hill. (2007).
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}.
    Cambridge University Press.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi.completed}} 
}
\examples{
data(CHAIN)
CHAIN.info <- mi.info(CHAIN)

# You can update the mi.info object in the below two ways
CHAIN.info <- update(CHAIN.info, "type", 
  list(age.W1="continuous", b05.W1="unordered-categorical"))
CHAIN.info <- mi.info.update.type(CHAIN.info, 
  list(age.W1="continuous", b05.W1="unordered-categorical"))
}


\keyword{methods}
\keyword{manip}

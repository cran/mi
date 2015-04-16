\name{fit_model}
\docType{methods}
\alias{fit_model}
\alias{fit_model-methods}
\title{Wrappers To Fit a Model}
\description{
The methods are called by the \code{\link{mi}} function to model a given
\code{\link{missing_variable}} as a function of all the other 
\code{\link{missing_variable}}s and also their missingness pattern.
By overwriting these methods, users can change the way a 
\code{\link{missing_variable}} is modeled for the purposes of imputing
its missing values. See also the table in \code{\link{missing_variable}}.
}
\usage{
fit_model(y, data, ...)
}
\arguments{
  \item{y}{An object that inherits from \code{\link{missing_variable-class}} or
     missing
}
  \item{data}{A \code{\link{missing_data.frame}}

}
  \item{\dots}{Additional arguments, not currently utilized
}
}
\details{
In \code{\link{mi}}, each \code{\link{missing_variable}} is modeled as a function of 
all the other \code{\link{missing_variable}}s plus their missingness pattern. The
\code{fit_model} methods are typically short wrappers around a statistical model fitting
function and return the estimated model. The model is then passed to one of the 
\code{\link{mi-methods}} to impute the missing values of that \code{\link{missing_variable}}.

Users can easily overwrite these methods to estimate a different model, such as wrapping
\code{\link{glm}} instead of \code{\link[arm]{bayesglm}}. See the source code for examples,
but the basic outline is to first extract the \code{X} slot of the 
\code{\link{missing_data.frame}}, then drop some of its columns using the \code{index} slot
of the \code{\link{missing_data.frame}}, next pass the result along with the \code{data} slot
of \code{y} to a statistical fitting function, and finally returned the appropriately classed
result (along with the subset of \code{X} used in the model).

Many of the optional arguments to a statistical fitting function can be specified using the
slots of \code{y} (e.g. its \code{family} slot) or the slots of \bold{data} (e.g. its 
\code{weights} slot).

The exception is the method where \code{y} is missing, which is used internally by 
\code{\link{mi}}, and should \emph{not} be overwritten unless great care is taken to understand
its role.
}
\value{
If \code{y} is missing, then the modified \code{\link{missing_data.frame}} passed to
\code{data} is returned. Otherwise, the estimated model is returned as a classed
list object.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_variable}}, \code{\link{mi}}, \code{\link{get_parameters}}
}
\examples{
getMethod("fit_model", signature(y = "binary", data = "missing_data.frame"))
setMethod("fit_model", signature(y = "binary", data = "missing_data.frame"), def =
function(y, data, ...) {
  to_drop <- data@index[[y@variable_name]]
  X <- data@X[, -to_drop]
  start <- NULL
  # using glm.fit() instead of bayesglm.fit()
  out <- glm.fit(X, y@data, weights = data@weights[[y@variable_name]], start = start, 
                 family = y@family, Warning = FALSE, ...)
  out$x <- X
  class(out) <- c("glm", "lm") # not "bayesglm" class anymore
  return(out)
})
\dontrun{
if(!exists("imputations", env = .GlobalEnv)) {
  imputations <- mi:::imputations # cached from example("mi-package")
}
imputations <- mi(imputations) # will use new fit_model() method for binary variables
}
}
\keyword{regression}
\keyword{DirectedTowardDevelopeRs}

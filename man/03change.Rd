\name{03change}
\docType{methods}
\alias{03change}
\alias{change}
\alias{change-methods}
\alias{change_family}
\alias{change_imputation_method}
\alias{change_link}
\alias{change_model}
\alias{change_size}
\alias{change_transformation}
\alias{change_type}
\title{Make Changes to Discretionary Characteristics of Missing Variables}
\description{
These methods change the family, imputation method, size, type, and
so forth of a \code{\link{missing_variable}}. They are typically
called immediately before calling \code{\link{mi}} because they
affect how the conditional expectation of each \code{\link{missing_variable}}
is modeled.
}
\usage{
change(data, y, to, what, ...)
change_family(data, y, to, ...)
change_imputation_method(data, y, to, ...)
change_link(data, y, to, ...)
change_model(data, y, to, ...)
change_size(data, y, to, ...)
change_transformation(data, y, to, ...)
change_type(data, y, to, ...)
}
\arguments{
  \item{data}{A \code{\link{missing_data.frame}} (typically) but can be missing for all but
     the \code{change} function
}
  \item{y}{A character vector (typically) naming one or more \code{\link{missing_variable}}s
    within the \code{\link{missing_data.frame}} specified by the \bold{data} argument. 
    Alternatively, \bold{y} can be the name of a class that inherits from 
    \code{\link{missing_variable}}, in which case all \code{\link{missing_variable}}s of
    that class within \code{data} will be changed. Can also be an vector of integers or a 
    logical vector indicating which \code{\link{missing_variable}}s to change.
}
  \item{what}{Typically a character string naming what is to be changed, such
     as \code{"family"}, \code{"imputation_method"}, \code{"size"}, \code{"transformation"}, 
     \code{"type"}, \code{"link"}, or \code{"model"}. Alternatively, it can be a scalar value, 
     in which case all occurances of that value for the variable indicated by \code{y} will be changed to
     the value indicated by \code{to}
}
  \item{to}{Typically a character string naming what \code{y} should be changed to, 
     such as one of the admissible families, imputation methods, transformations, or types.
     If missing, then possible choices for the \code{to} argument will be helpfully printed
     on the screen. If \code{what} is a number, then \code{to} should be the number (or \code{NA})
     that the value designated by \code{what} will be recoded to. See the Details section for more information.
}
  \item{\dots}{Other arguments, not currently utilized}
}
\details{
In order to run \code{\link{mi}} correctly, data must first be specified to be ready for multiple imputation using the \code{\link{missing_data.frame}} function.  For each variable, \code{missing_data.frame} will record information required by \code{mi}: the variable's type, distribution family, and link function; whether a variable should be standardized or tranformed by a log function or square root; what specific model to use for the conditional distribution of the variable in the \code{mi} algorithm and how to draw imputed values from this model; and whether additional rows (for the purposes of prediction) are required. \code{missing_data.frame} will attempt to guess the correct type, family, and link for each variable based on its class in a regular \code{data.frame}.  These guesses can be checked with \code{show} and adjusted if necessary with \code{change}.  Any further additions to the model in regards to variable transformations, custom conditional models, or extra non-observed predictive cases must be specified with \code{change} before \code{mi} is run.

In general, most users will only use the \code{change} command.  \code{change} will then call \code{change_family}, \code{change_imputation_method}, \code{change_link}, \code{change_model}, \code{change_size}, \code{change_transformation}, or \code{change_type} depending on what characteristic is specified with the \code{what} option. The other change_* functions can be called directly but are primarily intended to be called indirectly by the change function.

\describe{
\item{\code{what = "type"}}{Change the subclass of variable(s) \code{y}.  \code{to} should be a character vector whose elements are subclasses of the \code{\link{missing_variable-class}} and are documented further there. Among the most commonly used subclasses are \code{"unordered-categorical"}, \code{"ordered-categorical"}, \code{"binary"}, \code{"interval"}, \code{"continuous"}, \code{"count"}, and \code{"irrelevant"}.}

\item{\code{what = "family"}}{Change the distribution family for variable(s) \code{y}. \code{to} must be of class \code{\link{family}} or a list where each element is of class \code{\link{family}}. If a variable is of \code{\link{binary-class}}, then the family must be \code{\link{binomial}} (the default) or possibly \code{\link{quasibinomial}}. If a variable is of \code{\link{ordered-categorical-class}} or \code{\link{unordered-categorical-class}}, use the \code{\link{multinomial}} family. If a variable is of \code{\link{count-class}}, then the family must be \code{\link{quasipoisson}} (the default) or \code{\link{poisson}}. If a variable is continuous, there are more choices for its family, but \code{\link{gaussian}} is the default and the others are not supported yet.}

\item{\code{what = "link"}}{Change the link function for variable(s) \code{y}. \code{to} can be any of the supported link functions for the existing \bold{family}. See \code{\link{family}} for details; however, not all of these link functions have appropriate \code{\link{fit_model}} and \code{\link{mi-methods}} yet.}

\item{\code{what = "model"}}{Change the conditional model for variable \code{y}. It usually is not necessary to change the model, since it is actually determined by the class, family, and link function of the variable.  This option can be used, however, to employ models that are not among those listed above.\code{to} should be a character vector of length one indicating what model should be used during the imputation process. Valid choices for binary variables include \code{"logit"}, \code{"probit"} \code{"cauchit"}, \code{"cloglog"}, or quasilikelihoods \code{"qlogit"}, \code{"qprobit"}, \code{"qcauchit"}, \code{"qcloglog"}.  For ordinal variables, valid choices include \code{"ologit"}, \code{"oprobit"}, \code{"ocauchit"}, and \code{"ocloglog"}.  For count variables, valid choices include \code{"qpoisson"} and \code{"poisson"}. Currently the only valid option for gaussian variables is \code{"linear"}. To change the model for unordered-categorical variables, see the estimator slot in \code{\link{missing_variable}}.}

\item{\code{what = "imputation_method"}}{Change the method for drawing imputed values from the conditional model specified for variable(s) \code{y}. \code{to} should be a character vector of length one or of the same length as \code{y} naming one of the following imputation methods: \code{"ppd"} (posterior predictive distribution), \code{"pmm"} (predictive mean matching), \code{"mean"} (mean imputation), \code{"median"} (median imputation), \code{"expectation"} (conditional expectation imputation).}

\item{\code{what = "size"}}{Optionally add additional rows for the purposes of prediction.  \code{to} should be a single integer. If \code{to} is non-negative but less than the number of rows in the \code{\link{missing_data.frame}} given by the \code{data} argument, then \code{\link{missing_data.frame}} is augmented with \code{to} more rows, where all the additional observations are missing. If \code{to} is greater than the number of rows in the \code{\link{missing_data.frame}}given by the \code{data} argument, then the \code{\link{missing_data.frame}} is extended to have \code{to} rows, where the observations in the surplus rows are missing. If \code{to} is negative, then any additional rows in the \code{\link{missing_data.frame}} given by the \code{data} argument are removed to restore it to its original size.}

\item{\code{what = "transformation"}}{Specify a particular transformation to be applied to variable(s) \code{y}. \code{to} should be a character vector of length one or of
the same length as \code{y} indicating what transformation function to use. Valid choices are \code{"identity"} for no transformation, \code{"standardize"} for standardization (using twice the standard deviation of the observed values), \code{"log"} for natural logarithm transformation, \code{"logshift"} for a \code{log(y + a)} transformation where \code{a} is a small constant, or \code{"sqrt"} for square-root transformation. Changing the transformation will also change the inverse transformation in the appropriate way. Any other value of \code{to} will produce an informative error message indicating that the transformation and inverse transformation need to be changed manually.}

\item{what = a value}{Finally, if both \code{what} and \code{to} are values then the former is recoded to the latter for all
occurances within the missing variable indicated by \code{y}.}
}
}
\value{
If the \bold{data} argument is not missing, then the method returns this argument with the 
specified changes. If \bold{data} is missing, then the method returns an object that inherits 
from the \code{\link{missing_variable-class}} with the specified changes.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_variable}}, \code{\link{missing_data.frame}}
}
\examples{
# STEP 0: GET DATA
data(nlsyV, package = "mi")

# STEP 1: CONVERT IT TO A missing_data.frame
mdf <- missing_data.frame(nlsyV)
show(mdf)

# STEP 2: CHANGE WHATEVER IS WRONG WITH IT
mdf <- change(mdf, y = "momrace", what = "type", to = "un")
mdf <- change(mdf, y = "income", what = "imputation_method", to = "pmm")
mdf <- change(mdf, y = "binary", what = "family", to = binomial(link = "probit"))
mdf <- change(mdf, y = 5, what = "transformation", to = "identity")
show(mdf)
}
\keyword{manip}
\keyword{AimedAtUseRs}

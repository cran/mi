\name{04mi}
\Rdversion{1.1}
\docType{class}
\alias{04mi}
\alias{mi}
\alias{mi-class}
\alias{mi-methods}
\title{Multiple Imputation
}
\description{
The \code{mi} function cannot be run in isolation.  It is the most important step of a multi-step process to perform multiple imputation.  The data must be specified as a \code{\link{missing_data.frame}} before \code{mi} is used to impute missing values for one or more \code{\link{missing_variable}}s. An iterative algorithm is used where each \code{\link{missing_variable}} is modeled (using \code{\link{fit_model}}) as a function of all the other \code{\link{missing_variable}}s and their missingness patterns.  This documentation outlines the technical uses of the \code{mi} function.  For a more general discussion of how to use \code{mi} for multiple imputation, see \code{\link{mi-package}}. 
}
\usage{
mi(y, model, ...)
## Hidden arguments:
## n.iter = 30, n.chains = 4, max.minutes = Inf, seed = NA, verbose = TRUE,
## save_models = FALSE, parallel = .Platform$OS.type != "windows"
}
\arguments{
  \item{y}{Typically an object that inherits from the \code{\link{missing_data.frame-class}},
    although many methods are defined for subclasses of the \code{\link{missing_variable-class}}.
    Alternatively, \code{y = "parallel"} the appropriate parallel backend will be
    registered but no imputation performed. See the Details section.
}
  \item{model}{Missing when \code{y = "parallel"} or when \code{y} inherits from the 
    \code{\link{missing_data.frame-class}} but otherwise should be the result of a call to 
    \code{\link{fit_model}}.
}
  \item{\dots}{Further arguments, the most important of which are
  \describe{
    \item{\code{n.iter}}{number of iterations to perform, defaulting to 30}
    \item{\code{n.chains}}{number of chains to use, ideally equal to the number of virtual cores
      available for use, and defaulting to 4}
    \item{\code{max.minutes}}{hard time limit that defaults to 20}
    \item{\code{seed}}{either \code{NA}, which is the default, or a psuedo-random number seed}
    \item{\code{verbose}}{logical scalar that is \code{TRUE} by default, indicating that
      progress of the iterative algorithm should be printed to the screen, which does not
      work under Windows when the chains are executed in parallel}
    \item{\code{save_models}}{logical scalar that defaults to \code{FALSE} but if \code{TRUE} indicates
    	that the models estimated on a frozen completed dataset should be saved. This option should be used if the user is interested in evaluating the quality of the models 			run after the last iteration of the \code{mi} algorithm, but saving these models consumes much more RAM}
    \item{\code{debug}}{logical scalar indicating whether to run in debug mode, which forces the processing to be sequential, and allows developers to capture errors within 			chains}
    \item{\code{parallel}}{if TRUE, then parallel processing is used, if available.  If FALSE, sequential processing is used. In addition, ths argument may be an object produced 		by \code{\link[parallel]{makeCluster}}}
  }
}
}
\details{
It is important to distinguish the two \code{mi} methods that are most relevant to users from the many \code{mi} methods that are less relevant. The primary \code{mi} method is that where \code{y} inherits from the \code{\link{missing_data.frame-class}} and \code{model} is omitted. This method \dQuote{does} the imputation according to the additional arguments described under \dots above and returns an object of class \code{"mi"}. Executing two or more independent chains is important for monitoring the convergence
of each chain, see \code{\link{Rhats}}.

If the chains have not converged in the amount of iterations or time specified, the second important \code{mi} method is that where \code{y} is an object of class \code{"mi"} and \code{model} is omitted, which continues a previous run of the iterative imputation algorithm. All the arguments described under \dots above remain applicable, except for \code{n.chains} and \code{save_RAM} because these are established by the previous run that is being continued.

The numerous remaining methods are of less importance to users.  One \code{mi} method is called when \code{y = "parallel"} and \code{model} is omitted. This method merely sets up the parallel backend so that the chains can be executed in parallel on the local machine.  We use the \code{\link{mclapply}} function in the \pkg{parallel} package to implement parallel processing on non-Windows machines, and we use the \pkg{snow} package to implement parallel processing on Windows machines; we refer users to the documentation for these packages for more detail about parallel processing. Parallel processing is used by default on machines with multiple processors, but sequential processing can be used instead by using the \code{parallel=FALSE} option. If the user is not using a mulitcore computer, sequential processing is used instead of parallel processing.

The first two \code{mi} methods described above in turn call a \code{mi} method where \code{y} inherits from the \code{\link{missing_data.frame-class}} and \code{model} is that which is returned by one of the \code{\link{fit_model-methods}}. The methods impute values for the originally missing values of a \code{\link{missing_variable}} given a fitted model, according to the \bold{imputation_method} slot of the \code{\link{missing_variable}} in question. Advanced users could define new subclasses of the \code{\link{missing_variable-class}} in which case it may be necessary to write such a \code{mi} method for the new class. It will almost certainly be necessary to add to the 
\code{\link{fit_model-methods}}. The existing \code{mi} and \code{fit-model-methods} should provide a template for doing so.
}
\value{
If \code{model} is missing and \code{n.chains} is positive, then the \code{mi} method will return an object of 
class \code{"mi"}, which has the following slots:
\describe{
  \item{call}{the call to \code{mi}}
  \item{data}{a list of \code{\link{missing_data.frame}}s, one for each chain}
  \item{total_iters}{an integer vector that records how many iterations have been performed}
}
There are a few methods for such an object, such as \code{\link{show}}, \code{\link{summary}},
\code{\link{dimnames}}, \code{\link{nrow}}, \code{\link{ncol}}, etc.

If \code{mi} is called on a \code{\link{missing_data.frame}} with \code{model} missing and a nonpositive
\code{n.chains}, then the \code{\link{missing_data.frame}} will be returned after allocating storeage.

If \code{model} is not missing, then the \code{mi} method will impute missing values for the \code{y}
argument and return it.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_data.frame}}, \code{\link{fit_model}}
}
\examples{
# STEP 0: Get data
data(CHAIN, package = "mi")

# STEP 1: Convert to a missing_data.frame
mdf <- missing_data.frame(CHAIN) # warnings about missingness patterns
show(mdf)

# STEP 2: change things
mdf <- change(mdf, y = "log_virus", what = "transformation", to = "identity")

# STEP 3: look deeper
summary(mdf)

# STEP 4: impute
\dontrun{
imputations <- mi(mdf)
}
}
\keyword{classes}
\keyword{regression}
\keyword{AimedAtusers}

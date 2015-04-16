\name{rdata.frame}
\alias{rdata.frame}
\title{Generate a random data.frame with tunable characteristics}
\description{
This function generates a random \code{\link{data.frame}} with a
missingness mechanism that is used to impose a missingness pattern. The primary
purpose of this function is for use in simulations
}
\usage{
rdata.frame(N = 1000, 
            restrictions = c("none", "MARish", "triangular", "stratified", "MCAR"),
            last_CPC = NA_real_, strong = FALSE, pr_miss = .25, Sigma = NULL, 
            alpha = NULL, experiment = FALSE, 
            treatment_cor = c(rep(0, n_full - 1), rep(NA, 2 * n_partial)),
            n_full = 1, n_partial = 1, n_cat = NULL,
            eta = 1, df = Inf, types = "continuous", estimate_CPCs = TRUE)
}
\arguments{
  \item{N}{integer indicating the number of observations}
  \item{restrictions}{character string indicating what restrictions to impose on
     the the missing data mechansim, see the Details section}
  \item{last_CPC}{a numeric scalar between \eqn{-1} and \eqn{1} exclusive or 
    \code{NA_real_} (the default). If not \code{NA_real_}, then this value will
    be used to construct the correlation matrix from which the data are drawn.
    This option is useful if restrictions is \code{"triangular"} or \code{"stratified"},
    in which case the degree to which \code{last_CPC} is not zero causes a violation of 
    the Missing-At-Random assumption that is confined to the last of the partially 
    observed variables}
  \item{strong}{Integer among 0, 1, and 2 indicating how strong to
    make the instruments with multiple partially observed variables,
    in which case the missingness indicators for each partially observed variable
    can be used as instruments when predicting missingness on other partially
    observed variables. Only applies when \code{restrictions = "triangular"}}
  \item{pr_miss}{numeric scalar on the (0,1) interval or vector
    of length \code{n_partial} indicating the proportion of observations
    that are missing on partially observed variables}
  \item{Sigma}{Either \code{\link{NULL}} (the default) or a correlation matrix
    of appropriate order for the variables (including the missingness
    indicators). By default, such a matrix is generated at random.}
  \item{alpha}{Either \code{\link{NULL}}, \code{\link{NA}}, or a numeric
    vector of appropriate length that governs the skew of a multivariate
    skewed normal distribution; see \code{\link[sn]{rmsn}}. The appropriate
    length is \code{n_full - 1 + 2 * n_partial} iff none of the variable types
    is nominal. If some of the variable types are nominal, then the appropriate
    length is \code{n_full - 1 + 2 * n_partial + sum(n_cat) - length(n_cat)}.
    If \code{\link{NULL}}, \code{alpha} is taken to be zero, in which case the
    data-generating process has no skew. If \code{\link{NA}}, \code{alpha}
    is drawn from \code{\link{rt}} with \code{df} degrees of freedom}
  \item{experiment}{logical indicating whether to simulate a randomized
    experiment}
  \item{treatment_cor}{Numeric vector of appropriate length indicating the
    correlations between the treatment variable and the other variables, which
    is only relevant if \code{experiment = TRUE}. The appropriate length is
    \code{n_full - 1 + 2 * n_partial} iff none of the variable types is nominal.
    If some of the variable types are nominal, then the appropriate length is
    \code{n_full - 1 + 2 * n_partial + sum(n_cat) - length(n_cat)}. If
    treatment_cor is of length one and is zero, then it will be recylced to
    the appropriate length. The treatment variable should be uncorrelated with
    intended covariates and uncorrelated with missingness on intended
    covariates. If any elements of treatment_cor are \code{\link{NA}}, then
    those elements will be replaced with random draws. Note that the order of
    the random variables is: all fully observed variables,all partially observed
    but not nominal variables, all partially observed nominal variables, all
    missingness indicators for partially observed variables.}
  \item{n_full}{integer indicating the number of fully observed variables}
  \item{n_partial}{integer indicating the number of partially observed
    variables}
  \item{n_cat}{Either \code{\link{NULL}} or an integer vector (possibly of
    length one) indicating the number of categories in each partially observed
    nominal or ordinal variable; see the Details section}
  \item{eta}{Positive numeric scalar which serves as a hyperparameter in the
    data-generating process. The default value of 1 implies that the correlation
    matrix among the variables is jointly uniformally distributed, using
    essentially the same logic as in the \pkg{clusterGeneration} package}
  \item{df}{positive numeric scalar indicating the degress of freedom for the
    (possibly skewed) multivariate t distribution, which defaults to
    \code{\link{Inf}} implying a (possibly skewed) multivariate normal 
    distribution}
  \item{types}{a character vector (possibly of length one, in which case it
    is recycled) indicating the type for each fully observed and partially
    observed variable, which currently can be among \code{"continuous"},
    \code{"count"}, \code{"binary"}, \code{"treatment"} (which is binary),
    \code{"ordinal"}, \code{"nominal"}, \code{"proportion"}, \code{"positive"}.
    See the Details section. Unique abbreviations are acceptable.}
  \item{estimate_CPCs}{A logical indicating whether the canonical partial correlations
    between the partially observed variables and the latent missingnesses should
    be estimated. The default is \code{TRUE} but considerable wall time can be saved
    by switching it to \code{FALSE} when there are many partially observed variables.}
}
\details{
By default, the correlation matrix among the variables and missingness indicators
is intended to be close to uniform, although it is often not possible to achieve
exactly. If \code{restrictions = "none"}, the data will be Not Missing At Random
(NMAR). If \code{restrictions = "MARish"}, the departure from Missing At Random
(MAR) will be minimized via a call to \code{\link{optim}}, but generally will
not fully achieve MAR. If \code{restrictions = "triangular"}, the MAR assumption
will hold but the missingness of each partially observed variable will only 
depend on the fully observed variables and the other latent missingness indicators.
If \code{restrictions = "stratified"}, the MAR assumption will hold but the
missingness of each partially observed variable will only depend on the fully
observed variables. If \code{restrictions = "MCAR"}, the Missing Completely At
Random (MCAR) assumption holds, which is much more restrictive than MAR.

There are some rules to follow, particularly when specifying \code{types}.
First, if \code{experiment = TRUE}, there must be exactly one treatment
variable (taken to be binary) and it must come first to ensure that the
elements of \code{treatment_cor} are handled properly. Second, if there are any
partially observed nominal variables, they must come last; this is to ensure
that they are conditionally uncorrelated with each other. Third, fully observed
nominal variables are not supported, but they can be made into ordinal variables
and then converted to nominal after the fact. Fourth, including both ordinal and
nominal partially observed variables is not supported yet, Finally, if any
variable is specified as a count, it will not be exactly consistent with the
data-generating process. Essentially, a count variable is constructed from a
continuous variable by evaluating \code{\link{pt}} on it and passing that to
\code{\link{qpois}} with an intensity parameter of 5. The other non-continuous
variables are constructed via some transformation or discretization of a continuous
variable.

If some partially observed variables are either ordinal or nominal (but not both), 
then the \code{n_cat} argument governs how many categories there are. If \code{n_cat}
is \code{NULL}, then the number of categories defaults to three. If
\code{n_cat} has length one, then that number of categories will be used for all
categorical variables but must be greater than two. Otherwise, the length of 
\code{n_cat} must match the number of partially observed categorical variables and
the number of categories for the \eqn{i}th such variable will be the \eqn{i}th element
of \code{n_cat}.
}
\value{
A list with the following elements:
\enumerate{
  \item{true}{ a \code{\link{data.frame}} containing no \code{\link{NA}} values}
  \item{obs}{ a \code{\link{data.frame}} derived from the previous with some
    \code{\link{NA}} values that represents a dataset that could be observed}
  \item{empirical_CPCs}{ a numeric vector of empirical Canonical Partial
     Correlations, which should differ only randomly from zero iff
     \code{MAR = TRUE} and the data-generating process is multivariate normal}
  \item{L}{ a Cholesky factor of the correlation matrix used to generate the
      true data}
}
In addition, if \code{alpha} is not \code{\link{NULL}}, then the following
elements are also included:
\enumerate{
  \item{alpha}{ the \code{alpha} vector utilized}
  \item{sn_skewness}{ the skewness of the multivariate skewed normal distribution
    in the population; note that this value is only an approximation of the
    skewness when \code{df < Inf}}
  \item{sn_kurtosis}{ the kurtosis of the multivariate skewed normal distribution
    in the population; note that this value is only an approximation of the
    kurtosis when \code{df < Inf}}
}
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung
Su, Masanao Yajima, Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{data.frame}}, \code{\link{missing_data.frame}}
}
\examples{
rdf <- rdata.frame(n_partial = 2, df = 5, alpha = rnorm(5))
print(rdf$empirical_CPCs) # not zero
rdf <- rdata.frame(n_partial = 2, restrictions = "triangular", alpha = NA)
print(rdf$empirical_CPCs) # only randomly different from zero
print(rdf$L == 0) # some are exactly zero by construction
mdf <- missing_data.frame(rdf$obs)
show(mdf)
hist(mdf)
image(mdf)
# a randomized experiment
rdf <- rdata.frame(n_full = 2, n_partial = 2, 
                   restrictions = "triangular", experiment = TRUE,
                   types = c("t", "ord", "con", "pos"),
                   treatment_cor = c(0, 0, NA, 0, NA))
Sigma <- tcrossprod(rdf$L)
rownames(Sigma) <- colnames(Sigma) <- c("treatment", "X_2", "y_1", "Y_2",
                                        "missing_y_1", "missing_Y_2")
print(round(Sigma, 3))
}
\keyword{utilities}

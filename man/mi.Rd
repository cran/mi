\name{mi}
\docType{genericFunction}
% Classes
\alias{mi-class}
% functions
\alias{mi}
\alias{mi,data.frame-method}
\alias{mi,mi-method}
\alias{is.mi}
\alias{bugs.mi}
\alias{call.mi}
\alias{converged}
\alias{data.mi}
\alias{imp.mi}
\alias{info.mi}
\alias{imp}
\alias{m}
\alias{is.mi,mi-method}
\alias{bugs.mi,mi-method}
\alias{call.mi,mi-method}
\alias{converged,mi-method}
\alias{data.mi,mi-method}
\alias{imp.mi,mi-method}
\alias{info.mi,mi-method}
\alias{m,mi-method}
\alias{print,mi-method}
\alias{imp,mi-method}
  
% methods
\alias{mi,mi-method}
\alias{show,mi-method}

\title{ Multiple Iterative Regression Imputation }

\description{
 Produce a multiple imputed matrix applying the elementary functions iteratively 
 to the variables with missingness in the data randomly imputing each variable
 and looping through until approximate convergence.
}

\usage{
\S4method{mi}{data.frame}( object, info,  n.imp = 3, n.iter = 30, 
    R.hat = 1.1, max.minutes = 20, rand.imp.method = "bootstrap", 
    preprocess = TRUE, continue.on.convergence = FALSE,
    seed = NA, check.coef.convergence = FALSE, 
    add.priors = prior.control(), post.run = TRUE)
    
\S4method{mi}{mi}( object, info, n.imp = 3, n.iter = 30, 
    R.hat = 1.1, max.minutes = 20, rand.imp.method = "bootstrap", 
    preprocess = TRUE, continue.on.convergence = FALSE,
    seed = NA, check.coef.convergence = FALSE)

}

\arguments{
  \item{object}{A data frame containing the incomplete data. Missing data are coded as NA or mi object.}
  \item{info}{\code{\link{mi.info}} object.}
  \item{n.imp}{Number of multiple imputations. The default is \code{m = 3}.}
  \item{n.iter}{Number of iterations to get convergence. The default is 5.}
  \item{R.hat}{R.hat statistic for convergence check, default is 1.1.}
  \item{max.minutes}{Maximum minutes to stop iterating.  The default is 20.}  
  \item{seed}{Random seed}
  \item{rand.imp.method}{Method for random imputation, see \code{\link{random.imp}}}
  \item{preprocess}{Preprocess the data according to the variable types, 
    see \code{\link{mi.preprocess}}}
  \item{continue.on.convergence}{If set to \code{TRUE} the \code{mi} will run 
    until maximum iteration is reached or maximum minutes pass.}
  \item{check.coef.convergence}{default = \code{FALSE}}
  \item{add.priors}{a list of parameters for controlling the process of adding priors for
      \code{\link{mi}}. See the documentation for \code{\link{prior.control}} for details.}  
  \item{post.run}{default is \code{TRUE} which will run 20 more iterations after the \code{mi} is finished
    if and only if some priors have been added into the \code{mi} process. This is to mitigate the 
    influence the importance of the priors to the whole procedure.}
}

\value{ 
A list of object of class \code{mi}, which stands for \dQuote{multiple imputation}.

  Each object is itself a list of 10 elements.
  \item{call}{Theimputation model}
  \item{data}{The original data frame}
  \item{m}{ The number of imputations.}
  \item{mi.info}{Information matrix of the \code{mi}}
  \item{imp}{A list of length(m) of imputations.}
  \item{converged}{Binary variable to indicate if the \code{mi} has converged.}
  \item{coef.conv}{Binary variable to indicate if the coefs of \code{mi} model have converged, return
    \code{NULL} if \code{check.coef.convergence = FALSE}}
  \item{bugs}{BUGS array of the mean and sd of each iteration.}
  \item{preprocess}{Binary variable to indicate if \code{preprocess=TRUE} in the \code{mi} process}
  \item{mi.info.preprocessed}{Information matrix that actually used in the \code{mi} if \code{preprocess=TRUE}}
  
  Each \code{imp[[m]]} is itself a list containg k variable lists of 3 objects:
  
  \item{\code{imp[[m]][[k]]@model}}{the specified models used for imputing missing values}
  \item{\code{imp[[m]][[k]]@expected}}{a list of vectors of length n-n.mis (number of complete observed data), specifying the
    estimated values of the models}
  \item{\code{imp[[m]][[k]]@random}}{a list of vectors of length n.mis (number of NAs), specifying the random predicted 
    values for imputing missing data}
}

\details{
Generate multiple imputations for incomplete data using iterative regression imputation. 
If the variables with missingness are a matrix Y with columns Y(1), . . . , Y(K) and the fully observed
predictors are X, this entails first imputing all the missing Y values using some
crude approach (for example, choosing imputed values for each variable by randomly
selecting from the observed outcomes of that variable); and then imputing Y(1)
given Y(2), . . . , Y(K) and X; imputing Y(2) given Y(1), Y(3), . . . , Y(K) and X (using
the newly imputed values for Y(1)), and so forth, randomly imputing each variable
and looping through until approximate convergence. 
}

\references{
    Kobi Abayomi, Andrew Gelman and Marc Levy. (2008).
    \dQuote{Diagnostics for multivariate imputations}.
    \emph{Applied Statistics} 57, Part 3: 273--291.

    Andrew Gelman and Maria Grazia Pittau.
    \dQuote{A flexible program for missing-data imputation and model checking.}
    \emph{Technical report}. Columbia University, New York.
   
    Andrew Gelman and Jennifer Hill. (2007).
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}.
    Cambridge University Press.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi.completed}}, \code{\link{mi.data.frame}},
    \code{\link{mi.continuous}}, \code{\link{mi.dichotomous}},
    \code{\link{mi.count}}, \code{\link{mi.categorical}},
    \code{\link{mi.polr}},  \code{\link{typecast}}, 
    \code{\link{mi.info}}, \code{\link{mi.preprocess}}  
}

\examples{
# simulate fake data
set.seed(100)
n <- 100
u1 <- rbinom(n, 1, .5)
v1 <- log(rnorm(n, 5, 1))
x1 <- u1*exp(v1)
u2 <- rbinom(n, 1, .5)
v2 <- log(rnorm(n, 5, 1))
x2 <- u2*exp(v2)
x3 <- rbinom(n, 1, prob=0.45)
x4 <- ordered(rep(seq(1, 5),100)[sample(1:n, n)])
x5 <- rep(letters[1:10],10)[sample(1:n, n)]
x6 <- trunc(runif(n, 1, 10))
x7 <- rnorm(n)
x8 <- factor(rep(seq(1,6),10)[sample(1:n, n)])
x9 <- rpois(n, 50)
x10 <- runif(n, 0.1, .99)
y <- x1 + x2 + x9 + rnorm(n)
fakedata <- cbind.data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

# randomly create missing values
dat <- mi:::.create.missing(fakedata, pct.mis=10)

# get information matrix of the data
inf <- mi.info(dat)

# update the variable type of a specific variable to mi.info
inf <- update(inf, "type", list(x9="count"))

# run the imputation
## this is for test only
IMP <- mi(dat, info=inf, n.iter=6, add.priors=prior.control(K=1))

# pick up where you left off
# IMP <- mi(IMP)       ## NOT RUN

## this is the suggested (defautl) way of running mi, NOT RUN
# IMP <- mi(dat, info=inf, add.priors=prior.control(K=1))

# convergence checking
converged(IMP)  ## You should get FALSE here because only n.iter is small 
bugs.mi(IMP)    ## BUGS object to look at the R hat statistics

# visually check the imputation
plot(IMP)
}
\keyword{manip}
\keyword{models}

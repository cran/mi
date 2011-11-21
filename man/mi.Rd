\name{mi}
%\docType{genericFunction}
% Classes
\alias{mi-class}
% functions
\alias{mi}
\alias{is.mi}
\alias{bugs.mi}
\alias{call.mi}
\alias{converged}
\alias{data.mi}
\alias{imp.mi}
\alias{info.mi}
\alias{imp}
\alias{m}
% methods
\alias{mi,data.frame-method}
\alias{mi,mi.preprocessed-method}
\alias{mi,mi-method}
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
\alias{show,mi-method}

\title{Multiple Iterative Regression Imputation}

\description{
 Generate a multiply imputed matrix applying the elementary functions iteratively 
 to the variables with missingness in the data randomly imputing each variable
 and looping through until approximate convergence.
}

\usage{
\S4method{mi}{data.frame}(object, info,  n.imp = 3, n.iter = 30, 
    R.hat = 1.1, max.minutes = 20, rand.imp.method = "bootstrap", 
    run.past.convergence = FALSE,
    seed = NA, check.coef.convergence = FALSE, 
    add.noise = noise.control())

\S4method{mi}{mi.preprocessed}(object, n.imp = 3, n.iter = 30, 
    R.hat = 1.1, max.minutes = 20, rand.imp.method = "bootstrap", 
    run.past.convergence = FALSE,
    seed = NA, check.coef.convergence = FALSE, 
    add.noise = noise.control())

    
\S4method{mi}{mi}(object, n.iter = 30, 
    R.hat = 1.1, max.minutes = 20, rand.imp.method = "bootstrap", 
    run.past.convergence = FALSE,  seed = NA)

}

\arguments{
  \item{object}{A data frame or an \code{mi} object that contains an incomplete data. \code{mi} identifies \code{NA}s as the missing data.}
  \item{info}{The \code{\link{mi.info}} object.}
  \item{n.imp}{The number of multiple imputations. Default is 3 chains.}
  \item{n.iter}{The maximum number of imputation iterations. Default is 30 iterations.}
  \item{R.hat}{The value of the \code{R.hat} statistic used as a convergence criterion. Default is 1.1.}
  \item{max.minutes}{The maximum minutes to operate the whole imputation process. Default is 20 minutes.}
  \item{rand.imp.method}{The methods for random imputation. Currently, \code{mi} implements only the \code{boostrap} method.}
%  \item{preprocess}{Default is \code{TRUE}.  \code{mi} will transform the variables that are of \code{nonnegative}, \code{positive-continuous}, and \code{proportion} types.}
  \item{run.past.convergence}{Default is \code{FALSE}. If the value is set to be \code{TRUE},  \code{mi} will run until the values of either \code{n.iter} or \code{max.minutes} are reached even if the imputation is converged.}
  \item{seed}{The random number seed.}
  \item{check.coef.convergence}{Default is \code{FALSE}. If the value is set to be \code{TRUE}, \code{mi} will check the convergence of the coefficients of imputation models.}
  \item{add.noise}{A list of parameters for controlling the process of adding noise to \code{mi} via \code{\link{noise.control}}.}
}

\value{ 
A list of object of class \code{mi}, which stands for \dQuote{multiple imputation}.

  Each object is itself a list of 10 elements.
  \item{call}{Theimputation model.}
  \item{data}{The original data frame.}
  \item{m}{The number of imputations.}
  \item{mi.info}{Information matrix of the \code{mi}.}
  \item{imp}{A list of length(m) of imputations.}
  \item{mcmc}{A mcmc list that stores lists of means and sds of the imputed data.}
  \item{converged}{Binary variable to indicate if the \code{mi} has converged.}
  \item{coef.mcmc}{A mcmc list that stores lists of means of regression coefficients of the conditonal models.}
  \item{coef.converged}{Binary variable to indicate if the coefs of \code{mi} model have converged, return
    \code{NULL} if \code{check.coef.convergence = FALSE}}
  \item{preprocess}{Binary variable to indicate if \code{preprocess=TRUE} in the \code{mi} process}
  \item{mi.info.preprocessed}{Information matrix that actually used in the \code{mi} if \code{preprocess=TRUE}.}
  
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
    Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).

    Kobi Abayomi, Andrew Gelman and Marc Levy. (2008).
    \dQuote{Diagnostics for multivariate imputations}.
    \emph{Applied Statistics} 57, Part 3: 273--291.
   
    Andrew Gelman and Jennifer Hill. (2007).
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}.
    Cambridge University Press.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  M. Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi.completed}}, \code{\link{mi.data.frame}},
    \code{\link{mi.continuous}}, \code{\link{mi.binary}},
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
x8 <- factor(rep(seq(1,10),10)[sample(1:n, n)])
x9 <- runif(n, 0.1, .99)
x10 <- rpois(n, 4)
y <- x1 + x2 + x7 + x9 + rnorm(n)
fakedata <- cbind.data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

# randomly create missing values
dat <- mi:::.create.missing(fakedata, pct.mis=30)

# get information matrix of the data
inf <- mi.info(dat)

# update the variable type of a specific variable to mi.info
inf <- update(inf, "type", list(x10="count"))

# run the imputation without data transformation
IMP <- mi(dat, info=inf, check.coef.convergence=TRUE, 
  add.noise=noise.control(post.run.iter=10))

# run the imputation with data transformation  
dat.transformed <- mi.preprocess(dat, inf)
IMP <- mi(dat.transformed, n.iter=6, check.coef.convergence=TRUE, 
  add.noise=noise.control(post.run.iter=10))

# no noise
# IMP <- mi(dat, info=inf, n.iter=6, add.noise=FALSE) ## NOT RUN

# pick up where you left off
 IMP <- mi(IMP, n.iter = 6)      

## this is the suggested (defautl) way of running mi
# IMP <- mi(dat, info=inf) ## NOT RUN

# convergence checking
converged(IMP, check = "data")  ## You should get FALSE here because only n.iter is small 
converged(IMP, check = "coefs")
IMP.bugs1 <- bugs.mi(IMP, check = "data")    ## BUGS object to look at the R hat statistics
IMP.bugs2 <- bugs.mi(IMP, check = "coefs")   ## BUGS object to look at the R hat statistics
plot(IMP.bugs1)  ## visually check R.hat

# visually check the imputation
plot(IMP)
}
\keyword{manip}
\keyword{models}

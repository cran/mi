\name{mi}
\docType{class}
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
%\alias{plot.mi}
\title{ Multiple Iterative Regression Imputation }
\description{
 Produce a multiple imputed matrix applying the elementary functions iteratively 
 to the variables with missingness in the data randomly imputing each variable
 and looping through until approximate convergence.
}

\usage{
mi( object, info, type = NULL, n.imp = 3, n.iter = 30, 
    max.minutes = 20, rand.imp.method = "bootstrap", 
    preprocess = FALSE, continue.on.convergence = FALSE,
    seed = NA, check.coef.convergence = FALSE, 
    augment.data = FALSE, K = 0) 
}
\arguments{
  \item{object}{A data frame containing the incomplete data. Missing data are coded as NA's or mi object.}
  \item{info}{ mi.info object. }
  \item{type}{Vector of types.  When you specify a type, types for all the columns must be specified.}
  \item{n.imp}{ Number of multiple imputations. The default is m = 3.}
  \item{n.iter}{ Number of iterations to get convergence. The default is 5. }
  \item{max.minutes}{ Maximum minutes to stop iterating.  The default is 20. }  
  \item{seed}{ Random seed.}
  \item{rand.imp.method}{ Method for random imputation }
  \item{preprocess}{ Preprocess the data according to the info matrix. }
  \item{continue.on.convergence}{ If set to TRUE the mi will run until maximum iteration is reached or maximum minutes pass. }
  \item{check.coef.convergence}{default = FALSE}
  \item{augment.data}{Augment the data with 10\% of the random draws from the observed data, 
                default= FALSE.}
  \item{K}{The cooling parameter, default is 0. If K > 0, the imputation will randomly switch between
        randomly drawing from the observation or drawing from the modeled based values.  And 
        with each iteration, the probability of cooling (q) decreases by number of iterations, such that
        q = K/S.}
  \item{\dots}{options for plot.mi.}
  
}

\value{ A list of object of class mi, which stands for multiple imputation.
  Each object is itself a list of 8 elements.
  \item{data}{The original data frame.}
  \item{imp.dat}{A data frame with the columns to be imputed.}
  \item{obs.dat}{A data frame with the completed columns.}
  \item{m}{ The number of imputations.}
  \item{nmis}{An array containing the number of missing observations per columns.}
  \item{imp}{A list of length(m) of imputations.}
  \item{converged}{Binary variable to indicate if mi has converged.}
  \item{bugs}{BUGS array of the mean and sd of each iteration.}
  \item{data.tmp}{If mi quits with errors, \code{data.tmp} return to the global environment}
  
  The \code{imp} method creates a list of length(m) of imputations, whose names are: 
  \code{Imputation1}, \code{Imputation2}, \code{Imputation3}.
  Each \code{imp[[m]]} is itself a list containg:
  
  - \code{imp[[m]]$Imp.Models}: the specified models used for imputing NA's in each columns of dat;
  
  - \code{imp[[m]]$Random.predicted}: a list of vectors of length n.mis (number of NA's), specifying the random predicted values for imputing
  missing data. For the "mixed" variables the vectors of random values are three: the random values
  predicted by using the binomial distribution (corresponding to the the first step of the imputation procedure);
  the random values predicted by using the normal distribution (corresponding to the second step of the imputation procedure)
  and finally the vector of random values (obtained multiplying the previous two vectors) whose values are positive
  whether missing values are positive, otherwise are equal to zero. For the categorical variables
  the random values are predicted by using the Multinomial ditribution;
  
  - \code{imp[[m]]$Random.predicted}: a list of vectors of length n-n.mis (number of complete observed data), specifying the
  estimated values of the models. For the "mixed" variables the vectors of estimated values are two,
  according to the two steps imputation procedure;
  
  - \code{imp[[m]]$Residual.values}: a list of vectors of residuals will be used for checking the models.
  For the "mixed" variables the vectors of residuals are two,
  according to the two steps imputation procedure;
  
  - \code{imp[[m]]$Imputed.matrix}: a data frame with the missing data imputed.
  
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
    Andrew Gelman and M. Grazia Pittau,  
    A flexible program for missing-data imputation and model checking, 
    Technical report, Columbia University, New York; Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  Yu-Sung Su \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi.matrix}} 
}
\examples{
data(CHAIN)
imp.CHAIN <- mi( CHAIN, n.imp = 3, n.iter = 6 )

is.mi( imp.CHAIN )    ## Is this a mi object?
data.mi ( imp.CHAIN ) ## You can get the original data

mi.mt <- mi.matrix( imp.CHAIN, m = 1 ) ## The imputed matrix for the first imputation
mi.df <- mi.data.frame( imp.CHAIN, m = 1 ) ## The imputed data frame for the second imputation

##############################
# Convergence checking
##############################
converged( imp.CHAIN )   ## You should get FALSE because its only 5 iterations
bugs.mi( imp.CHAIN )     ## BUGS object to look at the R hat statistics

# NOT RUN
#imp.CHAIN <- mi( imp.CHAIN, n.iter=5 ) ## You can pick up from where you left off
}
\keyword{ models }

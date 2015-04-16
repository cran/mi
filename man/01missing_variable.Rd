\name{01missing_variable}
\Rdversion{1.1}
\docType{class}
\alias{01missing_variable}
\alias{missing_variable}
\alias{missing_variable-class}
\alias{MatrixTypeThing-class}
\alias{WeAreFamily-class}
\title{Class "missing_variable" and Inherited Classes}
\description{
The missing_variable class is essentially the data comprising a variable plus all
the metadata needed to understand how its missing values will be imputed. However, no variable is
merely of missing_variable class; rather every variable is of a class that inherits from the 
missing_variable class. Even if a variable has no missing values, it needs to be coerced to a class 
that inherits from the missing_variable class before it can be used to impute values of other 
missing_variables. Understanding the properties of different subclasses of the missing_variable class 
is essential for modeling and imputing them. The \code{\link{missing_data.frame-class}} is essentially
a list of objects that inherit from the missing_variable class, plus metadata need to understand how
these missing_variables relate to each other.  Most users will never need to call \code{missing_variable} directly since it is called by \code{\link{missing_data.frame}}.
}
\section{Objects from the Classes}{The missing_variable class is virtual, so no objects 
  may be created from it. However, the missing_variable generic function can be used to 
  instantiate an object that inherits from the missing_variable class by specifying its 
  \code{type} argument. A user would call the \code{\link{missing_data.frame}}
  function on a \code{\link{data.frame}}, which in turn calls the missing_variable function
  on each column of the \code{\link{data.frame}} using various heuristics to guess the
  \code{type} argument.
}
\usage{
missing_variable(y, type, ...)
## Hidden arugments not included in the signature:
## favor_ordered = TRUE, favor_positive = FALSE, 
## variable_name = deparse(substitute(y))
}
\arguments{
  \item{y}{Can be any vector, some of whose values may be \code{\link{NA}}, which will
    comprise the \bold{raw_data} slot of a missing_variable (see the Slots section). It is 
    recommended that this vector \emph{not} have any transformations, such as a log-transformation. 
    Any continuous variable can be transformed using the function in its \bold{transformation} slot. 
    The transformations and other discretionary aspects of a missing_variable are typically changed
    by calling the \code{\link{change}} function on a \code{\link{missing_data.frame}} 
    See the Slots section for more details.
}
  \item{type}{Missing or a character string among the classes that inherit from the missing_variable
    class. If missing, the constructor will guess (sometimes incorrectly) based on the characteristics 
    of the variable. The best way to improve the guessing of categorical variables is to 
    use the \code{\link{factor}} function --- possibly with \code{ordered = TRUE} --- to create
    (possibly ordered) factors that will correctly be coerced to objects of 
    \code{\link{unordered-categorical-class}} and \code{\link{ordered-categorical-class}} respectively. 
    If you fail to do so, the hidden arguments that are not included in the signature affect the guesses. 
    If \code{favor_ordered = TRUE}, which is the default, it will tend to guess that variables with few 
    unique values are should be coerced to \code{\link{ordered-categorical-class}} and 
    \code{\link{unordered-categorical-class}} otherwise. If \code{favor_positive = FALSE}, which is the 
    default, it will tend to guess that variables with many unique values are merely continuous, whether 
    or not all the observed values are positive. If \code{favor_positive = TRUE} nonnegative or positive
    variables will get coerced to
    \code{\link{nonnegative-continuous-class}} or \code{\link{positive-continuous-class}}. See the Slots
    section and the specific help pages for more details on the subclasses.
}
  \item{\dots}{Further hidden arguments that are not in the signature. The \code{favor_ordered} and
   \code{favor_positive} arguments are documented immediately above. The \code{variable} name argument
   can be used to control what gets put in the \bold{variable_name} slot, see the Slots section below.
}
}
\section{Slots}{
  In the following table, indentation indicates inheritance from the class with less indentation, and
  italics indicates that the class is virtual so no variables can be created with that class. Inherited
  classes inherit the transformations, families, link functions, and \code{\link{fit_model-methods}}
  from their parent class, although these are often superceeded by analogues that are tailored for the
  inherited class. Also note, the default transformation for the continuous class is a standardization 
  using \emph{twice} the standard deviation of the observed values.

  The distinction between the transformation entailed by the \code{\link{family}} and the transformation
  entailed by the function in the \bold{tranformation} slot may be confusing at this point. The former pertains
  to how the linear predictor of a variable is mapped to the space of a variable when it is on the left-hand
  side of a generalized linear model. The latter pertains --- for continuous variables only --- to how the
  values in the \bold{raw_data} slot are mapped into those in the \bold{data} and thus affects how a continuous 
  variable enters into the model whether it is on the left or right-hand side. The classes are discussed in 
  much more detail below.
  \tabular{lll}{
    \bold{Class name [transformation]}   \tab \bold{Default family and link} \tab \bold{Default \code{\link{fit_model}}} \cr
    \emph{missing_variable}            \tab none                  \tab throws error \cr
    \code{  } \emph{categorical}                  \tab none        \tab throws error \cr
    \code{  } \code{  } unordered-categorical         \tab \code{binomial(link = 'logit')} \tab \code{\link[nnet]{multinom}} \cr
    \code{  } \code{  } ordered-categorical           \tab \code{binomial(link = 'logit')} \tab \code{\link[arm]{bayespolr}} \cr
    \code{  } \code{  } \code{  } binary              \tab \code{binomial(link = 'logit')} \tab \code{\link[arm]{bayesglm}} \cr
    \code{  } \code{  } \code{  } interval            \tab \code{gaussian{link = 'identity'}} \tab \code{\link[survival]{survreg}} \cr
    \code{  } continuous[standardize]                 \tab \code{gaussian{link = 'identity'}} \tab \code{\link[arm]{bayesglm}} \cr
    \code{  } \code{  } semi-continuous[identity]     \tab                                     \tab                             \cr
    \code{  } \code{  } \code{  } nonnegative-continuous[logshift] \tab                     \tab \cr
    \code{  } \code{  } \code{  } \code{  } SC_proportion[squeeze] \tab  \code{binomial(link = 'logit')} \tab \code{\link[betareg]{betareg}} \cr
    \code{  } \code{  } positive-continuous[\code{\link{log}}]           \tab                    \tab                              \cr
    \code{  } \code{  } \code{  } proportion[identity] \tab              \code{binomial(link = 'logit')} \tab \code{\link[betareg]{betareg}} \cr
    \code{  } \code{  } bounded-continuous[identity]  \tab \tab \cr
    \code{  } count                           \tab \code{quasipoisson{link = 'log'}}     \tab      \code{\link[arm]{bayesglm}}    \cr
    \code{  } irrelevant                      \tab                                       \tab      throws error                   \cr
    \code{  } \code{  } fixed                 \tab                               \tab      throws error                   \cr
  }

  The missing_variable class is virtual and has the following slots (this information is primarily directed at developeRs):
  \describe{
    \item{\code{variable_name}:}{Object of class \code{\link{character}} of length one naming the variable}
    \item{\code{raw_data}:}{Object of class \code{"ANY"} representing the observations
      on a variable, some of which may be \code{\link{NA}}. No method should ever change 
      this slot at all. Instead, methods should change the \bold{data} slot.}
    \item{\code{data}:}{Object of class \code{"ANY"}, which is initially a copy of the
      \bold{raw_data} slot --- transformed by the function in the \bold{transformation} slot 
       for continuous variables only --- and whose \code{\link{NA}} values are replaced during
       the multiple imputation process. See \code{\link{mi}}}
    \item{\code{n_total}:}{Object of class \code{"integer"} which is the \code{\link{length}}
       of the \bold{data} slot}
    \item{\code{all_obs}:}{Object of class \code{"logical"} of length one indicating whether
       all values of the \bold{data} slot are observed and thus not \code{\link{NA}} }
    \item{\code{n_obs}:}{Object of class \code{"integer"} of length one indicating the number
       of values of the \bold{data} slot that are observed and thus not \code{\link{NA}} }
    \item{\code{which_obs}:}{Object of class \code{"integer"}, which is a vector indicating 
       the positions of the observed values in the \bold{data} slot}
    \item{\code{all_miss}:}{Object of class \code{"logical"} of length one indicating whether
       all values of the \bold{data} slot are \code{\link{NA}} }
    \item{\code{n_miss}:}{Object of class \code{"integer"} of length one indicating the number
       of values of the \bold{data} slot that are \code{\link{NA}} }
    \item{\code{which_miss}:}{Object of class \code{"integer"}, which is a vector indicating
       the positions of the missing values in the \bold{data} slot }
    \item{\code{n_extra}:}{Object of class \code{"integer"} of length one indicating how many
       (missing) observations have been added to the end of the \bold{data} slot that are not
       included in the \bold{raw_data} slot. Although the extra values will be imputed, they
       are not considered to be \dQuote{missing} for the purposes of defining the previous
       three slots}
    \item{\code{which_extra}:}{Object of class \code{"integer"}, which is a vector indicating
       the positions of the extra values at the end of the \bold{data} slot }
    \item{\code{n_unpossible}:}{Object of class \code{"integer"} of length one indicating the
       number of values that are logically or structurally unobservable}
    \item{\code{which_unpossible}:}{Object of class \code{"integer"} indicating the positions
       of the unpossible values in the \bold{data} slot } 
    \item{\code{n_drawn}:}{Object of class \code{"integer"} of length one which is the sum of
       the \bold{n_miss} and \bold{n_extra} slots}
    \item{\code{which_drawn}:}{Object of class \code{"integer"} which is a vector concatinating
       the \bold{which_miss} and \bold{which_extra} slots }
    \item{\code{imputation_method}:}{Object of class \code{"character"} of length one indicating
       how the \code{\link{NA}} values are to be imputed. Possibilities include \dQuote{ppd} for
       imputation from the posterior predictive distribution, \dQuote{pmm} for imputation via
       predictive mean matching, \dQuote{mean} for mean-imputation, \dQuote{median} for 
       median-imputation, \dQuote{expectation} for conditional mean-imputation. With enough
       programming effort, other kinds of imputation can be defined and specified here.}
    \item{\code{family}:}{Object of class \code{"WeAreFamily"} that will typically be passed to 
       \code{\link{glm}} and similar functions during the multiple imputation process}
    \item{\code{known_families}:}{Object of class \code{\link{character}} indicating the families
       that are known to be supported for a class; see \code{\link{family}}}
    \item{\code{known_links}:}{Object of class \code{\link{character}} indicating what link functions
       are known to be supported by the elements of the \bold{known_families} slot; see 
       \code{\link{family}}}
    \item{\code{imputations}:}{Object of class \code{"MatrixTypeThing"} with rows equal to the number
       of iterations (initially zero) of the multiple imputation algorithm and columns equal to the 
       \bold{n_drawn} slot. The rows are appropriately extended and then filled by the 
       \code{\link{mi}} function}
    \item{\code{done}:}{Object of class \code{"logical"} of length one indicating whether the
       \code{\link{NA}} values in the \bold{data} slot have been replaced by imputed values}
    \item{\code{parameters}:}{Object of class \code{"MatrixTypeThing"} with rows equal to the number
       of iterations (initially zero) of the multiple imputation algorithm and columns equal to the number 
       of estimated parameters when modeling the \bold{data} slot. The rows are appropriately extended
       and then filled by the \code{\link{mi}} function}
    \item{\code{model}:}{Object of class \code{"ANY"} which can be filled by an object that is output
       by one of the \code{\link{fit_model-methods}}, which is done by default by \code{\link{mi}}
       when all the iterations have completed}
    \item{\code{fitted}:}{Object of class \code{"ANY"} although typically a vector or matrix that 
       contains the fitted values of the model in the slot immediately above. Note that the
       \bold{fitted} slot is filled by default by \code{\link{mi}}, although the \bold{model} slot
       is left empty by default to save RAM.}
    \item{\code{estimator}:}{Object of class \code{"character"} of length one indicating which pre-existing 
    	\code{\link{fit_model}} to use for an unordered-categorical variable.  Options are \code{"mnl"}, in which 
    	\code{\link[nnet]{multinom}} from the \pkg{nnet} package is used to fit the values of the unordered 
    	categorical variable; and \code{"rnl"}, in which each category is separately modeled as the positive 
    	binary outcome against all other categories using a \code{\link[arm]{bayesglm}} \code{fit_model} and 
    	the probabilities of each category are normalized to sum to 1 after each model is run. In general, 
    	\code{"rnl"} is slightly less accurate than \code{"mnl"}, but runs much more quickly especially when 
    	the unordered categorical variable has many unique categories.}
  }
  The WeAreFamily class is a class union of \code{\link{character}} and \code{\link{family}}, while the
  MatrixTypeThing class is a class union of \code{\link{matrix}} only at the moment.
}
\value{
The missing_variable function returns an object that inherits from the missing_variable class.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_data.frame}}, \code{\link{categorical-class}},  \code{\link{unordered-categorical-class}}, 
\code{\link{ordered-categorical-class}}, \code{\link{binary-class}}, \code{\link{interval-class}}, 
\code{\link{continuous-class}}, \code{\link{semi-continuous-class}}, \code{\link{nonnegative-continuous-class}},
\code{\link{SC_proportion-class}}, \code{\link{censored-continuous-class}}, 
\code{\link{truncated-continuous-class}}, \code{\link{bounded-continuous-class}}, 
\code{\link{positive-continuous-class}}, \code{\link{proportion-class}}, \code{\link{count-class}}
}
\examples{
# STEP 0: GET DATA
data(nlsyV, package = "mi")

# STEP 0.5 CREATE A missing_variable (you never need to actually do this)
income <- missing_variable(nlsyV$income, type = "continuous")
show(income)

# STEP 1: CONVERT IT TO A missing_data.frame
mdf <- missing_data.frame(nlsyV) # this calls missing_variable() internally
show(mdf)
}
\keyword{classes}
\keyword{AimedAtUseRs}
\keyword{DirectedTowardDevelopeRs}

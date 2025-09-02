\name{02missing_data.frame}
\Rdversion{1.1}
\docType{class}
\alias{02missing_data.frame}
\alias{missing_data.frame-class}
\alias{missing_data.frame}
\title{Class "missing_data.frame"}
\description{
This class is similar to a \code{\link{data.frame}} but is customized for the situation in 
which variables with missing data are being modeled for multiple imputation. This class primarily 
consists of a list of \code{\link{missing_variable}}s plus slots containing metadata indicating how the
\code{\link{missing_variable}}s relate to each other. Most operations that work for a
\code{\link{data.frame}} also work for a missing_data.frame.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("missing_data.frame", ...)}.
However, useRs almost always will pass a \code{\link{data.frame}} to the 
missing_data.frame constructor function to produce an object of missing_data.frame class.
}
\usage{
missing_data.frame(y, ...)
## Hidden arguments not included in the signature
## favor_ordered = TRUE, favor_positive = FALSE, 
## subclass = NA_character_,
## include_missingness = TRUE, skip_correlation_check = FALSE
}
\arguments{
  \item{y}{Usually a \code{\link{data.frame}}, possibly a numeric matrix, 
    possibly a list of \code{\link{missing_variable}}s.}
  \item{\dots}{Hidden arguments. The \code{favor_ordered} and \code{favor_positive}
    arguments are passed to the \code{\link{missing_variable}} function and are 
    documented under the \code{type} argument. Briefly, they affect the heuristics
    that are used to guess what class a variable should be coerced to. The 
    \code{subclass} argument defaults to \code{\link{NA}} and can be used to specify
    that the resulting object should inherit from the missing_data.frame class
    rather than be an object of \code{missing_data.frame} class.

    Any further arguments are passed to the \code{\link{initialize-methods}} for
    a missing_data.frame. They currently are \code{include_missingness}, which 
    defaults to \code{TRUE} and indicates that the missingness pattern of the other
    variables should be included when modeling a particular \code{\link{missing_variable}}, 
    and \code{skip_correlation_check}, which defaults to FALSE and indicates whether
    to skip the default check for whether the observed values of each pair of \code{\link{missing_variable}}s 
    has a perfect absolute Spearman \code{\link{cor}}relation.
}
}
\section{Slots}{
  This section is primarily aimed at developeRs. A missing_data.frame inherits from
  \code{\link{data.frame}} but has the following additional slots:
  \describe{
    \item{\code{variables}:}{Object of class \code{"list"} and each list element
      is an object that inherits from the \code{\link{missing_variable-class}} }
    \item{\code{no_missing}:}{Object of class \code{"logical"}, which is a vector
      whose length is the same as the length of the \bold{variables} slot indicating 
      whether the corresponding \code{\link{missing_variable}} is fully observed }
    \item{\code{patterns}:}{Object of class \code{\link{factor}} whose length is equal
      to the number of observation and whose elements indicate the missingness pattern
      for that observation}
    \item{\code{DIM}:}{Object of class \code{"integer"} of length two indicating
      first the number of observations and second the length of the \bold{variables}
      slot }
    \item{\code{DIMNAMES}:}{Object of class \code{"list"} of length two providing
      the appropriate number rownames and column names }
    \item{\code{postprocess}:}{Object of class \code{"function"} used to create
      additional variables from existing variables, such as interactions between
      two \code{\link{missing_variable}}s once their missing values have been
      imputed. Does not work at the moment}
    \item{\code{index}:}{Object of class \code{"list"} whose length is equal to 
      the number of \code{\link{missing_variable}}s with some missing values. Each
      list element is an integer vector indicating which columns of the \bold{X}
      slot must be dropped when modeling the corresponding \code{\link{missing_variable}} }
    \item{\code{X}:}{Object of \code{\link{MatrixTypeThing-class}} with rows equal to the
      number of observations and is loosely related to a \code{\link{model.matrix}}. Rather 
      than repeatedly parsing a \code{\link{formula}} during the multiple imputation process,
      this \bold{X} matrix is created once and some of its columns are dropped when
      modeling a \code{\link{missing_variable}} utilizing the \bold{index} slot.
      The columns of the \bold{X} matrix consists of numeric representations of the 
      \code{\link{missing_variable}}s plus (by default) the unique missingness patterns }
    \item{\code{weights}:}{Object of class \code{"list"} whose length is equal to one
       or the number of \code{\link{missing_variable}}s with some missing values. Each 
       list element is passed to the corresponding argument of \code{\link[arm]{bayesglm}} 
       and similar functions. In particular, some observations can be given a weight
       of zero, which should drop them when modeling some \code{\link{missing_variable}}s}
    \item{\code{priors}:}{Object of class \code{"list"} whose length is equal to the number
       of \code{\link{missing_variable}}s and whose elements give appropriate values for
       the priors used by the model fitting function wraped by the \code{\link{fit_model-methods}}; 
       see, e.g., \code{\link[arm]{bayesglm}}}
    \item{\code{correlations}:}{Object of class \code{"matrix"} with rows and
        columns equal to the length of the \bold{variables} slot. Its strict upper
        triangle contains Spearman \code{\link{cor}}relations between pairs of
        variables (ignoring missing values), and its strict lower triangle contains
        Squared Multiple Correlations (SMCs) between a variable and all other
        variables (ignoring missing values). If either a Spearman correlation or
        a SMC is very close to unity, there may be difficulty or error messages
        during the multiple imputation process.}
    \item{\code{done}:}{Object of class \code{"logical"} of length one indicating
        whether the missing values have been imputed}
    \item{\code{workpath}:}{Object of class \code{\link{character}} of length one indicating
        the path to a working directory that is used to store some objects}
  }
}
\details{
In most cases, the first step of an analysis is for a useR to call the 
\code{missing_data.frame} function on a \code{\link{data.frame}} whose variables
have some \code{\link{NA}} values, which will call the \code{\link{missing_variable}}
function on each column of the \code{\link{data.frame}} and return the \code{\link{list}}
that fills the \bold{variable} slot. The classes of the list elements will depend on the
nature of the column of the \code{\link{data.frame}} and various fallible heuristics. The
success rate can be enhanced by making sure that columns of the original 
\code{\link{data.frame}} that are intended to be categorical variables are 
(ordered if appropriate) \code{\link{factor}}s with labels. Even in the best case
scenario, it will often be necessary to utlize the \code{\link{change}} function to 
modify various discretionary aspects of the \code{\link{missing_variable}}s in the 
\bold{variables} slot of the missing_data.frame. The \code{\link{show}} method for
a missing_data.frame should be utilized to get a quick overview of the 
\code{\link{missing_variable}}s in a missing_data.frame and recognized what needs
to be \code{\link{change}}d.
}
\section{Methods}{
  There are many methods that are defined for a missing_data.frame, although some
  are primarily intended for developers. The most relevant ones for users are:
  \describe{
    \item{change}{\code{signature(data = "missing_data.frame", y = "ANY", what = "character", to = "ANY")}
      which is used to change discretionary aspects of the \code{\link{missing_variable}}s
      in the \bold{variables} slot of a missing_data.frame}
    \item{hist}{\code{signature(x = "missing_data.frame")} which shows histograms
      of the observed variables that have missingness}
    \item{image}{\code{signature(x = "missing_data.frame")} which plots 
      an image of the \bold{missingness} slot to visualize the pattern of missingness
      when \code{grayscale = FALSE} or the pattern of missingness in light of the
      observed values (\code{grayscale = TRUE}, the default)}
    \item{mi}{\code{signature(y = "missing_data.frame", model = "missing")} which 
      multiply imputes the missing values}
    \item{show}{\code{signature(object = "missing_data.frame")} which gives an overview
      of the salient characteristics of the \code{\link{missing_variable}}s in the 
      \bold{variables} slot of a missing_data.frame }
    \item{summary}{\code{signature(object = "missing_data.frame")} which produces the same
      result as the \code{\link{summary}} method for a \code{\link{data.frame}}}
  }
  There are also S3 methods for the \code{\link{dim}}, \code{\link{dimnames}}, and \code{\link{names}}
  generics, which allow functions like \code{\link{nrow}}, \code{\link{ncol}}, \code{\link{rownames}},
  \code{\link{colnames}}, etc. to work as expected on \code{missing_data.frame}s. Also, accessing
  and changing elements for a \code{missing_data.frame} mostly works the same way as for a
  \code{\link{data.frame}}
}
\value{
The \code{missing_data.frame} constructor function returns an object of class \code{missing_data.frame} 
or that inherits from the \code{missing_data.frame} class.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{change}}, \code{\link{missing_variable}}, \code{\link{mi}},
\code{\link{experiment_missing_data.frame}}, \code{\link{multilevel_missing_data.frame}}
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
hist(mdf)
image(mdf)

# STEP 4: impute
\dontrun{
imputations <- mi(mdf)
}

## An example with subsetting on a fully observed variable
data(nlsyV, package = "mi")
mdfs <- missing_data.frame(nlsyV, favor_positive = TRUE, favor_ordered = FALSE, by = "first")
mdfs <- change(mdfs, y = "momed", what = "type", to = "ord")
show(mdfs)

}
\keyword{classes}
\keyword{manip}
\keyword{AimedAtUseRs}
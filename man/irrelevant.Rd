\name{irrelevant}
\Rdversion{1.1}
\docType{class}
\alias{irrelevant}
\alias{irrelevant-class}
\alias{fixed-class}
\alias{group-class}
\title{Class "irrelevant" and Inherited Classes}
\description{
The irrelevant class inherits from the \code{\link{missing_variable-class}} and is used to 
designate variables that are excluded from the models used to impute the missing values of 
\dQuote{relevant} variables. For example, if a survey has an \dQuote{id} variable that 
simply distinguishes observations, the user should designate it as irrelevant, although it 
will automatically be classified so if its name is either \dQuote{id} or starts with punctuation 
(including underscores). The fixed class inherits from the irrelevant class and is used 
for variables that are constant (within a sample). A variable that is instantiated from the
fixed class cannot have any missing values. The group class inherits from the fixed 
class and is used like a \code{\link{factor}} to spit samples in multilevel modeling; see 
\code{\link{multilevel_missing_data.frame-class}}. None of these classes have an additional
slots. Aside from these facts, the rest of the documentation here is primarily directed toward developeRs.
}
\section{Objects from the Classes}{The \code{\link{missing_variable}} generic function can be used to 
  instantiate an object that inherits from the irrelevant class by specifying 
  \code{type = "irrelevant"}, \code{type = "fixed"}, or \code{type = "group"}.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_variable-class}}
}
\examples{
# STEP 0: GET DATA
data(nlsyV, package = "mi")

# STEP 0.5 CREATE A missing_variable (you never need to actually do this)
first <- missing_variable(as.factor(nlsyV$first), type = "group")
show(first)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}


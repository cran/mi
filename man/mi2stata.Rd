\name{mi2stata}
\alias{mi2stata}
\title{Exports completed data in Stata (.dta) or comma-separated (.csv) format}
\description{
This function exports completed data from an object of \code{\link{mi-class}} in which \code{m} completed 
\code{\link{data.frame}}s are appended to the end of the raw data. Two additional variables are added which indicate the row number and distinguish the \code{\link{data.frame}}s.  The outputed file is either Stata 
(.dta) or comma-separated (.csv) format, and can be easily registered in Stata as multiply imputed data.}
\usage{
mi2stata(imputations, m, file, missing.ind=FALSE, ...)
}
\arguments{
  \item{imputations}{Object of \code{\link{mi-class}}}
  \item{m}{The number of completed datasets to append onto the raw data}
  \item{file}{The filename, either a full path or relative to the working directory, where the file will be saved.  
	  Filenames must end in either '.dta' or '.csv'.  Files with names ending in '.dta' will be saved  
    as a Stata data file, and files with names ending in '.csv' will be saved as a comma-separated file.}
  \item{missing.ind}{If \code{TRUE}, includes a binary variable for each variable with \code{\link{NA}} values,
	indicating the observations which were originally missing. Defaults to \code{FALSE}.}
  \item{\dots}{Further arguments passed to \code{\link[foreign]{write.dta}} for Stata files, or to \code{\link{write.table}}
	for .csv files.}
}

\details{
The function calls \code{\link{complete}} to construct \code{m} completed \code{\link{data.frame}}s, and uses 
\code{\link{rbind}} to append them to the bottom of the raw data that still contains all of the missing values. 
Two new variables are added: \code{_mi}, which contains the observation numbers; and \code{_mj}, which indexes the
\code{\link{data.frame}}s.

To save a Stata .dta file, end the filename with '.dta'. To save a comma-separated file, end the filename with 
.csv'. Stata files are loaded into Stata using Stata's \code{use} command, and comma-separated files can be loaded
by typing \code{insheet using} \emph{filename}\code{, comma names clear}. Once the file is loaded into Stata, the
data must be registered as multiply imputed before any subsequent analyses can be performed. In Stata version 11 or
later, type \code{mi import mice} to register the data.  The \code{_mi} and \code{_mj} variables will be replaced
by variables named \code{_mi_id} and \code{_mi_m} respectively. In Stata version 10 or earlier, install the
\code{MIM} package by typing \code{findit mim} and installing package \code{st0139_1}.  Then the prefix \code{mim:}
must be added to any command using the multiply imputed data.

Any observations which are unpossible (legitimately skipped, and are not imputed, see 
\code{\link{missing_variable}}) will remain missing in the complete data, but will not be indicated as missing by these variables.  If there are
any unpossible values, missing indicators are included automatically.
}
\value{
\code{NULL}
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao 
Yajima, Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{complete}}, \code{\link{mi}}, \code{\link[foreign]{write.dta}}, \code{\link{write.table}}
}
\examples{
fn <- paste(tempfile(), "dta", sep = ".")
if(!exists("imputations", env = .GlobalEnv)) {
  imputations <- mi:::imputations # cached from example("mi-package")
}
mi2stata(imputations, m=5, file=fn , missing.ind=TRUE)
}
\keyword{utilities}

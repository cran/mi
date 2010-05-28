\name{CHAIN}
\docType{data}
\alias{CHAIN}
\title{
  Subset of variables from the CHAIN project, a longitudinal
  cohort study of people living with HIV in New York City.
}
\description{
  The CHAIN cohort was
  recruited in 1994 from a large number of medical care and social service agencies
  serving HIV in New York City.  Cohort members were interviewed up to 8
  times through 2002. A total of 532 CHAIN participants completed at least one interview
  at either the 6th, 7th or 8th, and 508, 444, 388 interviews were completed
  respectively at rounds 6,7 and 8th.
}
\usage{data(CHAIN)}
\format{
  A data frame with 532 observations on the following 8 variables.
  \describe{
    \item{\code{h39b.W1}}{
      log of self reported viral load level at round 6th
      (0 represents undetectable level).
    }
    \item{\code{age.W1}}{
      age at time of interview at round 6th.
    }
    \item{\code{c28.W1}}{
      family annual income. Values range from under
      \$5,000 to \$70,000 or over at round 6th.
    }
    \item{\code{pcs.W1}}{
      a continuous scale of physical health with a theoretical range between 0 and 100 
      (better health is associated with higher scale values) at round 6th.
    }
    \item{\code{mcs37.W1}}{
      a binary measure of poor mental health ( 1=Yes, 0=No ) at round 6th.
    }
    \item{\code{b05.W1}}{
      ordered interval for the CD4 count
      (indicator of how much damage HIV has caused to the immune system) at round 6th.
    }
    \item{\code{haartadhere.W1}}{
      a three-level ordered variable:
        0=Not currently taking HAART (highly Active antiretroviral therapy),
        1=taking HAART nonadherent,
        2=taking HAART adheremt at round 6th.
    }
    %\item{\code{h39b.W2}}{
    %  log of self reported viral load level at round 7th.
    %  (0 represents undetectable level).
    %}
  }
}
\details{
  A missing value in the virus load level (h39b)
  was assigned to individuals who either could not recall
  their viral load level, did not have a viral load test in the six month
  preceding the interview, or reported their viral loads as "good" or "bad".
}
\source{
  http://cchps.columbia.edu/research.cfm
}
\references{
  Messeri P, Lee G, Abramson DA, Aidala A, Chiasson MA, Jones JD. (2003).
  \dQuote{Antiretroviral therapy and declining AIDS mortality in New York City}.
  \emph{Medical Care} 41:512--521.
}
\seealso{
  \code{\link{mi}}
}
\keyword{datasets}

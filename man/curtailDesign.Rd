\name{curtailDesign}
\alias{curtailDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for calculating the probability for efficacy given known information
%%  ~~function to do ... ~~
}
\description{calculates the probability for efficacy given the Z value
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
curtailDesign(theObject, current0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{current0}{The current Z value
%%     ~~Describe \code{current0} here~~
}
}

\value{
numeric
}

\examples{
design1<-sequential.design()
design1<-calcBoundaries(design1)
design1<-setCurrentLook(design1,1)
prob1<-curtailDesign(design1,1.5)

}


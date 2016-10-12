\name{calcBoundaries}
\alias{calcBoundaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for calculating the efficacy and futility boundaries
%%  ~~function to do ... ~~
}
\description{
Calculates the efficacy and futility boundaries. This only needs to be done once for a new design.
}
\usage{
calcBoundaries(theObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
}

\value{
numeric
}

\examples{
design1<-sequential.design()
design1<-calcBoundaries(design1)
design1@lower.boundary
design1@upper.boundary
}



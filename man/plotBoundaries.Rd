\name{plotBoundaries}
\alias{plotBoundaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that plots the efficacy and futility boundaries
%%  ~~function to do ... ~~
}
\description{Plots the efficacy and futility boundaries
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotBoundaries(theObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
}

\examples{
design1<-sequential.design()
design1<-calcBoundaries(design1)
plotBoundaries(design1)
}


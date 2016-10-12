\name{printSummary}
\alias{printSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that prints the cumulative probabilities for efficacy and futility
%%  ~~function to do ... ~~
}
\description{Prints the cumulative probability for efficacy and futility under the null and alternative hypotheses, the corresponding p-values, and the boundaries for Z at each look
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
printSummary(theObject)
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
printSummary(design1)
}

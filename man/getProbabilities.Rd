\name{getProbabilities}
\alias{getProbabilities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that calculates the cumulative probabilities to declare efficacy and futility
%%  ~~function to do ... ~~
}
\description{Calculates the cumulative probabilities to declare efficacy and futility under the null hypothesis and the alternative hypothesis. It also returns the p-values for declaring efficacy and futility.

}
\usage{
getProbabilities(theObject)
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
probs<-getProbabilities(design1)
}

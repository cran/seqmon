\name{summary}
\alias{summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that shows the cumulative probabilities for efficacy and futility
%%  ~~function to do ... ~~
}
\description{Shows the cumulative probability for efficacy and futility under the null and alternative hypotheses, the corresponding p-values, and the boundaries for Z at each look
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary(theObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
}

\examples{
design2<-calcBoundaries(sequential.design())
summary(design2)
}

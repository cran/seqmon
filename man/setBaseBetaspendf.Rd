\name{setBaseBetaspendf}
\alias{setBaseBetaspendf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that sets the base beta spending function
%%  ~~function to do ... ~~
}
\description{Sets the base beta spending function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setBaseBetaspendf(theObject, funct0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{funct0}{The base beta spending function. It needs to be defined before this method is called.
%%     ~~Describe \code{funct0} here~~
}
}

\value{
an object of class "sequential.design"
}

\examples{
design1<-sequential.design()
f2<-function (t) 0.15*t^2.5
design1<-setBaseBetaspendf(design1,f2)
}


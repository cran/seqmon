\name{setBaseAlphaspendf}
\alias{setBaseAlphaspendf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that sets the base alpha spending function
%%  ~~function to do ... ~~
}
\description{Sets the base alpha spending function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setBaseAlphaspendf(theObject, funct0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{funct0}{The base alpha spending function. It needs to be defined before this method is called.
%%     ~~Describe \code{funct0} here~~
}
}

\value{
an object of class "sequential.design"
}

\examples{
design1<-sequential.design()
f1<-function (t) 0.025*t^3.5
design1<-setBaseAlphaspendf(design1,f1)
}


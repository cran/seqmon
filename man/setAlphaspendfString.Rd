\name{setAlphaspendfString}
\alias{setAlphaspendfString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that Sets the expression of the base alpha spending function as a string
%%  ~~function to do ... ~~
}
\description{Sets the expression of the base alpha spending function as a string. This function is only used if one needs to display the base alpha spending function as a string. This function DOES NOT update the base alpha spending function. One can use setBaseAlphaspendf() to change the base alpha spending function. The spending functions and their string expressions should be defined only once per object. They should not be updated during any interim update to the design.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setAlphaspendfString(theObject, string0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{string0}{The string of the expression of the base alpha spending function. Its argument need to be 't'.
%%     ~~Describe \code{string0} here~~
}
}

\value{
an object of class "sequential.design"
}

\examples{
design1<-sequential.design()
design1<-setAlphaspendfString(design1,'0.025*t^4')
}

\name{setTimes}
\alias{setTimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that sets the look times
%%  ~~function to do ... ~~
}
\description{Sets the look times. It is to be called only for the inital design, not for updating the design.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setTimes(theObject, time0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{time0}{The look times.
%%     ~~Describe \code{time0} here~~
}
}

\value{
an object of class "sequential.design"
}

\examples{
design1<-sequential.design()
design1<-setTimes(design1,c(1,2,3))

}

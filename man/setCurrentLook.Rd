\name{setCurrentLook}
\alias{setCurrentLook}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that sets the current look number
%%  ~~function to do ... ~~
}
\description{Sets the current look number. The curent look is the one that last took place.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setCurrentLook(theObject, look0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{look0}{The curent look number
%%     ~~Describe \code{look0} here~~
}
}
\details{The curent look is the one that last took place. One can only set the current look forward. If the new current look number attempted is less than the old current look number, no action will take place and the current look number will not be updated.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
an object of class "sequential.design"
}

\examples{
design1<-sequential.design()
design1<-setCurrentLook(design1,2)
}


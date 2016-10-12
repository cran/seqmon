\name{setDatestamp}
\alias{setDatestamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that sets the date stamp of the design object
%%  ~~function to do ... ~~
}
\description{Sets the date stamp of the design object
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setDatestamp(theObject, date0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{date0}{The date value.
%%     ~~Describe \code{date0} here~~
}
}

\value{
an object of class "sequential.design"
}

\examples{
design1<-sequential.design()
design1<-setDatestamp(design1,as.POSIXct("2015-10-30"))
}


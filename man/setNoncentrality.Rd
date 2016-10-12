\name{setNoncentrality}
\alias{setNoncentrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that sets the noncentrality parameter
%%  ~~function to do ... ~~
}
\description{Sets the noncentrality parameter.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setNoncentrality(theObject, noncent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{noncent}{The noncentrality parameter
%%     ~~Describe \code{noncent} here~~
}
}
\details{The noncentrality paraeter is the expected drift at the end of the study. For example, if the study has a power of 80\% using a one sided Z-test with 2.5\% type 1 error, the noncentrality parameter is q(0.975)+q(0.8), where q() is the percentile function of the standard normal distribution.  
%%  ~~ If necessary, more details than the description above ~~
}
\value{
an object of class "sequential.design"
}

\examples{
design1<-sequential.design()
noncent<-qnorm(0.975,0,1)+qnorm(0.8,0,1)
design1<-setNoncentrality(design1,noncent)
}


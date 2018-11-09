\name{updateDesign}
\alias{updateDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that updates the design
%%  ~~function to do ... ~~
}
\description{Updates the design. This can be done in the process of the study,  when the future look times need to be changed from those originally planned.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateDesign(theObject, futureTimes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{The sequential design object
%%     ~~Describe \code{theObject} here~~
}
  \item{futureTimes}{The future look times. 
%%     ~~Describe \code{futureTimes} here~~
}
}
\details{The efficacy and futility boundaries will be updated according to the new future look times. If the new final look is before the planned final look, the efficacy and futility boundaries will be updated, but the alpha and beta spending functions need not be updated. If the new final look is after the planned final look, the efficacy and futility boundaries will be updated, as well as the alpha and beta spending functions.
The details are given in Proschan, Lan, and
Wittes(2006) and Schoenfeld (2001).
No historical information such as the past look times, the past alpha and beta spent, or the baseline spending function is updated.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
an object of class "sequential.design"
}
\references{
Proschan, MA; Lan, KKG; Wittes JT,"Statistical Monitoring of Clinical Trials: A Unified Approach", Chapter 6, Springer 2006.

Schoenfeld DA, "A Simple Algorithm for Designing Group Sequential Clinical Trials", Biometrics. 2001 Sep;57(3):972-4.
}
\author{
David A Schoenfeld, PhD and Hui Zheng, PhD
}

\examples{
design1<-sequential.design()
design1<-setTimes(design1,c(1,2))
design1<-calcBoundaries(design1)
design1<-setCurrentLook(design1,1)
design2<-updateDesign(design1,c(3))

}


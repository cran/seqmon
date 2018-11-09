\name{print}
\alias{print}
\title{Function that displays the features of the design}

\description{
'print' displays the look times, the base alpha and beta spending functions, and the noncentrality parameter
}

\usage{
print(theObject)
}
\arguments{
  \item{theObject}{The sequential design object}
}
\examples{
design1<-sequential.design()
design1<-calcBoundaries(design1)
design1<-setAlphaspendfString(design1,"0.025*t^4")
design1<-setBetaspendfString(design1,"0.15*t^3")
print(design1)
}


\name{seqmon-package}
\alias{seqmon-package}
%\alias{seqmon}
\docType{package}
\title{
seqmon
}
\description{
a package for creating, monitoring and modifying a group sequential design
}
\details{

The DESCRIPTION file:
DESCRIPTION

}
\author{David A Schoenfeld, PhD and Hui Zheng, PhD}


\references{
Proschan, MA, Lan, KKG, Wittes, JT, Statistical Monitoring of Clinical Trials: A Unified Approach, Springer, 2006

Schoenfeld DA, "A Simple Algorithm for Designing Group Sequential Clinical Trials", Biometrics. 2001 Sep;57(3):972-4.
}


\examples{
design1<-sequential.design()
design1<-calcBoundaries(design1)
print(design1)
summary(design1)
}

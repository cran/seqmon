\name{sequential.design}
\alias{sequential.design}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The sequential design class
%%  ~~function to do ... ~~
}
\description{The S4 sequential design class
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sequential.design(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Additional arguments passed to the methods.
}
}
\details{The sequential design class stores the information of a sequential design, including revision history. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
an object of the class "sequential.design"
}
\references{
Proschan, MA; Lan, KKG; Wittes JT,"Statistical Monitoring of Clinical Trials: A Unified Approach", Chapter 6, Springer 2006.

Schoenfeld DA, "A Simple Algorithm for Designing Group Sequential Clinical Trials", Biometrics. 2001 Sep;57(3):972-4.
}
\author{
David A. Schoendfeld, PhD and Hui Zheng, PhD
}

\examples{
design1<-sequential.design()
}


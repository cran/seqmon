\name{seqmon}
\alias{seqmon}
\title{Sequential Monitoring of Clinical Trials}
\description{
 A program that computes the probabilities of crossing boundaries in a group sequential
 clinical trial. It implements the Armitage-McPherson and Rowe (1969) algorithm using the method
 described in Schoenfeld D. (2001). Assume that there is a sequence of test statistics 
 \eqn{z_1,\dots,z_m}{z1,...zm} in a clinical trial. Each statistic has a standard normal
 distribution under the null hypothesis. Let \eqn{a_1,\dots,a_m}{a1,...,am} and
 \eqn{b_1,\dots,b_m}{b1,...,bm} be a lower and an upper boundary and let 
 \eqn{t_1,\dots,t_m}{t1,...,tm} be the amount of information that was used to calculate each statistic.
 The function calculates the probability that \eqn{z_j<a_j,j\leq i}{zj<aj, j<=i} and
 \eqn{b_j<z_j,j\leq i}{bj<zj, j<=i} for  \eqn{i=1,\dots,m}{i=1,...,m}. Probabilities for an alternative
 hypothesis can be found by adding an offset to \eqn{a_1,\ldots,a_m}{a1,...,am} and   
 \eqn{b_1,\ldots,b_m}{b1,...,bm} equal to the expected value of the statistic.
}
\usage{
seqmon(a,b,t,int)
}
\value{
  A m x 2 matrix giving the the cumulative probabilities of crossing the lower boundary
  and the probabilities of crossing the upper boundary.
}
\note{The test statistic is assumed to be standardized so that it's value at any point in time
is normally distributed random variable with mean zero and variance one. The numerator of the test 
statistic is assumed to be a gaussian process with independent increments.}

\arguments{\item{a}{A vector of the lower boundaries at information time t1,t2,...,tm}
\item{b}{A vector of the upper boundaries at the information times t1,t2,...tm}
\item{t}{A vector of the information times, usually 1:m}
\item{int}{A vector of the number of intervals to divide up (a[k],b[k]), 500 should be adequate}
}
\examples{
##The following gives the probablility of crossing 
##the boundaries for a O'Brien Flemming (1979) lower and upper bound
##with five looks at the data under the null hypothosis.
z<-2.04
a<- -z*sqrt(5/(1:5))
b<- +z*sqrt(5/(1:5))
t<- 1:5
int<- 500* array(c(1),5)
seqmon(a,b,t,int)
##This gives the probabilities under the alternative hypothesis if the expected value of the mean difference over it's
##standard error for one group is 1.5
u<-1.5
seqmon(a+u*sqrt(1:5),b+u*sqrt(1:5),t,int)
}
\references{
Armitage, P., McPherson, C. K. and Rowe, B. C. (1969) 
``Repeated significance tests on accumulating data'' 
\emph{Journal of the Royal Statistical Society, Series A, General}, 132, 235-244 

 Schoenfeld D. (2001) ``A simple Algorithm for Designing Group Sequential Clinical
 Trials'' \emph{Biometrics} 27 , pp, 972-974. 

 O'Brien, Peter C. , and Fleming, Thomas R.  (1979), 
  ``A multiple testing procedure for clinical trials'', \emph{Biometrics}, 35 , 549-556 
}
\keyword{design}
\keyword{htest}

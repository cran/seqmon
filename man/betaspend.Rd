\name{betaspend}
\alias{betaspend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that calculates the lower boundaries for futility
%%  ~~function to do ... ~~
}
\description{Calculates the lower boundaries for futility at each look
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
betaspend(levels, upperboundary, t, int = rep(500, length(t)), noncent, tol = 0.005)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{levels}{The cumulative beta spending at each look time
%%     ~~Describe \code{levels} here~~
}
  \item{upperboundary}{The upper efficacy boundaries at each look 
%%     ~~Describe \code{upperboundary} here~~
}
  \item{t}{Normalized look times
%%     ~~Describe \code{t} here~~
}
  \item{int}{The numbers of intervals the solution space is partitioned into
%%     ~~Describe \code{int} here~~
}
  \item{noncent}{The noncentrality parameter
%%     ~~Describe \code{noncent} here~~
}
  \item{tol}{Tolerance of the solution using uniroot
%%     ~~Describe \code{tol} here~~
}
}
\value{
numeric
}

\examples{

f<- function(t) 0.025*t^4
g<- function(t) 0.15*t^3
t<-c(0.33,0.67,1)
cum_alphas<-f(t)
cum_betas<-g(t)
noncent<-qnorm(0.975)+qnorm(0.85)
upper_boundaries<-alphaspend(cum_alphas,t,int=rep(500, length(t)),tol=0.005)
lower_boundaries<-betaspend(cum_betas, upper_boundaries, t, int = rep(500,3), noncent, tol = 0.005) 
}



\name{alphaspend}
\alias{alphaspend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that calculates the upper boundaries for efficacy 
}
\description{Calculates the upper boundaries for efficacy at each look time
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
alphaspend(levels, t, int = rep(500, length(t)), tol = 0.005)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{levels}{The cumulative alpha spending at each look time
%%     ~~Describe \code{levels} here~~
}
  \item{t}{Normalized look times
%%     ~~Describe \code{t} here~~
}
  \item{int}{The number of intervals the solution space is partitioned into
%%     ~~Describe \code{int} here~~
}
  \item{tol}{Tolerance of the solution using uniroot
%%     ~~Describe \code{tol} here~~
}
}

\value{
numeric
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

f<- function(t) 0.025*t^4
t<-c(0.33,0.67,1)
cum_probs<-f(t)
alphaspend(levels=cum_probs,t,int=rep(500, length(t)),tol=0.005)

}


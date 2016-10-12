\name{seqmon}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{seqmon}
\title{Generic function that calculates the cumulative alpha and beta spending
%%  ~~function to do ... ~~
}
\description{Calculates the cumulative alpha and beta spending
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
seqmon(a, b, t, int = rep(500, length(t)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{upper boundaries
%%     ~~Describe \code{a} here~~
}
  \item{b}{lower boundaries
%%     ~~Describe \code{b} here~~
}
  \item{t}{time of looks
%%     ~~Describe \code{t} here~~
}
  \item{int}{number of intervals that the Z-space is partitioned into 
%%     ~~Describe \code{int} here~~
}
}

\value{
numeric
}

\examples{
t<-c(0.33,0.67,1)
f<- function(t) 0.025*t^4
g<-function(t) 0.20*t^3
a<-f(t)
b<-g(t)
seqmon(a, b, t, int = rep(500, length(t)))
}

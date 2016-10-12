\name{curtail}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{curtail}
\title{Generic function that calculates the probability to declare efficacy at the end of study given the Z value at the current look
%%  ~~function to do ... ~~
}
\description{Calculates the probability to declare efficacy at the end of study given the Z value at the current look
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
curtail(lower.boundary,upper.boundary,look,t,noncen,current=lower.boundary[look])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lower.boundary}{lower boundaries
%%     
}
  \item{upper.boundary}{upper boundaries
%%     
}
  \item{look}{current look number
%%     
}
  \item{t}{time of looks
%%     
}
  \item{noncen}{noncentrality parameter
%%     
}
  \item{current}{current Z statistic
%%     
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
noncen<-pnorm(0.975)+pnorm(0.8)
curtail(b,a,1,t,noncen)
}

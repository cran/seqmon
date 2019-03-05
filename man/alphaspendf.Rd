\name{alphaspendf}
\alias{alphaspendf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The default alpha spending function
%%  ~~function to do ... ~~
}
\description{The default alpha spending function
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
alphaspendf(t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
The normalized look times
}
}
\value{
numeric
}

\examples{
t<-c(0.33,0.67,1)
alphas<-alphaspendf(t)

## The function is currently defined as
function (t) 
0.025 * t^4
}


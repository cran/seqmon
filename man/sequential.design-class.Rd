\name{sequential.design-class}
\Rdversion{1.1}
\docType{class}
\alias{sequential.design-class}
\alias{sequential.design,sequential.design-class}
\alias{calcBoundaries,sequential.design-method}
\alias{curtailDesign,sequential.design-method}
\alias{getProbabilities,sequential.design-method}
\alias{plotBoundaries,sequential.design-method}
\alias{print,sequential.design-method}
\alias{summary,sequential.design-method}
\alias{setAlphaspendfString,sequential.design-method}
\alias{setBaseAlphaspendf,sequential.design-method}
\alias{setBaseBetaspendf,sequential.design-method}
\alias{setBetaspendfString,sequential.design-method}
\alias{setCurrentLook,sequential.design-method}
\alias{setDatestamp,sequential.design-method}
\alias{setNoncentrality,sequential.design-method}
\alias{setTimes,sequential.design-method}
\alias{updateDesign,sequential.design-method}

\title{Class \code{"sequential.design"}}
\description{
The sequential design class
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{sequential.design(...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{lower.boundary}:}{Object of class \code{"numeric"}  }
    \item{\code{upper.boundary}:}{Object of class \code{"numeric"}  }
    \item{\code{times}:}{Object of class \code{"numeric"}  }
    \item{\code{noncentrality}:}{Object of class \code{"numeric"}  }
    \item{\code{base.alpha.spend}:}{Object of class \code{"function"}  }
    \item{\code{base.beta.spend}:}{Object of class \code{"function"}  }
    \item{\code{base.alpha.spend.string}:}{Object of class \code{"character"}  }
    \item{\code{base.beta.spend.string}:}{Object of class \code{"character"}  }
    \item{\code{current.look}:}{Object of class \code{"numeric"}  }
    \item{\code{current.alpha.spend}:}{Object of class \code{"numeric"}  }
    \item{\code{current.beta.spend}:}{Object of class \code{"numeric"}  }
    \item{\code{times.history}:}{Object of class \code{"numeric"}  }
    \item{\code{alpha.spent.history}:}{Object of class \code{"numeric"}  }
    \item{\code{beta.spent.history}:}{Object of class \code{"numeric"}  }
    \item{\code{alpha.func.history}:}{Object of class \code{"numeric"}  }
    \item{\code{beta.func.history}:}{Object of class \code{"numeric"}  }
    \item{\code{date.stamp}:}{Object of class \code{"POSIXct"}  }
  }
}
\section{Methods}{
  \describe{
    \item{calcBoundaries}{\code{signature(theObject = "sequential.design")}: ... }
    \item{curtailDesign}{\code{signature(theObject = "sequential.design")}: ... }
    \item{getProbabilities}{\code{signature(theObject = "sequential.design")}: ... }
    \item{plotBoundaries}{\code{signature(theObject = "sequential.design")}: ... }
    \item{printDesign}{\code{signature(theObject = "sequential.design")}: ... }
    \item{summaryDesign}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setAlphaspendfString}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setBaseAlphaspendf}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setBaseBetaspendf}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setBetaspendfString}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setCurrentLook}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setDatestamp}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setNoncentrality}{\code{signature(theObject = "sequential.design")}: ... }
    \item{setTimes}{\code{signature(theObject = "sequential.design")}: ... }
    \item{updateDesign}{\code{signature(theObject = "sequential.design")}: ... }
	 }
}

\examples{
showClass("sequential.design")
}
\keyword{classes}


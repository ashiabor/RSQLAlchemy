\name{control-class}
\Rdversion{1.1}
\docType{class}
\alias{control-class}

\title{Class \code{"control"}}
\description{
control is a class with one field "variables" which holds internal variables created during an application.
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\author{
Awo Ashiabor
}

\seealso{
\code{\linkS4class{engine}} 
\code{\linkS4class{session}} 
\code{\linkS4class{tableRecords}} 

}
\examples{
showClass("control")
}
\keyword{classes}
\section{Fields}{
  \describe{
     \item{\code{variables}:}{Object of class \code{list} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{addVariable(name, value)}:}{ assign a value to a name }
    \item{\code{setVariables(value)}:}{ set all internal variables to the list value }
    \item{\code{getVariables()}:}{ get all internal variables }
  }
}

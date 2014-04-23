\name{tableRecords-class}
\Rdversion{1.1}
\docType{class}
\alias{tableRecords-class}

\title{Class \code{"tableRecords"}}
\description{
tableRecords a class to abstract table contents with methods for data processing. It is a class with one field "dataFrame" which holds a dataFrame representation of a table. Its methods include query functions such as  "orderBy", "limit", "offset", filterBy", "all", "first", "one", scalar" and "count"
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\author{
Awo Ashiabor
}

\seealso{
\code{\linkS4class{session}} 
\code{\linkS4class{engine}} 

}
\examples{
\dontrun{
showClass("tableRecords")
}}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{dataFrame}:}{Object of class \code{data.frame} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{count()}:}{ count of observations in table }
    \item{\code{scalar()}:}{ if only one observation in table return observation else return nothing }
    \item{\code{one()}:}{ if only one observation in table return observation else return a message explaining that there are either no observations or multiple observations }
    \item{\code{first()}:}{ the first observation in table }
    \item{\code{all()}:}{ all observations in table }
    \item{\code{filterBy(...)}:}{ filter table using clause provided in argument }
    \item{\code{offset()}:}{ return observations in table offset by an index using the matrix function "[,]" }
    \item{\code{limit()}:}{return a subset of observations in table by applying the matrix function "[,]" }
    \item{\code{orderBy(...)}:}{ return observations in table by a specified order }
    \item{\code{setDataFrame(value)}:}{ assign the contents of the tableRecords to the dataframe provided }
    \item{\code{getDataFrame()}:}{ fetch contents of tableRecords in a dataframe format}
    \item{\code{initialize(...)}:}{ initialize tableRecords}
  }
}

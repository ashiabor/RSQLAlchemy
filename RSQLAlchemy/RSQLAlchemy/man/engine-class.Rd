\name{engine-class}
\Rdversion{1.1}
\docType{class}
\alias{engine-class}

\title{Class \code{"engine"}}
\description{
This reference class connects the application to a sqlite database. The engine class has one field "databaseName" which holds the connection string to the sqlite database. It has functions "query"" and "insert"" that allows users to query and insert records in a database
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
Python version of SQL Alchemy engine can be referenced at http://docs.sqlalchemy.org/en/rel_0_9/core/engines.html#sqlalchemy.create_engine
}
\author{
Awo Asiabor
}
\examples{
\dontrun{
showClass("engine")
}}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{databaseName}:}{Object of class \code{character} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{insert(tableName, dFrame)}:}{ insert records encapsulated in dataframe into database table named tableName}
    \item{\code{query(sqlStatement)}:}{ query database with free-form sql statement sqlStatement }
    \item{\code{setDatabaseName(value)}:}{ change database connection with new database name provided }
    \item{\code{getDatabaseName()}:}{ fetch name of database to which engine is connect }
    \item{\code{initialize(...)}:}{ initialize an engine }
  }
}

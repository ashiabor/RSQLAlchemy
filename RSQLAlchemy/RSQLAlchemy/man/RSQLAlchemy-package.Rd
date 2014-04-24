\name{RSQLAlchemy-package}
\alias{RSQLAlchemy-package}
\alias{RSQLAlchemy}
\docType{package}
\title{
OOP abstraction of sqlite databases
}
\description{
This package provides an OOP abstraction of database connections, tables and records. It enables users to process data in a sqlite database without employing SQL commands.This package is an R version of the Python package SQL Alchemy.
}
\details{
\tabular{ll}{
Package: \tab RSQLAlchemy\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-10\cr
License: \tab MIT\cr
Depends: \tab RSQLite\cr
}
Very simple to use. It consists of 4 reference classes and a function.

1. control:=> a class to track internal variables  
2. engine:=> a class to abstract database connections     
3. tableRecords:=> a class to abstract table contents  
4. session:=> a class that provides a workspace for RSQLAlchemy applications
5. mapTable:=> a function to abstract tables and records as OOP objects  

}
\author{
Awo Ashiabor

Maintainer: Awo Ashiabor <ashiabor@alum.mit.edu>
}
\references{
Details of Python version, SQLAlchemy can be found on the web at http://docs.sqlalchemy.org
}

\keyword{ RSQLAlchemy }
\keyword{ SQLAlchemy }
\keyword{ session }
\keyword{ control }
\keyword{ engine }
\keyword{ tableRecords }
\keyword{ mapTable }
\keyword{ query }

\examples{
\dontrun{
#create an engine connected to a database "geno.db"
geno_engine=engine(databaseName="geno.db")

#start a SQLAlchemy session
mySession<-session()

#bind engine to session
mySession$bind(geno_engine)

#query snp table 
mySession$query("snp")
}}

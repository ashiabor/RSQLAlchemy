\name{engine}
\alias{engine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
abstraction of sqlite database connection
}
\description{
This reference class connects the application to a sqlite database. The engine class has one field "databaseName" which holds the connection string to the sqlite database. It has functions "query"" and "insert"" that allows users to query and insert records in a database
}
\usage{
#connect to a sqlite database by name
engine(...)
}

\arguments{
  \item{\dots}{name of sqlite database with the '.db' suffix
}
}
\details{
An engine is created with engine(databaseName). The engine created can be called to run a query against a table in the database and also insert records in a table in database.
}
\value{
An engine reference class is created. Nothing returned.
}
\references{
Python version of SQL Alchemy engine can be referenced at http://docs.sqlalchemy.org/en/rel_0_9/core/engines.html#sqlalchemy.create_engine
}
\author{
Awo Ashiabor
}
\examples{
\dontrun{
##create an engine connected to attached test.db sqlite database
testEngine=engine(databaseName=testDB)

## query snp table in geno.db database
testEngine$query('select * from snp')
}
\dontrun{
## insert records into snp table in test.db sqlite database
recordSamples<-testEngine$query('select * from snp')[1:2,]
recordSamples
recordSamples[1,1]<-'SNPQQ'
recordSamples[2,1]<-'SNPWW'
recordSamples
testEngine$insert(tableName='snp',dFrame=recordSamples)
testEngine$query('select * from snp')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ RSQLAlchemy }
\keyword{ engine }
\keyword{ query }
\keyword{ insert }

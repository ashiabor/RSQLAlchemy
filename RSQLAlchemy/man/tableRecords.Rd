\name{tableRecords}
\alias{tableRecords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Abstract content of a database table
}
\description{
tableRecords a class to abstract table contents with methods for data processing. It is a class that provides database query methods such as filter, orderby, count, etc... Query results from a session are encapsulated as tableRecordss to enable further processing such as "orderBy", "limit", "offset", filterBy", "all", "first", "one", scalar" and "count"
}
\usage{
tableRecords(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{dataFrame whose rows translate to database table records}
}


\value{
A tableRecords reference class object is created. Nothing is returned.
}
\author{
Awo Ashiabor
}
\examples{

##create a tableRecords using quakes data
quakesTableRecords<-tableRecords(dataFrame=quakes)
quakesTableRecords

##query tableRecords
quakesTableRecords$count()
quakesTableRecords$orderBy("stations")
quakesTableRecords$orderBy("stations","mag")#complex orders
quakesTableRecords$limit()[1:5,]
quakesTableRecords$offset()[5:10,]

#filter observations returned from query
quakesTableRecords$filterBy(stations==15) #equals
quakesTableRecords$filterBy(stations!=15) #not equals
\dontrun{
quakesTableRecords$filterBy(stations %like% "1") #like
quakesTableRecords$filterBy(stations %!like% "1") #not like
quakesTableRecords$filterBy(stations %in% c(10,15)) #in
quakesTableRecords$filterBy(stations %!in% c(10,15)) #not in
quakesTableRecords$filterBy(is.null(stations)) #is null
quakesTableRecords$filterBy(!is.null(stations)) #is not null
quakesTableRecords$filterBy(stations==15 | mag>4) #or
quakesTableRecords$filterBy(stations==15 & mag>4)#and
}

#lists and scalars
quakesTableRecords #prints entire table
quakesTableRecords$all() #returns every observation
quakesTableRecords$first() #returns first observation 

# count
quakesTableRecords
quakesTableRecords$count()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ TableRecords }
\keyword{ orderBy }
\keyword{ filterBy }
\keyword{ limit }
\keyword{ scalar }
\keyword{ all }
\keyword{ first }
\keyword{ one }
\keyword{ count }

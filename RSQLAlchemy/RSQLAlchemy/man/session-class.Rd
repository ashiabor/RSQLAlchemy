\name{session-class}
\Rdversion{1.1}
\docType{class}
\alias{session-class}

\title{Class \code{"session"}}
\description{
This class provides provides a platform for connecting other RSQLAlchemy building blocks in a package. The session class has fields engine and objects. The engine of a session represents the database connection, while the $objects constitute a temporary store of objects created during the application. Methods include "add", "add_all", "delete" and delete_all" which save and remove objects from session, "bind" a function that connects the session to an engine (database connection) and functions to write and read from database: commit", "query","join", "outerjoin", "innerjoin", "leftjoin", "rightjoin".
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
The Python version of SQL Alchemy session class documented at http://docs.sqlalchemy.org/en/rel_0_9/orm/session.html#sqlalchemy.orm.session.Session
}
\author{
Awo Ashiabor
}
\examples{
\dontrun{
showClass("session")
#start a session
mySession<-session()

#bind an engine to session
mySession$bind(engine(databaseName="geno.db"))

#query, limit and/or offset observations returned from query
mySession$query("snp") #prints entire table
mySession$query("snp.snp_id")
mySession$query("snp","snp.snp_id")
mySession$query("snp","snp.snp_id")$limit()[1:5,]
mySession$query("snp","snp.snp_id")$offset()[5:10,]
mySession$query("snp","genotype")$limit()[1:5,] #query multiple tables (outer join)
}}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{engine}:}{Object of class \code{engine} database connection }
    \item{\code{objects}:}{Object of class \code{list} a store of instances of classes created and added to session }
  }
}
\section{Methods}{
  \describe{
    \item{\code{setEngine(value)}:}{ set database connection to engine specified }
    \item{\code{getEngine()}:}{ get the engine bound to session }
    \item{\code{setObjects(value)}:}{ set temporary object store to objects specified }
    \item{\code{getObjects()}:}{ get objects temporarily saved in session }
    
    \item{\code{query(...)}:}{ fetch contents of tables or table columns specified }
    \item{\code{join(...)}:}{ join tables in a cartesian fashion }
    \item{\code{outerjoin(...)}:}{ outerjoin tables }
    \item{\code{innerjoin(...,joinOn)}:}{ create an inner join based on clause specified in joinOn parameter. join currently only takes clauses with equal evaluations "==" eg. tableA.ID==tableB.ID }
    \item{\code{leftjoin(...,joinOn)}:}{ create a left join based on clause specified in joinOn parameter. join currently only takes clauses with equal evaluations "==" eg. tableA.ID==tableB.ID}
    \item{\code{rightjoin(...,joinOn)}:}{ create an right join based on clause specified in joinOn parameter. join currently only takes clauses with equal evaluations "==" eg. tableA.ID==tableB.ID }
    
    \item{\code{commit()}:}{ send all objects temporarily saved in database to database -> "insert" }
    
    \item{\code{delete_all(...)}:}{ remove all objects from temporary session store}
    \item{\code{delete(obj, name)}:}{ remove specified objects from temporary session store }
    \item{\code{add_all(...)}:}{ add all objects specified to temporary session store }
    \item{\code{add(obj, name)}:}{ remove object from temporary session store }
    
    \item{\code{bind(engn)}:}{ link session to database abstracted in engine}
    \item{\code{initialize(...)}:}{ initialize a session }
  }
}

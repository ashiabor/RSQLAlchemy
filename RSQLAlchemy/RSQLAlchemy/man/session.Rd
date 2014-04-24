\name{session}
\alias{session}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Framework for managing RSQLALchemy objects
}
\description{
This class provides provides a platform for connecting other RSQLAlchemy building blocks in a package. The session class has fields engine and objects. The engine of a session represents the database connection, while the $objects constitute a temporary store of objects created during the application. Methods include "add", "add_all", "delete" and delete_all" which save and remove objects from session, "bind" a function that connects the session to an engine (database connection) and functions to write and read from database: commit", "query","join", "outerjoin", "innerjoin", "leftjoin", "rightjoin".
}
\usage{
session(...)
}
\arguments{
  \item{\dots}{engine and or session objects}
}

\references{
The Python version of SQL Alchemy session class documented at http://docs.sqlalchemy.org/en/rel_0_9/orm/session.html#sqlalchemy.orm.session.Session
}
\author{
Awo Ashiabor
}
\examples{

#start a session
mySession<-session()

#map snp table to snp class and create an instance of snp
snp<-mapTable(tableName="snp",
              columns='snp_id="character",
               chr="character",
               all_A="character",
               all_B="character"');

SNP50 <- snp(snp_id='SNP50', chr="1", all_A="T", all_B="C")

#check objects added to session
mySession$objects
length(mySession$objects)
names(mySession$objects)

#add an object to session
mySession$add(SNP50)
length(mySession$objects)
names(mySession$objects)
mySession$objects

#replace an object previously added to session  
SNP50$snp_id<-"SNPCHANGE"
mySession$add(SNP50)#previous version of SNP50 is replaced
length(mySession$objects)
names(mySession$objects)
mySession$objects

#add multiple objects at once
SNPA <- snp(snp_id='SNPA', chr="1", all_A="T", all_B="C")
SNPB <- snp(snp_id='SNPB', chr="1", all_A="T", all_B="C")
mySession$add_all(SNPA,SNPB)
length(mySession$objects)
names(mySession$objects)
mySession$objects

#delete an object previously added to session
length(mySession$objects)
names(mySession$objects)
mySession$delete(SNP50)
length(mySession$objects)
names(mySession$objects)

#delete multiple objects previously added to session
mySession$delete_all(SNPA,SNPB)
length(mySession$objects)
names(mySession$objects)

\dontrun{
#bind an engine to session
mySession$bind(engine(databaseName=testDB))

#query, limit and/or offset observations returned from query
mySession$query("snp") #prints entire table
mySession$query("snp.snp_id") #prints snp_id column in table
mySession$query("snp","snp.snp_id")
mySession$query("snp","snp.snp_id")$limit()[1:5,]
mySession$query("snp","snp.snp_id")$offset()[5:10,]

#order observations returned from query
mySession$query("snp") #prints entire table
mySession$query("snp")$orderBy("all_A")
mySession$query("snp")$orderBy("all_A","all_B")#complex orders

#filter observations returned from query
mySession$query("snp")$filterBy(all_A=="A") #equals
mySession$query("snp")$filterBy(all_A!="A") #not equals


#lists and scalars
mySession$query("snp") #prints entire table
(mySession$query("snp"))$all() #returns every observation
(mySession$query("snp"))$first() #returns first observation 

# count
mySession$query("snp")
mySession$query("snp")$count()

#joins
mySession$join("snp","genotype")# outerjoin
mySession$join("snp","genotype.genotype")
mySession$join("snp","genotype")$filterBy(snp.snp_id==genotype.snp_id)
mySession$innerjoin("snp","genotype",joinOn="snp.snp_id==genotype.snp_id")
mySession$leftjoin("snp","genotype",joinOn="snp.snp_id==genotype.snp_id")
mySession$rightjoin("snp","genotype",joinOn="snp.snp_id==genotype.snp_id")

#commit
SNPINSERT <- snp(snp_id='SNPINSERT', chr="1", All_A="T", All_B="C")
mySession$add(SNPINSERT)
mySession$commit()#inserts into snp table in the database a row with values mapped to SNPINSERT
mySession$query("snp")
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ session }
\keyword{ engine }
\keyword{ tableRecords }
\keyword{ bind }
\keyword{ query }
\keyword{ limit }
\keyword{ offset }
\keyword{ all }
\keyword{ one }
\keyword{ first }
\keyword{ scalar }
\keyword{ count }
\keyword{ filterBy }
\keyword{ orderBy }
\keyword{ add }
\keyword{ add_all }
\keyword{ delete }
\keyword{ delete_all }
\keyword{ ~kwd2 }

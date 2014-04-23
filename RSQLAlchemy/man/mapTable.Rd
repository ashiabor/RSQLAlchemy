\name{mapTable}
\alias{mapTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map a database table to a reference class
}
\description{
mapTable is a function that maps the structure of a database table to a reference class. It is the R equivalent of the mapping process in the python database toolkit, SQLAlchemy. The return type is a reference class generator whose field names match the schema of the corresponding database table.
}
\usage{
mapTable(tableName = "", columns = NULL, primaryKey = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tableName}{
name of database table to which returned class is linked
}
  \item{columns}{
schema description of columns of table
}
  \item{primaryKey}{
primary key(s) of table
}
}
\details{
A reference class generator is produced.
}
\references{
The python version, SQLAlchemy mapping function is documented at http://docs.sqlalchemy.org/en/rel_0_9/orm/mapper_config.html#sqlalchemy.orm.mapper
}
\author{
Awo Ashiabor
}
\examples{

##map a class snp to an snp database table with columns snp_id, chr, all_A, all_B all of ##which are character fields.
snp<-mapTable(tableName="snp",
               columns='snp_id="character",
                        chr="character",
                        all_A="character",
                        all_B="character"');
## with snp table mapped to snp class, generate instances of snp              
SNPA <- snp(snp_id='SNPA', chr="1", all_A="T", all_B="C")
SNPB <- snp(snp_id='SNPB', chr="1", all_A="T", all_B="C")

## view instances of snp class, access fields
SNPA
SNPB$snp_id

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mapTable }
\keyword{ RSQLAlchemy }
\keyword{ database  }
\keyword{ table  }

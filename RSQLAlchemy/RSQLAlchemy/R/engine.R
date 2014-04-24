## R_SQLAlchemy
## session
## Author: Awo Ashiabor
##
## engine is a class that provides a framework for connecting to sqlite databases
## An engine has one field databaseName
##  engine fields:
##                databaseName:=> name of sqlite database
##  engine Methods:  
##                "query", "insert"


.onLoad <- function(lib, pkg) {
   pckgPath <-system.file(package = "RSQLAlchemy")
   testDB <<- paste(pckgPath,"/exec/test.db",sep="")
}

engine<-setRefClass("engine", 
                    fields=list(databaseName="character"),
                    methods=list(initialize = function(...) {
                      callSuper(...)             
                      .self
                    }))
engine$accessors("databaseName");

engine$methods( 
  
  #query runs a sqlStatement on database and returns a dataframe of results
  query=function(sqlStatement){
    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname=.self$databaseName)
    result <- fetch(dbSendQuery(con, sqlStatement), n=-1) #return all results
    dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)
    dFrame <- as.data.frame(result)
    return(dFrame)
  },
  
  insert=function(tableName="",dFrame=NULL){
    m <- dbDriver("SQLite")
    con <- dbConnect(m, dbname=.self$databaseName)
    
    colLen=length(dFrame[1,]) 
    valuesStr<-"?"
    for(i in 2:colLen){valuesStr<-paste(valuesStr,",?")}
    insertStatement <- paste("insert into ",tableName," values (",valuesStr,")")
    dbGetPreparedQuery(con, insertStatement, dFrame)
    dbDisconnect(con)
  }
)
                    
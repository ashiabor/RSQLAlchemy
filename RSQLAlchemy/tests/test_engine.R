##TEST RSQLAlchemy

#setup
library("RSQLAlchemy")
library("RSQLite")


testDB<-paste(system.file("exec",package="RSQLAlchemy"),"/test.db",sep="",collapse="")
#create an engine connected to test.db
testEngine=engine(databaseName=testDB)
#connect to test.db using RSQLite
testDB=dbConnect(dbDriver("SQLite"), dbname=testDB)


#TEST engine

testEngineQuery <- function() {
  
  sql = 'select * from snp'
  expectedResult <-fetch(dbSendQuery(testDB, sql),n=-1)
  expected_types <- sapply(expectedResult,typeof)
  gotResult <- testEngine$query(sql)
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  checkIdentical(expected_types, sapply(gotResult, typeof))
  checkIdentical(expectedResult, gotResult)
  
  #clear db connections
  lapply(dbListResults(testDB), dbClearResult)
}

testEngineInsert <- function() {
  
  sql = 'select * from snp'
  sampleRecords <- fetch(dbSendQuery(testDB, sql), n=-1)[1:2,]
  sampleRecords[1,1]<-'SNPINSERT1'
  sampleRecords[2,1]<-'SNPINSERT2'
  testEngine$insert(tableName='snp',dFrame=sampleRecords)
  
  revisedTable <- testEngine$query('select * from snp')
  rowExists<-function(row,dFrame){
    nrow(merge(row,dFrame))>0
  }
  gotResult <- rowExists(sampleRecords,revisedTable)
  expectedResult <- TRUE
  
  checkIdentical(expectedResult, gotResult)
  #clear db connections
  lapply(dbListResults(testDB), dbClearResult)
}

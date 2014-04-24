
##TEST RSQLAlchemy

#setup
library("RSQLAlchemy")
library("RSQLite")

testDB<-paste(system.file("exec",package="RSQLAlchemy"),"/test.db",sep="",collapse="")
#create an engine connected to test.db
testEngine=engine(databaseName=testDB)
#connect to test.db using RSQLite
testDB=dbConnect(dbDriver("SQLite"), dbname=testDB)

#TEST SESSION
testSessionBind <- function() {
  
  mySession<-session()
  mySession$bind(testEngine)  
  
  expectedResult<-testEngine
  gotResult <- mySession$engine
  
  checkIdentical(expectedResult, gotResult)
  
}

testSessionQuery <- function() {
  
  mySession<-session()
  mySession$bind(testEngine)  
  
  expectedResult<-testEngine$query('select * from snp')
  gotResult <- (mySession$query("snp"))$dataFrame
  
  checkIdentical(expectedResult, gotResult)
  
}

testSessionJoin <- function() {
  
  mySession<-session()
  mySession$bind(testEngine) 
  
  
  sql = 'select * from snp join genotype on 1=1'
  expectedResult <-fetch(dbSendQuery(testDB, sql), n=-1)
  
  fetchedTable<-mySession$join("snp","genotype")  
  gotResult <- fetchedTable$dataFrame
  #simplify name from tbl.fieldName to fieldName
  namesGotResult<-names(gotResult)
  fetchTblName<-function(compoundName) {sub(".*\\.","",compoundName)}
  names(gotResult)<-lapply(namesGotResult, fetchTblName)
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  checkIdentical(apply(expectedResult,2,sort,decreasing=F), apply(gotResult,2,sort,decreasing=F))  
  
  #clear db connections
  lapply(dbListResults(testDB), dbClearResult)
  
}

testSessionInnerJoin <- function() {
  
  mySession<-session()
  mySession$bind(testEngine) 
    
  sql = 'select * from snp inner join genotype on snp.snp_id==genotype.snp_id'
  expectedResult <-fetch(dbSendQuery(testDB, sql), n=-1)
  #remove duplicate columns
  expectedResult<-expectedResult[duplicated(lapply(expectedResult, summary))]
  
  fetchedTable<-mySession$innerjoin("snp","genotype",joinOn="snp.snp_id==genotype.snp_id")
  gotResult <- fetchedTable$dataFrame
  #simplify name from tbl.fieldName to fieldName
  namesGotResult<-names(gotResult)
  fetchTblName<-function(compoundName) {sub(".*\\.","",compoundName)}
  names(gotResult)<-lapply(namesGotResult, fetchTblName)
  
  checkIdentical(dim(expectedResult), dim(gotResult))

  #clear db connections
  lapply(dbListResults(testDB), dbClearResult)
  
}

testSessionLeftJoin <- function() {
  
  mySession<-session()
  mySession$bind(testEngine) 
    
  sql = 'select * from snp left join genotype on snp.snp_id==genotype.snp_id'
  expectedResult <-fetch(dbSendQuery(testDB, sql), n=-1)
  #remove duplicate columns
  expectedResult<-expectedResult[duplicated(lapply(expectedResult, summary))]
  
  fetchedTable<-mySession$leftjoin("snp","genotype",joinOn="snp.snp_id==genotype.snp_id")
  gotResult <- fetchedTable$dataFrame
  #simplify name from tbl.fieldName to fieldName
  namesGotResult<-names(gotResult)
  fetchTblName<-function(compoundName) {sub(".*\\.","",compoundName)}
  names(gotResult)<-lapply(namesGotResult, fetchTblName)
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  
  #clear db connections
  lapply(dbListResults(testDB), dbClearResult)
  
}

testSessionRightJoin <- function() {
  
  mySession<-session()
  mySession$bind(testEngine) 
  
  sql = 'select * from genotype left join snp on snp.snp_id==genotype.snp_id'
  expectedResult <-fetch(dbSendQuery(testDB, sql), n=-1)
  #remove duplicate columns
  expectedResult<-expectedResult[duplicated(lapply(expectedResult, summary))]
  
  fetchedTable<-mySession$rightjoin("snp","genotype",joinOn="snp.snp_id==genotype.snp_id")
  gotResult <- fetchedTable$dataFrame
  #simplify name from tbl.fieldName to fieldName
  namesGotResult<-names(gotResult)
  fetchTblName<-function(compoundName) {sub(".*\\.","",compoundName)}
  names(gotResult)<-lapply(namesGotResult, fetchTblName)
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  
  #clear db connections
  lapply(dbListResults(testDB), dbClearResult)
  
}
testSessionAdd <- function() {
  
  mySession<-session()
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                        chr="character",
                        all_A="character",
                        all_B="character"');
  
  SNPTEST <- snp(snp_id='SNPTEST', chr="1", all_A="T", all_B="C")
  
  prevObjects<-mySession$objects
  mySession$add(SNPTEST)
  currObjects<-mySession$objects
  
  prevObjects[["SNPTEST"]]<-SNPTEST
  expectedResult <-prevObjects
  gotResult <- currObjects
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  checkIdentical(expectedResult, gotResult)
  
}
testSessionAddToReplace <- function() {
  
  mySession<-session()
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                        chr="character",
                        all_A="character",
                        all_B="character"');
  
  SNPTEST <- snp(snp_id='SNPTEST', chr="1", all_A="T", all_B="C")
  mySession$add(SNPTEST)
  prevSavedSNPTEST<-mySession$objects[["SNPTEST"]]
  
  SNPTEST$snp_id<-"SNPCHANGE"
  mySession$add(SNPTEST)
  currSavedSNPTEST<-mySession$objects[["SNPTEST"]]
    
  gotResult <- currSavedSNPTEST$snp_id
  expectedResult <-"SNPCHANGE"
  
  checkIdentical(expectedResult, gotResult)
  
}
testSessionAddAll <- function() {
  
  mySession<-session()
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                        chr="character",
                        all_A="character",
                        all_B="character"');
  
  SNPA <- snp(snp_id='SNPA', chr="1", all_A="T", all_B="C")
  SNPB <- snp(snp_id='SNPB', chr="1", all_A="T", all_B="C")
  
  prevObjects<-mySession$objects
  mySession$add_all(SNPA,SNPB)
  currObjects<-mySession$objects
  
  prevObjects[["SNPA"]]<-SNPA
  prevObjects[["SNPB"]]<-SNPB
  expectedResult <-prevObjects
  gotResult <- currObjects
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  
}
testSessionDelete <- function() {
  
  mySession<-session()
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                chr="character",
                all_A="character",
                all_B="character"');
  
  SNPTEST <- snp(snp_id='SNPTEST', chr="1", all_A="T", all_B="C")  
  mySession$add(SNPTEST) 
  
  prevObjects<-mySession$objects  
  mySession$delete(SNPTEST)
  currObjects<-mySession$objects
  
  gotResult <- currObjects[["SNPTEST"]]
  expectedResult <-NULL
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  checkIdentical(expectedResult, gotResult)
  
}
testSessionDeleteAll <- function() {
  
  mySession<-session()
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                chr="character",
                all_A="character",
                all_B="character"');
  
  SNPA <- snp(snp_id='SNPA', chr="1", all_A="T", all_B="C")
  SNPB <- snp(snp_id='SNPB', chr="1", all_A="T", all_B="C") 
  mySession$add_all(SNPA,SNPB)
  
  prevObjects<-mySession$objects  
  mySession$delete_all(SNPA,SNPB)
  currObjects<-mySession$objects
  
  gotResult <- c(currObjects[["SNPA"]],currObjects[["SNPB"]])
  expectedResult <-NULL
  
  checkIdentical(dim(expectedResult), dim(gotResult))
  checkIdentical(expectedResult, gotResult)
  
}

testSessionCommit <- function() {
  
  mySession<-session()
  mySession$bind(testEngine)  
  
  sql = 'delete from snp where snp_id="SNPINSERT"'
  expectedResult <-dbSendQuery(testDB, sql)
  
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                        chr="character",
                        all_A="character",
                        all_B="character"');
  SNPINSERT <- snp(snp_id='SNPINSERT', chr="1", all_A="T", all_B="C")
  mySession$add(SNPINSERT)
  prevInTable<-(mySession$query("snp"))$filterBy(snp_id=="SNPINSERT")
  
  mySession$commit()
  
  insertInTable <- (mySession$query("snp"))$filterBy(snp_id=="SNPINSERT")
  gotResult <- insertInTable$dataFrame
  expectedResult<-SNPINSERT$dataFrame
  
  is.null(dim(prevInTable))
  checkIdentical(dim(expectedResult), dim(gotResult))
  sapply(1:ncol(gotResult),
         function(i) {checkEquals(gotResult[,i],expectedResult[[i]])})
  
}
testSessionCommitPrimaryKeyViolation <- function() {
  
  mySession<-session()
  mySession$bind(testEngine)  
  
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                chr="character",
                all_A="character",
                all_B="character"',
                primaryKey="snp_id");
  SNP1 <- snp(snp_id='SNP1', chr="1", all_A="T", all_B="C")
  SNP1Copy <- snp(snp_id='SNP1', chr="1", all_A="T", all_B="C")
  
  mySession$add_all(SNP1,SNP1Copy)
  
  checkEquals(tryCatch(mySession$commit(),
           warning=function(e)TRUE,
           error=function(e)TRUE),TRUE)
  
}

#clear db connections
lapply(dbListResults(testDB), dbClearResult)
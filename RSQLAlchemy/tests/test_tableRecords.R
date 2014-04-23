##TEST RSQLAlchemy

#setup
library("RSQLAlchemy")
library("RSQLite")

## TEST tableRecords
testTableRecordsLimit <- function() {
  
  quakesTableRecords<-tableRecords(dataFrame=quakes)
  
  expectedResult<-quakes[1:3,]
  gotResult <- quakesTableRecords$limit()[1:3,]
  
  checkIdentical(expectedResult, gotResult)
  
}
testTableRecordsOffset <- function() {
  
  quakesTableRecords<-tableRecords(dataFrame=quakes) 
  
  expectedResult<-quakes[2:4,]
  gotResult <- quakesTableRecords$offset()[2:4,]
  
  checkIdentical(expectedResult, gotResult)
  
}

testTableRecordsSimpleOrderBy <- function() {
  
  quakesTableRecords<-tableRecords(dataFrame=quakes[1:20,]) 
  
  orderedTable<-quakesTableRecords$orderBy("stations")
  sortedColumn<-orderedTable$dataFrame[,"stations"]
  
  expectedResult<-TRUE
  gotResult <- !is.unsorted(sortedColumn, na.rm = FALSE, strictly = FALSE)
  
  checkIdentical(expectedResult, gotResult)
  
}
testTableRecordsFilterBy <- function() {
  
  quakesTableRecords<-tableRecords(dataFrame=quakes) 
  
  filteredTable<-quakesTableRecords$filterBy(stations==14)
  
  expectedResult<-quakes[quakes$stations==14,]
  gotResult<-filteredTable$dataFrame
  
  checkIdentical(expectedResult, gotResult)
  
}
testTableRecordsAll <- function() {
  
  quakesTableRecords<-tableRecords(dataFrame=quakes) 
  
  fetchedTable<-quakesTableRecords$all()
  
  expectedResult<-quakes[]
  gotResult<-fetchedTable$dataFrame
  
  checkIdentical(expectedResult, gotResult)
  
}
testTableRecordsFirst <- function() {
  
  quakesTableRecords<-tableRecords(dataFrame=quakes) 
  
  fetchedTable<-quakesTableRecords$first()
  
  expectedResult<-quakes[1,]
  gotResult<-fetchedTable$dataFrame
  
  checkIdentical(expectedResult, gotResult)
  
}
testTableRecordsCount <- function() {
  
  quakesTableRecords<-tableRecords(dataFrame=quakes) 
    
  expectedResult<-length(quakes[,1])
  gotResult<-quakesTableRecords$count()
  
  checkIdentical(expectedResult, gotResult)
  
}
testTableRecordsOne <- function() {
  
  #table with one record, returns the table with one record
  myquakes<-quakes[1,]
  quakesTableRecords<-tableRecords(dataFrame=myquakes)   
  expectedResult<-quakesTableRecords
  gotResult<-quakesTableRecords$one()  
  checkIdentical(expectedResult, gotResult)
  
  #table with multiple records, returns message "Multiple rows found"
  myquakes<-quakes
  quakesTableRecords<-tableRecords(dataFrame=myquakes)   
  expectedResult<-"Multiple rows found"
  gotResult<-quakesTableRecords$one()  
  checkIdentical(expectedResult, gotResult)
  
  #table with no records, returns message "No rows found"
  myquakes<-quakes[FALSE,]
  quakesTableRecords<-tableRecords(dataFrame=myquakes)   
  expectedResult<-"No rows found"
  gotResult<-quakesTableRecords$one()  
  checkIdentical(expectedResult, gotResult)
  
}

testTableRecordsScalar <- function() {
  
  #table with one record, returns the table with one record
  myquakes<-quakes[1,]
  quakesTableRecords<-tableRecords(dataFrame=myquakes)   
  expectedResult<-quakesTableRecords
  gotResult<-quakesTableRecords$scalar()  
  checkIdentical(expectedResult, gotResult)
  
  #table with multiple records, returns nothing
  myquakes<-quakes
  quakesTableRecords<-tableRecords(dataFrame=myquakes)   
  expectedResult<-NULL
  gotResult<-quakesTableRecords$scalar()  
  checkIdentical(expectedResult, gotResult)
  
  #table with no records, returns message "No rows found"
  myquakes<-quakes[FALSE,]
  quakesTableRecords<-tableRecords(dataFrame=myquakes)   
  expectedResult<-"No rows found"
  gotResult<-quakesTableRecords$scalar()  
  checkIdentical(expectedResult, gotResult)
  
}

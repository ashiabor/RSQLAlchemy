## R_SQLAlchemy
## tableRecords
## Author: Awo Ashiabor
##
## tableRecords is a class that provides database query methods such as filter, orderby, count, etc...
## session results are encapsulated as tableRecords to enable further query processes such as filters and so on..
##
## A tableRecords has one field dataFrame
##  tableRecords fields:
##                dataFrame:=> dataFrame representation of database table
##  tableRecords Methods:  
##                "orderBy", "limit", "offset", filterBy", 
##                "all", "first", "one", scalar", "count"
##

tableRecords <- setRefClass("tableRecords",
                        fields = list(dataFrame="data.frame"
                                      ),
                        methods=list(initialize = function(...) {
                          callSuper(...)             
                          .self
                        }))

tableRecords$accessors("dataFrame");

tableRecords$methods( 
  
  #orders by asc order
  orderBy=function(...){
    sortnames <- c(...)
    dFrame<-dataFrame[do.call("order", dataFrame[sortnames]), ]
    tableRecords(dataFrame=dFrame)#return tableRecords
  },
  
  limit=function(){
    dataFrame #returns dataframe for subset by []
  },
  
  offset=function(){
    dataFrame #return dataframe for offset by []
  },
  
  filterBy=function(...){#takes multiple filter functions and combinations    
        
    #define and redefine condition functions
    `%like%` <- function (x, y) {grepl(y,x)}    
    `%!like%` <- function (x, y) {!grepl(y,x)} 
    `%!in%` <- function (x, y) {!(x%in%y)}
    is.null <- function (x) {
      i <- sapply(x, function(x) length(x)==0)
      x[i] <- lapply(x[i], function(x) "NA")
      return(x=="NA")}    
   
    dFrame<-subset(dataFrame,...) 
    tableRecords(dataFrame=dFrame)
  },
  
  all=function(){#returns all records
    
    nrows=length(dataFrame[,1])
    if(nrows== 0) warning('No rows found')
    else{.self }   
  },
  
  first=function(){#returns first record
    nrows=length(dataFrame[,1])
    if(nrows== 0) warning('No rows found')
    else{
      filterFirst<-c(TRUE,rep(FALSE,nrows-1)) #vector [TRUE,FALSE,FALSE,...,FALSE]
      dFrame<-subset(dataFrame,filterFirst)
      tableRecords(dataFrame=dFrame)}
  },
  
  one=function(){#returns one record if only one record available else error messages
    nrows=length(dataFrame[,1])
    if(nrows== 0) warning('No rows found')
    else if(nrows > 1) warning( 'Multiple rows found')
    else{.self}
  },
  
  scalar=function(){#returns one record if only one record available; no error messages
    nrows=length(dataFrame[,1])
    if(nrows== 0) warning('No rows found')
    else if(nrows== 1){.self}
  },
  
  count=function(){#returns errors if no records
    nrows=length(dataFrame[,1])
    if(nrows== 0) warning('No rows found')
    else{nrows}
  }
  
)



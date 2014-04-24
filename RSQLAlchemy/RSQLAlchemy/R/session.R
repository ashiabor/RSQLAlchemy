## R_SQLAlchemy
## session
## Author: Awo Ashiabor
##
## session is a class that provides a framework for managing RSQLAlchemy objects
##
## A session has fields engine and objects
##  session fields:
##                engine:=> abstraction of database connection
##                objects:=> a store of instances of classes created and added to session 
##  session Methods:  
##                "add", "add_all", "delete", delete_all", bind", commit", "select", "query"
##                "join", "outerjoin", "innerjoin", "leftjoin", "rightjoin"
##
##  OTHER DETAILS
##------------------------------------------------------------------------------------
## 1. Initialize a session:
## > mySession<-session()
##
## 2. Connect session to sql database
## > mySession$bind(testEngine)
##  
## 3. Add objects to session -> temporary store instances before comitting to database
## > snp<-mapObject(tableName="snp",
##                  columns='snp_id="character",
##                           chr="character",
##                           all_A="character",
##                           all_B="character"',
##                  primaryKey="snp_id");
## > SNP01 <- snp(snp_id='SNP01', chr="1", all_A="T", all_B="C")
## > mySession$objects
##      list()
## > length(mySession$objects)
##      [1] 0
## > mySession$add(SNP01)
## > length(mySession$objects)
##      [1] 1
## > SNP02 <- snp(snp_id='SNP02', chr="1", all_A="A", all_B="T")
## > SNP03 <- snp(snp_id='SNP03', chr="5", all_A="A", all_B="C")
## > mySession$add_all(SNP01,SNP02,SNP03) #SNP01 is replaced; 3 session objects instead of 4
## > length(mySession$objects)
##    [1] 3 
##
## 4. Commit session objects to database -> send objects to sqlite database
## > mySession$commit()
##
## 5. Query tables in session
## > mySession$query('snp') ... etc...
##

session <- setRefClass("session",
                       fields = list( engine="engine",
                                      objects="list"),
                      methods=list(initialize = function(...) {
                                      callSuper(...)
                                      .self
                                    })
          )

session$methods( 
  
  bind=function(engn){
    .self$engine <- engn
  },
  
  add=function(obj,name=NULL){
    .self$objects[[deparse(substitute(obj))]] <- obj
  },
  
  add_all=function(...){
     objnames <- as.list(substitute(list(...)))[-1L]
     setNames(list(...), objnames)
     
     addByName<-function(name) {
                .self$objects[[as.character(name)]] <- eval(parse(text=name))}
     
     invisible(sapply(objnames, addByName))
   },
  
  delete=function(obj,name=NULL){
    .self$objects[[deparse(substitute(obj))]] <- NULL
  },
  
  delete_all=function(...){
    objnames <- as.list(substitute(list(...)))[-1L]
    setNames(list(...), objnames)
    
    deleteByName<-function(name) {
                  .self$objects[[as.character(name)]] <- NULL}
    
    invisible(sapply(objnames, deleteByName ))
  },

  commit = function() {
    
    #save object in central location, a dataframe named tblName_MASTER
    saveCentral = function(obj) {
      record<-obj$dataFrame
      
      centralLocnName<-paste(class(obj),"_MASTER", collapse="", sep="")
      centralLocn<-RSQLAlchemyControl$variables[[centralLocnName]]
      centralLocn<-rbind(centralLocn,record)
      
      #enforce primary key if tbl has primary key      
      if(!is.null(obj$primaryKey)){
                
        primaryKeyNames<-paste(obj$primaryKey, collapse=", ")
        primaryKeyFields <- c(obj$primaryKey)
        primaryKeyValue<-record[,primaryKeyFields] 
        
        tryCatch(
          
          #index primary keys on rownames
          rownames(centralLocn) <- apply(as.matrix(centralLocn[,primaryKeyFields]),
                                         1,
                                         function(x) paste(x,sep="",collapse="")),
          warning=function(e){
           stop("Primary key violation: non-unique value ", primaryKeyValue," in [",primaryKeyNames,"]")},
          error=function(e){
            stop("Primary key violation: non-unique value ", primaryKeyValue," in [",primaryKeyNames,"]")}
        )
      }
      
      #replace factors with characters
      i <- sapply(centralLocn, function(x) is.factor(x))
      centralLocn[i] <- lapply(centralLocn[i], as.character)
      
      #save revised central table
      RSQLAlchemyControl$addVariable(centralLocnName, centralLocn)
    }
    
    #save all session objects in central location
    sapply(.self$objects, saveCentral)
    
    #inserts  into database all instances of a given class (i.e. all records of a table)
    insertCls=function(cls){ 
      centralLocnName<-paste(cls,"_MASTER", collapse="", sep="")
      recordsToInsert<-RSQLAlchemyControl$variables[[centralLocnName]]
      (.self$engine)$insert(tableName=cls,dFrame=recordsToInsert)
    }
    
    #insert all instances of all classes into to database
    clsList=sapply(.self$objects, function(obj) class(obj))    
    sapply(clsList, insertCls)
    
    print("Records committed")
  },
  
  #selects from database tables
  select = function(...){
    
    fields<-list(...)
    myEngine <- .self$engine
    resultsByTbl<-list() #selections grouped by database table
    
    for(field in fields){ 
      
      sel<-data.frame()
      tblName<-sub("\\..*","",field)#fetch tbl from "tbl.field"
      
      #if first time selecting from table reset counter
      if(is.null(resultsByTbl[[tblName]])){count<-1}
      
      #if field is specified fetch field only
      if(grepl("\\.",field)){
        fieldName<-sub(".*\\.","",field)
        sel<-as.data.frame(myEngine$query(paste('select * from ',tblName))[,fieldName])
        names(sel)<-paste(tblName,fieldName,sep=".")
        
      }else{ #list all columns of table
        sel<-myEngine$query(paste('select * from ',tblName))          
        names(sel)<-paste(tblName,names(sel),sep=".")
      }
      
      #group selections by table
      if(count==1){resultsByTbl[[tblName]]<-sel}
      else{ resultsByTbl[[tblName]]<-cbind(resultsByTbl[[tblName]],sel)}
      count<-count+1
    }
    return(resultsByTbl)
    
  },
  
  
  #query returns a tableRecords representation of results of query
  query = function(...){
      Results<-data.frame()
      resultsByTbl<-.self$select(...) #queried fields grouped by database table
      
      
      #prepare final output by joining fields from multiple tables
      for(result in resultsByTbl){
        if(length(Results)==0){Results<-result}
        else{Results<-merge(x = Results, y = result,by=NULL)#outerjoin the different classes
        }
      }
      
      #if only one table queried, strip tableName from names of result columns    
      if(length(resultsByTbl)==1){
        namesResult<-names(Results)
        fetchTblName<-function(compoundName) {sub(".*\\.","",compoundName)}
        names(Results)<-lapply(namesResult, fetchTblName)
      }
      
          
      #replace factors with characters to allow future subsetting
      i <- sapply(Results, is.factor)
      Results[i] <- lapply(Results[i], as.character)
      
      return(tableRecords(dataFrame=Results)) 
      
    },
  
  #joins
  join=function(...){#outerjoin    
    .self$query(...)
  },
  outerjoin=function(...){#outerjoin    
    .self$query(...)
  },
  innerjoin=function(...,joinOn=""){
  #innerjoin function currently only accepts a==b join clauses 
  #function limited to join on two tables only
    
    if(!(grepl("\\=\\=",joinOn))) stop("incompatible join clause")
     
    #left side table is left of "=="
    byX<-sub("\\=\\=.*","",joinOn) #field to join from left table
    xTbl<-sub("\\..*","",byX) #left table
    
    #right side table
    byY<-sub(".*\\=\\=","",joinOn)
    yTbl<-sub("\\..*","",byY)
    
    Results<-data.frame()
    resultsByTbl<- .self$select(...) #queried fields grouped by database table    
    
    #inner join tables    
    Results<-merge(x = resultsByTbl[[xTbl]], y = resultsByTbl[[yTbl]], 
                       by.x =byX, by.y = byY)#innerjoin the tables
      
    
    #replace factors with characters to allow future subsetting
    i <- sapply(Results, is.factor)
    Results[i] <- lapply(Results[i], as.character)
    
    return(tableRecords(dataFrame=Results)) 
  },
  
  leftjoin=function(..., joinOn=""){
    #leftjoin function currently only accepts a==b join clauses 
    #function limited to join on two tables only 
    
    if(!(grepl("\\=\\=",joinOn))) stop("incompatible join clause")
    
    xTbl<-list(...)[[1]]
    yTbl<-list(...)[[2]]
    clause1<-sub("\\=\\=.*","",joinOn)    
    clause2<-sub(".*\\=\\=","",joinOn)
    
    
    if(grepl(xTbl, clause1)){
      byX <- clause1;
      byY <- clause2;
    }else{
      byX <- clause2;
      byY <- clause1;
      
    }
    
    Results<-data.frame()
    resultsByTbl<- .self$select(...) #queried fields grouped by database table    
    
    #left join tables    
    Results<-merge(x = resultsByTbl[[xTbl]], y = resultsByTbl[[yTbl]], 
                       by.x =byX, by.y = byY, all.x=TRUE)#leftjoin the tables
    
    
    #replace factors with characters to allow future subsetting
    i <- sapply(Results, is.factor)
    Results[i] <- lapply(Results[i], as.character)
    
    return(tableRecords(dataFrame=Results))  
  },
  rightjoin=function(...,joinOn=""){ 
    #rightjoin function currently only accepts a==b join clauses 
    #function limited to join on two tables only 
    
    if(!(grepl("\\=\\=",joinOn))) stop("incompatible join clause")
    
    xTbl<-list(...)[[1]]
    yTbl<-list(...)[[2]]
    clause1<-sub("\\=\\=.*","",joinOn)    
    clause2<-sub(".*\\=\\=","",joinOn)
    
    
    if(grepl(xTbl, clause1)){
      byX <- clause1;
      byY <- clause2;
    }else{
      byX <- clause2;
      byY <- clause1;
      
    }
    
    Results<-data.frame()
    resultsByTbl<- .self$select(...) #queried fields grouped by database table    
    
    #right join tables    
    Results<-merge(x = resultsByTbl[[xTbl]], y = resultsByTbl[[yTbl]], 
                       by.x =byX, by.y = byY, all.y=TRUE)#rightjoin the tables
    
    
    #replace factors with characters to allow future subsetting
    i <- sapply(Results, is.factor)
    Results[i] <- lapply(Results[i], as.character)
    
    return(tableRecords(dataFrame=Results))  
  }
)

session$accessors("objects");
session$accessors("engine");
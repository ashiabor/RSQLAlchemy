## R_SQLAlchemy
## mapTable
## Author: Awo Ashiabor
##
## mapTable is a function that maps a database table to a reference class
## mapTable is the R equivalent of the Mapping process in the Python version of SQL Alchemy
## The return type is a reference class generator with fields resembling schema of the database table 
##
## E.g. database table snp shown below 
## snp
##_______
##  snp_id  chr  all_A   all_B
##   SNP01   1     T     C
##   SNP02   1     A     T
##
## can be mapped to a class called snp using the command
## > snp<-mapTable(tableName="snp",
##                columns='snp_id="character",
##                         chr="character",
##                         all_A="character",
##                         all_B="character"',
##                primaryKey="snp_id");
##
## > snp
##  Generator for class "snp":
##  Class fields:
##              Name:  primaryKey  snp_id    chr         all_A     all_B     dataFrame
##              Class: ANY         character character   character character activeBindingFunction
##
## The returned class has fields: snp_id, chr, all_A and all_B corresponding to columns of database table
##
##
## ADDITIONAL DETAILS
##-------------------------------------------------------------------------------------------------
## 1. The returned class also has a field "primaryKey" for tracking primary key(s) of the database table.
##    Compound primary keys are entered as a list e.g. c("PrimaryKey1","PrimaryKey2")
##
## 2. The returned class includes a dataFrame field which holds a dataframe of the contents of the table.
##    The dataFrame field is implemented as an activeBinding field. This implementation was chosen to allow
##    future updates to the table to be automatically reflected in the dataframe
##      E.g.
##      > SNP01 <- snp(snp_id='SNP01', chr="1", all_A="T", all_B="C")
##      > SNP01$dataFrame
##                  snp_id  chr all_A all_B
##            SNP01 "SNP01"  "1" "T"   "C"
##
##      The chr of SNP01 is changed and the change is automatically reflected in the dataframe field
##      > SNP01$chr="2"
##      > SNP01$dataFrame
##              snp_id  chr all_A all_B
##       SNP01 "SNP01" "2" "T"   "C"
##
##
## 3. mapTable also creates a central location to store all instances of the mapped class. 
##    The central location is implemented as a dataFrame named tableName_Master.


mapTable <- function (tableName="",columns=NULL,primaryKey=NULL){
  #create a repository for internal variables
  if (!(exists("RSQLAlchemyControl"))) { assign("RSQLAlchemyControl", control$new(), envir = .GlobalEnv)}
  
  #create master table for class
  masterTableName<-paste(tableName,"_MASTER",sep="")
  masterTable<-eval(parse(text=paste('data.frame(',columns,')[FALSE,]')))
  RSQLAlchemyControl$addVariable(masterTableName, masterTable)
  
  #note primaryKey
  primaryKeyName<-paste(tableName,"_primaryKey",sep="")
  RSQLAlchemyControl$addVariable(primaryKeyName, primaryKey)
  
  #function to update dataFrame field with current values of class fields
  updateDataFrame <- function(){
    
    listFields<-rbind(.self$getRefClass()$fields())    
    #exclude dataFrame and primaryKey fields
    listFields<-subset(listFields, select=-c(dataFrame,primaryKey))[1,]    
    #create field<->value pairs
    dFrame<-rbind(sapply(names(listFields), function(x) {.self[[x]]}))

    return(dFrame)
  }
  
  #return mapped class with fields defined by schema of database table
  classDefn<-setRefClass(tableName,
                         eval(parse(text= paste(' fields = list(primaryKey="ANY",',
                                                columns,
                                                ',dataFrame=updateDataFrame)')))
                         ,methods=list(initialize = function(...) {
                           callSuper(...)                           
                           primaryKeyName<-paste(.self$getRefClass()$className,"_primaryKey",sep="")
                           .self$primaryKey <<- RSQLAlchemyControl$variables[[primaryKeyName]]
                           .self
                         }),
                         where=.GlobalEnv)
  
  return(classDefn)
  
}

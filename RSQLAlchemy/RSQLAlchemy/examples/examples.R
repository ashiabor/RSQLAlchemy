### CONTROL
# initialize a control
myControl = control$new()
# set a variable
myControl$addVariable("projectName", "RSQLAlchemy")
# fetch a variable
myControl$variables[["projectName"]] #returns 'RSQLAlchemy'


### ENGINE
# initialize an engine
testDB<-paste(system.file("exec",package="RSQLAlchemy"),"/test.db",sep="",collapse="")
myEngine=engine(databaseName=testDB)
# run a sql statement to read from database
myEngine$query('select * from snp')
# write to a database
twoTableRecords<-as.data.frame(cbind(snp_id=c("SNP01","SNP02"),chr = c("1","2"),all_A=c("A","T"),all_B=c("C","C")))
myEngine$insert(tableName='snp',dFrame=twoTableRecords)


### MAPTABLE
#map snp table structure to snp class
snp<-mapTable(
  tableName="snp",
  columns='snp_id="character",chr="character",all_A="character",all_B="character"',
  primaryKey="snp_id");
#create instances of snp (records for snp table)
SNPA <- snp(snp_id='SNPA', chr="1", all_A="T", all_B="C")



### TABLERECORDS
# create a tableRecords using quakes data
quakesTableRecords <- tableRecords(dataFrame = quakes)
## list and scalars (sampling records)
quakesTableRecords #prints entire table
quakesTableRecords$count()
quakesTableRecords$all() #returns every observation
quakesTableRecords$first() #returns first observation
quakesTableRecords$limit()[1:5, ]
quakesTableRecords$offset()[5:10, ]
# order records
quakesTableRecords$orderBy("stations")
quakesTableRecords$orderBy("stations", "mag") #multiple columns
# filter observations returned from query
quakesTableRecords$filterBy(stations == 15) #equals
quakesTableRecords$filterBy(stations != 15) #not equals
quakesTableRecords$filterBy(stations %like% "1") #like
quakesTableRecords$filterBy(stations %!like% "1") #not like
quakesTableRecords$filterBy(stations %in% c(10, 15)) #in
quakesTableRecords$filterBy(stations %!in% c(10, 15)) #not in
quakesTableRecords$filterBy(is.null(stations)) #is null
quakesTableRecords$filterBy(!is.null(stations)) #is not null
quakesTableRecords$filterBy(stations == 15 | mag > 4) #or
quakesTableRecords$filterBy(stations == 15 & mag > 4) #and


### SESSION
# start a session
mySession <- session()
# save or delete objects from session
mySession$add(SNPA) # add_all() saves multiple objects at once
mySession$delete(SNPA) # delete_all() removes multiple objects


# bind an engine to session
mySession$bind(engine(databaseName = testDB))

# query database
mySession$query("snp") #prints entire table
mySession$query("snp.snp_id") #prints snp_id column in table
mySession$query("snp", "snp.snp_id")
# process query results
mySession$query("snp")$orderBy("all_A") #tableRecords$orderBy() method applied to results of session$query()
mySession$query("snp")$filterBy(all_A == "A")
mySession$query("snp")$count()

# joins
mySession$join("snp", "genotype") # outerjoin
mySession$join("snp", "genotype")$filterBy(snp.snp_id == genotype.snp_id)
mySession$innerjoin("snp", "genotype", joinOn = "snp.snp_id==genotype.snp_id")
mySession$leftjoin("snp", "genotype", joinOn = "snp.snp_id==genotype.snp_id")
mySession$rightjoin("snp", "genotype", joinOn = "snp.snp_id==genotype.snp_id")

# commit
SNPINSERT <- snp(snp_id = "SNPINSERT", chr = "1", all_A = "T", all_B = "C")
mySession$add(SNPINSERT)
mySession$commit() #inserts into snp table in database a row with values associated wtih SNPINSERT
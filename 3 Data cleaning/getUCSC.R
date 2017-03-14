# ########################################################################################
# Get data from UCSC db
# 
# ########################################################################################


# ########################################################################################
# Import
# ########################################################################################

library(RMySQL)

###########################################################################################
# Connect to UCSC db

#Connect to server, list databases, close connection
dbUCSC <- dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
listDatabases<-dbGetQuery(dbUCSC,"show databases;")
dbDisconnect(dbUCSC)

head(listDatabases)


#Connect to database
db_hg19 <- dbConnect(MySQL(),user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")


#List tables
schema_hg19 <- dbListTables(db_hg19)
head(schema_hg19)

#List fields
fields_affyU133Plus2 <- dbListFields(db_hg19,"affyU133Plus2")
head(fields_affyU133Plus2)

#Query and get entire result
size_affyU133Plus2 <- dbGetQuery(db_hg19, "select count(*) from affyU133Plus2")
size_affyU133Plus2

#Query and result partial or complete
query <- dbSendQuery(db_hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query);
affyMisSmall <- fetch(query,n=10);
dbClearResult(query)
head(affyMisSmall)

# Read entire table
data_affyU133Plus2 <- dbReadTable(db_hg19, "affyU133Plus2")
head(data_affyU133Plus2)

#Close connection
dbDisconnect(db_hg19)



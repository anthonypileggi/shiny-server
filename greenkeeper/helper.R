library(mongolite)
library(magrittr)
library(ggplot2)

# Define the fields we want to save from the form
fields <- c("name", "brand", "type", "quantity", "cost")


# Setup MongoDB
options(mongodb = list(
  "host" = "ds159050.mlab.com:59050",
  "username" = "anthony",
  "password" = "thisisanthony"
))

databaseName <- "greenkeeper"
collectionName <- "inventory"


connectMongoDB <- function () {
  mongo(collection = collectionName,
         url = sprintf(
           "mongodb://%s:%s@%s/%s",
           options()$mongodb$username,
           options()$mongodb$password,
           options()$mongodb$host,
           databaseName))
}

# get ID for most recent inventory entry
getCurrentID <- function () {
  db <- connectMongoDB()
  x <- db$find('{}', fields = '{"_id":0, "id":1}')
  if (ncol(x) == 0) {
    return (0)
  } else {
    return (max(x[['id']], na.rm=TRUE))
  }
}

# save inventory entry to mongoDB
saveData <- function (data) {
  # Connect to the database
  db <- connectMongoDB()
  
  # Add some additional fields
  data <- as.data.frame(t(data))
  # - add id
  data$id <- getCurrentID() + 1
  # - add timestamp
  data$date <- as.numeric(Sys.time())
  
  # Insert the data into the mongo collection as a data.frame
  db$insert(data)
}


# load all inventory data from mongoDB
loadData <- function() {
  # Connect to the database
  db <- connectMongoDB()
  # Read all the entries
  data <- db$find()
  if (nrow(data)>0) {
    data[,c('id','name','brand','type','quantity','cost', 'date')]
  } else {
    NULL
  }
}
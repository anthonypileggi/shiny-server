library(mongolite)
library(magrittr)
library(ggplot2)

# Define the fields we want to save from the form
fields <- list (
            inventory = c("name", "brand", "type", "quantity", "cost"),
            consumption = c("id", "amount", "timestamp")
            )


# Setup MongoDB
options(mongodb = list(
  "host" = "ds159050.mlab.com:59050",
  "username" = "anthony",
  "password" = "thisisanthony"
))


## Connect to Mongo DB
## - silently return NULL if connection is unsuccessful
connectMongoDB <- function (collectionName, databaseName="greenkeeper") {
  tryCatch(
    mongo(collection = collectionName,
           url = sprintf(
             "mongodb://%s:%s@%s/%s",
             options()$mongodb$username,
             options()$mongodb$password,
             options()$mongodb$host,
             databaseName)),
    error = function(x) return(NULL)
  )
}

# get ID for most recent inventory entry in mangoDB
getCurrentID <- function (db) {
  x <- db$find('{}', fields = '{"_id":0, "id":1}')
  if (ncol(x) == 0) {
    return (0)
  } else {
    return (max(x[['id']], na.rm=TRUE))
  }
}

# save inventory entry to mongoDB
# - either set id (for `consumption`), or
# - increment + 1 relative to current data
saveData <- function (data, db, id=NULL) {

  # Add some additional fields
  data <- as.data.frame(t(data))
  # - add id
  if (is.null(id)) {
    data$id <- getCurrentID(db) + 1  
  } else {
    data$id <- id
  }
  
  # - add timestamp
  data$date <- as.numeric(Sys.time()) - 3600*7
  
  # Insert the data into the mongo collection as a data.frame
  db$insert(data)
}


# load all inventory data from mongoDB
#   - prepare data types
loadInventoryData <- function (db) {
  # Read all the entries
  data <- db$find()
  if (nrow(data)>0) {
    xcols <- intersect(names(data), 
                       c('id','name','brand','type','quantity','cost', 'date'))
    x <- data[,xcols]
    if ("quantity" %in% xcols)  x$quantity <- as.numeric(x$quantity)
    if ("cost" %in% xcols) x$cost <- as.numeric(x$cost)
    if ("id" %in% xcols) x$id <- as.numeric(x$id)
    x
  } else {
    NULL
  }
}

# load all consumption data 
#   - prepare data types
loadConsumptionData <- function (db) {
  data <- db$find()
  if (nrow(data)>0) {
    xcols <- intersect(names(data), c('id', 'amount', 'date'))
    x <- data[,xcols]
    x$amount <- as.numeric(x$amount)
    x$id <- as.numeric(x$id)
    x
  } else {
    NULL
  }
}


# get currently available inventory
getCurrentInventory <- function (inventory, consumption=NULL) {
  if (is.null(consumption)) {
    inventory %>% dplyr::mutate(stash = quantity,
                                stash_value = cost*(stash/quantity)) %>%
                  dplyr::filter(stash > 0)
  } else {
    
    dplyr::left_join(inventory, 
                     consumption %>% dplyr::group_by(id) %>% dplyr::summarize(consumed=sum(amount)),
                     by="id") %>%
      dplyr::mutate(stash = quantity - ifelse(is.na(consumed),0,consumed),
                    stash_value = cost*(stash/quantity)) %>%
      dplyr::filter(stash > 0)
  }
}
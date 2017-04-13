library(ggplot2)
library(plotly)
library(DT)

#' Load exchange rates from a local .csv file
#' 
#' @param dir file location
#' @return A data.frame with: {date, currency, rate}
#' @details If a date is not available, the API will return rates for the most recent non-missing date
#' @export
loadExchangeRates <- function (dir = getwd()) {
  file_name <- file.path(dir, 'exchange_rates.csv')
  x <- read.csv(file_name, stringsAsFactors=FALSE)
  x$date <- as.Date(x$date)
  return(x)
}


#' Update local exchange rates (.csv) with most current data (and fill gaps)
#' 
#' @param dir file location
#' @param start_date First day of data
#' @param end_date Last day of data
#' @return A data.frame with: {date, currency, rate}
#' @export
updateExchangeRates <- function (dir=getwd(), start_date=as.Date('2003-08-15'), end_date=Sys.Date()-1) {
  
  # Read existing exchange rates data
  x <- loadExchangeRates(dir)
  
  # Identify dates that need to be filled in
  dates <- unique(x$date)
  dates <- dates[!(dates %in% seq(start_date, end_date, by='day'))]
  
  # Fill in missing data
  for (i in seq_along(dates)) {
    cat("Updated data for", dates[i], " (", i, "out of", length(dates), ")\n")
    x <- rbind(x, getExchangeRatesAPI(dates[i]))
  }
  
  # Rearrange by date
  x <- x[order(x$date),]
  
  # Overwrite original file
  write.csv(x, 
            file = file.path(dir,"exchange_rates.csv"), 
            row.names = FALSE)
}

#' Get exchange rates for a specific date from fixer.io
#' 
#' @param date Date (format: YYYY-MM-DD)
#' @return A data.frame with: {actual_date, target_date, currency, rate}
#' @details If a date is not available, the API will return rates for the most recent non-missing date
#' @export
getExchangeRatesAPI <- function (date = Sys.Date()-1) {
  
  # Call API
  url <- paste0("http://api.fixer.io/", date, "?base=USD")
  x <- jsonlite::fromJSON(url)
  
  # Organize result
  out <- data.frame(date        = date,
                    currency    = names(x$rates),
                    rate        = as.numeric(sapply(x$rates, function(x) x, USE.NAMES=FALSE)),
                    stringsAsFactors = FALSE)
  
  # Add in USD rate (i.e., the base rate)
  out <- rbind(out[1,], out)
  out$currency[1] <- "USD"
  out$rate[1] <- 1
  
  return(out)
}


#' Compare exchange rates over time for two currencies
#' @param x data.frame from loadExchangeRates()
#' @param currency_to Convert to this currency
#' @param currency_from Convert from this currency
compareCurrency <- function (x, currency_to="USD", currency_from="GBP") {
  x <- subset(x, currency %in% c(currency_to,currency_from))
  x <- tidyr::spread(x, currency, rate) 
  x$rate <- x[[currency_to]] / x[[currency_from]]

  x$label <- sprintf('%s<br>%s = %s',
                     format(x$date,'%b %d, %Y'),
                     paste("1",currency_from),
                     paste(round(x$rate,2),currency_to))
  list(data = x,
       plot = x %>% ggplot(aes(x=date, y=rate)) + 
                    geom_line(aes(label=label)) +
                    labs(x=NULL, title=paste("Convert from", currency_from, "to", currency_to),
                         y = paste("Value of 1", currency_from, "in", currency_to)) +
                    theme_bw() + 
                    theme(plot.title = element_text(hjust = 0.5),
                          plot.subtitle = element_text(hjust = 0.5),
                  aspect.ratio = 3/5, legend.position="none")
       )
}

       
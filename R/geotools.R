require(jsonlite)

#' Use freegeoip.net for converting IP address to geo location
#'
#' @param ip vector strings with IP address
#' @param format 'list' or 'dataframe' default value is list for ip vector with only one value
#'
#' @example
#' new_df <- freegeoip(my_ip, format = "dataframe")
#'
#' @import jsonlite
#'
#' @export
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}

#' Calculates distance between two point using google API
#' You have to create own API key here: https://developers.google.com/maps/documentation/distance-matrix/
#'
#' @param origin start of journer
#' @param target eng of the journey
#' @param mode one of 'driving' (default), walking, bycycling or transit (public transport)
#' @param apikey
#'
#' @return list with distance in kilometers and time in minutes
#'
#' @example
#' calculateDistance("50.1254,14.2222", "50.1000,14.3333", apikey="thisisnotkey")
#'
#' @import jsonlite
#'
#' @export
calculateDistance <- function(origin, target, mode = "driving", apikey) {
  url <- sprintf("https://maps.googleapis.com/maps/api/distancematrix/json?origins=%s&destinations=%s&mode=%s&key=%s", origin, target,mode, apikey)
  json <- jsonlite::fromJSON(url)

  distance <- as.numeric(unlist(json$rows)[2]) / 1000
  time <- as.numeric(unlist(json$rows)[4]) / 60

  return(list(distance = distance, time = time))
}

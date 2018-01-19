require(rjson)

#' Use freegeoip.net for converting IP address to geo location
#'
#' @param ip vector strings with IP address
#' @param format 'list' or 'dataframe' default value is list for ip vector with only one value
#'
#' @example
#' new_df <- freegeoip(my_ip, format = "dataframe")
#'
#' @import rjson
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

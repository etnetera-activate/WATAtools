# library(XML)

#' Customizable XML parser for product feeds.
#' \code{heurekaFeed2df(doc)} create a new datafraame containing the flat structure of the Heureka product feed. It skips problematics tags.
#'
#' This  is a XML parser optimised for Heureka feed. It can replaces \code{XML::xmlTodataframe} in all cases when this function fails for some reason.
#'
#' @param doc XML parsed document or string or file
#' @param xpath the xpath to shopitem in the feed
#' @param isXML indicated if doc is parsed XML.
#' @param usewhich is experimental. I looks like function works faster when set to true
#' @param verbose sets debug messages on
#' @param skipTags is a collection of tags which will not be processed. Typically multiplve values in PARAM and DELIVERY
#'
#' @examples
#'
#' library(XML)
#' doc <- xmlParse("http://my.eshop.cz/heureka.xml")
#' df <- heurekaFeed2df(doc)
#'
heurekaFeed2df <-  function(doc, xpath="//SHOPITEM", isXML = TRUE, usewhich = TRUE, verbose = TRUE, skipTags=c("PARAM","DELIVERY")) {

  if (!isXML)
    doc = xmlParse(doc)

  #### get the records for that form
  nodeset <- getNodeSet(doc, xpath)

  ## get the field names
  var.names <- lapply(nodeset, names)

  ## get the total fields that are in any record
  fields = unique(unlist(var.names))
  fields <- fields[!(fields %in% skipTags)]
  if (verbose) message(fields)

  ## extract the values from all fields
  dl = lapply(fields, function(x) {
    xp <- paste0(xpath, "/", x)
    if (verbose) message(xp)
    xpathSApply(doc, xp , xmlValue)
  })

  ## make logical matrix whether each record had that field
  name.mat = t(sapply(var.names, function(x) fields %in% x))

  #name.mat <- name.mat[,-c("PARAM")]
  df = data.frame(matrix(NA, nrow = nrow(name.mat), ncol = ncol(name.mat)))
  names(df) = fields

  ## fill in that data.frame
  for (icol in 1:ncol(name.mat)) {
    message(sprintf("Row %d",icol))
    rep.rows = name.mat[, icol]
    if (usewhich)
      rep.rows = which(rep.rows)
    df[rep.rows, icol] = dl[[icol]]
  }

  return(df)
}



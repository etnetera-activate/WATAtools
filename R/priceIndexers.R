library(XML)
library(httr)
library(dplyr)
library(tidyr)


#' Customizable XML parser for product feeds.
#'
#' \code{heurekaFeed2df(doc)} create a new datafraame containing the flat structure of the Heureka product feed. It skips problematics tags.
#'
#' This  is a XML parser optimised for Heureka feed. It can replaces \code{XML::xmlTodataframe} in all cases when this function fails for some reason.
#' Function allow you to see debug messages and parametrize it's behavior.
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
#' @import XML
#' @export
heurekaFeed2df <-  function(doc, xpath="//SHOPITEM", isXML = TRUE, usewhich = TRUE, verbose = TRUE, skipTags=c("PARAM","DELIVERY")) {

  if (!isXML){
    if(verbose) message("Parsing XML ...")
    doc = xmlParse(doc)
  }

  #### get the records for that form
  if(verbose) message("Scanning XML ...")
  nodeset <- getNodeSet(doc, xpath)

  ## get the field names
  var.names <- lapply(nodeset, names)

  ## get the total fields that are in any record
  fields = unique(unlist(var.names))
  fields <- fields[!(fields %in% skipTags)]

  if (verbose) {
    message(paste("Found tags: ",    paste(fields,collapse=",")))
    message(paste("Excluded tags: ", paste(skipTags, collapse=",")))
  }

    ## extract the values from all fields
  dl = lapply(fields, function(x) {
    xp <- paste0(xpath, "/", x)
    if (verbose) message(paste("Extracting data:",xp))
    xpathSApply(doc, xp , xmlValue)
  })

  ## make logical matrix whether each record had that field
  name.mat = t(sapply(var.names, function(x) fields %in% x))

  #name.mat <- name.mat[,-c("PARAM")]
  df = data.frame(matrix(NA, nrow = nrow(name.mat), ncol = ncol(name.mat)))
  names(df) = fields

  ## fill in that data.frame
  for (icol in 1:ncol(name.mat)) {
    rep.rows = name.mat[, icol]
    if (usewhich)
      rep.rows = which(rep.rows)
    df[rep.rows, icol] = dl[[icol]]
  }

  return(df)
}


#' Downloads all reviews for eshop from heureka.cz
#'
#' \code{getHeurekaReviews(shopname)} downloads all reviews for specified eshop.
#'
#' @param shopName name of the eshop. Eg. datart-cz, alza-cz, etc. Data are retrieved from the eshop profile page. Eg. https://obchody.heureka.cz/kasa-cz/recenze/
#' @param fromDate (not implemented yet)use this for limit from which date are reviews downloaded. Default is 'download all history'
#' @param verbose indicate if debug messages should be displayed
#'
#' @examples
#' df <- getHeurekaReviews('kasa-cz')
#'
#' @import XML
#' @import httr
#' @import dplyr
#' @import tidyr
#'
#' @export
getHeurekaReviews <- function(shopName, fromDate = as.Date("1970-01-01"), verbose = T){
  #TODO: implement fromDate
  #TODO: clean code

  shopurl <- sprintf("https://obchody.heureka.cz/%s/recenze/",shopName)
  response <- GET(shopurl)

  # zjistime si kolik stranek maji recenze na URL
  doc <- htmlParse(content(response))
  pages <- as.numeric(xpathApply(doc, '//*[@id="text"]/p/a[5]/text()',xmlValue))
  if(verbose)message(paste("pages:",pages))
  out <- data.frame()

  # pro kazdou stranku postune stahneme data
  for(pageNum in 1:pages){
    url<-sprintf("%s?f=%d",shopurl,pageNum, pages)
    if(verbose)message(paste(url,pages,sep=" / "))

    doc <- htmlParse(content(GET(url)))

    #kolik je recenzi na strance. Typicky deset, ale co posledni stranka
    numReviews <- length(xpathSApply(doc,'//*[@class="review"]'))

    #postupne pro recenze sahame na elementy
    outPage <- data.frame()
    for(ri in 1:numReviews){

      #datum recenze
      dateXPath <- sprintf('//*[@class="review"][%d]/div[1]/p[2]/text()',ri)
      date<-xpathSApply(doc,dateXPath,xmlValue)

      #rating
      ratingXPath <- sprintf('//*[@class="review"][%d]/div[2]/h3/big/text()',ri)
      rating<-xpathSApply(doc,ratingXPath,xmlValue)

      #rating nemusi byt vzdy vyplnen, takze pokud neni doplnime nula.
      if(length(rating)!=1){
        rating <- 0;
      } else {
        #jinak prevedem na cislo
        rating <- as.numeric(gsub(pattern = "%",replacement = "",x = rating[[1]], fixed=T))
      }
      outPage <- rbind(outPage,data.frame(date=date[[1]],rating=rating))
    }
    out<-rbind(out, outPage)
  }

  #doplnime metadata
  out$shopname <- shopName
  out$dateDownload <- Sys.Date()


  dt <- out
  #### konverze datumu stringu na datum
  #vycistime si to
  dt$date <- as.character(levels(dt$date)[dt$date])
  dt$date <- tolower(iconv(dt$date, from="utf-8",to="ASCII//TRANSLIT"))

  #nahradime vyrazy 'pred 11 hodinami' a 'vcera' za datumy
  try(dt[grepl(dt$date,pattern = "vcera"),]$date <- format(Sys.Date()-1, "%d. %m %Y"),silent = T) #vcerejsi
  try(dt[grepl(dt$date,pattern = "(hodin|minut)"),]$date <- format(Sys.Date(), "%d. %m %Y"), silent = T) #dnesni


  #rozpadneme na den, mesi a rok
  dt$date <- gsub('pridano: ','',dt$date)
  dt<-separate(dt,col=date,into=c('day','month','year'),sep = " ")

  #udelame substituci za sklonovana jmena pomoci pojmenovaneho vektoru
  czMonths <- c(
    'ledna' = '01',
    'unora' = '02',
    'brezna' = '03',
    'dubna' = '04',
    'kvetna' = '05',
    'cervna' = '06',
    'cervence' = '07',
    'srpna' = '08',
    'zari' = '09',
    'rijna' = '10',
    'listopadu' = '11',
    'prosince' = '12'
  )
  dt[grepl("[a-z]",dt$month),]$month <- czMonths[dt[grepl("[a-z]",dt$month),]$month]

  #a konverze
  dt$date <- as.Date(paste(dt$day, dt$month, dt$year), format="%d. %m %Y")
  if(verbose) message("Done.")

  return(dt)
}

#test
#df <- getHeurekaReviews("gigamat-cz")


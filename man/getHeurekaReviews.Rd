% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/priceIndexers.R
\name{getHeurekaReviews}
\alias{getHeurekaReviews}
\title{Downloads all reviews for eshop from heureka.cz}
\usage{
getHeurekaReviews(shopName, fromDate = as.Date("1970-01-01"), verbose = T)
}
\arguments{
\item{shopName}{name of the eshop. Eg. datart-cz, alza-cz, etc. Data are retrieved from the eshop profile page. Eg. https://obchody.heureka.cz/kasa-cz/recenze/}

\item{fromDate}{(not implemented yet)use this for limit from which date are reviews downloaded. Default is 'download all history'}

\item{verbose}{indicate if debug messages should be displayed}
}
\description{
\code{getHeurekaReviews(shopname)} downloads all reviews for specified eshop.
}
\examples{
df <- getHeurekaReviews('kasa-cz')

}

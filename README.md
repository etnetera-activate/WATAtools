# WATAtools

R Package containing usefull tools for digital analytics in [Etnetera Activate](http://www.activate.cz/).

## Install

Install the [devtools](https://github.com/hadley/devtools) package:

	install.packages("devtools")
	library(devtools)

And then run the `install_github` command:

	devtools::install_github("etnetera-activate/WATAtools")
	library(WATAtools)
	
## Functions in the package

For price indexers
* heurekaFeed2df() ... customizable XML parser for Heureka.cz product feed
* getHeurekaReviews() ... scraping heureka reviews given eshop

For geolocation
* freegeoip() ... translate IP to geolocation using freegeoip.com

## Usage example

TODO




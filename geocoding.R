##
## Title:  Geo code addresses
## Date:   June, 2014
## Author: Omar Trejo Navarro
## Email:  otrenav [at] gmail [dot] com
##
## Ger coordinates of address using Google's
## geo-coding API.
##

## Note: For server reponses see documentation at
## https://developers.google.com/maps/documentation/geocoding/

require(rjson)

getCoordinates <- function(address) {
    url <- paste(
        "http://maps.google.com/maps/api/geocode/json?address=", 
        address, "&sensor=false", sep = "")
    map_data <- fromJSON(paste(readLines(url), collapse = ""))
    if (map_data$status == "OK") {
        coords <- cbind(
            map_data$results[[1]]$geometry$location$lat,
            map_data$results[[1]]$geometry$location$lng)
    } else {
        coords <- NULL
    }
    return(coords)
}

## Call with:

## address <- paste(
##     df$Calle[1], "+", 
##     df$Exterior[1], "+,", 
##     df$Colonia[1], sep = "")
## 
## getCoordinates(address)

##
## EOF
##

##
## DATATON 2014
##
## geocoding.R
##
## Get coordinates of address using
## Google's geo-coding API.
##
## Omar Trejo
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
        coord <- cbind(
            map_data$results[[1]]$geometry$location$lat,
            map_data$results[[1]]$geometry$location$lng)
    } else {
        coord <- c(NA, NA)
    }
    return(coord)
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

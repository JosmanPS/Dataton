##
## DATATON 2014
##
## geocode_databases.R
##
## Geo-code databases available from Zapopan.
##
## Omar Trejo
##

## Note: be careful with addresses that are not
##       well formatted and avoid sending unnecessary
##       requests to the Google servers.

setwd("~/Dropbox/Consultora/Projects/DATATON/")

##
## Eventos.csv
##

eventos <- read.csv("Input/Data/Zapopan/Eventos.csv")
## eventos <- read.csv("Output/Data/Eventos_geo.csv")

eventos <- cbind(eventos, 
                 matrix(data = NA, 
                        nrow = dim(eventos)[1], 
                        ncol = 2))

colnames(eventos)[14:15] <- c("Lat", "Long")

for (i in 1:dim(eventos)[1]) {
    if (any(is.na(eventos$Lat[i]), is.na(eventos$Long[i]))) {
        address <- paste(
            gsub(" ", "+", eventos$Calle[i], fixed = TRUE), "+", 
            gsub(" ", "+", eventos$Exterior[i], fixed = TRUE), ",+", 
            gsub(" ", "+", eventos$Colonia[i], fixed = TRUE), ",+", 
            "Zapopan", sep = "")
        print(i)
        print(address)
        if (address != "+NA,+") {
            eventos[i, c("Lat", "Long")] <- getCoordinates(address)
        }
    }
}

write.csv(eventos, file = "Output/Data/Eventos_geo.csv", row.names = FALSE)

##
## Tianguis.csv
##

## Note: There are some locale problems with this file.
##       To fix them we need to change the locale.
Sys.setlocale('LC_ALL','C')

tianguis <- read.csv("Output/Data/Tianguis_geo.csv")
colonias <- read.csv("Output/Data/Colonias_clean.csv")

tianguis <- cbind(tianguis, 
                 matrix(data = NA, 
                        nrow = dim(tianguis)[1], 
                        ncol = 3))

colnames(tianguis)[5:7] <- c("ColNombre", "Lat", "Long")

for (i in 1:dim(tianguis)[1]) {
    
    ## Find out the names of the colonias
    
    j <- 1
    while (tianguis$Colonia[i] != colonias$IdColonia[j]) {
        j <- j + 1
    }
    if (tianguis$Colonia[i] == colonias$IdColonia[j]) {
        tianguis$ColNombre[i] <- as.character(colonias$NomColonia[j])
    } else {
        tianguis$ColNombre[i] <- NA
    }
}

for (i in 1:dim(tianguis)[1]) {
    
    ## Find out the coordinates
    ## at street-colonia level
    
    if (any(is.na(tianguis$Lat[i]), is.na(tianguis$Long[i]))) {
        if (!is.na(tianguis$ColNombre[i])) {
            address <- paste(
                gsub(" ", "+", tianguis$Calle[i], fixed = TRUE), ",+", 
                gsub(" ", "+", tianguis$ColNombre[i], fixed = TRUE), ",+", 
                "Zapopan", sep = "")
        } else {
            address <- paste(
                gsub(" ", "+", tianguis$Calle[i], fixed = TRUE), "+",
                "Zapopan", sep = "")
        }
    }
    print(i)
    print(address)
    tianguis[i, c("Lat", "Long")] <- getCoordinates(address)
}

for (i in 1:dim(tianguis)[1]) {
    
    ## Find out the coordinates
    ## at colonia level only (less precise)
    ## if previous method did not work.
    
    if (any(is.na(tianguis$Lat[i]), is.na(tianguis$Long[i]))) {
        if (!is.na(tianguis$ColNombre[i])) {
            address <- paste(
                gsub(" ", "+", tianguis$ColNombre[i], fixed = TRUE), ",+", 
                "Zapopan", sep = "")
        } else {
            address <- NA
        }
    }
    print(i)
    print(address)
    if (!is.na(address)) {
        tianguis[i, c("Lat", "Long")] <- getCoordinates(address)
    } else {
        tianguis[i, c("Lat", "Long")] <- c(NA, NA)
    }   
}

write.csv(tianguis, file = "Output/Data/Tianguis_geo_clean.csv", row.names = FALSE)

##
## Colonias.csv
##

colonias <- read.csv("Output/Data/Colonias_clean.csv")

colonias <- cbind(colonias, 
                 matrix(data = NA, 
                        nrow = dim(colonias)[1], 
                        ncol = 4))

colnames(colonias)[46:49] <- c("LatxNombre", "LongxNombre", "LatxCP", "LongxCP")

for (i in 1:dim(colonias)[1]) {
    
    ## Find out coordinates using the colonia name
    
    if (any(is.na(colonias$LatxNombre[i]), is.na(colonias$LongxNombre[i]))) {
        address <- paste(
            gsub(" ", "+", colonias$NomColonia[i], fixed = TRUE), "+",
            "Zapopan", sep = "")
        print(i)
        print(address)
        colonias[i, c("LatxNombre", "LongxNombre")] <- getCoordinates(address)
    }
}

for (i in 1:dim(colonias)[1]) {
    
    ## Find out coordinates using the colonia name
    
    if (any(is.na(colonias$LatxCP[i]), is.na(colonias$LongxCP[i]))) {
        if (any(is.na(colonias$CP[i]), colonias$CP[i] == 0)) {
            address <- NA
        } else {
            address <- paste(
                gsub(" ", "+", colonias$CP[i], fixed = TRUE), "+",
                "Zapopan", sep = "")
        }
        print(i)
        print(address)
        if (!is.na(address)) {
            colonias[i, c("LatxCP", "LongxCP")] <- getC)oordinates(address)
        }
    }
}

write.csv(colonias, file = "Output/Data/Colonias_clean_geo.csv", row.names = FALSE)

##
## EOF
##

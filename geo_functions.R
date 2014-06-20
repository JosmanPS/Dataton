#
# Geographical representations
# Dataton 2014
#
# Luis Roman Garcia 
#

library(rgdal)
library(gtools)
library(maptools)
library(ggplot2)
library(ggmap)
library(maps)
library(foreign)
library(plyr)
library(geosphere)
library(gooJSON)
library(foreign)
library(RCurl)
library(RJSONIO)
library(shapefiles)

# Functions 

# Tesselate: makes a uniform grid over the given map.
# Returns: map, coordinates of the cuts, area of blocks

tesselate <- function(grids, map, alpha = .3) {
    
    results <- list()
    intercepts <- ceiling(sqrt(grids + 1))
    
    top_left     <- c(-103.48068, 20.78433)
    bottom_left  <- c(-103.48068, 20.589055)
    top_right    <- c(-103.322068, 20.78433)
    bottom_right <- c(-103.322068, 20.589055)
    
    
    h_lines <- data.frame(x = rep(top_left[1], intercepts),
                          y = seq(top_left[2], bottom_left[2],length = intercepts),
                          xend = rep(top_right[1], intercepts),
                          yend = seq(top_right[2], bottom_right[2],length = intercepts))
    
    v_lines <- data.frame(x = seq(top_left[1], top_right[1],length = intercepts), 
                          y = rep(top_left[2], intercepts),
                          xend = seq(bottom_left[1],bottom_right[1],length = intercepts),
                          yend = rep(bottom_right[2],intercepts))
    
    map <- map + geom_segment(data = h_lines, aes(x = x,
                                                  y = y,
                                                  xend = xend,
                                                  yend = yend), 
                              color = "red", alpha = alpha) 
    
    map <- map + geom_segment(data = v_lines, aes(x = x,
                                                  y = y,
                                                  xend = xend,
                                                  yend = yend), 
                              color = "red", alpha = alpha)
    map <- map +  geom_hline(yintercept = h_lines$y, col = "red", alpha = alpha)
    map <- map +  geom_vline(xintercept = v_lines$x, col = "red", alpha = alpha)
    
    x <- seq(top_left[1], top_right[1],length = intercepts) 
    y <- seq(top_right[2], bottom_right[2],length = intercepts)
    k = 1
    xcoord <- c()
    ycoord <- c()
    for (i in 1:(length(y)-1)){
      for(j in 1:(length(x)-1)){
        xcoord[k] <- (x[j] + x[j + 1])/2
        ycoord[k] <- (y[i] + y[i + 1])/2
        k = k + 1
      }
    }
    points <- data.frame(x = xcoord, y = ycoord)
    
    results[[1]] <- map
    # ordinates
    results[[2]] <- seq(top_left[2], bottom_left[2],length = intercepts)
    # abscises
    results[[3]] <- seq(bottom_left[1],bottom_right[1],length = intercepts)
    # area of block
    results[[4]] <- (abs((bottom_right[1]-top_left[1]))*abs((top_left[2]-bottom_right[2])))/grids
    # centers of blocks
    results[[5]] <- points
    
    return(results)
}

# blocks: gets the upper left and bottom right coordinates of the blocks in a tesselations

blocks <- function(ordinates, abscises){
    block <- list()
    k <- 1
    for (i in 1:(length(ordinates)-1)){
        for ( j in 1:(length(abscises)-1)){
            entry <- list()
            entry[[1]] <- c(abscises[j], ordinates[i])  # upper left
            entry[[2]] <- c(abscises[j + 1], ordinates[i + 1])  # bottom right
            block[[k]] <- entry
            k = k + 1
        }
    }
    block
}

# in.block: counts the population inside a block
in.block <- function(block, pop){
    pop.block <- list()
    for ( i in 1:length(block)){
        list   <- block[[i]] 
        pop.xreduce  <- c()
        pop.xyreduce <- c()
        # greater than upper left x coordinate
        pop.xreduce  <- pop[pop[, 1] > as.numeric(list[[1]][1]), ]
        # smaller than lower right x coordinate
        pop.xreduce  <- pop.xreduce[pop.xreduce[, 1] < as.numeric(list[[2]][1]), ]
        # greater than lower right y coordinate
        pop.xyreduce <- pop.xreduce[pop.xreduce[, 2] > as.numeric(list[[2]][2]), ]
        # smaller than upper left y coordinate
        pop.xyreduce <- pop.xyreduce[pop.xyreduce[, 2] < as.numeric(list[[1]][2]), ]
        data.block <- list()
        data.block[[1]] <- list  # upper left and lower right coordinates of each block
        pop.xyreduce$celda <- rep(i,nrow(pop.xyreduce))
        data.block[[2]] <- pop.xyreduce  # data of locations per block
        data.block[[3]] <- sum(pop.xyreduce[, 3])  # total population per block
        pop.block[[i]]  <- data.block  
    }
    pop.block
}

# in.block.fac: number of facilities per cell
in.block.fac <- function(block, pop){
    pop.block <- list()
    for ( i in 1:length(block)){
        list   <- block[[i]] 
        pop.xreduce  <- c()
        pop.xyreduce <- c()
        # greater than upper left x coordinate
        pop.xreduce  <- pop[pop[, 1] > as.numeric(list[[1]][1]), ]
        # smaller than lower right x coordinate
        pop.xreduce  <- pop.xreduce[pop.xreduce[, 1] < as.numeric(list[[2]][1]), ]
        # greater than lower right y coordinate
        pop.xyreduce <- pop.xreduce[pop.xreduce[, 2] > as.numeric(list[[2]][2]), ]
        # smaller than upper left y coordinate
        pop.xyreduce <- pop.xyreduce[pop.xyreduce[, 2] < as.numeric(list[[1]][2]), ]
        data.block <- list()
        data.block[[1]] <- list  # upper left and lower right coordinates of each block
        pop.xyreduce$celda <- rep(i,nrow(pop.xyreduce))
        data.block[[2]] <- pop.xyreduce  # data of locations per block
        data.block[[3]] <- nrow(pop.xyreduce)  # total population per block
        pop.block[[i]]  <- data.block  
    }
    pop.block
}

# function get coordinates
getCoordinates <- function(address) {
    result <- list()
    url <- paste("http://maps.googleapis.com/maps/api/geocode/json?address=",
                 address,"&sensor=false",sep="")
    map_data <- fromJSON(paste(readLines(url),collapse=""))
    if(length(map_data$results)>=1){
        result[[1]] <- map_data$results[[1]]$formatted_address[1]
        result[[2]] <- map_data$results[[1]]$geometry$bounds$northeast$lng[1]
        result[[3]] <- map_data$results[[1]]$geometry$bounds$northeast$lat[1]
        Sys.sleep(.002)
    }else{
        result = NA
    }
    result
}



url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
    if(verbose) cat(address,"/n")
    u <- url(address)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status=="OK") {
        lat <- x$results[[1]]$geometry$location$lat
        lng <- x$results[[1]]$geometry$location$lng
        location_type <- x$results[[1]]$geometry$location_type
        formatted_address <- x$results[[1]]$formatted_address
        return(c(lat, lng, location_type, formatted_address))
    } else {
        return(c(NA,NA,NA, NA))
    }
}

# create geom segment
GeomSegment2 <- proto(ggplot2:::GeomSegment, {
    objname <- "geom_segment2"
    draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
        if (is.linear(coordinates)) {
            return(with(coord_transform(coordinates, data, scales),
                        segmentsGrob(x, y, xend, yend, default.units="native",
                                     gp = gpar(col=alpha(colour, alpha), lwd=size * .pt,
                                               lty=linetype, lineend = "round"),
                                     arrow = arrow)
            ))
        }
    }})

geom_segment2 <- function(mapping = NULL, data = NULL, stat =
                              "identity", position = "identity", arrow = NULL, ...) {
    GeomSegment2$new(mapping = mapping, data = data, stat = stat,
                     position = position, arrow = arrow, ...)
}

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
# plot zapopan
zap.map      <- get_map(location = "Zapopan", zoom = 12, maptype = "toner",
                        source = "stamen")
zap.map.plot <- ggmap(zap.map)
contorno <- read.table("D:/Usuarios/luis.roman/Dropbox/Sibila/Projects/Dataton/Input/Data/Zapopan/contorno.txt",
                       header = F, sep = ",")
zap.full <- zap.map.plot + geom_polygon(data = contorno, aes(x = V1, y = V2), col = "darkred", size = 1)
zap.full
# Read in files
# shapefile streets
data.shape.street<-read.shp("./input/Data/INEGI/Vialidades/jal_eje_vial.shp")
coords.street <- ldply(data.shape.street[[1]],
                       function(t){t <- t[8]$points})
# plot streets
zap.map.plot + geom_point(data = coords.street, aes(x = X, y = Y), 
                          col = "red4", size = .4) 

# shapefile and data services
data.services <- read.dbf("D:/Usuarios/luis.roman/Dropbox/Sibila/Projects/Dataton/Input/Data/INEGI/Servicios/jal_servicios_p.dbf")
data.shape.services <-read.shp("D:/Usuarios/luis.roman/Dropbox/Sibila/Projects/Dataton/Input/Data/INEGI/Servicios/jal_servicios_p.shp")
coords.services <- data.frame(x = data.shape.services[[1]][, 2],
                              y = data.shape.services[[1]][, 3],
                              class = data.services[[1]][ ,4])
# plot services
zap.map.plot + geom_point(data = coords.services, aes(x = x,
                                                      y = y,
                                                      col = class))

# data set crimen
crimen <- read.csv("D:/Usuarios/luis.roman/Dropbox/Sibila/Projects/Dataton/Input/Data/Zapopan/eventos_tryout.csv")
crimen <- crimen[, c(7, 8, 12, 1, 3)]

# plot crime
zap.tes.plot + geom_point(data = crimen, aes(x = X, y = Y,
                                             col = Descripcion))

# data set parques
parques <- read.csv("D:/Usuarios/luis.roman/Dropbox/Sibila/Projects/Dataton/Input/Data/Zapopan/Parques.csv")
# plot parques
zap.tes.plot + geom_point(data = parques, aes(x = Longitud, y = Latitud))

# data set llamadas
llamadas <- read.csv("D:/Usuarios/luis.roman/Dropbox/Sibila/Projects/Dataton/Input/Data/Movistar/Movistar_Zapopan.csv")
# plot llamadas
zap.tes.plot + geom_point(data = llamadas, aes(x = Longitud, y = Latitud))













# Dataton 
# Luis Manuel Roman Garcia
# Data integration

# segmentacion zapopan en k celdas.
k <- 900
zap.tes <- tesselate(k, zap.map.plot, alpha = .1)
zap.block <- blocks(zap.tes[[2]], zap.tes[[3]])

# conteo servicios y tipo de servicios por celda.
serv.per.cell <- in.block.fac(zap.block, coords.services)
data.serv.cell <- data.frame(matrix(NA, nrow = length(serv.per.cell),
                                    ncol = 13))
for(i in 1:length(serv.per.cell)){
  data.serv.cell[i,1]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "CENTRO DE ASISTENCIA MÉDICA"))
  data.serv.cell[i,2]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "CEMENTERIO"))
  data.serv.cell[i,3]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "ESCUELA"))
  data.serv.cell[i,4]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "MERCADO"))
  data.serv.cell[i,5]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "TANQUE DE AGUA"))
  data.serv.cell[i,6]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "INSTALACIÓN DEPORTIVA O RECREATIVA"))
  data.serv.cell[i,7]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "INSTALACIÓN DIVERSA"))
  data.serv.cell[i,8]  <- nrow(subset(serv.per.cell[[i]][[2]],class == "POZO"))
  data.serv.cell[i,9] <- nrow(subset(serv.per.cell[[i]][[2]],class == "RASGO ARQUEOLÓGICO"))
  data.serv.cell[i,10] <- nrow(subset(serv.per.cell[[i]][[2]],class == "ESTRUCTURA ELVEADA"))
  data.serv.cell[i,11] <- nrow(subset(serv.per.cell[[i]][[2]],class == "PALACIO DE GOBIERNO"))
  data.serv.cell[i,12] <- nrow(subset(serv.per.cell[[i]][[2]],class == "PLAZA"))
  data.serv.cell[i,13] <- nrow(subset(serv.per.cell[[i]][[2]],class == "TEMPLO"))
}
colnames(data.serv.cell) <- unique(data.services[[1]][4])[,1]
data.serv.cell$lat  <- zap.tes[[5]]$y
data.serv.cell$long <- zap.tes[[5]]$x
head(data.serv.cell)

# conteo crimen y tipo de crimen por día
crime.per.cell <- in.block.fac(zap.block, crimen)
data.crime.cell <- data.frame(matrix(NA, nrow = length(serv.per.cell),
                                    ncol = 13))
for(i in 1:length(crime.per.cell)){
  data.crime.cell[i,1]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ASALTO PERSONA"))
  data.crime.cell[i,2]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ASALTO A NEGOCIO"))
  data.crime.cell[i,3]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "VEH. ROBADO"))
  data.crime.cell[i,4]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ROBO AUTOPARTES"))
  data.crime.cell[i,5]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ROBO A PERSONA"))
  data.crime.cell[i,6]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ROBO A CASA HAB."))
  data.crime.cell[i,7]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ASALTO VEHICULO"))
  data.crime.cell[i,8]  <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ROBO A NEGOCIO"))
  data.crime.cell[i,9] <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ASALTO A BANCO"))
  data.crime.cell[i,10] <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ASALTO CASA HAB."))
  data.crime.cell[i,11] <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "VIOLACION"))
  data.crime.cell[i,12] <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "ROBO A BANCO"))
  data.crime.cell[i,13] <- nrow(subset(crime.per.cell[[i]][[2]],Descripcion == "OCCISO"))
}
colnames(data.crime.cell) <- unique(crimen$Descripcion)
head(data.crime.cell)

# servicios y crimen juntos
data.full <- cbind(data.serv.cell, data.crime.cell)

# gráfica zapopan segmentado
tot.crime.cell <- laply(crime.per.cell, function(t){t <- t[[3]]})
zap.tes.plot <- zap.tes[[1]] + geom_point(data = subset(zap.tes[[5]], tot.crime.cell != 0), 
                                          aes(x = x, y = y), 
                                          col = "darkblue", size = 7, shape = 15, 
                                          alpha = .3)
zap.tes.plot

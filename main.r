## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
##  1. Daniel Felipe Villa Rengifo
##  2. Lenguaje: R
##  3. Tema: Climadiagrama
##  4. Fuentes:
##     http://www.weatherbase.com/weather/weather.php3?s=1108&units=#
##     https://www.benjaminbell.co.uk/2018/04/walter-and-lieth-climate-diagrams-in-r.html"


## ------------------------------------------------------------------------------------
# leemos la base de datos:
library(tidyverse)

weather <- read_csv(file = "weatherdata-64-750.csv", col_types = cols())

#1. Pasaremos los datos a los que son de character a date:
library(lubridate)

weather$Date <- mdy(weather$Date)

### Ahora creamos una funcion que nos filtre por año que queremos hacer el climadiagrama:#######
################################################

filtro <- function(x, d){
    "filtra el data.frame según un año, para despues expulsar otro data.frame de promedios mensuales"
    # escogeos los años de la columna 1 de "x" en un vector
    y <- year(x[[1]])
    # Filtro los datos según un año especifico
    db <- filter(x, y == d)
    
    # escogo los años del data.frame creado de la columna 1
    m <- month(db[[1]])
    
    # Creo datos mensuales promedios
    #################
    
    #enero
    f <- filter(db, m == 1)
    ene <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #Febrero
    f <- filter(db, m == 2)
    feb <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #marzo
    f <- filter(db, m == 3)
    marzo <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #abril
    f <- filter(db, m == 4)
    abril <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #mayo
    f <- filter(db, m == 5)
    mayo <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #junio
    f <- filter(db, m == 6)
    junio <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #julio
    f <- filter(db, m == 7)
    julio <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #agos
    f <- filter(db, m == 8)
    agos <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))

    #septiembre
    f <- filter(db, m == 9)
    sep <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #octubre
    f <- filter(db, m == 10)
    oct <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))

    #noviembre
    f <- filter(db, m == 11)
    nov <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #diciembre
    f <- filter(db, m == 12)
    dic <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    # Creamos el data.frame
    climadata <- data.frame("X_1" = c("prec", "tmax", "tmin", "abstmp") ,
                            "Ene" = ene, "Feb" = feb, "Mar" = marzo,
                            "Abr" = abril, "May" = mayo, "Jun" = junio,
                            "Jul" = julio, "Ago" = agos, "Sep" = sep,
                            "Oct" = oct, "Nov" = nov, "Dic" = dic)
    return(climadata)
}


# Creado el data.frame con los datos de 1980
climadata1980 <- filtro(weather, 1980)

# observe el archivo en Climadata1980.csv
write.csv(climadata1980, file = "Climadata1980.csv", row.names = F)

# Para el Climadiagrama necesitamos una matriz de cilamadata1980:

#pasar a formato de matriz
matrizdata1980 <- as.matrix(climadata1980[,-1])
colnames(matrizdata1980) <- NULL
print(matrizdata1980)

# Vamos a hacer una comparación, es decir dos climadiagramás para el año 2001

# Creado el data.frame con los datos de 2001
climadata2001 <- filtro(weather, 2001)

# observe el archivo en Climadata2001.csv
write.csv(climadata2001, file = "Climadata2001.csv", row.names = F)

#pasar a formato de matriz
matrizdata2001 <- as.matrix(climadata2001[,-1])
colnames(matrizdata2001) <- NULL
print(matrizdata2001)


## ------------------------------------------------------------------------------------
# Crear los climadiagramas de ambos lugares:

"Importamos la libreria necesaria, instalamos localmente el paquete, ya que para esta version de R no lo recibe, pero localmente si"
install.packages("iki.dataclim_1.0.tar.gz", repos = NULL, type = "source")

library(iki.dataclim)

# Guardamos la grafica de 1980 y 2001
png(filename = "ClimaDiagrama1980.png")

plotWalterLieth(matrizdata1980, station = "Bello,\nAntioquia\n[Lat: 6.40, Long: -75]", alt = weather$Elevation[1], per = "1980")

dev.off()

## 2001
png(filename = "ClimaDiagrama2001.png")

plotWalterLieth(matrizdata2001, station = "Bello,\nAntioquia\n[Lat: 6.40, Long: -75]", alt = weather$Elevation[1], per = "2001")

dev.off()


## ------------------------------------------------------------------------------------
# Ahora haremos un climadiagrama de 1979 hasta el 2014:

# Creamos algo así como una replica de filtro solo que modificamos algunas cosas:

filtro <- function(x){
    "Crea algo como un summary de promedio de algunas variables de la base de datos"
    # EL data.frame "x" sera ahora "db"
    db <- x
    
    # escogo los años del data.frame creado de la columna 1
    m <- month(db[[1]])
    
    # Creo datos mensuales promedios
    #################
    
    #enero
    f <- filter(db, m == 1)
    ene <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #Febrero
    f <- filter(db, m == 2)
    feb <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #marzo
    f <- filter(db, m == 3)
    marzo <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #abril
    f <- filter(db, m == 4)
    abril <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #mayo
    f <- filter(db, m == 5)
    mayo <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #junio
    f <- filter(db, m == 6)
    junio <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #julio
    f <- filter(db, m == 7)
    julio <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #agos
    f <- filter(db, m == 8)
    agos <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))

    #septiembre
    f <- filter(db, m == 9)
    sep <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #octubre
    f <- filter(db, m == 10)
    oct <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))

    #noviembre
    f <- filter(db, m == 11)
    nov <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    #diciembre
    f <- filter(db, m == 12)
    dic <- c(mean(f[[7]]), mean(f[[5]]), mean(f[[6]]), mean(f[[6]]))
    
    # Creamos el data.frame
    climadata <- data.frame("X_1" = c("prec", "tmax", "tmin", "abstmp") ,
                            "Ene" = ene, "Feb" = feb, "Mar" = marzo,
                            "Abr" = abril, "May" = mayo, "Jun" = junio,
                            "Jul" = julio, "Ago" = agos, "Sep" = sep,
                            "Oct" = oct, "Nov" = nov, "Dic" = dic)
    return(climadata)
}

prom <- filtro(weather)

#pasar a formato de matriz
matrizdata <- as.matrix(prom[,-1])
colnames(matrizdata) <- NULL
print(matrizdata)

png(filename = "ClimaDiagrama1979_2014.png")

plotWalterLieth(matrizdata, station = "Bello,\nAntioquia\n[Lat: 6.40, Long: -75]", alt = weather$Elevation[1], per = "1979-2014")

dev.off()

"Ahora podemos ver que a comparaicón de los datos de 1980, la temperatura esta por debajo de la media maxima, en cambio con el del 2001 esta sobre la media maxima"

"Notamos que la media de precipitación es un poco más de lo normal, esto se asemaja más a los niveles de precipitación del choco, lo mismo que la temperreatura, ya que en el municipio de bello, el promedio ronda los 23 C°"

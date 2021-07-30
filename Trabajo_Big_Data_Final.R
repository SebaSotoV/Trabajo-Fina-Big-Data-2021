################################################################################################################
#############################################################################
###########DATOS DE PERFUMES DE HOMBRE#######################################
#############################################################################
install.packages("rvest")
install.packages("gdata")
library("rvest")
library("gdata")

#descargando la pagina
saraim <- read_html("https://sairam.cl/perfumes-de-hombre/position/asc#body")
perfumechile <- read_html("https://www.chileperfume.cl/categoria-producto/perfumes/perfume-hombre/?orderby=popularity")
eliteperfume <- read_html("https://www.eliteperfumes.cl/collections/para-el")

#listado de Productos 
listadoprodcutos <- html_nodes(saraim,css = ".caption")
listadoprodcutos1 <- html_nodes(perfumechile,css = ".shop-container")
listadoprodcutos2 <- html_nodes(eliteperfume,css = ".productitem--info")

#### Descripcion de los prodcutos
titulo <- html_nodes(listadoprodcutos, "h3")
titulos <- html_text(titulo)
titulos <- as.data.frame(titulos)

titulos1 <- html_nodes(listadoprodcutos1, "a")
textTitulos1 <- html_text(titulos1)
textTitulos1 <- gsub("[\n\t\t\t\t\t]","",textTitulos1)
textTitulos1 <- gsub("[+]","",textTitulos1)
textTitulos1 <- as.data.frame(textTitulos1)
textTitulos1 <- textTitulos1[c(3,6,9,12,15,18,21,24,27,30,33,36),]

titulos2 <- html_nodes(listadoprodcutos2,"h2")
textTitulos2 <- html_text(titulos2)
textTitulos2 <- gsub("[\n]","",textTitulos2)
textTitulos2 <- gsub("                  ","",textTitulos2)
textTitulos2 <- gsub("              ","",textTitulos2)
textTitulos2 <- as.data.frame(textTitulos2)


#precios antes
precioantes <- html_nodes(listadoprodcutos,css = ".product-block-discount")
textoPrecioAntes <- html_text(precioantes)
textoPrecioAntes <- gsub("[.]","",textoPrecioAntes)
textoPrecioAntes <- gsub("[$]","",textoPrecioAntes)
textoPrecioAntes <- as.numeric(textoPrecioAntes)

precioantes1 <- html_nodes(listadoprodcutos1,"bdi")
textoPrecioAntes1 <- html_text(precioantes1)
textoPrecioAntes1 <- gsub("[.]","",textoPrecioAntes1)
textoPrecioAntes1 <- gsub("[$]","",textoPrecioAntes1)
textoPrecioAntes1 <- as.numeric(textoPrecioAntes1[c(1,3,5,7,9,11,13,15,17,19,21,23)])


precioantes2 <- html_nodes(listadoprodcutos2,css= ".money")
textoPrecioAntes2 <- html_text(precioantes2)
textoPrecioAntes2 <- gsub("[.]","",textoPrecioAntes2)
textoPrecioAntes2 <- gsub("[$]","",textoPrecioAntes2)
textoPrecioAntes2 <- gsub("[\n          ]","",textoPrecioAntes2)
textoPrecioAntes2 <- as.numeric(textoPrecioAntes2[c(1,9,17,25,33,41,49,57,65,73,81,89,97,105,113,121,129,137,145,153,161,169,177,185)])


#precio despues
preciodespues <- html_nodes(listadoprodcutos,css = ".product-block-list")
textoPrecioDespues <- html_text(preciodespues)
textoPrecioDespues <- gsub("[.]","",textoPrecioDespues)
textoPrecioDespues <- gsub("[$]","",textoPrecioDespues)
textoPrecioDespues <- as.numeric(textoPrecioDespues)

preciodespues1 <- html_nodes(listadoprodcutos1,"bdi")
textoPrecioDespues1 <- html_text(preciodespues1)
textoPrecioDespues1 <- gsub("[.]","",textoPrecioDespues1)
textoPrecioDespues1 <- gsub("[$]","",textoPrecioDespues1)
textoPrecioDespues1 <- as.numeric(textoPrecioDespues1[c(2,4,6,8,10,12,14,16,18,20,22,24)])

preciodespues2 <- html_nodes(listadoprodcutos2,css= ".money")
textoPrecioDespues2 <- html_text(preciodespues2)
textoPrecioDespues2 <- gsub("[.]","",textoPrecioDespues2)
textoPrecioDespues2 <- gsub("[$]","",textoPrecioDespues2)
textoPrecioDespues2 <- gsub("[\n          ]","",textoPrecioDespues2)
textoPrecioDespues2 <- as.numeric(textoPrecioDespues2[c(5,13,21,29,37,45,53,61,69,77,85,93,101,109,117,125,133,141,149,157,165,173,181,189)])


#(almacenando Informacion sairam.cl) creacion del dataframe 
sairam.cl<- data.frame(titulos,textoPrecioAntes,textoPrecioDespues)
colnames(sairam.cl) <-c("Nombre","Precio Antes", "Precio Ahora")
sairam.cl$Variacion <-(sairam.cl$`Precio Ahora`-sairam.cl$`Precio Antes`)/sairam.cl$`Precio Antes`*100
sairam.cl

#(almacenando Informacion chileperfume.cl) creacion del dataframe 
chileperfume.cl<- data.frame(textTitulos1,textoPrecioAntes1,textoPrecioDespues1)
colnames(chileperfume.cl) <-c("Nombre","Precio Antes", "Precio Ahora")
chileperfume.cl$Variacion <- (chileperfume.cl$`Precio Ahora`- chileperfume.cl$`Precio Antes`)/chileperfume.cl$`Precio Antes`*100
chileperfume.cl

#(almacenando Informacion sairam.cl) creacion del dataframe 
eliteperfume.cl<- data.frame(textTitulos2,textoPrecioAntes2,textoPrecioDespues2)
colnames(eliteperfume.cl) <-c("Nombre","Precio Antes", "Precio Ahora")
eliteperfume.cl$Variacion <- (eliteperfume.cl$`Precio Ahora`- eliteperfume.cl$`Precio Antes`)/eliteperfume.cl$`Precio Antes`*100
eliteperfume.cl

#############################################################################
###########DATOS DE PERFUMES DE MUJER########################################
#############################################################################

#descargando la pagina
sairam.m <- read_html("https://sairam.cl/perfumes-de-mujer")
chileperfume.m <- read_html("https://www.chileperfume.cl/categoria-producto/perfumes/perfume-mujer/?orderby=popularity")
eliteperfume.m <- read_html("https://www.eliteperfumes.cl/collections/para-ella")

#listado de Productos 
listadoprodcutos5 <- html_nodes(sairam.m,css = ".caption")
listadoprodcutos6 <- html_nodes(chileperfume.m,css = ".shop-container")
listadoprodcutos7 <- html_nodes(eliteperfume.m,css = ".productitem--info")


#### titulos de los prodcutos
titulos5 <- html_nodes(listadoprodcutos5, "h3")
textTitulos5 <- html_text(titulos5)
textTitulos5 <- as.data.frame(textTitulos5)

titulos11 <- html_nodes(listadoprodcutos6, "a")
textTitulos11 <- html_text(titulos11)
textTitulos11 <- gsub("[\n\t\t\t\t\t]","",textTitulos11)
textTitulos11 <- gsub("[+]","",textTitulos11)
textTitulos11 <- as.data.frame(textTitulos11)
textTitulos11 <- textTitulos11[c(3,6,9,12,15,18,21,24,27,30,33,36),]

titulos22 <- html_nodes(listadoprodcutos7,"h2")
textTitulos22 <- html_text(titulos22)
textTitulos22 <- gsub("[\n]","",textTitulos22)
textTitulos22 <- gsub("                  ","",textTitulos22)
textTitulos22 <- gsub("              ","",textTitulos22)
textTitulos22 <- as.data.frame(textTitulos22)


#precios antes
precioantes5 <- html_nodes(listadoprodcutos5,css = ".product-block-discount")
textoPrecioAntes5 <- html_text(precioantes5)
textoPrecioAntes5 <- gsub("[.]","",textoPrecioAntes5)
textoPrecioAntes5 <- gsub("[$]","",textoPrecioAntes5)
textoPrecioAntes5 <- as.numeric(textoPrecioAntes5)

precioantes11 <- html_nodes(listadoprodcutos6,"bdi")
textoPrecioAntes11 <- html_text(precioantes11)
textoPrecioAntes11 <- gsub("[.]","",textoPrecioAntes11)
textoPrecioAntes11 <- gsub("[$]","",textoPrecioAntes11)
textoPrecioAntes11 <- as.numeric(textoPrecioAntes11[c(1,3,5,7,9,11,13,15,17,19,21,23)])


precioantes22 <- html_nodes(listadoprodcutos7,css= ".money")
textoPrecioAntes22 <- html_text(precioantes22)
textoPrecioAntes22 <- gsub("[.]","",textoPrecioAntes22)
textoPrecioAntes22 <- gsub("[$]","",textoPrecioAntes22)
textoPrecioAntes22 <- gsub("[\n          ]","",textoPrecioAntes22)
textoPrecioAntes22 <- as.numeric(textoPrecioAntes22[c(1,9,17,25,33,41,49,57,65,73,81,89,97,105,113,121,129,137,145,153,161,169,177,185)])


#precio despues
preciodespues5 <- html_nodes(listadoprodcutos5,css = ".product-block-list")
textoPrecioDespues5 <- html_text(preciodespues5)
textoPrecioDespues5 <- gsub("[.]","",textoPrecioDespues5)
textoPrecioDespues5 <- gsub("[$]","",textoPrecioDespues5)
textoPrecioDespues5 <- as.numeric(textoPrecioDespues5)

preciodespues11 <- html_nodes(listadoprodcutos6,"bdi")
textoPrecioDespues11 <- html_text(preciodespues11)
textoPrecioDespues11 <- gsub("[.]","",textoPrecioDespues11)
textoPrecioDespues11 <- gsub("[$]","",textoPrecioDespues11)
textoPrecioDespues11 <- as.numeric(textoPrecioDespues11[c(2,4,6,8,10,12,14,16,18,20,22,24)])

preciodespues22 <- html_nodes(listadoprodcutos7,css= ".money")
textoPrecioDespues22 <- html_text(preciodespues22)
textoPrecioDespues22 <- gsub("[.]","",textoPrecioDespues22)
textoPrecioDespues22 <- gsub("[$]","",textoPrecioDespues22)
textoPrecioDespues22 <- gsub("[\n          ]","",textoPrecioDespues22)
textoPrecioDespues22 <- as.numeric(textoPrecioDespues22[c(5,13,21,29,37,45,53,61,69,77,85,93,101,109,117,125,133,141,149,157,165,173,181,189)])

#(almacenando Informacion sairam.cl) creacion del dataframe 
sairam.cl.m<- data.frame(textTitulos5,textoPrecioAntes5,textoPrecioDespues5)
colnames(sairam.cl.m) <-c("Nombre","Precio Antes", "Precio Ahora")
sairam.cl.m$Variacion <-(sairam.cl.m$`Precio Ahora`-sairam.cl.m$`Precio Antes`)/sairam.cl.m$`Precio Antes`*100
sairam.cl.m

#(almacenando Informacion chileperfume.cl) creacion del dataframe 
chileperfume.cl.m<- data.frame(textTitulos11,textoPrecioAntes11,textoPrecioDespues11)
colnames(chileperfume.cl.m) <-c("Nombre","Precio Antes", "Precio Ahora")
chileperfume.cl.m$Variacion <- (chileperfume.cl.m$`Precio Ahora`- chileperfume.cl.m$`Precio Antes`)/chileperfume.cl.m$`Precio Antes`*100
chileperfume.cl.m

#(almacenando Informacion sairam.cl) creacion del dataframe 
eliteperfume.cl.m<- data.frame(textTitulos22,textoPrecioAntes22,textoPrecioDespues22)
colnames(eliteperfume.cl.m) <-c("Nombre","Precio Antes", "Precio Ahora")
eliteperfume.cl.m$Variacion <- (eliteperfume.cl.m$`Precio Ahora`- eliteperfume.cl.m$`Precio Antes`)/eliteperfume.cl.m$`Precio Antes`*100
eliteperfume.cl.m

##############################################################################
#######################GRAFICOS y ESTADISTICA DESCRIPTIVA#####################
###################################PERFUMES###################################
##############################################################################

install.packages("ggplot2")
install.packages("dplyr")
install.packages("forcats")
library(ggplot2)
library(dplyr)
library(forcats)

########Grafico de Muestra de Perfumes Hombres y sus Precios#############
###############Graficos Sairam Hombre ######################################
par(mfrow=c(3,1))
ts.plot(sairam.cl[,2], col="blue", main ="Precio Antes Perfume Hombre Sairam", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(sairam.cl[,3], col="red", main ="Precio Ahora Perfume Hombre Sairam", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(sairam.cl[,4], col="green", main ="Variacion Perfume Hombre Sairam", 
        gpars = list(xlab="N°Perfumes", ylab="Variacion (%)"), lty=1)
plot(sairam.cl[,c(2:4)])

###############Graficos ChilePerfume Hombre ######################################
par(mfrow=c(3,1))
ts.plot(chileperfume.cl[,2], col="blue", main ="Precio Antes Perfume Hombre ChilePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(chileperfume.cl[,3], col="red", main ="Precio Ahora Perfume Hombre ChilePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(chileperfume.cl[,4], col="green", main ="Variacion Perfume Hombre ChilePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Variacion (%)"), lty=1)
plot(chileperfume.cl[,c(2:4)])

###############Graficos ElitePerfume Hombre ######################################
par(mfrow=c(3,1))
ts.plot(eliteperfume.cl[,2], col="blue", main ="Precio Antes Perfume Hombre ElitePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(eliteperfume.cl[,3], col="red", main ="Precio Ahora Perfume Hombre ElitePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(eliteperfume.cl[,4], col="green", main ="Variacion Perfume Hombre ElitePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Variacion (%)"), lty=1)
plot(eliteperfume.cl[,c(2:4)])

########Grafico de Muestra de Perfumes Mujeres y sus Precios#############
###############Graficos Sairam Mujer ######################################
par(mfrow=c(3,1))
ts.plot(sairam.cl.m[,2], col="blue", main ="Precio Antes Perfume Mujer Sairam", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(sairam.cl.m[,3], col="red", main ="Precio Ahora Perfume Mujer Sairam", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(sairam.cl.m[,4], col="green", main ="Variacion Perfume Mujer Sairam", 
        gpars = list(xlab="N°Perfumes", ylab="Variacion (%)"), lty=1)
plot(sairam.cl.m[,c(2:4)])

###############Graficos ChilePerfume Mujer ######################################
par(mfrow=c(3,1))
ts.plot(chileperfume.cl.m[,2], col="blue", main ="Precio Antes Perfume Mujer ChilePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(chileperfume.cl.m[,3], col="red", main ="Precio Ahora Perfume Mujer ChilePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(chileperfume.cl.m[,4], col="green", main ="Variacion Perfume Mujer ChilePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Variacion (%)"), lty=1)
plot(chileperfume.cl.m[,c(2:4)])

###############Graficos ElitePerfume Mujer ######################################
par(mfrow=c(3,1))
ts.plot(eliteperfume.cl.m[,2], col="blue", main ="Precio Antes Perfume Mujer ElitePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(eliteperfume.cl.m[,3], col="red", main ="Precio Ahora Perfume Mujer ElitePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Precio"), lty=1)
ts.plot(eliteperfume.cl.m[,4], col="green", main ="Variacion Perfume Mujer ElitePerfume", 
        gpars = list(xlab="N°Perfumes", ylab="Variacion (%)"), lty=1)
plot(eliteperfume.cl.m[,c(2:4)])


###########################Estadisticas de los Perfumes#####################################
######################Estadisticas de Perfumes de Hombres###################################
Tabla.Promedio <- data.frame(Indicador=c("Promedio Precio Antes", "Promedio Precio Ahora", "Promedio de Variacion"),
                    Sairam=c(mean(sairam.cl$`Precio Antes`),mean(sairam.cl$`Precio Ahora`),mean(sairam.cl$Variacion)),
                    ChilePerfume=c(mean(chileperfume.cl$`Precio Antes`),mean(chileperfume.cl$`Precio Ahora`),mean(chileperfume.cl$Variacion)),
                    ElitePerfume=c(mean(eliteperfume.cl$`Precio Antes`), mean(eliteperfume.cl$`Precio Ahora`),mean(eliteperfume.cl$Variacion)))

Tabla.Maximo <- data.frame(Indicador=c("Maximo Precio Antes", "Maximo Precio Ahora", "Maximo de Variacion"),
                             Sairam=c(max(sairam.cl$`Precio Antes`),max(sairam.cl$`Precio Ahora`),min(sairam.cl$Variacion)),
                             ChilePerfume=c(max(chileperfume.cl$`Precio Antes`),max(chileperfume.cl$`Precio Ahora`),min(chileperfume.cl$Variacion)),
                             ElitePerfume=c(max(eliteperfume.cl$`Precio Antes`), max(eliteperfume.cl$`Precio Ahora`),min(eliteperfume.cl$Variacion)))

Tabla.Minimo <- data.frame(Indicador=c("Minimo Precio Antes", "Minimo Precio Ahora", "Minimo de Variacion"),
                             Sairam=c(min(sairam.cl$`Precio Antes`),min(sairam.cl$`Precio Ahora`),max(sairam.cl$Variacion)),
                             ChilePerfume=c(min(chileperfume.cl$`Precio Antes`),min(chileperfume.cl$`Precio Ahora`),max(chileperfume.cl$Variacion)),
                             ElitePerfume=c(min(eliteperfume.cl$`Precio Antes`), min(eliteperfume.cl$`Precio Ahora`),max(eliteperfume.cl$Variacion)))

Tabla.Desviacion.Estandar <- data.frame(Indicador=c("SD Precio Antes", "SD Precio Ahora", "SD de Variacion"),
                           Sairam=c(sd(sairam.cl$`Precio Antes`),sd(sairam.cl$`Precio Ahora`),sd(sairam.cl$Variacion)),
                           ChilePerfume=c(sd(chileperfume.cl$`Precio Antes`),sd(chileperfume.cl$`Precio Ahora`),sd(chileperfume.cl$Variacion)),
                           ElitePerfume=c(sd(eliteperfume.cl$`Precio Antes`), sd(eliteperfume.cl$`Precio Ahora`),sd(eliteperfume.cl$Variacion)))

Tabla.Varianza <- data.frame(Indicador=c("Varianza Precio Antes", "Varianza Precio Ahora", "Varianza de Variacion"),
                           Sairam=c(var(sairam.cl$`Precio Antes`),var(sairam.cl$`Precio Ahora`),var(sairam.cl$Variacion)),
                           ChilePerfume=c(var(chileperfume.cl$`Precio Antes`),var(chileperfume.cl$`Precio Ahora`),var(chileperfume.cl$Variacion)),
                           ElitePerfume=c(var(eliteperfume.cl$`Precio Antes`),var(eliteperfume.cl$`Precio Ahora`),var(eliteperfume.cl$Variacion)))

######################Estadisticas de Perfumes de Mujer######################################
Tabla.Promedio.m <- data.frame(Indicador=c("Promedio Precio Antes", "Promedio Precio Ahora", "Promedio de Variacion"),
                             Sairam=c(mean(sairam.cl.m$`Precio Antes`),mean(sairam.cl.m$`Precio Ahora`),mean(sairam.cl.m$Variacion)),
                             ChilePerfume=c(mean(chileperfume.cl.m$`Precio Antes`),mean(chileperfume.cl.m$`Precio Ahora`),mean(chileperfume.cl.m$Variacion)),
                             ElitePerfume=c(mean(eliteperfume.cl.m$`Precio Antes`), mean(eliteperfume.cl.m$`Precio Ahora`),mean(eliteperfume.cl.m$Variacion)))

Tabla.Maximo.m <- data.frame(Indicador=c("Maximo Precio Antes", "Maximo Precio Ahora", "Maximo de Variacion"),
                           Sairam=c(max(sairam.cl.m$`Precio Antes`),max(sairam.cl.m$`Precio Ahora`),min(sairam.cl.m$Variacion)),
                           ChilePerfume=c(max(chileperfume.cl.m$`Precio Antes`),max(chileperfume.cl.m$`Precio Ahora`),min(chileperfume.cl.m$Variacion)),
                           ElitePerfume=c(max(eliteperfume.cl.m$`Precio Antes`), max(eliteperfume.cl.m$`Precio Ahora`),min(eliteperfume.cl.m$Variacion)))

Tabla.Minimo.m <- data.frame(Indicador=c("Minimo Precio Antes", "Minimo Precio Ahora", "Minimo de Variacion"),
                           Sairam=c(min(sairam.cl.m$`Precio Antes`),min(sairam.cl.m$`Precio Ahora`),max(sairam.cl.m$Variacion)),
                           ChilePerfume=c(min(chileperfume.cl.m$`Precio Antes`),min(chileperfume.cl.m$`Precio Ahora`),max(chileperfume.cl.m$Variacion)),
                           ElitePerfume=c(min(eliteperfume.cl.m$`Precio Antes`), min(eliteperfume.cl.m$`Precio Ahora`),max(eliteperfume.cl.m$Variacion)))

Tabla.Desviacion.Estandar.m <- data.frame(Indicador=c("SD Precio Antes", "SD Precio Ahora", "SD de Variacion"),
                                        Sairam=c(sd(sairam.cl.m$`Precio Antes`),sd(sairam.cl.m$`Precio Ahora`),sd(sairam.cl.m$Variacion)),
                                        ChilePerfume=c(sd(chileperfume.cl.m$`Precio Antes`),sd(chileperfume.cl.m$`Precio Ahora`),sd(chileperfume.cl.m$Variacion)),
                                        ElitePerfume=c(sd(eliteperfume.cl.m$`Precio Antes`), sd(eliteperfume.cl.m$`Precio Ahora`),sd(eliteperfume.cl.m$Variacion)))

Tabla.Varianza.m <- data.frame(Indicador=c("Varianza Precio Antes", "Varianza Precio Ahora", "Varianza de Variacion"),
                             Sairam=c(var(sairam.cl.m$`Precio Antes`),var(sairam.cl.m$`Precio Ahora`),var(sairam.cl.m$Variacion)),
                             ChilePerfume=c(var(chileperfume.cl.m$`Precio Antes`),var(chileperfume.cl.m$`Precio Ahora`),var(chileperfume.cl.m$Variacion)),
                             ElitePerfume=c(var(eliteperfume.cl.m$`Precio Antes`),var(eliteperfume.cl.m$`Precio Ahora`),var(eliteperfume.cl.m$Variacion)))

install.packages("xlsx")
library("xlsx")

##Exportamos las tablas de datos de hombres a excel
write.xlsx(chileperfume.cl, "EstPerfumesH.xlsx", sheetName = "ChilePerfume")
write.xlsx(eliteperfume.cl, "EstPerfumesH.xlsx", sheetName = "ElitePerfume", append = TRUE)
write.xlsx(sairam.cl, "EstPerfumesH.xlsx", sheetName = "Sairam", append = TRUE)
write.xlsx(Tabla.Maximo, "EstPerfumesH.xlsx", sheetName = "Maximo", append = TRUE)
write.xlsx(Tabla.Desviacion.Estandar, "EstPerfumesH.xlsx", sheetName = "Desv Estandar", append = TRUE)
write.xlsx(Tabla.Minimo, "EstPerfumesH.xlsx", sheetName = "Minimo", append = TRUE)
write.xlsx(Tabla.Promedio, "EstPerfumesH.xlsx", sheetName = "Promedio", append = TRUE)
write.xlsx(Tabla.Varianza, "EstPerfumesH.xlsx", sheetName = "Varianza", append = TRUE)

##Exportamos las tablas de datos de mujeres a excel
write.xlsx(chileperfume.cl.m, "EstPerfumesM.xlsx", sheetName = "ChilePerfume")
write.xlsx(eliteperfume.cl.m, "EstPerfumesM.xlsx", sheetName = "ElitePerfume", append = TRUE)
write.xlsx(sairam.cl.m, "EstPerfumesM.xlsx", sheetName = "Sairam", append = TRUE)
write.xlsx(Tabla.Maximo.m, "EstPerfumesM.xlsx", sheetName = "Maximo", append = TRUE)
write.xlsx(Tabla.Desviacion.Estandar.m, "EstPerfumesM.xlsx", sheetName = "Desv Estandar", append = TRUE)
write.xlsx(Tabla.Minimo.m, "EstPerfumesM.xlsx", sheetName = "Minimo", append = TRUE)
write.xlsx(Tabla.Promedio.m, "EstPerfumesM.xlsx", sheetName = "Promedio", append = TRUE)
write.xlsx(Tabla.Varianza.m, "EstPerfumesM.xlsx", sheetName = "Varianza", append = TRUE)


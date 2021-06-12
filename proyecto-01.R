## PROYECTO 01

#Cargamos el directorio de trabajo
setwd("C:/Users/Daniel Segura/Documents/Tarea PROGRA")

#Se importan los datos de los rios banano y estrella
dcl <- read.csv("./liberia_datos_climaticos.csv", na.strings = "")
head(dcl)
dim(dcl)

#Limpiamos los datos que puedan estar vacioso 
dcl <- na.omit(dcl)
dcl[!complete.cases(dcl),]

names(dcl) <- c(
  "fecha",
  "temperatura", 
  "huRelativa",
  "velViento", 
  "lluvia", 
  "irradiacion",
  "evaTranspi"
)
attach(dcl)

# Carga de ggplot2
library(ggplot2)
# Carga de dplyr
library(dplyr)
# Carga de plotly
library(plotly)
library(gridExtra)
library(gtable)
library(grid)


# Conversion de valores char a numericos
dcl$temperatura <- as.numeric(gsub(",",".", dcl$temperatura))
dcl$huRelativa <- as.numeric(gsub(",",".", dcl$huRelativa))
dcl$velViento <- as.numeric(gsub(",",".", dcl$velViento))
dcl$lluvia <- as.numeric(gsub(",",".", dcl$lluvia))
dcl$irradiacion <- as.numeric(gsub(",",".", dcl$irradiacion))
dcl$evaTranspi <- as.numeric(gsub(",",".", dcl$evaTranspi))

# Conversion de fecha en char a date
dcl <- dcl %>% mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))


# Visualizacion de los datos sin nungun tipo de trato.
## Temperatura
a1 <- ggplot(dcl, aes(fecha, temperatura))+
  geom_line(color="orange")+
  geom_point()+
  ggtitle("Temperatura")+
  ylab("[°C]")+
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())


## Humedad Relatica
a2 <- ggplot(dcl, aes(fecha, huRelativa))+
  geom_col(color="blue")+
  ggtitle("Humedad relativa")+
  ylab("[%]")+
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

## Viento
a3 <- ggplot(dcl, aes(fecha, velViento))+
  geom_col(color="green")+
  ggtitle("Velocidad Viento")+
  ylab("[m/s]")+
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

## LLuvia
a4 <- ggplot(dcl, aes(fecha, lluvia))+
  geom_col(color="orange")+
  ggtitle("Lluvia")+
  ylab("[mm]")+
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

## Irradiacion
a5 <- ggplot(dcl, aes(fecha, irradiacion))+
  geom_line(color="yellow")+
  geom_point()+
  ggtitle("Irradiacion")+
  ylab("[W/m2]")+
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())


## EvapoTranspiracion
a6 <- ggplot(dcl, aes(fecha, evaTranspi))+
  geom_col(color="blue")+
  ggtitle("EvapoTranspiracion")+
  ylab("[mm]")+
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.title.x.bottom = element_blank()
  )

## Visualizamos los 6 datos de Liberia
grid.arrange(a1,a2,a3,a4,a5,a6, ncol=1, nrow=6)

# Manipulacion de los datos y su visualizacion
dcl_by_month <- dcl %>% group_by(month = format(fecha, "%Y-%m")) %>% arrange(month)

## Promediacion de las 6 variables
dcl_monthly <- dcl_by_month %>% summarise(temperatura = mean(temperatura))
dcl_monthly <- dcl_by_month %>% summarise(huRelativa = mean(huRelativa)) %>% 
  merge(dcl_monthly, by="month")
dcl_monthly <- dcl_by_month %>% summarise(velViento = mean(velViento)) %>% 
  merge(dcl_monthly, by="month")
dcl_monthly <- dcl_by_month %>% summarise(lluvia = mean(lluvia)) %>% 
  merge(dcl_monthly, by="month")
dcl_monthly <- dcl_by_month %>% summarise(irradiacion = mean(irradiacion)) %>% 
  merge(dcl_monthly, by="month")
dcl_monthly <- dcl_by_month %>% summarise(evaTranspi = mean(evaTranspi)) %>% 
  merge(dcl_monthly, by="month")

## Generamos los graficos para los variables promediadas
p1 <- ggplot(dcl_monthly, aes(x=month, y=temperatura, group=1)) + 
  geom_line(color = "yellow")+
  geom_point()+
  ggtitle("Promedios mensuales de temperatura de Liberia") +
  ylab("Temp [°C]") +
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

p2 <- ggplot(dcl_monthly, aes(x=month, y=huRelativa)) +
  geom_col(color = "blue")+ 
  ggtitle("Promedios mensuales de humedad relativa de Liberia") +
  xlab("Fecha") + 
  ylab("HumedadRelativa (%)") +
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

p3 <- ggplot(dcl_monthly, aes(x=month, y=velViento)) +
  geom_col(color = "green")+ 
  ggtitle("Promedios mensuales de velocidad de viento en Liberia") +
  xlab("Fecha") + 
  ylab("Velocidad [m/s]") +
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

p4 <- ggplot(dcl_monthly, aes(x=month, y=lluvia)) +
  geom_col(color = "blue")+ 
  ggtitle("Promedios mensuales de lluvia en Liberia") +
  xlab("Fecha") + 
  ylab("Lluvia [mm]") +
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

p5 <- ggplot(dcl_monthly, aes(x=month, y=irradiacion, group=1)) +
  geom_line(color = "orange") +
  geom_point()+ 
  ggtitle("Promedios mensuales de irradiacion de Liberia") +
  ylab("Irradiacion [W/m2]") +
  theme(axis.text.x = element_blank(), axis.title.x.bottom = element_blank())

p6 <- ggplot(dcl_monthly, aes(x=month, y=evaTranspi)) +
  geom_col(color = "blue")+ 
  ggtitle("Promedios mensuales de EvapoTranspiracion en Liberia") +
  xlab("Fecha") + 
  ylab("EvapoTranspiracion [mm]") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Visualizacion de los 6 datos de Liberia
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=1, nrow=6)


# Comparacion de las datos de Lluvia y EvapoTranspiracion
## visualizacion de lluvia y evapotranspiracion en lineas
## Manipulacion de lo datos de irradiacion y lluvia
dcl_by_month <- dcl %>% 
  group_by(month = format(fecha, "%Y-%m")) %>% 
  arrange(month)
dcl_by_month <- dcl_by_month %>% 
  summarise(irradiacion = sum(irradiacion), lluvia = sum(lluvia) )

plot_ly(
  data = dcl_by_month,
  x = ~ month, y = ~ irradiacion,
  name = "Irradiavcion",
  type = "scatter",
  mode = "lines",
  line = list(color = "yellow")
) %>% add_trace(
  y = dcl_by_month$lluvia,
  name = "Lluvia",
  mode = "lines",
  line = list(color = "blue")
) %>% layout(
  title = "Acumulaciones mensuales de irradiacion y lluvia en Liberia",
  yaxis = list(title = "mm"),
  xaxis = list(title = "Fecha")
)

# Carga de hrbrthemes (temas adicionales para ggplot2)
library(hrbrthemes)

# Relacion entre los datos 
## Relacion entre temperatura y lluvia
c1 <- ggplot(
  dcl, aes(x = temperatura)
) + geom_point(
  aes(y = lluvia),
  colour = "orange"
) +
  ggtitle("Temperatura - lluvia") +
  xlab("Temperatura [°C]") +
  ylab("Lluvia [mm]")

## Relacion entre temperatura y lluvia
c2 <- ggplot(
  dcl, aes(x = huRelativa)
) + geom_point(
  aes(y = lluvia),
  colour = "blue"
) +
  ggtitle("Humedad Relativa - lluvia") +
  xlab("Humedad Relativa [%]") +
  ylab("Lluvia [mm]")

## Relacion entre Velocidad Viento y lluvia
c3 <- ggplot(
  dcl, aes(x = velViento)
) + geom_point(
  aes(y = lluvia),
  colour = "orange"
) +
  ggtitle("Velocidad Viento - lluvia") +
  xlab("Velocidad viento [m/s]") +
  ylab("Lluvia [mm]")

## Relacion Irradiacion y lluvia
c4 <- ggplot(
  dcl, aes(x = irradiacion)
) + geom_point(
  aes(y = lluvia),
  colour = "yellow"
) +
  ggtitle("Irradiacion - lluvia") +
  xlab("Irradiacion [W/m2]") +
  ylab("Lluvia [mm]")

## Relacion entre Evapotranspiracion y lluvia
c5 <- ggplot(
  dcl, aes(x = evaTranspi)
) + geom_point(
  aes(y = lluvia),
  colour = "green"
) +
  ggtitle("EvapoTranspiracion - lluvia", ) +
  xlab("EvapoTranspiracion [mm]") +
  ylab("Lluvia [mm]")

## Relacion entre Irradiacion y Humedad relativa
c6 <- ggplot(
  dcl, aes(x = irradiacion)
) + geom_point(
  aes(y = huRelativa),
  colour = "purple"
) +
  ggtitle("Irradiacion - Humedad") +
  xlab("Irradiacion [W/m2]") +
  ylab("Humedad [%]")

grid.arrange(
  c1, c2, c3, c4,c5,c6, ncol = 3,
  bottom=textGrob("Relaciones entre datos de Liberia")
)


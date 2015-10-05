#Definir directorio
setwd("C:/Users/familia CB/Desktop/Datos Control")

#Abrir archivo
file <- paste0(getwd(),"/Control2.csv")
data <- read.csv2("Control2.csv", head= TRUE, sep = ';')
str(data)

#3.- a)Retorno esperado para cada empresa
RIGPA <- mean(data$RETORNO.IGPA)
RIGPA
RSMSAAM <- mean(data$RETORNO.SM.SAAM)
RSMSAAM
RSQM.B <- mean(data$RETORNO.SQM.B)
RSQM.B
RVAPORES <- mean(data$RETORNO.VAPORES)
RVAPORES

#b)Desviacion estandar de los retornos de cada empresa
DsIGPA <- sd(data$RETORNO.IGPA)
DsIGPA
DsSMSAAM <- sd(data$RETORNO.SM.SAAM)
DsSMSAAM
DsSQM.B <- sd(data$RETORNO.SQM.B)
DsSQM.B
DsVAPORES <-sd(data$RETORNO.VAPORES)
DsVAPORES


#c)Matriz de correlacion 
x <- data.frame(data$RETORNO.IGPA, data$RETORNO.SM.SAAM, data$RETORNO.SQM.B, data$RETORNO.VAPORES)
cor(x)


#4.- Regresion para cada empresa
#SM.SAAM.S.A.
regSMSAAM <- lm(data$RETORNO.SM.SAAM ~ data$RETORNO.IGPA, data=data)
regSMSAAM

#SQM-B
regSQM.B <- lm(data$RETORNO.SQM.B ~ data$RETORNO.IGPA, data=data)
regSQM.B

#VAPORES
regVAPORES <- lm(data$RETORNO.VAPORES ~ data$RETORNO.IGPA, data=data)
regVAPORES
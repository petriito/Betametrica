############ PROYECTO FINAL ###########################
############ MODULO 7 #################################
############ LUIS PEDRO RODRIGUEZ VILLALTA ############
########### DEPOSITOS SISTEMA BANCARIO GUATEMALA#############

library(readxl)
library(randomForest)
library(rpart)
library(e1071)
library(gbm)
library(dplyr)
library(lubridate)
library(fpp2)
library(seasonal)
library(TSrepr)
library(Metrics)
library(seastests)
library(rugarch)
library(tsutils)
library(tidyr)
library(dplyr)
library(seasonalview)
library(ggplot2)
library(prophet)
library(zoo)
library(autoplotly)
library(writexl)
library(seasonalview)
library(trend)
library(lmtest)
library(forecast)
library(urca)
library(highcharter)

##file.choose()






#############################SECCCION A############################




# Cargar el archivo

df <- read_excel("C:\\Users\\lprodriguez\\Desktop\\EXPERTO EN CIENCIA DE ATOS\\MODULO 7\\PROYECTO FINAL\\data2.xlsx")
df$FECHA <- as.Date(df$FECHA)
str(df)
data_sistema=select(df,FECHA, VALOR)
str(data_sistema)


## Datos no estan pivoteados, por lo que hay que ordenar por total de mes y agrupar
data_sistema <- data_sistema %>%
  mutate(AÑO = year(FECHA),
         MES = month(FECHA)) %>%
  group_by(AÑO, MES) %>%
  summarise(TOTAL_MES = sum(VALOR), .groups = "drop")

# recompongo la fecha uniendo MES y AÑO
data_sistema <- data_sistema %>%
  mutate(FECHA = ymd(paste(AÑO, MES, "01")) %>% 
           ceiling_date(unit = "month") - days(1))

##select data frame compuesto
data_sistema= select(data_sistema,FECHA,TOTAL_MES)
View(data_sistema)
##valores con sentido
summary(data_sistema$TOTAL_MES)
sum(data_sistema$TOTAL_MES)

### crear crecimiento interanual
data_sistema <- data_sistema %>%
  mutate(var_por_anual = ifelse(row_number() > 12, 
                                (TOTAL_MES/lag(TOTAL_MES, 12) - 1) * 100, 
                                NA)) %>%
  replace_na(list(var_por_anual = 0))  # o cualquier otro valor que prefieras para los NA
data_sistema=select(data_sistema,FECHA, var_por_anual) 
###eliminar las primeras 12 filas con 0
data_sistema=filter(data_sistema,var_por_anual!=0)
##### ver que tenga sentido la data
summary(data_sistema$var_por_anual)

##EXAMINAR DATAS
str(data_sistema)  
View(data_sistema)














##########crear serie temporal###########################
ts_data_sistema <-ts(data_sistema$var_por_anual, frequency=12, start=2014)
ts_data_sistema

# analisis exploratorio serie
##grafico serie temp
# Es necesario para usar autoplot con objetos de series temporales
depositos=ts_data_sistema
autoplot(log(depositos), colour = "#00a0dc") +
  labs(title = "Variación Interanual Sistema de 2014-2023",       
       x = "Tiempo",
       y = "% variacion") +
  theme_bw()



#Estacionariedad en media, estacionalidad, heterocedasticidad, normalidad, gr?ficos 
nortsTest::check_residuals(log(depositos),unit_root ="adf",
                           normality="epps",arch="Lm", seasonal="ocsb", plot=TRUE)

##serie es no estacionaria, estacionaria en estacionalidad, normal
## homocedastica, falta volverla estacionaria


#Gr?fico de estacionalidad,
tsutils::seasplot(log(depositos),outplot=3)
##  El valor p de 0.98 proporciona una evidencia estadística fuerte de que no hay estacionalidad en la serie.

##Si la estacionalidad es multiplicativa, no aplica dado que no hay estacionalidad
#mseastest(log(depositos), outplot=2)
## la grafica da la respuesta y dijo que es aditiva pero dado el p valor alto
## no es significativa ni importante estadisticamente aditiva

############# tendencia
tsutils::seasplot(log(depositos),outplot=2)
### es muy cecano a uno tendencia no es significativa














########### TRANSFORMAR SERIE DE TIEMPO
##tratar raiz unitaria (no estacionariedad)###############

###CUANTAS DERIVADAS POR SER NO ESTACIONARIA PARA VOLVERLA ESTACIONARIA###
nsdiffs(log(depositos), test=c("ocsb"))
nsdiffs(log(depositos), test=c("ch"))
## NO REQUIERE ninguna derivada, es estacionaria según estas pruebas






############# pruebas de estacionariedad ###############

kpss1<-ur.kpss(log(depositos),
               type=c("tau"),
               lags = "short")
summary(kpss1)
##h0 estacionariedad
"Test is of type: tau with 4 lags. 

Value of test-statistic is: 0.1465 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.119 0.146  0.176 0.216"
### como el estadistico es mayor, se rechaza h0
## la serie no es estacionaria con un 90 y 95%


















#formalmente ciertos test

ers1<-ur.ers(log(depositos),
             type=c("DF-GLS"),
             model=c("trend"))
summary(ers1)
#h0= raiz unitaria


"Value of test-statistic is: -2.64

Critical values of DF-GLS are:
                 1pct  5pct 10pct
critical values -3.46 -2.93 -2.64"

"como estadistico no es mayor no rechazo h0 de 
raiz unitaria por tanto hay raiz unitaria
y es no estacionaria"















pp1<-ur.pp(log(depositos),
           type="Z-tau",
           model=c("trend"))
summary(pp1)

#h0 raiz unitaria
"Value of test-statistic, type: Z-tau  is: -2.7082 

           aux. Z statistics
Z-tau-mu              0.7569
Z-tau-beta            0.4397

Critical values for Z statistics: 
                     1pct      5pct     10pct
critical values -4.039334 -3.448744 -3.149308"

# dado q esta no es mayor que varios calc se concluye
## que hay raiz unitaria no se rechaza h0 y la serie es no estacionaria


































###############SECCION B###################






#veamos el grado de los correlogramas
#capturemos la forma del modelo (proceso)

par(mfrow=c(1,2))
Acf(log(depositos))
Pacf(log(depositos))

##buscar mejor modelo PDQ
m0=auto.arima(log(depositos))
m0

#mejor modelo
"Series: log(depositos) 
ARIMA(1,0,2)(0,0,1)[12] with non-zero mean 

Coefficients:
         ar1     ma1     ma2     sma1    mean
      0.8714  0.2898  0.2428  -0.5526  2.3500
s.e.  0.0500  0.0979  0.0912   0.0853  0.0566

sigma^2 = 0.01199:  log likelihood = 92.11
AIC=-172.21   AICc=-171.45   BIC=-155.64"





## correr un escenario
m1 <- Arima(log(depositos), 
            order = c(1,0,2),
            seasonal=list(order=c(0,0,1)))
m1
"Coefficients:
         ar1     ma1     ma2     sma1    mean
      0.8714  0.2898  0.2428  -0.5526  2.3500
s.e.  0.0500  0.0979  0.0912   0.0853  0.0566"

#division coeficientes es mayor q 2 siempre, son funcionales
#correctos indicadores AIC , BIC, AIcc

## se lige ARIMA , dado que la serie no presenta estacinalidad
## tampoco presenta tendencia.













################ VALIDACION MODELO #############################

summary(m1)# RESULTADOS
"
Training set error measures:
                        ME      RMSE        MAE       MPE     MAPE      MASE       ACF1
Training set -0.0003799548 0.1071169 0.07571241 -0.311183 3.485254 0.1964884 0.01696044
> "
#excelente mae y mape






######## INDEPENDENCIA RESIDUOS############
Box.test(m1$residuals,
         type=c("Ljung-Box"),
         lag = 6)
plot(m1$residuals)
#Ho:Residuos independientes
#si vp<0.05 rechazo ho.

"Box-Ljung test

data:  m1$residuals
X-squared = 0.75647, df = 6, p-value = 0.9932
"

#p valor es muy alto no se rechaza h0 de residuos independientes no existe autocorrelacion





############## CORRELOGRAMAS ###################
##examinar mejor modelo examinando correlogramas simple y parcial

par(mfrow=c(1,2))
Acf(m1$residuals)
Pacf(m1$residuals)

## se ha extraido la mayor parte de información de AR1, MA2, D0




### grafico circulo unitario
##PRIMERO EJECUTAR GRAFICO ARMA INVERSE VER CIRCULOS DE AR Y MA RAICES
# AL VER SI  LOS PUNTOS ESTAN DENTRO DE LOS DOS CIRCULOS SIRVE 
##PARA PREDECIR
tail(depositos,n=1)

plot(m1)
















#########PRONOSTICO

#grafico dinamico
f1.1 <- forecast(m1,h=4)
hchart(f1.1)


#pero esta en logaritmos...  se usa exponente para convertir
### original vrs modelo

par(mfrow=c(1,1))
str(f1)
cols <- c("blue","red","brown")
ts.plot(depositos, exp(f1.1$fitted),
        exp(f1.1$mean),col=cols)
legend("topleft", c("original",
                    "ajustado","proyectado"), 
       cex=1.2,fill=cols)

#El valor ajustado por el modelo se se parece al original, pero en los
## periodos recientes el pronostico es relativamente mayor al historico
## seria recomendable comparar con otros modelos

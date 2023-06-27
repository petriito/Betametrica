#PROYECTO FINAL MODULO 5 CLUSTERS
# ALUMNO LUIS PEDRO RODRIGUEZ VILLALTA
# 27-06-2023


#----- Clusters Jerarquicos #----

library(openxlsx)
library(tidyverse)
library(dplyr)
library(factoextra)
library(cluster)
library(devtools)
library("fpc")
library(NbClust)
file.choose()
data <- read.xlsx("C:\\Users\\Petro\\Desktop\\EXPERTO EN CIENCIA DE DATOS\\MODULO 5\\MACHINE-LEARNING-1_SL8ZUQ\\BASES\\parte 2\\BOL_BP_MAY_ 2017.xlsx",
                  sheet = "INDICADORES")
View(data)
##carpinteria
##elimino metadata totalmente innecesaria del exce luego hago dos operaciones de pivoteo con tidyverse

#pivoteo de filas a columnas, primera columna a filas por valor  
data2=data %>% pivot_longer(-NOMBRE.DEL.INDICADOR,
                    names_to='INDICADOR',
                    values_to='VALORES') 
View(data2)

##pivoteo, filas primera columna que deben pasar a categorias una columna por categoria
data3= data2 %>% pivot_wider(names_from = NOMBRE.DEL.INDICADOR, values_from = VALORES)
view(data3)
nombres <- data3$INDICADOR
#FIN carpinteria dataframe


#se escala la base para mantener a una sola escala todas las variables
base <- as.data.frame(scale(data3[,-1]))
## todos con una misma escala normalizada
row.names(base) <- nombres


## AGLOMERACION hc method
#"ward.D", "ward.D2", "single", "complete", "average


##DISTANCIA hc metric
# "euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski"



#### CLUSTER 1
# Compute hierarchical clustering and cut into 4 clusters
cluster1 <- hcut(base, k = 4, stand = TRUE, 
                 hc_metric = "manhattan",hc_method = "ward.D2")

# Visualize el cluster de mejor manera
fviz_dend(cluster1, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
## coef divisivilidad
ncluster <- diana(base,metric = "manhattan")
ncluster$dc

### categoriza individualmente CAPITAL, VISION FUND y luego dos clusters con varios bancos y obtiene un coef de divisi. de 0.843,
## por lo que se  concluye que este cluster agrupa mejor los grupos de bancos





#### CLUSTER 2
# Compute hierarchical clustering and cut into 4 clusters
cluster2 <- hcut(base, k = 4, stand = TRUE, 
                 hc_metric = "canberra",hc_method = "ward.D")

# Visualize el cluster de mejor manera
fviz_dend(cluster2, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
## coef divisivilidad
ncluster <- diana(base,metric = "canberra")
ncluster$dc

#categorizo este cluster dos grupos de 3-4 bancos y dos grupos de 7-9 bancos,  y obtiene un coeficiente de div de 0.819866






















############### CLUSTER NO JERARQUICO

data <- read.xlsx("C:\\Users\\Petro\\Desktop\\EXPERTO EN CIENCIA DE DATOS\\MODULO 5\\MACHINE-LEARNING-1_SL8ZUQ\\BASES\\parte 2\\BOL_BP_MAY_ 2017.xlsx",
                  sheet = "INDICADORES")
View(data)
##carpinteria
##elimino metadata totalmente innecesaria del exce luego hago dos operaciones de pivoteo con tidyverse

#pivoteo de filas a columnas, primera columna a filas por valor  
data2=data %>% pivot_longer(-NOMBRE.DEL.INDICADOR,
                            names_to='INDICADOR',
                            values_to='VALORES') 
View(data2)

##pivoteo, filas primera columna que deben pasar a categorias una columna por categoria
data3= data2 %>% pivot_wider(names_from = NOMBRE.DEL.INDICADOR, values_from = VALORES)
view(data3)
nombres <- data3$INDICADOR
#FIN carpinteria dataframe


#se escala la base para mantener a una sola escala todas las variables
base <- as.data.frame(scale(data3[,-1]))
## todos con una misma escala normalizada
row.names(base) <- nombres

#se especifíca la distancia y el método de aglomeración
##muestra los 4 grupos de cluster
cnj <- kmeans(base,4)
cnj

#media de cada cluster
aggregate(base,
          by=list(cnj$cluster),
          FUN=mean)
#visualizando el cluster
fviz_cluster(cnj,data=base)
### Tal y como sucedio con los cluster jerarquicos y se concluyo,
## lo mismo sucede con el cluster no jerarquico clasifica 2 cluster
## individuales de CAPITAL Y VISIONFUND y los otros 2 en dos grupos de 9-11
## bancos


#----- ¿Cuántos clusters son los óptimos? # -------
clustersoptimos <- NbClust(base,
                           distance = "euclidean",
                           min.nc = 2,
                           max.nc = 12,
                           method="ward.D",
                           index = "all")
#***** Conclusion *****                            
#>According to the majority rule, the best number of clusters is  3, es decir
# que tomando en cuenta la distancia y metodo de aglomeracion elegidos
## el numero optimo es 3 clusters


















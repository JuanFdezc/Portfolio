#NOMBRES: Christopher Cobo y Juan Fernández
#FECHA: 10/12/2023



# Paquetes ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)

# Ver los datos que tenemos -----------------------------------------------

golpes <- read.table("C:/Users/juanf/Downloads/Coup_Data_v2.0.0-1-1.csv", sep = ",", header = TRUE)

# Ver la tabla con los datos
View(golpes)

#numero de filas
filas <- nrow(golpes)
filas
#numero de columnas
cols <- ncol(golpes)
cols

#nombre de las variables
golpesestado <- names(golpes)
print(golpesestado)

unique(golpes$event_type)

# 1. estudio y analisis descriptivo ------------------------------------------


tab_country <-table(golpes$country)
tab_country
view(tab_country)
mean(tab_country)
max(tab_country)
tab_year <- table(golpes$year)
mean(tab_year)
tab_year
max(tab_year)
unique(golpes$country)
unique(golpes$year)


barplot(tab_country)
barplot(tab_year)
                
                

aggregate(x = golpes$realized,
          by = list(golpes$year, golpes$country),
          FUN = sum)

aggregate(x = golpes$unrealized,
          by = list(golpes$year, golpes$country),
          FUN = sum)

aggregate(x = golpes$conspiracy,
          by = list(golpes$year),
          FUN = sum)


aggregate(x = golpes$attempt,
          by = list(golpes$year),
          FUN = sum)


# Grafico de barras de event type
tab_eventype <- table(golpes$event_type)
tab_eventype
181/943
336/943
426/943
barp1 <- barplot(tab_eventype, 
         col = c("royalblue", "seagreen", "purple"))


# Grafico de barras de military

tab_military <- table(golpes$military)
tab_military
tab_military/filas
barp2 <- barplot(tab_military,
         col = c("firebrick", "seagreen"),
         legend.text =c( "No", "Si"),
         main = "Golpes de estado por parte de los militares")

# Grafico de barras de dissident

tab_dissident <- table(golpes$dissident)
tab_dissident
tab_dissident/filas
barp3 <- barplot(tab_dissident,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado por parte de disidentes/oponentes")

# Grafico de barras de rebel

tab_rebel <- table(golpes$rebel)
tab_rebel
tab_rebel/filas
barp4 <- barplot(tab_rebel,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado por parte de rebeldes")


# Grafico de barras de palace

tab_palace <- table(golpes$palace)
tab_palace
tab_palace/filas
barp5 <- barplot(tab_palace,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"), 
                 main = "Golpes de estado por parte de palacio")

# Grafico de barras de foreign

tab_foreign <- table(golpes$foreign)
tab_foreign
tab_foreign/filas
barp <- barplot(tab_foreign,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"), 
                main = "Golpes de estado con apoyo extranjero")

# Grafico de barras de auto

tab_auto <- table(golpes$auto)
tab_auto
tab_auto/filas
barp7 <- barplot(tab_auto,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpe de estado autoprovocado")

# Grafico de barras de resign

tab_resign <- table(golpes$resign)
tab_resign
tab_resign/filas
barp7 <- barplot(tab_resign,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que se producen una resignacion")

# Grafico de barras de popular

tab_popular <- table(golpes$popular)
tab_popular
tab_popular/filas
barp9 <- barplot(tab_popular,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que se producen revueltas populares")

# Grafico de barras de counter

tab_counter <- table(golpes$counter)
tab_counter
tab_counter/filas
barp10 <- barplot(tab_counter,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que se produce un contra golpe")

# Grafico de barras de other

tab_other <- table(golpes$other)
tab_other
tab_other/filas
barp11 <- barplot(tab_other,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que no se identifican al actor o al evento")


# Grafico de barras de noharm

tab_noharm <- table(golpes$noharm)
tab_noharm
tab_noharm/filas
barp12 <- barplot(tab_noharm,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"), 
                 main = "Golpes de estado en los que el ejecutivo de puesto no fue herido")


# Grafico de barras de killed

tab_killed<- table(golpes$killed)
tab_killed
tab_killed/filas
barp13 <- barplot(tab_killed,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que el ejecutivo depuesto fue asesinado")


# Grafico de barras de home arrests

tab_harrests <- table(golpes$harrest)
tab_harrests
tab_harrests/filas
barp14 <- barplot(tab_harrests,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que el ejecutivo depuesto estuvo bajo arresto domiciliario")


# Grafico de barras de jailed

tab_jailed <- table(golpes$jailed)
tab_jailed
tab_jailed/filas
barp115 <- barplot(tab_jailed,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que el ejecutivo depuesto fue encarcelado")


# Grafico de barras de tried

tab_tried <- table(golpes$tried)
tab_tried
tab_tried/filas
barp16 <- barplot(tab_tried,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"), 
                 main = "Golpes de estado en los que el ejecutivo depuesto fue llevado a juicio")


# Grafico de barras de fled
tab_fled <- table(golpes$fled)
tab_fled
tab_fled/filas
barp17 <- barplot(tab_fled,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que el ejecutivo depuesto huyo³")


# Grafico de barras de exile

tab_exile <- table(golpes$exile)
tab_exile
tab_exile/filas
barp18 <- barplot(tab_exile,
                 col = c("firebrick", "seagreen"),
                 legend.text =c( "No", "Si"),
                 main = "Golpes de estado en los que el ejecutivo depuesto fue exiliado")



# 2. Componentes principales (PCA) ------------------------------
library(corrgram)

# Borramos cowcode: Ya identificamos al pais con su id y el propio nombre del 
# pais por lo que ya no necesitamos este identificador
golpes <- select(golpes, -cowcode)

# Juntamos id con country para tener un unico identificador de cada pais y golpe de estado:
golpes$coup_id <- paste(golpes$coup_id, golpes$country, sep = " _ ")

# Borramos country ya que estÃ¡ indicado en el id ya:
golpes <- select(golpes, -country)

# Borramos unrealized, realized, conspiracy, attempt, year, coup_id; ya que 
# de esta forma evitamos la redundancia en los datos.
golpes <- select(golpes, -unrealized)
golpes <- select(golpes, -realized)
golpes <- select(golpes, -conspiracy)
golpes <- select(golpes, -attempt)
golpes <- select(golpes, -year)

# Ponemos el nombre del pais con el id del golpe de estado como indice de las filas.
row.names(golpes) <- golpes$coup_id
golpes <- select(golpes, -coup_id)


# Convertimos a matriz el df y nos quedamos con las columnas de la 2 a la 19.
events.data <- as.matrix(golpes[,2:19])


# Creamos la variable event_type:
event_type <- as.numeric(d1$event_type == "coup", 1, 0)
table(event_type)
# 517 golpes no realizados: conspiracy y attempted
# 426 realizados: coup

# Calculo de los componentes principales:
colMeans(golpes.data)
apply(golpes.data, 2, sd) # El 2 significa que lo hacemos por columnas, 1 por filas

golpes.pr <- prcomp(golpes.data, scale = T)
golpes.pr

# Resultados:
summary(golpes.pr)
biplot(golpes.pr)

# Calculamos la variabilidad de cada componente:
pr.var <- golpes.pr$sdev^2
pr.var

# VArianza explicada por CP:
pve <- pr.var/sum(pr.var)

## Grafico de la varianza explicada:
plot(pve, xlab = "Componentes principales",
     ylab = "Proporcion de varianza explicada",
     ylim = c(0, 1), type="b")

plot(cumsum(pve), xlab = "Componentes principales",
     ylab = "Proporcion de varianza explicada acumulada",
     ylim = c(0, 1), type="b")



# 3. Clusters -------------------------------------------------------------
library(factoextra)
library(cluster)
library(dendextend)

# Clustering jerarquico:
## Definimos los metodos de union:
metodos <- c("average", "single", "complete")
names(metodos) <- c("average", "single", "complete")

# Calculamos el coeficiente de aglomeracion:
ac <- function(x){
  agnes(golpes.pr$x[,1:4], method = x)$ac
}

sapply(metodos, ac)

# Seleccionamos el metodo "complete"
cluster <- agnes(golpes.pr$x[,1:4], method = "complete")
cluster

par(mfrow=c(1,1))
pltree(cluster, cex = 0.6, hang = -1, "Dendrograma")

# Cantidad optima de clusters: --------------------------------------------
p1 <- fviz_nbclust(golpes.pr$x[,1:4], FUN = hcut, method = "wss",
                   k.max= 15)+
  ggtitle("Metodo Elbow")

p2 <- fviz_nbclust(golpes.pr$x[,1:4], FUN = hcut, method = "silhouette",
                   k.max= 15)+
  ggtitle("Metodo silhouette")

p3 <- fviz_nbclust(golpes.pr$x[,1:4], FUN = hcut, method = "gap_stat",
                   k.max= 15)+
  ggtitle("Metodo Gap")

# Graficamos los tres metodos:
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

# Distancia de la matriz:
d <- dist(golpes.pr$x[,1:4] ,method = "euclidean")

# Creamos el cluster
final_clust <- hclust(dist(golpes.pr$x[, 1:4]), method = "complete")


final_clust_clust <- cutree(final_clust, k=2)
table(final_clust_clust)

final_clust_clust <- cutree(final_clust, k=4)
table(final_clust_clust)


# Visualizacion final:
fviz_cluster(list(data = golpes.pr$x[,1:2], cluster = final_clust_clust))









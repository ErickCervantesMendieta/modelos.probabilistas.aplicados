#Erick Cervantes Mendieta
#Tarea 6
#Modelos Probabilistas Aplicados

if (!require("ggplot2")) {
    install.packages("ggplot2")
}
require(ggplot2)

if (!require("gplots")) {
    install.packages("gplots")
}
require(gplots)

if (!require("plotly")) {
    install.packages("plotly")
}
require(plotly)

datos <- read.table("Prueba1.txt", header = TRUE)
datosc = numeric
datosc <- c(datos$Hombres, datos$Mujeres)
media = 9.1 

#t.test(datosc, mu = media)
#wilcox.test(datosc, mu = media, conf.int = TRUE)
#t.test(datos$Hombres, mu = media)
#t.test(datos$Mujeres, mu = media)
#var.test(datos$Hombres,datos$Mujeres)
#wilcox.test(datos$Hombres, datos$Mujeres)

png('datos1.png', width=2000,height=1800,res=300)
qqnorm(datosc, xlab = "Valores teóricos", ylab = "Valores del grado de escolaridad en México", main = NULL, col = "firebrick")
qqline(datosc)
dev.off()

png('datos2.png', width=2000,height=1800,res=300)
boxplot(datos$Hombres, datos$Mujeres, names=c("Hombres","Mujeres"), col = c("#65E8D7", "#E865DC"), notch=TRUE, ylab = "Grado promedio")
medias <- c(mean(datos$Hombres),mean(datos$Mujeres))
points(medias,pch=18,col="#B2041E")
dev.off()

datos2 <- read.table("Prueba2.txt", header = TRUE)

png('datos3.png', width=2000,height=1800,res=300)
qqnorm(datos2$Tasa, xlab = "Valores teóricos", ylab = "Valores de la tasa de abandono escolar en México", main = NULL, col = "springgreen4")
qqline(datos2$Tasa)
dev.off()

#shapiro.test(datos2$Tasa)

datos3 <- read.table("Prueba3.txt", header = TRUE)
#ks.test(datos3$Primaria, datos3$Superior)
#var.test(datos3$Primaria, datos3$Superior)
pr <- data.frame(datos3$Primaria)
names(pr) = c("Tasa")
pr$nivel <- 'Primaria'
su <- data.frame(datos3$Superior)
names(su) = c("Tasa")
su$nivel <- 'Superior'
datosss <- rbind(pr, su)

p <- ggplot(datosss, aes(Tasa, fill = nivel)) + geom_density(alpha = 0.2)
fig <- ggplotly(p)
#print(fig)

png('datos4.png', width=2000,height=1800,res=300)
boxplot(datos3$Primaria, datos3$Superior, names=c("Primaria","Superior"), col = c("#31B4EE", "#EEB231"), xlab = "Nivel", ylab = "Tasa")
dev.off()

datos4 <- read.table("Prueba4.txt", header = TRUE)
data4 <- as.table(as.matrix(datos4))

png('datos5.png', width=2000,height=1800,res=300)
balloonplot(t(data4), main = NULL, xlab ="", ylab="", label = FALSE, show.margins = FALSE)
dev.off()

#chisq.test(datos4)
#qchisq (0.95, 4)

datos5 <- read.table("Prueba5.txt", header = TRUE)

png('datos6.png', width=2000,height=1800,res=300)
plot(datos5$Hombres,datos5$Mujeres, xlab = "Hombres", ylab = "Mujeres", col = "#750815")
dev.off()

#cor.test(datos5$Hombres,datos5$Mujeres)

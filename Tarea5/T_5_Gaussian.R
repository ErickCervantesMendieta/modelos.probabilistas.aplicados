#Erick Cervantes Mendieta
#Tarea 5
#Modelos Probabilistas Aplicados

if (!require("ggplot2")) {
    install.packages("ggplot2")
}
require(ggplot2)

n = 5000
gaussian = function(mu, sigma) {
    u = runif(2);
    z0 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
    z1 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
    datos = c(z0, z1);
    return (sigma * datos + mu);
}

#Tanto z0 como z1 forman parte del conjunto de números pseudoaleatorios generados
media = 0
desv = 1
aleatorio = numeric()
while(length(aleatorio) < n) {
	aleatorio = c(aleatorio, gaussian(media, desv))
}

png('pnormal.png', width=2000,height=1800,res=300)
hist(aleatorio, main = NULL, freq = FALSE, xlab = "Números obtenidos con el Generador", ylab = "Frecuencia", col = "#FE745B", xlim = c(-4, 4))
curve(dnorm(x,media,desv), add = TRUE, col = "#03380B")
dev.off()

png('qqnormal.png', width=2000,height=1800,res=300)
print(ggqqplot(aleatorio, main = NULL, xlab = "Valores teóricos", ylab = "Valores de los números pseudoaleatorios", col = "#04231F"))
dev.off()


#Separamos z0 y z1, con esto tenemos dos conjuntos de números pseudoaleatorios generados
a1 = numeric()
a2 = numeric()
while(length(a1) < n) {
	a1 = c(a1, gaussian(media, desv)[1])
	a2 = c(a2, gaussian(media, desv)[2])
}

png('pnormalsep.png', width=2000,height=1800,res=300)
par(mfrow = c(1, 2))
hist(a1, main = NULL, freq = FALSE, xlab = "Números obtenidos utilizando z0", ylab = "Frecuencia", col = "#74D0C6")
curve(dnorm(x,media,desv), add = TRUE, col = "#521308")
hist(a2, main = NULL, freq = FALSE, xlab = "Números obtenidos utilizando z1", ylab = "Frecuencia", col = "#79D074")
curve(dnorm(x,media,desv), add = TRUE, col = "#521308")
dev.off()

png('qqnormalsep1.png', width=2000,height=1800,res=300)
print(ggqqplot(a1, main = NULL, xlab = "Valores teóricos", ylab = "Valores de los números pseudoaleatorios con z0", col = "#04231F"))
dev.off()

png('qqnormalsep2.png', width=2000,height=1800,res=300)
print(ggqqplot(a2, main = NULL, xlab = "Valores teóricos", ylab = "Valores de los números pseudoaleatorios con z1", col = "#04231F"))
dev.off()

#shapiro.test(aleatorio)
#shapiro.test(a1)
#shapiro.test(a2)




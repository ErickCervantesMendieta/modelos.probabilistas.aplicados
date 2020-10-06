#Erick Cervantes Mendieta
#Tarea 5
#Modelos Probabilistas Aplicados

if (!require("swfscMisc")) {
    install.packages("swfscMisc")
}
require(swfscMisc)

#Generador Congruencial Mixto
n = 5000
uniforme = function(n, semilla) {
    a = 80
    c = 1000
    m = 115 
    datos = numeric()
    x = semilla
    while (length(datos) < n) {
        x = (a * x + c) %% m
        datos = c(datos, x)
    }
    return(datos / m)
}

png('puniforme.png', width=2000,height=1800,res=300)
par(mfrow = c(1, 2))
hist(uniforme(n,10169), main = NULL, xlab = "Números obtenidos con el Generador", ylab = "Frecuencia", col = "#F14D0A")
hist(runif(n), main = NULL, xlab = "Distribución Uniforme", ylab = "Frecuencia", col = "#F1880A")
dev.off()

#cat(uniforme(20,4))
#cat(uniforme(5000,37))
#uniform.test(hist(uniforme(5000,10169)))
#Z = ((mean(uniforme(5000,37))-0.5)*sqrt(n)) / sqrt(1/12)

#Generador de Wichmann-Hill
uniforme2 = function(n, s1, s2, s3){
	datos2 = numeric()
    	while (length(datos2) < n) {
      	s1 = (171 * s1) %% 30269
    		s2 = (172 * s2) %% 30307
   		s3 = (170 * s3) %% 30323
		u = (s1/30269 + s2/30307 + s3/30323) %% 1	
	      datos2 = c(datos2, u)
    }
    return(datos2)
}

png('WHuniforme.png', width=2000,height=1800,res=300)
par(mfrow = c(1, 2))
hist(uniforme2(n,3,11,10865), main = NULL, xlab = "Generador de Wichmann-Hill", ylab = "Frecuencia", col = "#780822")
hist(runif(n), main = NULL, xlab = "Distribución Uniforme", ylab = "Frecuencia", col = "#C82C1A")
dev.off()

#uniform.test(hist(uniforme2(n,3,11,10865)))



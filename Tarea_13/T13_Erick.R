#Erick Cervantes Mendieta
#Tarea 13
#Modelos Probabilistas Aplicados

n = 1000
cont = numeric()
suma = 0
set.seed(175)
for(i in 1:n){ 
	dado = sample(1:6, 1)
	if(dado == 1){
		suma = suma + 1
	}
	cont = c(cont, suma/i)
}

png('dado.png', width=2000,height=1800,res=300)
plot.ts(cont, xlab = "Número de lanzamientos", ylab = "Frecuencia relativa", col = "#305605")
abline(1/6, 0, col = "red")
dev.off()
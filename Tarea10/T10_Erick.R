#Erick Cervantes Mendieta
#Tarea 10
#Modelos Probabilistas Aplicados

#Ejercicio 1 - pág. 247

n = 100000
suma = 0
gc = numeric()
for(i in 1:n){
	carta = sample(2:10,1)
	if(carta %% 2 == 0){
		suma = suma - 1;
		gc = c(gc, -1)
	}
	else{
		suma = suma + 1;
		gc = c(gc, 1)
	}
}
ganancia = suma / n
message(sprintf("El valor de la ganancia obtenida es: %s\n", ganancia))

png('1_247.png', width=2000,height=1800,res=300)
barplot(table(gc)/n, density = 50, col = heat.colors(2), xlab = "Valores de x", ylab = "P(x)")
dev.off()


#Ejercicio 6 - pág. 247

x1 = numeric()
y1 = numeric()
mult = numeric()
for(i in 1:n){
	dado = sample(1:6, 2, T)
	x1 = c(x1,dado[1] + dado[2])
	y1 = c(y1,dado[1] - dado[2])
	mult = c(mult,(dado[1] + dado[2]) * (dado[1] - dado[2]))
}
message(sprintf("El valor esperado de X es: %s\n", mean(x1)))
message(sprintf("El valor esperado de Y es: %s\n", mean(y1)))
message(sprintf("El valor esperado de XY es: %s\n", mean(mult)))

png('6_247x.png', width=2000,height=1800,res=300)
barplot(table(x1)/n, density = 50, col = terrain.colors(11), xlab = "Valores de x", ylab = "P(x)")
dev.off()
png('6_247y.png', width=2000,height=1800,res=300)
barplot(table(y1)/n, density = 50, col = topo.colors(11), xlab = "Valores de y", ylab = "P(y)")
dev.off()
png('6_247xy.png', width=2000,height=1800,res=300)
barplot(table(mult)/n, density = 50, col = heat.colors(11), las = 2, xlab = "Valores de xy", ylab = "P(xy)")
dev.off()


#Ejercicio 15 - pág. 249

ganancia2 = 0
cont = numeric()
bolas <- c(rep(1, 2), rep(-1, 3))
for(i in 1:n){
	juego <- sample(bolas, 5, F)
	if(juego[1] == 1 || (juego[2] == 1 && juego[3] == 1)){
		ganancia2 = ganancia2 + 1;
		cont = c(cont,1)
	}
	else if((juego[1] == -1 && juego[2] == 1 && juego[3] == -1 && juego[4] == 1) || (juego[3] == 1 && juego[4] == 1)){
		#ganancia2 = ganancia2;
		cont = c(cont,0)
	}
	else{
		ganancia2 = ganancia2 - 1;
		cont = c(cont,-1)
	}
}
ganancia2 = ganancia2 / n
message(sprintf("El valor de la ganancia obtenida es: %s\n", ganancia2))

png('15_249.png', width=2000,height=1800,res=300)
barplot(table(cont)/n, density = 50, col = rainbow(3), xlab = "Valores de x", ylab = "P(x)")
dev.off()


#Ejercicio 1 - pág. 263

num = numeric()
for(i in 1:n){
	num = c(num, sample(-1:1,1))
}
message(sprintf("El valor de la media: %s\n", mean(num)))
message(sprintf("El valor de la varianza: %s\n", var(num)))
message(sprintf("El valor de la desviación estándar es: %s\n", sd(num)))
#print(table(num)/n)
png('1_263.png', width=2000,height=1800,res=300)
barplot(table(num)/n, density = 50, col=1:dim(table(num)), xlab = "Valores de x", ylab = "P(x)")
dev.off()


#Ejercicio 9 - pág. 264

dado_k = numeric()
for(i in 1:n){
	dado_k = c(dado_k, sample(1:6, 1, prob = c(1/21, 2/21, 3/21, 4/21, 5/21, 6/21)))
}
message(sprintf("El valor de la media: %s\n", mean(dado_k)))
message(sprintf("El valor de la varianza: %s\n", var(dado_k)))
message(sprintf("El valor de la desviación estándar es: %s\n", sd(dado_k)))

png('9_264.png', width=2000,height=1800,res=300)
barplot(table(dado_k)/n, density = 50, col = rainbow(6), xlab = "Valores de x", ylab = "P(x)")
dev.off()


#Ejercicio 12 - pág. 280

a = 0
b = 1
x = runif(n, a, b)
y = runif(n, a, b)
x2 = x ^ y
v_x = round(x2,1)
table(v_x)

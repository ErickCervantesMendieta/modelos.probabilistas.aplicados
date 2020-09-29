me = 1
lamda = 6
mu = exp(-lamda)
cu = numeric()
ce = numeric()
cn = numeric()
repl = 10000
br = 5

for (replica in 1:repl) {
    	du = c(1)
    	de = numeric()
	dn = numeric()
 	while (prod(du) > mu) {
        	du = c(du, runif(1))
    	}
    	while (sum(de) < me) {
        	de = c(de, rexp(1, lamda))
    	}
	while (sum(dn) < me) {
		dn = c(dn, rnorm(1, lamda, sqrt(lamda)))
	}
    	cu = c(cu, length(du))
    	ce = c(ce, length(de))
	cn = c(cn, length(dn))
}

png('forma.png', width=2000,height=1800,res=300)
par(mfrow = c(1, 3))
hist(cu, main = NULL, xlab = "Generador distribución Uniforme", ylab = "Frecuencia")
hist(ce, main = NULL, xlab = "Generador distribución Exponencial", ylab = "Frecuencia")
hist(rpois(repl, lamda), main = NULL, xlab = "Gráfica distribución Poisson", ylab = "Frecuencia")
dev.off()

png('formanormal.png', width=2000,height=1800,res=300)
par(mfrow = c(1, 2))
hist(cn, main = NULL, xlab = "Generador distribución Normal", ylab = "Frecuencia")
hist(rpois(repl, lamda), main = NULL, xlab = "Gráfica distribución Poisson", ylab = "Frecuencia")
dev.off()


#Aproximaciones
lambda = 7
X_u = numeric()
X_e = numeric()
repl = 1000
#br = 5

for(i in 1:repl){
	k = 0
	j = 0
	U = runif(1)
	Y_u = exp(-lambda)
	Y_e = -(1/lambda)*log(U)
	sum = Y_e
	while(U >= Y_u){
		k = k+1
		Y_u = Y_u + exp(-lambda)*lambda^k/gamma(k+1);
	}
	X_u = c(X_u,k)
	while(sum < 1){
		U = runif(1)
		Y_e = -(1/lambda)*log(U)
		sum = sum + Y_e 
		j = j + 1
	}
	X_e = c(X_e, j)
}

png('aprox.png', width=2000,height=1800,res=300)
par(mfrow = c(1, 3))
hist(X_u, main = NULL, xlab = "Aproximación distribución Uniforme", ylab = "Frecuencia", breaks = br)
hist(X_e, main = NULL, xlab = "Aproximación distribución Exponencial", ylab = "Frecuencia", breaks = br)
hist(rpois(repl, lambda), main = NULL, xlab = "Gráfica distribución Poisson", ylab = "Frecuencia", breaks = br)
dev.off()
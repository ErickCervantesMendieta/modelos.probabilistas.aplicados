#Erick Cervantes Mendieta
#Tarea 9
#Modelos Probabilistas Aplicados


n = 100
a = 0
b = 1
x = numeric()

set.seed(2020)
x1 = runif(n, a, b)
x2 = runif(n, a, b)
x3 = x1 ^ x2
x = round(x3,1)
table(x)




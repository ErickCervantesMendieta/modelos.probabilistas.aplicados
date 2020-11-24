#Erick Cervantes Mendieta
#Tarea 12
#Modelos Probabilistas Aplicados

#Ejercicio 1 - pág. 392

z = seq(0, 1.5, length = 100)
png('1_392.png', width=2000,height=1800,res=300)
plot(z,z-0.5+(0.5*z/(z-2)),type="l",col="blue",lwd=3, panel.first=grid(), xlab = "z")
points(1,0, col = "red")
dev.off()

z = seq(0, 1, length = 100)
png('1_392e.png', width=2000,height=1800,res=300)
plot(z,z - (1/3)+((2/3)*z / (2*z-3)),type="l",col="blue",lwd=3, panel.first=grid(), xlab = "z")
points(1/2,0, col = "red")
points(1,0, col = "red")
dev.off()

z = seq(0, 1, length = 100)
png('1_392f.png', width=2000,height=1800,res=300)
plot(z,z - exp(2*z-2),type="l",col="blue",lwd=3, panel.first=grid(), xlab = "z")
points(0.2,0, col = "red")
points(1,0, col = "red")
dev.off()



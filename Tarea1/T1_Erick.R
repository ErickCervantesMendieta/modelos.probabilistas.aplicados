#Erick Cervantes Mendieta
#Tarea 1
#Modelos Probabilistas Aplicados

datos <- read.table("Educacion.dat", header = TRUE)
print(datos)
print(summary(datos))
png('T1.png',width=2000,height=1600,res=300)
par(bg="#F1F3F6")
colCajas <- c("#F14D0A","#CEDA0A","#F1880A","#E7CD3A")
boxplot(datos,xlab="Nivel educativo",ylab="Proporción de la matrícula",main="Tasa neta de matriculación\nCiclo Escolar 2018-2019
",col=colCajas)
dev.off()

#Erick Cervantes Mendieta
#Tarea 11
#Modelos Probabilistas Aplicados

datos <- read.table("datos_Chi.txt", header = TRUE)
print(chisq.test(datos))

png('mosaico.png', width=2000,height=1800,res=300)
mosaicplot(datos, color=TRUE, main = NULL)
dev.off()

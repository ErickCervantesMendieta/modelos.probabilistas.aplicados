#Erick Cervantes Mendieta
#Tarea 13
#Modelos Probabilistas Aplicados

datos <- read.table("melate.txt", header = TRUE)

p1 <- table(datos$R1)
p2 <- table(datos$R2)
p3 <- table(datos$R3)
p4 <- table(datos$R4)
p5 <- table(datos$R5)
p6 <- table(datos$R6)
p7 <- table(datos$R7)

(p1)/margin.table(p1)
(p2)/margin.table(p2)
(p3)/margin.table(p3)
(p4)/margin.table(p4)
(p5)/margin.table(p5)
(p6)/margin.table(p6)
(p7)/margin.table(p7)
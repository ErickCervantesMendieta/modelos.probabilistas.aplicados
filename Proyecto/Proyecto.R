#Erick Cervantes Mendieta
#Proyecto
#Modelos Probabilistas Aplicados

peso <- c(18, 18,18,19,18,20,20,19,19,18)
harina <- c(rep(c("almendras", "integral"), c(5,5)))
endulzante <- rep(c("stevia", "glass", "splenda", "a_morena", "panela"), each = 1, times = 2)
datos <- data.frame(harina = harina, endulzante = as.factor(endulzante), peso =peso)

anova <- aov(peso ~ harina * endulzante, data = datos)
summary(anova)






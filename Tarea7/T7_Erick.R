#Erick Cervantes Mendieta
#Tarea 7
#Modelos Probabilistas Aplicados

if (!require("dplyr")) {
    install.packages("dplyr")
}
require(dplyr)

if (!require("trafo")) {
    install.packages("trafo")
}
require(trafo)

if (!require("ggplot2")) {
    install.packages("ggplot2")
}
require(ggplot2)

if (!require("GGally")) {
    install.packages("GGally")
}
require(GGally)

n = 100
a = 0
b = 1
lamda = 6
media = 0
desv = 1
b1 = 5
b2 = -2

set.seed(2020)
x1 = runif(n, a, b)
x2 = runif(n, 10, 35)
x3 = rnorm(n, media, desv)

y1 = - b1 * x1 + b2
y2 = 2 * x2 ^ 3 + rnorm(n)
y3 = (3 / x2) - (6 * x3^2)

datos1 <- data.frame(x1, y1)
png('f1.png', width=2000,height=1800,res=300)
scatter.smooth(x = datos1$x1, y = datos1$y1, main = NULL, xlab = "x1", ylab = "y1", lpars = list(col = "red", lwd = 1.5, lty = 3), col = "#0A3D68")
dev.off()
linMod1 <- lm(y1 ~ x1)
#summary(linMod1)
#assumptions(linMod1)

datos2 <- data.frame(x2,y2)
png('f2.png', width=2000,height=1800,res=300)
scatter.smooth(x = datos2$x2, y = datos2$y2, main = NULL, xlab = "x2", ylab = "y2", lpars = list(col = "red", lwd = 1.5, lty = 3), col = "#0A3D68")
dev.off()
linMod2 <- lm(y2 ~ x2)
#summary(linMod2)
#assumptions(linMod2)

datos2_fit_reciprocal = reciprocal(linMod2)
png('f3.png', width=2000,height=1800,res=300)
scatter.smooth(x = datos2$x2, y = datos2_fit_reciprocal$yt, main = NULL, xlab = "x2", ylab = "1 / y2", lpars = list(col = "red", lwd = 1.5, lty = 3), col = "#0A3D68")
dev.off()
linMod2_re <- lm(datos2_fit_reciprocal$yt ~ x2)
#summary(linMod2_re)

datos2_fit_log = lm(log(y2) ~ x2)
png('f4.png', width=2000,height=1800,res=300)
scatter.smooth(x = datos2$x2, y = log(datos2$y2), main = NULL, xlab = "x2", ylab = "log(y2)", lpars = list(col = "red", lwd = 1.5, lty = 3), col = "#0A3D68")
dev.off()
#summary(datos2_fit_log)

datos2_fit_box = boxcox(linMod2, plotit = FALSE)
png('f5.png', width=2000,height=1800,res=300)
scatter.smooth(x = datos2$x2, y = datos2_fit_box$yt, main = NULL, xlab = "x2", ylab = "y2 transformado", lpars = list(col = "red", lwd = 1.5, lty = 3), col = "#0A3D68")
dev.off()
linMod2_box <- lm(datos2_fit_box$yt ~ x2)
#summary(linMod2_box)
#datos2_fit_box$lambdahat

datos3 <- data.frame(y3, x2, x3)
png('f6.png', width=2000,height=1800,res=300)
pairs(x = datos3, lower.panel = NULL)
dev.off()
png('f7.png', width=2000,height=1800,res=300)
print(ggpairs(datos3))
dev.off()

modelo_mul <- lm(y3 ~ x2 + x3)
#summary(modelo_mul)

modelo_mul_t <- lm(y3 ~ log(x2) + log(x3))
#summary(modelo_mul_t)
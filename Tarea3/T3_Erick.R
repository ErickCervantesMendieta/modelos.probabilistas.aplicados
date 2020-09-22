#Erick Cervantes Mendieta
#Tarea 3
#Modelos Probabilistas Aplicados

require(gutenbergr)
require(tidytext)
require(tidyr)
require(dplyr)
require(tm)
require(ngram)
require(ggraph)
require(igraph)
require(tidygraph)
require(tokenizers)

libro <- gutenberg_download(c(21004))

#letras
letras = libro %>% unnest_tokens(chars,text,"characters")
freq_letras = as.data.frame(sort(table(letras$chars), decreasing = T))
names(freq_letras) = c('Letra', 'Frecuencia')
freq_e = numeric()
cont = 0
for(i in 1:length(letras$chars)){
	cont = cont + 1
	if(letras$chars[i] == "e"){
		freq_e = c(freq_e,cont)		
		cont = 0
	}
}
p = freq_letras$Frecuencia[1]/length(letras$chars)
n = length(letras$chars)

#Gráfica datos del libro
png('T3_e_datos.png',width=2000,height=1600,res=200)
plot(table(freq_e), col = "#F14D0A", main = NULL, xlab = "Letra e", ylab = "Frecuencia")
dev.off()

#Gráfica simulando una Geométrica
png('T3_e_rgeo.png',width=2000,height=1600,res=200)
set.seed(1234)
plot(table(rgeom(n,p)), col = "#588714", main = NULL, xlab = "Letra e (simulando con rgeom)", ylab = "Frecuencia")
dev.off()

#Gráfica simulando una Binomial
png('T3_e_rbinom.png',width=2000,height=1600,res=200)
resultados = numeric() 
set.seed(10)
for(i in n){
	aleatorio = round(runif(1, min(freq_e), max(freq_e)),0)
	resultados = c(resultados, rbinom(n,aleatorio,p))
}
plot(table(resultados), col = "#F662AE", main = NULL, xlab = "Letra e (simulando con rbinom)", ylab = "Frecuencia")
dev.off()

#Gráfica simulando una Binomial Negativa
png('T3_e_rnbinom.png',width=2000,height=1600,res=200)
resultados2 = numeric() 
set.seed(30)
for(i in n){
	aleatorio = round(runif(1, min(freq_e), max(freq_e)),0)
	resultados2 = c(resultados2, rnbinom(n,aleatorio,p))
}
plot(table(resultados2), col = "#875314", main = NULL, xlab = "Letra e (simulando con rnbinom)", ylab = "Frecuencia")
dev.off()

png('T3_e_todas.png',width=2000,height=1600,res=200)
par(mfrow=c(2,2))
plot(table(freq_e), col = "#F14D0A", main = NULL, xlab = "(a) Letra e", ylab = "Frecuencia")
plot(table(rgeom(n,p)), col = "#588714", main = NULL, xlab = "(b) Letra e (simulando con rgeom)", ylab = "Frecuencia")
plot(table(resultados), col = "#F662AE", main = NULL, xlab = "(c) Letra e (simulando con rbinom)", ylab = "Frecuencia")
plot(table(resultados2), col = "#875314", main = NULL, xlab = "(d) Letra e (simulando con rnbinom)", ylab = "Frecuencia")
dev.off()

#Palabras
palabras = libro %>% unnest_tokens(word, text, "words")
freq_palabras = as.data.frame(sort(table(palabras$word), decreasing = T))
names(freq_palabras) = c('Palabra', 'Frecuencia')
freq_singing = numeric()
cont = 0
for(i in 1:length(palabras$word)){
	cont = cont + 1
	if(palabras$word[i] == "singing"){
		freq_singing = c(freq_singing,cont)		
		cont = 0
	}
}
p_palabra = freq_palabras$Frecuencia[1]/length(palabras$word)
n_palabra = length(palabras$word)

#Gráfica datos del libro
png('T3_singing_datos.png',width=2000,height=1600,res=200)
plot(table(freq_singing), col = "#62F6AC", main = NULL, xlab = "Palabra singing", ylab = "Frecuencia")
dev.off()

#Gráfica simulando una Binomial
png('T3_singing_rbinom.png',width=2000,height=1600,res=200)
resultados2 = numeric() 
set.seed(2020)
for(i in n_palabra){
	aleatorio = round(runif(1, min(freq_e), max(freq_e)),0)
	resultados3 = c(resultados2, rbinom(n_palabra,aleatorio,p_palabra))
}
plot(table(resultados3), col = "#B062F6", main = NULL, xlab = "Palabra singing (simulando con rbinom)", ylab = "Frecuencia")
dev.off()

png('T3_singing_todas.png',width=2000,height=1600,res=200)
par(mfrow=c(1,2))
plot(table(freq_singing), col = "#62F6AC", main = NULL, xlab = "(a) Palabra singing", ylab = "Frecuencia")
plot(table(resultados3), col = "#B062F6", main = NULL, xlab = "(b) Palabra singing (simulando con rbinom)", ylab = "Frecuencia")
dev.off()

#ngrama (n = 3)
grama = libro %>% unnest_tokens(ngram, text, "ngrams", n = 3)
grama %>% count(ngram, sort = T)
#grama <- grama %>% separate(ngram, into = c("uno", "dos", "tres"), sep = " ") %>% filter(!uno %in% stopwords(kind = "en")) %>% filter(!dos %in% stopwords(kind = "en")) %>% filter(!tres %in% stopwords(kind = "en")) %>% count(uno, dos, tres)
freq_ngram = numeric()
cont = 0
for(i in 1:length(grama$ngram)){
	cont = cont + 1
	if(grama$ngram[i] == "the singing mouse"){
		freq_ngram = c(freq_ngram,cont)		
		cont = 0
	}
}

#Gráfica datos del libro
png('T3_ngram.png',width=2000,height=1600,res=200)
plot(table(freq_ngram), col = "purple", main = NULL, xlab = "Frase: the singing mouse", ylab = "Frecuencia")
dev.off()

#Longitud de los ngramas
largo_ngram = numeric()
for(i in 1:length(grama$ngram)){
	largo_ngram = c(largo_ngram, nchar(grama$ngram[i]))
}

#Gráfica de la longitud de los ngramas
png('T3_largongram.png',width=2000,height=1600,res=200)
plot(table(largo_ngram), col = "#1C33D9", main = NULL, xlab = "Largo de los 3gramas", ylab = "Frecuencia")
dev.off()





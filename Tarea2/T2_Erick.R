#Erick Cervantes Mendieta
#Tarea 2
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

libro <- gutenberg_download(c(21004))

#letras

letras = libro %>% unnest_tokens(chars,text,"characters")
freq_letras = as.data.frame(sort(table(letras$chars), decreasing = T))
names(freq_letras) = c('Letra', 'Frecuencia')
freq_letras
freq_oletras = freq_letras[freq_letras$Frecuencia > 30,]
png('T2_letras.png',width=2000,height=1600,res=200)
colBarras <- c("#F14D0A","#CEDA0A","#F1880A","#E7CD3A")
barplot(freq_oletras$Frecuencia, names.arg = freq_oletras$Letra, xlab = "Letras", ylab = "Frecuencia", col = colBarras)
dev.off()

#palabras

palabras = libro %>% unnest_tokens(word, text, "words")
freq_palabras = as.data.frame(sort(table(palabras$word), decreasing = T))
names(freq_palabras) = c('Palabra', 'Frecuencia')
freq_palabras
freq_opalabras = freq_palabras[freq_palabras$Frecuencia > 100,]
png('T2_palabras.png',width=1800,height=1400,res=250)
colBarras <- c("#F14D0A","#CEDA0A","#F1880A","#E7CD3A")
barplot(freq_opalabras$Frecuencia, names.arg = freq_opalabras$Palabra, xlab = "Frecuencia", col = colBarras, horiz = 1, las = 1)
dev.off()

#ngrama (n = 2)

bigrama = libro %>% unnest_tokens(bigram, text, "ngrams", n = 2)
bigrama %>% count(bigram, sort = T)
bigrama <- bigrama %>% separate(bigram, into = c("uno", "dos"), sep = " ") %>% filter(!uno %in% stopwords(kind = "en")) %>% filter(!dos %in% stopwords(kind = "en")) %>% count(uno, dos)
png('T2_bigrama.png',width=1800,height=1400,res=200)
set.seed(75)
g_bigrama = bigrama %>% filter(n > 3) %>% graph_from_data_frame() %>% ggraph(layout = 'kk') + geom_edge_link(arrow = arrow(type = "closed",length = unit(.075, "inches"))) + geom_node_point() + geom_node_text(aes(label = name), vjust = 1, hjust = 1) + theme_void()
print(g_bigrama)
dev.off()
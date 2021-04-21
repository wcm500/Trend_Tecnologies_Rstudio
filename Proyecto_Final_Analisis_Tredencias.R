library(readtext)
library(tm)
library(stringr)
library(ggplot2)
library(syuzhet)
library(ggrepel)
library(dplyr)
library("wordcloud")
#Crear la corpora con los PDFs 
#Integrantes
#William Courrau Martinez
#Michael Monge
#Adrian Campos Artavia
#________________________________________________________________________________
#Analisis con corpus 
rtext <- readtext("D:/Mineria de datos/PROYECTO FINAL/Trend_Tecnologies_Rstudio/PDF Technology")
#Agregar un corpus
rtext_corpus <- VCorpus(VectorSource(rtext$text))
#minuscula
rtext_corpus <- tm_map(rtext_corpus, content_transformer(tolower))
#Eliminar numeros
rtext_corpus <- tm_map(rtext_corpus, removeNumbers) 
#Agregar mis propias  stopwords
myStopWords<- c("will","use","can","technolog","new")
#Eliminar stopwords
rtext_corpus <- tm_map(rtext_corpus, removeWords, c(stopwords("english"),"technolog")) 

rtext_corpus <- tm_map(rtext_corpus, removeWords, c(stopwords("english"),"will")) 

rtext_corpus <- tm_map(rtext_corpus, removeWords, c(stopwords("english"),"can")) 

rtext_corpus <- tm_map(rtext_corpus, removeWords, c(stopwords("english"),"new")) 

#Eliminar signos de puntuacion
rtext_corpus <- tm_map(rtext_corpus, removePunctuation)
#Eliminar espacios en blanco
rtext_corpus <- tm_map(rtext_corpus, stemDocument)

rtext_corpus <- tm_map(rtext_corpus, stripWhitespace) 
#Tokenizar
rtext_data_dtm <- DocumentTermMatrix(rtext_corpus)

rtext_data_dtm <- removeSparseTerms(rtext_data_dtm,0.01) # 1% de uso menor eliminado
rtext_data_dtm

freq <- sort(colSums(as.matrix(rtext_data_dtm)), decreasing=TRUE)  # Graficar la frecuencia de la palabras
freq

wfd <- data.frame(word=names(freq), freq=freq) # pasar a dataframe
head(wfd)


#Grafica de palabras frecuentes
ggplot(subset(wfd, freq>30), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(x = "Palabra", 
       y = "Frecuencia de las palabras", 
       title = "Palabras mas repetidas")

#Grafico de nube palabras
wordcloud(rtext_corpus, 
          max.words=150, scale=c(3,.9),random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(6, "BrBG"))



#___________________________________________________________________________________________________________

#install.packages("readtext")
library(readtext)
library(tm)
library(stringr)
library(ggplot2)
#install.packages("ggrepel")
#install.packages("syuzhet")
library(syuzhet)
library(ggrepel)
library(dplyr)
library("wordcloud")
#Crear la corpora con los PDFs 

#________________________________________________________________________________
#Analisis con corpus 
rtext <- readtext("D:/Mineria de datos/PROYECTO FINAL/Trend_Tecnologies_Rstudio/PDF Technology")

rtext_corpus <- VCorpus(VectorSource(rtext$text))

rtext_corpus <- tm_map(rtext_corpus, content_transformer(tolower))

rtext_corpus <- tm_map(rtext_corpus, removeNumbers) 

myStopWords<- c("will","use","can","technolog","new")

rtext_corpus <- tm_map(rtext_corpus, removeWords, c(stopwords("english"),myStopWords)) 

rtext_corpus <- tm_map(rtext_corpus, removePunctuation)

rtext_corpus <- tm_map(rtext_corpus, stemDocument)

rtext_corpus <- tm_map(rtext_corpus, stripWhitespace) 

rtext_data_dtm <- DocumentTermMatrix(rtext_corpus)

rtext_data_dtm <- removeSparseTerms(rtext_data_dtm,0.01) # 1% de uso menor eliminado
rtext_data_dtm

freq <- sort(colSums(as.matrix(rtext_data_dtm)), decreasing=TRUE)  
freq

wfd <- data.frame(word=names(freq), freq=freq)
head(wf)

ggplot(subset(wfd, freq>30), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=freq), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle=-90, hjust=0))+
  theme_minimal()+
  labs(x = "Palabra", 
       y = "Frecuencia de las palabras", 
       title = "Palabras mas repetidas")

wordcloud(rtext_corpus, 
          max.words=150, scale=c(3,.9),random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(6, "BrBG"))



#___________________________________________________________________________________________________________

rtext <- readtext("D:/Mineria de datos/PROYECTO FINAL/Trend_Tecnologies_Rstudio/PDF Technology")%>%
  tolower() %>%
  removeWords(stopwords("english")) %>% 
  strsplit(" ") %>%
  unlist() %>% #producir un vector 
  stemDocument()%>%
  removeNumbers()%>%
  stripWhitespace()%>%
  removePunctuation()%>%
  str_replace_all("[^[:alpha:][:space:]]*", "") 
# eliminar palabras determinadas por cuenta propia.


#Eliminar los pescios en blanco & stopwords
myStopwords <- c(stopwords('english'),"will","use","can","technolog","new")# to add words
rtext = rtext[!rtext %in% c("",myStopwords)]

#crear elcorpus despues de la limpieza de texto
rtext_corpus <- VCorpus(VectorSource(rtext))
#rtext_corpus_clean <- tm_map(rtext_corpus, removeWords,myStopwords ) 
#rtext_data_dtm <- DocumentTermMatrix(rtext_corpus_clean)
#comentarios_palabras <- rtext_data_dtm$dimnames

#Tabla para crear la frecuencia de palabras
wordfreqs <- rtext %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))
colnames(wordfreqs) <- c("Palabras", "Frecuencia")
head(wordfreqs)

#Las 15 palabras mas frecuentes y sus frecuencias para graficar del 1 al 15
wfd <- table(rtext)
wfd <- wfd[order(wfd, decreasing = T)]
wfd <- wfd[1:15]
View(wfd)
#Pasar a dataframe para generar visualizaciones con GGPLOT
wfd <- as.data.frame(wfd)

#Grafico en barras para ver las palabras mas comun
ggplot(data=wfd, aes(x=rtext, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(x = "Palabra", 
       y = "Frecuencia de las palabras", 
       title = "Palabras mas repetidas")

# create wordcloud
wordcloud(words = wordfreqs$Palabras, freq = wordfreqs$Frecuencia, 
          max.words=150, scale=c(3,.9),random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(6, "BrBG"))



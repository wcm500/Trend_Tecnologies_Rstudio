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
rtext_corpus_clean <- tm_map(rtext_corpus, removeWords,myStopwords ) 
rtext_data_dtm <- DocumentTermMatrix(rtext_corpus_clean)
comentarios_palabras <- rtext_data_dtm$dimnames



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

#Frecuencia relativa (Falta)
ggplot(wfd, aes(x=rtext, y=Freq, group =1)) + 
  geom_smooth(aes(y = Freq, x = rtext), color = "goldenrod2")+
  geom_line(aes(y = Freq, x = rtext), color = "indianred4") +         
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(name ="Relative Frequency (per 1,000 words)")

#https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html?

#https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/

#https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/advancing-text-mining/
  
syuzhet_vector <- get_sentiment(rtext, method="syuzhet")
summary(syuzhet_vector)
# Find associations 
rtext <- Corpus(VectorSource(rtext))
rtext_dtm <- TermDocumentMatrix(rtext)
rtext <- as.matrix(rtext_dtm)
findAssocs(rtext_dtm, terms = findFreqTerms(rtext_dtm, lowfreq = 5), corlimit = 0.25)

d<-get_nrc_sentiment(rtext)
head(d,10)
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:30000]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Sentimientos dentro los textos")



#________________________________________________________________________________
#Analisis con corpus 
rtext <- readLines(file.choose())
rtext_corpus <- VCorpus(VectorSource(rtext))
rtext_corpus_clean <- tm_map(rtext_corpus_clean,content_transformer(tolower))
rtext_corpus_clean <- tm_map(rtext_corpus_clean, 
                                     removeNumbers) 
rtext_corpus_clean <- tm_map(rtext_corpus_clean, removeWords, c(stopwords("english"),"will","can")) 
rtext_corpus_clean <- tm_map(rtext_corpus_clean, 
                                     removePunctuation)
rtext_corpus_clean <- tm_map(rtext_corpus_clean, 
                                     stemDocument)
rtext_corpus_clean <- tm_map(rtext_corpus_clean, 
                                     stripWhitespace) 

rtext_data_dtm <- DocumentTermMatrix(rtext_corpus)

comentarios_palabras <- rtext_data_dtm$dimnames

wordfreqs <- rtext_data_dtm$dimnames$Terms %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))
wordfreqs <- rtext %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))

colnames(wordfreqs) <- c("Palabras", "Frecuencia")
head(wordfreqs)

#Las 15 palabras mas frecuentes y sus frecuencias para graficar
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

matriz <- as.matrix(rtext_data_dtm)
matrizOrdenada <- sort(rowSums(matriz),decreasing=TRUE)
dataframePalabras <- data.frame(palabra = comentarios_palabras$Terms[matrizOrdenada],freq=matrizOrdenada)
head(dataframePalabras, 10)

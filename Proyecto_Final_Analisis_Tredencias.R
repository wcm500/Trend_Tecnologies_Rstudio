#install.packages("readtext")
library(readtext)
library(tm)
library(stringr)
library(ggplot2)
#install.packages("ggrepel")
install.packages("syuzhet")
library(syuzhet)
library(ggrepel)
library(dplyr)
library("wordcloud")
#Crear la corpora con los PDFs 
rtext <- readtext("D:/Mineria de datos/PROYECTO FINAL/PDF Technology")%>%
  tolower() %>%
  removeWords(stopwords("english")) %>% 
  strsplit(" ") %>%
  unlist() %>%
  stemDocument()%>%
  removeNumbers()%>%
  stripWhitespace()%>%
  removePunctuation()%>%
  str_replace_all("[^[:alpha:][:space:]]*", "")
View(rtext)
#Eliminar los pescios en blanco
rtext = rtext[!rtext %in% c("")]

#Tabla para crear las plabras con frecuencias
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
#rtext <- as.matrix(rtext_dtm)
findAssocs(rtext_dtm, terms = findFreqTerms(rtext_dtm, lowfreq = 5), corlimit = 0.25)

#d<-get_nrc_sentiment(rtext)
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








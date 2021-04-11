#install.packages("readtext")
library(readtext)
library(tm)
library(stringr)
library(ggplot2)
#install.packages("ggrepel")
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

rtext = rtext[!rtext %in% c("")]



trimws(rtext)

wordfreqs <- rtext %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))
colnames(wordfreqs) <- c("Word", "Frequency")

head(wordfreqs)

wfd <- table(rtext)
wfd <- wfd[order(wfd, decreasing = T)]
wfd <- wfd[1:15]
View(wfd)
#Pasar a dataframe para generar visualizaciones con GGPLOT
wfd <- as.data.frame(wfd)

# start plot
#barplot(wfd, las = 1, ylim = c(0,2000), las=2)
#text(seq(0.7, 11.5, 1.2), wfd+150, wfd)

ggplot(data=wfd, aes(x=rtext, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(x = "Palabra", 
       y = "Frecuencia de las palabras", 
       title = "Palabras mas repetidas")

# create wordcloud
wordcloud(words = wordfreqs$Word, freq = wordfreqs$Frequency, 
          max.words=50, random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(6, "BrBG"))


rtext<-Corpus(VectorSource(rtext[["text"]]))
#Aplicar el Text Mining y el NPL
"Uso de minuscula"
rtext_clean <- tm_map(rtext,content_transformer(tolower))
"Eliminar Numeros"
rtext_clean <- tm_map(rtext_clean,removeNumbers) 
"Eliminar StopWords"
rtext_clean <- tm_map(rtext_clean,removeWords,stopwords()) 
"Eliminar signos de puntuacion"
rtext_clean <- tm_map(rtext_clean, removePunctuation)
"palabraas a la raiz"
rtext_clean <- tm_map(rtext_clean,stemDocument)
"Espacios en blanco"
rtext_clean <- tm_map(rtext_clean,stripWhitespace) 
"Tokenizar"
rtext_dtm <- DocumentTermMatrix(rtext_clean)

rtext_dtm <- DocumentTermMatrix(rtext, 
                               control = list(
                                 tolower = TRUE,
                                 removeNumbers = TRUE,
                                 stopwords = TRUE,
                                 removePunctuation = TRUE,
                                 stemming = TRUE
                               ))
rtext_dtm.df<-as.data.frame(as.matrix(rtext_dtm))

# extract number of words per chapter
library(dplyr)
words <- sapply(rtext_dtm, function(x) length(x))
# inspect data
words





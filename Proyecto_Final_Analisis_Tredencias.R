install.packages("readtext")
library(readtext)
library(tm)
#Crear la corpora con los PDFs 
rtext <- readtext::readtext("D:/Mineria de datos/PROYECTO FINAL/PDF Technology")
Corpus(VectorSource(rtext[["text"]]))
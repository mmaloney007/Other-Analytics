install.packages(c("NLP", "openNLP", "RWeka", "qdap", "tm", "wordcloud", "Rstem", "coreNLP"))

install.packages("SnowballC")

library("NLP")
library("openNLP")
library("RWeka")
library("qdap")
library("tm")
library("wordcloud")
#library(Rstem)
library("SnowballC")
library("coreNLP")

downloadCoreNLP(type="base")

initCoreNLP()

# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# and extract to your program files folder

# here is a pdf for mining
url <- "http://www.noisyroom.net/blog/RomneySpeech072912.pdf"
dest <- tempfile(fileext = ".pdf")
download.file(url, dest, mode = "wb")

# set path to pdftotxt.exe and convert pdf to text
exe <- "/usr/local/bin/pdftotext"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)
#shell.exec(filetxt)

filetxt

# do something with it, i.e. a simple word cloud 
library(tm)
library(wordcloud)
library(SnowballC)

txt <- readLines(filetxt) # don't mind warning..

txt <- tolower(txt)
txt <- removeWords(txt, c("\\f", stopwords()))

corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
d$stem <- wordStem(row.names(d), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# remove web address (very long string):
d <- d[nchar(row.names(d)) < 20, ]

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
wordcloud(d$word, d$freq)

## COMMENTING OUT
##library(coreNLP)
##initCoreNLP()

##output = annotateString(txt)

##getToken(output)[,c(1:3,6:7)]

##getDependency(output)

##getSentiment(output)

# remove files
##file.remove(dir(tempdir(), full.name=T)) # remove files

library(NLP)
library(openNLP)
library(magrittr)

## Create annotators

#install.packages("openNLPmodels.en",,"http://datacube.wu.ac.at/",type="source")

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
date_ann <-  Maxent_Entity_Annotator(kind = "date")
money_ann <- Maxent_Entity_Annotator(kind = "money")
percentage_ann <- Maxent_Entity_Annotator(kind = "percentage")
#misc_ann <- Maxent_Entity_Annotator(kind = "misc")

## "date", "location", "money", "organization", "percentage", "person", "misc"

#covert text to string
text <- as.String(txt)

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)

txt_annotations <- annotate(text, pipeline)

#txt_annotations <- annotate(text)

class(txt_annotations)

#First part of annotation

head(txt_annotations)

##We see that the annotation object contains a list of sentences (and also words) identified by position. That is, the first sentence in the document begins at character 1 and ends at character 111. The sentences also contain information about the positions of the words that comprise them.
##We can combine the txtgraphy and the annotations to create what the NLP package calls an AnnotatedPlainTextDocument. If we wishd we could also associate metadata with the object using the meta = argument.

txt_doc <- AnnotatedPlainTextDocument(text, txt_annotations)

#Now we can extract information from our document using accessor functions like sents() to get the sentences and words() to get the words. We could get just the plain text with as.character(txt_doc).

sents(txt_doc) %>% head(2)

# same with words
words(txt_doc) %>% head(10)

#Among the several kinds of annotators provided by the openNLP package is an entity annotator. An entity is basically a proper noun, such as a person or place name. Using a technique called named entity recognition (NER), we can extract various kinds of names from a document. In English, OpenNLP can find dates, locations, money, organizations, percentages, people, and times. (Acceptable values are "date", "location", "money", "organization", "percentage", "person", "misc".) We will use it to find people, places, and organizations since all three are mentioned in our sample paragraph.  These kinds of annotator functions are created using the same kinds of constructor functions that we used for word_ann() and sent_ann().

#person_ann <- Maxent_Entity_Annotator(kind = "person")
#location_ann <- Maxent_Entity_Annotator(kind = "location")
#organization_ann <- Maxent_Entity_Annotator(kind = "organization")

#Recall that we earlier passed a list of annotator functions to the annotate() function to indicate which kinds of annotations we wanted to make. We will create a new pipeline list to hold our annotators in the order we want to apply them, then apply it to the txt variable. Then, as before, we can create an AnnotatedPlainTextDocument.


#As before we could extract words and sentences using the getter methods words() and sents(). Unfortunately there is no comparably easy way to extract names entities from documents. But the function below will do the trick.
# Extract entities from an AnnotatedPlainTextDocument

entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

#Now we can extract all of the named entities using entities(txt_doc), and specific kinds of entities using the kind = argument. Let’s get all the people, places, and organizations.

entities(txt_doc, kind = "person")
entities(txt_doc, kind = "location")

entities(txt_doc, kind = "organization")

library(NLP)
library(openNLP)
library(magrittr)


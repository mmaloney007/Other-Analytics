#install.packages(c("NLP", "openNLP", "RWeka", "qdap", "tm", "wordcloud", "Rstem", "coreNLP"))

#install.packages("SnowballC")

library("NLP")
library("openNLP")
library("RWeka")
library("qdap")
library("tm")
library("wordcloud")
#library(Rstem)
library("SnowballC")
library("coreNLP")

#downloadCoreNLP(type="base")

#initCoreNLP()

# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# and extract to your program files folder

###DEFINE TEXT DIRECTOR

cname <- file.path("~", "Desktop", "code", "text")   

cname  

# here is a pdf for mining
url <- "http://www.noisyroom.net/blog/RomneySpeech072912.pdf"
dest <- tempfile("PDF", fileext = ".pdf")
download.file(url, dest, mode = "wb")
#system("chmod 777" dest)

# set path to pdftotxt.exe and convert pdf to text
exe <- "/usr/local/bin/pdftotext"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

##COMMENTING OUT MOVE FUNCTION
#my.file.rename <- function(from, to) {
#  todir <- dirname(to)
#  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
#  file.rename(from = from,  to = to)
#}

# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)
#shell.exec(filetxt)

###COPY TEMP FILE TO TEXT DIRECTORY
file.copy(filetxt, cname)


#my.file.rename(from = filetxt,
#               to = cname + "rabata.txt")

filetxt

library("rJava")
library("NLP")
library("openNLP")
library("RWeka")
library("qdap")
library("magrittr")

if(!require("openNLPmodels.en")) 
{
  install.packages("openNLPmodels.en",
                   repos = "http://datacube.wu.ac.at/",
                   type = "source")
}

dir(cname)   # Use this to check to see that your texts have loaded.

library(tm)   
docs <- Corpus(DirSource(cname))   

summary(docs) 

inspect(docs[2])

## STRIP WHITESPACE
docs <- tm_map(docs, stripWhitespace)   

#########
### PREPARE FOR NAMED ENTITY RECOGNITION
#########

##SWITCH BACK TO PLAIN TEXT DOCUMENT
docs <- tm_map(docs, PlainTextDocument)

## *This is the end of the preprocessing stage.*   

dataframe<-data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F)

dataframe

txt <- sapply(dataframe, as.String) # a string

library("rJava")
library("NLP")
library("openNLP")
library("RWeka")
library("qdap")
library("magrittr")

if(!require("openNLPmodels.en")) 
{
  install.packages("openNLPmodels.en",
                   repos = "http://datacube.wu.ac.at/",
                   type = "source")
}

# Example test
#bio <- readLines("~/Desktop/Code/text/anb-jarena-lee.txt")

bio <- paste(txt, collapse = " ")
bio <- paste(txt, collapse = '\"')
print(bio)

# Format required for NLP
bio <- as.String(bio)

# Annotate words and sentences
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(bio, list(sent_ann, word_ann))
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

# Get sentences and words
sents(bio_doc) %>% head(2)
words(bio_doc) %>% head(10)

#OpenNLP can find dates, locations, money, organizations, percentages,
#people, and times. (Acceptable values are "date", "location", "money",
#"organization", "percentage", "person", "misc"
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

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

## NAMED ENTITY RECOGNITION

entities(bio_doc, kind = "person")
entities(bio_doc, kind = "location")
entities(bio_doc, kind = "organization")

#REMOVE PUNCUATION
docs <- tm_map(docs, removePunctuation)  

## REMOVING @ AND OTHER "IT" TYPE LETTERS
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}   

##REMOVING NUMBERS
docs <- tm_map(docs, removeNumbers) 

#MAKING ALL WORDS LOOK THE SAME
docs <- tm_map(docs, tolower)  

#REMOVING STOP WORDS
docs <- tm_map(docs, removeWords, stopwords("english"))  

#REMOVING NON-IMPORTANT WORDS
docs <- tm_map(docs, removeWords, c("department", "email"))   

# Just replace "department" and "email" with words that you would like to remove.  

#COMBINING WORDS THAT SHOUDL STAY TOGETHER
for (j in seq(docs))
{
  docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
  docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
}

## REMOVE COMMON WORD ENDINGS
library(SnowballC)   
docs <- tm_map(docs, stemDocument)   

# make each letter lowercase
mydata.corpus <- tm_map(docs, tolower) 

# remove punctuation 
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

# remove generic and custom stopwords
my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

# build a term-document matrix
mydata.dtm <- TermDocumentMatrix(mydata.corpus)

# inspect the document-term matrix
mydata.dtm

# inspect most popular words
findFreqTerms(mydata.dtm, lowfreq=30)
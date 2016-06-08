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

## STRIP WHITESPACE
docs <- tm_map(docs, stripWhitespace)   

##SWITCH BACK TO PLAIN TEXT DOCUMENT
docs <- tm_map(docs, PlainTextDocument)

## *This is the end of the preprocessing stage.*   


### Stage the Data      
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)   

### Explore your data      
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix.csv")   
### FOCUS - on just the interesting stuff...   
#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
### Word Frequency   
head(table(freq), 20)   
# The above output is two rows of numbers. The top number is the frequency with which 
# words appear and the bottom number reflects how many words appear that frequently. 
#
tail(table(freq), 20)   
# Considering only the 20 greatest frequencies
#
# **View a table of the terms after removing sparse terms, as above.
freq <- colSums(as.matrix(dtms))   
freq   
# The above matrix was created using a data transformation we made earlier. 
# **An alternate view of term frequency:**   
# This will identify all terms that appear frequently (in this case, 50 or more times).   
findFreqTerms(dtm, lowfreq=1)   # Change "50" to whatever is most appropriate for your data.
#
#
#   
### Plot Word Frequencies
# **Plot words that appear at least 50 times.**   
library(ggplot2)   
wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq>0), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   
#  
## Relationships Between Terms
### Term Correlations
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.    
findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98   
# 
# Change "question" & "analysi" to terms that actually appear in your texts.
# Also adjust the `corlimit= ` to any value you feel is necessary.
#
# 
### Word Clouds!   
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)    

### Clustering by Term Similarity

### Hierarchal Clustering   
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="ward")   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   

### K-means clustering   
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 

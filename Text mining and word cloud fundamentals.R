#download packages.
install.packages("tm")  
install.packages("SnowballC") 
install.packages("wordcloud") 
install.packages("RColorBrewer") 
#load packages 
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#download the text from project gutenberg as doc.txt 
#load the corpus 
text <- readLines(file.choose())
#load the data as a text
docs <- Corpus(VectorSource(text))
#inspect the content of the text
inspect(docs)
#text transformation: Replacing special characters with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word: choose the words you you don't want to include in the text mining.
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("talk", "look")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)
#build a term document matrix 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#generate the word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=400, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "death", corlimit = 0.3)
head(d, 10)
#Plot word frequencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="pink", main ="Most frequent words",
        ylab = "Word frequencies")

if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile="Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}

blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)


object.size(blogs)
object.size(news)
object.size(twitter)

library(tm)

set.seed(4321)
data.sample <- c(sample(blogs, length(blogs) * 0.01),
                 sample(news, length(news) * 0.01),
                 sample(twitter, length(twitter) * 0.01))

# Create corpora and clean the data
corpora <- VCorpus(VectorSource(data.sample))
to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpora <- tm_map(corpora, to_space, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpora <- tm_map(corpora, to_space, "@[^\\s]+")
corpora <- tm_map(corpora, function(x) iconv(enc2utf8(x$content), sub = "byte"))
corpora <- tm_map(corpora, tolower)
corpora <- tm_map(corpora, removeWords, stopwords("en"))
corpora <- tm_map(corpora, removePunctuation)
corpora <- tm_map(corpora, removeNumbers)
corpora <- tm_map(corpora, stripWhitespace)
corpora <- tm_map(corpora, PlainTextDocument)


library(RWeka)
library(ggplot2)

datapoints <- 40

getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
makePlot <- function(data, label) {
  ggplot(data[1:datapoints,], aes(reorder(word, -freq), freq)) +
    labs(x = label, y = "Frequency") +
    theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
    geom_bar(stat = "identity", fill = I("grey50"))
}

# Get frequencies of most common n-grams in data sample
freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpora), 0.9999))
freq2 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpora, control = list(tokenize = bigram)), 0.9999))
freq3 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpora, control = list(tokenize = trigram)), 0.9999))

makePlot(freq3, paste(datapoints ," Most Common Trigrams", sep=","))
makePlot(freq2, paste(datapoints ," Most Common Bigrams", sep=","))
makePlot(freq3, paste(datapoints ," Most Common Unigrams", sep=","))

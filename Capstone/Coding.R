# Packages
library(dplyr)
library(tibble)
library(tidytext)
library(tm)
library(ggplot2)
library(wordcloud)
library(cluster)
library(ggthemes)
library(gridExtra)
library(RWeka)

# Directories
topdir <- getwd()
readdir <- paste(topdir, '/final/en_US', sep = '')
setwd(readdir)
# Reading the Data
blogs <- readLines(con = 'en_US.blogs.txt', skipNul = TRUE)
news <- readLines(con = 'en_US.news.txt', skipNul = TRUE)
twitter <- readLines(con = 'en_US.twitter.txt', skipNul = TRUE)
setwd(topdir)
# Create vector with all text and partition into working size.
set.seed(101)
fulldata <- c(blogs, news, twitter)
data <- sample(fulldata, 0.1*length(fulldata))
rm(fulldata); rm(blogs); rm(news); rm(twitter)

# Exploratory analysis with tm package

corpus <- VCorpus(VectorSource(data))
rm(data)
## Normalize the corpus
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)
# Get rid of extremely rare words
dtm <- DocumentTermMatrix(corpus)
sparsedtm <- removeSparseTerms(dtm, 0.9998)
freqs <- sort(colSums(as.matrix(sparsedtm)), decreasing = TRUE)
head(freqs, 20)

# Visualize the corpus
wf <- data.frame(word=names(freqs), freq= freqs, row.names = NULL)
g_wordfreq <- ggplot(data = wf[1:40,], aes(x = reorder(word, -freq), y=freq)) + 
    geom_bar(stat = 'identity') + xlab('Word') + ylab('Frequency') + 
    ggtitle('Frequencies of Most Used Words') + theme_tufte() + coord_flip()
g_wordfreq
wordcloud(names(freqs), freqs, max.words = 30)

g_freqdens <- ggplot(data = wf, aes(x=log(freq))) + geom_density(fill = 'pink', alpha = 0.3) + 
    xlab('Log(frequency)') + ylab('Density') + 
    ggtitle('Distribution of word frequencies') + theme_tufte()
g_freqdens

# N-Grams

## Digram
DigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
digram <- DocumentTermMatrix(corpus, control = list(tokenize = DigramTokenizer))
digramFreqWords <- findFreqTerms(digram, lowfreq = 100)
dgfreq <- sort(colSums(as.matrix(digram[,digramFreqWords])), decreasing = TRUE)
dg_df <- data.frame(word=names(dgfreq), freq= dgfreq, row.names = NULL)

g_dgdf <- ggplot(data = dg_df[1:40,], aes(x = reorder(word, -freq), y=freq)) + 
    geom_bar(stat = 'identity') + xlab('Words') + ylab('Frequency') + 
    ggtitle('What Two Words Are Used Together Most?') + theme_tufte() + coord_flip()

g_dgdens <- ggplot(data = dg_df, aes(x=log(freq))) + 
    geom_density(fill = 'lightblue', alpha = 0.5) + xlab('Log(frequency)') + 
    ylab('Density') + ggtitle('Distribution of digram frequencies') + theme_tufte()
g_dgdens
g_dgdf


## Trigram
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))
trigramFreqWords <- findFreqTerms(trigram, lowfreq = 10)
trigramFreqWords
tgfreq <- sort(colSums(as.matrix(trigram[,trigramFreqWords])), decreasing = TRUE)
head(tgfreq)

tg_df <- data.frame(word=names(tgfreq), freq= tgfreq, row.names = NULL)

g_tgdf <- ggplot(data = tg_df[1:40,], aes(x = reorder(word, -freq), y=freq)) + 
    geom_bar(stat = 'identity') + xlab('Words') + ylab('Frequency') + 
    ggtitle('What Three Words Are Used Together Most?') + theme_tufte() + coord_flip()

g_tgdens <- ggplot(data = tg_df, aes(x=log(freq))) + 
    geom_density(fill = 'yellow', alpha = 0.4) + xlab('Log(frequency)') + 
    ylab('Density') + ggtitle('Distribution of trigram frequencies') + theme_tufte()

g_tgdens
g_tgdf

grid.arrange(g_freqdens, g_dgdens, g_tgdens, nrow = 1)

# Number of words to explain 50% of wordcount
findhalf <- function(vector) {
    for (i in 1:length(vector)) {
        if (sum(vector[1:i])/sum(vector) >= 0.5) {
            return(i)
        }
    }
}
    
num1 <- findhalf(freqs)  
num2 <- findhalf(dgfreq)
num3 <- findhalf(tgfreq)
num1;num2;num3

findcoverage <- function(vector) {
    coverage <- 1:length(vector)
    for (i in 1:length(vector)) {
        coverage[i] <- sum(vector[1:i])/sum(vector)
    }
    df <- data.frame(id <- 1:length(vector),
                     cov <- coverage)
    names(df) <- c('Words', 'Coverage')
    return(df)
}
cov1 <- findcoverage(freqs)
cov2 <- findcoverage(dgfreq)
cov3 <- findcoverage(tgfreq)

covFreq <- ggplot(data = cov1[1:1000,], 
                  aes(x=Words, y=Coverage, col=Coverage>=0.5, fill=Coverage>=0.5)) + 
    geom_line() + theme_tufte() + geom_area()
covFreq

covDg <- ggplot(data = cov2[1:600,], 
                aes(x=Words, y=Coverage, col=Coverage>=0.5, fill=Coverage>=0.5)) + 
    geom_line() + theme_tufte() + geom_area() + xlab('Word Pairs')
covDg

covTg <- ggplot(data = cov3[1:1000,], 
                aes(x=Words, y=Coverage, col=Coverage>=0.5, fill=Coverage>=0.5)) + 
    geom_line() + theme_tufte() + geom_area() + xlab('Word Triplets')
covTg



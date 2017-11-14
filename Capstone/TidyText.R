# Libraries
library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(tidytext)
library(tm)
library(ggplot2)
library(wordcloud)
library(cluster)
library(ggthemes)
library(gridExtra)
library(readr)
library(stringr)
library(igraph)
library(ggraph)
library(scales)


# Directories
topdir <- getwd()
readdir <- paste(topdir, '/final/en_US', sep = '')
setwd(readdir)
# Reading the Data
blogs <- readLines(con = 'en_US.blogs.txt', skipNul = TRUE)
news <- readLines(con = 'en_US.news.txt', skipNul = TRUE)
twitter <- readLines(con = 'en_US.twitter.txt', skipNul = TRUE)
setwd(topdir)

# Explore the parts
data("stop_words")
## Blogs
blogdf <- data_frame(text = blogs)
blogwords <- blogdf %>%
    unnest_tokens(word, text)
blogcount <- blogwords %>%
    anti_join(stop_words) %>%
    filter(!word %in% 0:100) %>%
    count(word, sort = TRUE)
## News
newsdf <- data_frame(text = news)
newswords <- newsdf %>%
    unnest_tokens(word, text)
newscount <- newswords %>%
    anti_join(stop_words) %>%
    filter(!word %in% 0:100) %>%
    count(word, sort = TRUE)
## Twitter
twitterdf <- data_frame(text = twitter)
twitterwords <- twitterdf %>%
    unnest_tokens(word, text)
twittercount <- twitterwords %>%
    anti_join(stop_words) %>%
    filter(!word %in% 0:100) %>%
    count(word, sort = TRUE)


g_bl <- ggplot(blogcount[1:15,], aes(x = reorder(word, n), y = n)) + geom_col() + theme_tufte() +
    coord_flip() + xlab('') + ylab('') + ggtitle('Blogwords')
g_news <- ggplot(newscount[1:15,], aes(x = reorder(word, n), y = n)) + geom_col() + theme_tufte() +
    coord_flip() + xlab('') + ylab('') + ggtitle('Newswords')
g_twitter <- ggplot(twittercount[1:15,], aes(x = reorder(word, n), y = n)) + geom_col() + theme_tufte() +
    coord_flip() + xlab('Word') + ylab('Frequency') + ggtitle('Twitterwords')
grid.arrange(g_bl, g_news, g_twitter, ncol = 1)

## Correlation plots

frequencies <- bind_rows(mutate(blogwords, source = 'Blogs'),
                       mutate(newswords, source = 'News'), 
                       mutate(twitterwords, source = 'Twitter')) %>%
    anti_join(stop_words) %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(source, word) %>%
    group_by(source) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-n) %>%
    spread(source, proportion) %>%
    gather(source, proportion, Blogs:News) %>%
    arrange(desc(Twitter))

### Plot the data

corplot <- ggplot(frequencies[1:60000,], aes(x = proportion, y = Twitter, 
                                            color = abs(Twitter - proportion))) +
    theme_tufte() +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), high = "#ef8a62", low = "#67a9cf") +
    facet_wrap(~source, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Twitter", x = NULL)
    
corplot

ggsave(file = 'corplot.png', plot = corplot, device = 'png', scale = 1.3)

# Create vector with all text and partition into working size.
fulldata <- c(blogs, news, twitter)
dfbig  <- data_frame(text = fulldata)
rm(fulldata); rm(blogs); rm(news); rm(twitter)

df <- sample_frac(dfbig, 0.2)
rm(dfbig)

# Working with Tidy Text
words <- df %>%
    unnest_tokens(word, text)
    
# Remove Stopwords
data('stop_words')
words <- words %>%
    anti_join(stop_words)
# Remove Number Words

words <- words %>%
    filter(!word %in% 0:1000)

wordcount <- words %>%
    count(word, sort = TRUE)
    
ggplot(wordcount[1:20,], aes(x=reorder(word, n), y=n)) + geom_col() + theme_tufte() + coord_flip() +
    ylab('Word') + xlab('Frequency')


# Digram

digram <- df %>%
    unnest_tokens(digram, text, token = 'ngrams', n = 2) %>%
    separate(digram, c('word1', 'word2'), sep = ' ') %>%
    filter(!word1 %in% 0:1000) %>%
    filter(!word2 %in% 0:1000)

    




digramcount <- digram %>%
    count(word1, word2, sort = TRUE)

ggplot(digramcount[1:20,], aes(x=reorder(paste(word1, word2), n), y=n)) + 
    geom_col() + theme_tufte() + coord_flip() + xlab('Word Pair') + ylab('Frequency')

set.seed(101)

digram_graph <- digramcount %>%
    filter(n > 8000) %>%
    graph_from_data_frame()

a  <- arrow(type = 'closed', length = unit(.15, 'inches'))
g <- ggraph(digram_graph, 'fr') +
    geom_edge_link(aes(edge_alpha = n), edge_colour = 'grey30', show.legend = FALSE,
                   arrow = a, end_cap = circle(.12, 'inches')) + 
    geom_node_point(color = 'lightpink', size = 5) +
    geom_node_text(aes(label = name),repel = TRUE) +
    theme_void() + ggtitle('Connection Graph of Digrams')
g

#ggsave(filename = 'bigram.png', plot = g, device = 'png')

# Trigram

trigram <- df %>%
    unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>% 
    separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
    filter(!word1 %in% 0:1000) %>%
    filter(!word2 %in% 0:1000) %>%
    filter(!word3 %in% 0:1000)

trigramcount <- trigram %>%
    count(word1, word2, word3, sort=TRUE)

ggplot(trigramcount[1:20,], aes(x=reorder(paste(word1, word2, word3), n), y=n)) + 
    geom_col() + theme_tufte() + coord_flip() + xlab('Word Trio') + ylab('Frequency')

# Make Digram from Trigram for graphing
tgc1 <- trigramcount[,c(1,2,4)] %>%
    filter(n>100)
tgc2 <- trigramcount[,c(2,3,4)] %>%
    filter(n>100)
colnames(tgc2) <- c('word1', 'word2', 'n')
tgc <- rbind(tgc1, tgc2) %>%
    arrange(desc(n))



trigram_graph <- tgc %>%
    filter(n > 1000) %>%
    graph_from_data_frame()

t <- ggraph(trigram_graph, 'fr') +
    geom_edge_link(aes(edge_alpha = n), edge_colour = 'grey30', show.legend = FALSE,
                   arrow = a, end_cap = circle(.12, 'inches')) + 
    geom_node_point(color = 'lightblue', size = 5) +
    geom_node_text(aes(label = name),repel = TRUE) +
    theme_void() + ggtitle('Connection Graph of Trigrams')
t

grid.arrange(g, t)


# Quadgram

quadgram <- df %>%
    unnest_tokens(quadgram, text, token = 'ngrams', n = 4) %>% 
    separate(quadgram, c('word1', 'word2', 'word3', 'word4'), sep = ' ')

quadgramcount <- quadgram %>%
    count(word1, word2, word3, word4, sort = TRUE)
    
ggplot(quadgramcount[1:20,], aes(x=reorder(paste(word1, word2, word3, word4), n), y=n)) + 
    geom_col() + theme_tufte() + coord_flip() +
    ylab('Word') + xlab('Frequency')

qgcfilt <- quadgramcount %>%
    filter(n > 100)

qgc1 <- qgcfilt[,c(1,2,5)]
qgc2 <- qgcfilt[,c(2,3,5)]
colnames(qgc2) <- c('word1', 'word2', 'n')
qgc3 <- qgcfilt[,c(3,4,5)]
colnames(qgc3) <- c('word1', 'word2', 'n')

qgc <- rbind(qgc1, qgc2, qgc3) %>%
    ddply(.variables = c('word1', 'word2'), numcolwise(sum)) %>%
    arrange(desc(n)) %>%
    dplyr::as_data_frame()

qgcgraph <- qgc %>%
    filter(n > 500) %>%
    graph_from_data_frame()

q <- ggraph(qgcgraph, 'fr') +
    geom_edge_link(aes(edge_alpha = n), edge_colour = 'grey30', show.legend = FALSE,
                   arrow = a, end_cap = circle(.12, 'inches')) + 
    geom_node_point(color = 'goldenrod', size = 5) +
    geom_node_text(aes(label = name),repel = TRUE) +
    theme_void() + ggtitle('Connection Graph of Quadgrams')
q


grid.arrange(g, t, q)

ggsave(filename = 'digram.png', plot = g, device = 'png', scale = 1.2)
ggsave(filename = 'trigram.png', plot = t, device = 'png', scale = 1.2)
ggsave(filename = 'quadgram.png', plot = q, device = 'png', scale = 1.2)


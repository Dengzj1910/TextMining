# 
rm(list=ls())
install.packages("dplyr")
install.packages("tidytext")
install.packages("igraph")
install.packages("ggraph")
install.packages("widyr")
install.packages("tm")
#
text1 <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
class(text1)

#dplyr provides a flexible grammar of data manipulation. 
library(dplyr)

text_df <- data_frame(line = 1:4, text = text1)
text_df
data_frame

library(tidytext)
text_df %>% unnest_tokens(word, text)
unnest_tokens(text_df,word, text)
?unnest_tokens

#tydying the works of Jane Austen
install.packages("janeaustenr")
install.packages("stringr")
library(janeaustenr)
library(dplyr)
library(stringr)
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

class(original_books)
class(austen_books())

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)  # stop words are words that are not useful for an analysis, typically extremely common words such as ¡°the¡±, ¡°of¡±, ¡°to¡±, and so forth in English. We can remove stop words (kept in the tidytext dataset stop_words) with an anti_join().

tidy_books %>%
  count(word, sort = TRUE) 


library(ggplot2)
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#gutenbergr package:including tools both for downloading books and a coplete dataset of project gutenberg)
#https://ropensci.org/tutorials/gutenbergr_tutorial/ 
gutenberg_metadata %>%
  filter(title == "The Time Machine")

#1.5 Word frequency
install.packages("gutenbergr")
library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

##Chapter 2
library(tidytext)
sentiments
get_sentiments("afinn")#The AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment.
get_sentiments("bing")#The bing lexicon categorizes words in a binary fashion into positive and negative categories. 
get_sentiments("nrc") #he nrc lexicon categorizes words in a binary fashion (¡°yes¡±/¡°no¡±) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. 
?cumsum

library(tidyr)
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#%/% division remaining integer
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

#2.5 wordclouds
install.packages("wordcloud")
library("wordcloud")
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

install.packages("reshape2")
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

#2.6
PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")
PandP_sentences$sentence[2]


#chapter 3 :Analyzing word and document frequency:ty-idf
#idf:a iterm's inverse document frequency.
##1. term frequency
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()
groups(book_words)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)
book_words

library(ggplot22)

#show.legend = ture means ËµÃ÷
#the distribution of n/total for each novel,This is exactly what term frequency is.
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2,scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)
freq_by_rank


freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

#get the regression relationship between frequency and rank
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#bind-tf_idf: find the important words for the content of each document by decreasing the weight for commonly used words 
#             but not too common(like is ,of)
# the idf and tf_idf is 0 for these extrmely common words.
# The inverse document frequency (and thus tf-idf) is very low (near zero) for words that occur in many of the documents in a collection;
# The inverse document frequency will be a higher number for words that occur in fewer of the documents in the collection.
#https://zh.wikipedia.org/wiki/Tf-idf --wiki about tf_idf
#tf_idf:term frequency¨Cinverse document frequency
#idf :lg(total book n /(1+book contains this word m);tf(term frequency) tf_idf=idf*tf
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
tail(book_words,n=100) %>% print(n=100)

#
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#ungroup is to solve the data compatibility problem.
#coord_flip():Cartesian Coordinates With X And Y Flipped
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

## relationships between words

library(dplyr)
library(tidytext)
library(janeaustenr)

#ngrams:tokenize text into consecutive sequences of words.
# By seeing how often word X is followed by word Y, we can then build a model of the relationships between them.
# n:the number of words we wish to capture in each igram
#bigrams:pairs of two consecutive words,it has more information,useful in large text
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

austen_bigrams %>%  count(bigram, sort = TRUE)

#excludes stop-words
library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1,word2, sort = TRUE)
bigram_counts
#unite():recombine the columns into one.
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

#Analyzing bigrams,eg:the most common "streets" mentioned ind each book;
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

#the tf-idf analysis
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

library("ggplot2")
#using bigrams to provide context in sentiment analysis
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
#afinn:it is a lexicon
AFINN <- get_sentiments("afinn")
AFINN
#the most common sentiment-associated word to follow ¡°not¡± was ¡°like¡±, which would normally have a (positive) score of 2.
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()
not_words
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

# the same operation to not no never without.
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()
negated_words
negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~word1, ncol = 2, scales = "free") +
  coord_flip()

#visualizing a network of bigrams with ggraph
#ggraph, which extends ggplot2 to construct network plots, 
#and widyr, which calculates pairwise correlations and distances within a tidy data frame.
#visualizing all of the relationships among words simultaneously, rather than just the top few at a time.
# we can arrange the words into a network, or ¡°graph.¡±
# A graph has three variable:from :the node an edge is coming from
#;to :the node an edge is going towards;weight: A numeric value associated with each edge;
#igraph::graph_from_data_frame():takes a data frame of edges with columns for ¡°from¡±, ¡°to¡±, and edge attributes (in this case n)

library(igraph)

# original counts
bigram_counts
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>% filter( n > 20) %>% graph_from_data_frame()
bigram_graph

#convert igraph object into ggraph
#From the plot,you could know that most common combinations for 2 words are salutation.
#like miss lady sir
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#better looking
#edge_alpha =n  means the links based on how common or rare the bigram is.n is larger,the color is more darker.
#nd_cap option that tells the arrow to end before touching the node
#theme_void:empty theme;vjust:vertical adjustment hjust:horizonal adjust
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 3) +
  theme_void()


library(gutenbergr)
kjv <- gutenberg_download(10)
library(stringr)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

kjv_bigrams <- kjv %>%
  count_bigrams()

# filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()

##counting and correlating pairs of words with widyr package

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

library(widyr)

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs
#pair words correlation
#indicates how often they appear together relative to how often they appear separately.
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
word_cors %>%
  filter(item1 == "pounds")

#pick up particular interesting words and find the other words most associated with them
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
word_cors

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

## tidy document termmatrix objects
library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress
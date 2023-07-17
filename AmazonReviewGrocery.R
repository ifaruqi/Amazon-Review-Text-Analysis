## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Load Required Libraries
library(tidyverse)
library(jsonlite)
library(qdap)
library(stringr)
library(tm)
library(textclean)
library(hunspell)
library(reticulate)
library(qdap)
library(qdapDictionaries)
library(tidytext)
library(topicmodels)
library(dplyr)
library(lda)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Input Review Data
grocery_review <- stream_in(file("Grocery_and_Gourmet_Food_5.json"))
glimpse(grocery_review)

## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Pick Only Relevant Variables
grocery_review <- grocery_review[,c("overall", "reviewTime", "reviewerID", "asin", "reviewText")]

# Change Data Type for Review Time
grocery_review$reviewTime <- as.Date(grocery_review$reviewTime, format = "%m %d, %Y")

# Check Duplicate Based On All Columns
duplicate_rows <- duplicated(grocery_review)

# Get the duplicate rows
duplicate_rows <- grocery_review[duplicate_rows, ] # it is found that 57168 rows are duplicate

# Remove duplicate
grocery_review <- distinct(grocery_review)

# Check Updated Data
glimpse(grocery_review)

## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Summary of Grocery Reviews
review_summary <- grocery_review %>% 
	summarise("Number of Reviews" = n(), 
	          "Average Rating" = round(mean(overall, na.rm=TRUE),2), 
	          "Standard Deviation Rating" = round(sd(overall, na.rm=TRUE),2),
	          "Average Review Length" = round(mean(nchar(reviewText), na.rm=TRUE),0),
	          "Start Period" = min(reviewTime),
	          "End Period" = max(reviewTime),
	          "Number of Reviewer" = length(unique(reviewerID)))

review_summary


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Randomly pick 100000 reviews
set.seed(123)
grocery_review_sample <- grocery_review[sample(nrow(grocery_review), 100000), c("overall", "reviewTime", "reviewText", "reviewerID")]


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Summary of Grocery Reviews
review_sample_summary <- grocery_review_sample %>% 
	summarise("Number of Reviews" = n(), 
	          "Average Rating" = round(mean(overall, na.rm=TRUE),2), 
	          "Standard Deviation Rating" = round(sd(overall, na.rm=TRUE),2),
	          "Average Review Length" = round(mean(nchar(reviewText), na.rm=TRUE),0),
	          "Start Period" = min(reviewTime),
	          "End Period" = max(reviewTime),
	          "Number of Reviewer" = length(unique(reviewerID)))

review_sample_summary


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove HTML tags
grocery_review_sample$cleanReview <- str_replace_all(grocery_review_sample$reviewText, "<.*?>", "")

# Remove double spaces
grocery_review_sample$cleanReview <- str_replace_all(grocery_review_sample$cleanReview, "\\s{2,}", " ")


# Convert the letter to lower case
grocery_review_sample$cleanReview <- tolower(grocery_review_sample$cleanReview)

# Expand contractions
grocery_review_sample$cleanReview <- replace_contraction(
  grocery_review_sample$cleanReview,
  contraction = qdapDictionaries::contractions,
  replace = NULL,
  ignore.case = TRUE,
  sent.cap = TRUE
)

# Remove all special characters using regex
grocery_review_sample$cleanReview <- grocery_review_sample$cleanReview %>%
gsub("[^[:alpha:][:space:]]", " ",.)

# remove extra spaces
grocery_review_sample$cleanReview <- str_squish(grocery_review_sample$cleanReview)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Check Stop Words
stop_words
stop_words %>% group_by(lexicon) %>% count()


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create New Table for Further Analysis
grocery_review_sample$reviewID = seq(1:nrow(grocery_review_sample))

# Only pick relevant variables
review <- grocery_review_sample[,c("reviewID", "overall", "cleanReview")]


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Define a regular expression pattern to match hyphenated words
hyphen_pattern <- "[[:alnum:]]+(?:[-'][[:alnum:]]+)*"

# Define a custom tokenization function using the hyphen_pattern
custom_tokenize <- function(x) {
  str_extract_all(x, hyphen_pattern)
}

# Generate token
review_token = review%>%
    unnest_tokens(cleanReview,output=word_token,token=custom_tokenize)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove stop words
review_token = anti_join(review_token,stop_words,by=c("word_token"="word"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Stemming
library(hunspell)
review_token <- review_token %>%
  mutate(Hunspell_stem = hunspell_stem(word_token)) %>% 
  unnest(Hunspell_stem)

# Lemmatization
library(textstem)
review_token <- review_token %>%
  mutate(word_lemma = lemmatize_words(word_token)) %>% 
  unnest(word_lemma)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Most Common Words Overall
p <- review_token %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  head(5) %>%
  ggplot(.,aes(y=reorder(Hunspell_stem,n),x=n))+
  geom_bar(stat = 'identity', fill = "black")+
  ggtitle("Dominant Words - Overall")+
  labs(x = "Count", y = "Word") +
  theme_classic()

# Most Common Words per Rating
p1 <- review_token %>% 
  filter(overall == 1) %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "red")+
  ggtitle("Dominant Words - Rating 1")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p2 <- review_token %>% 
  filter(overall == 2) %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "orange")+
  ggtitle("Dominant Words - Rating 2")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p3 <- review_token %>%
  filter(overall == 3) %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "coral")+
  ggtitle("Dominant Words - Rating 3")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p4 <- review_token %>%
  filter(overall == 4) %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "lightgreen")+
  ggtitle("Dominant Words - Rating 4")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p5 <- review_token %>% 
  filter(overall == 5) %>%
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "green")+
  ggtitle("Dominant Words - Rating 5")+
  labs(x = "Count", y = "Word") +
  theme_classic()



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot Together
library(patchwork)
(p1 + p2) / (p3 + p4) / (p5 + p)
  plot_layout(nrow = 3, heights = c(1, 1, 1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Get top 20 words from each overall rating
top_20_1 <- review_token %>%
  filter(overall == 1) %>%
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 20, wt = n) %>%
  arrange(desc(n)) %>%
  rename(word = Hunspell_stem, frequency = n)

top_20_2 <- review_token %>%
  filter(overall == 2) %>%
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 20, wt = n) %>%
  arrange(desc(n)) %>%
  rename(word = Hunspell_stem, frequency = n)

top_20_3 <- review_token %>%
  filter(overall == 3) %>%
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 20, wt = n) %>%
  arrange(desc(n)) %>%
  rename(word = Hunspell_stem, frequency = n)

top_20_4 <- review_token %>%
  filter(overall == 4) %>%
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 20, wt = n) %>%
  arrange(desc(n)) %>%
  rename(word = Hunspell_stem, frequency = n)

top_20_5 <- review_token %>%
  filter(overall == 5) %>%
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 20, wt = n) %>%
  arrange(desc(n)) %>%
  rename(word = Hunspell_stem, frequency = n)

# List of dataframes
top_20 <- list(top_20_1, top_20_2, top_20_3, top_20_4, top_20_5)

# Perform inner join
common_words <- Reduce(function(x, y) inner_join(x, y, by = "word"), top_20)

# View the resulting dataframe
common_words


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Update stop words
newwords = data.frame(word=common_words$word,lexicon=rep("custom",1))
head(newwords)
allstopwords = rbind(newwords,stop_words)
head(allstopwords)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove stop words
review_token_clean = anti_join(review_token,allstopwords,by=c("Hunspell_stem"="word"))

nrow(review_token)
nrow(review_token_clean)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
p <- review_token_clean %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  head(5) %>%
  ggplot(.,aes(y=reorder(Hunspell_stem,n),x=n))+
  geom_bar(stat = 'identity', fill = "black")+
  ggtitle("Dominant Words - Overall")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p1 <- review_token_clean %>% 
  filter(overall == 1) %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "red")+
  ggtitle("Dominant Words - Rating 1")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p2 <- review_token_clean %>% 
  filter(overall == 2) %>% 
  count(Hunspell_stem, sort = TRUE) %>%
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "orange")+
  ggtitle("Dominant Words - Rating 2")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p3 <- review_token_clean %>% 
  filter(overall == 3) %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "coral")+
  ggtitle("Dominant Words - Rating 3")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p4 <- review_token_clean %>% 
  filter(overall == 4) %>% 
  count(Hunspell_stem, sort = TRUE) %>%
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "lightgreen")+
  ggtitle("Dominant Words - Rating 4")+
  labs(x = "Count", y = "Word") +
  theme_classic()

p5 <- review_token_clean %>% 
  filter(overall == 5) %>% 
  count(Hunspell_stem, sort = TRUE) %>% 
  top_n(n = 5) %>%
  ggplot(aes(y = reorder(Hunspell_stem, n), x = n)) +
  geom_bar(stat = 'identity', fill = "green")+
  ggtitle("Dominant Words - Rating 5")+
  labs(x = "Count", y = "Word") +
  theme_classic()


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot together
(p1 + p2) / (p3 + p4) / (p5 + p)
  plot_layout(nrow = 3, heights = c(1, 1, 1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
library(wordcloud)

# Word Cloud for Overall Rating
token_count <- review_token_clean %>% 
  group_by(Hunspell_stem) %>% count() %>% arrange(desc(n))

wordcloud(
  words = token_count$Hunspell_stem, 
  freq = token_count$n, 
  max.words = 10, 
  colors = "black"
)

# Word Cloud for Rating 1
token_count_1 <- review_token_clean %>% 
  filter(overall == 1) %>% 
  group_by(Hunspell_stem) %>% 
  count() %>% 
  arrange(desc(n))

wordcloud(
  words = token_count_1$Hunspell_stem, 
  freq = token_count_1$n, 
  max.words = 10, 
  colors = "red",
  scale=c(2, .5)
)

# Word Cloud for Rating 2
token_count_2 <- review_token_clean %>% 
  filter(overall == 2) %>% 
  group_by(Hunspell_stem) %>% 
  count() %>% 
  arrange(desc(n))

wordcloud(
  words = token_count_2$Hunspell_stem, 
  freq = token_count_2$n, 
  max.words = 10, 
  colors = "orange",
  scale=c(2, .5)
)

# Word Cloud for Rating 3
token_count_3 <- review_token_clean %>% 
  filter(overall == 3) %>% 
  group_by(Hunspell_stem) %>% 
  count() %>% 
  arrange(desc(n))

wordcloud(
  words = token_count_3$Hunspell_stem, 
  freq = token_count_3$n, 
  max.words = 10, 
  colors = "coral",
  scale=c(2, .5)
)

# Word Cloud for Rating 4
token_count_4 <- review_token_clean %>% 
  filter(overall == 4) %>% 
  group_by(Hunspell_stem) %>% 
  count() %>% 
  arrange(desc(n))

wordcloud(
  words = token_count_4$Hunspell_stem, 
  freq = token_count_4$n, 
  max.words = 10, 
  colors = "lightgreen",
  scale=c(2, .5)
)

# Word Cloud for Rating 5
token_count_5 <- review_token_clean %>% 
  filter(overall == 5) %>% 
  group_by(Hunspell_stem) %>% 
  count() %>% 
  arrange(desc(n))

wordcloud(
  words = token_count_5$Hunspell_stem, 
  freq = token_count_5$n, 
  max.words = 10, 
  colors = "green",
  scale=c(2, .5)
)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
review_token_counts <- review_token_clean %>%
count(Hunspell_stem,overall, reviewID, sort = TRUE) %>%
ungroup() %>%
rename(count=n)
head(review_token_counts)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
review_token_tfidf <- review_token_counts %>%
bind_tf_idf(Hunspell_stem, reviewID, count)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
str(review_token_tfidf)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
p_tfidf <- review_token_tfidf %>% 
  group_by(Hunspell_stem) %>% 
  summarize("tfidf_score" = sum(tf_idf)) %>% 
  arrange(desc(tfidf_score)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(Hunspell_stem, tfidf_score), x = tfidf_score)) +
  geom_bar(stat = 'identity', fill = "black") +
  ggtitle("Dominant Words - Overall")+
  labs(x = "TFIDF Score", y = "Word") +
  theme_classic()

p1_tfidf <- review_token_tfidf %>% 
  filter(overall==1) %>% 
  group_by(Hunspell_stem) %>% 
  summarize("tfidf_score" = sum(tf_idf)) %>% 
  arrange(desc(tfidf_score)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(Hunspell_stem, tfidf_score), x = tfidf_score)) +
  geom_bar(stat = 'identity', fill = "red") +
  ggtitle("Dominant Words - Rating 1")+
  labs(x = "TFIDF Score", y = "Word") +
  theme_classic()

p2_tfidf <- review_token_tfidf %>% 
  filter(overall==2) %>% 
  group_by(Hunspell_stem) %>% 
  summarize("tfidf_score" = sum(tf_idf)) %>% 
  arrange(desc(tfidf_score)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(Hunspell_stem, tfidf_score), x = tfidf_score)) +
  geom_bar(stat = 'identity', fill="orange") +
  ggtitle("Dominant Words - Rating 2")+
  labs(x = "TFIDF Score", y = "Word") +
  theme_classic()

p3_tfidf <- review_token_tfidf %>% 
  filter(overall==3) %>% 
  group_by(Hunspell_stem) %>% 
  summarize("tfidf_score" = sum(tf_idf)) %>% 
  arrange(desc(tfidf_score)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(Hunspell_stem, tfidf_score), x = tfidf_score)) +
  geom_bar(stat = 'identity', fill="coral" ) +
  ggtitle("Dominant Words - Rating 3")+
  labs(x = "TFIDF Score", y = "Word") +
  theme_classic()

p4_tfidf <- review_token_tfidf %>% 
  filter(overall==4) %>% 
  group_by(Hunspell_stem) %>% 
  summarize("tfidf_score" = sum(tf_idf)) %>% 
  arrange(desc(tfidf_score)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(Hunspell_stem, tfidf_score), x = tfidf_score)) +
  geom_bar(stat = 'identity', fill="lightgreen") +
  ggtitle("Dominant Words - Rating 4")+
  labs(x = "TFIDF Score", y = "Word") +
  theme_classic()

p5_tfidf <- review_token_tfidf %>% 
  filter(overall==5) %>% 
  group_by(Hunspell_stem) %>% 
  summarize("tfidf_score" = sum(tf_idf)) %>% 
  arrange(desc(tfidf_score)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(Hunspell_stem, tfidf_score), x = tfidf_score)) +
  geom_bar(stat = 'identity', fill="green") +
  ggtitle("Dominant Words - Rating 5")+
  labs(x = "TFIDF Score", y = "Word") +
  theme_classic()



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot together
(p1_tfidf + p2_tfidf) / (p3_tfidf + p4_tfidf) / (p5_tfidf + p_tfidf)
  plot_layout(nrow = 3, heights = c(1, 1, 1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# n-gram analysis
library(ngramr)
library(quanteda)
library(tm)

ngramsentence <- review %>%
  select(cleanReview, overall) %>% 
  unnest_tokens(sentence, cleanReview, token = "sentences")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# 2-gram 
twograms <- ngramsentence %>%
  unnest_tokens(term, sentence, token = "ngrams", n = 2) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_length(first) > 1 & str_length(second) > 1) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="black") +
  ggtitle("Dominant Words - Overall")+
  labs(x = "Frequency", y = "2-gram") +
  theme_classic()

twograms_1 <- ngramsentence %>%
  filter(overall == 1) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 2) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_length(first) > 1 & str_length(second) > 1) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="red") +
  ggtitle("Dominant Words - Rating 1")+
  labs(x = "Frequency", y = "2-gram") +
  theme_classic()


twograms_2 <- ngramsentence %>%
  filter(overall == 2) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 2) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_length(first) > 1 & str_length(second) > 1) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="orange") +
  ggtitle("Dominant Words - Rating 2")+
  labs(x = "Frequency", y = "2-gram") +
  theme_classic()


twograms_3 <- ngramsentence %>%
  filter(overall == 3) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 2) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_length(first) > 1 & str_length(second) > 1) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="coral") +
  ggtitle("Dominant Words - Rating 3")+
  labs(x = "Frequency", y = "2-gram") +
  theme_classic()

twograms_4 <- ngramsentence %>%
  filter(overall == 4) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 2) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_length(first) > 1 & str_length(second) > 1) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="lightgreen") +
  ggtitle("Dominant Words - Rating 4")+
  labs(x = "Frequency", y = "2-gram") +
  theme_classic()


twograms_5 <- ngramsentence %>%
  filter(overall == 5) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 2) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_length(first) > 1 & str_length(second) > 1) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="green") +
  ggtitle("Dominant Words - Rating 5")+
  labs(x = "Frequency", y = "2-gram") +
  theme_classic()



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot together
(twograms_1 + twograms_2) / (twograms_3 + twograms_4) / (twograms_5 + twograms)
  plot_layout(nrow = 3, heights = c(1, 1, 1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# 3-gram
threegrams <- ngramsentence %>%
  unnest_tokens(term, sentence, token = "ngrams", n = 3) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second", "third"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_detect(third, "[a-z]") & ( 
           str_length(first) > 1 & str_length(second) > 1 & str_length(third) > 1)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="black") +
  ggtitle("Dominant Words - Overall")+
  labs(x = "Frequency", y = "3-gram") +
  theme_classic()
  
threegrams_1 <- ngramsentence %>%
  filter(overall == 1) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 3) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second", "third"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_detect(third, "[a-z]") & ( 
           str_length(first) > 1 & str_length(second) > 1 & str_length(third) > 1)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="red") +
  ggtitle("Dominant Words - Rating 1")+
  labs(x = "Frequency", y = "3-gram") +
  theme_classic()

threegrams_2 <- ngramsentence %>%
  filter(overall == 2) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 3) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second", "third"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_detect(third, "[a-z]") & ( 
           str_length(first) > 1 & str_length(second) > 1 & str_length(third) > 1)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="orange") +
  ggtitle("Dominant Words - Rating 2")+
  labs(x = "Frequency", y = "3-gram") +
  theme_classic()

threegrams_3 <- ngramsentence %>%
  filter(overall == 3) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 3) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second", "third"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_detect(third, "[a-z]") & ( 
           str_length(first) > 1 & str_length(second) > 1 & str_length(third) > 1)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="coral") +
  ggtitle("Dominant Words - Rating 2")+
  labs(x = "Frequency", y = "3-gram") +
  theme_classic()

threegrams_4 <- ngramsentence %>%
  filter(overall == 4) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 3) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second", "third"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_detect(third, "[a-z]") & ( 
           str_length(first) > 1 & str_length(second) > 1 & str_length(third) > 1)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="lightgreen") +
  ggtitle("Dominant Words - Rating 4")+
  labs(x = "Frequency", y = "3-gram") +
  theme_classic()


threegrams_5 <- ngramsentence %>%
  filter(overall == 5) %>% 
  unnest_tokens(term, sentence, token = "ngrams", n = 3) %>%
              count(term,sort = T) %>%
            ungroup() %>% 
  separate(term, into = c("first","second", "third"), sep = " ", remove = FALSE) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("first" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("second" = "word")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]") & str_detect(third, "[a-z]") & ( 
           str_length(first) > 1 & str_length(second) > 1 & str_length(third) > 1)) %>% 
  head(5) %>% 
  ggplot(aes(y = reorder(term, n), x = n)) +
  geom_bar(stat = 'identity', fill="green") +
  ggtitle("Dominant Words - Rating 5")+
  labs(x = "Frequency", y = "3-gram") +
  theme_classic()



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot together
(threegrams_1 + threegrams_2) / (threegrams_3 + threegrams_4) / (threegrams_5 + threegrams)
  plot_layout(nrow = 3, heights = c(1, 1, 1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Check occurence of each rating
table(grocery_review_sample$overall)
prop.table(table(grocery_review_sample$overall))


## ----echo=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------
# Importing dictionary
bing_dictionary <- tidytext::get_sentiments("bing")
afinn_dictionary <- tidytext::get_sentiments("afinn")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Assign value to each token
bing_sentiment <- left_join(review_token,bing_dictionary,by = c("word_token"="word"))
bing_sentiment <- bing_sentiment %>% rename("bing_sentiment" = "sentiment")

# Use mutate to replace the values in bing_sentiment
bing_sentiment <- mutate(bing_sentiment, 
                  bing_sentiment = case_when(
                    is.na(bing_sentiment) ~ 0,
                    bing_sentiment == "positive" ~ 1,
                    bing_sentiment == "negative" ~ -1
                  ))

# We can then compute the overall sentiment for each message by summing the sentiment scores
bing_msg <- bing_sentiment %>%
  group_by(overall,reviewID) %>%
  summarise(bing = sum(bing_sentiment)) %>% 
  arrange((bing))

# Average Sentiment Score for Each Rating
bing_msg %>%
  group_by(overall) %>%
  summarise(bing = mean(bing))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a linear model to predict overall rating based on sentiment score
bing_msg$overall <- as.numeric(bing_msg$overall)
(rating_by_sentiment <- lm(overall ~ bing, data = bing_msg))
summary(rating_by_sentiment)
cbind(coefficient=coef(rating_by_sentiment), confint(rating_by_sentiment))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
library(nnet)

# Split the data into training and testing sets (70% for training, 30% for testing)
set.seed(123)
sample_bing <- bing_msg[sample(nrow(bing_msg), 5000),]
trainIndex <- createDataPartition(sample_bing$overall, p = 0.7, list = FALSE)
trainBing <- sample_bing[trainIndex, ]
testBing <- sample_bing[-trainIndex, ]

# Build a model
bingModel <- lm(overall ~ bing, data = trainBing)

# Predict the ratings on the test data
bingPrediction <- round(predict(bingModel, newdata = testBing, type = "response"),0)

# Evaluate the accuracy of the model
bingAccuracy <- mean(bingPrediction == testBing$overall)
cat("Bing Accuracy:", bingAccuracy, "\n")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Assign value to each token
afinn_sentiment <- left_join(review_token,afinn_dictionary,by = c("word_token"="word"))
afinn_sentiment <- afinn_sentiment %>% rename("afinn_sentiment" = "value")

# Use mutate to replace the values in afinn_sentiment
afinn_sentiment$afinn_sentiment <-  replace_na(afinn_sentiment$afinn_sentiment,0)

# Calculate the sentiment on each review
afinn_msg <- afinn_sentiment %>%
  group_by(overall, reviewID) %>%
  summarise(afinn = sum(afinn_sentiment))

# Calculate average sentiment score for each rating
afinn_msg %>%
  group_by(overall) %>%
  summarise(afinn = mean(afinn))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a linear model to predict overall rating based on sentiment score
afinn_msg$overall <- as.numeric(afinn_msg$overall)
(rating_by_sentiment <- lm(overall ~ afinn, data = afinn_msg))
summary(rating_by_sentiment)
cbind(coefficient=coef(rating_by_sentiment), confint(rating_by_sentiment))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the data into training and testing sets (70% for training, 30% for testing)
set.seed(123)
sample_afinn <- afinn_msg[sample(nrow(afinn_msg), 5000),]
trainIndex <- createDataPartition(sample_afinn$overall, p = 0.7, list = FALSE)
trainAfinn <- sample_afinn[trainIndex, ]
testAfinn <- sample_afinn[-trainIndex, ]

# Build a model
afinnModel <- lm(overall ~ afinn, data = trainAfinn)

# Predict the ratings on the test data
afinnPrediction <- round(predict(afinnModel, newdata = testAfinn, type = "response"),0)

# Evaluate the accuracy of the model
afinnAccuracy <- mean(afinnPrediction == testAfinn$overall)
cat("Afinn Accuracy:", afinnAccuracy, "\n")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create the DTM matrix of counts and assess its dimensions
dtm <- review_token_clean %>% 
  select(reviewID, Hunspell_stem, overall) %>% 
  count(reviewID,Hunspell_stem) %>% cast_dtm(reviewID,Hunspell_stem,n)

dtm <- removeSparseTerms(dtm,0.98)

# Remove row (review) that has all zero value
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# Check the updated dimensions of the matrix
dtm

## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Prepare data for building the model
X <- as.data.frame(as.matrix(dtm))
X$reviewID <- as.integer(dtm$dimnames$Docs)
X <- left_join(X,review,by=c("reviewID"))
X <- select(X,-c("reviewID","cleanReview"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the dataset into training and testing sets
set.seed(123)
X$overall <- as.factor(X$overall) 
idx <- sample(1:nrow(X), size = 0.7 * nrow(X), replace = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Multinomial Count Model
multinom_model <- multinom(overall ~ ., data = X[idx,])

# Predict the ratings for the test data using the multinomial logistic regression model
predictions <- predict(multinom_model, newdata = X[-idx,])

# Get the actual ratings from the test data
actual_ratings <- X[-idx,]$overall

# Compare the predicted ratings with the actual ratings
comparison <- data.frame(Actual = actual_ratings, Predicted = predictions)

# Overall performance
sum(comparison$Actual == comparison$Predicted)/length(comparison$Actual)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Create the DTM matrix of counts and assess its dimensions
dtm_tfidf <- review_token_clean %>%
  count(reviewID,Hunspell_stem) %>% 
  ungroup() %>%
  rename(count=n) %>%  
  bind_tf_idf(Hunspell_stem, reviewID, count) %>%
  cast_dtm(reviewID, Hunspell_stem, tf_idf)

dtm_tfidf <- removeSparseTerms(dtm_tfidf,0.98)

# Remove row (review) that has all zero value
dtm_tfidf <- dtm[rowSums(as.matrix(dtm_tfidf)) > 0, ]

dtm_tfidf


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Prepare data for building the model
X_tfidf <- as.data.frame(as.matrix(dtm_tfidf))
X_tfidf$reviewID <- as.integer(dtm_tfidf$dimnames$Docs)
X_tfidf <- left_join(X_tfidf,review,by=c("reviewID"))
X_tfidf <- select(X_tfidf,-c("reviewID","cleanReview"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the dataset into training and testing sets
set.seed(123)
X_tfidf$overall <- as.factor(X_tfidf$overall) 
idx <- sample(1:nrow(X_tfidf), size = 0.7 * nrow(X_tfidf), replace = FALSE)

# Multinomial TFIDF Model
multinom_tfidf_model <- multinom(overall ~ ., data = X_tfidf[idx,])

# Predict the ratings for the test data using the multinomial logistic regression model
predictions_tfidf <- predict(multinom_tfidf_model, newdata = X_tfidf[-idx,])

# Get the actual ratings from the test data
actual_ratings_tfidf <- X_tfidf[-idx,]$overall

# Compare the predicted ratings with the actual ratings
comparison_tfidf <- data.frame(Actual = actual_ratings_tfidf, Predicted = predictions_tfidf)

# Overall performance
sum(comparison_tfidf$Actual == comparison_tfidf$Predicted)/length(comparison_tfidf$Actual)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Generate the topic with LDA
library(topicmodels)
dtm_topic <- review_token_clean %>% 
  select(reviewID, Hunspell_stem, overall) %>% 
  count(reviewID,Hunspell_stem) %>% cast_dtm(reviewID,Hunspell_stem,n)
topic_model <- LDA(dtm_topic,k = 15,method = "Gibbs")


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Prepare data for the model
library(tidyverse)
library(tidytext)
X_topic <- tidy(topic_model, matrix = "gamma")
head(X_topic)

# Group by document and topic and sum the gamma values (which represent the probability).
X1_topic <- X_topic %>%
  group_by(document,topic) %>%
  summarize(Prob = sum(gamma)) %>%
  ungroup()
X1_topic$topic = paste0("T",X1_topic$topic)
head(X1_topic)

# Get the topics across the columns
library(tidyr)
X11_topic <- X1_topic %>%
  pivot_wider(names_from = topic, values_from = Prob)
X11_topic <- as.data.frame(X11_topic)
X11_topic$document = as.integer(X11_topic$document)

# Create the dataframe with the target variable included
X12_topic = left_join(X11_topic,review,embedding,by=c("document"="reviewID"))
head(X12_topic)

# Prepare the data for prediction
X2_topic <- select(X12_topic,-c("document","cleanReview"))
X_topic <- as.data.frame(X2_topic)

# Drop one column to avoid multicolinearity
X_topic <- select(X_topic,-c("T15"))



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the dataset into training and testing sets
library(randomForest)
set.seed(123)
X_topic$overall <- as.factor(X_topic$overall) 
idx <- sample(1:nrow(X_topic), size = 0.7 * nrow(X_topic), replace = FALSE)

# Random Forest
rf_topic_model <- randomForest(overall ~ ., data = X_topic[idx,], ntree=1000, mtry=4)

# Predict the ratings for the test data using the multinomial logistic regression model
predictions_topic <- predict(rf_topic_model, newdata = X_topic[-idx,])

# Get the actual ratings from the test data
actual_ratings_topic <- X_topic[-idx,]$overall

# Compare the predicted ratings with the actual ratings
comparison_topic <- data.frame(Actual = actual_ratings_topic, Predicted = predictions_topic)

# Overall performance
sum(comparison_topic$Actual == comparison_topic$Predicted)/length(comparison_topic$Actual)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Setup Python Environment 
library(reticulate)
library(here)
use_python("/Users/ismailfaruqi/anaconda3/bin/python3")

# Retrieve/force initialization of Python
reticulate::py_config()

# Check if python is available
reticulate::py_available()


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Install Python package into virtual environment
reticulate::py_install("transformers", pip = TRUE)

# Also installing pytorch just as a contingency?
reticulate::py_install(c("torch", "sentencepiece"), pip = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Import transformers
transformers <- reticulate::import("transformers")

# Set tokenizer and model
tokenizer <- transformers$AutoTokenizer$from_pretrained("LiYuan/amazon-review-sentiment-analysis")
model <- transformers$AutoModelForSequenceClassification$from_pretrained("LiYuan/amazon-review-sentiment-analysis")

# Create the sentiment analysis pipeline
sentimentchecker <- transformers$pipeline("sentiment-analysis", model = model, tokenizer = tokenizer)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Check maximum input for the model
tokenizer$max_model_input_sizes

# Add new variable: review_length
filtered_review <- review %>% 
  mutate(review_length = nchar(cleanReview))

# Maximum and minimum length of the review
max(filtered_review$review_length, na.rm=TRUE)
min(filtered_review$review_length, na.rm=TRUE)

# Distribution of Review Length
ggplot(filtered_review, aes(review_length))+
  geom_histogram(aes(y=..density..), fill='salmon1') +
  geom_density()+ xlim(0,3500) +
  theme(legend.position = "none", axis.ticks.length=unit(-0.15, "cm"),
        axis.ticks.margin=unit(0.5, "cm"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x="Length of Review",y="No. of reviews",
     subtitle = "Distribution of length of the reviews")

# Filter the review based on the length (as BERT-based model can only process 512 tokens)
filtered_review <- filtered_review %>%
filter(review_length>5) %>%
filter(review_length<1000)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Randomly pick 5000 reviews to reduce computational load
set.seed(123)
sample_review <- filtered_review[sample(nrow(filtered_review), 5000), c("overall", "cleanReview", "review_length")]


# Calculate sentiment
sample_review$sentiment_label <- lapply(sample_review$cleanReview, function(review) {
  result <- sentimentchecker(review)[[1]]$label
})

sample_review$sentiment_score <- lapply(sample_review$cleanReview, function(review) {
  result <- sentimentchecker(review)[[1]]$score
})

# Calculate Error
sample_review$predicted_rating <-substr(sample_review$sentiment_label, 1, 1)

# Calculate performance
sample_review$overall <- as.numeric(sample_review$overall)
sample_review$predicted_rating <- as.numeric(sample_review$predicted_rating)

# Label the result of prediction
sample_review$result <- ifelse(sample_review$overall == sample_review$predicted_rating, TRUE, FALSE)

# See the average length for TRUE and FALSE answer
sample_review %>%
  group_by(result) %>%
  summarise(mean_length = mean(review_length))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Evaluate performance of Huggingface model
accuracy <- mean(sample_review$overall == sample_review$predicted_rating)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Satisfied Customer
satisfied <- review_token_clean %>% 
  filter(overall >= 4)

# Dissatisfied Customer
dissatisfied <- review_token_clean %>% 
  filter(overall <= 2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# DTM for Satisfied Customer
dtm_satisfied <- satisfied %>% count(reviewID,Hunspell_stem) %>% cast_dtm(reviewID,Hunspell_stem,n)

# Remove Sparsity
dtm_satisfied <- removeSparseTerms(dtm_satisfied,0.98)

# Remove row (review) that has all zero value
dtm_satisfied <- dtm_satisfied[rowSums(as.matrix(dtm_satisfied)) > 0, ]

# Check the updated dimensions of the matrix
dim(dtm_satisfied)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

# Identify topic with LDA. Number of Topic (k) is the optimal number from perplexity value
topic_model_satisfied <- LDA(dtm_satisfied,k = 5,method = "Gibbs")

# Examine the topic
topics_satisfied <- tidy(topic_model_satisfied, matrix = "beta")

# Get top terms for each topic
top_terms_satisfied <- topics_satisfied %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

# Plot top terms for each topic
top_terms_satisfied %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Evaluate Number of Topics
#perplexity(topic_model_satisfied,dtm_satisfied)

# Perplexity
#set.seed(123)
#topics_satisfied <- c(2:7)
#perplexity_df <- data.frame(perp_value=numeric())
#for (i in topics_satisfied){
#  fitted <- LDA(dtm_satisfied, k = i, method = "Gibbs")
#  perplexity_df[i,1] <- perplexity(topic_model_satisfied,dtm_satisfied)
#}

# Plot Perplexity
#g <- ggplot(data=perplexity_df, aes(x= as.numeric(row.names(perplexity_df)))) + labs(y="Perplexity",x="Number of topics") + ggtitle("Perplexity")
#g <- g + geom_line(aes(y=perp_value), colour="green")
#g



## ------------------------------------------------------------------------------------------------------------------------------------------------------
top_terms_satisfied %>% 
  filter(topic == 1) %>% 
  select(term) %>% 
  print()
  
top_terms_satisfied %>% 
  filter(topic == 2) %>% 
  select(term)

top_terms_satisfied%>% 
  filter(topic == 3) %>% 
  select(term)

top_terms_satisfied %>% 
  filter(topic == 4) %>% 
  select(term)

top_terms_satisfied %>% 
  filter(topic == 5) %>% 
  select(term)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate Posterior Probability
terms <- terms(topic_model_satisfied, 15)
topics_prob_satisfied <- posterior(topic_model_satisfied)$topics
colnames(topics_prob_satisfied) <- apply(terms, 2, paste, collapse = ",")
colSums(topics_prob_satisfied)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# DTM for Satisfied Customer
dtm_dissatisfied <- dissatisfied %>% count(reviewID,Hunspell_stem) %>% cast_dtm(reviewID,Hunspell_stem,n)

# Remove Sparsity
dtm_dissatisfied <- removeSparseTerms(dtm_dissatisfied,0.98)

# Remove row (review) that has all zero value
dtm_dissatisfied <- dtm_dissatisfied[rowSums(as.matrix(dtm_dissatisfied)) > 0, ]

# Check the updated dimensions of the matrix
dim(dtm_dissatisfied)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

# Identify topic with LDA. Number of Topic (k) is the optimal number from perplexity value
topic_model_dissatisfied <- LDA(dtm_dissatisfied,k = 5,method = "Gibbs")

# Examine the topic
topics_dissatisfied <- tidy(topic_model_dissatisfied, matrix = "beta")

# Get top terms for each topic
top_terms_dissatisfied <- topics_dissatisfied %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

# Plot top terms for each topic
top_terms_dissatisfied %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# # Evaluate Number of Topics
# perplexity(topic_model_dissatisfied,dtm_dissatisfied)
# 
# # Perplexity
# set.seed(123)
# topics_dissatisfied <- c(2:7)
# perplexity_df <- data.frame(perp_value=numeric())
# for (i in topics_dissatisfied){
#   fitted <- LDA(dtm_dissatisfied, k = i, method = "Gibbs")
#   perplexity_df[i,1] <- perplexity(topic_model_dissatisfied,dtm_dissatisfied)
# }
# 
# # Plot Perplexity
# g <- ggplot(data=perplexity_df, aes(x= as.numeric(row.names(perplexity_df)))) + labs(y="Perplexity",x="Number of topics") + ggtitle("Perplexity")
# g <- g + geom_line(aes(y=perp_value), colour="green")
# g



## ------------------------------------------------------------------------------------------------------------------------------------------------------
top_terms_dissatisfied %>% 
  filter(topic == 1) %>% 
  select(term) %>% 
  print()
  
top_terms_dissatisfied %>% 
  filter(topic == 2) %>% 
  select(term)

top_terms_dissatisfied%>% 
  filter(topic == 3) %>% 
  select(term)

top_terms_dissatisfied %>% 
  filter(topic == 4) %>% 
  select(term)

top_terms_dissatisfied %>% 
  filter(topic == 5) %>% 
  select(term)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate Posterior Probability
terms <- terms(topic_model_dissatisfied, 15)
topics_prob_dissatisfied <- posterior(topic_model_dissatisfied)$topics
colnames(topics_prob_dissatisfied) <- apply(terms, 2, paste, collapse = ",")
colSums(topics_prob_dissatisfied)


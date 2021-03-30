library(tidyverse)
library(lubridate) 
library(scales)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(broom)
library(reshape2)
library(visibly)
library(igraph)
library(ggraph)
library(widyr)
library(plotly)





## Get data
trump_tweets <- read_csv("tweets_11-06-2020.csv")
biden_tweets <- read_csv("JoeBidenTweets.csv")

trump_tweets <- trump_tweets %>% 
  mutate(timestamp = date, tweet = text, likes = favorites) %>%
  filter(!isRetweet) %>%
  select(id, timestamp, tweet, retweets, likes)

biden_tweets <- biden_tweets %>% 
  select(-url, -replies, -quotes)

bt_tweets <- bind_rows(trump_tweets %>%
                         mutate(person = "Trump"),
                       biden_tweets %>%
                         mutate(person = "Biden")) %>%
  mutate(timestamp = ymd_hms(timestamp))

summary(bt_tweets)

## Tweet distribution

group.colors <- c(Trump = colors()[552], Biden = colors()[26])

bt_tweets %>% ggplot(aes(timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 30, show.legend = FALSE) +
  geom_vline(xintercept = c(as.POSIXct("2015-06-16", tz = "UTC"), 
                            as.POSIXct("2019-04-25", tz = "UTC"))) +
  scale_fill_manual(values = group.colors)+
  facet_wrap(~person, ncol = 1)

## Word frequency 

remove_reg <- "&amp;|&lt;|&gt;" # to remove & < >
bt_tweets_tidy<- bt_tweets %>%
  filter(!str_detect(tweet, "^RT")) %>% # to remove retweet
  mutate(text = str_remove_all(tweet, remove_reg),
         text = str_remove_all(tweet, 
                               "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)")) %>% # to remove http links
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word, # to remove stop words 
         !word %in% str_remove_all(stop_words$word, "'"), # to remove ' in word
         !word %in% c("amp", "lt", "gt"), # remove amp/lt/gt in word
         str_detect(word, "[a-z]")) 

frequ <- bt_tweets_tidy %>%
  group_by(person) %>%
  count(word, sort = TRUE) %>%
  left_join(bt_tweets_tidy %>%
              group_by(person) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)
frequ

frequ <- frequ %>% select(person, word, freq) %>%
  spread(person, freq) %>%
  arrange(Biden, Trump)

frequ %>% ggplot(aes(Biden, Trump)) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

bt_tweets_tidy_campaign <- bt_tweets_tidy %>% filter(timestamp >= as.Date("2019-04-25"))
frequ_2019 <- bt_tweets_tidy_campaign %>%
  group_by(person) %>%
  count(word, sort = TRUE) %>%
  left_join(bt_tweets_tidy %>%
              group_by(person) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequ_2019 <- frequ_2019 %>% select(person, word, freq) %>%
  spread(person, freq) %>%
  arrange(Biden, Trump)

frequ_2019 %>% ggplot(aes(Biden, Trump)) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

## Wordcloud 

#### 1. Total word counts  
bt_tweets_tidy %>%
  group_by(word) %>%
  filter(!str_detect(word, "^@")) %>%
  count() %>%
  with(wordcloud(word, n, min.freq = 200, max.word = 400, 
                 rot.per =0.35, random.order = FALSE,
                 colors = brewer.pal(12, "Paired"), scale = c(3,1)))

#### 2. Hashtags 

bt_tweets_tidy %>%
  filter(person == "Trump") %>%
  group_by(word) %>%
  filter(str_detect(word, "#")) %>%
  count() %>%
  with(wordcloud(word,n, min.freq = 1, max.word = 300, 
                 rot.per =0.35, random.order = FALSE,
                 colors = brewer.pal(12, "Paired"), scale = c(3,0.8)))

bt_tweets_tidy %>%
  filter(person == "Biden") %>%
  group_by(word) %>%
  filter(str_detect(word, "#")) %>%
  count() %>%
  with(wordcloud(word,n, min.freq = 1, max.word = 300, 
                 rot.per =0.35, random.order = FALSE,
                 colors = brewer.pal(12, "Paired"), scale = c(3,0.8)))

#### 3. Separation in sentiments

# trump words
bt_tweets_tidy %>%
  filter(person == "Trump" & !word == "trump") %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill =0) %>%
  comparison.cloud(colors = brewer.pal(10, "Paired"), max.words = 400, 
                   match.colors = TRUE, title.size = 1.5, 
                   random.order = FALSE, scale = c(3,0.5))

# biden words 
bt_tweets_tidy %>%
  filter(person == "Biden" & !word == "trump") %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill =0) %>%
  comparison.cloud(colors = brewer.pal(10, "Paired"), max.words = 400, 
                   match.colors = TRUE, title.size = 1.5, 
                   random.order = FALSE, scale = c(3,0.5))


## Odds ratio

bt_words_ratio <- bt_tweets_tidy_campaign %>%
  filter(!str_detect(word, "^@")) %>% # remove tags on individual accounts 
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>% # only keep the words that are used more than 10 times 
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Biden / Trump)) %>%
  arrange(desc(logratio))

# Here it's just a demonstration of how to calculate the odds ratio manunally.
bt_tweets_tidy_campaign %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>% 
  # use ungroup here is because if use summarise the data would automatically reduce the dimension
  spread(person, n, fill = 0) %>%
  mutate(biden_t = sum(Biden),
         trump_t = sum(Trump),
         br = (Biden+1)/(biden_t+1), 
         tr = (Trump+1)/(trump_t+1), 
         logr = log(br/tr)) %>%
  arrange(abs(logr))

## the words more likely from the otherside 
group_colors <- c("FALSE" = colors()[26], "TRUE" = colors()[552]) 

## the words more likely from the otherside 
tratio <- bt_words_ratio %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  filter(logratio<0)

bratio <- bt_words_ratio %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  filter(logratio>0)  

subplot(plot_ly(bratio, x=~logratio, y=~word, type = "bar", name = "Biden", color = I("blue")),
        plot_ly(tratio, x=~logratio, y=~word, type = "bar", name = "Trump", color = I("red"), yaxis="y2"),
        shareX = TRUE) %>%
  layout(legend = list(x = 100, y = 0.5),
         title = "log odds ratio (Biden/Trump)",
         width = 800) %>%
  theme_plotly()

## Retweets and likes

#### 1. Retweets

bt_totals <- bt_tweets_tidy %>%
  group_by(person, id) %>%
  summarise(rts = first(retweets)) %>%
  group_by(person) %>%
  summarise(total_rts = sum(rts))

bt_totals

bt_word_by_rts <- bt_tweets_tidy %>%
  group_by(id, person, word) %>%
  summarise(rts = first(retweets)) %>%
  group_by(person, word) %>%
  summarise(retweets = median(rts), uses =n()) %>%
  left_join(bt_totals) %>%
  filter(retweets != 0) %>%
  ungroup()

bt_word_by_rts

group.colors <- c(Trump = colors()[552], Biden = colors()[26])

sub_rtt <- bt_word_by_rts %>%
  filter(uses >= 5 &
           person == "Trump") %>%
  group_by(person) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup()

sub_rtb <- bt_word_by_rts %>%
  filter(uses >= 5 &
           person == "Biden") %>%
  group_by(person) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup()

subplot(sub_rtt %>%
          plot_ly(x=~retweets, y=~word, type = "bar", color = I("red"), name = "Trump"),
        sub_rtb %>%
          plot_ly(x=~retweets, y=~word, type = "bar", color =I("blue"), name = "Biden")) %>%
  layout(title = "Median number of retweets",
         legend = list(x = 100, y = 0.5),
         width = 850) %>%
  theme_plotly()

#### 2. Likes

bt_totals_fav <- bt_tweets_tidy %>%
  group_by(person, id) %>%
  summarise(fav = first(likes)) %>%
  group_by(person) %>%
  summarise(total_favs = sum(fav))

bt_totals_fav

bt_word_by_fav <- bt_tweets_tidy %>%
  group_by(id, person, word) %>%
  summarise(fav = first(likes)) %>%
  group_by(person, word) %>%
  summarise(favs = median(fav), uses =n()) %>%
  left_join(bt_totals_fav) %>%
  filter(favs != 0) %>%
  ungroup()

bt_word_by_fav

sub_trump <- bt_word_by_fav %>%
  filter(uses >= 5 &
           person == "Trump") %>%
  group_by(person) %>%
  top_n(10, favs) %>%
  arrange(favs) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup()

sub_biden <- bt_word_by_fav %>%
  filter(uses >= 5 &
           person == "Biden") %>%
  group_by(person) %>%
  top_n(10, favs) %>%
  arrange(favs) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup()

subplot(sub_trump %>%
          plot_ly(x=~favs, y=~word, type = "bar", color = I("red"), name = "Trump"),# %>%
        #layout(legend = list(title = "Trump's ")),
        sub_biden %>%
          plot_ly(x=~favs, y=~word, type = "bar", color =I("blue"), name = "Biden")) %>%
  layout(title = "Median number of likes",
         legend = list(x = 100, y = 0.5),
         width = 850) %>%
  theme_plotly()

## Changes in word use

#### 1. Data prepping

bt_words_by_time <- bt_tweets_tidy_campaign %>%
  filter(!str_detect(word, "^@")) %>% # to filter out account taging in Twitter
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 50)

#### 2. Nested Model

bt_nested_data <- bt_words_by_time %>%
  group_by(person, word) %>%
  nest()

bt_nested_data

model <- function(x){
  glm(cbind(count, time_total) ~ time_floor, data = x, family = "binomial")
}

# map(bt_nested_data$data, model), I forgot how to use map() so put it here for reference 

bt_nested_models <- bt_nested_data %>%
  mutate(models = map(data, model))

bt_nested_models
### different way to run glm over nested data
# bt_nested_models <- bt_nested_data %>%
#  mutate(models = map(data, ~glm(cbind(count, time_total) ~ time_floor, 
#                      ., family = "binomial")))
###

slopes <- bt_nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% filter(adjusted.p.value <0.05)


#### 3. Graphing

bt_words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "Trump" &
           word_total > 350) %>%
  plot_ly() %>%
  add_lines(x=~time_floor, y=~count/time_total, color = ~word, mode = "line", type = "scatter") %>%
  layout(
    title = "Trump's word trends",
    xaxis = list(title="Time"),
    yaxis = list(title="Frequency"),
    legend= list(title=list(text='<b> Word </b>'),
                 x = 100, y = 0.5),
    width = 800
  ) %>%
  theme_plotly()

bt_words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "Biden" &
           word_total > 350) %>%
  plot_ly() %>%
  add_lines(x=~time_floor, y=~count/time_total, color = ~word, mode = "line", type = "scatter") %>%
  layout(
    title = "Biden's word trends",
    xaxis = list(title="Time"),
    yaxis = list(title="Frequency"),
    legend= list(title=list(text='<b> Word </b>'),
                 x = 100, y = 0.5),
    width = 800
  ) %>%
  theme_plotly()
## Whole lotta sentiments

#### 1.Data prepping
custom_stop_words <- bind_rows(tibble(word = c("trump"), 
                                      lexicon = c("custom")), stop_words)

bt_tweets_tidy<- bt_tweets %>%
  arrange(timestamp) %>%
  mutate(tweetnumber = row_number()) %>%
  filter(!str_detect(tweet, "^RT")) %>%
  mutate(text = str_remove_all(tweet, remove_reg),
         text = str_remove_all(tweet, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% custom_stop_words$word,
         !word %in% str_remove_all(custom_stop_words$word, "'"),
         !word %in% c("amp", "lt", "gt"),
         str_detect(word, "[a-z]"))

bt_afinn <- bt_tweets_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(person, index = tweetnumber %/% 150) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bt_bing_and_nrc <- bind_rows(
  bt_tweets_tidy %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing"),
  bt_tweets_tidy %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(person, method, index = tweetnumber %/% 150, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bt_all_sent <- bind_rows(bt_afinn, bt_bing_and_nrc)

#### 2. Change of sentiment over time 
bt_all_sent %>% filter(person == "Biden") %>% 
  ggplot(aes(index, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

bt_all_sent %>% filter(person == "Trump") %>% 
  ggplot(aes(index, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# checking the difference of NRC and Bing 
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)
get_sentiments("bing") %>% 
  count(sentiment)

#### 3. Overall cumulative sentiments

bt_tweets_sent <- bt_tweets_tidy %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(tweetnumber, timestamp, person, word) %>% 
  summarise(value) %>%
  ungroup()

tb <- list(
  tickfont = list(color = "#2ca02c40"),
  overlaying = "y",
  side = "right",
  title = "sentiment difference",
  titlefont = list(textangle=45),
  zeroline = F
)

bt_tweets_sent %>% 
  filter(person == "Trump" &
           !word %in% "trump" &
           timestamp >= "2015-06-16") %>%
  mutate(positivity = cumsum(if_else(value>0, value, 0)),
         negativity = cumsum(abs(if_else(value<0, value, 0)))) %>% 
  plot_ly() %>% 
  add_lines(x=~tweetnumber, y=~positivity, name='positive') %>% 
  add_lines(x=~tweetnumber, y=~negativity, name='negative', color = I("red")) %>%
  add_lines(x=~tweetnumber, y=~positivity - negativity, name="difference", 
            yaxis="y2", color = I("#ff8400")) %>% 
  layout(
    title = "Trump's overall cumulative sentiment",
    yaxis = list(title='absolute cumulative sentiment'),
    yaxis2 = tb,
    width = 800
  ) %>% 
  theme_plotly()

bt_tweets_sent %>% 
  filter(person == "Biden" &
           !word %in% "trump" &
           timestamp >= "2019-04-25") %>%
  mutate(positivity = cumsum(if_else(value>0, value, 0)),
         negativity = cumsum(abs(if_else(value<0, value, 0)))) %>% 
  plot_ly() %>% 
  add_lines(x=~tweetnumber, y=~positivity, name='positive') %>% 
  add_lines(x=~tweetnumber, y=~negativity, name='negative', color = I("red")) %>%
  add_lines(x=~tweetnumber, y=~positivity - negativity, name="difference", 
            yaxis="y2", color = I("#ff8400")) %>% 
  layout(
    title = "Biden's overall cumulative sentiment",
    yaxis = list(title='absolute cumulative sentiment'),
    yaxis2 = tb,
    width = 800
  ) %>% 
  theme_plotly()

#### 4. What words contribute to sentiments?

wordcount_t <- bt_tweets_tidy %>%
  filter(timestamp >= "2015-06-16") %>%
  inner_join(get_sentiments("bing")) %>%
  count(person, word, sentiment, sort = TRUE) %>%
  ungroup()

wordcount_b <- bt_tweets_tidy %>%
  filter(timestamp >= "2019-04-25") %>%
  inner_join(get_sentiments("bing")) %>%
  count(person, word, sentiment, sort = TRUE) %>%
  ungroup()
###### Trump
wordcount_t_p <- wordcount_t %>%
  filter(person== "Trump" &
           !word %in%custom_stop_words$word &
           sentiment == "positive") %>%
  top_n(10) %>%
  ungroup()


wordcount_t_n <- wordcount_t %>%
  filter(person== "Trump" &
           !word %in%custom_stop_words$word &
           sentiment == "negative") %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup()

###### Biden
wordcount_b_p <- wordcount_b %>%
  filter(person== "Biden" &
           !word %in%custom_stop_words$word &
           sentiment == "positive") %>%
  top_n(10) %>%
  ungroup()

wordcount_b_n <- wordcount_b %>%
  filter(person== "Biden" &
           !word %in%custom_stop_words$word &
           sentiment == "negative") %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup()

### Trump
subplot(plot_ly(wordcount_t_n, x=~n, y=~word, type = "bar", name = "Negative", color = I("brown4"), width = 800),
        plot_ly(wordcount_t_p, x=~n, y=~word, type = "bar", name = "Positive", color = I("dodgerblue"), yaxis="y2", width = 800),
        shareX = TRUE) %>%
  layout(legend = list(x = 100, y = 0.5),
         title = "Trump's word contribution") %>%
  theme_plotly()
### Biden
subplot(plot_ly(wordcount_b_n, x=~n, y=~word, type = "bar", name = "Negative", color = I("brown4"), width = 800),
        plot_ly(wordcount_b_p, x=~n, y=~word, type = "bar", name = "Positive", color = I("dodgerblue"), yaxis="y2", width = 800),
        shareX = TRUE) %>%
  layout(legend = list(x = 100, y = 0.5),
         title = "Biden's word contribution") %>%
  theme_plotly()


# ggplotly(wordcount_t %>%
#            filter(person== "Trump" &
#                     !word %in%custom_stop_words$word) %>%
#            group_by(sentiment) %>%
#            top_n(10) %>%
#            ungroup() %>%
#            mutate(word = reorder(word, n)) %>%
#            ggplot(aes(n, word, fill = sentiment)) +
#            geom_col(show.legend = FALSE) +
#            facet_wrap(~sentiment, scales = "free_y") +
#            labs(title = "Trump",
#                 x = "Contribution to sentiment",
#                 y = NULL)) %>%
#   theme_plotly()
# 
# ggplotly(wordcount_b %>%
#            filter(person== "Biden" &
#                     !word %in%custom_stop_words$word) %>%
#            group_by(sentiment) %>%
#            top_n(10) %>%
#            ungroup() %>%
#            mutate(word = reorder(word, n)) %>%
#            ggplot(aes(n, word, fill = sentiment)) +
#            geom_col(show.legend = FALSE) +
#            facet_wrap(~sentiment, scales = "free_y") +
#            labs(title = "Biden",
#                 x = "Contribution to sentiment",
#                 y = NULL)) %>%
#   theme_plotly()
#wordcount_t %>%
#           filter(person== "Trump" &
#                    !word %in%custom_stop_words$word) %>%
#           group_by(sentiment) %>%
#           top_n(10) %>%
#           ungroup() %>%
#           mutate(word = reorder(word, n)) %>%
#           ggplot(aes(n, word, fill = sentiment)) +
#           geom_col(show.legend = FALSE) +
#           facet_wrap(~sentiment, scales = "free_y") +
#           labs(title = "Trump",
#                x = "Number of sentiment",
#                y = NULL) 

#wordcount_b %>%
#           filter(person== "Biden" &
#                    !word %in%custom_stop_words$word) %>%
#           group_by(sentiment) %>%
#           top_n(10) %>%
#           ungroup() %>%
#           mutate(word = reorder(word, n)) %>%
#           ggplot(aes(n, word, fill = sentiment)) +
#           geom_col(show.legend = FALSE) +
#           facet_wrap(~sentiment, scales = "free_y") +
#          labs(title = "Biden",
#                x = "Number of sentiment",
#               y = NULL)

## Connecting words 

#### 1. Ngrams

bt_bigrams <- bt_tweets %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2) %>%
  filter(!str_detect(bigram, "http") &
           !str_detect(bigram, "www.") &
           !str_detect(bigram, ".com") &
           !str_detect(bigram, ".org$") &
           !str_detect(bigram, "^t.co") &
           !str_detect(bigram, "\\d") &
           !str_detect(bigram, "^amp"))

bigrams_sep <- bt_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filt <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_graph_t <- bigrams_filt %>%
  filter(!word2 == "amp" &
           timestamp >= "2015-06-16" &
           person == "Trump") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigrams_graph_b <- bigrams_filt %>%
  filter(!word2 == "amp" &
           timestamp >= "2019-04-25" &
           person == "Biden") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 20) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigrams_graph_t, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "red", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void() +
  ggtitle("Trump")

ggraph(bigrams_graph_b, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "blue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  ggtitle("Biden")

#### 2. Correlation between words

bt_tweets_tidy<- bt_tweets %>%
  arrange(timestamp) %>%
  mutate(sec_by100 = row_number()%/%100) %>%
  filter(!str_detect(tweet, "^RT") &
           sec_by100 > 0) %>%
  mutate(text = str_remove_all(tweet, remove_reg),
         text = str_remove_all(tweet, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% custom_stop_words$word,
         !word %in% str_remove_all(custom_stop_words$word, "'"),
         !word %in% c("amp", "lt", "gt"),
         str_detect(word, "[a-z]") &
           !str_detect(word, "http") &
           !str_detect(word, "@") &
           !str_detect(word, "cont") &
           !str_detect(word, "www.") &
           !str_detect(word, ".com$") &
           !str_detect(word, ".org$") &
           !str_detect(word, "^tco") &
           !str_detect(word, "\\d") & 
           !str_detect(word, "^amp") &
           !str_detect(word, "realdonald"))

word_cors_t <- bt_tweets_tidy %>%
  filter(timestamp >= "2015-06-16" &
           person == "Trump") %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, sec_by100, sort = TRUE)

word_cors_b <- bt_tweets_tidy %>%
  filter(timestamp >= "2019-04-25" &
           person == "Biden") %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, sec_by100, sort = TRUE)

word_cors_t %>%
  filter(correlation > .50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "red", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  ggtitle("Trump")

word_cors_b %>%
  filter(correlation > .50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "blue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  ggtitle("Biden")

#### logistic regression + lasso regularization 
bt_tweets <- bt_tweets %>% mutate(id = row_number())
bt_text <- bt_tweets %>% mutate(text = str_remove_all(tweet, remove_reg),
                                text = str_remove_all(tweet, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
                                person = as.factor(if_else(person=="Biden", "Biden", "Trump"))) %>%
  filter(!text =="") %>% 
  select(-timestamp, tweet, -retweets, -likes) %>%
  unnest_tokens(word, text, token = "tweets") %>% 
  filter(!word %in% stop_words$word & !word %in% c("amp", "lt", "gt"))

text_10 <- bt_text %>% 
  group_by(word) %>% 
  filter(n() >=10) %>%
  ungroup()

## building the model 
library(rsample)
tweets_split <- bt_tweets %>%
  select(id) %>%
  initial_split()

train_data <- training(tweets_split)
test_data <- testing(tweets_split)

text_10 %>%
  count(id, word) %>%
  inner_join(train_data)

words_spar <- text_10 %>%
  count(id, word) %>%
  inner_join(train_data) %>%
  cast_sparse(id, word, n)

words_rown <- as.integer(rownames(words_spar))

tweets_joined <- tibble(id = words_rown) %>%
  left_join(bt_tweets %>%
              select(id, person))

library(glmnet)
library(doMC)
registerDoMC(cores = 8) # parallel processing

is_biden <- tweets_joined$person == "Biden"
logist_model <- cv.glmnet(words_spar, is_biden,
                          family = "binomial",
                          parallel = TRUE, keep = TRUE
)
length(is_biden)
length(words_spar)

plot(logist_model)
log(logist_model$lambda.min)
plot(logist_model$glmnet.fit)

library(broom)

logist_coefs <- logist_model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == logist_model$lambda.1se)

p_1 <- logist_coefs %>%
  group_by(estimate > 0) %>%
  top_n(15, abs(estimate)) %>%
  ungroup() %>%
  filter(estimate > 0)

p_2 <- logist_coefs %>%
  group_by(estimate > 0) %>%
  top_n(15, abs(estimate)) %>%
  ungroup() %>%
  filter(estimate < 0)

# fig_1 <- plot_ly(p_1, x = ~estimate, y = ~fct_reorder(term, estimate), type = "bar", 
#                color = I("blue"), name = "Increase")
# fig_2 <- plot_ly(p_2, x = ~estimate, y = ~fct_reorder(term, estimate), type = "bar", 
#                  color = I("red"), name = "Decrease")

subplot(plot_ly(p_1, x = ~estimate, y = ~fct_reorder(term, estimate), type = "bar", 
                color = I("blue"), name = "Increase", width = 800),
        plot_ly(p_2, x = ~estimate, y = ~fct_reorder(term, estimate), type = "bar", 
                color = I("red"), name = "Decrease", yaxis = "y2", width = 800), shareX = TRUE) %>%
  layout(legend = list(x = 100, y = 0.5),
         title = "Biden that you?") %>%
  theme_plotly()




# logist_coefs %>%
#   group_by(estimate > 0) %>%
#   top_n(15, abs(estimate)) %>%
#   ungroup() %>%
#   ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
#   geom_col(alpha = 0.5, show.legend = FALSE) +
#   coord_flip() +
#   labs(
#     x = "Words",
#     title = "The coefs"
#   ) + theme_minimal()

int <- logist_coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

logist_class <- text_10 %>%
  inner_join(test_data) %>%
  inner_join(logist_coefs, by = c("word" = "term")) %>%
  group_by(id) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(int + score))

logist_class

# model metric
library(yardstick)

logist_metrics <- logist_class %>%
  left_join(bt_tweets %>%
              select(person, id), by = "id") %>%
  mutate(person = as.factor(person))

logist_metrics %>%
  roc_curve(person, probability) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(
    color = "blue",
    size = 2
  ) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "black",
    size = 1.2
  ) +
  labs(
    title = "Was it Biden or Trump",
    subtitle = "ROC curve for text classification, predicting which text was from Biden or Trump"
  ) + theme_minimal()

# AUC estimates 
logist_metrics %>%
  roc_auc(person, probability)

# confusion metric
logist_metrics %>%
  mutate(
    prediction = case_when(
      probability > 0.5 ~ "Biden",
      TRUE ~ "Trump"
    ),
    prediction = as.factor(prediction)
  ) %>%
  conf_mat(person, prediction)

# retweets + likes 
library(caret)
train_index <- createDataPartition(bt_tweets$person, p = .8, list = FALSE)
training_bt <- bt_tweets[train_index,]
testing_bt  <- bt_tweets[-train_index,]

## Naive Bayes
nb_bt <- train(person ~ likes + retweets,
               data = training_bt,
               method = "naive_bayes",
               trControl = trainControl(method = "none"),
               tuneGrid = data.frame(laplace = 0,
                                     usekernel = FALSE,
                                     adjust = FALSE))
nb_predict <- predict(nb_bt,
                      newdata = testing_bt)
nb_confm <- confusionMatrix(nb_predict, as.factor(bt_tweets[-train_index, ]$person))
nb_confm
## LogitBoost
library(caTools)
logitboost_bt <- train(person ~ likes + retweets,
                       data = training_bt,
                       method = "LogitBoost",
                       trControl = trainControl(method = "none"))

logitboost_predict <- predict(logitboost_bt,
                              newdata = testing_bt)

logitboost_confm <- confusionMatrix(logitboost_predict, as.factor(bt_tweets[-train_index, ]$person))
logitboost_confm
## Support Vector Machine 
svm_bt <- train(person ~ likes + retweets,
                data = training_bt,
                method = "svmLinearWeights2",
                trControl = trainControl(method = "none"),
                tuneGrid = data.frame(cost = 1, 
                                      Loss = 0, 
                                      weight = 1))
svm_predict <- predict(svm_bt,
                       newdata = testing_bt)
svm_confm <- confusionMatrix(svm_predict, as.factor(bt_tweets[-train_index, ]$person))
svm_confm
## Random Forrest 
rf_bt <- train(person ~ likes + retweets,
               data = training_bt,
               method = "ranger",
               trControl = trainControl(method = "none"),
               tuneGrid = data.frame(mtry = floor(sqrt(dim(train_index)[2])),
                                     splitrule = "gini",
                                     min.node.size = 1))

rf_predict <- predict(rf_bt,
                      newdata = testing_bt)

rf_confm <- confusionMatrix(rf_predict, as.factor(bt_tweets[-train_index, ]$person))
rf_confm

model_results <- bind_rows(
  nb_confm$overall,
  logitboost_confm$overall,
  svm_confm$overall, 
  rf_confm$overall,
) %>%
  as.data.frame() %>%
  mutate(model = c("Naive-Bayes", "LogitBoost", "SVM", "Random forest"))

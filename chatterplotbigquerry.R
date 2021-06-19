library(bigrquery)

library(tidyverse)

project <- "YOUR_PROJECT"          # put your project ID here !!

# get row count of full bigquery table
sql <- "SELECT COUNT(*)
        FROM `bigquery-public-data.hacker_news.comments`"

query_exec(sql, project = project, 
           use_legacy_sql = F)

## 0 bytes processed
##       f0_
## 1 8399417

# randomly sample 0.1% of 8 million+ records
sql <- "SELECT *
        FROM `bigquery-public-data.hacker_news.comments`
        WHERE RAND() <= .001"

hn_comments <- query_exec(sql, project = project, 
                          use_legacy_sql = F, 
                          max_pages = Inf)

## 3.4 gigabytes processed

# inspect size & data types of results
hn_comments %>% str(nchar.max = 20)

## 'data.frame': 8294 obs. of  10 variables:
## $ id     : int  51430| __truncated__ ...
## $ by     : chr  "kvb" "icey" "iofj" "tghw" ...
## $ author : chr  "kvb" "icey" "iofj" "tghw" ...
## $ time   : int  13595| __truncated__ ...
## $ time_ts: POSIXct, format: "201"| ...
## $ text   : chr  "Yep"| __truncated__ ...
## $ parent : int  51427| __truncated__ ...
## $ deleted: logi  NA NA NA NA NA NA ...
## $ dead   : logi  NA NA NA NA NA NA ...
## $ ranking: int  0 0 0 0 0 0 0 0 0 0 ...

# print first full text comment
hn_comments$text[1]

## [1] "I do think that both approaches are legitimate, but conflating engineering with mathematics just causes unnecessary confusion, and feeds pointless Internet flamewars over semantics."


# plot histogram of rankings & log of rankings
hist(hn_comments$ranking)
hist(log(hn_comments$ranking))


# But first, we’ll need to preprocess the natural language text data, as prescribed in Tidy Text Mining:

library(tidytext)

# tokenize text at the single word (aka unigram) level
hn_word_tokens <- hn_comments %>% unnest_tokens(word, input = text)

# remove stop words (e.g. 'a', 'the', 'and')
hn_word_tokens_no_stop <- hn_word_tokens %>% anti_join(get_stopwords())

# create word counts
hn_word_counts <- hn_word_tokens_no_stop %>% count(word, sort = T)

# print top 10 most frequent words
hn_word_counts %>% head(10)

# compute avg comment ranking of each word
hn_word_avg_rank <- hn_word_tokens_no_stop %>%
  group_by(word) %>%
  summarize(avg_rank = mean(ranking))
# filter word counts & join to average ranks
hn_counts_and_ranks <- hn_word_counts %>% filter(n < 2000 ) %>% 
  left_join(hn_word_avg_rank, by = "word")

#Now we’re ready to select the top 100 words & construct our chatterplot!

library(ggplot2)
library(ggrepel)

# select the top 100 words by n (aka word count)
hn_counts_and_ranks %>% top_n(100, wt = n) %>%
  
  # construct ggplot
  ggplot(aes(avg_rank, n, label = word)) +
  
  # ggrepel geom, make arrows transparent, color by rank, size by n
  geom_text_repel(segment.alpha = 0, 
                  aes(colour=avg_rank, size=n)) + 
  
  # set color gradient,log transform & customize legend
  scale_color_gradient(low="green3", high="violetred", 
                       trans = "log10",
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position ="top")) +
  # set word size range & turn off legend
  scale_size_continuous(range = c(3, 10),
                        guide = FALSE) +
  scale_x_log10() +     # use log-scale for x-axis
  ggtitle(paste0("Top 100 words from ",
                 nrow(hn_comments),   # dynamically include row count
                 " Hacker News article comments, by frequency"),
          subtitle = "word frequency (size) ~ avg comment ranking (color)") + 
  labs(y = "Word frequency", x = "Avg rank (log scale)") +
  
  # minimal theme & customizations
  theme_minimal() +
  theme(legend.position=c(.99, .99),
        legend.justification = c("right","top"),
        panel.grid.major = element_line(colour = "whitesmoke"))


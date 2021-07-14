# https://www.r-bloggers.com/awesome-twitter-word-clouds-in-r/
####################################################
#install.packages("rtweet")
#install.packages("tidytext")
#install.packages("wordcloud2")

library(tidyverse)
library(lubridate)
library(tidytext)
library(stringr)
library(rtweet)
library(wordcloud2)
library(googlesheets4)


########## loco failure word cloud

ss = "https://docs.google.com/spreadsheets/d/1nCyg0R7w90eRO0RzqeMjFroLXIxPE_W47jk4QEOCuuQ/edit#gid=258913452"
lf16 <- googlesheets4::read_sheet(ss, sheet =1,   range = NULL, col_names = TRUE, col_types = NULL,
                                  na = "",    trim_ws = TRUE,  skip = 0,
                                  n_max = Inf,      # guess_max = min(1000, n_max),
                                  .name_repair = "unique" )


mywords <-   select(lf16, `Output File Formats`)  %>%   gather(document, word)
mywords
mytable <- mywords   %>%  unnest_tokens( mywordlist, word )   # worked - mywordlist !!!
mytable
data(stop_words)      # data of common stop words like "the", "of" #View(stop_words)            

myTable  <-  mytable  %>%  anti_join( stop_words,  by = c("mywordlist" = "word")  ) 

myTable               # my master list of 1 lakhs words !!

mywordtable <- myTable %>%  count(mywordlist, sort = TRUE )  %>% filter( !is.na(mywordlist) )


myfinalTable  <-  mywordtable           %>%       #Remove other nonsense words
                  filter( !mywordlist   %in% 
                                        c('due',  'dep', 'rep' , 't.co', 'https', 
                                           'handmaidstale',  'watch' )   )

wordcloud2( myfinalTable, size=0.7, 
                         # shape = 'star'
                         shape	= 'circle'   # 'cardioid', 'diamond'  'triangle-forward', 
                                             # 'triangle', 'pentagon', and 'star'.
                         # shape	=  'cardioid',
                         # color = "random-light", 
                         # color = colorVec ??
                         # backgroundColor = "grey",
                         # minRotation = -pi/2, maxRotation = -pi/2,
                         # fontWeight = "bold"
                         # rotateRatio = 0.9
                   )  


#######################################################

url <- "https://raw.github.com/VladAluas/Text_Analysis/master/Datasets/Text_review.csv"
# I prefer vroom to ingest csv, but you can use readr::read_csv() if you fancy it more
library(vroom)
reviews <- vroom::vroom(url)
reviews

library(tidytext)
library(tidyverse)
reviews %>% unnest_tokens("Word", "Text")  # column name to be created (Word) and  source column (Text)
# function took all the sentences from the Text column and broke them down into a format that has one word per row
# transformed all the words to lower case and removed all the special symbols (e.g.  $ 

# create a function, word_frequency() that contains all the steps we want to apply to the graph. We will also replace some characters, so we will not double or under count some words.

reviews_tidy <- reviews %>%  unnest_tokens("Word", "Text") %>%
                mutate(Word = str_replace(Word, "'s", ""))  # We also want to prevent the analysis in showing 6t and 6t's as two separate words

# to display graphically a word frequency plot, create a function that will store all the operations we will repeat several times
word_frequency <- function(x, top = 10){
        x %>%
        count(Word, sort = TRUE) %>%           # We need a word count
        # We want to create a factor from the word column with the levels showing the most frequent words as top level
        # This is just for aestethic reasons, however, it helps make the point
        mutate(Word = factor(Word, levels = rev(unique(Word)))) %>% 
        # We use the "top" variable defined in the function so we can decide how many words we want to use 
        top_n(top) %>%  ungroup() %>%    # useful later to use a grouping variable and will do nothing if we don't  
       
        ggplot(mapping = aes(x = Word, y = n)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = NULL)
     }  # end function

reviews_tidy %>% word_frequency() # n = 10 default
reviews_tidy %>% word_frequency(8) #> Selecting by n


# with anti join common stop words in Same dataset as before with an extra code line
reviews_tidy <- reviews %>%
    unnest_tokens("Word", "Text") %>%
    anti_join(stop_words, by = c("Word" = "word")) %>%  # anti_join just keeps the rows common to both data sets
    mutate(Word = str_replace(Word, "'s", ""))  %>% #filter( !mywordlist   %in%  c('due',  'dep', 'rep' , 't.co', 'https',  'handmaidstale',  'watch' )   )
# The graph is the same as before, we just changed the dataset
reviews_tidy %>% word_frequency(20)  #> Selecting by n

reviews_tidy %>%
    group_by(Model) %>% 
    word_frequency(5) +
    facet_wrap(~ Model, scales = "free_y") # This is just to split the graph into multiple graphs for each model
#> Selecting by n

# comparison in models
#  simple formula bind_tf_idf() that assigns weights to words using the principles below:
# words with high frequency in all the documents: low weight
# words with high frequency in just one of the documents and not the other: high weight
# words with low frequency across the board: low weight

review_tf_idf <-  reviews_tidy %>%  
                  count(Model, Word, sort = TRUE) %>%
                  bind_tf_idf(Word, Model, n)

review_tf_idf %>%
    arrange(desc(tf_idf))

review_tf_idf %>%
    arrange(desc(tf_idf)) %>%     # sort the data in descending order to create the factors for each term
    mutate(Word = factor(Word, levels = rev(unique(Word))) ) %>%  # create factors 
    group_by(Model) %>%
    top_n(5) %>%      # Select just the top 5 words for each model
    ungroup() %>%
    # Our Plot
    ggplot(mapping = aes(x = Word, y = tf_idf, fill = Model)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = NULL) +
    coord_flip() +
    facet_wrap(~ Model, scales = "free_y")

## Sentiment Analysis

# the most basic method, and the one that we will cover today, is to simply associate each word in the review to a sentiment.
# simpy count positive and negative words

# straight forward and the tidyverse package comes to our aid with some libraries that already have these associations made and are also validated against multiple sources.
library(textdata)
get_sentiments("afinn") # textdata required to download the AFINN lexicon. 
# AFFIN from Finn Årup Nielsen, gives score between -5 and +5 to each word.
get_sentiments("bing") # works # bing from Bing Liu and collaborators
get_sentiments("nrc") # textdata required, # nrc from Saif Mohammad and Peter Turney
# nrc classified either as fear, negative or sadness. This is useful if you want to check for a specific sentiment, 


conclusion_afinn <- reviews %>%
    filter(str_detect(Segment, "Conclusion")) %>%
    unnest_tokens("Word", "Text") %>%
    anti_join(stop_words, by = c("Word" = "word")) %>%
    # We will get the sentiments with a inner_join since the words that don't have a match, don't have a score value
    inner_join(get_sentiments("afinn"), by = c("Word" = "word"))
# each token has been unnested, and assigned a sentiment value.

conclusion_afinn %>%
    group_by(Model) %>%
    summarise(Score = sum(value)) %>%
    arrange(desc(Score)) %>%
    mutate(Model = factor(Model, levels = rev(unique(Model)))) %>%
    ggplot(mapping = aes(x = Model, y = Score)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL)
# which model has the most positive and negative reviews
conclusion_bing <- reviews %>%
    filter(str_detect(Segment, "Conclusion")) %>%
    unnest_tokens("Word", "Text") %>%
    anti_join(stop_words, by = c("Word" = "word")) %>%
    inner_join(get_sentiments("bing"), by = c("Word" = "word"))
conclusion_bing

# just add the sentiment to the grouping.

conclusion_bing %>%
    group_by(Model, sentiment) %>%
    count() %>%
    ungroup() %>%
    mutate(Model = reorder(Model, n)) %>%
    ggplot(mapping = aes(x = Model, y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = NULL, y = "Negative vs positive sentiment / Model") +
    facet_wrap(~ sentiment, ncol = 2)

# word cloud to communicate the prevalence of a word in a text or speech. 
#install.packages("wordcloud")
library(wordcloud) #> Loading required package: RColorBrewer
reviews_tidy %>%
    count(Word) %>%
    with(wordcloud(Word, n, max.words = 100))

wordcloud2( myfinalTable, size=0.7, 
            # shape = 'star'
            shape	= 'circle'   # 'cardioid', 'diamond'  'triangle-forward', 
            # 'triangle', 'pentagon', and 'star'.
            # shape	=  'cardioid',
            # color = "random-light", 
            # color = colorVec ??
            # backgroundColor = "grey",
            # minRotation = -pi/2, maxRotation = -pi/2,
            # fontWeight = "bold"
            # rotateRatio = 0.9
      )  


#library(reshape2)
reviews_tidy %>%
    inner_join(get_sentiments("bing"), by = c("Word" = "word")) %>%
    count(Word, sentiment, sort = TRUE) %>%
    acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#202121", "#797C80"), max.words = 50)






# 
# AI !!
#install.packages("httr")
library(httr)

# DeepAI to use the website to generate text

r = httr::POST( url = "https://api.deepai.org/api/text-generator",
               body = list("text" = "How to become a railway man?"),
              add_headers(.headers = c("api-key" = "quickstart-QUdJIGlzIGNvbWluZy4uLi4K"))
             )
cat( content(r, "text"), "\n") # convert raw result to text

##Python Code
# import requests
# r = requests.post( "https://api.deepai.org/api/text-generator",
#                    data={  'text': 'How to become a data scientist in 30 days?', },
#                   headers={'api-key': 'quickstart-QUdJIGlzIGNvbWluZy4uLi4K'}
#                  )
# print(r.json())

## AI-Writer  AI to write an article for you
# AI-writer is an online platform where, you can ask the AI to write an article for you. 
# It also generates citations & sources for your article

#6 Learn to be less perfectionist and to prioritize


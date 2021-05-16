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

mytable <- mywords   %>%  unnest_tokens( mywordlist, word )   # worked - mywordlist !!!

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

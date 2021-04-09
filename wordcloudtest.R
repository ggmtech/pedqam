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
library(googlesheets)

########## loco failure word cloud
gs_ls("locofail1617")  

lf16key <-  gs_title("locofail1617")
lf16    <-  gs_read(ss = lf16key,  ws = "setout1617" , skip = 1, col_names = TRUE, stringsAsFactors=FALSE)

mywords <-   select(lf16, `Cause reported`)  %>%   gather(document, word)

mytable <- mywords   %>%  unnest_tokens( mywordlist, word )   # worked - mywordlist !!!

data(stop_words)      # data of common stop words like "the", "of" #View(stop_words)            

myTable  <-  mytable  %>%  anti_join( stop_words,  by = c("mywordlist" = "word")  ) 

myTable               # my master list of 1 lakhs words !!

mywordtable <- myTable %>%  count(mywordlist, sort = TRUE )  %>% filter( !is.na(mywordlist) )


myfinalTable  <-  mywordtable           %>%       #Remove other nonsense words
                  filter( !mywordlist   %in% 
                                        c('due',  'dep', 'rep' , 't.co', 'https', 
                                           'handmaidstale',  'watch' )   )

wordcloud2(myfinalTable, size=0.7, 
                         # shape = 'star'
                         shape	= 'circle'   # 'cardioid', 'diamond'  'triangle-forward', 
                                             # 'triangle', 'pentagon', and 'star'.
                         # color = "random-light", 
                         # color = colorVec ??
                         # backgroundColor = "grey",
                         # minRotation = -pi/2, maxRotation = -pi/2,
                         # fontWeight = "bold"
                         # rotateRatio = 0.9
                   )  


#######################################################

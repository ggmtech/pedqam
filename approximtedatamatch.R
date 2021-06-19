
# https://cran.r-project.org/web/packages/RecordLinkage/vignettes/BigData.pdf

rm(list = ls())
if (!require(devtools)) install.packages("devtools") # devtools::install_github("boxuancui/DataExplorer")  #, ref = "develop"

packages <- c("tidyverse",   "magrittr",  "here", "lubridate", "anytime",  "googlesheets4", "readxl", "janitor",
              "DT", "DataExplorer", "inspectdf", "summarytools", "skimr", "here", "devtools", "naniar",
              "scales",    "coefplot", "cowplot", "drat",
              "fuzzyjoin",
              "knitr", "gridExtra","plotly", "timetk", "ggforce", "ggraph", 
              "survival" , "survminer", "ggpubr","GGally", "ggrepel", "ggridges",
              "viridis", "viridisLite",  "ggthemes", #"themr",  #  "ggfortify",  "ggfortify", 
              "magick", "ggfx" ,  "prismatic"
)
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
lapply(packages, library, character.only = TRUE) #%>% invisible()

library(fuzzyjoin); library(googlesheets4)
# Get Data raw ###################################################################
# googlesheets4::sheet_add() sheet_append() sheet_copy() sheet_delete() sheet_write() sheet_properties() sheet_rename()
# googlesheets4::read_sheet(ss, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, or "cidDl"
#                          skip = 0, na = "", trim_ws = TRUE, n_max = Inf, guess_max = min(1000, n_max),.name_repair = "unique" )
# myfile = read_file(myuri) 

# x %>% stringdist_join(  y,    by = NULL,    max_dist = 2,         
#                       method = "jw",  #method = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
#                       ignore_case, distance_col
#                       mode = "inner",  ignore_case = FALSE,   distance_col = NULL,   ...   )# mode= "inner", "left", "right", "full" "semi", or "anti"
# 
#stringdist_inner_join(x, y, by = NULL, distance_col = NULL, ...)


## My data
ss01 = "https://docs.google.com/spreadsheets/d/1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg" # MASTER VENDOR LIST 2021
sheet_names(ss01)  # see sheets names
sheet1 = "LT3 UVAM"
sheet2 = "LT3PCDOapr21"

googlesheets4::read_sheet(ss1, sheet1, skip = 0, col_names = TRUE, col_types = NULL, trim_ws = TRUE) %>%
  janitor::clean_names() -> LT3UVAM

LT3UVAM %>% names() #View() "aa_name"  "dept_name"  "sub_directorate_name" "item_id" "item_description""sub_item_id" "vendors" "sub_item_desc""remarks"                         "SUB_ITEM_DESC"        "Remarks"

googlesheets4::read_sheet(ss1, sheet2, skip = 0, col_names = TRUE, col_types = NULL, trim_ws = TRUE) %>%
  janitor::clean_names() -> LT3PCDO

LT3PCDO  %>% names() # View()  "sn"   "item_description" "dte"   "vendors"   "reasons"  "remarks"         




####################################################################
# one to one match as assignment
library(stringdist)
a<-data.frame(name=c('Ace Co','Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),  price = c(10,13,2,1,15,1)  )
b<-data.frame(name=c('Ace Co.','Bayes Inc.','asdf'),                   qty  =  c(9,99,10)         )

a<- LT3UVAM %>% mutate(name = item_description )
b<- LT3PCDO %>% mutate(name = item_description )

####################################################################### better
library(fuzzyjoin); library(dplyr);


stringdist_join(a, b, 
                by = "name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(name.x) %>%
  slice_min(order_by = dist, n = 1)  %>% 
  select(   dept_name, sub_directorate_name, name.x,  dte,  name.y , dist)  %>%
  View()

#################################################


d <- expand.grid(a$name,b$name) # Distance matrix in long form
names(d) <- c("a_name","b_name")
d$dist <- stringdist::stringdist(d$a_name,d$b_name, method="jw") # String edit distance (use your favorite function here)

# Greedy assignment heuristic (Your favorite heuristic here)
greedyAssign <- function(a,b,d){   x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable, 
                                                           # 1 for already assigned, -1 for unassigned and unassignable
                   while(any(x==0)){ min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
                    a_sel <- a[d==min_d & x==0][1] 
                    b_sel <- b[d==min_d & a == a_sel & x==0][1] 
                    x[a==a_sel & b == b_sel]          <-   1
                    x[x==0     & (a==a_sel|b==b_sel)] <-  -1      }
                cbind(a=a[x==1],b=b[x==1],d=d[x==1])   }

data.frame(  greedyAssign( as.character(d$a_name),  
                           as.character(d$b_name), 
                           d$dist)                 )


# Many-to-one case (not an assignment problem):

do.call(  rbind, 
          unname(  by(d,   d$a_name,     function(x) x[x$dist == min(x$dist),]  )    
                )   
        )        %>% View()



# seen ok for upto 0.25 distance only


###########################
ss01 = "https://docs.google.com/spreadsheets/d/1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg" # MASTER VENDOR LIST 2021

sheet_names(ss01)  # see sheets names

sheet1 = "LT3 UVAM"
sheet2 = "LT3PCDOapr21"


googlesheets4::read_sheet(ss1, sheet1, skip = 0, col_names = TRUE, col_types = NULL, trim_ws = TRUE) %>%
janitor::clean_names() -> LT3UVAM

LT3UVAM %>% names() #View() "aa_name"  "dept_name"  "sub_directorate_name" "item_id" "item_description""sub_item_id" "vendors" "sub_item_desc""remarks"                         "SUB_ITEM_DESC"        "Remarks"


googlesheets4::read_sheet(ss1, sheet2, skip = 0, col_names = TRUE, col_types = NULL, trim_ws = TRUE) %>%
janitor::clean_names() -> LT3PCDO

LT3PCDO  %>% names() # View()  "sn"   "item_description" "dte"   "vendors"   "reasons"  "remarks"         



fuzzyjoin::stringdist_join(LT3UVAM,  LT3PCDO, 
                           by = NULL , #c( item_description = item_description    ),
                           mode = "left",
                           ignore_case = FALSE, 
                           method = "jw", 
                           max_dist = 99, 
                           distance_col = "dist") %>%
 # group_by(`SUB_DIRECTORATE_NAME`) %>%
  View()


LT3UVAM %>% 
          fuzzyjoin::stringdist_join( LT3PCDO, 
                           by = c( item_description = "item_description" ),
                           mode = "left",
                           ignore_case = FALSE, 
                           method = "jw", 
                           max_dist = 0.25, 
                           distance_col = "dist") %>%
   group_by(sub_directorate_name) %>% #View()
   select(item_description.x, item_description.y, dist ) %>%
   View()

# directorate name
LT3PCDO  %>% 
  fuzzyjoin::stringdist_join( LT3UVAM, 
                              by = c( dte = "sub_directorate_name",
                                      
                                      item_description = "item_description"
                                      ),
                              mode = "full",  #left",
                              ignore_case = FALSE, 
                              method = "jw", 
                              max_dist = 0.25, 
                              distance_col = "dist") %>%
  group_by(sub_directorate_name) %>% #View()
  select(dte, sub_directorate_name, dist ) %>%
  View()



LT3PCDO  %>% 
  fuzzyjoin::stringdist_join( LT3UVAM, 
                              by = c( item_description = "item_description"
                                      
                              ),
                              mode = "full",  #left",
                              ignore_case = FALSE, 
                              method = "jw", 
                              max_dist = 0.25, 
                              distance_col = "dist") %>%
  group_by(sub_directorate_name) %>% #View()
  select(item_description.x, item_description.y, dist ) %>%
  View()





LT3UVAM %>%
      # inner_join(LT3PCDO, by = c(item_description = item_description)   )
       stringdist_inner_join(LT3PCDO, by = c(item_description = item_description))

  group_by(name.x) %>%
  slice_min(order_by = dist, n = 1)





fuzzyjoin::stringdist_join(a, b, 
                           by = "name",
                           mode = "left",
                           ignore_case = FALSE, 
                           method = "jw", 
                           max_dist = 99, 
                           distance_col = "dist") %>%
  group_by(name.x) %>%
  slice_min(order_by = dist, n = 1)





fuzzyjoin::stringdist_join(a, b, 
                by = "name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(name.x) %>%
  slice_min(order_by = dist, n = 1)






## 
fuzzy_join(
  x,
  y,
  by = NULL,
  match_fun = NULL,
  multi_by = NULL,
  multi_match_fun = NULL,
  index_match_fun = NULL,
  mode = "inner",  #  "inner", "left", "right", "full" "semi", or "anti"
  # ...
)



# or like fuzzy_inner_join(x, y, by = NULL, match_fun, ...)

# difference_anti_join	Join two tables based on absolute difference between their columns
# difference_full_join	Join two tables based on absolute difference between their columns

# genome_anti_join	Join two tables based on overlapping genomic intervals: both a
# geo_anti_join	Join two tables based on a geo distance of longitudes and latitudes
# interval_anti_join	Join two tables based on overlapping (low, high) intervals

# misspellings	A corpus of common misspellings, for examples and practice
# regex_anti_join	Join two tables based on a regular expression in one column matching the other
# stringdist_anti_join	Join two tables based on fuzzy string matching of their columns










##############################################
########## another



library(stringdist)
a<-data.frame(name=c('Ace Co','Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),price=c(10,13,2,1,15,1))
b<-data.frame(name=c('Ace Co.','Bayes Inc.','asdf'),qty=c(9,99,10))



# solving as assignement problem

d        <-   expand.grid(a$name,b$name) # Distance matrix in long form
names(d) <-   c("a_name","b_name")
d$dist   <-   stringdist(d$a_name,d$b_name, method="jw") # String edit distance (use your favourite function here)


# Greedy assignment heuristic (Your favorite heuristic here)
greedyAssign <- function(a,b,d){
                 x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable, 
                                           # 1 for already assigned, -1 for unassigned and unassignable
                while(any(x==0)){
                    min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
                    a_sel <- a[d==min_d & x==0][1] 
                    b_sel <- b[d==min_d & a == a_sel & x==0][1] 
                    x[a==a_sel & b == b_sel] <- 1
                    x[x==0 & (a==a_sel|b==b_sel)] <- -1
                              }
                 cbind(a=a[x==1],b=b[x==1],d=d[x==1])
                }

data.frame(greedyAssign(  as.character(d$a_name),  as.character(d$b_name),   d$dist)  )


# solving not as assignment

do.call(  rbind,  
          unname(   by(d,  d$a_name,   function(x) x[x$dist == min(x$dist), ]  )  )  
        )

#use method="jw" to produce desired results. See help("stringdist-package")

#######################################

# text mining
library(tm)

vignette(tm)




pres <- c(" Obama, B.","Bush, G.W.","Obama, B.H.","Clinton, W.J.")
# agrep is what you want? It searches for approximate matches using the Levenshtein edit distance.

sapply(pres,agrep,pres)


lapply(pres, agrep, pres, value = TRUE)



#########################
library(RecordLinkage)
showClass("RLBigData")
showClass("RLBigDataDedup")
showClass("RLBigDataLinkage")

# deduplicate with two blocking iterations and string comparison
data(RLdata500)
data(RLdata10000)
rpairs1 <- RLBigDataDedup(RLdata500,
                          identity = identity.RLdata500,
                          blockfld = list(1,3), strcmp = 1:4)
# link two datasets with phonetic code
s1 <- 471:500
s2 <- sample(1:10000, 300)
identity2 <- c(identity.RLdata500[s1], rep(NaN, length(s2)))
dataset <- rbind(RLdata500[s1,], RLdata10000[s2,])
rpairs2 <- RLBigDataLinkage(RLdata500, dataset,
                            identity1 = identity.RLdata500,
                            identity2 = identity2, phonetic = 1:4,
                            exclude = "lname_c2")

# supervised classificaiton
train <- getMinimalTrain(compare.dedup(RLdata500,
                                       identity = identity.RLdata500,
                                       blockfld = list(1,3)))
rpairs1 <- RLBigDataDedup(RLdata500,  identity = identity.RLdata500)
classif <- trainSupv(train, "rpart", minsplit=2)
result <- classifySupv(classif, rpairs1)

showClass("RLResult")

getTable(result)
## < table of extent 0 x 0 >
getErrorMeasures(result)

# weight based
rpairs1 <- epiWeights(rpairs1)
result <- epiClassify(rpairs1, 0.5)
getTable(result)


getPairs(result, min.weight=0.7, filter.link="link")


getFalsePos(result)

getFalseNeg(result)


### RecordLinkage package was removed from CRAN, use stringdist instead:
  
library(stringdist)

ClosestMatch2 = function(string, stringVector){   stringVector[   amatch(string,    stringVector,   maxDist=Inf )    ]    }



## RecordLinkage again
library(RecordLinkage)

ClosestMatch2 = function(string, stringVector){ 
                                                distance = levenshteinSim(string, stringVector);
                                                stringVector[distance == max(distance)]
  
}







############
#devtools::install_github("ColinFay/tidystringdist")
# install.packages("tidystringdist")
library(tidystringdist)   
library(dplyr)
companies <- tibble(comp = c("3M",
                             "3M Company",
                             "3M Co",
                             "A & R LOGISTICS INC",
                             "AR LOGISTICS INC",
                             "A & R LOGISTICS LTD",
                             "ABB GROUP",
                             "ABB LTD",
                             "ABB INC"))
# Setup so that each company is compared to every other company
compare <- tidy_comb_all(companies, comp)

# Have a quick look at the data to show the company  ; # 3M being compared to all other companies
# excludes the the comparison being made with itself
head(compare, 9)

companies_translation <- expand.grid( companies$comp,  companies$comp ) %>% 
                         rename(V1 = Var1,   V2 = Var2 )                %>%
                         tidy_stringdist( . )

companies_translation  %>% View()


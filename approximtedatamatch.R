
library(googlesheets4)

# using fuzzy join

a <- data.frame(name = c('Ace Co', 'Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),
                price = c(10, 13, 2, 1, 15, 1))
b <- data.frame(name = c('Ace Co.', 'Bayes Inc.', 'asdf'),
                qty = c(9, 99, 10))

ss1 = "https://docs.google.com/spreadsheets/d/1byFykdwzExDUs5npEygtmd2q3P_KR2o7ZZ0xEkai75k/edit#gid=1869317471"
sheet1 = "QAMvendors"
ss2 = " "
sheet2 = " "

QAMallvendors <- googlesheets4::read_sheet(ss1, sheet1, skip = 2, col_names = TRUE, col_types = NULL, trim_ws = TRUE)
QAMallvendors %>% View()


library(fuzzyjoin); 
library(dplyr);

stringdist_join(a, b, 
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
  ...
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




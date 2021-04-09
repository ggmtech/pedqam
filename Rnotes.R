######
grep('stringtomatch', strlist, ignore.case ="True")   # grepl() for logical
# regexpr() no of patterns found. 
# sub(pattern, replaced_string, string) replaces the only first occurrence of the string to be replaced and returns modified
# gsub(pattern, replaced_string, string) replaces all occurrences and returns the modified string.
# str_remove(string, pattern, ignore.case=False) and str_remove_all()   removes and returns the modified string

#
str_extract("aaa12xxx", "[0-9]+")   # 
# str_extract(fruit, "nana") is shorthand for str_extract(fruit, regex("nana"))
stringr::str_extract(string, pattern)

str_extract_all(string, pattern, simplify = FALSE) # FALSE list char vectors.TRUE char matrix.

str_replace_all(text, "\\s+", " ")  # correct badly spaced text nline
#\p{property name} matches any character with specific unicode property
#\P{property name} - compliment ]
# \w \b 
# [^abc]: matches anything except a, b, or c.
# [\^\-]: matches ^ or -.
# [:punct:]: punctuation., [:alpha:]: letters.
# [:lower:]: lowercase letters., [:upper:]: upperclass letters. [:digit:]: digits.
# [:alnum:]: letters and numbers. [:cntrl:]: control characters.
# [:graph:]: letters, numbers, and punctuation.
# [:print:]: letters, numbers, punctuation, and whitespace.
# [:space:]: space characters (basically equivalent to \s).
# [:blank:]: space and tab.




7718955555




packages <- c("tidyverse", "lubridate", "parsedate", "janitor",
              "cowplot", "ggpubr",  "here", "scales", "stringr", 
              "writexl", "readxl",         #"xlsx", needs java
              "googledrive" , "googlesheets4",               
              #"summarytools",  
              "knitr",  "kableExtra",  
              "alluvial", "ggalluvial"  )


installed_packages <- packages %in% rownames(  installed.packages() )
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }

lapply(packages, library, character.only = TRUE) # %>% invisible()

#library(devtools) ; devtools::install_github("tidyverse/googlesheets4")

install.packages("tidyverse", type="source")
remove.packages("rlang")
library(tidyverse)
devtools::install_github("tidyverse/tidyverse")

############################
library(usethis)
ui_info("hi!")
analysis_year <- 2019
ui_todo("Querying demographics data for
        {analysis_year}...")

# Automating templates
clean_data <- function(analysis_year) {
    usethis::use_template(
        template = "clean_data.R",
        save_as = "01_clean-data.R",
        data = list(analysis_year = analysis_year),
        package = "demographicsreport"
    )
}
###############
library(osmdata) ## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright
library(sf)

vancouver_highways <- opq(bbox = 'Vancouver Canada') %>% add_osm_feature(key = 'highway') %>% osmdata_sf()

vancouver_highways <- opq(bbox = 'New Delhi India') %>%
                      add_osm_feature(key = 'railway', value = c('subway', 'rail') )%>%   #highway
                      osmdata_sf()

vancouver_highways

plot(vancouver_highways$osm_lines$geometry)

######################## to be used
# drive_auth()  # authorise user, cmpenr done
# drive_user()  # see user authorised
# drive_find("xari", n_max = 30) # List files
# drive_find("Master")
#  range = "B3:F80" | cell_cols(1:4) | cell_limits(  c(1, 4), c(5, NA) | cell_rows(1:1000) ), locale, trim_ws, na, comment, n_max

sheetgid = "1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg"
sheetid = "ItemMaster400"
sheetid3 = "ListofLT5Vendors"

#MasterList1 <- read_sheet( sheetgid, sheetid)
MasterList1 <- read_sheet(sheetgid, sheetid, 
                          skip = 0, col_names = TRUE,  na = "", trim_ws = TRUE, .name_repair = "unique" ) %>% 
               rowid_to_column()  %>%
               as_tibble()


glimpse(MasterList1)
MasterList1$Item

MasterLT5 <- read_sheet(sheetgid, sheetid3, 
                          skip = 0, col_names = TRUE,  na = "", trim_ws = TRUE, .name_repair = "unique" ) %>% 
    rowid_to_column()  %>%
    as_tibble()

MasterLT5 %>% View()
MasterLT5$Item

#MasterList1  %>%  mutate( DateApplication = parsedate::parse_date(DateApplication)) %>% 
#                  mutate(qtr = paste0(substring(year(DateApplication),1,4),"/Q",quarter(DateApplication, with_year = FALSE, fiscal_start = 4))) %>% 
#                  View()


#quarter(x, with_year = TRUE, fiscal_start = 11)
#parse_date(dates, approx = TRUE, default_tz = "UTC")
# Cross tabulation
# library(janitor)
#FreshVRegistration  %>%  
    #mutate( DateApplication = parsedate::parse_date(DateApplication)) %>% 
    #x mutate( DateApplication = excel_numeric_to_date(DateApplication, include_time = TRUE)  ) %>%
    # mutate(qtr = paste0(substring(year(DateApplication),1,4),"/Q",quarter(DateApplication, with_year = FALSE))) %>%   #, fiscal_start = 4
    # tabyl(Directorate, qtr) %>%
    # adorn_totals(c("row", "col")) %>%         #adorn_totals(): Add totals row, column, or both.
    # adorn_ns() %>%
    # adorn_title("combined") %>%   #adorn_title(): add a title to a tabyl (or other data.frame).
    # adorn_percentages(rounding = "half up", digits = 0) %>%  # percentages along either axis or over the entire tabyl
    # adorn_pct_formatting(digits = 2) %>%   
    # adorn_ns() %>%  #Adornments have options to control axes, rounding, and other relevant formatting choices (
    # View()


# also good for tibbles !! 
# mtcars %>% adorn_totals(c("row", "col")) #%>% adorn_percentages("col")


#mutate_if(is.numeric, funs(replace_na(., 0)))
#na_if(y, "")
#starwars %>%
#    select(name, eye_color) %>%
#    mutate(eye_color = na_if(eye_color, "unknown"))

#starwars %>%
#    mutate_if(is.character, list(~na_if(., "unknown")))
# df %>% replace_na(list(x = 0, y = "unknown"))

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
x1 <- c("Jan", "fel", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
y2 <- parse_factor(x1, levels = month_levels)  # gives warning else factor(x1, levels = month_levels) drops silenlty

factor(x1) # make factores in alphabatical order
f1 <-  x1 %>% factor( levels = unique(x1))
f2 <-  x1 %>% factor() %>% fct_inorder()

MasterList1 %>% mutate( value = as.numeric(as.character(`AA Value in Crores`)   ))  %>% as.data.frame()-> temp #%>% View()

glimpse(temp)

MasterList1 %>% count()
MasterList1 %>% count(  Less3Vendor )
MasterList1 %>% count(  Less3Vendor ,  `AA Value in Crores` )

RDSOmarch31 <- MasterList1

RDSOmarch31 %>% filter(!is.na(`RlyBd Del List SN`) ) %>%   
               # mutate(  StatusMarch31 = if_else(is.na(`Under Process`) , NA, `Under Process` )   )%>%
                mutate( fct_infreq(as.character(Vendors) )) %>%
                mutate(StatusMarch31 = fct_lump(StatusApril30, n =8)) %>%
               count()
               # mutate( Business = as.double(`AA Value in Crores`) ) %>% 
               # filter(!is.na(Business)) %>%
               #  group_by(Directorate) %>%


RDSOmarch31 %>%  filter(!is.na(`RlyBd Del List SN`) ) %>%
                 mutate(VDnow = if_else( StatusApril30 ==  "Approved" , "Approved", "Awaited" ,  missing = NULL  ) ) %>% 
                 mutate(VDnow = parse_factor(VDnow, levels(c("Approved", "Not Approved" ) )))  %>% 
                 View() #str() 
    


mutate(VDnow = if_else(  is.na(VDnow ),   "Under Process" , StatusApril30, missing = NULL  ) ) %>%
    group_by(Directorate, VDnow) %>% count() %>%  spread(VDnow, n)           
    


RDSOmarch31 %>%  filter(!is.na(`RlyBd Del List SN`) ) %>% 
    mutate(VDnow = if_else(StatusApril30 == "Under Process", StatusApril30, StatusApril30 ) ) %>% 
    mutate( StatusMarch31 = as.factor(StatusApril30) ) %>%
    group_by(Directorate, StatusApril30) %>% count() %>%
    spread(StatusApril30, n)           
    

RDSOmarch31 %>% mutate( Business = as.double(`AA Value in Crores`)   )  %>%
                mutate(  LessVendors = if_else(Vendors < 3 , "Less", "OK")   )  %>%
                select( -SN, -UID, -Item, -`RlyBd Del List SN`, -`Remarks ( if critically required, else avoid)`,
                        -`PL Number`, -Less3Vendor, -`Cat I-II-III`, -`AA Value in Crores`)  %>% 
                mutate( Less2cr = if_else(Business < 2, "Less2Crores", "BusinessOver2Cr", missing = "Not Available")  )  ->  vlist
vlist

vlist %>%  filter()  %>% count()
all = count(vlist)
vlist %>% count(LessVendors)



##########

a<-data.frame(name=c('Ace Co','Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),price=c(10,13,2,1,15,1))
b<-data.frame(name=c('Ace Co.','Bayes Inc.','asdf'),qty=c(9,99,10))

a <- MasterLT5
b <- MasterList1


library(stringdist)

d        <- expand.grid( a$Item,  b$Item) # Distance matrix in long form
names(d) <- c("a_name","b_name")
d$dist <- stringdist(d$a_name,d$b_name, method="jw") # String edit distance (use your favorite function here)

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

data.frame(greedyAssign(as.character(d$a_name),as.character(d$b_name),d$dist))



# or
a <- data.frame(name = c('Ace Co', 'Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),
                price = c(10, 13, 2, 1, 15, 1))
b <- data.frame(name = c('Ace Co.', 'Bayes Inc.', 'asdf'),
                qty = c(9, 99, 10))

library(fuzzyjoin); library(dplyr);



stringdist_join(a, b,        by = "Item",      mode = "left",    ignore_case = FALSE, 
                method = "jw",       max_dist = 99, 
                distance_col = "dist") %>%     View()
                group_by(Item.x) %>%           #View()
                top_n(1, -dist)  %>% View()





########################## using excel
# excel_sheets("/Users/gk/Google Drive/gkvdlist.xlsx")
# To load all sheets in a workbook, use lapply
#path <- excel_sheets("/Users/gk/Google Drive/gkvdlist.xlsx")
#?lapply(read_xlsx(path), read_excel, path = path)
#QAdata    <- readxl::read_xlsx("/Users/gk/Google Drive/gkvdlist.xlsx", sheet = 1)
#QAdel     <- readxl::read_xlsx("/Users/gk/Google Drive/gkvdlist.xlsx", sheet = "Del132")
#QAdatamix <- readxl::read_xlsx("/Users/gk/Google Drive/QAfull.xlsx", sheet = 1)

QAall    <-        excel_sheets("/Users/gk/Google Drive/QAsuperlist.xlsx")
QAall      <- readxl::read_xlsx("/Users/gk/Google Drive/QAsuperlist.xlsx", sheet = "VD638")
QArbupdate <- readxl::read_xlsx("/Users/gk/Google Drive/QAsuperlist.xlsx", sheet = "Update132")
QArbupdate %>% select(ListRB, StatusMarch) %>% mutate(ListRB = toupper(ListRB)) -> QArb
QArb  %>% str()


left_join( QAall,  QArb , by = c(  "ListRB" = "ListRB" ) ) -> QAmaster #%>% View()
str(QAmaster)
QAmaster %>% View()
QAmaster %>% count(StatusMarch)

QAmaster %>% mutate(Less3Vendor = if_else(as.numeric(Vendors)  <3 , "Less3Vendor" , "OK"   )) %>%
    select(SN, Directorate,  UID,  Item ,  Vendors, Less3Vendor, AAVcr, Prospects , ListRB, StatusMarch ) -> QAMedit

QAMedit %>% View() 

write_xlsx(QAMedit, path = "/Users/gk/Google Drive/QAedit.xlsx", col_names = TRUE)

# prepare for alluviam
str(QAMedit)

QAMedit %>%  mutate(ListRB = ifelse(is.na(ListRB), "NotSent" , "SentRB") ) %>%
    mutate(Less3Vendor = ifelse( is.na(Less3Vendor) , "OK" , Less3Vendor ) )%>%
    group_by(Directorate,  ListRB, Less3Vendor) %>%  
    count(Directorate )  -> QAplot
QAplot


############## or latest googled  RDSOmarch31
RDSOmarch31  %>% View()

str(RDSOmarch31)


# Status on march 31 
RDSOmarch31 %>%     #mutate(VDnow = ifelse(StatusMarch31 == "Approved"|StatusMarch31 == "Merged" ,  "Deleted", "RDSOitem"    ) ) %>%
                    group_by(Directorate, StatusMarch31) %>% count() %>%
                    spread(StatusMarch31, n) # %>% kable()

                  


# items break up of less then three items :
RDSOmarch31 %>%     filter(Vendors  < 3)   %>%
                    filter(!StatusMarch31 =="Approved" | is.na(StatusMarch31) )  %>% View()
                    #group_by(Directorate,  Less3Vendor) %>%  
                    count( )  #%>%
                    spread(Less3Vendor, n)

   


#### Eulerr
RDSOmarch31 %>% group_by(Directorate) %>% count()  -> list1
list1
library(eulerr)

three_inside_fourth <- euler(c("A-Totalitems" = 639,
                               "A-Totalitems&B-BoardProposed" = 132, "A-Totalitems&C" = 3, "A-Totalitems&D" = 3,
                               "A-Totalitems&B-BoardProposed&C" = 20, "A-Totalitems&B-BoardProposed&D" = 32, "A-Totalitems&C&D" = 42,
                               "A-Totalitems&B-BoardProposed&C&D" = 15))
plot(three_inside_fourth)
 

str(RDSOmarch31)



# Dot chart of a single numeric vector
dotchart(mtcars$mpg, labels = row.names(mtcars), cex = 0.6, xlab = "mpg")

dotchart(RDSOmarch31$Vendors, labels = RDSOmarch31$Directorate, cex = 0.6, xlab = "mpg")


barplot(RDSOmarch31$Vendors, names = RDSOmarch31$Directorate)


completely_contained <- euler(c("A" = 15, "B" = 15, "C" = 3,
                                "A&B" = 3, "A&C" = 0, "B&C" = 0,
                                "A&B&C" = 3))
?euler
plot(completely_contained,
    # labels = list(col = c("white", "black", "black") ),
     labels = c("To m \n ggg", "Greg", "Alberta"),   # "A&B" , "A&C", "B&C" , "A&B&C"
     edges = list(col = "white", lex = 2),
     fills = c("magenta", "cyan", "yellow")         # orange
      )   


plot(euler(as.table(apply(Titanic, 2:4, sum))))

Titanic
class(Titanic)
typeof(Titanic)
as_tibble(Titanic)  
Titanic %>% apply( 2:4, sum) %>% as.table() %>% euler() %>% plot()

     
three_inside_fourth <- euler(c("A" = 30,
                               "A&B" = 3, "A&C" = 3, "A&D" = 3,
                               "A&B&C" = 2, "A&B&D" = 2, "A&C&D" = 2,
                               "A&B&C&D" = 1))
plot(three_inside_fourth, labels = c("To m \n ggg", "Greg", "Alberta", "hhgkhjgk"),)



## for alluvial plot data
RDSOmarch31 %>%     mutate(ListRB = ifelse(is.na(`RlyBd Del List SN`), "NotSent" , "SentRB") ) %>%
                    mutate(Less3Vendor = ifelse( is.na(Less3Vendor) , "OK" , Less3Vendor ) )%>%
                    group_by(Directorate,  ListRB, Less3Vendor) %>%  
                    count(Directorate )  -> QAplot

QAplot 



################  plot the alluivial 
QAplot

alluvial(QAplot[ , 1:3],
         freq   = QAplot$n,
         col    = ifelse(QAplot$Less3Vendor == "Less3Vendor", "red", "blue"),
         #border = ifelse(!is.na(QAplot$ListRB), "blue", "red"),
         #hide   = QAplot$ProposedDEL_RB < 2, #uncomplicate by ignoring small
         cex    = 0.7 ,    #font size
         alpha  = 0.4 ,   
         # blocks=FALSE   # for merging
         gap.width = 0.1,
         cw = 0.1         # colomn width
         # other options ..layer order, gap.width, xw,cw,blocks, 
         # ordering,axis_lables, 
)

#######################################









QAall %>% View()
str(QAall)


QAall %>% count()                   #  641 item list
QAall %>% count(Directorate)        #  Directratewise
QAall %>% filter( !is.na(ListRB) )  %>%  count()   # 129   list sent to board, not non avl
QAall %>% mutate(less3vendor = if_else( as.numeric(Vendors) <3, TRUE, FALSE )  ) %>%  count(less3vendor) # View()

# less vendor and less then 2 crores  # 60 TRUE
QAall %>% mutate(less3vendor = if_else( as.numeric(Vendors) <3, TRUE, FALSE )  ) %>% 
    filter(  as.numeric(AAVcr) > 2    )   %>%
    count(less3vendor) # View()

# less vendor and More then 2 crores  # 54 + 2
QAall %>% mutate(less3vendor = if_else( as.numeric(Vendors) <3, TRUE, FALSE )  ) %>% 
    filter(  as.numeric(AAVcr) < 2.001    )   %>%
    count(less3vendor) # View()

# After removing "proposed"    # 68 TRUE + 4 NA
QAall %>% mutate(less3vendor = if_else( as.numeric(Vendors) < 3 , TRUE, FALSE )  ) %>%  
    filter( !is.na(ListRB) ) %>%  count(less3vendor) # View()

# After only removing "deltedby board"    # 
QAall %>% mutate(less3vendor = if_else( as.numeric(Vendors) < 3 , TRUE, FALSE )  ) %>%  
    filter( !is.na(   ?????ListRB  ) ) %>%  count(less3vendor) # View()




QAdata
QAdel
QAdata %>% count(Directorate)

inner_join(QAdata, QAdel ,  by = c("Item" = "Item"))  %>% View()

full_join(QAdata, QAdel ,  by = c("Item" = "Item"))  -> QAfull   # %>% View()
QAfull
write_xlsx(QAfull, path = "/Users/gk/Google Drive/QAfull.xlsx", col_names = TRUE)

# write_xlsx(x = daily, path = "daily.xlsx", col_names = TRUE)
# write_xlsx( x      ,  path = tempfile(fileext = ".xlsx"),  col_names = TRUE,  format_headers = TRUE )

left_join(QAdata, QAdel ,  ) 

#setoutbak <- read_csv("/Users/gk/Google Drive/setouts1819final.csv") #  OK !
glimpse(QAdata)
QAdata %>% View()

#tally(QAdata$ProposedDEL_RB)


QAdata  %>% mutate(Less3Vendor = ifelse( as.numeric(Vendors) > 3, "OK", "LessThen3")   ) %>%
    group_by(Directorate,ProposedDEL_RB , Less3Vendor) %>% 
    count(`Item Description` , )  -> QAplot

QAplot
alluvial(QAplot[,1:2], 
         freq   = QAplot$n,
         #col    = ifelse(QAplot$ProposedDEL_RB == "No", "red", "blue"),
         border = ifelse(QAplot$ProposedDEL_RB == "No", "blue", "red"),
         #hide   = QAplot$ProposedDEL_RB < 2, #uncomplicate by ignoring small
         cex    = 0.7 ,    #font size
         alpha  = 0.4 ,   
         # blocks=FALSE   # for merging
         gap.width = 0.1,
         cw = 0.1         # colomn width
         # other options ..layer order, gap.width, xw,cw,blocks, 
         # ordering,axis_lables, 
)



install.packages('tinytex')
tinytex::install_tinytex()

install_tinytex(
    force = FALSE,
    dir = "auto",
    repository = "ctan",
    extra_packages = NULL,
    add_path = TRUE
)
##########
as.data.frame(Titanic)

ggplot(as.data.frame(Titanic),
       aes( y = Freq, 
            axis1 = Survived, 
            axis2 = Sex, 
            axis3 = Class,
            axis4 = Age )  )      +
    geom_alluvium( aes(fill = Class),
                   width = 0, 
                   knot.pos = 0, 
                   reverse = FALSE) +
    guides(fill = FALSE)  +
    geom_stratum(width = 1/5, reverse = FALSE) +
    geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
    scale_x_continuous(breaks = 1:4, 
                       labels = c("Survived", "Sex", "Class", "Age") ) +
    # coord_flip() +
    #ggtitle("Titanic survival by class and sex") 
    ggtitle("passengers on Titanic", "stratified by demographics and survival")
####################



reducedData = myFormattedData %>% 
    group_by(replicate, position, size) %>% 
    summarise(count = sum(count)) %>% 
    ungroup()
#################


tit <- as.data.frame(Titanic, stringsAsFactors = FALSE)
head(tit)
glimpse(tit)

# Plain and simple
alluvial(  tit[ , 1:4 ],   freq   = tit$Freq  )

# or with bells
alluvial(tit[,1:4], 
         freq   = tit$Freq,
         col    = ifelse(tit$Survived == "Yes", "red", "blue"),
         border = ifelse(tit$Survived == "Yes", "blue", "grey"),
         hide   = tit$Freq < 1, #uncomplicate by ignoring small
         cex    = 0.7 ,    #font size
         alpha  = 0.4 ,   
         # blocks=FALSE   # for merging
         gap.width = 0.1,
         cw = 0.1         # colomn width
         # other options ..layer order, gap.width, xw,cw,blocks, 
         # ordering,axis_lables, 
)

# alluvial(..., freq, col = "gray", border = 0, layer, hide = FALSE,
#         alpha = 0.5, gap.width = 0.05, xw = 0.1, cw = 0.1, blocks = TRUE,
#         ordering = NULL, axis_labels = NULL, cex = par("cex"),
#         cex.axis = par("cex.axis"))

# simplest only two cross
# Survival status and Class

tit  %>%  group_by(Class, Survived)    %>% 
    summarise(  n = sum(Freq)  ) %>% 
    select(1,2)  %>% 
    alluvial(  freq = tit2d$n  )


tit %>% group_by(Class, Survived)  %>%
    summarise(  n = sum(Freq)  )        -> tit2d
tit2d
alluvial(  tit2d[,1:2], freq=tit2d$n  )

#same
tit %>% group_by(Class, Survived)       %>%
    summarise(  n = sum(Freq)  )    %>% 
    select(1,2)                     %>%
    alluvial(   freq=tit2d$n   )

#Three variables Sex, Class, and Survived:

# Survival status, Sex, and Class
tit %>% group_by(Sex, Class, Survived) %>%
    summarise(n = sum(Freq)) -> tit3d

alluvial(   tit3d[,1:3],    freq=tit3d$n)

# Customizing colors
# Colors of the alluvia can be customized with col, border and alpha arguments. For example:

alluvial(    tit3d[,1:3],   
             freq=tit3d$n,
             col = ifelse( tit3d$Sex == "Female",  "pink",  "lightskyblue"),
             border = "green",  
             # blocks=FALSE,
             alpha = 0.7
)

# hide small items

alluvial(tit2d[,1:2], freq=tit2d$n, hide=tit2d$n < 150)

# This skips drawing the alluvia corresponding to the following rows in tit data frame:
tit2d %>% select(Class, Survived, n) %>% filter(n < 150)

#Changing “layers”
# By default alluvia are plotted in the same order in which the rows are ordered in the dataset.

d <- data.frame(
    x = c(1, 2, 3),
    y = c(3 ,2, 1),
    freq=c(1,1,1)
)
d



#As there are three rows, we will have three alluvia:

alluvial(  d[,1:2],       freq=d$freq, col=1:3, alpha=1)
alluvial(  d[ 3:1, 1:2 ], freq=d$freq, col=3:1, alpha=1)             # Reversing the order
alluvial(  d[,1:2],       freq=d$freq, col=1:3, alpha=1,  layer=3:1) #  with single para


pal <- c("red4", "lightskyblue4", "red", "lightskyblue")

tit %>%    mutate(      ss = paste(Survived, Sex),
                        k  = pal[ match(ss, sort(unique(ss))) ]
) -> tit
tit
alluvial(    tit[,c(4,2,3)],  
             freq=tit$Freq,
             hide = tit$Freq < 10,
             col = tit$k,
             border = tit$k,
             blocks=FALSE,
             ordering = list(   NULL,    NULL,  order(tit$Age, tit$Sex )   )
)


###############  vertical  #########################

library(ggalluvial)
as.data.frame(Titanic)

ggplot(as.data.frame(Titanic),
       aes( y = Freq, 
            axis1 = Survived, 
            axis2 = Sex, 
            axis3 = Class,
            axis4 = Age )  )      +
    geom_alluvium( aes(fill = Class),
                   width = 0, 
                   knot.pos = 0, 
                   reverse = FALSE) +
    guides(fill = FALSE)  +
    geom_stratum(width = 1/5, reverse = FALSE) +
    geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
    scale_x_continuous(breaks = 1:4, 
                       labels = c("Survived", "Sex", "Class", "Age") ) +
    # coord_flip() +
    #ggtitle("Titanic survival by class and sex") 
    ggtitle("passengers on Titanic", "stratified by demographics and survival")


###########





library(networkD3)
nodes = data.frame("name" =    c("Node A",       # Node 0
                                 "Node B",        # Node 1
                                 "Node C",        # Node 2
                                 "Node D",        # Node 3
                                 "Nodes E"
))       

links = as.data.frame(  matrix(
    c(  0, 1, 5,           # Each row represents a link. The first number
        0, 2, 20,          # represents the node being conntected from. 
        1, 3, 30,          # the second number represents the node connected to.
        2, 3, 40,          # The third number is the value of the node
        3, 4, 25),
    byrow = TRUE, ncol = 3))

names(links) = c("source", "target", "value")

links

sankeyNetwork( Links = links, 
               Nodes = nodes,
               Source = "source", 
               Target = "target",
               Value = "value", 
               NodeID = "name",
               fontSize= 18, 
               nodeWidth = 30  )




######

data(majors)
# omit missing lodes and incident flows
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student) ) +
    geom_alluvium(fill = "darkgrey", na.rm = TRUE) +
    geom_stratum(aes(fill = curriculum), color = NA, na.rm = TRUE) +
    #scale_y_reverse() + 
    #geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = "black")
    theme_bw()


## Not run: 
# Recreate Bostock Sankey diagram: http://bost.ocks.org/mike/sankey/
# Load energy projection data
URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/' , 'master/JSONdata/energy.json')
energy <- jsonlite::fromJSON(URL)
str(energy) # a List of 2
unnest(energy)
# Plot
sankeyNetwork(Links = energy$links, 
              Nodes = energy$nodes, 
              Source = 'source',
              Target = 'target', 
              Value = 'value', 
              NodeID = 'name',
              units = 'TWh', 
              fontSize = 12, 
              nodeWidth = 30)

# Colour links
energy$links$energy_type <- sub(' .*', '', energy$nodes[energy$links$source + 1, 'name'])

sankeyNetwork(  Links = energy$links, 
                Nodes = energy$nodes, 
                Source = 'source',
                Target = 'target', 
                Value = 'value', 
                NodeID = 'name',
                LinkGroup = 'energy_type', 
                NodeGroup = NULL             )




#########
# devtools::install_github("corybrunson/ggalluvial", ref = "optimization")
library(ggalluvial)

titanic_wide <- data.frame(Titanic)
ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,    y = Freq)) +
    scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05)) +
    xlab("Demographic") +
    geom_alluvium(aes(fill = Survived)) +
    geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
    #coord_flip() +
    theme_minimal() +
    ggtitle("passengers on the maiden voyage of the Titanic",
            "stratified by demographics and survival") +
    theme(legend.position = 'bottom')

## 
ggplot(titanic_wide,
       aes( axis1 = Survived, axis2 = Sex, axis3 = Class,  y = Freq )) +
    geom_alluvium(aes(fill = Class), width = 0, knot.pos = 0, reverse = FALSE) +
    guides(fill = FALSE) +
    geom_stratum(width = 1/8, reverse = FALSE) +
    geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
    scale_x_continuous(expand = c(0, 0), breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
    scale_y_discrete(expand = c(0, 0)) +
    #coord_flip() +
    ggtitle("Titanic survival by class and sex")


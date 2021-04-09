# alluvial daiagram
#install.packages(alluvial)
library(tidyverse)    # for %>%
library(readxl)
# library(xlsx)   # neds java
library(writexl)  # install.packages("writexl")

library(alluvial)
library(ggalluvial)

remotes::install_github("davidgohel/officedown")
install.packages("officedown")
install.packages(c('tinytex', 'rmarkdown'))
tinytex::install_tinytex()
# after restarting RStudio, confirm that you have LaTeX with 
tinytex:::is_tinytex() 
#tinytex::tlmgr_install("babel-portuges")

# not on CRAN yet
if (!require("remotes")) install.packages("remotes", repos = "https://cran.rstudio.org")
remotes::install_github("rstudio/bookdown")
remotes::install_github("ismayc/thesisdown")


######################## to be used
library(googledrive) # better  use  for files suggested by jenny bryon
library(googlesheets)
drive_user()  # user authorised
drive_auth()  # authorise user

# faled ?? drive_find(n_max = 30) #??drive_find("Master") #??drive_find("MASTER VENDOR LIST March2020")
# works with direct correct file name
allvendorhandle   <-   gs_title("MASTER VENDOR LIST March2020")      # gs by title

allvendorhandle
# range = "R1C1:R4C3" | "B3:F80" | cell_cols(1:4) | cell_limits(  c(1, 4), c(5, NA) | cell_rows(1:1000) )
# locale, trim_ws, na, comment, n_max
RDSOmarch31           <-   gs_read(ss = allvendorhandle,   ws = 1 , skip = 2, col_names = TRUE,verbose = TRUE) # %>% rowid_to_column()
RDSOmarch31  %>%   View()

##########################
setwd("/Users/gk/Google Drive")   #  getwd()
list.files("/Users/gk/Google Drive/")

excel_sheets("/Users/gk/Google Drive/gkvdlist.xlsx")

# To load all sheets in a workbook, use lapply
#path <- excel_sheets("/Users/gk/Google Drive/gkvdlist.xlsx")
#?lapply(read_xlsx(path), read_excel, path = path)
#QAdata    <- readxl::read_xlsx("/Users/gk/Google Drive/gkvdlist.xlsx", sheet = 1)
#QAdel     <- readxl::read_xlsx("/Users/gk/Google Drive/gkvdlist.xlsx", sheet = "Del132")
#QAdatamix <- readxl::read_xlsx("/Users/gk/Google Drive/QAfull.xlsx", sheet = 1)

excel_sheets("/Users/gk/Google Drive/QAsuperlist.xlsx")

QAall <- readxl::read_xlsx("/Users/gk/Google Drive/QAsuperlist.xlsx", sheet = "VD638")
QAall %>% View()

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


mydb  <- QAMedit 
mydb <- RDSOmarch31
QAMedit %>%  mutate(ListRB = ifelse(is.na(ListRB), "NotSent" , "SentRB") ) %>%
             mutate(Less3Vendor = ifelse( is.na(Less3Vendor) , "OK" , Less3Vendor ) )%>%
            group_by(Directorate,  ListRB, Less3Vendor) %>%  
            count(Directorate )  -> QAplot
QAplot


## or latest googled
str(QAMedit)

mydb %>%  
  mutate(ListRB = ifelse(is.na(ListRB), "NotSent" , "SentRB") ) %>%
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
         hide   = tit$Freq < 100, #uncomplicate by ignoring small
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
titanic_wide <- data.frame(Titanic)
head(titanic_wide)

ggplot(data = titanic_wide,
        aes(axis1 = Class, axis2 = Sex, axis3 = Age, y = Freq)) +
        scale_x_discrete(  limits = c("Class", "Sex", "Age"), expand = c(.1, .05)   ) +
        xlab("Demographic") +
        geom_alluvium(aes(fill = Survived)) +
        geom_stratum() + 
        geom_text(stat = "stratum", label.strata = TRUE) +
        theme_minimal() +
        ggtitle("passengers on  Titanic", "stratified by demographics and survival")


# or
#The data is in “wide” format, but ggalluvial also recognizes data in “long” format and can convert between the two:
titanic_long <- to_lodes_form(data.frame(Titanic), key = "Demographic", axes = 1:3)
ggplot(data = titanic_long,
       aes(x = Demographic, stratum = stratum, alluvium = alluvium,
           y = Freq, label = stratum)) +
    geom_alluvium(aes(fill = Survived)) +
    geom_stratum() + geom_text(stat = "stratum") +
    theme_minimal() +
    ggtitle("passengers on Titanic", "stratified by demographics and survival")

#########################################



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
       coord_flip() +
       ggtitle("Titanic survival by class and sex")


##################################
library(googleVis)

df=data.frame(country = c("US", "GB", "BR"),  val1 = c(10,13,14),   val2 = c(23,12,32)           )

Line <- gvisLineChart(df)
plot(Line)

Bubble <- gvisBubbleChart(Fruits, 
                          idvar="Fruit", 
                          xvar ="Sales", 
                          yvar="Expenses",
                          colorvar="Year", 
                          sizevar="Profit",
                          options=list( hAxis='{minValue:75, maxValue:125}')      )
plot(Bubble)





############ install.packages('easyalluvial')
library('easyalluvial')

knitr::kable( head(mtcars2) )

alluvial_wide( data = mtcars2
               , max_variables = 5
               , fill_by = 'first_variable' )


knitr::kable( head(quarterly_flights) )
alluvial_long( quarterly_flights
               , key = qu
               , value = mean_arr_delay
               , id = tailnum
               , fill = carrier )
# Marginal Histograms
alluvial_wide( data = mtcars2
               , max_variables = 5
               , fill_by = 'first_variable' ) %>%
             add_marginal_histograms(mtcars2)


# Interactive Graphs
suppressPackageStartupMessages( require(parcats) )

p = alluvial_wide(mtcars2, max_variables = 5)

parcats(p, marginal_histograms = TRUE, data_input = mtcars2)





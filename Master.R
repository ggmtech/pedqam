
# keyboard shortcut  Cmd + Shift + F10.    # comments ctr sht c

rm(list = ls(all.names = TRUE))  # will clear all objects includes hidden objects.
rm(list = ls(all       = TRUE))
#invisible( lapply( paste0('package:', names(sessionInfo()$otherPkgs)),  detach, character.only=TRUE, unload=TRUE )     )
gc()                             # free memory and report usage.but does not delete variables
sessionInfo()

#install.packages(c('tinytex', 'rmarkdown'))
#tinytex::install_tinytex()
# restarting RStudio, confirm LaTeX with 
#tinytex:::is_tinytex() 

if (!require("remotes")) install.packages("remotes", repos = "https://cran.rstudio.org")

#remotes::install_github("rstudio/bookdown")
#remotes::install_github("ismayc/thesisdown")
<<<<<<< HEAD
remotes::install_github("davidgohel/officedown")
dir <- system.file(package = "officedown", "examples", "bookdown")
file.copy(dir, getwd(), recursive = TRUE, overwrite = TRUE)
rmarkdown::render_site("bookdown")
browseURL("bookdown/_book/bookdown.docx")




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


=======
#remotes::install_github("davidgohel/officedown")
#dir <- system.file(package = "officedown", "examples", "bookdown")
#file.copy(dir, getwd(), recursive = TRUE, overwrite = TRUE)

#rmarkdown::render_site("bookdown")
#rmarkdown::powerpoint_presentation()
# browseURL("bookdown/_book/bookdown.docx") ?
>>>>>>> 4bb9860ba0f8c4292b05ce8b6918fa91b0eddbe7
############################

# load libraries
packages <- c("tidyverse", 
              "ggpubr", #not installing??
              "lubridate", "stringr", "writexl", "readxl", 
              "parsedate", "janitor",
              "cowplot",  
              "tidyquant", "patchwork",
              #"summarytools",  "here", "scales", 
              "googledrive" , "googlesheets4", 
              "eulerr",
              "knitr",  "kableExtra",  
              "bookdown",                    #"thesisdown",
              "alluvial", "ggalluvial")
installed_packages <- packages %in% rownames(  installed.packages() )
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }

lapply(packages, library, character.only = TRUE) # %>% invisible()

# library(cowplot)
# library(ggthemes)      # or install_github('cttobin/ggthemr') #library(themr) # ggthemr("<theme name>") #ggthemr_reset()
# library(plotly)        # plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO", label_size=12, align="v, scale = c(1, 1, .85, .9))


theme_set( theme_bw()  +  theme( legend.position = "top" )   )
library(ggpubr)          # with plotgrid, and cowplot


####### Load Data ###################
## load or 
## input(loadpackages) 

#setwd("/Users/gk/Google Drive")   #  
#ls()  
#list.files()  #ls("/Users/gk/Google Drive/setouts1819final.csv")
#list.files("/Users/gk/Google Drive/setouts1819final.csv")
#setoutbak <- read_csv("/Users/gk/Google Drive/setouts1819final.csv") #  OK !
# write_excel_csv(x, path, na = "NA", append = FALSE, col_names = !append)

#readxl::read_xlsx("/Users/gk/Google Drive/setouts1819final.csv", sheet = 1)
#writexl::write_xlsx(setoutdata1419)
#readxl::read_excel( "/Users/gk/Google Drive/allsetout.xlsx",  sheet= 1,  skip = 1, col_names = TRUE, range = NULL)      # cf ""
# or                       ::read_xlsx(    filepath                   ,    sheet = NULL, skip = 0, col_names = TRUE, range = NULL, ..
# , col_types = NULL,  na = "", trim_ws = TRUE,  n_max = Inf, guess_max = min(1000, n_max)   )
#  range = NULL, col_types = NULL,  na = "", trim_ws = TRUE,  n_max = Inf, guess_max = min(1000, n_max)  )


# left_join(x, y) : returns all rows from x,        and all columns from x and y.
# inner_join(x, y): returns all rows from x where matching values in y, and all columns from x , y

########### 


library(googlesheets4)   

ss = "https://docs.google.com/spreadsheets/d/1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg/edit#gid=1931748367"
sheet_names(ss) 

# googlesheets4::sheet_add() sheet_append() sheet_copy() sheet_delete() sheet_write() sheet_properties() sheet_rename()
# googlesheets4::read_sheet( ss, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, skip = 0, na = "", trim_ws = TRUE )
    # , n_max = Inf  ,  guess_max = min(1000, n_max),  .name_repair = "unique" )

ItemMaster2021 <-   googlesheets4::read_sheet(ss, sheet = "ItemMaster2021" , col_types = "c"  )  # col_types = "ccilDD"
VR2021Jan      <-   googlesheets4::read_sheet(ss, sheet = "VR2021Jan"      , col_types = "c"  ) 
ItemMaster2021
mydata1 <- ItemMaster2021
mydata2 <- VR2021Jan
mydata2  %>% View()

ss2 = "https://docs.google.com/spreadsheets/d/1tDxKHHOia7KC-x5b5SHLUOZPsTbxy6O1Gxa7Abo9hDg/edit#gid=784151696"
sheet_names(ss2)  # "QAM" "QAS" "QAC" "QAE"

itemmaster_mech  <-  googlesheets4::read_sheet(ss2, sheet = "QAM" , skip = 2 , col_types = "c", trim_ws = TRUE)
itemmaster_st    <-  googlesheets4::read_sheet(ss2, sheet = "QAS" , skip = 2 , col_types = "c", trim_ws = TRUE)
itemmaster_civil <-  googlesheets4::read_sheet(ss2, sheet = "QAC" , skip = 2 , col_types = "c", trim_ws = TRUE)
itemmaster_elect <-  googlesheets4::read_sheet(ss2, sheet = "QAE" , skip = 2 , col_types = "c", trim_ws = TRUE)

itemmaster_mech$QAwing  <- "QAM" 
itemmaster_st$QAwing  <- "QAST"
itemmaster_civil$QAwing  <- "QAC"
itemmaster_elect$QAwing  <- "QAE"
itemmaster_elect %>% View()

str(itemmaster_mech)
itemmaster_mech$item_name %>% unique() %>%  View()
#?itemmaster_mech %>%  unique(.item_name) %>%  View()

allnames <- c(a=names(itemmaster_mech),
                           b= names(itemmaster_civil), 
                           c= names(itemmaster_st), 
                           d=names(itemmaster_elect) , .id = "id")
itemmaster_civil %>% 
                    full_join( itemmaster_elect) %>% 
                    full_join( itemmaster_mech) %>% 
                    full_join( itemmaster_st) %>% 
                    View()
# Filtering joins: filter out rows in df1, don't modify columns
# anti_join() : all rows of (left) x with no match in y.  ; semi_join is reverese where there is match : filters x table
# left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

# Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
# Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
# Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
# Cross join: merge(x = df1, y = df2, by = NULL)
# by = c("CustomerId", "OrderId"). for multiple variables
# by.x = "CustomerId_in_df1", by.y = "CustomerId_in_df2"
# merge(x=df1,y=df2, by.x=c("x_col1","x_col2"), by.y=c("y_col1","y_col2"))

library(sqldf)


df3 <- sqldf("SELECT CustomerId, Product, State 
              FROM df1
              JOIN df2 USING(CustomerID)")    ## inner join

df4 <- sqldf("SELECT CustomerId, Product, State 
              FROM df1
              LEFT JOIN df2 USING(CustomerID)") ## left join (substitute 'right' for right join)

# library(data.table)
# dt1 <- data.table(df1, key = "CustomerId")
# dt2 <- data.table(df2, key = "CustomerId")
# joined.dt1.dt.2 <- dt1[dt2]
## df1[,"State"]<-df2[df1[ ,"Product"], "State"]

# dplyr  union(), union_all() , can also by merge() and rbind().
# union_all(df1,df2) with all
# union(df1,df2)     without duplicates
# df_unionall = rbind(df1,df2) 
# df_union1 = merge(df1,df2,all=TRUE)
# union with rbind() then unique()
# bind_cols( ..., .name_repair = c("unique", "universal", "check_unique", "minimal")  )

# gather(), spread(), separate(), unit()
# separate(data, col, into, sep= "", remove = TRUE)  #-into: The name of the new variables
# unit(data, col, conc ,sep= "", remove = TRUE)
(spec <- table(iris$Species))
waffle::waffle(spec)
waffle::waffle(spec, rows = 3, legend_pos = "bottom")
waffle::waffle(spec, rows = 15, colors = c("lightgrey", "darkgrey", "red"))
waffle::waffle(spec / 10, rows = 5, xlab = "1 square = 10 flowers")# do not forget to annotate 1 sq = 10 units!

waffle::iron( waffle::waffle(spec / 5, rows = 5, title = "iron() combines waffles"),
              waffle::waffle(spec / 10, rows = 5, xlab = "1 square = 10 flowers")  )  # combiing waffles with waffle::iron

#  replace the tiles by pictures from the extrafont package. Plus ggwaffle developed now
#  swarmplot, or beeswarmplot, and is already hosted on CRAN in the form of the ggbeeswarm package
# install.packages("ggbeeswarm")
library(ggbeeswarm)  # geom_quasirandom and geom_beeswarm, official ggplot2
#### theme_set(theme_light()) # sets a default ggplot theme
ggplot(iris, aes(Species, Sepal.Length)) + ggbeeswarm::geom_beeswarm()
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_beeswarm(size = 2)
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_beeswarm(size = 3, cex = 3)  # cex to adjust spacing for large data
#  geom_beeswarm(groupOnX = FALSE)  to turn off separation
# For another grouping variable besides those on the axis,  consider using the dodge.width
ggplot(iris, aes(Species, Sepal.Length, col = Sepal.Length > 5)) + geom_beeswarm(dodge.width = 0.5)
# geom_quasirandom, alternative geom_jitter
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_quasirandom()
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_quasirandom(size = 2, method = "smiley") # !! it allows many patterns!


joined.dt1.dt.2 <- dt1[dt2]
itemmastercris <- bind_rows(itemmaster_mech, itemmaster_st, itemmaster_civil, itemmaster_elect  )

itemmastercris %>% View()

######## Data Exploration #####

# clean_names() with default naming
janitor::clean_names()    # snake case  or clean_names(case = "small_camel") 
janitor::get_dupes()      # identify duplicated records
janitor::remove_empty(c("rows", "cols"))
janitor::round_to_fraction()
janitor::excel_numeric_to_date(41103.01, include_time = TRUE) # convert_to_datetime()
janitor::row_to_names()
# generate frequency table

# tabulate and arrange data
janitor::tabyl(mydata1, c(2,3,4) )  %>% janitor::adorn_totals()  
val <- c("hi", "med", "med", "lo")
janitor::tabyl(val)   
# formatting
janitor::adorn_totals() %>% 
janitor::adorn_pct_formatting() %>% 
rmarkdown::paged_table()

# Data Manipulation

# filter data
dplyr::filter(    brand   == "Starbucks", 
                  country == "US", 
                  state_province != "AK", 
                  state_province != "HI" ) 
# rowid_to_column()
#setdiff(b,a)  ;  setdiff(a,b)  ; intersect(a,b)  # common in a and b




# plot missing data (using raw data)
library(DataExplorer)
DataExplorer::plot_missing(
    title   = "% of Missing Data (filtered to cols w/missing data)",
    data    = coffee_chains_raw,
    ggtheme = tidyquant::theme_tq(), 
    missing_only = TRUE) 


library(summarytools)

summarytools::dfSummary(RDSOmech)
view(dfSummary(RDSOmech))   # mix up of mdy and dmy

# freq(RDSOmech$x4, plain.ascii = FALSE, style = "rmarkdown")
# print(ctable(tobacco$smoker, tobacco$diseased, prop = "r"), method = "render")

RDSOmech %>% distinct()   %>%   mutate(ICMS = NA, CRO = NA  ) %>% View() #, pldate = mdy(pldate)  )  %>% 
################################################################################################################

colnames(RDSOmech)
setdiff(  colnames(RDSOmech), colnames(RDSOcivil) )

RDSOallitems <- bind_rows(RDSOmech, RDSOsig,  RDSOcivil,  RDSOelect )

RDSOallitems %>% View()
str(RDSOallitems)

### summarise data and infrence ###########################
# RDSOallitems %>% group_by(group,     var1)    %>% tally()

RDSOallitems %>% group_by(mech,   cat)    %>% tally()  # works
RDSOallitems %>% count( "SHED", "failcat")   %>% View() 
RDSOallitems %>% add_count(SHED, failcat)  %>% select(SHED, n) %>% arrange(desc(n))

add_count()



####################################################
plot(RDSOallitems$Value)

ggplot( RDSOallitems, aes(x=Datets, y=freq)) + 
                   geom_jitter(col = "blue") +    
                   scale_x_datetime(breaks=date_breaks("1 year")) + ylim(0, 25)



# Alluvial diagaram
library(ggfortify)
autoplot(RDSOallitems) +    # time series element
    labs(title="RDSOallitems") +               
    theme(plot.title = element_text(hjust=0.5))


#####################################
# exploration
library(summarytools)
descr(RDSOallitems, style = "rmarkdown")   # stats = "common", transpose = TRUE, headings = FALSE) for summary
view(dfSummary(RDSOallitems))              #  view(dfSummary(nrshedsetouts))

library("DataExplorer")
create_report(RDSOallitems)  # All consolidated reports
introduce(RDSOallitems)
plot_intro(RDSOallitems)
plot_missing(RDSOallitems)
plot_bar(RDSOallitems)
plot_correlation(na.omit(RDSOallitems), maxcat = 5L)

data_list <-  list( RDSOallitems,  RDSOallitems )   # list of databases to visualise structure
plot_str( data_list )

# install.packages("ExPanDaR")
# library(ExPanDaR) interactive no use

library(esquisse)   #helps in launching the add-in graphs drag and drop!
esquisse::esquisser() 

RDSOallitems %>% add_count(SHED)  %>% View()  # adds n column
RDSOallitems
RDSOallitems %>% add_tally()  %>% View()  # adds total count colun else use %>% tally()
#?RDSOallitems %>% group_by("mech") > % filter( !is.na("RDSOallitems$SN") ) %>% tally(sort = TRUE)


#############
RDSOallitems
names(RDSOallitems)
library(googleVis)
library(plyr)
gg<- gvisMotionChart(RDSOallitems,    # Your data has 9736 rows, but idvar and timevar only define 4338 unique rows.
                     idvar   = "type", #"country",
                     timevar = "Date", #"year",
                     xvar    = "Date", #"GDP",
                     yvar    =  "where" ,   "Life Expectancy",
                     sizevar =   2, #    "Population",
                     colorvar =  "Dept" #  "region"
)

plot(gg)

cat(gg$html$chart, file="chart1.html")


###### examine

RDSOallitems %>% filter(!is.na(Date) ) %>% group_by(Date)  %>% # year(Date), month(Date)  )  %>%
    summarise(   n = n()    )    ->    RDSOvanalyise  # , !is.na(ICMS) 
RDSOvanalyise
## learnpoint  df %>% summarize(function(colname[conditional_colname])) 

#colnames(nrsetouts)  <- c("sumdates", "totalcases" )

RDSOvanalyise  %>% View()
str(RDSOvanalyise)

RDSOvanalyise 



RDSOvanalyise  %>%    ggplot( aes(x = Date, y = n) )   +  theme_bw()  +
    geom_jitter(  aes(x = Date, y = n, color =  as.factor( year(Date)  )    ) ,  size =3.5 , alpha = 0.25 )  +
    #  scale_size(range = c(0.5, 12)) + # Adjust the range of points size 
    geom_smooth( method = "loess" ) + #method=lm,     se=TRUE )  +   # color, size ,linetype , fill 
    ## auto", se=TRUE, fullrange=FALSE, level=0.95 (confidence),  method = “loess”(small obs, lm = liniar model),  method = "lm", formula = y ~ poly(x, 3), se = FALSE)
    # "auto", "lm", "glm", "gam", "loess" 
    
    geom_hline(yintercept = 12, size = 0.8, colour="black", alpha = 0.8 )  + 
    geom_vline( xintercept =  as.Date("2016-08-02") , size = 0.5, colour="black", alpha = 0.5 , text=element_text(size=6) )  +  # as Traction department
    geom_text(aes(x=as.Date("2016-08-02"), label= "2 Aug 2016", #" paste0( " Changeover date ", "2016-08-02") , 
                  y=15 ),  angle=90, vjust = -1 , alpha = 0.2, text=element_text(size=10) ) + # , colour="blue" , text=element_text(size=11)
    #geom_text(aes(x=as.Date("2016-08-02"), label="Traction Dept",   y=15), colour="green", angle=90, vjust = 1.2, text=element_text(size=11) ) +
    geom_vline( xintercept =  as.Date("15/12/2017")   )  +  # Elect loco not coming
    #? scale_fill_discrete(name = "Legends", labels = c("A", "B", "C")) +
    # ?labs(fill = "Dose (mg)") +  # ???
    geom_abline()  +
    #annotate("text", x = ymd("2017-11-30"), y = 20, label = "italic(LocoFail) ^ 2 == 0.75", parse = TRUE) +
    annotate("text",  x = ymd("2018-03-31"), y = 11, label = "Elect Locos introduced in Diesel Shed", parse = FALSE) +
    
    #  theme(legend.title = element_blank() )  +
    # scale_x_discrete(limits = c("2", "0.5", "1" ))  +
    scale_fill_discrete(name = "Years", labels = c("A"," B", "C")) +
    # scale_fill_viridis_d() +
    #  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    # ? theme(legend.position = c(0.7, 0.2),  legend.direction = "horizontal") +
    # ?facet_grid(~ as.factor(year(nrsetouts$sumdates) )  ) +
    # theme(legend.position = "top" )  +
    rotate_x_text(45) +
    theme(legend.position = c(0.7, 0.8),   legend.direction = "horizontal") +
    labs(title = "Temperatures\n", x = "Date [°Y-M-D]", y = "Total Loco Troubles", color = "black") +
    labs(   title    = " All Diesel loco trouble reported over last five years",          
            subtitle = " Total Daily Diesel loco traubles reported ", 
            caption  = " (Details of each single case is avialble in shareable googlesheet link)"  ,  
            color =  " Year ",  
            size =  "size" , 
            alpha = " Size"  )   #  +
# guides( color=guide_legend("my title"), fill = guide_legend(reverse = TRUE) )  # add guide properties by aesthetic  



#labs(x="miles per gallon", y="displacement", size="horsepower",  col="# of cylinders", shape="# of gears")

library(GGally)
ggpairs(RDSOvanalyise[,2])+ theme_bw()

library(factoextra)
RDSOvanalyise[ , 5:5 ] %>%
    scale() %>%                           # Scale the data
    dist() %>%                            # Compute distance matrix
    hclust(method = "ward.D2") %>%        # Hierarchical clustering
    fviz_dend(cex = 0.5, k = 4, palette = "jco") # Visualize and cut 

#######
ggplot(iris, aes(Sepal.Length)) +
    geom_histogram(aes(fill = Species, color = Species), bins = 20,  position = "identity", alpha = 0.5) +
    scale_fill_viridis_d() +
    scale_color_viridis_d()

############# second graph


RDSOvanalyise  %>%  ggplot() +
    geom_point(  aes(x = Date, y = n,  col = ifelse(Date > dmy("1/08/2016"), "red", "blue")  )  )  + 
    geom_smooth( aes(x = Date, y = n), method = "loess" ) +
    # scale_x_continuous(breaks = pretty(x, n = 10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    scale_color_manual(values=c('#999999','#E69F00','#56B4E9'),  guide=FALSE)

    geom_smooth( method = "loess" ) + #method=lm,     se=TRUE )  +   # color, size ,linetype , fill 
    ## auto", se=TRUE, fullrange=FALSE, level=0.95 (confidence),  method = “loess”(small obs, lm = liniar model),  method = "lm", formula = y ~ poly(x, 3), se = FALSE)
    # "auto", "lm", "glm", "gam", "loess" 
    geom_hline(yintercept = 12, size = 0.8, colour="black", alpha = 0.8 )  + 
    geom_vline( xintercept =  as.Date("2016-08-02") , size = 0.5, colour="black", alpha = 0.5 , text=element_text(size=6) )  +  # as Traction department
    geom_text(aes(x=as.Date("2016-08-02"), label= "2 Aug 2016", #" paste0( " Changeover date ", "2016-08-02") , 
                  y=15 ),  angle=90, vjust = -1 , alpha = 0.2, text=element_text(size=10) ) + # , colour="blue" , text=element_text(size=11)
    #geom_text(aes(x=as.Date("2016-08-02"), label="Traction Dept",   y=15), colour="green", angle=90, vjust = 1.2, text=element_text(size=11) ) +
    geom_vline( xintercept =  as.Date("15/12/2017")   )  +  # Elect loco not coming
    #? scale_fill_discrete(name = "Legends", labels = c("A", "B", "C")) +
    # ?labs(fill = "Dose (mg)") +  # ???
    geom_abline()  +
    theme(legend.title = element_blank()) +
    # ? theme(legend.position = c(0.7, 0.2),  legend.direction = "horizontal") +
    # ?facet_grid(~ as.factor(year(nrsetouts$sumdates) )  ) +
    theme(legend.position = "top" )  +
    labs(title = "Temperatures\n", x = "Date [°Y-M-D]", y = "Total Loco Troubles", color = "black") +
    labs(   title    = " All Diesel loco trouble reported over last three years",          
            subtitle = " Total Daily Diesel loco traubles reported ", 
            caption  = " (Details of each single case is avialble in shareable googlesheet link)"        )


# p + annotate("text", x = 4, y = 25, label = "Some text")
# p + annotate("text", x = 2:5, y = 25, label = "Some text")  # multi same lables
# p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2")) # multi lables

# p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,  alpha = .2) # rect area zone
# p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,  colour = "blue")   # Line segment
# p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,  colour = "red", size = 1.5) # error bar
# p + annotate("text", x = 4, y = 25, label = "italic(R) ^ 2 == 0.75",  parse = TRUE) # parse to commands
# p + annotate("text", x = 4, y = 25, label = "paste(italic(R) ^ 2, \" = .75\")", parse = TRUE)
# 

# p + labs(). Change legend title.   Use p + labs(fill = “dose”) for geom_boxplot(aes(fill = dose))    
# p + scale_x_discrete(limits = c(“2”, “0.5”, “1”)). Change the order of the item in the legend.
# p + scale_fill_discrete(name = “Dose”, labels = c(“A”, “B”, “C”)). Rename the legend title and text labels.    
#########################
# Data table

ggplot() + theme_nothing() + annotate("text", x = 4, y = 12, cex=12.5, label = " ") +
    #annotate("text", x = 4, y = 13, cex=12.5, label = "Second ") +
    # geom_label(aes(fill = factor(cyl)), colour = "white", fontface = "bold")  +
    #geom_label(" this label") + 
    text(4,8,"big",cex=1.5)
# 
geom_label_repel(  aes( label = rownames(df), fill = factor(cyl) ), color = 'white',  size = 3.5 )
geom_text_repel(   aes(label = rownames(df)),                                         size = 3.5 )

text(4,4,"big",cex=1.5)


RDSOvanalyise1 <- left_join(d1, d2, by = c("rowid" = "rowid")  )
setouts

summarytools::dfSummary(RDSOmech) %>% view()
freq(RDSOvanalyise$Date, plain.ascii = FALSE, style = "rmarkdown")
view(dfSummary(RDSOvanalyise))

# pivot_longer(- rowid, -Date, -where, -Loco, -Shed,  names_to = "attribute", values_to = "atvalue")
gather(- rowid, -Date, -where, -Mech, -Cat,  key = "attribute", value = "atvalue" ) %>% 
    View()


RDSOvanalyise %>% group_by( year(Date) ) %>%  summarise(n =)


pivot_longer(-religion, names_to = "income", values_to = "count")
pivot_longer(`2000`:`2017`, names_to = "year", values_to = "value")
pivot_wider(names_from = variable, values_from = value)

RDSOvanalyise %>% pivot_wider(names_from = SHED, values_from = count() )

# mutate usefulfn  +, - etc, log(), lead(), lag(), 
# , row_number(),  percent_rank(), ntile(), dense_rank(), min_rank(),
# cumsum(), cummean(), cummin(), cummax(), cumany(), cumall() , cume_dist()
# na_if(), if_else(),  case_when(), recode(), coalesce(), 
Sys.getlocale("LC_TIME")

slice(setoutdata1819clean, 51:60)
setoutdata1819clean[51:60,   ]
nth(setoutdata1819clean, n = 51)
select(arrange(bison, desc(year)), year, ITISscientificName)
bison <- mutate_each(bison, funs(tolower), contains("name"))

#??str_detect( lf18final$"Brief actual cause", lock)     
lf18final %>% recode(  ICMS = "yes", YES = "yes" ) %>% View()
#recode(num_vec, "a", "b", "c", .default = "other")
mtcars %>%    mutate( mpg = replace(mpg,  cyl==4, NA)   ) 


#mutate(x = replace(x, x<0, NA))
#mutate(mpg=replace(mpg, cyl==4, NA)) %>%

# df[df$height == 20, "height"] <- NA
# df %>% mutate(height = replace(height, height == 20, NA))
# df %>% mutate(height = ifelse(height == 20, NA, height))
# mutate(X25 = ifelse(X25 == "Other", Other, X25))
# mutate(mpg=replace(mpg, cyl==4, NA)) %>%
#  mutate(x = replace_na(x, 0))



#################################################################
#################################################################
## Base summery
base::summary(data)
by(data, data$SHED, summary)

# with  group, apply multipel functoin to one variable
mtcars %>%  group_by(cyl) %>%  summarise_each( funs(min_mpg = min,  max_mpg = max),   mpg   )
# more compact with one funtion summerise_each to many variables,  provide  
mtcars %>%  group_by(cyl) %>%  summarise_each( funs(mean), mpg, disp)

# apply many functions to many variables  - or less if want, but give names to vars
mtcars %>%   group_by(cyl) %>% 
    summarise_each(funs(min, max) , mpg, disp) %>%
    setNames(c("gear", "min_mpg", "min_disp", "max_mpg", "max_disp"))


dim( na.omit(RDSOvanalyise) )

summary(aggr(lf18final, sortVar=TRUE))$SHED

matrixplot(RDSOvanalyise, sortby = 4)

marginplot(RDSOvanalyise[,c("SN","ICMS")])


library(GGally)
ggpairs(data)
ggpairs(data, mapping = aes(colour = category))

library(descriptr)
ds_summary_stats(data$score)
ds_screener(data)


library(naniar)
gg_miss_var(lf18final)
getAnywhere(gg_miss_var())

##### usingg Hmisc
Hmisc::describe(data)


library(psych)
psych::describe(data)
psych::describeBy(data, data$type)
psych::describeBy(data, data$type, mat = TRUE)  # matrix of output

library(skimr)
skim(data)
skim_with(integer = list(hist = NULL))

data %>% group_by( SHED)  %>%     skim()

##########################################
library(summarytools)
summarytools::descr(data)
summarytools::descr(data, transpose = TRUE)

dfSummary(data)           # Better 
view(dfSummary(data))     # (view) View in HTML note small v in view

dfSummary( data, 
           plain.ascii = FALSE, 
           style = "grid", 
           graph.magnif = 0.75, 
           valid.col = FALSE, 
           tmp.img.dir = "/tmp" )   # for markdown printing



library(GGally)
data[, 1:10]  -> data1 #  %>% View()
ggpairs(data1)
ggpairs(data1, mapping = aes(colour = SHED), cardinality_threshold= 2000)   # ??? unable 


library(descriptr)
ds_summary_stats(data$score)


library(VIM)
matrixplot(data, sortby = 2)
marginplot(data[ ,  c("SHED","Loco")]  ) 
aggr(data,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)  # ok
summary(aggr(data,prop=TRUE,combined=TRUE))$combinations 



library(prophet)
data %>% filter(!is.na(Date)) %>% group_by(`Date`) %>% mutate(ds = dmy(Date ) )  %>% summarise( y = n() ) -> t
m <- prophet(t)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)           # some how plotted
prophet_plot_components(m, forecast)


##############################################################################
##############################################################################
data <- setoutdata1819clean

gs_new(  title = "setouts1819final",  ws_title = "setouts1819final",   input = setoutdata1819clean,  verbose = TRUE , trim = TRUE) #col_extent = NULL,


# or faster with gs_upload
write_csv( setoutdata1819clean , "/Users/gk/Google Drive/setouts1819final.csv",    na = "NA", append = FALSE)
gs_upload("/Users/gk/Google Drive/setouts1819final.csv", sheet_title = NULL, verbose = TRUE, overwrite = TRUE) # faster

gs_ls("setou")  
gs_ls("setou")$sheet_title    # # yes above done


# lf18finalhandle <- gs_title("setouts1819fina.csv", ws = 1, skip = 0, cpl  ) # it was csv
# read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", ...)

#lf18finalread <- read.csv("/Users/gk/Google Drive/setouts1819final.csv", header = TRUE,  quote = "\"", skip =0, dec = "."  ) 
#, fill = TRUE, col.names = c(), stringsAsFactors = TRUE, strip.white = TRUE, sep = ",", nrows	= 4636, check.names = TRUE,
# row.names = col vector or idcolumn, na.strings ="NA"

setout1819finalread <- read_csv("/Users/gk/Google Drive/setouts1819final.csv", col_names = TRUE, locale = default_locale(),
                                skip = 0, n_max = Inf, na = c("", "NA"), trim_ws = TRUE )
spec(setout1819finalread) 
glimpse(setout1819finalread)
str(setout1819finalread)
setout1819finalread  %>% View()

setouts1819 <- setout1819finalread


# or using gs_read
mtcars_sheet <- gs_new(title = "mtcars", ws_title = "first_sheet", input = mtcars)
#insert the rownames vertically in column B as anchor B2
gs_edit_cells(mtcars_sheet, ws = "first_sheet", anchor = "B2", input = rownames(mtcars), byrow = FALSE)
######################

################# Now we have data in lf18finalread  ####### 
#################  lf18finalread  ####### ####### ####### 

setouts1819  %>% View()
#??setouts1819 %>%  summarise (min_mpg = min(mpg), max_mpg = max(mpg), mean_mpg = mean(mpg), mean_disp = mean(disp))
colnames(setouts1819)
setouts1819p1   <-   setouts1819  %>% select( ., 
                                              `Date` , `Loco`, SHED, System, pltype, ICMS,  # `PL RESP`,
                                              `Train`,
                                              "Brief FIR", 
                                              "Failreason" , 
                                              "Brief main sub assembly",
                                              `System`,
                                              #`Brief RESP cause`,
                                              # everything() ,
                                              `Responsibilty`   )    %>%   
    # mutate( resp1 =  paste0(SHED, `Loco no.` )  )    %>%
    mutate( MU = ifelse( str_length(`Loco`)  > 5 ,  "MU",  "SU" )  )  %>%
    mutate( HHP = ifelse( str_sub(`Loco`, 1, 2)  %in% c("12", "40", "70") ,  " HHP" , "ALCO" ) ) %>% 
    filter(SHED %in% c("AMV", "LKO", "TKD", "LDH"  ) )  %>%
    arrange(System, `Loco`, Date)   %>%
    # filter(!is.na(Date) )   %>% 
    select(`Date` , `Loco`, `MU`, `HHP`, everything() )       


View(setouts1819p1) 
view(dfSummary(setouts1819p1))     # (view) View in HTML note small v in view


count(setouts1819$Loco) 

setouts1819p1 %>% group_by(ymd("Date"))  %>% summarise( countsn =  count( ymd("Date") )   )

setouts1819 %>% group_by("ICMS") %>% summarise_each (funs(mean) , mean_mpg = mpg) #summarise( mean_mpg = n()   )
str()

setouts1819 %>%  filter( pltype == "DIR",  SHED %in% c( "TKD", "LDH", "LKO", "SSB")   ) %>%
    group_by(pltype, SHED) %>%  summarise( pl = n()) 

punctuality1819

# Days of failures

setouts1819 %>%
    tidyr::extract(`SCH.`, into = c("DOCe"),  "DOC-(\\d{1,2}.\\d{1,2}.\\d{2})"  , remove = FALSE  ) %>%
    tidyr::extract(`SCH.`, into = c("POHe"),  "POH-(\\d{1,2}.\\d{1,2}.\\d{2})"  , remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("M24e"),  "(M24-\\d{2}.\\d{2}.\\d{2})"      , remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("Monthe"),"(M\\d{1,2}-\\d{2}.\\d{2}.\\d{2})", remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("Te"),  "(T\\d{0,3}-\\d{2}.\\d{2}.\\d{2})"  , remove = FALSE  ) %>% 
    mutate( DOC = dmy(DOCe)   )      %>% 
    mutate(POH = dmy(POH)  )   %>%
    mutate( M24 = dmy(`M24/M72/M48/6Y`)  ) %>% 
    mutate( T90 = dmy(`T/T90`)  ) %>% 
    mutate( T   = dmy(`T/T90`)  ) %>% 
    mutate( Td   = dmy( str_sub(Te,-8,-1))    ) %>% 
    mutate(DOCdays = Date - DOC)   %>%
    mutate(Tdays = Date - Td )   %>%
    select(`Loco no.`, `SCH.`,DOC, DOCe, DOCdays,   T, Tdays,
           POH,
           M24,M24e,
           T90,
           
           Monthe, Te, 
           everything())   ->  Td #    %>%     View()
glimpse(Td)

Td %>% filter(Tdays < 120 , Tdays > 0 ) %>%  arrange(Tdays) -> Td
plot(Td$Tdays)


View(setouts1819)


##########
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library(survival)   # essential for fitting
library(survminer)  # for cutomisable plot with table
lung
fit <- survfit(Surv(time, status) ~ 1, data = lung)



##
library("googlesheets4")

ss = "https://docs.google.com/spreadsheets/d/10aeSJpy43V-Ou2caF5cl5njxjV7bxkZXIFaUz6F3W0Q/edit#gid=169651091"
sheet = "dataCR"
empaddatacr <- googlesheets4::read_sheet(ss, sheet,
                           range = NULL,
                           col_names = TRUE,
                           col_types = "cccDDccc", # NULL,
                           na = "",
                           trim_ws = TRUE,
                           skip = 1,
                          # n_max = Inf,    guess_max = min(1000, n_max),
                           .name_repair = "unique")

names(empaddatacr)
empaddatacr$status <- 1 


empaddatacr %>% View()
# empaddatacr %>% mutate( Faildate2 = as.Date(empaddatacr$FailDate, format= "%m/%d/%Y"),
#                         makedate = as.Date(empaddatacr$MFdate, format= "%m - %Y")
#                         ) #, days = (mdy(empaddatacr$FailDate) - MFDate)
# df %>%  mutate(date = format(ymd(readr::parse_number(str), truncated = 2), '%m-%Y'))
# parse_date(empaddatacr$MFDate, orders = c("dmy", "mdy"))

empaddatacr %>% mutate(days = (   FailDate - MFDate)  , days2 = as.numeric(days) ) %>% 
                filter(days2 >0, days2 < 900)   -> empadsurv
empadsurv 

# same empaddatacr %>% mutate(days = difftime( FailDate, MFDate , units = "days")   )
# difftime("2020-5-16", "2020-1-15", units = "days")  #weeks

empadsurv


# replcating not failed data: df[rep(seq_len(nrow(df)), times = 2), ]


empadsurvfail <- 

fit <- survfit(Surv(days2, status) ~ 1, data =empadsurv)


# Drawing curves
ggsurvplot(fit, color = "#2E9FDF")

Td$status <- 2
#Td$status[21:nrow(Td) ] <- 1
Td

fit <- survfit(Surv(Tdays, status) ~ 1, data = Td)

ggsurvplot(fit, color = "#2E9FDF")


fit<- survfit(  Surv(Tdays, status)  ~  HHP,     data = Td   )  # multi Surv(time, status) ~ rx + adhere,
ggsurvplot(fit,
           #fun = "event",
           #fun = "cumhaz",
           fun = function(y) y*100,   # arb fn
           #palette = c("#E7B800", "#2E9FDF"), # custom color palette
           #palette = "Dark2",
           conf.int = TRUE, # Add confidence interval
           pval = TRUE,  # Add p-value,   3sigma  p< 0.0001
           #size = 3,  # change line size
           xlab = "Time in days",   # customize X axis label.
           break.time.by = 20, # break time axis by 250
           linetype = "strata", # change line type by groups
           # censor.shape="|", censor.size = 4, # shape of censor
           # legend.labs = c("A", "B", "C", "D", "E", "F")
           risk.table = TRUE, 
           risk.table.y.text.col = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           xlim = c(0, 100),
           risk.table.height = 0.25, # risk table height of total height
           risk.table.y.text = FALSE,# show bars instead of names in text annotations
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.25,
           conf.int.style = "step",  # customize style of confidence intervals
           # ? surv.median.line = "hv",  # add the median survival pointer.
           surv.median.line = "hv",  # add the median survival pointer from c("none", "hv", "h", "v")..
           ggtheme = theme_bw() # theme_light(), Change ggplot2 theme
)   -> lifeplot2

lifeplot2



# ggsurvplot() 
#ggsurvplot_list(),   ggsurvplot_facet(), ggsurvplot_group_by(), ggsurvplot_add_all(), ggsurvplot_combine()



# number of char in string  cchar(), nchar(NA, keepNA=TRUE) ,  str_length("foo")
########## model from https://rpkgs.datanovia.com/survminer/index.html ##
fit <- survfit(Surv(time, status) ~ sex, data = lung)
ggsurv <- ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    data = lung,             # data used to fit survival curves.
    risk.table = TRUE,       # show risk table.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE,         # show confidence intervals for point estimates of survival curves.
    palette = c("#E7B800", "#2E9FDF"),
    xlim = c(0,500),         # present narrower X axis, but not affectsurvival estimates.
    xlab = "Time in days",   # customize X axis label.
    break.time.by = 100,     # break X axis in time intervals by 500.
    ggtheme = theme_light(), # customize plot and risk table with a theme.
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.height = 0.25, # the height of the risk table
    risk.table.y.text = FALSE,# show bars instead of names in text annotations in legend of risk table.
    ncensor.plot = TRUE,      # plot the number of censored subjects at time t
    ncensor.plot.height = 0.25,
    conf.int.style = "step",  # customize style of confidence intervals
    surv.median.line = "hv",  # add the median survival pointer from c("none", "hv", "h", "v")..
    # group.by = " " ,
    # facet.by = " " ,
    # ?add.all,
    legend.labs = c("Male", "Female")    # change legend labels.
)
ggsurv


# Fit a Cox proportional hazards model
#fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, data = ovarian)
#ggforest(fit.coxph, data = ovarian)


library("ggfortify")

# Heatmap
df <- mtcars[, c("mpg", "disp", "hp", "drat", "wt")]
df <- as.matrix(df)
Td
Td[ , c("Loco no.", "Tdays" )]  # ??
df <-  Td[, c(as.factor("Loco no."), as.numeric("Tdays") )]   # ??

autoplot(scale(df)) # 

# autoplot() can handle also other time-series-likes packages, including:
# zoo::zooreg()  ; xts::xts() ; timeSeries::timSeries() ; tseries::irts() ; forecast::forecast() ; vars:vars()


library(changepoint)
autoplot(cpt.meanvar(AirPassengers))
library(strucchange)
autoplot(breakpoints(Nile ~ 1))

# Principal component analysis
df <- iris[, -5]   # Prepare the data
pca <- prcomp(df, scale. = TRUE) 
autoplot(pca, loadings = TRUE, loadings.label = TRUE,   data = iris, colour = 'Species')

# K means
autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3, frame = TRUE)

library(cluster)
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')

library(survival)
fit <- survfit(Surv(time, status) ~ sex, data = lung)
autoplot(fit)


autoplot(AirPassengers, ts.colour = 'blue') + 
    ggtitle('AirPassengers') + xlab('Year') + ylab('Passengers') 

set.seed(1)
autoplot(kmeans(iris[-5], 3), data = iris)   + scale_colour_brewer()

#########################################

setouts1819 %>%
    filter(., SHED %in% c("TKD", "LDH", "LKO", "AMV", "SSB")) %>%    
    filter(., !is.na(Date) )        %>%                            # remove indirect cases without date
    filter(ICMS == "YES")           %>%
    ggplot() + 
    geom_bar( aes(x= factor( month(Date, label = TRUE), 
                             levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ),
                  fill = SHED  ), 
              alpha = 0.5,  width=.80, position =  "dodge" )   +  facet_wrap(~SHED)  +
    # scale_fill_discrete(drop=FALSE) + 
    scale_x_discrete( drop=FALSE ) +       # to include factors with zero count
    theme( legend.position = "none" ) +  theme_bw(base_size = 11) +
    labs( x=   "Loco setouts per month" )                         +       
    labs(title    =  "Loco setouts per month in 2018-19")         +
    labs(subtitle =  "Loco setouts of NR sheds")                  +
    labs(caption  =  "(based on allsetout data sheet 21 march 2019...)" )    ->    gp1 
gp1

setouts1819  %>%  
    filter(., SHED %in% c("TKD", "LDH", "LKO", "AMV")) %>%      #View()
    # mutate(  Date = dmy(Date) ) %>%  
    # distinct()   %>% 
    #filter(!is.na(Date))  %>%          # remove indirect cases
    #filter(ICMS == "YES")  %>%
    ggplot() + 
    geom_bar( aes(x= parse_factor( month( Date, label = TRUE) ,     # lable to displays the abr name
                                   levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")    ),
                  fill = SHED ), 
              alpha = 0.5,
              width= 0.85, position = "dodge" ) + 
    scale_x_discrete(drop=FALSE) +       # to include factors with zero count
    facet_wrap(~SHED) +  
    coord_flip( ylim = c(3,50), xlim = c(1, 13) ) +            # better way for limiting graphs
    # coord_cartesian( ylim = c(3,50), xlim = c(1, 13) ) +      # months +na # removes scale_x_continuous(limits = c(-5000, 5000))
    # scale_color_viridis(trans="log")  +
    theme(legend.position = "none") + theme_bw(base_size = 10) +
    labs(x=" Loco setouts per month - NR Sheds")   + 
    labs(y=" setouts counts per month")   + 
    labs(title = "Loco setouts per month in 2017-18") +
    labs(subtitle = "Loco setouts per month of NR only") +
    labs(caption = "(based on set out data from 21/3/2019 sheet...)")        -> gp2 

gp2
##### plot grids  of plots #######

plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO")
# ? plot_grid(gp1, gp2, NULL, ggsurv, labels = "AUTO")


## cause wise analysis   #########################################
# ord_ages_class <- c(factor ordered list "", "", "")
# new_ages %>%   mutate(age_class = factor(age_class, levels = ord_ages_class)) #is.factor(), is.ordered()
# new_ages %>%   fct_relevel( ord_ages_class) %>% data.frame(age_class = .) -> plot_ages_forcats

#data$carb <- factor(data$carb, levels = data$carb[order(-data$mpg)])
#lfcause16$System  <- as.factor(lfcause16$System)
#lfcause16$System <- factor(lfcause16$System, levels = lfcause16$Date[ order(as.factor( lfcause16$System) ) ] )

#lf18 %>% mutate(System = factor(System,    levels = orderlevellist ))

setouts1819

setouts1819  %>%    select(., Date, `Loco no.`,  SHED, `System`,  
                           `Main sub assembly`, everything() )  %>%
    distinct() %>%                           # remove duplicate and blank
    filter(., !is.na(Date) ) %>%             # remove repercussions, 
    filter(., SHED %in% c( "LKO" , "TKD", "LDH" )) %>%    # , "TKD", "LDH"
    mutate( System =  factor(System ) )  %>%
    # filter(., `ALCO/ HHP` == "HHP" ) %>%
    # mutate( System = fct_lump(  System,  n = 30  )  ) %>%     #  lump to prop or n, or -n reverse
    # mutate( System = fct_recode( System, "Water Cooling System" = "WCS")  ) %>% # not reqd
    mutate( System = fct_collapse( System,
                                   `Power Assemblies` = c("PP", "WCS+PP", "PP+TSC", "PP/WCS", "PP/FOS"),
                                   `Air Brake System` = c("ABS", "CCB", "CCB1.5","CCB2.0"),
                                   `Compressor` = c("Comp", "Comp.", "COMP"),
                                   `Water Cooling System` = c("WCS", "wcs", "wcs"),
                                   `Fuel oil system` = c("FOS"),
                                   `Lube oil system` = c("LOS", "los"),
                                   `Turbo Super Charger` = c("TSC", "Turbo"),
                                   `Under Gear`      = c("U/G", "UG" , "u/g", "Under Gear"),
                                   `Other Mechanical`      = c("O/M"),
                                   `Radiator and assem.` = c(   "Rad & its assembly",
                                                                "Rad & assembly"       ),
                                   `Engine Governor` = c("Gov.", "Gov" , "GOV" ),
                                   
                                   `Traction Motors` = c("TM", "TM3" ),
                                   `Traction Alternator TG` = c("TA/CA/AG" , "TA/TG", "TA/CA", "TA", "Traction Alternator/ TG"),
                                   `Small Motors` = c("Small motor" , "Small Motor"),
                                   `CROW BAR`      = c("Crow bar", "CRO BAR "),
                                   `Convetors and invertors` = c("Cab", "Convetors and invertors", "Rectifier"),
                                   `Reversor & BKT`  = c("Rev. and BKT", 
                                                         "Rev and BKT", "Rev. & BKT", "BKT& REV",
                                                         "REV/BKT",     "Rev.& BKT", 
                                                         "Reversor & BKT", "Reversor", "Rev & BKT" ,
                                                         "REV &BKT","Reversor & BKT"),
                                   `Relay & Contactors` = c("Relay & Contactors", "Relay & contactor",
                                                            "Relay & CONTACTOR", "Contactors",
                                                            "Relay & Contactor"),
                                   `Power & Control Cables` = c("Power & CNL Cable","Power& CNL cable",
                                                                "Power & CNL cable", "Power &CNL cable","Power & cnl CABLE",
                                                                "Power & CNLcable" ,"Power& CNL cable" ),
                                   
                                   
                                   `Head Light or Flasher` = c("HL & FL", "HL& FL"),
                                   `Dynamic Brake System`  = c("Dy.Brake", "Brake chopper resistor"),
                                   Accidental = c("Accident"),
                                   `Switch & CCT Breaker` = c("Switch & CCT breaker", 
                                                              "Switch & cct breaker", 
                                                              "Switch & CCt breaker",
                                                              "Switch & Cct. breaker" ,
                                                              "Switch & Ccct breaker",
                                                              "Switch & Cct breaker"),
                                   `Batteries`  = c("Battries", "Battery"),
                                   `Control Stand` = c("CNL stand" , "TCC" ,  "TFT" ),
                                   `Other Electrical` = c("O/E", "o/e", "Resistance"),
                                   `Other Gen and Misc`  = c("Other gen" , "Other Gen")
    )   )  ->   lf18cause


levels(lf18cause$System)

lf18cause

# levels(droplevels(subdf$letters), reorder = FALSE)  # FALSE to preserve origibal order
# fct_infreq() %>%     fct_rev()  %>%     
# fct_reorder(lf18cause$System`,  lf18cause$Date  ,  .desc = TRUE) %>% 
# lf18t$System %>%  fct_relevel("Power & Cable", "Power & Control Cables") %>% levels()    


gp3  <- lf18cause  %>%   filter(HHP == "ALCO")  %>%  filter(!is.na(System))  %>%
    ggplot()    + 
    geom_bar( aes(  x =  fct_rev( fct_infreq( factor(System) ) ), fill = SHED ),  alpha = 0.8, position = "dodge") + 
    facet_wrap( ~SHED ) +   
    # facet_grid(HHP ~ SHED ) +  
    # scale_y_continuous(limits = c(0,50)) +   # ok but removes the plot altogether
    coord_flip( ylim = c(3,30) ) +  #, xlim = c(1, 13) ) +  
    # geom_text(  aes(x = System, y = System, label = System) ,  hjust = 0.50,  color = "#FF22FF" ) +
    theme_bw()  +
    labs(title = "The top 25 leading cause of loco troubles")   +
    labs(subtitle = " Analysis of set outs of all NR locos data in 2017-18")   +
    labs(x = "The cause wise loco failures/ troubles")  

gp3  + draw_image(
    "https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png",
    x = 5, y = 2.5, width = 12, height = 11.5
)

plot_grid(gp1, gp2, NULL, gp3, labels = "AUTO")


TKDlife
dplyr::lead(TKDlife)
####################################


# ggplotly(gp3)  # issues with Cairo
lf18cause

lf18cause      %>%       filter( !is.na(System) )  %>% 
    filter( SHED  %in% c("LDH", "TKD", "LKO")  ) %>% 
    group_by( System)  %>%
    summarise( tcases= n() )  %>%    #   View()   
    filter(tcases >5) %>%     
    ggplot( ) + coord_flip() +
    #?? scale_color_viridis(trans="log") +
    geom_bar( aes(x= reorder( System, tcases), 
                  y= tcases   ),
              position = "dodge", stat= "identity" )                

# ?? spread(lf18cause, System, tcases) %>%  t()   # transpose


# ?? lf18cause  %>%  filter(n> 0)  %>% #arrange(System) %>%
ggplot()    +   
    geom_bar(    aes(x = System, y= tcases), fill = `System` , stat = "identity"  )   +
    coord_flip() + 
    theme(legend.position = "none")

################################
p <-ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
p + geom_text()  +  annotate("text", label = "plot mpg vs. wt", x = 4, y = 35, size = 8, colour = "red")
p + geom_text(check_overlap = TRUE)
p + geom_text(aes( colour = factor(cyl))) + scale_colour_discrete(l = 40)
p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")), parse = TRUE)

p + geom_label(size = 3, family = "Times New Roman", aes(colour = factor(cyl)) )
p + geom_label(aes(fill = factor(cyl), size = wt), colour = "white", fontface = "bold" )

p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)

# Aligning labels and bars 
df <- data.frame(   x = factor(c(1, 1, 2, 2)),    
                    y = c(1, 3, 2, 1),
                    grp = c("a", "b", "a", "b") )

# ggplot2 doesn't know you want to give the labels the same virtual width as bars:
ggplot(data = df, aes(x, y, group = grp)) +
    geom_col( aes(fill = grp), position = "dodge") +
    geom_text(aes(label = y), position = "dodge")
# So tell it:
ggplot(data = df, aes(x, y, group = grp)) +
    geom_col(aes(fill = grp), position = "dodge") +
    geom_text(aes(label = y), position = position_dodge(0.9))
# Use you can't nudge and dodge text, so instead adjust the y postion
ggplot(data = df, aes(x, y, group = grp)) +
    geom_col(aes(fill = grp), position = "dodge") +
    geom_text(  aes(label = y, y = y + 0.05), position = position_dodge(0.9), vjust = 0  )


############ using farcats  ########
monthlevels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# using farcats
gss_cat %>% mutate(marital = marital %>% 
                       fct_infreq() %>%    
                       fct_rev()) %>%
    ggplot(aes(marital)) +  geom_bar()


# fct_recode() for variables specified
# Can assign multiple old levels to the same new level: !!!
# collapse a lot of levels, fct_collapse() is a useful variant of fct_recode(). 

gss_cat %>%
    mutate(partyid = fct_collapse(partyid,
                                  other = c("No answer", "Don't know", "Other party"),
                                  rep = c("Strong republican", "Not str republican"),
                                  ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                  dem = c("Not str democrat", "Strong democrat")
    )) %>%
    count(partyid)

# lump together all the small groups with fct_lump():  !!
# Avoid overcollapse, use the n parameter to specify how many groups (excluding other) we want to keep:
gss_cat %>%
    mutate(relig = fct_lump(relig, n = 10)) %>%
    count(relig, sort = TRUE) %>%
    print(n = Inf)

# use like  ?   data(tips, package = "reshape2")
# plot by factore if no factor by alphabatic
#make factores will plot levelwise tips2$day <- factor(tips2$day,levels = c("Fri", "Sat", "Sun", "Thur"))

# sorting numerical freq with reorder
ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")

#If you want to change the levels, 
#use `tips2$day <- factor(tips2$day,levels = c("First nice Name", "Second nice name", "jköljklj", "Thkljökljur"))`.

##################

#Center: mean(), median() ,  sd(), IQR(), mad(), min(), max(), quantile(), first(), last(), nth(),
#Count: n(), n_distinct(), any(), all()


lf18cause %>% group_by(SHED) %>%
    summarise(totalm  = n() , 
              mxdate = max(Date), 
              mndate = min(Date) , 
              tressd= sd(Date)   )  -> lf16pm

lf16pm

lf16pm %>% arrange( totalm ) %>% top_n( 10 , totalm ) %>%   #top_n(x, n, wt) wt =  variable for ordering
    ggplot(aes(  x= reorder(SHED, totalm) ,  y = totalm ) ) +
    geom_bar(  stat = "identity", fill = "dark red" , alpha = 0.2 ) +
    
    labs(title = "The top 10 highest failure sheds" )  +
    
    labs(y= " The no failures") +
    labs(x= "The major sheds") +
    coord_flip()  + theme_bw() +
    geom_text(  aes(label = totalm) ,   hjust = 01.50,  color = "#FF22FF" ) 
# scale_y_continuous(labels = scales::comma_format()) 



lf16pm
lf16pm %>% filter(year(mxdate) == 2017 |year(mxdate) == 2016 ) %>% 
    arrange( totalm ) %>% top_n(10, totalm ) %>%
    ggplot(aes(x = reorder(SHED, -desc(totalm)), y = totalm)) +
    geom_bar(stat = "identity", fill = "dark red") +
    labs(title = "Busiest container ports in the world") +
    labs(subtitle = '2014, in order of shipping volume') +
    labs(x = "Port", y = "Shipping\nVolume") +
    scale_y_continuous(labels = scales::comma_format()) +
    coord_flip()



#########################

mutate( fdate = coalesce(fdate, pldate ) )


#ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")

#tips2$day <- factor(tips2$day,levels = c("Fri", "Sat", "Sun", "Thur"))
#ggplot(tips2, aes(x = day, y = perc)) + geom_bar(stat = "identity")


shedwise %>%
    ggplot() + geom_bar(aes(x= fdate, fill = shed) ) + #, position = "dodge") + 
    scale_x_date( limits = as.Date(c("2016-04-01","2016-05-01")) ) +
    scale_x_date(position = "top") +
    # scale_x_date(breaks      = as.Date(c("2016-05-01", "2016-07-01")),
    #        minor_breaks = as.Date(c("2016-04-01", "2016-01-15", "2016-08-01", "2016-11-01"))) +
    # scale_x_date( expand = c(0, 10) )   +
    labs(title = "Title As on date 25 june" ) +
    labs(subtitle = "As on date 25 june" ) +
    #coord_flip() +
    theme_bw() +
    ggtitle("limits = as.Date(c(\"1980-01-01\",\"2000-01-01\"))") 


###########  tidyverse - tidr package gather, spread, unite, separate


stocks <- data.frame(    time = as.Date('2009-01-01') + 0:9,
                         X = rnorm(10, 0, 1),   Y = rnorm(10, 0, 2),  Z = rnorm(10, 0, 4)  )
stocks
stocksm <- stocks %>% gather(stock, price, -time)
stocksm
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)


shedwise
shedwise %>% #select(make, shed, locono, shedrly) %>%
    spread(., key = make , value = shed ) #??


# unite columns names by unite( "new collumn in quotes", fields, sep = "-etc" )
nlf16 %>% unite(col = "shedrly", shed, shedrly, sep = " of " )   # good worked

#separate(data, col, into, sep = "[^[:alnum:]]+")
#separate(my_data4, col = "Murder_Assault", into = c("Murder", "Assault"), sep = "-")
separate(nlf16, "fdate", into = c("YEAR", "Month", "day"), sep = "-")   #worked


##############

#?count(shedwise, locono) %>% group_by(shed) %>%  arrange(desc(n))



########### learn 
select()
# inside you ma use 
# starts_with(), ends_with(), contains(), matches(), num_range(), one_of(), everything()
#To drop variables, use -. ,  except for :, - and c(), all complex expressions are evaluated outside the data frame
# The three scoped variants of select() (select_all(), select_if() and select_at()) 
# three variants of rename() (rename_all(), rename_if(), rename_at()) 
# tibble::rownames_to_column(). to prserve colomn names





#########################
lf16  %>% 
    mutate( Date = as.Date(Date, "%m/%d/%Y")) %>%  
    mutate( 'PL-DATE' = as.Date('PL-DATE', "%m/%d/%Y")) 

str(lf16)
names(lfnr16) -> nlf
colnames(lfnr16)
nlf

lfnr16 <- select(lfnr16,  "Terrotory" , "PL", PLdate =  "PL-DATE" , "Date" ,  "Link owner", "Loco no."  , "SHED" )


lf <- dplyr::rename(lf16,  "PLDATE" = "PL-DATE"   )
View(lf)
lf <- select(lf, !is.na(SHED))


lf %>% 
    filter( SHED == "TKD" | SHED == "LDH" | SHED == "LKO" | SHED == "AMV" | SHED == "SSB" | SHED == "KLK") %>%
    ggplot() + geom_bar(aes(x=SHED, fill = SHED) ) + theme_bw()  # + coord_flip() 


mutate(lf16, Date = coalesce(Date, PL-DATE) )           #collese

lf16 <- distinct(lf16)

filter(lf16, !is.na(Date)  )

filter(lf16, is.na(Month))

filter(lf16, is.na(RLY) & is.na(SHED))

filter(lf16, is.na(SHED))

lfnr16 <- filter(lf16,  SHED == "TKD" | SHED == "LDH" | SHED == "LKO" | SHED == "AMV" | SHED == "SSB" | SHED == "KLK")

??lfnr16 %>% dplyr::rename( c( "as.Date(PLDATE)" = "as.Date(PL-DATE)" ) ) %>% View()

## workaround
names(lfnr16$`PL-DATE`) <- "PLDATE"
names(lfnr16)
lfnr16$`PL-DATE`
lfnr16$`Date`
ifelse(is.na(lfnr16$Date), lfnr16$`PL-DATE`, lfnr16$Date)

lfnr16 %>% mutate( Date = coalesce(Date, PL-DATE) ) %>% View()

spec(lfnr16)

?rename
# eg filter(!CARRIER %in% c("UA", "AA") & ORIGIN == "SFO" )
filter(lf16, !SHED %in% c("TKD", "LDH", "LKO", "AMV", "KLK" )  )

names(lf16)
colnames(lf16)
row.names(lf16)
rownames(lf16)

# rectify the data 
####### rectify data

# check if Date = PL-DATE
filter(lf16, !("Date" == "PL-DATE") )

#lf16 %>% mutate(x = coalesce(x, y))
mutate(lf16, Date = as.Date(Date, "%m/%d/%Y") )
mutate(lf16, PL-DATE = as.Date(PL-DATE), "%m/%d/%Y") 

??mutate(lf16,  Date1 = as.Date(Date, "%m/%d/%Y"), PL-DATE1 = as.Date(PL-DATE, "%m/%d/%Y")  ) 
??mutate(lf16, PLDATE = as.Date(PL-DATE), "%m/%d/%Y" )  

lf16
mutate(lf16, Date = coalesce(Date, PL-DATE) )

## ? df %>% mutate(x = ifelse(is.na(x), y, x))
## ? df[is.na(x),x := y]

## fill(data, ..., .direction = c("down", "up"))

## discount_data_df %>%  mutate(Date = as.Date(Date)) %>% 
##   complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%   fill(`Discount Rate`)

##discount_data_df %>% mutate(Date = as.Date(Date)) %>%
##   complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
##  group_by(Product) %>%   fill(`Discount Rate`)


# check 
#? filter(lf16, PL_DATE == Date )
filter(lf16, PL_DATE == Date )

#??spread(lf16, SHED, Month)

# names(lf17) <- lf17[1,]



# gs_read() is a wrapper  common methods to read data from the API and transform it for downstream use.

View(lf17)
lf17




# plot histogram
ggplot(lf16) +  geom_histogram(aes(x = Month, fill = Month), stat = "count" ) + theme_bw() + coord_flip() 
ggplot(lf16) +  geom_histogram(aes(x = Month              ), stat = "count" ) + theme_bw() + coord_flip() 

#ggplot(   ) +  geom_boxplot( lf17, aes(  x=reorder(format(lf17$X1,'%B'), lf17$X1),  stat="count" )

ggplot(lf16) +  geom_histogram(aes(x=Month, fill = Month), stat = "count" ) + theme_bw()

ggplot(lf17) +  geom_histogram(aes(x=X1 , fill = X1), stat="count")   + theme_bw() # good looking

ggplot(lf17) +  geom_histogram(aes(x=X2 , fill = X2), stat="count") + coord_flip()  + theme_bw()







#remove first k row being the file name  slice( k:n() )

# plot histogram

#ggplot(   ) +  geom_boxplot( lf17, aes(  x=reorder(format(lf17$X1,'%B'), lf17$X1),  stat="count" )

ggplot() +  geom_histogram( df= lf16, aes(x=Month) , stat = count )+ coord_flip()  + theme_bw()

ggplot(lf17) +  geom_histogram(aes(x=X1 , fill = X1), stat="count")   + theme_bw() # good looking

ggplot(lf17) +  geom_histogram(aes(x=X2 , fill = X2), stat="count") + coord_flip()  + theme_bw()

# names(lf17) <- lf17[1,]

##########
# working example rightward flow aesthetics for vaccine survey data
data(vaccinations)
vaccinations
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, fill = response,  label = round(a, 3)   )     )  +
    geom_lode() +  geom_flow()  +
    geom_stratum(alpha = 0)     +
    geom_text(stat = "stratum")

# time series alluvia of WorldPhones data  # not wow
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,  aes(x = Year, alluvium = Region, y = Telephones)) +
    geom_flow(aes(fill = Region, colour = Region), width = 0)
# time series bump chart
gg <- ggplot(alluvial::Refugees,  aes(y = refugees, x = year, alluvium = country))

gg + geom_alluvium(aes(fill = country, colour = country),
                   width = 1/4, alpha = 2/3, decreasing = FALSE)
# time series line plot of refugees data, sorted by country
gg + geom_alluvium(aes(fill = country, colour = country),
                   decreasing = NA, width = 0, knot.pos = 0)


####### ggdendro ########
library(ggdendro)
hc  <-  hclust(dist(USArrests), "ave")
hc
hcdata <- dendro_data(hc, type="rectangle")
hcdata
ggplot() + 
    geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_text(data=label(hcdata), aes(x=x, y=y, label=label, hjust=0), size=3) +
    coord_flip() + 
    scale_y_reverse(expand=c(0.2, 0))

####  plotting directly from object class hclust ####
ggdendrogram(hc)
ggdendrogram(hc, rotate=TRUE)

### demonstrate converting hclust to dendro using dendro_data first
hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE) + labs(title="Dendrogram in ggplot2")

# alluvial
library(alluvial)

lf18 %>% group_by( Terrotory, SHED, RLY) %>%  count(SHED)-> locof
#remove missing values
loco2 <-  locof  %>%  replace_na(list( Month = "UN", SHED = "UN", RLY = "UN")) %>% 
    arrange(SHED)               %>% 
    filter(Terrotory == "NR")   %>% 
    mutate(  sheds = paste(RLY, SHED)  )   #,   k = pal[ match(ss, sort(unique(ss))) ]
loco2

alluvial(       loco2[ , c(1 , 3,  5 , 4) ],   
                col   =  ifelse( loco2$RLY == "NR",   "red" ,  c( "pink", "green", 4:86)  ) , # color ordering of layers
                # col    = ifelse( loco2$RLY == "NR",   "red" ,  "blue") ,
                # border = ifelse( loco2$RLY == "NR",   "red" ,  "blue") ,
                freq  = loco2$n , 
                hide  = loco2$n < 10 , 
                axis_labels = c("Territorial", "Resp.Rly", "Sheds", "Total"), #else default to cname
                # blocks = FALSE, # colour blocks also
                xw = 0.0,        # connection curvature 0 better
                cw = 0.1,        # block width inches?
                cex = 0.7,       # Lable text size
                cex.axis = 0.8,  # axis lable sixe
                gap.width=0.001 , 
                layer= c( 86:1 ), # list all layer order to reorder layers
                alpha = 0.4               
                # ordering = list( NULL, order(tit$RLY, tit$Terrotory == "NR" ),  NULL  ) #order(tit$SHED, tit$Terrotory == "NR" )
)   ->  lfalluviam
lfalluviam
??save_plot("setoutsalluv.png" , lfalluviam, base_aspect_ratio = 1.3)


################### Good so far   #########
###########  using ggalluviam  ######
library(ggalluvial)

loco2[ , c(1 ,2, 3,  5 , 4) ]

gg <- ggplot( loco2[ , c(1 , 2, 3,  5 , 4) ],
              aes(   y = n,
                     axis1 = Terrotory, 
                     axis2 = RLY, 
                     axis3 = sheds ,
                     axis4 = n  )  )  +
    geom_stratum()                +   
    geom_text(stat = "stratum", label.strata = TRUE)      +
    scale_x_discrete( limits = c("Terrotory", "RLY", "sheds", "n")  )
gg
gg + geom_flow(aes(fill   =  ifelse( loco2$RLY == "NR",   "green" ,  "red"  ),  
                   col   =  ifelse( loco2$RLY == "NR",   "red" ,  c( "pink", "green", 4:86)  ),  # color ordering of layers
                   # col   =  ifelse( loco2$RLY == "NR",   "red" ,  c( 2:8)  ),  # color ordering of layers
                   # alpha    = ifelse( loco2$RLY == "NR",   0.9 , 0.3 )
                   size = 3,
                   linetype = "dotted"
                   #
),
#alpha = 0.2), 
stat     = "alluvium",
#    aes.bind = TRUE,  
#xw = 0.0,        # connection curvature
#cw = 0.1,        # block width inches?
#cex = 0.007,       # Lable text size
#cex.axis = 0.8,  # axis lable sixe
#    gap.width=0.3 ,   
# lode.ordering = lode_ord,
lode.guidance = "leftward" ) +
    #  lode_rightward(n, i) +
    # geom_stratum(width = .4) +   
    labs( title= "Loco troubles compiles (sets outs) ", 
          x = "Category" ,  y = "Total troubles reported"  )  +
    # geom_alluvium(fill = "darkgrey", na.rm = TRUE)  +
    # geom_stratum(aes(fill = sheds), color = NA, na.rm = TRUE)  # contnewus to descret ?
    # coord_flip()  +
    # facet_wrap(~ RLY, scales = "fixed") +
    theme(legend.position = "none" ) + # c(0.8, 0.8)) +
    theme_base() 

# cowplot::plot_grid(iris1, iris2, labels = "AUTO")



###################
arrange(loco2, RLY) -> loco2
alluvial(  loco2[,c(1,3,2)],   freq   = loco2$n  )

# or with bells
# ord <- list(NULL, with(tit3d, order(Sex, Survived)), NULL)
ord <- list(NULL, with(loco2, order(SHED, n)))

alluvial(tit[,2:3], 
         freq   = tit$n,
         col    = ifelse(tit$RLY == "NR", "red", "blue"),
         border = ifelse(tit$SHED == "TKD", "blue", "grey"),
         hide   = tit$n < 12, #uncomplicate by ignoring small
         cex    = 0.7,    #font size
         alpha  = 0.4    #  , blocks=FALSE   # for merging; ? 
         #?? , ordering=ord  #ord <- list(NULL, with(tit, order(RLY, n)))
         # other options ...
)

######

# to complete    group_by(Product) %>%   fill(`Discount Rate`) # for group wise
#                 complete(Date = seq.Date( min(Date), max(Date), by="days"))  
# incompletedata  %>% group_by(Date) %>%  tally(Date) %>% 
#                    complete( `PL-DATE` = seq.Date(  min(`PL-DATE`),  max(`PL-DATE`),  by="day" )  )
#
# write.csv( lf18,  "/Users/gk/Google Drive/locofailjan19.csv")
# write_csv(x, path, na = "NA", append = FALSE, col_names = !append) # faster , write_delim
# write.table(lf18, file = "foo.csv",  sep = ","  ,  col.names = NA,  qmethod = "double")
# read.table(   "foo.csv", sep = ",",    row.names = 1 ,  header = TRUE   ) 

#######################
dfm <- mtcars
dfm$cyl <- as.factor(dfm$cyl)   # Convert the cyl variable to a factor
dfm$name <- rownames(dfm)      ## Add the name colums
dfm$mpg_z <- (dfm$mpg -mean(dfm$mpg))/sd(dfm$mpg)
dfm$mpg_grp <- factor(ifelse(dfm$mpg_z < 0, "low", "high"), levels = c("low", "high"))

ggbarplot(dfm, x = "name", y = "mpg_z",
          fill = "mpg_grp",           # change fill color by mpg_level
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in descending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "MPG z-score",
          legend.title = "MPG Group",
          rotate = TRUE,             # Rotate axis ~ coord flif
          ggtheme = theme_minimal()
)


ggdotchart(dfm, x = "name", y = "mpg_z",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 2), # Change segment color and size
           group = "cyl",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(dfm$mpg_z,1),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9,  vjust = 0.5) #,   # Adjust label parameters
           # ggtheme = theme_pubr()                        # ggplot2 theme
)   # +  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")
#########################################

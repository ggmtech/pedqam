# EM Pads life analysis
library(tidyverse); library(readxl) ; library(lubridate); library(cowplot)
# library(ggthemes)      # or install_github('cttobin/ggthemr') #library(themr) # ggthemr("<theme name>") #ggthemr_reset()
# library(plotly)        # plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO")
library(googlesheets4)    # gs_auth(new_user = TRUE) #

library("survival") ; library("survminer")

getwd()
#setwd("/Users/gk/Google Drive")
setwd("/Users/gk/github/empads")
ls()  # files in memory
list.files()  # files in curr dir



###############################
library(googlesheets4)   
ss = "https://docs.google.com/spreadsheets/d/1lshFtdQf87ONyGdMHbwDRvRtWPM8B_bRPtuuq67P6xE"
sheet_names(ss) 

# googlesheets4::sheet_add() sheet_append() sheet_copy() sheet_delete() sheet_write() sheet_properties() sheet_rename()
# googlesheets4::read_sheet( ss, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, skip = 0, na = "", trim_ws = TRUE )
# , n_max = Inf  ,  guess_max = min(1000, n_max),  .name_repair = "unique" )

EMpadsupplies <-   googlesheets4::read_sheet(ss, sheet = "EMpadsupplied" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies %>% janitor::clean_names() %>% 
                  dplyr::mutate( dmdate = dmy(d_mdate), qty1 =  as.numeric(qty) , syear = year(dmdate), smonth = month(dmdate)   ) %>%
                  select(dmdate,  qty1, firm, railway   )  %>%     # , everything()
                  na.omit(dmdate) -> empadsup

empadsup %>%          View()

#install.packages("DataExplorer")

# .getPageLayout	Calculate page layout index
# dummify	Dummify discrete features to binary columns
# drop_columns	Drop selected variables
# group_category	Group categories for discrete features
# create_report	Create report
# DataExplorer-package	Data Explorer
# .ignoreCat	Truncate category
# configure_report	Configure report template
# plotDataExplorer.grid	Plot objects with gridExtra
# .lapply	Parallelization
# plot_intro	Plot introduction
# plot_boxplot	Create boxplot for continuous features
# plotDataExplorer.multiple	Plot multiple objects
# plot_scatterplot	Create scatterplot for all features
# plot_prcomp	Visualize principal component analysis
# plot_qq	Plot QQ plot
# plot_str	Visualize data structure
# plot_correlation	Create correlation heatmap for discrete features
# plotDataExplorer	Default DataExplorer plotting function
# plot_density	Plot density estimates
# introduce	Describe basic information
# profile_missing	Profile missing values
# plot_histogram	Plot histogram
# split_columns	Split data into discrete and continuous parts
# .getAllMissing	Get all missing columns
# plot_missing	Plot missing value profile
# update_columns	Update variable types or values
# set_missing	Set all missing values to indicated value
# plotDataExplorer.single	Plot single object
# plot_bar	Plot bar chart
##library(DataExplorer)
if (!require(devtools)) install.packages("devtools")
devtools::install_github("boxuancui/DataExplorer")  #, ref = "develop"
devtools::install_github("boxuancui/DataExplorer", ref = "develop")
empadsup  %>%       create_report()





library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

# set working directory to ensure R can find the file we wish to import # setwd("working-dir-path-here")

# plot Air Temperature Data across 2009-2011 using daily data
empadsup %>%  ggplot( )  +    
               #  geom_point(na.rm=TRUE) + 
                 # geom_point(  aes(x = dmdate, y = qty1 , size = qty1 , fill = year(dmdate)),      na.rm=TRUE, color="blue",  pch=1) +
                  geom_bar( aes(x = dmdate, y = qty1 , size = qty1 , fill = year(dmdate)),    stat="identity", na.rm = TRUE) +
                 #ok stat_smooth(aes(x = dmdate, y = qty1 , size = qty1 , fill = year(dmdate)),  colour="green") +
                #  scale_size_manual(1, 5) + 
                  scale_size_continuous(1,500) + 
                  (scale_x_date(breaks=scales::date_breaks("6 months"),   labels=scales::date_format("%b %y") )) +
                  ggtitle("EM pads supplies") +
                  xlab("Date") + ylab("Qty supplied") +
                  facet_wrap(~firm) + theme_bw()


empadsup %>%  ggplot( aes(dmdate, qty1), stat_bin(qty1) )  +    geom_histogram(na.rm=TRUE) + facet_wrap(~firm)

library(timetk)
interactive <- FALSE  # Setup for the plotly charts (# FALSE returns ggplots)
empadsup %>% timetk::plot_time_series(dmdate, qty1,       .interactive = interactive,    .plotly_slider = TRUE)


EMpadsupplies %>% mutate(   )  %>% View()
ItemID      <-   googlesheets4::read_sheet(ss, sheet = "ItemID"      , col_types = "c"  ) 




#######################
# https://www.r-bloggers.com/2020/12/a-heatmap-as-calendar/
# instalamos los paquetes si hace falta
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ragg")) install.packages("ragg")
if(!require("lubridate")) install.packages("lubridate")
# paquetes
library(tidyverse)
library(lubridate)
library(ragg)










######################







unique(TKDpacloc$Loco)
TKDallfitments  <-  rbind(TKDpacloc,TKDpareplace )
glimpse()  #View()


TKDlifetable  %>% mutate( Loco     = as.factor(Loco) ,  
                          item     = as.factor(item), 
                          Loc      = as.factor(Loc), 
                          Make     = as.factor(Make), 
                          Reason   = as.factor(Reason),
                          datefit  = as.Date(datefit),
                          faildate = as.Date(faildate),
                          life     = as.numeric(life)      )       %>%    
                  mutate( item = fct_collapse(item,
                                               "Piston" = c( "P", "Piston" ),
                                               "Liner"  = c("L", "Liner" ),
                                               "LP"   = c("LP", "PA")
                                                )      )                 %>%  
                  mutate(uloc2 = paste(Loco, Loc) , uloc3 = paste(Loco, Loc, item)) %>% 
                  group_by(uloc2)           %>% 
                  arrange(uloc2, datefit)            %>%   
                  filter(!is.na(Loco)   )    ->  TKDlifetable2     #    %>%    View()
TKDlifetable2

#####  Only liners  Now lead lag function ########

## lag function   eg    dt %>% group_by(location) %>% mutate(lvar = lag(var))
## seems group_by(name) %>% mutate(next.score = lead(score, order_by=name), before.score = lag(score, order_by=name) )
# from tidyverse fn lead or lag
# lead(x, n = 1L, default = NA, order_by = NULL, ...)
# x	 is vector of values,  n is number of positions to lead or lag by
# default is value to  use for non-existent rows. Defaults to NA.
# order_by   override the default ordering to use another vector
####

# Liners only
TKDlifetable2   %>%    filter(!item == "Piston" )  %>%  group_by(uloc2) %>% 
                       mutate(  dateL = lead(datefit)          )        %>%
                       mutate(  life = (dateL - datefit) )              %>%
                       mutate(  censure = ifelse(!Reason == "OOC", 1, 0   )    )   -> TKDlifeL   # %>% View()

# pistons only
TKDlifetable2   %>%    filter(!item == "Liner" )  %>%  group_by(uloc2) %>%         #  View()
                       mutate(  dateP = lead(datefit)          )       %>%
                       mutate(  life = (dateP - datefit) )             %>% 
                       mutate(  censure = ifelse(!Reason == "OOC", 1, 0   )    ) -> TKDlifeP   #  %>% View()


TKDlifetable2
TKDlifeL     %>% View()
TKDlifeP


############  Survial of TKD liners   #############
######### 31march TKD data  #######

TKDlifeL
# Reduce the make factors to EMD, GE, TRANSLOCO, TRANSOTHER, Other
levels(TKDlifeL$Make)
TKDlifeL  %>%  # mutate( Make = factor(Make )      )      %>%   
                 mutate( Make = fct_collapse( Make,
                                 `EMD` = c("EMD", "EMD"),
                                 `GE` = c("GE", "GE LINER", "Ge make liner"), 
                                 `TRANSLOCO` = c("CC", "TRANS", "TRANS/HL", "Trans loco liner", "TRANS LOCO",
                                                 "HL/TRANS" ),
                                 #`TRANS/EMD` = c("TRANS/EMD", "TRANS/ EMD"),
                                  OthersMix = c("NA", "Others", "others",
                                                "TRANS/EMD", "TRANS/ EMD",
                                                "GE/TRANS", "TRANS/GE", "FMG/HL", "GE/EMD", "DLW",
                                                "FMG",  "HL", 
                                                "KAR", "KAR","SPR-15D", "SPR-15D" )  
    )  
    )      ->  TKDlifeLfinal

TKDlifeLfinal
levels(TKDlifeLfinal$Make)

names(TKDlifeL)  # "Loco" "datefit""item" "Loc" "Make" "Reason" "faildate" "life" "uloc2" "uloc3" "dateL" "censure"


TKDpafit <- survfit( Surv(life) ~ Make , data = TKDlifeLfinal )
TKDpafit

ggsurvplot(
    TKDpafit,                     # survfit object with calculated statistics.
    # fun = "event",
    # fun = "cumhaz",       # fun = function(y) y*100 ,
    # linetype = "strata",     # change line type by groups
    size = 2,                # change line size
    data = TKDlifeLfinal,             # data used to fit survival curves.
    
    conf.int = TRUE,         # show confidence intervals for  point estimates of survival curves.
    
    #pval = TRUE,             # show p-value of log-rank test.
    #pval = 0.03
    #pval = "The hot p-value is: 0.031"
    #pval.coord = c(0, 0.03)
    #pval.size = 4,
    #pval.method = TRUE,
    #pval.method.size = 3,
    #log.rank.weights = "1",
    #conf.int.style = "ribbon",
    #conf.int.alpha = 0.2,
    
    
    # ?conf.int.fill = "blue",
    # palette = c("#E7B800", "#2E9FDF"), # custom color palette, match varibles
    palette = "Dark2",
    xlim = c(0,2000),         # present narrower X axis, but not affect survival estimates.
    xlab = "Failure Time in Days",   # customize X axis label.
    break.time.by = 90,     # break X axis in time intervals by 500.
    #ggtheme = theme_light(), # customize plot and risk table with a theme.
    ggtheme = theme_bw() ,
    
    #censor.shape="|", 
    #censor.size = 4,
    # ncensor.plot = TRUE,      # plot the number of censored subjects at time t
    # ncensor.plot.height = 0.25,
    # conf.int.style = "step",  # customize style of confidence intervals
    
    #font.main = c(16, "bold", "darkblue"),
    #font.x = c(14, "bold.italic", "red"),
    #font.y = c(14, "bold.italic", "darkred"),
    #font.tickslab = c(12, "plain", "darkgreen"),
    # legend = "bottom", 
    #legend = c(0.2, 0.2),
    #legend.title = "Sex",
    #legend.labs = c("Male", "Female"),
    
     surv.median.line = "v",  # add the median survival pointer. c("none", "hv", "h", "v")
    # legend = "bottom" , 
    # legend.labs =      c("Male", "Female"),    # change legend labels.
    
    risk.table = TRUE,       # show risk table.
    # tables.theme = theme_cleantable(),
    risk.table.col = "strata" , 
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.height = 0.25 #, # the height of the risk table
    #risk.table.y.text = FALSE # show bars instead of names in text annotations in legend of risk table.
)    -> ggsurv

ggsurv  #+ draw_image("https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png", x = 5, y = 2.5, width = 2, height = 1.5    )

#########################


######### Changing Labels %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(     title    = "The Power assembly failures over time since commissioning in TKD shed",                     
                                       subtitle = "Based on TKD shed actual data as on 30/03/19 and life given in days" ,  
                                       caption  = "TRANSLOCO liners failes withing a year ! "          )  #+
            #draw_image("https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png", x = 5, y = 2.5, width = 2, height = 1.5    )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = " TKD Liners in service after so many days and thier Survival table",          
                                       subtitle = " Population in Locos and time to fail in days", 
                                       caption  = " TKD data as on 30/03/2019"        )

##### Labels for ncensor plot  #######
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(   title    = "censured data", 
                                                     subtitle = "over the time.",
                                                     caption  = "TKD data "  )

ggsurv

# Changing the font size, style and color
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Applying the same font style to all the components of ggsurv:
# survival curves, risk table and censor part

ggsurv <- ggpar(  
    ggsurv,
    font.title    = c(16, "bold", "darkblue"),         
    font.subtitle = c(15, "bold.italic", "purple"), 
    font.caption  = c(14, "plain", "orange"),        
    font.x        = c(14, "bold.italic", "red"),          
    font.y        = c(14, "bold.italic", "darkred"),      
    font.xtickslab = c(12, "plain", "darkgreen"),
    legend = "top"
)

ggsurv


############ Using specific fonts for risk table and ncensor plots #########

# Font for Risk Table
ggsurv$table <- ggpar(      ggsurv$table,
                            font.title    = c(13, "bold.italic", "green"),
                            font.subtitle = c(15, "bold", "pink"),
                            font.caption  = c(11, "plain", "darkgreen"),
                            font.x        = c(8, "bold.italic", "orange"),
                            font.y        = c(11, "bold.italic", "darkgreen"),
                            font.xtickslab = c(9, "bold", "red")
)


# Font for ncensor plot
ggsurv$ncensor.plot <- ggpar(
    ggsurv$ncensor.plot,
    font.title    = c(13, "bold.italic", "green"),
    font.subtitle = c(15, "bold", "pink"),
    font.caption  = c(11, "plain", "darkgreen"),
    font.x        = c(8, "bold.italic", "orange"),
    font.y        = c(11, "bold.italic", "darkgreen"),
    font.xtickslab = c(9, "bold", "red")
)

ggsurv

print(ggsurv)


##### Facet by one grouping variables: rx  ###################
TKDpa1
TKDpafit <- survfit(Surv(lifemonths) ~ Make , data = TKDpa1 )
lifemonths
TKDpafit <- survfit( Surv(lifemonths) ~ Make, data = TKDpa1  )
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = "Component")   #,    palette = "jco", pval = TRUE
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = c("Component", "adhere") ) #,   palette = "jco", pval = TRUE)   # Facet by two grouping

########  till 31 march  #############

######## graphs ############
??library("ggalt")
circle.df <- iris %>% filter(Species == "setosa")
ggplot(iris, aes(Petal.Length, Petal.Width) ) +
            geom_point(aes(colour = Species)) + 
            geom_encircle( data  = circle.df,  linetype = 1, size=3, col = "red", alpha = 0.2)

ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(size = qsec), alpha = 0.5) + scale_size(range = c(0.5, 12) ) 


TKDpa1 %>% mutate(Loco1 = as.factor(Loco)) -> TKDpa1

ggplot(TKDpa1, aes(x = Loco1 , y = lifemonths)) +   geom_col() +   rotate_x_text(angle = 45)

ggplot(TKDpa1,   aes( x = reorder(Loco1, Loco1) , y = lifemonths)  ) +
    geom_segment(  aes(x = Loco1, xend = Loco1, 
                       y = 0,     yend = lifemonths , col = Make) , alpha = 0.8, size =5   ) + 
    geom_point( aes(color = Make), size = 3 ) +
    geom_text( aes(label = lifemonths), nudge_y = 3) + 
    #scale_color_viridis_d() +
    theme_pubclean() +
    rotate_x_text(45) +
    coord_flip() +
    theme_bw()  #theme_cowplot()

########### Graphs above  ################



# levels(factor(df$col))    OR    unique(df$col)
  # distinct(uloc) %>% View()


# inner_join(df1, df2),   left_join(df1, df2), full_join(df1, df2)
# semi_join(df1, df2) #keep only observations in df1 that match in df2.
# anti_join(df1, df2) #drops all observations in df1 that match in df2.    
# bind_rows, 
# union()  merge from both data frames but keep only the distinct (unique) rows  
# ‘intersect’, return only the duplicated rows among the data frames
# ‘setdiff’, which would return only the rows that are not duplicated.
    
# spread() distributes a pair of key:value columns into a field of cells. The unique values of the key column become the column names of the field of cells.
spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE,  sep = NULL)
                    # fill missing values, convert type, drop facotr ordeing, sep keymane-keyvalue
gather(table4, "myKeyCol", "myValueCol", colomestocollaps )  # 3:5 or c("cols", "cols3",  "etc") or -coltoExclude
                                    # na.rm = FALSE, convert = FALSE, factor_key = FALSE ) # factor_key preserve ording
# separate() and unite()
# separate(table3, rate, into = c("cases", "population"), sep = "/") #sep = "/", 1, "", 
                               # remove = FALSE, convert = FALSE, extra = drop/merge/NA
# unite_(mtcars, "vs_am", c("vs","am"))     # joinsep = "_", "/", "", etc

# table
# (loco+palocation) + make+ datefit+     datefail+reason ??
# sort loco+palocation - desc datefit
# group on loco+palocation
# if datefail == na , fill with next record date
# time series of 
# samelocopalocaton if second date, take faildata from below data

#apply(Ages, 2, median)  # apply( subsetdata, c(1 for row, 2 for colomns), Funtion you want to apply )

# subetting df[1:4, 3:4]  or c("Name", "Surname") 



###############################

TKDpa %>% distinct(Component)  

TKDpa  %>%  mutate( Component = factor(Component )      )      %>%   
            mutate( Component = fct_collapse( Component,
                               `Liner` = c("Liner"),
                               `Piston` = c("Piston"),
                               `Piston Rings` = c("Ring (Piston)"),
                               `Gasket` = c("Gasket"),
                               `Valve` = c("Valve"),
                               `Head` = c("Head"),
                                Others = c("NA", "Others", "others")  
                               )  
                      )       %>%
             mutate( Make =  factor(Make ) )     %>%   
             mutate( Make = fct_collapse( Make,
                                      `EMD` = c("EMD", "EMD"),
                                      `GE` = c("GE", "GE LINER", "Ge make liner"), 
                                      `CC` = c("CC","Trans loco liner", "TRANS LOCO"),
                                      `KAR` = c("KAR", "KAR"),
                                      `SPR-15D` = c("SPR-15D", "SPR-15D"),
                                       Others = c("NA", "Others", "others")  
                                       )  
                      )      ->  TKDpa1
















#########


TKDpa1

TKDpafit <- survfit(Surv(lifemonths) ~ Make , data = TKDpa1 )

ggsurvplot(
    TKDpafit,                     # survfit object with calculated statistics.
    # fun = "event",
    # fun = "cumhaz",
    # fun = function(y) y*100 ,
    #linetype = "strata",     # change line type by groups
    size = 2,                # change line size
    data = TKDpa,             # data used to fit survival curves.
    
    
    #pval = TRUE,             # show p-value of log-rank test.
    #conf.int = TRUE,         # show confidence intervals for  point estimates of survival curves.
    # ?conf.int.fill = "blue",
    # palette = c("#E7B800", "#2E9FDF"), # custom color palette, match varibles
    palette = "Dark2",
    xlim = c(0,100),         # present narrower X axis, but not affect survival estimates.
    xlab = "Time in Months",   # customize X axis label.
    break.time.by = 5,     # break X axis in time intervals by 500.
    #ggtheme = theme_light(), # customize plot and risk table with a theme.
    ggtheme = theme_bw(),
    
   # ncensor.plot = TRUE,      # plot the number of censored subjects at time t
   # ncensor.plot.height = 0.25,
   # conf.int.style = "step",  # customize style of confidence intervals
    
    #font.main = c(16, "bold", "darkblue"),
    #font.x = c(14, "bold.italic", "red"),
    #font.y = c(14, "bold.italic", "darkred"),
    #font.tickslab = c(12, "plain", "darkgreen"),
    # legend = "bottom", 
    #legend = c(0.2, 0.2),
    #legend.title = "Sex",
    #legend.labs = c("Male", "Female"),
    
    # surv.median.line = "hv",  # add the median survival pointer. c("none", "hv", "h", "v")
    # legend = "bottom" , 
    # legend.labs =      c("Male", "Female"),    # change legend labels.
    
    risk.table = TRUE,       # show risk table.
    # tables.theme = theme_cleantable(),
    risk.table.col = "strata",
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.height = 0.25, # the height of the risk table
    risk.table.y.text = FALSE # show bars instead of names in text annotations in legend of risk table.
     )    -> ggsurv

ggsurv

#### Changing Labels %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(     title    = "The Power assembly failurs over time in TKD shed",                     
                                       subtitle = "Based on limited data at TKD on 30/03/19",  
                                       caption  = "CC lines are the worst"          )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = "Failure of liners in Locos - Survival table",          
                                       subtitle = "and data is evedent that CC make lines failing too soon.", 
                                       caption  = "TKD data"        )

# Labels for ncensor plot 
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(   title    = "censured data", 
                                                     subtitle = "over the time.",
                                                     caption  = "TKD data "  )

ggsurv

# Changing the font size, style and color
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Applying the same font style to all the components of ggsurv:
# survival curves, risk table and censor part

ggsurv <- ggpar(  
    ggsurv,
    font.title    = c(16, "bold", "darkblue"),         
    font.subtitle = c(15, "bold.italic", "purple"), 
    font.caption  = c(14, "plain", "orange"),        
    font.x        = c(14, "bold.italic", "red"),          
    font.y        = c(14, "bold.italic", "darkred"),      
    font.xtickslab = c(12, "plain", "darkgreen"),
    legend = "top"
)

ggsurv


# Using specific fonts for risk table and ncensor plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Font for Risk Table
ggsurv$table <- ggpar(      ggsurv$table,
                            font.title    = c(13, "bold.italic", "green"),
                            font.subtitle = c(15, "bold", "pink"),
                            font.caption  = c(11, "plain", "darkgreen"),
                            font.x        = c(8, "bold.italic", "orange"),
                            font.y        = c(11, "bold.italic", "darkgreen"),
                            font.xtickslab = c(9, "bold", "red")
)


# Font for ncensor plot
ggsurv$ncensor.plot <- ggpar(
    ggsurv$ncensor.plot,
    font.title    = c(13, "bold.italic", "green"),
    font.subtitle = c(15, "bold", "pink"),
    font.caption  = c(11, "plain", "darkgreen"),
    font.x        = c(8, "bold.italic", "orange"),
    font.y        = c(11, "bold.italic", "darkgreen"),
    font.xtickslab = c(9, "bold", "red")
)

ggsurv

print(ggsurv)


############################

# Facet by one grouping variables: rx
TKDpa1
TKDpafit <- survfit(Surv(lifemonths) ~ Make , data = TKDpa1 )
lifemonths
TKDpafit <- survfit( Surv(lifemonths) ~ Make, data = TKDpa1  )
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = "Component")   #,    palette = "jco", pval = TRUE
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = c("Component", "adhere") ) #,   palette = "jco", pval = TRUE)   # Facet by two grouping




















#####################################################
#####################################################


# ggsurvplot(), ggsurvplot_list() , ggsurvplot_facet(), ggsurvplot_group_by(), ggsurvplot_add_all(), ggsurvplot_combine()
lung
lung$status <- 1

fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
fit2 <- survfit(Surv(time, status) ~ sex, data = lung)
fit3 <- survfit( Surv(time, status) ~ sex + rx + adhere,  data = colon )
fit4 <- survfit( Surv(time, status) ~ rx + adhere, data = colon )


#####################################################
# Fit a Cox proportional hazards model
surv_object
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps,   data = ovarian)
ggforest(fit.coxph, data = ovarian)
#
#################################################
ggsurvplot(fit2 )   # most basic plot of fitted data
ggsurvplot_facet(fit2 , data = colon ,  facet.by = "adhere") + theme_bw()  


# Customise   Change color, linetype by strata, risk.table color by strata

ggsurvplot(fit4, 
           # fun = "event",
           fun = "cumhaz",        #fun = function(y) y*100 ,
           pval = TRUE, conf.int = TRUE,
           
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           
           linetype = "strata", # Change line type by groups
           # palette = c("#E7B800", "#2E9FDF")  # match the variables
           ggtheme = theme_bw() # Change ggplot2 theme
)

############################################################
ggsurvplot_facet(fit2, data = colon, facet.by = "adhere",  palette = "jco", pval = TRUE) + theme_bw()
ggsurvplot_facet(fit2 , data = colon ,  facet.by = "adhere") + theme_bw()  

colon
ggplot(data = colon) + #geom_dotplot( aes( y= age, x = time) )
    geom_point(aes( y= age, x = time, col = status) ) 


####### Combine curves 
fit <- list(PFS = fit1, OS = fit2)
ggsurvplot_combine(fit, lung)



############################################################

ggsurvplot(
    fit2,                     # survfit object with calculated statistics.
    # fun = "event",
    # fun = "cumhaz",
    fun = function(y) y*100 ,
    linetype = "strata",     # change line type by groups
    size = 1,                # change line size
    data = lung,             # data used to fit survival curves.
    
    
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE,         # show confidence intervals for  point estimates of survival curves.
    # ?conf.int.fill = "blue",
    # palette = c("#E7B800", "#2E9FDF"), # custom color palette, match varibles
    palette = "Dark2",
    xlim = c(0,500),         # present narrower X axis, but not affect survival estimates.
    xlab = "Time in Months",   # customize X axis label.
    break.time.by = 100,     # break X axis in time intervals by 500.
    #ggtheme = theme_light(), # customize plot and risk table with a theme.
    ggtheme = theme_bw(),
    
    ncensor.plot = TRUE,      # plot the number of censored subjects at time t
    ncensor.plot.height = 0.25,
    conf.int.style = "step",  # customize style of confidence intervals
    
    font.main = c(16, "bold", "darkblue"),
    font.x = c(14, "bold.italic", "red"),
    font.y = c(14, "bold.italic", "darkred"),
    font.tickslab = c(12, "plain", "darkgreen"),
    # legend = "bottom", 
    legend = c(0.2, 0.2),
    legend.title = "Sex",
    legend.labs = c("Male", "Female"),
    
    # surv.median.line = "hv",  # add the median survival pointer. c("none", "hv", "h", "v")
    # legend = "bottom" , 
    # legend.labs =      c("Male", "Female"),    # change legend labels.
    
    risk.table = TRUE,       # show risk table.
    # tables.theme = theme_cleantable(),
    risk.table.col = "strata",
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.height = 0.25, # the height of the risk table
    risk.table.y.text = FALSE # show bars instead of names in text annotations in legend of risk table.
)    -> ggsurv

ggsurv

# Changing Labels
# %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(     title    = "Survival curves",                     
                                       subtitle = "Based on Kaplan-Meier estimates",  
                                       caption  = "created with survminer"          )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = "Note the risk set sizes",          
                                       subtitle = "and remember about censoring.", 
                                       caption  = "source code: website.com"        )

# Labels for ncensor plot 
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(   title    = "Number of still not failed / censured", 
                                                     subtitle = "over the time.",
                                                     caption  = "source code: website.com"  )

ggsurv

# Changing the font size, style and color
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Applying the same font style to all the components of ggsurv:
# survival curves, risk table and censor part

ggsurv <- ggpar(  
    ggsurv,
    font.title    = c(16, "bold", "darkblue"),         
    font.subtitle = c(15, "bold.italic", "purple"), 
    font.caption  = c(14, "plain", "orange"),        
    font.x        = c(14, "bold.italic", "red"),          
    font.y        = c(14, "bold.italic", "darkred"),      
    font.xtickslab = c(12, "plain", "darkgreen"),
    legend = "top"
)

ggsurv


# Using specific fonts for risk table and ncensor plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Font for Risk Table
ggsurv$table <- ggpar(      ggsurv$table,
                            font.title    = c(13, "bold.italic", "green"),
                            font.subtitle = c(15, "bold", "pink"),
                            font.caption  = c(11, "plain", "darkgreen"),
                            font.x        = c( 8, "bold.italic", "orange"),
                            font.y        = c(11, "bold.italic", "darkgreen"),
                            font.xtickslab = c(9, "bold", "red")
)


# Font for ncensor plot
ggsurv$ncensor.plot <- ggpar(
    ggsurv$ncensor.plot,
    font.title    = c(13, "bold.italic", "green"),
    font.subtitle = c(15, "bold", "pink"),
    font.caption  = c(11, "plain", "darkgreen"),
    font.x        = c(8, "bold.italic", "orange"),
    font.y        = c(11, "bold.italic", "darkgreen"),
    font.xtickslab = c(9, "bold", "red")
)

ggsurv

print(ggsurv)



###########
ggsurvplot()          #: Draws survival curves with the 'number at risk' table, the cumulative number of events table and the cumulative number of censored subjects table.
arrange_ggsurvplots() #: Arranges multiple ggsurvplots on the same page.
ggsurvevents()        #: Plots the distribution of event's times.
surv_summary()        #: Summary of a survival curve. Compared to the default summary() function, surv_summary() creates a data frame containing a nice summary from survfit results.
surv_cutpoint()       #: Determines the optimal cutpoint for one or multiple continuous variables at once. Provides a value of a cutpoint that correspond to the most significant relation with survival.
pairwise_survdiff()   #: Multiple comparisons of survival curves. Calculate pairwise comparisons between group levels with corrections for multiple testing.
# Diagnostics of Cox Model
ggcoxzph()           #: Graphical test of proportional hazards. Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve using ggplot2. Wrapper around plot.cox.zph().
ggcoxdiagnostics()   #: Displays diagnostics graphs presenting goodness of Cox Proportional Hazards Model fit.
ggcoxfunctional()    #: Displays graphs of continuous explanatory variable against martingale residuals of null cox proportional hazards model. It helps to properly choose the functional form of continuous variable in cox model.

ggforest()           #: Draws forest plot for CoxPH model  # Summary of Cox Model
ggadjustedcurves()   #: Plots adjusted survival curves for coxph model.
ggcompetingrisks()   # Competing Risks, Plots cumulative incidence curves for competing risks.

# more at http://www.sthda.com/english/rpkgs/survminer/, and check out the documentation and usage 

###################################################################

# Facet by one grouping variables: rx
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
fit <- survfit( Surv(time, status) ~ sex, data = colon )
ggsurvplot_facet(fit, colon, facet.by = "rx",    palette = "jco", pval = TRUE)
ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),   palette = "jco", pval = TRUE)  # Facet by two grouping variables: rx and adhere

# Another fit
fit2 <- survfit( Surv(time, status) ~ sex + rx,  data = colon )
ggsurvplot_facet(fit2, colon, facet.by = "adhere",  palette = "jco", pval = TRUE)


# Faceting

ggsurv <- ggsurvplot(fit3, data = colon,
                     fun = "cumhaz", conf.int = TRUE,
                     risk.table = TRUE, risk.table.col="strata",
                     ggtheme = theme_bw())
ggsurv                     
# Faceting survival curves
curv_facet <- ggsurv$plot + facet_grid(rx ~ adhere)
curv_facet
# 
# # Faceting risk tables:
# # Generate risk table for each facet plot item

ggsurv$table + facet_grid(rx ~ adhere, scales = "free")+
    theme(legend.position = "none")
# 
#  # Generate risk table for each facet columns
tbl_facet <- ggsurv$table + facet_grid(.~ adhere, scales = "free")
tbl_facet + theme(legend.position = "none")
# 
# # Arrange faceted survival curves and risk tables
g2 <- ggplotGrob(curv_facet)
g3 <- ggplotGrob(tbl_facet)
min_ncol <- min(ncol(g2), ncol(g3))
g <- gridExtra::rbind.gtable(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
g$widths <- grid::unit.pmax(g2$widths, g3$widths)
grid::grid.newpage()
grid::grid.draw(g)
# 
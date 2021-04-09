library(tidyverse, readxl, lubridate)
library("survival", "survminer", "cowplot" )
library(gridExtra, cowplot)

# library(hrbrthemes)
# library(ggthemes)      # or install_github('cttobin/ggthemr') #library(themr) # ggthemr("<theme name>") #ggthemr_reset()
# library(plotly)        # plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO")

library(googlesheets)             
setwd("/Users/gk/Google Drive")   #getwd()  #ls()  # list.files()  # difference 
gs_auth()      #gs_auth(new_user = TRUE)  #  gs_user()    # current user 

# locate files and names
gs_ls("TKDpa2")                     # list the worksheets containing "setout"                                   
gs_ls("ONE")                        # list the worksheets containing "ONE .."
gs_ls("AMV")[1,1]

# Files  by title
TKDpahandle    <-   gs_title( "TKDpa"  )     
TKDpahandle2   <-   gs_title( "TKDpa2"  )    
AMVhandle      <-   gs_title("AMVpowerassemblies")

# Sheets
TKDpa          <-   gs_read(ss = TKDpahandle,   ws = 1 , skip = 0, col_names = TRUE, verbose = TRUE)
TKDpac         <-   gs_read(ss = TKDpahandle2,  ws = 1 , skip = 0, col_names = TRUE, verbose = TRUE)
TKDpaf         <-   gs_read(ss = TKDpahandle2,  ws = 2 , skip = 0, col_names = TRUE, verbose = TRUE)

AMVliferaw     <-   gs_read(ss = AMVhandle,     ws = 1 , skip = 0, col_names = TRUE, verbose = TRUE)

AMVliferaw  %>% View()   # contain multiple locations
AMVliferaw  %>% separate(Location, into = c("Loc" , "misc"), extra = "warn" ) %>% mutate(paste0("Loc", Loc))  %>% View()

###############################
TKDpac %>% View()
TKDpaf %>% View()
TKDpa  %>% View()
intersect(TKDpaf,TKDpa)
AMVliferaw  %>% View()  # all cases of failurs only, no censure

##############
TKDpac  %>% View()   # now change the "1" to "Loc1" etc
colnames(TKDpac) <- c("Loco No.", "DOC" , "Item" , "Loc1" , "Loc2", "Loc3", "Loc4" , "Loc5","Loc6", "Loc7", "Loc8", 
                      "Loc9",  "Loc10" , "Loc11" , "Loc12" , "Loc13" , "Loc14" , "Loc15" ,  "Loc16"   )



TKDpac  
TKDpac$Reason  <- "Commission"

TKDpac 

TKDpaf  



############

# Failure cases location is not "ALL
TKDpaf
TKDpaf %>%  filter(Reason == "OOC", !Reason = "6Y", !Reason == "SCH.")   %>%           #filter(!Location == "ALL")  %>%
            mutate(datefit = as.(Date) )  %>%  
            mutate(   Loc = paste0("Loc", Location)   ) %>%  
            select(Loco = `Loco No.`, datefit , item , Loc , Make, Reason )  ->  TKDpafail
TKDpafail   %>% View()


# Schedule cases location is ALL
TKDpaf %>% filter(Location == "ALL")  %>%
           mutate(datefit = dmy(Date))  %>%  
           mutate(   Loc =  Location   ) %>%  
            select(Loco = `Loco No.`, datefit , item , Loc , Make, Reason )  ->  TKDpaschedule
TKDpaschedule   %>% View()

# On TKDpaschedule,   Replicate change of PA to all 1:16 location for Location == "ALL" 
# Add those Loc1:Loc16 new colums list in Loc and fill with value list TKDpaf16$Make using listsvector locations
locations <- c("Loc1" , "Loc2", "Loc3", "Loc4" , "Loc5","Loc6", "Loc7", "Loc8", 
                "Loc9",  "Loc10" , "Loc11" , "Loc12" , "Loc13" , "Loc14" , "Loc15" ,  "Loc16"   )
locations

TKDpaschedule[locations]  <- TKDpaschedule$Make   # ******* Magic way to add colums with values !! 

TKDpaschedule   %>% View()          # tkd 37 data expended for locations with copied make
# Add the piston cases for LP in schedule
lp <-  TKDpaschedule  %>% filter(item == "LP") %>% mutate(item = "P")

TKDpaschedule  <- rbind(TKDpaschedule, lp) 

TKDpaschedule2  <- TKDpaschedule

# match the tables initial commissioned and schedule all changes

TKDpaschedule2 # Data for schulde changes L and P

TKDcommission  <-  TKDpac  # Data initial fitment of L and P
TKDcommission  %>% mutate(Loco = `Loco No.`,  datefit = dmy(DOC)  )  %>% 
                   select(Loco, datefit,  Item, Reason, everything() , -DOC , -`Loco No.`)  -> TKDcommission2
TKDcommission2   #  Loco ,datefit,    Item ,  Loc1 : Loc16, Reason

#Add both data after making common structure
colnames(TKDcommission)
colnames(TKDpaschedule)


TKDpaschedule2   %>% mutate(Item = item) %>%
                   select(Loco, datefit, Item,  everything(), -item , -Loc, -Make)  -> TKDpaschedule3

#TKDpaschedule2  %>% mutate(Loco = as.factor(Loco), Item = as.factor(item), Make = as.factor(Make), Loc = as.factor(Loc), Reason = as.factor(Reason) ) %>%
#?                    select(Loco, item, Make, Reason, Loc1,  Loc2,  Loc3,  Loc4,  Loc5,  Loc6,  Loc7,  Loc8,  Loc9,  Loc10, Loc11, Loc12, Loc13, Loc14, Loc15, Loc16) 
TKDcommission2
TKDpaschedule3


TKDnewfitments  <- rbind(TKDpaschedule3,TKDcommission2) 
TKDnewfitments  %>% View()

TKDnewfitments  %>% filter(!Reason == "OOC")  -> TKDnewliners

TKDnewliners   # all new liner replacement



###########################


TKDpaf16s            # 161 data

# Tidy location with make , with spread and  gather, gather(`1999`, `2000`, key = "year", value = "cases")
TKDpaf16s %>% gather(-`Loco No.`, -Date , -Reason, -item , -Location , -Make, 
                     key = "Loc", value = "Mak")     %>%     
    mutate(datefit = dmy(Date) )             %>%            
    select(Loco = `Loco No.`, datefit, item , Loc, Make, Reason, 
           -Mak, -Location , -Date)     ->  TKDpaSch        


#####  combined data of Schedule and failure replacements
TKDpaSch
TKDpafail
names(TKDpaSch)
names(TKDpafail)

TKDpareplace <- rbind(TKDpaSch, TKDpafail)
# ? TKDpareplace   <- left_join( TKDpaSch, TKDpafail  )
TKDpareplace
TKDpareplace %>% View()


##################################
# initial fitments
TKDpac 
TKDpac %>% gather(-`Loco No.`, -DOC ,  -Item ,  key = "Loc", value = "Make")     %>%   
    mutate( Reason = "DOC", datefit = dmy(DOC) )  %>%   
    select(Loco = `Loco No.`, datefit, item = Item , Loc, Make, Reason, -DOC)   -> TKDpacloc

TKDpacloc


###############################
TKDpacloc     # all commissioned assemblies 2502 data
TKDpareplace  # all out of course replacement
names(TKDpacloc)
names(TKDpareplace)

unique(TKDpacloc$Loco)


TKDallfitments  <-  rbind(TKDpacloc,TKDpareplace )
TKDallfitments

TKDallfitments    %>% View()  # all power assembly fitments

########  Now we have TKDlifetable #######################
#########                          ######################

TKDlifetable  <-  TKDallfitments
TKDlifetable$faildate <- NA
TKDlifetable$life <- NA
TKDlifetable                %>% glimpse()  #View()


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
#####  Only liners  Now lead lag function
## lag function   eg    dt %>% group_by(location) %>% mutate(lvar = lag(var))
## seems group_by(name) %>% mutate(next.score = lead(score, order_by=name), before.score = lag(score, order_by=name) )
# from tidyverse fn lead or lag
# lead(x, n = 1L, default = NA, order_by = NULL, ...)
# x	 is vector of values,  n is number of positions to lead or lag by
# default is value to  use for non-existent rows. Defaults to NA.
# order_by   override the default ordering to use another vector
####

# Liners only
TKDlifetable2  %>% View()

TKDlifetable2   %>%    filter(!item == "Piston" )                       %>%  
                       group_by(uloc2)                                  %>%   
                       mutate(  dateL = lead(datefit)      )            ->    TKDlifedates  #   View()

TKDlifedates

TKDlifedates   %>%     mutate(  life =   as.Date("2019-03-11") - datefit)    %>%   # View() # yes !!!

                       mutate(  censure = ifelse(!Reason == "OOC", 0, 1   )    )     ->   TKDlifeLiners
TKDlifeLiners


# aso solve problem of loc without lcoation data above

# pistons only
TKDlifetable2   %>%    filter(!item == "Liner" )  %>%  group_by(uloc2) %>%         #  View()
    mutate(  dateP = lead(datefit)          )       %>%
    mutate(  life = (dateP - datefit) )             %>% 
    mutate(  censure = ifelse(!Reason == "OOC", 0, 1   )    ) -> TKDlifeP   #  %>% View()


TKDlifetable2
TKDlifeL     %>% View()
TKDlifeP

TKDlifeL %>% group_by(Loc) %>% filter(is.na(life)) %>% 
    summarise(mean = mean(life), n = n())
    summarise( mean(life) )

#########################  Survial of TKD liners ###########
#########################  Survial of TKD liners ###########
######################### 31march TKD data  ####

TKDlifeLiners
# Reduce the make factors to EMD, GE, TRANSLOCO, TRANSOTHER, Other
levels(TKDlifeLiners$Make)
TKDlifeLiners  %>%  # mutate( Make = factor(Make )      )      %>%   
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

TKDlifeLfinal   %>% View()

levels(TKDlifeLfinal$Make)

names(TKDlifeL)  # "Loco" "datefit""item" "Loc" "Make" "Reason" "faildate" "life" "uloc2" "uloc3" "dateL" "censure"


TKDlifeLfinal %>%   filter(datefit > dmy("01/04/2010"))  -> TKDnewliners 
TKDnewliners  %>% View()

#library(summarytools)
#summarytools::descr(data)

# plot the data using ggplot2  ???????/
? TKDnewliners  %>% filter(life < 200 ) %>% # mutate( life2 = round( as.nemeric(life, 2)) )   %>% 
                  ??  ggplot( aes( x = datefit ,  y = ( today() - datefit ) ) ) +  geom_point()

## use TKDlife for exploring  ##########

TKDlife <- TKDnewliners 

TKDlife   %>% View()

# TKDpafit <- survfit( Surv(life ) ~ Make , data = TKDlife )
TKDpafit <- survfit( Surv(life, censure   ) ~ Make , data = TKDlife )
TKDpafit

ggsurvplot(
    TKDpafit,                     # survfit object with calculated statistics.
    # fun = "event",
    # fun = "cumhaz",       # fun = function(y) y*100 ,
    # linetype = "strata",     # change line type by groups
    size = 2,                # change line size
    data = TKDlife,             # data used to fit survival curves.
    
    conf.int = TRUE,         # show confidence intervals for  point estimates of survival curves.
    
    #pval = TRUE,             # show p-value of log-rank test.
    #pval = 0.03              # or pval = "The hot p-value is: 0.031"
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
    xlim = c(0,1000),         # present narrower X axis, but not affect survival estimates.
    xlab = "Failure Time in Days",   # customize X axis label.
    break.time.by = 90,     # break X axis in time intervals by 500.
    #ggtheme = theme_light(), # customize plot and risk table with a theme.
    ggtheme = theme_bw() ,
    
    #censor.shape="|", 
    #censor.size = 4,
     ncensor.plot = TRUE,      # plot the number of censored subjects at time t
     ncensor.plot.height = 0.25,
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


ggsurv   


################  Good chart !!
library(gghighlight)
TKDlife    

TKDlife %>%     
            ggplot( aes(#x =  fct_rev(   fct_infreq(  factor(Loco ) )   ) ,
                       x =  reorder(Loco, Loco),
                        y =  life     ) )    +
            geom_linerange( aes(ymin = 0,   ymax = ( today() - datefit ) ,  col = Reason   ) , size = 1) +
            geom_point(  aes(shape = Make, size = 2.5, color = Make),   stroke = 1, cex = 2) +
            #scale_shape_manual(values = c(1, 3, 4)) +
            labs(y = "Time (years)",  x = "Subject ID" )  + 
             font("xlab", size = 8, color = "blue", angle = 45  ) +    # angle ?
            #axis.text.x = element_text(angle = 90, color = "blue") +
           # coord_flip() +
            theme_bw() +  
            #theme(axis.text.x = element_blank(), axis.ticks = element_blank()) 
            ###  use element_text(color, size, face, family, angle, hjust, vjust)
            ### use element_line(coor, size, linetype)
             #gghighlight(   Reason == "6Y" ,      #life > 3500 & life <= 400,  
                    #     unhighlighted_colour = alpha("steelblue", 0.4),
                     #      use_direct_label = TRUE, label_key = name, label_params = list(size = 5)) +
             #  rotate_x_text(angle = 45)  # also ok
            theme (axis.text.x = element_text(angle = 45,  hjust = 1.0)  )      

#### not 
install.packages("factoextra")
library(factoextra)
install.packages("cluster")
library(cluster)
res.dist <- get_dist( TKDlife$life, stand = TRUE, method = "pearson") 
res.dist
fviz_dist(res.dist,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
########
orca
su_obj <- Surv(orca$time, orca$all)
su_obj <- Surv(TKDlife$life, TKDlife$censure   )
su_obj <- Serv()
str(su_obj)



m1 <- coxph(su_obj ~ sex + I((age-65)/10) + stage, data = orca)

#?? m1 <- coxph(su_obj ~ TKDlife$SHED , data = TKDlife)

summary(m1)

ggforest(m1, data = orca)



orca2 <- orca %>%
    filter(stage != "unkn") %>%
    mutate(st3 = Relevel(droplevels(stage), list(1:2, 3, 4)))
m2 <- coxph(Surv(time, all) ~ sex + I((age-65)/10) + st3, data = orca2, ties = "breslow")
round(ci.exp(m2), 4)


grid.arrange(
    ggforest(m1, data = orca),
    ggforest(m2, data = orca2),
    ncol = 2
)




?I
##########################
su_obj  <-  Surv(TKDlife$life, TKDlife$censure   )

m1 <- coxph(su_obj ~ TKDlife$Make + I(   (   TKDlife$datefit  -  as.Date("25/02/2010")   )   ) + TKDlife$Loc,  data = TKDlife)
summary(m1)
ggforest(m1, data = TKDlife)

#########################
# cox

orca   <-  read.table("http://www.stats4life.se/data/oralca.txt")

orca   <-  mutate(orca,  all = event != "Alive")
orca$all
su_obj <-  Surv(  orca$time,   orca$all   ) 

str(su_obj)

m1 <- coxph(su_obj ~ sex + I((age-65)/10) + stage, data = orca)
summary(m1)
ggforest(m1, data = orca)

gridExtra:: grid.arrange(    ggforest(m1, data = orca),
                             ggforest(m1, data = orca),
                             ggforest(m1, data = orca),
                             ncol = 2
                        )

#### Changing Labels %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(     title    = "The Power assembly failures over time since commissioning in TKD shed",                     
                                       subtitle = "Based on TKD shed actual data as on 30/03/19 and life given in days" ,  
                                       caption  = "TRANSLOCO liners failes withing a year ! "          )  +
                                       draw_label( "DRAFT!" ,  size = 50, angle = 30,  alpha = .2, x = 200, y = 50) + # adding draw layers !!
                                       draw_image("https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png", x = 50, y = 25, width = 20, height = 15    )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = " TKD Liners in service after so many days and thier Survival table",          
                                       subtitle = " Population in Locos and time to fail in days", 
                                       caption  = " TKD data as on 30/03/2019"        )

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










###############
#####################  till 31 march





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
sort loco+palocation - desc datefit
group on loco+palocation
if datefail == na , fill with next record date
time series of 
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

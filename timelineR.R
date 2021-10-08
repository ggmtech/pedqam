#devtools::install_github("daheelee/timelineS")

# 5 fucntions
# timelineS:  horizontal timeline with event descriptions at corresponding dates.
# timelineG:  faceted timelines for grouped data.
# durPlot: Plots boxplot, histogram, density plot, scatter plot, line plot and prints summary statistics for date duration data.
# durCalc: Calculates duration between two dates,to filter rows. Returns additional columns of durations in different units.
# durSummary: Returns summary statistics for date duration data.



library(timelineS)
mj_life

timelineS(mj_life, main = "Life of Michael Jackson")

timelineS(mj_life, main = "Life of Michael Jackson", 
          label.direction = "updown", 
          label.length = c(0.2,0.8,0.4,1.2), label.position = 3, label.angle = 90,label.color = "green",
          line.color = "blue",  
          point.color = "red", pch = "*", scale.cex = 0.85)

# general
timelineS(mj_life, 
          main = "Life of Michael Jackson",  #NA, 
          xlab = "(c) sub Life of Michael Jackson", #NA, 
          buffer.days = 150,
          line.color = "blue", #gray46", 
          line.width = 5, 
          
          scale = "year", 
          scale.format = "%Y", #"%d%m%Y"
          scale.font = 2, scale.orient = 1,scale.cex = 1,
          scale.above = FALSE,  
          scale.tickwidth = 1,
          # labels = paste(df[[1]], df[[2]]),   # make your lables
          label.direction = "updown", # "up", "down", "downup",
          label.length = c(0.5,0.5,0.8,0.8), 
          # label.position = c(1,3),
          label.color = "red", label.cex = 0.95, label.font = 0.61, label.angle = -90,
          pch = 20, point.cex = 1, 
          point.color = "gray44" )

getwd()
list.dirs()
list.files()

mydata = readxl::read_xlsx(path = "C:/Users/Lenovo/Downloads/" , "final Annexure-I (pending vendor registration cases)")


life_country
timelineG(df=life_country, 
          names="Name",
          start="Start", end="End",  
          phase="Phase",   # colours
          group1="Country", group2="Gender"    
          )    # + theme_bw()

# durPlot function gives five different plots by default. 
# You can set facet=TRUE to get faceted plots.
life_exp
durPlot(life_exp, 
        start="Birth", end="Death", 
        group="Country", timeunit="years", 
         facet=TRUE, binwidth=3, alpha=0.7, title=TRUE , plot_type= "boxplot") # "all" ?

?durPlot()
# plot_type	 One of "all", "boxplot", "histogram", "density", "scatter", "line". Default is "all".

################      For interactive visualisation widget
#install.packages("timevis")
#devtools::install_github("daattali/timevis")
library(timevis)   

timevis()

timevis(  data.frame(id = 1:2,
                     content = c("one", "two"),
                     start = c("2016-01-10", "2016-01-14"),
                     end = c("2016-01-13", "2016-01-16") )
       )



#add dataframe
data <- data.frame(
    id      = 1:4,
    content = c("Item one"  , "Item two"  ,"Ranged item", "Item four"),
    start   = c("2016-01-10", "2016-01-11", "2016-01-20", "2016-02-14 15:00:00"),
    end     = c(NA          ,           NA, "2016-02-04", NA)
)

timevis(data) # js based interactive?
###### Every item must have a content and a start variable. 
###### If the item is a range rather than a single point in time, you can supply an end as well. 
###### id is only required if you want to access or manipulate an item. 
###### see ?timevis() 
?timevis() 

# if editable = TRUE option, then the user will be able to add new items 
# by double clicking, modify items by dragging, and delete items by selecting them.
# You can use the groups feature to group together multiple items into different “buckets”


timevis() %>%
    addItem(list(id = "item1", content = "one", start = "2016-08-01",  end = "2016-09-11")) %>%
    centerItem("item1")




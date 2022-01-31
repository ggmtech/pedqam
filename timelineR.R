#devtools::install_github("daheelee/timelineS")

library(timelineS) # 5 fn: timelineS, timelineG, durPlot, durCalc, durSummary
mj_life
timelineS(  mj_life,    main = "Life of Michael Jackson" )   ## simplest !

# timelineS:  horizontal timeline with event descriptions at corresponding dates.
# timelineG:  faceted timelines for grouped data.
# durPlot: Plots boxplot, histogram, density plot, scatter plot, line plot and prints summary statistics for date duration data.
# durCalc: Calculates duration between two dates,to filter rows. Returns additional columns of durations in different units.

# durSummary: Returns summary statistics for date duration data.

#Timeline with Event Labels
#timelineS(df, main = NA, xlab = NA, buffer.days = 600,
# line.width = 5, line.color = "gray44",
# scale = "year", scale.format = "%Y", scale.font = 2, scale.orient = 1,
# scale.above = FALSE, scale.cex = 1, scale.tickwidth = 2,
# labels = paste(df[[1]], df[[2]]), label.direction = "downup",
# label.length = c(0.5,0.5,0.8,0.8), label.position = c(1,3),
# label.color = "gray44", label.cex = 0.8, label.font = 1, label.angle = 0,
# pch = 20, point.cex = 1, point.color = "gray44")
# df:  Data frame for events and dates. First column for event names and second column for dates 
# buffer.days	: Additional days  before and after  event dates on the timeline. Default 600 days.
# line.width	: Timeline width; default 5
# line.color	: Timeline color.
# scale	: Scale on timeline. One of "year","quarter", "month", "week" or "day". See seq.Date.
# scale.format	: Scale format; default "%Y".
# scale.font	: Integer values Default is 2. (1:plain, 2:bold, 3:italic, 4:bold italic, 5:symbol).
# scale.orient	: Orientation of scale; default 1(upright)
# scale.above	: If TRUE, the scale shows above the line.
# scale.cex	: Scale font size relative to cex.
# scale.tickwidth	: Width of scale tick; default 2.
# labels	: Event labels. Events and corresponding dates as default.
# label.direction	:  "downup","updown","up", or "down", default is "downup". See details.
# label.length	: Distance from timeline. single/vector of lengths. Default c(0.5, 0.5, 0.8, 0.8). 
# label.position	: Integer specifying label positions; default c(1,3). See details.
# label.color	: Label color(s).
# label.cex	: Font size(s) of event labels; default 0.8.
# label.font	: Integer specifying label font; default 1.
# label.angle	: Angle of text in the label.
# pch	: End point symbol(s).
# point.cex	:  End points size(s).
# point.color	: End points color(s).

library(timelineS)
mj_life
timelineS(  mj_life,   main = "Life of Michael Jackson")  # ## simplest

## Labels above timeline and other change in aesthetics
timelineS(mj_life, 
          main = "Life of \n Michael Jackson", 
          label.direction = "up", 
          label.length = c(0.25,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,   0.9 ),  
          label.position = 4,  #default c(1,3)
          line.color = "blue",   # main line scale
          line.width	= 8,
          scale = "year", scale.format = "%b-%Y", scale.font = 1,scale.cex = .8, scale.orient = 2,
          label.color = "red",   # leader line
          label.angle = 90,      # text rotation
          point.color = "green", pch = "*"  , point.cex= 2     # leader end point
          )


life_country
# timelineG(df, start, end, names, phase = NA, group1 = NA, group2 = NA,
#           width = 2, color = "grey", theme = NULL, other = NULL)
# names	: Column in df for names of each timeline
# phase	: Column in df for phases.
# group1	:Column in df for groups as rows. Default is NA.
# group2	:Column in df for groups as the columns. Default is NA.
# width	: Width of each timeline. Default is 2.
# color	: Color of timelines, only used when phase is not provided.
# theme	: Add theme elements if needed.
# other	: Add other elements if needed.


### Plot timelines, no group
timelineG(df = life_country, 
          names = "Name", start = "Start", end = "End",  phase="Phase", color = "grey") # color, if no phase
### Plot timelines row-grouped by "Country"
timelineG(df = life_country, start = "Start", end = "End", names = "Name", phase = "Phase", group1 = "Country")
### Plot timelines row-grouped by "Country" and column-grouped by "Gender"
timelineG(df=life_country,         # Grouped Data Faceted Timelines
          names="Name",  phase="Phase",   start="Start", end="End",    # name, colour seg, timeseg
          group1="Country",      # row-grouped by "Countr       # y axis facet
          group2="Gender"        # column-grouped by "Gender"    # x axis facet
)    # + theme_bw() #no ggplot thene


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


# durPlot
# durPlot function gives five different plots by default. 
# durPlot(df, start, end, group = NA, timeunit = "days", plot_type = "all",
# facet = FALSE, facet.nrow = NULL, theme = NULL, other = NULL,
# fill_color = "black", line_color = "black", groupcolor = TRUE,
# point_size = 2, alpha = NA, binwidth = 0.5, show_legend = TRUE,
# title=FALSE, title_boxplot = "Boxplot", title_histogram = "Histogram",
# title_density = "Density Plot", title_scatter = "Scatter Plot",
# title_line = "Line Plot")

# Set facet=TRUE to get faceted plots.
# df	: Dataframe with start dates, end dates and groups.
# start/end	: # Column in df for start/end dates.
# group	:Column in df for groups. Default is NA.
# timeunit : "day(s)", "week(s)", "month(s)", "quarter(s)", "semiannual", "halfyear",or "year(s)".
# plot_type	: "all", "boxplot", "histogram", "density", "scatter", "line"
# facet	:If TRUE, wraps plots in group facets
# facet.nrow	# Number of rows for facet wrap
# theme	: Add theme elements if needed.
# fill_color	# line_color	# groupcolor	
# If FALSE, fill_color and line_color used for all groups. Default is TRUE.
# point_size :  Point size for scatterplot
# alpha	 # Color transparency [0,1]
# binwidth	for histogram; default 0.5.
# show_legend	: Default is TRUE
# title	: # If TRUE, puts main titles for each plot
# title_boxplot	:# Title for boxplot title
# title_histogram	:# Title for histogram
# title_density: # Title for density plot
# title_scatter: # Title for scatter plot
# title_line	 : # Title for line plot
life_exp
durPlot(life_exp, 
        #plot_type= "histogram", "boxplot", "histogram", "density", "scatter", "line". Default: "all".
        plot_type= "boxplot" ,
        start="Birth", end="Death", 
        group="Country", 
        timeunit="years", 
        facet=TRUE, 
        binwidth=3, alpha=0.7, title=TRUE, 
        )   # 



## durCalc()

### Filter people who lived longer than 85 years
durCalc(life_exp, start = "Birth", end = "Death",     timeunit = "years",  filterlength = 85 )

### How old each person would be as of January 1, 2000 if they were alive
durCalc(life_exp, start = "Birth", end = as.Date("2000-1-1"),       timeunit = "years")

### Use as unit-converter between two dates
durCalc(start = as.Date("2010-12-1"), end = as.Date("2015-4-26"),   timeunit = "weeks")


#durSummary()
durSummary(life_exp, start = "Birth", end = "Death",  group = "Country",  timeunit = "years")

################################################################
################################################################
################################ For interactive visualisation widget
#install.packages("timevis")
#devtools::install_github("daattali/timevis")
library(timevis)   # timevis()

timevis::timevis(  data.frame(id = 1:2,
                     content = c("one",        "two"       ),
                     start   = c("2016-01-10", "2016-01-14"),
                     end     = c("2016-01-13", "2016-01-16") )
                 )



#add dataframe
data <- data.frame(
    id      = 1:4,
    content = c("Item one"   , "Item two"  ,"Ranged item", "Item four"),
    start   = c("2012-01-11" , "2016-01-11", "2016-01-20", "2016-02-14 15:00:00"),  #all needs start
    end     = c( "2016-01-10" ,   NA ,       "2019-02-04",                    NA)   # endpoint time
                   )
data
timevis::timevis(data) # js based interactive range plot
# timevis()
###### Every item must have a content and a start variable, end also it it is a time range
###### id required if you want to access or manipulate an item. 
?timevis() 

# if editable = TRUE option, then the user will be able to add new items 
# by double clicking, modify items by dragging, and delete items by selecting them.
# You can use the groups feature to group together multiple items into different “buckets”


timevis() %>%
    addItem(list(id = "item1", content = "one", start = "2016-08-01",  end = "2016-09-11")) %>%
    centerItem("item1")


#----------------------- Minimal data -----------------
timevis( data.frame(id = 1:2,
               content = c("one", "two"),
               start = c("2016-01-10", "2016-01-12"))
)

#----------------------- Hide the zoom buttons, allow items to be editable -----------------
timevis( data.frame(id = 1:2,
         content = c("one", "two"),
         start = c("2016-01-10", "2016-01-12")),
         showZoom = FALSE,
         options = list(editable = TRUE, height = "200px")
       )

#----------------------- You can use %>% pipes to create timevis pipelines -----------------
timevis() %>% setItems(
                data.frame(
        id = 1:2,
        content = c("one", "two"),
        start = c("2016-01-10", "2016-01-12")
    )) %>%
    setOptions(list(editable = TRUE)) %>%
    addItem(list(id = 3, content = "three", start = "2016-01-11")) %>%
    setSelection("3") %>%
    fitWindow(list(animation = FALSE))

#------- Items can be a single point or a range, and can contain HTML -------
timevis(  
    data.frame(id = 1:2,
               content = c("one", "two<br><h3>HTML is supported</h3>"),
               start = c("2016-01-10", "2016-01-18"),
               end = c("2016-01-14", NA),
               style = c(NA, "color: red;")
              )
      )


data.frame(    id = 1:2,
               content = c("one", "two<br><h3>HTML is supported</h3>"),
               style = c(NA, "color: red;"),
              # type = c("point", "background"),
               start = c("2016-01-10", "2016-01-18"),
               end = c("2016-01-14", NA)
         )  %>% timevis( )


#----------------------- Alternative look for each item -----------------
timevis(
    data.frame(id = 1:2,
               content = c("one", "two"),
               start = c("2016-01-10", "2016-01-14"),
               end = c(NA, "2016-01-18"),
               type = c("point", "background") )
)

#----------------------- Using a function in the configuration options -----------------
?timevis( data.frame(id = 1,
                    content = "double click anywhere<br>in the timeline<br>to add an item",
                    start = "2016-01-01"),
                    options = list(
                        editable = TRUE,
                        onAdd = htmlwidgets::JS('function(item, callback) {
                                           item.content = "Hello!<br/>" + item.content;
                                            callback(item);
                                          }')
                        )
         )
#----------------------- Using groups -----------------
timevis(data = data.frame(
    start = c(Sys.Date(), Sys.Date(), Sys.Date() + 1, Sys.Date() + 2),
    content = c("one", "two", "three", "four"),
    group = c(1, 2, 1, 2)),
    groups = data.frame(id = 1:2, content = c("G1", "G2"))
)


#----------------------- Getting data out of the timeline into Shiny -----------------
if (interactive()) { library(shiny)
    data <- data.frame(
        id = 1:3,
        start = c("2015-04-04", "2015-04-05 11:00:00", "2015-04-06 15:00:00"),
        end = c("2015-04-08", NA, NA),
        content = c("<h2>Vacation!!!</h2>", "Acupuncture", "Massage"),
        style = c("color: red;", NA, NA)
    )
    
    ui <- fluidPage(
        timevisOutput("appts"),
        div("Selected items:", textOutput("selected", inline = TRUE)),
        div("Visible window:", textOutput("window", inline = TRUE)),
        tableOutput("table")
    )
    
    server <- function(input, output) {
        output$appts <- renderTimevis( timevis( data, options = list(editable = TRUE, multiselect = TRUE, align = "center") )  )
    
        output$selected <- renderText( paste(input$appts_selected, collapse = " ") )
        output$window <- renderText( paste(input$appts_window[1], "to", input$appts_window[2]) )
        output$table <- renderTable( input$appts_data )
    }
    shinyApp(ui, server)
}


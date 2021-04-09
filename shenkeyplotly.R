install.packages("plotly")

library(plotly)
library(rjson)
library(Cairo)

p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p     #worked !


# By default, Plotly for R runs locally in your web browser or in the R Studio viewer.

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
     geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
     geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
p

p <- ggplotly(p)



json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
    type = "sankey",
    domain = list(                 # worked by makind c to list
        x =  c(0,1),
        y =  c(0,1)
    ),
    orientation = "h",
    valueformat = ".0f",
    valuesuffix = "TWh",
    
    node = list(
        label = json_data$data[[1]]$node$label,
        color = json_data$data[[1]]$node$color,
        pad = 15,
        thickness = 15,
        line = list(
            color = "black",
            width = 0.5
        )
    ),
    
    link = list(
        source = json_data$data[[1]]$link$source,
        target = json_data$data[[1]]$link$target,
        value =  json_data$data[[1]]$link$value,
        label =  json_data$data[[1]]$link$label
    )
) %>% 
    layout(
        title = "Energy forecast for 2050 ",
        font  = list( size = 10 ),
        xaxis = list(showgrid = F, zeroline = F),
        yaxis = list(showgrid = F, zeroline = F)
    )

p  # not working



# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="sankey-basic")
chart_link

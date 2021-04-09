#shenky
#https://plot.ly/r/sankey-diagram/

library(plotly)
packageVersion('plotly')

#Basic Sankey Diagram

p <- plot_ly(
    type = "sankey",
    orientation = "h",
    
    node = list(
        label = c("A1", "A2", "B1", "B2", "C1", "C2"),
        color = c("blue", "blue", "blue", "blue", "blue", "blue"),
        pad = 15,
        thickness = 20,
        line = list( color = "black", width = 0.5   )
    ),
    
    link = list(
        source = c(0,1,0,2,3,3),
        target = c(2,3,3,4,4,5),
        value =  c(8,4,2,8,4,2)
    )
) %>% 
    layout(
        title = "Basic Sankey Diagram",
        font = list( size = 10  )
    )


# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="sankey-basic-example")
chart_link


#Create Canvas
library(plotly)

p <- plot_ly(
    type = "sankey",
    domain = c(
        x =  c(0,1),
        y =  c(0,1)
    ),
    orientation = "h",
    valueformat = ".0f",
    valuesuffix = "TWh"
) %>%
    layout(
        title = "Energy forecast for 2050, UK - Department of Energy & Climate Change",
        font = list(
            size = 10
        ),
        xaxis = list(showgrid = F, zeroline = F),
        yaxis = list(showgrid = F, zeroline = F)
    )

#Add Nodes
library(plotly)
library(rjson)

json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
    type = "sankey",
    domain = c(
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
    )
) %>% 
    layout(
        title = "Energy forecast for 2050, UK - Department of Energy & Climate Change",
        font = list(
            size = 10
        ),
        xaxis = list(showgrid = F, zeroline = F),
        yaxis = list(showgrid = F, zeroline = F)
    )

# Add Links
library(plotly)
library(rjson)

json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
    type = "sankey",
    domain = c(
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
        title = "Energy forecast for 2050<br>Source: Department of Energy & Climate Change, Tom Counsell via <a href='https://bost.ocks.org/mike/sankey/'>Mike Bostock</a>",
        font = list(
            size = 10
        ),
        xaxis = list(showgrid = F, zeroline = F),
        yaxis = list(showgrid = F, zeroline = F)
    )

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="sankey-basic")
chart_link












#Style Sankey Diagram
library(plotly)
library(rjson)

json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy_dark.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
    type = "sankey",
    domain = c(
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
        title = "Energy forecast for 2050<br>Source: Department of Energy & Climate Change, Tom Counsell via <a href='https://bost.ocks.org/mike/sankey/'>Mike Bostock</a>",
        font = list(
            size = 10,
            color = 'white'
        ),
        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
        yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black'
    )

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="sankey-dark")
chart_link

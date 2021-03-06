---
title: "Dashboard visualizations in R: Distrubution"
author: "Kristian Larsen"
output: 
    flexdashboard::flex_dashboard:
    orientation: rows
vertical_layout: scroll
---
    
```{r setup, include=FALSE}
library(ggplot2)
library(plotly)
theme_set(theme_classic())
```

Row
-----------------------------------------------------------------------
    
    ### Chart A: Histogram with Auto Binning
    
    ```{r}
g <- ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
    labs(title="Histogram with Auto Binning", 
         subtitle="Engine Displacement across Vehicle Classes")  
ggplotly(p = ggplot2::last_plot())
```


### Chart B: Histogram with Fixed Bins

```{r}
g + geom_histogram(aes(fill=class), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
    labs(title="Histogram with Fixed Bins", 
         subtitle="Engine Displacement across Vehicle Classes") 
ggplotly(p = ggplot2::last_plot())
ggplotly(p = ggplot2::last_plot())
```

Row
-----------------------------------------------------------------------
    
    ### Cart C: Histogram on a Categorical variable
    
    ```{r}
library(ggplot2)
theme_set(theme_classic())
g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable", 
         subtitle="Manufacturer across Vehicle Classes") 
ggplotly(p = ggplot2::last_plot())
```

### Cart D: Density plot

```{r}
library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
    labs(title="Density plot", 
         subtitle="City Mileage Grouped by Number of cylinders",
         caption="Source: mpg",
         x="City Mileage",
         fill="# Cylinders")
ggplotly(p = ggplot2::last_plot())
```

Row
-----------------------------------------------------------------------
    
    ### Cart E: Box plot
    
    ```{r}
library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="City Mileage grouped by Class of vehicle",
         caption="Source: mpg",
         x="Class of Vehicle",
         y="City Mileage")
ggplotly(p = ggplot2::last_plot())
```

### Cart F: Box plot

```{r}
library(ggthemes)
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(aes(fill=factor(cyl))) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Box plot", 
         subtitle="City Mileage grouped by Class of vehicle",
         caption="Source: mpg",
         x="Class of Vehicle",
         y="City Mileage")
ggplotly(p = ggplot2::last_plot())
```

Row
-----------------------------------------------------------------------
    
    ### Cart G: Box plot + Dot plot
    
    ```{r}
library(ggplot2)
theme_set(theme_bw())

# plot
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot() + 
    geom_dotplot(binaxis='y', 
                 stackdir='center', 
                 dotsize = .5, 
                 fill="red") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Box plot + Dot plot", 
         subtitle="City Mileage vs Class: Each dot represents 1 row in source data",
         caption="Source: mpg",
         x="Class of Vehicle",
         y="City Mileage")
ggplotly(p = ggplot2::last_plot())
```

### Cart H: Tufte Boxplot


```{r}
library(ggthemes)
library(ggplot2)
theme_set(theme_tufte())  # from ggthemes

# plot
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_tufteboxplot() + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Tufte Styled Boxplot", 
         subtitle="City Mileage grouped by Class of vehicle",
         caption="Source: mpg",
         x="Class of Vehicle",
         y="City Mileage")
```

Row
-----------------------------------------------------------------------
    
    ### Cart I: Violin plot
    
    ```{r}
library(ggplot2)
theme_set(theme_bw())

# plot
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
    labs(title="Violin plot", 
         subtitle="City Mileage vs Class of vehicle",
         caption="Source: mpg",
         x="Class of Vehicle",
         y="City Mileage")
ggplotly(p = ggplot2::last_plot())
```

### Cart J: Population Pyramid


```{r}
library(ggplot2)
library(ggthemes)
options(scipen = 999)  # turns of scientific notations like 1e+40

# Read data
email_campaign_funnel <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/email_campaign_funnel.csv")

# X Axis Breaks and Labels 
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

# Plot
ggplot(email_campaign_funnel, aes(x = Stage, y = Users, fill = Gender)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks,   # Breaks
                       labels = lbls) + # Labels
    coord_flip() +  # Flip axes
    labs(title="Email Campaign Funnel") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5), 
          axis.ticks = element_blank()) +   # Centre plot title
    scale_fill_brewer(palette = "Dark2")  # Color palette
```
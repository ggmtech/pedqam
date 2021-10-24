LT3 <- tibble::tribble(
    ~Month, ~ItemsLessThan3Vendors,
  "Jan-19",                   241L,
  "Feb-19",                   242L,
  "Mar-19",                   243L,
  "Apr-19",                   246L,
  "May-19",                   251L,
  "Jun-19",                   254L,
  "Jul-19",                   246L,
  "Aug-19",                   243L,
  "Sep-19",                   225L,
  "Oct-19",                   223L,
  "Nov-19",                   222L,
  "Dec-19",                   219L,
  "Jan-20",                   220L,
  "Feb-20",                   220L,
  "Mar-20",                   163L,
  "Apr-20",                   137L,
  "May-20",                   137L,
  "Jun-20",                   107L,
  "Jul-20",                    97L,
  "Aug-20",                    96L,
  "Sep-20",                    86L,
  "Oct-20",                    77L,
  "Nov-20",                    74L,
  "Dec-20",                    71L,
  "Jan-21",                    60L,
  "Feb-21",                    60L,
  "Mar-21",                    57L,
  "Apr-21",                    57L,
  "May-21",                    46L,
  "Jun-21",                    47L,
  "Jul-21",                    49L,
  "Aug-21",                    47L,
  "Sep-21",                    47L
  )

LT3

library(tidyverse)
library(lubridate)
library(timetk)
# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE

LT3 %>% mutate( months = my(Month)  ) %>% 
  plot_time_series( .date_var = months, 
                    ItemsLessThan3Vendors, 
                   #.interactive = interactive,
                   # .plotly_slider = TRUE
                   )


library(timelineS)
LT3 %>% mutate( mymonths = my(Month)  ) %>% select(- Month, mymonths, ItemsLessThan3Vendors)  -> tmp


#%>% filter( year(month)  = "2021" ) -> LT4
timelineS(tmp,  main = "Life of Michael Jackson")
mj_life


LT3 %>% mutate( months = my(Month)  ) %>% 
  ggplot2::ggplot(  aes(x = months, y = ItemsLessThan3Vendors )  , color = "red"  ) + 
  #geom_point()   + 
  geom_col( aes(fill = "blue")) + 
  geom_text(aes(label=ItemsLessThan3Vendors), vjust=-0.3, size=3.5) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_date(labels = date_format("%m-%Y") ) +  
  labs(title = "Reduction in items with less than 3 Vendors over the time time",
       x = "Month and Year",
       y = "Number of RDSO controlled items with less than 3 vendors"  ) +
 #  theme_minimal() + 
  
  theme_bw() +
  theme(legend.position = "none")  #theme_bw()                  # +   geom_text(   ItemsLessThan3Vendors  )

# p + geom_text(aes(label = Frequency), size = 3, hjust = 0.5, vjust = 3, position =     "stack") 
# p + geom_text(size = 3, position = position_stack(vjust = 0.5))
# 
# ggplot(Data, aes(x = Year, y = Frequency, fill = Category, label = Frequency)) +
# geom_bar(stat = "identity") +
# geom_text(size = 3, position = position_stack(vjust = 0.5))



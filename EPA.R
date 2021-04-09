library(tidyverse)
library(knitr)
library(rvest)
library(hrbrthemes) 
library(ggrepel)   

epi_2018_site <- "https://epi.envirocenter.yale.edu/epi-topline"

epi_2018_df <-  read_html(epi_2018_site)    %>%    
                html_table()                %>%   #html_table(x, header = NA, trim = TRUE, fill = FALSE, dec = ".")
                as.data.frame()             %>%
                arrange(EPI.Ranking)        
    
glimpse(epi_2018_df)


births <- read_html("https://www.ssa.gov/oact/babynames/numberUSbirths.html")
html_nodes(births, "table")[[2]]
html_table(  html_nodes(births, "table" )[[2]]     )

html_table(  html_nodes(births, "table" ) ,  fill = TRUE )   #fill for inconsitant columns

# If the table is badly formed, and has different number of rows in each column use fill = TRUE due to incorrect colspan specification.

skiing    <-   read_html("http://data.fis-ski.com/dynamic/results.html?sector=CC&raceid=22395")
skiing    %>%  html_table(fill = TRUE)


## Which Countries are the top 5 countries 
epi_2018_df %>% arrange(EPI.Ranking) %>% head(n=5) %>% kable()
## Which Countries are the bottom 5?
epi_2018_df %>% arrange(EPI.Ranking) %>% tail(n=5) %>% kable()


epi_2018_df %>% 
                 arrange( EPI.Ranking ) %>%
                 ggplot(aes( x = Environmental.Health ,  y = Ecosystem.Vitality)) +
                 geom_point( aes( color = Environmental.Performance.Index )  ) +
                 geom_path(size=0.2, color="#33333390", linetype=3)+
                 scale_color_viridis_c( name="EPI" )     +
                # theme_ipsum_rc() +
                 geom_smooth(method="lm", se=F, color="#33333330")      +
                 ggrepel::geom_text_repel(  aes(label=paste0(EPI.Ranking,".",Country) ), 
                                            data = . %>% filter( EPI.Ranking<=10 | EPI.Ranking>=170 | Country %in% c("Canada","United States of America","Japan","Ireland") ),
                                           #family="Roboto Condensed", 
                                            min.segment.length=0)       +
                 labs(title = "Environmental Health vs Ecosystem Vitality",
                 subtitle = "Environmental Health = Rises with Economic Growth & Prosperity\nEcosystem Vitality = Comes Under Strain from Industrialization & Urbanization",
                 caption = "Data from https://epi.envirocenter.yale.edu/epi-topline"   )


# Using countrycode package, I wanted to append extra information about country, such as “Continent” and “Region”.
# I’ve coloured plot with “continent” to see if there’s a cluster…

library(countrycode)
library(ggalt)

epi_2018_df <- epi_2018_df %>% 
               mutate(continent = countrycode(Country ,"country.name", "continent", warn=F),
               country_code = countrycode(Country, "country.name", "iso3c",nomatch = ""),
               region = countrycode(Country, "country.name", "region", warn=F))

epi_2018_df <- epi_2018_df  %>%
               mutate( detail_url = paste0( "https://epi.envirocenter.yale.edu/epi-country-report/" , country_code))

epi_2018_df
## Micronesia didn't get categorized...

epi_2018_df %>% 
    ggplot(aes( x=Environmental.Health , y= Ecosystem.Vitality, color=continent)) +
    geom_point( aes(color=continent), size=3, alpha=0.6) +
    geom_encircle(na.rm=T, s_shape=1, linetype=3, alpha=0.6) +
    scale_color_viridis_d(name="Continent", na.value="grey", option="A", end=0.9) +
    geom_rug(alpha=0.3) +
 #?   theme_ipsum_rc() +
    geom_text_repel(  aes(  label=paste0(EPI.Ranking,".",Country)  ),   #? family="Roboto Condensed", 
                        min.segment.length=0, size=2, 
                       color="#00000090", segment.colour = "#33333350") +
    labs( title = "Environmental Health vs Ecosystem Vitality",
          subtitle = "Colour = Continent") +
    scale_x_continuous(breaks=round(fivenum(epi_2018_df$Environmental.Health),1)) +
    scale_y_continuous(breaks=round(fivenum(epi_2018_df$Ecosystem.Vitality),1))


#Using countrycode package, I wanted to append extra information about country, such as “Continent” and “Region”.

#EPI and GDP per Capiata is correlated strongly. So I’ve decided to create another scatter plot showing that relationship.

epi_2018_df_comb

epi_2018_df_comb %>%    # ??
    ggplot(aes(x=GDPpc, y=Environmental.Performance.Index )) +
    geom_point(aes(color=PopDensity)) +
    theme_ipsum_rc() +
    geom_smooth(se=F, method="lm", color="#33333330") +
    scale_x_comma() +  
    scale_color_viridis_c(trans="log10") +
 #?   geom_text_repel(aes(label=country), 
  #?                  data = . %>% filter(GDPpc > 60000 | Environmental.Performance.Index > 80 | 
#?                                            country %in% c("United States of America","Japan","Canada","India","China")),
                  #? family="Roboto Condensed", min.segment.length = 0, nudge_x=10) +
    labs(title="GDP per Capita vs EPI",
         caption = "Data Source: https://epi.envirocenter.yale.edu/epi-downloads",
         xlab ="GDP per Capita - 2018", ylab="Environmental Performance Index")




########### learn sorting  #############
# accompnying code for  https://jozefhajnala.gitlab.io/r/r008-sorting-data/
# Original data src: # http://ec.europa.eu/eurostat/web/sector-accounts/data/annual-data in million Euro

gdi <- read.csv( url("https://jozefhajnala.gitlab.io/r/post/data/ESA2010_GDI.csv"),  stringsAsFactors = FALSE   )

gdi_reversed_rows <- gdi[nrow(gdi):0, ]
gdi_reversed_cols <- gdi[, ncol(gdi):0]
gdi_reversed      <- gdi[nrow(gdi):0, ncol(gdi):0, drop = FALSE]

rowidx            <- order(gdi[, "Y.2016"])      # Get the order by GDI in 2016
gdi_sorted        <- gdi[rowidx, , drop = FALSE]

gdi_sorted <- gdi[ order( gdi[, "Y.2016"] ),  , drop = FALSE]  # We can of course do it in one go:

gdi_sorted[, c(1, 23)]    # Look at the 2 relevant columns of the result

rowidx <- order(gdi[, "Y.2016"], gdi[, "country"])     # Use multiple vectors for ordering
gdi[rowidx, c(1, 23), drop = FALSE]


# To order in descending order, we can use `decreasing = TRUE` ----------------
# To see NAs first we can use `na.last = FALSE`
rowidx <- order(gdi[, "Y.2016"], decreasing = TRUE, na.last = FALSE)
gdi[rowidx, c(1, 23), drop = FALSE]

# Order by GDI in 2016 descending and then by country alphabetically ----------
rowidx <- order(-gdi[, "Y.2016"], gdi[, "country"])
gdi[rowidx, c(1, 23), drop = FALSE]

# Reverse order of-non numeric vectors with -xtfrm ----------------------------
rowidx <- order(gdi[, "Y.2016"], -xtfrm(gdi[, "country"]))
gdi[rowidx, c(1, 23), drop = FALSE]




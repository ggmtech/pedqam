# https://www.r-bloggers.com/2021/07/working-with-web-data-in-r-part-ii-apis/
# http://api.open-notify.org/ is a simple example of an API server. 
# It has two end points: one that tells you where the International Space Station (ISS) is right now, 
# and one that tells you who is in space at this moment. Let’s use the second endpoint.


library(tidyverse)
library(httr)
library(jsonlite)
# the name of the end point is called "astros."
req <- GET("http://api.open-notify.org/astros")
str(req)

# We can wrap our req object around the httr::content() it has an as = argument : "raw",   "text", or "parsed"

req_content <- content(req, as = "text")
people_list <- fromJSON(req_content, flatten = TRUE)
str(people_list)

people <- as_tibble(people_list$people)
people

theme_set(theme_minimal())
people %>% 
    count(craft) %>% 
    ggplot(aes(x = craft, y = n, fill = craft)) +
    geom_col() +
    scale_fill_viridis_d(option = "magma", begin = 0.4, end = 0.8) +
    coord_flip() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold"), 
          legend.position = "none") +
    labs(
        title = "Number of people in space right now",
        subtitle = "By spacecraft",
        x = element_blank(),
        y = element_blank()
    )



# OAuth 2.0 is a way for a resource server (say, Twitter API) to grant access to (part of) your account to an app (say, a Twitter alternative app, or rtweet) on your behalf. 
# It is safer than previous industry standards, see the video below.
# OAuth 2.0 dance is to get such an access token. you will also get a refresh token (similarly a string, e.g. 3bdeb3bd19a3093674f4e7ba6e1be1706ab1f16af9ebf3b79a67a807c622b650



# OAuth endpoint : via httr::oauth_endpoint()) an object holding the URLs  to request and refresh an access token. 
# Or built-in httr::oauth_endpoints() to popular APIs.
#  httr::oauth_endpoint() ? By reading the API docs and looking at examples. ???? E.g. compare ORCID API docs and rorcid source code.

# create an OAuth app (via httr::oauth_app()) which is an object holding the name, secret and ID of your third-party app



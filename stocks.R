## By capital gains 
## # bhav copy on 31 Jan 2021

packages <- c("tidyverse",   "magrittr",  "here", "devtools",
              "lubridate", "anytime", "googlesheets4", "readxl", "DT", "knitr",
              "DataExplorer", "inspectdf", "summarytools", "skimr",  "broom", "naniar", 
              
              #"ggplot2", "dplyr", "tidyr", "magrittr", "stringr","forcats", "reshape2",
              "ggrepel", "scales", "ggpubr", "ggridges","GGally",  "ggthemes", #"themr",
              "gridExtra","plotly", "timetk", "ggforce", "ggraph", 
              "cowplot", "drat", "coefplot",
              "viridis", "viridisLite",    
              #"sf",
              "survival", "survminer", "ggfortify",
              "magick"  # #"gfx" ,   "prismatic"              
)
installed_packages <- packages %in% rownames(installed.packages())
if ( any( !installed_packages ) ) { install.packages(packages[!installed_packages]) }
lapply(packages, library, character.only = TRUE) #%>% invisible()



##############################################################################################
# Get raw Data from google sheet VR Portal Status ##########################################
ss = "https://docs.google.com/spreadsheets/d/1IjRze1S4Ahy8TWDwQd_OEkotFRfKJK_jigBB6ab0x1Q/edit#gid=334628482" # my ITR21 google sheet

sheet_names(ss)  # see sheets names
mysheet = "hdsc21"
gkitr21 <-googlesheets4::read_sheet(ss, sheet = mysheet , col_names = TRUE,  col_types = NULL , 
                                    skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"

gkitr21
gkitr21 %>% View()
csvfile <- "EQ_ISINCODE_310118.CSV"
bhav31jan2018 <- read_csv("EQ_ISINCODE_310118.CSV")

# Detailed
bhav31jan2018 <- read_csv(file="EQ_ISINCODE_310118.CSV" ,
col_names = TRUE,
col_types = NULL,
col_select = NULL,
id = NULL,
locale = default_locale(),
na = c("", "NA"),
# quoted_na = TRUE,
# quote = "\"",
# comment = "",
trim_ws = TRUE,
skip = 0,
#n_max = 10000, # Inf,
#guess_max = min(1000, n_max),
name_repair = "unique",
#num_threads = readr_threads(),
progress = show_progress(),
show_col_types = should_show_types(),
skip_empty_rows = TRUE,
lazy = TRUE)

xlfile <- "stock31jan2018.xlsx"
read_excel(path = xlfile, sheet = NULL, range = NULL, col_names = TRUE,
           col_types = NULL, na = "", trim_ws = TRUE, skip = 0,
          # n_max = Inf, guess_max = min(1000, n_max),
          # progress = readxl_progress(), 
           .name_repair = "unique")

stock31jan18 <- read_excel("stock31jan2018.xlsx", sheet = "BSE 31.01.2018")
stock31jan18nse <- read_excel("stock31jan2018.xlsx", sheet = "NSE31.01.2018")
stock31jan18nse
stock31jan18

bhav31jan2018
bhav31jan2018 %>% View()

indexedgkitr <-  dplyr::left_join( gkitr21, bhav31jan2018 , by =c("ISIN" = "ISIN_CODE" )   )
indexedgkitr <-  dplyr::left_join( gkitr21, stock31jan18nse , by =c("SECURITY" = "Name" )   )

indexedgkitr <- fuzzyjoin::stringdist_inner_join(gkitr21, stock31jan18nse , by =c("SECURITY" = "Name" ) )

Share

indexedgkitr
indexedgkitr  %>% View()


gkitr21


gs4_auth()  # autorise
gs4_create() # Creates random named new Sheet: foliaged-kawala. or
gs4_create("test_sheet")
df2 <- gs4_create("test_sheet2", sheets = list(data = df)) # wiith data

df4 <- gs4_create("test_sheet4",  sheets = "2008 results" )
# individual sheet deleted sheet_delete() nut not entire spreadsheets using gs4.
#  sheet_write() like  gs4_create(). Ican  copy data from one Google spreadsheet to another,
df5 <- sheet_write(df)
sheet_write(Products, df5, sheet = "Products")
range_write(df5, chosen_column, sheet='Products', range = "G") # adding data to colomn "G"
# We can clear the values in this column by using range_clear() or range_flood():
range_clear(df5, sheet='Products', range = "G")  # or
range_flood(df5, sheet='Products', range = "G")

sheet_append(df5, ProductsNew, sheet = 2)

library(alluvial)

mysheet = "VendorDisposalsOct21"

myaluvialdata <-  googlesheets4::read_sheet(ss, sheet = mysheet , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"

myaluvialdata

myaluvialdata %>% vtree::vtree( "Directorate")
myaluvialdata %>% vtree::vtree( "CCA_done")
myaluvialdata %>% vtree::vtree( c("CCA_done",  "Directorate" ) )
myaluvialdata %>% vtree::vtree( c( "Directorate" , "CCA_done") 
                                # ,showlegend=TRUE,shownodelabels=FALSE
)


ToothGrowth
vtree::vtree(ToothGrowth,"supp dose",  splitwidth = 80, sameline=TRUE,
             summary="len>20 \n %pct% length > 20" )



InsectSprays
vtree::vtree(InsectSprays, "spray",
             splitwidth = 80,sameline=TRUE,
             summary = "count \ncounts: %list%%noroot%",
             cdigits = 0 )




myaluvialdata %>%   #View()
  select(Directorate, Time_Category, CCA_done , Approved_Or_Closed) %>% 
  mutate( Time_Category = as_factor(Time_Category) ,
          CCA_done = as_factor(CCA_done),
          Directorate = as_factor(Directorate) ) %>% 
  group_by(Directorate,  Time_Category, CCA_done, Approved_Or_Closed) %>%
  summarise(Freq = n() ) %>% ungroup()  -> tmp
tmp
tmp %>% vtree::vtree( "Directorate CCA_done",  
                      # horiz=FALSE, 
                      #showlegend=TRUE,
                      # shownodelabels= TRUE
                      # labelnode=list(Sex=c(Male="M",Female="F"))
)

# If you have NAs, you can try to replace them in this way:
# # first, you should use the option stringsAsFactor = F in the data,
# # second, replace them with something you like: data[is.na(data)] <- 'nothing'

# not clear about NA problems to me
tmpdf <- as.data.frame(tmp[, c(1:2, 5)], stringsAsFactors = FALSE)


filter(is.na(tmpdf) )
tmpdf
tmp[is.na(tmp)]  <- "No"
tmpdf[is.na(tmpdf)] <- "qa"

class(tmpdf)
names(tmpdf)

# not ok
tmpdf[ , 1:2] %>% count(Directorate ,  Time_Category) -> tmpdf
tmpdf








# Stock 
library(quantmod)
library()
# Read csv file directly into R
ticker_list <- read.csv("https://www1.nseindia.com/content/indices/ind_nifty50list.csv")
class(ticker_list)
head(ticker_list[,1:3], 5)  # Check if data was red in correctly


# Note that for India, tickers need to be appended with ".NS"
tickers <- paste0(ticker_list$Symbol, ".NS")

# Pull data using quantmod::getSymbols
# Since getsymbols assigns data for each ticker to a 
# separate variable, we'll use a loop to pull data for one
# ticker at a time and append to a data.frame
ticker_df <- data.frame()

pb <- txtProgressBar(min = 1, max = length(tickers), style = 3)

for(nms in tickers){
  df <- getSymbols(Symbols = nms, verbose = F, src = "yahoo", auto.assign = F)
  
  colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
  df <- data.frame(df)
  df$ticker <- nms
  df$date <- rownames(df)
  ticker_df <- rbind(ticker_df, df)
  
  setTxtProgressBar(pb, which(tickers == nms))
}


# We'll need to do some data cleaning 
# Using only closing prices
prices_df <- pivot_wider(data = ticker_df, 
                         id_cols = "date", 
                         names_from = "ticker", 
                         values_from = "close")

# For simplicity, we'll remove all NAs
prices_df <- na.omit(prices_df)

# Check date range for which data is available
range(prices_df$date)
## [1] "2017-11-17" "2021-10-29"

# Check dimensions
dim(prices_df)
## [1] 973  51

# Chart to check if data has been downloaded correctly 
prices_df %>% 
  
  # Convert to long form for easy plotting with ggplot
  gather(key = "ticker", value = "price", -date) %>%
  
  # Attach industry
  left_join(ticker_list %>% 
              mutate(ticker = paste0(Symbol, ".NS")) %>% 
              select(ticker, industry = Industry),
            by = "ticker") %>% 
  mutate(date = as.Date(date)) %>% 
  
  # Showing only metals
  filter(industry == "METALS") %>% 
  
  # Plot with ggplot
  ggplot( aes(x = date, y = price, color = ticker) ) + 
  geom_line(size = 0.8) + 
  theme_minimal() + 
  scale_color_brewer(palette = "RdBu") + 
  labs(title = "Closing Prices", 
       subtitle = "Nifty 50 metal stocks",
       x = "Date", 
       y = "Closing Price") + 
  theme(legend.position = "top", 
        legend.title = element_text(colour = "transparent"), 
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))

# Historical returns

# Calculate daily returns
returns_df <- apply(prices_df[,-1], 2, function(vec){
  ret <- vec/lag(vec) - 1
  return(ret)
})
returns_df <- as.data.frame(returns_df)
returns_df <- returns_df[-1,]  ## Remove first row since that's NA

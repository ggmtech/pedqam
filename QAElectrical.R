# QA Electrical avearge time
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
ss = "https://docs.google.com/spreadsheets/d/1VD_908SBM7j8pp7siBN-egvibSgSctpjpnf8lAzkuCU" #
ss = "https://docs.google.com/spreadsheets/d/1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg"

sheet_names(ss)  # see sheets names

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
tmpdf[ , 1:2]  %>% alluvial::alluvial(   freq = tmpdf$n ) # ??????

alluvial::alluvial( tmp[, c(1:2, 5)],  freq = tmpdf$Freq ) # ??????


allu <- data %>% 
  group_by(Diagnose1, Diagnose2, Diagnose3) %>%   # grouping
  summarise(Freq = n())                           # adding frequencies

# here the plot
alluvial(allu[,1:3], freq=allu$Freq)


tit <- as.data.frame(Titanic, stringsAsFactors = FALSE)
class(tit)
tit
alluvial(  tit[,1:4],   freq   = tit$Freq  )

             
library(naniar)  
 #  replace_with_na(replace = list(x = c(-99, -98))) 
  

################################

# Data of all directorate for 17, 18, 19, 20.4 using "AllCases"
Allrdsocases <-  googlesheets4::read_sheet(ss, sheet = "edited1" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
Allrdsocases %>% janitor::clean_names() %>%    View() #  glimpse()   

Allpre2019 <-  googlesheets4::read_sheet(ss, sheet = "Allpre2019" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "") 

Allpre2019  %>% janitor::clean_names() %>% filter( !is.na(supplier_name) ) %>%     View()
               mutate(   )     # col_types = "ccilDD"



Allrdsocases %>% janitor::clean_names() %>%  #glimpse() 
  dplyr::mutate(  ApplyDate     =  ymd( parse_date_time(date_applied , orders =  c("dmy") ) ) ,
                  #DisposalDate  =  parse_date_time(date_of_approval, orders =  c("dmy") ),
                  #days_taken    =  ( DisposalDate - ApplyDate ) ,
                  #FY_Disposal   =  year( DisposalDate)  ,
                  Qtr_Applied  =  quarter( ApplyDate , with_year = TRUE, fiscal_start = 4   ),
                  Qtr  =         quarter( ApplyDate   ),
                  year =   year(ApplyDate ) #,
                  #ApplyDate     =  parse_date_time(date_of_application , orders =  c("dmy") ) ,
                  #DisposalDate  =  parse_date_time(date_of_approval, orders =  c("dmy") ),
                  #days_taken    =  as.numeric( DisposalDate - ApplyDate ) ,
                  #FY_Disposal   =  year( DisposalDate)  ,
                  #monthdisposed = lubridate::month( DisposalDate, label = TRUE, abbr = TRUE),
                  #Qtr_disposal  =  lubridate::quarter( DisposalDate , with_year = TRUE, fiscal_start = 4   )
                #  txtdate = str_extract( ApplyDate , ".dd.dd.dddd"   )  
                  
                      )                %>%       
           #filter( !is.na(DisposalDate) ) %>%   
           # glimpse() #View()
           # filter( ApplyDate < as.Date("2020-04-01") ) %>% 
            select( directorate, vendor_name, item_for_which_applied, ApplyDate, current_status, reasons_for_delay ) %>% 
           # View()
            googlesheets4::sheet_write(   ss = ss, sheet =  "Curated435" )  








###############
myaluvialdata %>% 
  plot_missing(
  ggtheme = theme_tq(),
  title = str_glue( 'Exploring Missing Data ( N =  {count(myaluvialdata)} )' )
  )
###############


library(tidyverse)  # Work-Horse Package
library(tidyquant)  # Business Ready Plots 
library(scales)     # Scaling Data for Plots

# Step 2 - Visualize Data
mtcars_tbl <- mtcars %>% rownames_to_column( var = "car") %>% as_tibble()
mtcars_tbl

mtcars_tbl %>% 
  ggplot( . ,  aes( x = car,  y = mpg  , fill = am    ) , colour=NA )  +
  geom_col() +
  geom_label( aes(label = mpg) , fill = "white",  hjust = "center" ) +
  
  facet_wrap(~ am) +  # Facet by a categorical feature
  coord_flip() +
  theme_tq() +    # Formatting
  #scale_x_continuous() + 
  scale_x_discrete() +  # if x is character/factor  corelate with fill
  #scale_fill_tq() +
#  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0)) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs( #title = str_glue("Comparison of Educational Attainment ({year_f})" ),
       # subtitle = str_glue("{county} vs. Overall National Statistics"),
       caption  = "Census Data",
       x = "", y = "") 


########################################################################################
### Elect
AllQAE <-  googlesheets4::read_sheet(ss, sheet = "AllQAECases" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
AllQAE %>% janitor::clean_names() %>%     # View #glimpse()      
dplyr::mutate(  ApplyDate     =  parse_date_time(date_of_application , orders =  c("dmy") ) ,
                DisposalDate  =  parse_date_time(date_of_approval, orders =  c("dmy") ),
                days_taken    =  as.numeric( DisposalDate - ApplyDate ) ,
                FY_Disposal   =  year( DisposalDate)  ,
                monthdisposed =  lubridate::month( DisposalDate, label = TRUE, abbr = TRUE),
                Qtr_disposal  =  lubridate::quarter( DisposalDate , with_year = TRUE, fiscal_start = 4   )
)                %>%     
  # filter( status == "Approved"  ) %>% #          !is.na(DisposalDate) ) %>%  
  
  arrange(FY_Disposal,monthdisposed ) %>%  
  group_by( status, FY_Disposal,monthdisposed) %>% 
  summarise(DisposalDate , n = n(), avgtime =  mean(days_taken)     )       -> QAelectSummary
QAelectSummary %>%  #glimpse() #str()  
  View() 

#QAelectSummary %>%   googlesheets4::sheet_write(ss , sheet = "QAElecSummary" )
QAelectSummary %>% filter( status == "Approved" | status == "Closed") %>%   #str() #View()
  ggplot2::ggplot( ) +  aes( y = avgtime,  x = FY_Disposal, fill = FY_Disposal)  + 
  #scale_x_date()     + 
  geom_col( ) +
  facet_grid(~FY_Disposal )


QAelectSummary %>%
  group_by(FY_Disposal) %>%
  plot_time_series(DisposalDate, 
                   n, 
                   .facet_ncol = 2, 
                   .facet_scales = FY_Disposal,
                   .interactive = interactive )


#########################################################




  tidyr::pivot_wider( id_cols     = supplier_name, names_sort = FALSE, names_sep = "_",
                      names_from  = c( VENDOR , FY ) , 
                      values_from = Qfailed ,    values_fill = NULL,
                      values_fn   = sum    # NULL, mean
  )       %>%
  View()

# Now Vendor wise
WarrentyPosted %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED) ) %>%  filter( Qfailed > 0 ) %>%
  tidyr::pivot_wider( id_cols  = VENDOR, 
                      names_sort = FALSE, 
                      names_sep = "_",
                      names_from  =  FY,   # c( Rly , FY ) , 
                      values_from = Qfailed ,    
                      values_fill = NULL,
                      values_fn   = sum    # NULL, mean
  )       %>%
  View()

# Now Rly wise
WarrentyPosted %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED) ) %>%  filter( Qfailed > 0 ) %>%
  tidyr::pivot_wider( id_cols     = Rly, 
                      names_sort = FALSE, 
                      names_sep = "_",
                      names_from  =  FY,   # c( Rly , FY ) , 
                      values_from = Qfailed ,    values_fill = NULL,
                      values_fn   = sum    # NULL, mean
  )       %>%
  View()

WarrentyPosted %>% View()


##### Supplies data cleaning ##################################################
EMpadsupplies    %>%  dplyr::select( Period, Railway, Firm, PO , POdate, DM, DMdate, Qty, Value) %>%    #str() #   View()
  dplyr::mutate(  Qty = as.numeric(Qty), 
                  Value = as.numeric(Value)    ) %>%
  dplyr::filter( !is.na(Qty) )   %>%  
  # View()  #1526 non na qty
  # Now First clean EM pads vendor names Make
  dplyr::mutate( Make = as_factor(Firm)  ) %>%
  dplyr::mutate( Make = fct_collapse(Make, 
                                     VRC = c("VRC Continental (Unit of BESCO Ltd )",  "VRC"),
                                     ARL = c( "Avadh Rail Infra Ltd.",  "ARL", "ARL"),
                                     ARYAN = c( "Aryan Exporters Pvt Ltd" , "ARYAN", "Aryan Exporters" , "Aryan" , "ARYN", "ARIREN"), 
                                     TAYAL = c( "Tayal & Co.",  "TC"),
                                     BASANT = c("Basant Rubber Factory Pvt. Ltd.","BASANT", "BASAANT", 
                                                "BRC", "Basant", "BASAN", "BASNT", "BASAT"  ),
                                     FAS = c( "Frontier Alloy Steels Ltd.", "FAS", "fas", "FASL"),
                                     HFL = c("Howrah Forgings Ltd",   "HFL", "Howrah Forgings, Ltd"),
                                     VRC = c("VRC"),
                                     MGM = c( "MGM Rubber Company", "MGM", "MGM Rubber, Kolkata"),
                                     PRAG = c("Prag Industries (India) Pvt Ltd",   "PRAG", "PARAG"), 
                                     TC = c("TAYAL", "TC", "T.C"),
                                     BONY = c("Bony Polymers Pvt Ltd", "BONY"),
                                     CALCAST = c("Calcast Ferrous Ltd.", "CALCAST"),
                                     MONDEEP = c("Mandeep Industries"),
                                     #Other = c( "VKC", "NV", "1", "2"),
                                     other_level = "Others"                          ) )   %>%      
  # View()
  # Now clean the consignee railway         
  # mutate(Rly = as_factor(Railway)  ) %>%
  #Rly = fct_inorder( ( "CR",  "ER", "ECR", "NR","NCR", "NWR", "NER", "WR", "WCR", "SR", "SCR", "SWR"),      values( "CR", "ER", "ECR", "NR","NCR", "NWR", "NER", "WR", "WCR", "SR", "SCR", "SWR" )  )
  # EMpadsupplies    %>%    # Alternative directly from row data
  # dplyr::select(  Railway, Firm, PO , DMdate, Qty, Value) %>%  # str() #  View()
  dplyr::mutate( Rly = fct_collapse( Railway,  # values( "CR", "ER", "ECR", "NR","NCR", "NWR", "NER", "WR", "WCR", "SR", "SCR", "SWR", )
                                     #   CR = c("Central Railway", "CENTRAL RAILWAY" , "C.RLY", "CR"), 
                                     ER = c("Eastern Railway", "ER"),
                                     ECR = c( "East Central Rly.", "East Central Railway","E.C.RLY",  "ECR"),
                                     ECoR = c("East Coast Railway", "ECOR", "ECoR"),
                                     NR = c("Northern Railway", "NORTHERN RAILWAY", "Northern Rly.",  "NR", "N. RLY"),
                                     NCR = c("North Central Railway","N.C.RLY", "NCR"),
                                     NER = c("North Eastern Railway", "NORTH EAST RAILWAY", "NER"),
                                     NFR = c("North Frontier Railway", "NORTH EAST FRONTIER RAILWAY",
                                             "N.F.RLY",  "N. F. RLY" ),
                                     NWR = c("North Western Railway", "NORTH WESTERN RAILWAY", "N.W.RLY", "NWR" ),
                                     WR = c("Western Railway", "WR"),
                                     WCR = c("West Central Railway", "WEST CENTRAL RAILWAY", "W. C. RLY", "WCR"),
                                     SECR = c("South East Central Railway",   "SECR"),
                                     SR = c("Southern Railway", "SOUTHERN RAILWAY", "SR", "S.RLY"),
                                     SER = c("South Eastern Railway", "SOUTH EASTERN RAILWAY", "S.E.RLY.",
                                             "S.E.RLY", "SER"),
                                     SECR = c("S.E.C.RLY"),
                                     SCR = c("South Central Railway", "SOUTH CENTRAL RAILWAY", "S.C.RLY", "SCR"),
                                     SWR = c("South Western Railway",  "S.W.RLY", "SWR"),
                                     #Jupitor = c("JUPITER WAGONS LIMITED,Kolkata", "M/S JUPITER ALLOYS & STEEL (INDIA) LTD."),
                                     #Oriental = c("Oriental Foundry Pvt. Ltd., Kutch, Gujarat", "Oriental Foundry Pvt. Ltd.", "ORIENTAL FOUNDRY PVT. LTD. Kutch, Gujarat"),
                                     #BrandAlloy = c("BRAND ALLOYS PVT. LTD., Hooghly (W.B.)"),
                                     #Siena    =      c("Siena Engineering Pvt. Ltd., Pithampur", "SIENA Engg. Pvt. Ltd."),
                                     #Braithwaith = c("BRAITHWAITE & CO. LIMITED,Kolkata"),
                                     #Texmeco = c("Texmaco Rail & Engineering Limited, Raipur"),
                                     #Besco = c("BESCO LIMITED (FOUNDRY DIVISION) , Kolkata"),
                                     other_level = "OEM"   )
  )      %>%
  # View()  # all ok so far
  dplyr::mutate( DMdate =  lubridate::dmy(DMdate)  ,
                 POdate =  lubridate::dmy(POdate)  ,
                 # DMdated2 = round_date( DMdatedt, unit = "month" ) 
                 Qtr    = lubridate::quarter(DMdate, with_year = TRUE, fiscal_start = 4),
                 FY = "FY",
                 DMdatefloor = lubridate::floor_date( DMdate, unit = "month" )  , 
  )  %>% 
  # View()
  select( Rly, Make, Period, POdate, DM, DMdate,  Qtr, PO, Qty, Value )  -> EMPadQAM  

EMPadQAM %>% glimpse()

sslife <- "https://docs.google.com/spreadsheets/d/1elSHjPakhrMHJsyGuP74FPm0NirIYPnT-DyQome48Lo" ## EMPads failure google sheet  "EMPadFailureZonalRailways"
EMPadQAM  %>%  googlesheets4::sheet_write( ss = sslife , sheet = "EMpadDM")


########### Get curated supply data now from google sheet     ########################################
EMPadDM  <-  googlesheets4::read_sheet( ss = sslife , sheet = "EMpadDM", col_names = TRUE, trim_ws = TRUE,
                                        col_types = "cccDcDccdd"  , skip = 0,  na = "") # col_types = "cDDcccildd" 
# Rly	Make	Period	POdate	DM	DMdate	Qtr	PO	Qty	Value
EMPadDM  %>% glimpse()


##### Supply details plot
# also plot # geom_line()
EMPadDM %>%   #filter(  year(DMdate ) == "2019" ) %>%   # less supplies in 2019 check data
  ggplot() + 
  geom_point(aes(x = DMdate, y = Qty, size = Qty),  color = "darkblue", alpha = 0.2) +
  #geom_col(aes(x = DMdate, y = Qty), width = 0.15,  color = "#09557f", alpha = 0.6, size = 0.6) +
  #geom_jitter(aes(x = DMdate, y = Qty), width = 0.15,  color = "#09557f", alpha = 0.6, size = 0.6) +
  scale_x_date(limits = as.Date(c("2016-01-01","2021-04-01"))  ) +
  
  labs(x = "Date",   y = "EM Pad Supplies ",  title = "Base Plot") +  theme_minimal()







####### EM pads failure data tidy #########
sslife <- "https://docs.google.com/spreadsheets/d/1elSHjPakhrMHJsyGuP74FPm0NirIYPnT-DyQome48Lo" ## EMPads failure google sheet  "EMPadFailureZonalRailways"

EMPadfailuresWR   <-  googlesheets4::read_sheet(sslife, sheet = "WR"       , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
#EMPadfailuresWR   <-  googlesheets4::read_sheet(sslife, sheet = "WRtidy"       , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 

EMPadfailuresNR   <-  googlesheets4::read_sheet(sslife, sheet = "NRgktest" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresSECR <-  googlesheets4::read_sheet(sslife, sheet = "SECR_Failure" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresCR   <-  googlesheets4::read_sheet(sslife, sheet = "CR" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresECR  <-  googlesheets4::read_sheet(sslife, sheet = "ECR_Failure" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresSR   <-  googlesheets4::read_sheet(sslife, sheet = "SR" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
# #  , , ER_Failure_qty, ",
EMPadfailuresWR %>% View()
EMPadfailuresWR$Rly <- "WR"  
# or EMPadfailuresWR <- googlesheets4::read_sheet(sslife, sheet = "WR", col_names = TRUE) %>% mutate(Rly = "WR")
EMPadfailuresNR$Rly <- "NR"
EMPadfailuresSECR$Rly <- "SECR"
EMPadfailuresCR$Rly <- "CR"
EMPadfailuresECR$Rly <- "ECR"
EMPadfailuresSR$Rly <- "SR"

MPadfailure <- NULL
MPadfailure <- full_join(EMPadfailuresWR, EMPadfailuresNR , by = NULL, suffix = c(".x", ".y") ) # or EMPadfailure <- rbind(EMPadfailuresWR, EMPadfailuresNR )
MPadfailure <- full_join(MPadfailure, EMPadfailuresSECR , by = NULL, suffix = c(".x", ".y") ) # or EMPadfailure <- rbind(EMPadfailuresWR, EMPadfailuresNR )
MPadfailure <- full_join(MPadfailure, EMPadfailuresCR , by = NULL, suffix = c(".x", ".y") ) # or EMPadfailure <- rbind(EMPadfailuresWR, EMPadfailuresNR )
MPadfailure <- full_join(MPadfailure, EMPadfailuresECR , by = NULL, suffix = c(".x", ".y") ) # or EMPadfailure <- rbind(EMPadfailuresWR, EMPadfailuresNR )
MPadfailure <- full_join(MPadfailure, EMPadfailuresSR , by = NULL, suffix = c(".x", ".y") ) # or EMPadfailure <- rbind(EMPadfailuresWR, EMPadfailuresNR )

MPadfailure %>% glimpse()
MPadfailure %>% str()
MPadfailure %>% filter(is.na(Rly))

MPadfailure0 <- MPadfailure %>% dplyr::select( "Date of manufacturing", "Date of replacement",
                                               "Type of failure", "Make of EM pad", "Rly" )

MPadfailure0 %>% count(Rly)
MPadfailure0 %>% View()  # 20148  #592 date problem # make_datetime(year, month, day, hour, minute)


#### TreshEDA1 ########################################
# separate data by  tmp %>% separate_rows( columns  , sep = "[^[:alnum:].]+", convert = FALSE)
MPadfailure0 %>% separate_rows( "Date of manufacturing", sep = ",", convert = FALSE) -> tmp# %>% View() # 20361
tmp %>% glimpse()
tmp %>% mutate(MakeDate =  lubridate::parse_date_time(paste( "01-", `Date of manufacturing` ),   orders = c("dmy","dmY" ,"mdy" ,"ymd"  ),  tz = "UTC" ),
               # FailDate =  lubridate::parse_date_time( paste( `Date of replacement`  ), orders = c("dmy", "mdy" ,"ymd"  ), tz = "UTC")
) %>% View()
tmp  %>% mutate(MakeDate =  paste( "01-",  str_trim(`Date of manufacturing`, side = c("both", "left", "right") )  )  ,
                newdate  = parse_date_time(     MakeDate,  orders = c("dmy", "m-Y-d", "mdy","ymd"  ) ) ) %>% View()

tmp %>% mutate (MakeDate =  lubridate::parse_date_time(paste( "01-", str_squish(`Date of manufacturing` ) ), orders = c("dmy" ),  tz = "UTC" ) ) %>% View()
# unclass(Sys.Date()), lubridate()::now(), lubridate::origin, as.POSIXct()/lt = calendar/local time.
# month(label = TRUE), abbr = true  # today(), now()
# quarter(release_date, with_year = TRUE)
# quarter(release_date, fiscal_start = 4)  
# base:: quarter(with_year = TRUE); quarter(fiscal_start = 4)	Fiscal starts in April; semester()	Get semester
# transact %>% mutate(  quarter_due = quarter(Due) ) %>% count(quarter_due)
# seq.Date(from = as.Date("2010-01-01"), to = as.Date("2019-12-31"), by = "year") #by = "quarter") #, length.out = 10)
seq.Date(from = as.Date("2010-01-01") , by = "-2 year", length.out = 10) # or , along.with = 1:10)
seq.Date(from = as.Date("2010-01-01") , by = "-1 quarter", to = as.Date("2000-01-01")) 

unlist(release_date); 
course_start + weeks(3)

interval(course_start, course_end)  # 2017-04-12 UTC--2017-04-21 UTC
int_overlaps(interval1 , interval2) # to check if two intervals overlap Logical
duration(second = 50) # or duration(50, "seconds") # also dseconds()
duration(week = 56)
period(5, "second") # period is a timespan defined in units such as years, months, and days
floor_date() # ceiling_date()   # also rollback(release_date) to last month-end or rollback(release_date, roll_to_first = TRUE)
# round_date(release_date, unit = "hour")  # second, hour, day,week, month,bimonth,quarter,season, halfyear

parse_date_time(c("2016", "2016-04"), orders = c("Y", "Ym"))

ymd(parse_date_time(c("2016.2", "2016-04"), orders = "Yq") )
# make_datetime_100 <- function(year, month, day, time) {make_datetime(year, month, day, time %/% 100, time %% 100) }
# as_date(), 
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
y2 <- parse_factor(x2, levels = month_levels)
# levels = unique(x1), fct_inorder()
temp <- lubridate::parse_date_time(date, c('mdY IMp', 'mdY HMS'))
temp[is.na(temp)] <- as.Date(as.numeric(date[is.na(temp)]), origin = "1899-12-30") # mix dates



#######################################################  


MPadfailure0 %>% glimpse() #View()
MPadfailure0 %>% 
  separate_rows( "Date of manufacturing", sep = ",", convert = FALSE) %>%
  
  dplyr::mutate( Make = fct_infreq(`Make of EM pad`),
                 MakeDate = as_date( parse_date_time(  paste( '01-', str_trim( (`Date of manufacturing`) , side = "both") ), orders = c("dmy", "mdy" ,"ymd"  ) ) ),
                 FailDate = as_date( parse_date_time( `Date of replacement` , orders = c("mdy", "dmy", "ymd" ,"Ymd"  ),  tz= "UTC", locale = Sys.getlocale("LC_TIME") ) ),
                 lifedays =  as.numeric( (FailDate - MakeDate) ) , # 
                 # as.numeric( difftime(Datefail, MakeDate, days ) ),
                 Reason   = as_factor(`Type of failure`) ,
                 dcencer = if_else( lifedays > 1460, 0, 1  )                   )  %>%     # View()   
  # dplyr::filter( !is.na(MakeDate )  )      %>%
  # dplyr::filter( FailDate < dmy("01/04/2021")  )   %>%
  # dplyr::filter( MakeDate > dmy("31/03/2015")  )   %>%
  #  dplyr::select( Rly, Make, MakeDate,  FailDate, Reason, lifedays, dcencer ) %>%      
  dplyr::mutate( #Make = fct_infreq(`Make of EM pad`), 
    Make = fct_collapse(Make, 
                        VRC = c("VRC"),
                        ARL = c("ARL", "ARL"),
                        ARYAN = c( "ARYAN", "Aryan Exporters" , "Aryan" , "ARYN", "ARIREN"), 
                        TAYAL = c("TC"),
                        BASANT = c("BASANT", "BASAANT", "BRC", "Basant", "BASAN", "BASNT", "BASAT"),
                        FAS = c("FAS", "fas", "FASL"),
                        HFL = c("HFL"),
                        VRC = c("VRC"),
                        MGM = c("MGM", "MGM Rubber, Kolkata"),
                        PRAG = c("PRAG", "PARAG"), 
                        TC = c("TAYAL", "TC", "T.C"),
                        Other = c( "VKC", "NV", "1", "2"),
                        other_level = NULL  
    ) )   %>%  
  dplyr::mutate(Make = fct_lump(Make, 15))     %>% 
  dplyr::mutate(   Reason = fct_collapse( Reason ,
                                          Rubber_Cracked = c("RUBBER CRACK", "RUBBER CRACK", "Rubber cracked",
                                                             "RUBBER CRACKED", "Rubber Cracked", "Rubber Crack"),
                                          Rubber_Crushed = c("CRUSHED", "Rubber crushed", "Crushed", "crushed"),
                                          Rubber_Perished = c("Rubberperished", "Perished", "Rubber Perished",
                                                              "EM PAD PERISHED", "prished", "RUBBER Perished",
                                                              "Rubber perished", "perished" , "PERISHED",
                                                              "01 EM pad perished"),
                                          Bond_Failure = c("1 EM pad bond failure", "01 EM pad bond failure",
                                                           "EM pad bond failure",
                                                           "Bond failure", "Bond Failure", "BOND FAILURE",
                                                           "R/Bonding failure", "bonf fail", "Rubber Bonding Fail",
                                                           "1 EM pad bond failure", "Rubber bond failure",
                                                           "bond failure","Bonding givenup", "Bond falilure",
                                                           "bond fail", "Bonding Failure ", "Bonding Failure",
                                                           "RUBBER BOND FAILURE & RUBBER BKN.", 
                                                           "METAL BOND REMOVED", "Rubber Bound Fail",
                                                           "Rubber Peeled"  , "BOND FAILIURE", "RUBBER BINDING",
                                                           "METAL RUBBER BOND REMOVED", "METAL RUBBER BONDREMOVED",
                                                           "Bond Crack", "Rabber bond failure", "Rubber Bond", "RUbber bond failure",
                                                           "Rubber seal open", "r seal open", "SEAL OPEN", "seal open",
                                                           "Sheared"),
                                          #  Rubber_Sheared = c("Sheared"),  # only SR Reported
                                          
                                          Plate_Cracked = c("PlateCracked", "Plate Cracked", "Metal Plate Broken",
                                                            "PLATE CRACK", "METAL PLATE CRACK", "Plate Broken",
                                                            "TOP PLATE BROKEN", "Top Plate Broken", "TOP PLATE CRACK",
                                                            "TOP PLATE BKN",
                                                            "Rubber Bound Fail Top Plate Broken" ),
                                          Broken =     c("Broken" , "broken", "bkn", "Bkn", "BNK", "EM PAD BROKEN", 
                                                         "BKN",    "EM PAD BKN", "Top plate broken"),
                                          Burnout = c("RUBBER BURNOUT", "Burnout", "Burnout"),
                                          other_level = "Other"          )
  )    %>%
  
  dplyr::select(  everything()  )               ->  EMpadlife

# Cause Vs Rly Reporting  
EMpadlife %>% count(Rly, Reason) %>% arrange(desc(n))  %>% pivot_wider( names_from = Rly, values_from = n)  %>% View()

EMpadlife  %>%     filter( !is.na(MakeDate )  )              %>%
  filter( FailDate < dmy("01/04/2021")  )   %>%
  filter( MakeDate > dmy("31/03/2013")  )   %>%   # count() #16426 #19230 >2013
  count(Rly, Make)                          %>%   #  View()
  pivot_wider( values_from = n , names_from = Rly) %>% 
  View()
# Tresh data # Discarded data as too old 2013 etc
EMpadlife  %>%     filter( is.na(MakeDate ) | FailDate > dmy("01/04/2021") | MakeDate < dmy("31/03/2013")  )   %>%   
  count(Rly, Make) %>% pivot_wider( values_from = n , names_from = c(Rly) ) %>% 
  View()                   
EMpadlife  %>%     filter(  FailDate > dmy("01/04/2021") | MakeDate < dmy("31/03/2015")  )  %>% View()  

EMpadlife  %>%     count(Rly, Make, Reason) %>% pivot_wider( values_from = n , names_from = Rly) %>% View()


# filter(%in%, is.na() ! & |, ==, <=, != , <)
####################################################
# EMpadlife  <- tmp %>% separate("Date of manufacturing", into = c("made1", "made2", "made3"), sep = "," ) %>%             # View()
#                 mutate( Date1 = parse_date_time( paste( '01-', made1  ), orders = c("dmy","mdy" ,"ymd"  ) ), 
#                         Date2 = parse_date_time( paste( '01-', made2  ), orders = c("dmy","mdy" ,"ymd"  ) ), 
#                         Date3 = parse_date_time( paste( '01-', made3  ), orders = c("dmy","mdy" ,"ymd"  ) ), 
#                         Make = `Make of EM pad`,
#                         Reason = `Type of failure`,
#                         Datefail = parse_date_time(`Date of replacement`, orders = c("dmy","mdy" ,"ymd"  ) )
#                         ) %>% 
#                 pivot_longer( names_to = "Made",  cols = c( "made1", "made2", "made3")  )   %>% 
#                 mutate(MakeDate = parse_date_time( paste( '01-', value  ), orders = c("dmy","mdy" ,"ymd"  ) ),
#                        lifedays =  #as.numeric( difftime(Datefail, MakeDate, days ) ),
#                                    as.numeric( (Datefail - MakeDate)/86400  ) ,
#                        dcencer = if_else( lifedays > 1460, 0, 1  ) )  %>%         # str()
#                filter(!is.na(MakeDate )  )      %>%
#                 dplyr::select( Rly, Make, Reason, MakeDate, Datefail,  lifedays, dcencer ) 
####################################################

EMpadlife %>% glimpse()
EMpadlife %>% count(Make)
EMpadlife %>% View()

EMpadlife %>% summarytools::dfSummary() %>% summarytools::view()   #summarytools::view(dfSummary(iris))

DataExplorer::create_report( EMpadlife )

EMpadlife %>% DataExplorer::plot_str()
EMpadlife %>% DataExplorer::plot_missing()
EMpadlife %>% DataExplorer::plot_intro()
EMpadlife %>% DataExplorer::introduce()
EMpadlife %>% DataExplorer::plot_bar()
EMpadlife %>% DataExplorer::plot_boxplot( by = fy) # error?
EMpadlife %>% filter( FailDate < dmy("01/04/2020") | MakeDate < dmy("31/03/2017"))      %>% DataExplorer::plot_density()
EMpadlife %>% filter( FailDate < dmy("01/04/2020") | MakeDate < dmy("31/03/2017"))     %>% DataExplorer::plot_histogram()
EMpadlife %>% DataExplorer::profile_missing()
EMpadlife %>% DataExplorer::group_category()  # error?
?EMpadlife %>% DataExplorer::plot_scatterplot() # error?
?EMpadlife %>% DataExplorer::update_columns() # error?


inspectdf::inspect_cat(EMpadlife)
inspect_imb(EMpadlife)
inspect_na(EMpadlife)
inspect_types(EMpadlife)


###### Write gSheet  sheet_write(data, ss = NULL, sheet = NULL) ######
# already def 
sslife <- "https://docs.google.com/spreadsheets/d/1elSHjPakhrMHJsyGuP74FPm0NirIYPnT-DyQome48Lo/edit#gid=1349265385"
EMpadlife  %>% googlesheets4::sheet_write( ss = sslife, sheet = "EMpadFail")

########################################################################



###### Read curated EM pad failure data from google sheet ##############
EMpadFDread <-  googlesheets4::read_sheet(  ss = sslife, sheet =  "EMpadFail", skip = 0, trim_ws = TRUE,
                                            col_names = TRUE,  col_types = "c" ,  na = "NA")  # col_types = "cDDcccildd" 

EMpadFDread %>%     #glimpse()   # All chr  # MakeDate: POSIXct[1:19746],  lifedays:
  mutate(Rly      = as_factor(Rly),
         Make     = as_factor(Make),
         Reason   = as_factor(Reason),
         MakeDate = lubridate::ymd(MakeDate )  , #as_date(MakeDate),
         FailDate = lubridate::ymd(FailDate) , # as_date(Datefail),
         lifedays = as.numeric(lifedays) ,
         dcencer  = as.logical(dcencer),
         YearFail = as_factor(  lubridate::year(FailDate) ), # , levels()
         YearMade = as_factor(  lubridate::year(MakeDate) ), 
  )  -> EMpadFD

EMpadFD %>% glimpse()


##############################################################
# Warranty Reporting from Railways







##################### Combine supply and failures ########
EMPadDM  # Supply Inspection DM Data
WarrentyPosted  # Warrenty portal data
EMpadFD  # Failure reported by Rly




# id_cols uniquly identifies, values_fn = sum,   c(  )
EMPadQAM2
EMPadQAM2  %>%  tidyr::pivot_wider( id_cols = c(DMdated2),  # That uniquely identify
                                    names_from = Rly, 
                                    values_from = Qty, 
                                    values_fn = sum  )  -> tmp3 #%>%  View()
#mutate(total = NR + WR + SCR    )  %>% View()
#  mutate(total = rowsum(  across( where(is.numeric) ) )  )  %>%
# mutate( recdtotal = sum( across( NCR:Texmeco ) ) )  %>%
rowwise() %>%
  mutate( sum = sum(c_across(NCR:Texmeco)),  sd = sd(c_across(NCR:Texmeco))   ) %>%
  View()     # Good

tmp3  %>% googlesheets4::sheet_write( ss = sslife, sheet = "empadsupplied")
tmp3 %>% glimpse()
tmp3  %>% group_by (as.character(DMdated2)  )  %>% rowwise() %>% mutate(total = sum(c_across(where(is.numeric)))) %>% View()
#x tmp3 %>% rowwise() %>%  mutate(m = mean(  select( -DMdated2, everything()  ) ) )
tmp3 %>% mutate( total = rowSums( across(  where(is.numeric)  )  )   ) %>% View()

# Plot facet grid
EMPadQAM2 %>% filter( Rly %in% c("NR", "WR", "NCR", "WCR")  ) %>%
  ggplot( aes(x=DMdated2, y=Qty)) +   
  geom_col( aes(y=Qty) ) +  geom_abline(colour= "green", wt = 2) + geom_line() +
  geom_point(colour= "red") +   
  scale_x_date( date_labels = "%Y %m ", limit=c(as.Date("2016-01-01"),as.Date("2021-03-31"))) + ylim(0, 3000)  +
  facet_grid(year(DMdated2) ~ Rly) +
  # scale_x_date(date_labels = "%Y %m ") + # xlab("") "%m-%Y"
  theme(axis.text.x=element_text( angle=60, hjust=1)) 


EMPadQAM2 %>% 
  ggplot( aes(x=DMdated2, y=Qty)) +   
  geom_col( aes(y=Qty, fill = Rly) ) +  geom_abline(colour= "green", wt = 2) + geom_line() +
  geom_point(colour= "red") +   
  scale_x_date( date_labels = "%Y %m ", limit=c(as.Date("2016-01-01"),as.Date("2021-03-31"))) + ylim(0, 3000)  +
  facet_grid(~year(DMdated2)) +
  # scale_x_date(date_labels = "%Y %m ") + # xlab("") "%m-%Y"
  theme(axis.text.x=element_text( angle=60, hjust=1)) + theme_bw() 



EMPadQAM2  %>%  tidyr::pivot_wider( id_cols = c( DMdated2),  # That uniquely identify
                                    names_from = Make, 
                                    values_from = Qty, 
                                    values_fn = sum  ) %>% 
  View()

EMpadlife3 %>%  group_by(Rly, Make, MakeDate) %>% summarise(   n = n() ) %>%   #ungroup() %>%
  pivot_wider(id_cols = c( MakeDate ),
              names_from = Rly, 
              values_from = n,
              values_fn = sum    )  %>% 
  View()


# Make wise
EMpadlife3 %>%  group_by(Rly, Make, MakeDate) %>% summarise(   n = n() ) %>%   #ungroup() %>%
  pivot_wider(id_cols = c( MakeDate ),
              names_from = Make, 
              values_from = n,
              values_fn = sum    )  %>% 
  View()



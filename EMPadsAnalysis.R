# curated file # EMPadDM  , # WarrentyPosted  # EMpadFD  # # WarrentyPosted %>% kbl()

# Main analysis file
rm(list = ls())
if (!require(devtools)) install.packages("devtools") # devtools::install_github("boxuancui/DataExplorer")  #, ref = "develop"

packages <- c("tidyverse",   "magrittr",  "here",
              
              "lubridate", "anytime",
              "googlesheets4", "readxl",
              "DataExplorer", "inspectdf", "summarytools", "skimr",
              "broom", "coefplot", "cowplot", "drat",
              
              "survival", "survminer", "ggfortify",
              "ggfortify", "DT", "knitr",
              "gridExtra","plotly", "timetk", "ggforce", "ggraph", 
              #"ggplot2", "dplyr", "tidyr", "magrittr", "stringr","forcats","reshape2",
              
              "ggpubr","scales", "GGally", "ggrepel", "ggridges",
              
              "ggthemes", #"themr",
              "viridis", "viridisLite",  
              "magick",
              "here", "devtools",
              #"cart", "psy", "car", "psych", "ordinal", "lmtest",  "dslabs",  
              "graphlayouts",  "interplot", "margins", 
              #"quantreg", "rlang", "scales", "socviz", "survey", "srvyr", 
              #"gapminder",
              # "pwr",  "doBy", 
              #"imputeMissings", "RcmdrMisc", "questionr", "vcd", "multcomp", 
              #"KappaGUI", "rcompanion", "FactoMineR", "factoextra", "corrplot", 
              #"ltm", "goeveg", "corrplot", "FSA", "MASS",  "nlme", 
              #"assist", "ggstatsplot",  "styler", "remedy", 
              #"snakecaser", "addinslist", "esquisse", "here",  
              #"funModeling", "pander", "cluster"
              #"sf", "maps", "mapproj", "mapdata", "MASS", 
              "naniar", "prismatic"
         )
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
lapply(packages, library, character.only = TRUE) #%>% invisible()


#devtools::install_github("alastairrushworth/inspectdf")# library(inspectdf)
##################################################################################
#raw_img <- magick::image_read("http:link") ; magick::image_ggplot(raw_img)
here::here()  # getwd() and list.files()
ls()  # global env variables / values in memory ;
##################################################################################


# Get Data raw ###################################################################
# googlesheets4::sheet_add() sheet_append() sheet_copy() sheet_delete() sheet_write() sheet_properties() sheet_rename()
# googlesheets4::read_sheet( ss, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, or "cidDl"
#         skip = 0, na = "", trim_ws = TRUE , n_max = Inf  ,  guess_max = min(1000, n_max),  .name_repair = "unique" )

########## EM pads supplies data tidy ######################### 
myfile = "https://raw.githubusercontent.com/ggmtech/pedqam/master/EMPadsAnalysis.R"
myfile = read_file(myfile) 
myfile %>% cat()  # also print("$a !! my name is $b and my number is $c")
knitr::read_chunk('external.R')
# code chunks:  ``` {r,  code = readLines("external.R")  } 
# knitr::opts_chunk$set(echo = TRUE, comment=NA )


ss = "https://docs.google.com/spreadsheets/d/1lshFtdQf87ONyGdMHbwDRvRtWPM8B_bRPtuuq67P6xE" ## EM Pads supplies data Google Sheet
sheet_names(ss)  # see sheets names
EMpadsupplies <-  googlesheets4::read_sheet(ss, sheet = "EMpadsupplied" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies %>% tibble::glimpse() #View()

WarrentyPosted <-  googlesheets4::read_sheet(ss, sheet = "WarrentyPosted" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"

WarrentyPosted %>% filter( FAILED > 0 ) %>% group_by(Rly) %>%
                  tibble::glimpse() 
                  View()
                  count(Rly)
                  dplyr::summarise( n= n()  ) 
                    #  mean(), median() sd(), IQR(), mad() min(), max(), quantile() first(), last(), nth(), n(), n_distinct() any(), all()
# Warrenty posted data FY wise
WarrentyPosted %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED) ) %>%  filter( Qfailed > 0 ) %>%
                   tidyr::pivot_wider( id_cols     = Rly, names_sort = FALSE, names_sep = "_",
                                       names_from  = c( VENDOR , FY ) , 
                                       values_from = Qfailed ,    values_fill = NULL,
                                       values_fn   = sum    # NULL, mean
                                       )       %>%
                    View()
# Now Vendor wise
WarrentyPosted %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED) ) %>%  filter( Qfailed > 0 ) %>%
                   tidyr::pivot_wider( id_cols     = VENDOR, names_sort = FALSE, names_sep = "_",
                                       names_from  =  FY,   
                                                     # c( Rly , FY ) , 
                                       values_from = Qfailed ,    values_fill = NULL,
                                       values_fn   = sum    # NULL, mean
                                       )       %>%
                    View()

# Now Rly wise
WarrentyPosted %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED) ) %>%  filter( Qfailed > 0 ) %>%
                   tidyr::pivot_wider( id_cols     = Rly, names_sort = FALSE, names_sep = "_",
                                       names_from  =  FY,   
                                                      # c( Rly , FY ) , 
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

sscrlife = "https://docs.google.com/spreadsheets/d/11A5lwOWa-G9SyCCgpkkxC4FFoUzD7Hc3yoYlPEohd0c"
EMPadfailuresCR   <-  googlesheets4::read_sheet(sscrlife, sheet = "CR" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresCR %>% View()
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


##############################################################################################################  
##############################################################################################################   
EMPadfailuresCR

MPadfailure0 <- EMPadfailuresCR
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

# inspect with single data youngGrades or with both with oldGrades
inspect_mem(youngGrades, oldGrades, show_plot = TRUE)
inspect_types(youngGrades, oldGrades, show_plot = TRUE) # inspect_types(allGrades, show_plot = TRUE)
inspect_na(youngGrades, oldGrades, show_plot = TRUE)
inspect_num(youngGrades, oldGrades, show_plot = TRUE)
inspect_imb(youngGrades, oldGrades, show_plot = TRUE)  # value distribution for categorical values.
inspect_cat(youngGrades, oldGrades, show_plot = TRUE)  # ** visualize the full distribution of our categorical values.
inspect_cor(allGrades, show_plot = TRUE)
inspect_cor(youngGrades, oldGrades, show_plot = TRUE) # pearson coreletion -1 to +1
# to clone #install.packages("usethis") #library(usethis) #use_course("https://github.com/lgellis/MiscTutorial/archive/master.zip")
# show_plot = TRUE is deprecated and will be removed in a future version. The show_plot()q

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
# Warrenty Reporting from Railways







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


       
##########################  failures
EMpadlife3 %>% View()
EMpadlife3 %>%  filter( MakeDate < as_date("2021-04-01"), MakeDate > as_date("2017-04-01") ,
                       FailDate < as_date("2021-04-01"), FailDate > as_date("2017-04-01")  ) %>% # View()
              group_by(MakeDate, Rly) %>% summarise( Rly, Qty = n() ) %>%                         #   View()
               ggplot(aes(x=MakeDate, Qty))  + geom_point() + #geom_bar(stat = ?? ) +
             scale_y_continuous(breaks = scales::pretty_breaks(n = 12),  
                      # minor_breaks = scales::pretty_breaks(n = 10), 
                       limit=c(0, 110) , position = "right" ) +  
               geom_col()  + facet_grid(Rly~1) # +  geom_smooth()

####  good !! 
EMpadlife3 %>% 
    filter(  FailDate < as_date("2020-11-01"), FailDate > as_date("2017-04-01") ) %>% #View()
    group_by(FailDate) %>% summarise( Rly,   Qty = n() ) %>%    #   View()
    
    ggplot() +  aes(x=FailDate, y= Qty) +  
    #geom_point( ) +
    geom_col(aes(x=FailDate, y= Qty))  +
    
    scale_x_date(breaks= scales::pretty_breaks(n = 20),  
                 date_minor_breaks = "7 days",
                 date_labels = "%Y %m ", 
                 limit=c(as.Date("2017-04-01"), as.Date("2021-03-31")) , 
                 position = "bottom" 
                 ) + 
    
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12),  
                       minor_breaks = scales::pretty_breaks(n = 10), 
                       limit=c(0, 110) , position = "right" ) +  
                     geom_col() + theme_bw()
    
    # ylim(0, 240)  +  # covered in scale
 #   scale_x_continuous(n.breaks = 10) +
    geom_smooth(aes(x=FailDate, y= Qty, fill= Rly))   +   theme_bw()

## Excellent graph above makedate and fail date

EMpadlife3 %>% glimpse()
EMpadlife3 %>% group_by(MakeDate) %>% summarise(  Qty = n() ) %>% View()
     pivot_wider( id_cols = c(MakeDate),  
                  names_from = Rly, 
                  values_from = Qty, 
                  values_fn = sum  ) %>% View()
EMpadlife3 %>% glimpse()

df3 %>% left_join(EMpadlife3 , by = c( "DMdated2"  = "MakeDatef"  ) ) %>% View()   # by = c("a" = "b") 




###################
EMpadlife3 %>% View()
#Kaplen Meier survival 
lifefit0 <- survfit( Surv( lifedays, dcencer) ~ 1 , data = EMpadlife3 )
lifefit1 <- survfit( Surv( lifedays, dcencer) ~ Make , data = EMpadlife3 )

lifefit2 <- survfit( Surv( lifedays, dcencer) ~ Rly , data = EMpadlife3 )
lifefit3 <- survfit( Surv( lifedays, dcencer) ~ SCY  , data = EMpadlife3 )
lifefit4 <- survfit( Surv( lifedays, dcencer) ~ FCY  , data = EMpadlife3 )


library(RColorBrewer)
nb.cols <- 18  # Define the number of colors you want
mycolors <- colorRampPalette(brewer.pal(8, 'Dark2'))(nb.cols)
#xdisplay.brewer.pal(n = 8, name = 'Dark2')

# empadlifefit %>%  
dev.off()
lifetable0 <-  lifefit0 %>%        
    ggsurvplot(
        #empadlifefit,            # survfit object with calculated statistics.
        # fun = "event",         # function
        # fun = "cumhaz",        # fun = function(y) y*100 ,
        # linetype = "strata",   # change line type by groups
        size = 1,                # change line size
        ##data = EMpadlife,        # data used to fit survival curves.
        date = lifefit, 
        conf.int = TRUE,         # show confidence intervals for  point estimates of survival curves.
        pval = TRUE,            # show p-value of log-rank test.
        
        #pval = "The hot p-value is: 0.031", or #pval = 0.03
        #pval.coord = c(0, 0.03),
        #pval.size = 4,
        #pval.method = TRUE,
        #pval.method.size = 3,
        #log.rank.weights = "1",
        conf.int.style = "ribbon",
        #conf.int.alpha = 0.2,
        
        # ?conf.int.fill = "blue",
        #palette = c("#E7B800", "#2E9FDF"), # custom color palette, match varibles
        #palette = "Dark2",
        palette = mycolors, # c("red", "green", "blue", "yellow","pink", "brown", "black", "blue","red",  "blue", "blue", "blue","red",  "blue"),
        xlim = c(0, 1500),         # present narrower X axis, but not affect survival estimates.
        xlab = "Failure Time in Days",   # customize X axis label.
        break.time.by = 90,     # break X axis in time intervals by 500.
        #ggtheme = theme_bw() , #theme_light(), # customize plot and risk table with a theme.
        
        #censor.shape="|", 
        #censor.size = 4,
        # ncensor.plot = TRUE,      # plot the number of censored subjects at time t
        # ncensor.plot.height = 0.25,
        # conf.int.style = "step",  # customize style of confidence intervals
        
        font.main = c(12, "bold", "darkblue"),
        font.x = c(8, "bold.italic", "red"),
        font.y = c(8, "bold.italic", "darkred"),
        
        #font.tickslab = c(12, "plain", "darkgreen"),
        # legend = "bottom", 
        #legend = c(0.2, 0.2),
        #legend.title = "Sex",
        #legend.labs = c("Male", "Female"),
        
        surv.median.line = "hv" , # add the median survival pointer. c("none", "hv", "h", "v")
        # legend = "bottom" , 
        # legend.labs =      c("Male", "Female"),    # change legend labels.
        
        risk.table = TRUE,       # show risk table.
        # tables.theme = theme_cleantable(),
        #risk.table.col = "strata" , 
        #risk.table.y.text.col = T,# colour risk table text annotations.
        risk.table.x.text.font = 5,# colour risk table text annotations.
        #risk.table.height = 0.5 #, # the height of the risk table
        #risk.table.y.text = FALSE # show bars instead of names in text annotations in legend of risk table.
    )  

lifetable0
lifetable1
lifetable3
lifetable4 

cowplot::plot_grid(print(lifetable3), print(lifetable4), labels = "AUTO", ncol = 1, nrow = NULL)



# x gridExtra::grid.arrange(plot1, plot2, ncol=2)
# pdf("foo.pdf") ; grid.arrange(plot1, plot2); dev.off()
#  + facet_wrap(~myGroup)
# cowplot::plot_grid(iris1, iris2, labels = "AUTO")
# p <- plot_grid(iris1, iris2, labels = "AUTO")  or save_plot("plot.pdf", p, ncol = 2)
# labs(title = "The Actual Long, Normal Title of Titliness", tag = "A")

# difftime("2020-5-16", "2020-1-15", units = "weeks") #units: Days, weeks, months, etc.

ggsave("plot.pdf", p)



# Font for Risk Table
ggsurv$table <- ggpar(      ggsurv$table,
                            font.title    = c(10, "bold.italic", "green"),
                            font.subtitle = c(10, "bold", "pink"),
                            font.caption  = c(8, "plain", "darkgreen"),
                            font.x        = c(8, "bold.italic", "orange"),
                            font.y        = c(8, "bold.italic", "darkgreen"),
                            font.xtickslab = c(8, "bold", "red")
)



EMpadlife3 %>%  ggsurvplot_facet( lifefit,   facet.by = "Make" )


EMpadlife3

summarise_by_time(.data = EMpadlife3,   
                  .date_var = MakeDate,
                  .by = "6 months"  , #  like "5 seconds", "week", or "3 months" second,  minute,hour, day, week, month, bimonth
                  # quarter,  season,   halfyear year, lubridate::ceiling_date().
                  .type =  "floor",     #c("floor", "ceiling", "round"),
                  # ...,  # min(x), n(), or sum(is.na(y))., 
                  # Sum: sum(), mean(), median(), sd(), var(), min(), max(), Count: dplyr::n(), dplyr::n_distinct()
                  # Position: dplyr::first(), dplyr::last(), dplyr::nth(),  Correlation: cor(), cov()
                  totals  = n(  ) # qty, first(qty1) # , r = railways
           )  -> tmp  #%>% str()
tmp
library(timetk)
tmp %>% timetk::plot_time_series( MakeDate, totals  ) #,  .color_var = month(dmdate) #, .interactive = FALSE, .color_lab = year(dmdate) )

tmp
library(timetk)
interactive <- FALSE  # Setup for the plotly charts (# FALSE returns ggplots)
tmp %>% filter(!is.na(totals) ) %>% 
         timetk::plot_time_series(MakeDate, totals   # .interactive = interactive,    .plotly_slider = TRUE
                                 )




kbl(text_tbl, booktabs = T) %>%
    kable_styling(full_width = F) %>%
    column_spec(1, bold = T, color = "red") %>%
    column_spec(2, width = "30em")



##################################################################################
# unique(), rbind(,)
# glimpse()  #View()
# empadsup  %>%  DataExplorer::create_report()

# scale_color_viridis(discrete = TRUE, option = "D") + scale_fill_viridis(discrete = TRUE) +
#   geom_smooth(aes(color = Species, fill = Species), method = "lm") + 
# barplot(1:10, col = viridis(10))
# RColorBrewer::display.brewer.all()
RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL, colorblindFriendly = FALSE)

# wesanderson ; names(wes_palettes)
# discrete_scale("fill", "manual", palette_Dark2)  # scale pkg



EMpadsupplies %>% janitor::clean_names() %>% 
    dplyr::mutate( dmdate = dmy(d_mdate), qty1 =  as.numeric(qty) , syear = year(dmdate), smonth = month(dmdate),  
                   # railway = fct_lump(railway, n = 18)
    ) %>%
    dplyr::select(dmdate,  qty1, firm, railway   )  %>%     # , everything()
    na.omit(dmdate) -> empadsup
empadsup %>%        View()
fct_count(empadsup$railway)
empadsup %>%   group_by(railway )  %>% summarise( railway, qty1 ) %>%   View()

empadsup %>% # mutate( railway = as.factor(railway)) %>% 
    mutate(railway = forcats::fct_collapse( railway, 
                                            CR = c( "C.RLY"	, "Central Railway",	"CENTRAL RAILWAY"	, 
                                                    "CR"	, "CR/W.Call"	, "CR/Warranty Call"	,	"CR/WARRANTY CALL") ,
                                            ECR = c("E.C.RLY"	,	"East Central Railway"	,	"East Central Rly."	,
                                                    "ECR/Warranty Call"),
                                            ER = c("Eastern Railway", "ER"),
                                            ECoR = c( "ECoR", 	"ECoR/Warranty Call", "East Coast Railway" ),
                                            NER = c("NORTH EAST RAILWAY",	"North Eastern Railway", "North Frontier Railway"),
                                            NWR = c("North Western Railway",	"NORTH WESTERN RAILWAY", "NWR/Warranty Call", "NWR/WarrantyCall", "N.W.RLY"),
                                            
                                            NR = c("Northern Railway", "NORTHERN RAILWAY", "Northern Rly.",	"NR", "NR/W.Call",
                                                   "NR/Warranty", "NR/Warranty Call", "NR/WARRANTY CALL", "N. RLY", "N.RLY", "N.RLY"),
                                            NCR = c("NCR","NCR/Warranty Call", "NCR/WARRANTY CALL", "North Central Railway", "N.C.RLY" ),
                                            NFR = c("N. F. RLY", "N.F.RLY", "NFR/Warranty Call", "NORTH EAST FRONTIER RAILWAY"),
                                            SCR = c( "SCR", "S.C.RLY", "SCR /Warranty Call", "SCR/Warranty Call", "SOUTH CENTRAL RAILWAY" , 
                                                     "South Central Railway", "South Central Railway", "South Central Railway"),
                                            SR = c("SR", "Southern Railway", "SOUTHERN RAILWAY", "S.RLY"),
                                            SECR = c("S.E.C.RLY" , "SECR/Warranty Call", "South East Central Railway"),
                                            SER = c("SER", "S.E.RLY", "S.E.RLY.", "SER/Warranty", "SER/Warranty Call", "SOUTH EASTERN RAILWAY",
                                                    "South Eastern Railway", "SOUTH EASTERN RAILWAY" ),
                                            SWR = c( "S.W.RLY", "South Western Railway"),
                                            WCR = c("WCR", "W. C. RLY", "WCR/ Warranty Call", "WCR/Warranty Call", "WCR/WARRANTY CALL",
                                                    "West Central Railway", "WEST CENTRAL RAILWAY"),
                                            WR = c("WR", "Western Railway", "WR/W.Call", "WR/Warranty", "WR/Warranty Call", "WR/WARRANTY CALL"),
                                            KR = c("Konkan Railway"),
                                            
                                            other_level = "Pvt misc"        
    ) ) %>%
    mutate(month = format(dmdate, "%m"), 
           year = format(dmdate, "%Y"), 
           Qtr = as_factor(lubridate::quarter(dmdate , fiscal_start = 4,with_year = TRUE ) )
    ) %>%  # or can use date2 = format(date, "%Y-%m"))
    mutate( Qtr = fct_reorder( Qtr , as.numeric(Qtr))  ) %>%
    group_by(railway, year, Qtr , month) %>%
    summarise(total = sum(qty1) ) -> empaddm  #%>% View() #
empaddm %>% View()
empaddm %>%   #group_by( railway, year,  Qtr ) %>%  #View()
    ungroup()  %>%       select( -year) %>% #str()
    # mutate( Qtr = fct_reorder( Qtr )  ) %>% View()
    #expand_grid(Qtr) %>%
    # summarise( across(starts_with(total), sum)) %>% #->  t
    pivot_wider(names_from = fct_reorder(Qtr) ,  values_from = total) %>%
    # pivot_wider(names_from = c(`year(dmdate)` ,  `month(dmdate)`), values_from = qty1) %>%
    
    # also summarise( across(qty1, sum) ) %>%
    # summarise( across(starts_with('r'), sum)) %>%
    # arrange(desc(qty1))  %>%
    # rowwise() %>%  mutate( sum = sum(c_across(a:z)),   sd = sd(c_across(a:z))    )  %>%
    View()

t

TKDlifetable  %>% mutate( Loco     = as.factor(Loco) ,  
                          item     = as.factor(item), 
                          Loc      = as.factor(Loc), 
                          Make     = as.factor(Make), 
                          Reason   = as.factor(Reason),
                          datefit  = as.Date(datefit),
                          faildate = as.Date(faildate),
                          life     = as.numeric(life)      )       %>%    
                 mutate( item = fct_collapse(item,
                                "Piston" = c( "P", "Piston" ),
                                "Liner"  = c("L", "Liner" ),
                                "LP"   = c("LP", "PA")     )      )   %>%  
    mutate(uloc2 = paste(Loco, Loc) , uloc3 = paste(Loco, Loc, item)) %>% 
    group_by(uloc2)                           %>% 
    arrange(uloc2, datefit)                   %>%   
    filter(!is.na(Loco)   )                   ->  TKDlifetable2     #    %>%    View()

TKDlifetable2

##################################################################################

summarise_by_time(.data = empadsup,   
                  .date_var = dmdate,
                  #.by = "3 months"  , #  like "5 seconds", "week", or "3 months" second,  minute,hour, day, week, month, bimonth
                  # quarter,  season,   halfyear year, lubridate::ceiling_date().
                  .type =  "floor",     #c("floor", "ceiling", "round"),
                  # ...,  # min(x), n(), or sum(is.na(y))., 
                  # Sum: sum(), mean(), median(), sd(), var(), min(), max(), Count: dplyr::n(), dplyr::n_distinct()
                  # Position: dplyr::first(), dplyr::last(), dplyr::nth(),  Correlation: cor(), cov()
                  totals  = qty1 #first(qty1) # , r = railways
)

# plot 
taylor_30_min
taylor_30_min %>% plot_time_series(date, value, .color_var = week(date),  .interactive = FALSE, .color_lab = "Week")
empadsup
empadsup %>% plot_time_series(dmdate, qty1, 
                              .color_var = month(dmdate) #, .interactive = FALSE, .color_lab = year(dmdate) 
                             )


walmart_sales_weekly
walmart_sales_weekly %>%
    group_by(Store, Dept) %>%
    plot_anomaly_diagnostics(Date, Weekly_Sales, 
                             .facet_ncol = 3, .interactive = FALSE)

empadsup %>%  #group_by(firm, railway) %>%
    plot_anomaly_diagnostics(dmdate, qty1,  .facet_ncol = 3,  .interactive = FALSE   )

empadsup %>% plot_seasonal_diagnostics(dmdate, qty1, .interactive = FALSE)

empadsup %>%  #group_by(railway) %>%
    #summarise( across
    # also summarise( across(qty1, sum) ) %>%
    pivot_wider(names_from = firm, 
                values_from =  qty1  )  %>%
    group_by( year(dmdate)   )%>%
    summarise( across( -dmdate , everything() ) ,  sum ) %>%
    #arrange(desc(qty1))  %>%
    View()

# A tibble: 24 x 4


##################################################################################

##################################################################################
#scale_color_viridis(): Change the color of points, lines and texts
#scale_fill_viridis(): Change the fill color of areas (box plot, bar plot, etc)
# viridis(n), magma(n), inferno(n) and plasma(n):


failurs <- EMPadfailures

str(failurs)
failurs  %>% View()
failurs %>% janitor::clean_names() %>% 
    mutate( #datefail = dmy(date_of_replacement) ,  
        datefail = parse_date_time(date_of_replacement, orders = c("dmy", "mdy", "Y-m-d") ),
        #makedate =  dmy(paste( '01-', `date_of_manufacturing`  )  ),
        # makedate =  parse_date_time( paste( '01-', `date_of_manufacturing`  ), orders = c("dmy") ) ,
        makedate =  parse_date_time( paste( '01-', date_of_manufacturing  ), orders = c("dmy","mdy" ,"ymd"  ) ),
        lifedays = datefail - makedate ,
        life = as.numeric(lifedays),
        make  =  as_factor(make_of_em_pad) 
    )  ->  empaddays   #%>% View() #
empaddays$status = 1
empaddays %>% mutate( status = if_else( life > 2200, 0, 1) )  %>% 
    select( make, life, status, makedate, datefail )   -> padslife         # %>%     View()

padslife  %>%     View()

empaddays    %>% filter(is.na(makedate))  %>% View()
empaddays    %>% filter(!is.na(makedate)) %>% View()

str(empaddays)

##################################################################################


# parse_date_time(date_time, orders = c("dmY HM", "mdY HM", "Ymd HMS"))
# parse_date_time(c("2016.2", "2016-04"), orders = "Yq")
# parse_date_time(c("2016", "2016-04"), orders = c("Y", "Ym")
library(skimr)
skimr::skim(empaddays)

library(DataExplorer) # data %>%  DataExplorer::create_report()
DataExplorer::create_report(failurs)
DataExplorer::create_report(empaddays)
plot_intro(empaddays)
plot_intro # see ggplot code


library(inspectdf) #devtools::install_github("alastairrushworth/inspectdf") or CRAN
inspect_types(empaddays)
inspect_mem(empaddays)
#inspect_types(youngGrades, oldGrades, show_plot = TRUE)
starwars %>% group_by(gender) %>% inspect_mem()
inspect_cat(starwars) %>% show_plot(plot_type = 1 , high_cardinality > 100)

# show_plot(x,text_labels = TRUE,alpha = 0.05,high_cardinality = 0,plot_layout = NULL,
#  col_palette = 0, plot_type = 1, label_thresh = 0.1, label_angle = NULL,label_color = NULL,
#  label_size = NULL )
# inspect_types() summary of column types
# inspect_mem() summary of memory usage of columns
# inspect_na() columnwise prevalence of missing values
# inspect_cor() correlation coefficients of numeric columns
# inspect_imb() feature imbalance of categorical columns
# inspect_num() summaries of numeric columns
# inspect_cat() summaries of categorical columns

inspect_na(empaddays) %>% show_plot() 
show_plot(inspect_types(empaddays))
inspect_num(empaddays) %>% show_plot()
inspect_cat(empaddays) %>% show_plot()
inspect_cor(empaddays) %>% show_plot()
inspect_imb(empaddays) %>% show_plot()

# .getPageLayout	Calculate page layout index
# dummify	Dummify discrete features to binary columns
# drop_columns	Drop selected variables
# group_category	Group categories for discrete features
# create_report	Create report
# .ignoreCat	Truncate category
# configure_report	Configure report template
# plotDataExplorer.grid	Plot objects with gridExtra
# .lapply	Parallelization
# plot_intro	Plot introduction
# plot_boxplot	Create boxplot for continuous features
# plotDataExplorer.multiple	Plot multiple objects
# plot_scatterplot	Create scatterplot for all features
# plot_prcomp	Visualize principal component analysis
# plot_qq	Plot QQ plot
# plot_str	Visualize data structure
# plot_correlation	Create correlation heatmap for discrete features
# plotDataExplorer	Default DataExplorer plotting function
# plot_density	Plot density estimates
# introduce	Describe basic information
# profile_missing	Profile missing values
# plot_histogram	Plot histogram
# split_columns	Split data into discrete and continuous parts
# .getAllMissing	Get all missing columns
# plot_missing	Plot missing value profile
# update_columns	Update variable types or values
# set_missing	Set all missing values to indicated value
# plotDataExplorer.single	Plot single object
# plot_bar	Plot bar chart

##################################################################################

# plot Air Temperature Data across 2009-2011 using daily data
empadsup %>%  ggplot( )  +    
    #  geom_point(na.rm=TRUE) + 
    # geom_point(  aes(x = dmdate, y = qty1 , size = qty1 , fill = year(dmdate)),      na.rm=TRUE, color="blue",  pch=1) +
    geom_bar( aes(x = dmdate, y = qty1 , size = qty1 , fill = year(dmdate)),    stat="identity", na.rm = TRUE) +
    #ok stat_smooth(aes(x = dmdate, y = qty1 , size = qty1 , fill = year(dmdate)),  colour="green") +
    #  scale_size_manual(1, 5) + 
    scale_size_continuous(1,500) + 
    (scale_x_date(breaks=scales::date_breaks("6 months"),   labels=scales::date_format("%b %y") )) +
    ggtitle("EM pads supplies") +
    xlab("Date") + ylab("Qty supplied") +
    facet_wrap(~firm) + theme_bw()


empadsup %>%  ggplot( aes( qty1),  stat_bin(qty1)  )  + 
    geom_histogram(na.rm=TRUE, bins=5, binwidth = 2.5) +    # binwidth = 0.01
    facet_wrap(~firm)
# stat_count() or stat_bin(mapping = NULL, data = NULL, geom = "bar", position = "stack",
# binwidth = NULL,bins = NULL,center = NULL,boundary = NULL,breaks = NULL,closed = c("right", "left"),
# pad = FALSE, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE )


library(timetk)
interactive <- FALSE  # Setup for the plotly charts (# FALSE returns ggplots)
empadsup %>% timetk::plot_time_series(dmdate, qty1,       .interactive = interactive,    .plotly_slider = TRUE)

EMpadsupplies %>% mutate(   )  %>% View()

##################################################################################


empaddays$status = 1
empaddays  %>% View()
empaddays$lifedays
hist( as.numeric(empaddays$lifedays) , xlim = c(0, 2000), nclass = 1000 )#, xlab="Length of Survival Time", main="Histogram of Survial Time of EM Pads")
# good plot highlighting ROH/POH
str(empaddays)


empaddays %>% select(lifedays, status, make) %>%   
    # na.omit() %>% 
    mutate( empaddays1 = as.character(empaddays),
            empaddays2 = as.numeric(empaddays1)   ) -> empaddays

empaddays  %>% na.omit() %>%  View()


empadlifefit <- survfit( Surv( empaddays$lifedays, status) ~ 1                , data = empaddays )
empadlifefit <- survfit( Surv( empaddays$life, status) ~ empaddays$make   , data = empaddays )


padslife  %>% str() #View()
padslife  %>%   mutate( make = fct_collapse(make, 
                                            VRC = c("VRC", "VR"),
                                            ARL = c("ARL", "ARL"),
                                            ARYAN = c( "ARYAN"), 
                                            TAYAL = c("TC"),
                                            BASANT = c("BASANT", "BASAANT", "BRC"),
                                            FAS = c("FAS"),
                                            HFL = c("HFL"),
                                            VRC = c("VRC"),
                                            MGM = c("MGM"),
                                            PRAG = c("PRAG"),
                                            TC = c("TAYAL"),
                                            Other = c( "VKC", "NV"),
                                            other_level = NULL
                                            
) )   %>%  
    mutate( make = fct_lump(make, 13) ) %>% 
    group_by(make) -> padslife # %>%   count() %>% View()
padslife

#group_by(make) %>% filter(  count() < 100 )
lifefit <- survfit( Surv( padslife$life, status) ~ padslife$make , data = padslife )
lifefit

print(empadlifefit)
summary(empadlifefit)
plot(empadlifefit)

##################################################################################


library(RColorBrewer)
nb.cols <- 18 # Define the number of colors you want
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
# Create a ggplot with 18 colors # Use scale_fill_manual
ggplot(df) + 
    geom_col(aes(name, Sepal.Length, fill = factor(Sepal.Length))) +
    scale_fill_manual(values = mycolors)
# empadlifefit %>%  
dev.off()
lifefit %>%        
    ggsurvplot(
        #empadlifefit,            # survfit object with calculated statistics.
        # fun = "event",         # function
        fun = "cumhaz",        # fun = function(y) y*100 ,
        # linetype = "strata",   # change line type by groups
        size = 3,                # change line size
        ##data = empaddays,        # data used to fit survival curves.
        date = lifefit, 
        conf.int = TRUE,         # show confidence intervals for  point estimates of survival curves.
        pval = TRUE,            # show p-value of log-rank test.
        #pval = 0.03
        #pval = "The hot p-value is: 0.031",
        #pval.coord = c(0, 0.03),
        #pval.size = 4,
        #pval.method = TRUE,
        #pval.method.size = 3,
        #log.rank.weights = "1",
        #conf.int.style = "ribbon",
        #conf.int.alpha = 0.2,
        
        # ?conf.int.fill = "blue",
        #palette = c("#E7B800", "#2E9FDF"), # custom color palette, match varibles
        #palette = "Dark2",
        palette = c("red", "red", "blue", "blue","red", "red", "blue", "blue","red",  "blue", "blue", "blue","red",  "blue"),
        xlim = c(0, 2200),         # present narrower X axis, but not affect survival estimates.
        xlab = "Failure Time in Days",   # customize X axis label.
        break.time.by = 90,     # break X axis in time intervals by 500.
        #ggtheme = theme_bw() , #theme_light(), # customize plot and risk table with a theme.
        
        #censor.shape="|", 
        #censor.size = 4,
        # ncensor.plot = TRUE,      # plot the number of censored subjects at time t
        # ncensor.plot.height = 0.25,
        # conf.int.style = "step",  # customize style of confidence intervals
        
        #font.main = c(16, "bold", "darkblue"),
        #font.x = c(14, "bold.italic", "red"),
        #font.y = c(14, "bold.italic", "darkred"),
        #font.tickslab = c(12, "plain", "darkgreen"),
        # legend = "bottom", 
        #legend = c(0.2, 0.2),
        #legend.title = "Sex",
        #legend.labs = c("Male", "Female"),
        
        surv.median.line = "hv"  # add the median survival pointer. c("none", "hv", "h", "v")
        # legend = "bottom" , 
        # legend.labs =      c("Male", "Female"),    # change legend labels.
        
        #risk.table = TRUE,       # show risk table.
        # tables.theme = theme_cleantable(),
        #risk.table.col = "strata" , 
        #risk.table.y.text.col = T,# colour risk table text annotations.
        #risk.table.height = 0.25 #, # the height of the risk table
        #risk.table.y.text = FALSE # show bars instead of names in text annotations in legend of risk table.
    )    -> ggsurv

ggsurv






imgholi = "https://static.langimg.com/imagenext/nbtblogs/photo/astro/wp-content/uploads/2020/03/holi1-2.jpg"
ggsurv  %++% cowplot::draw_image("https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png", x = 5, y = 2.5, width = 2, height = 1.5    )

ggsurv  %++% cowplot::draw_image(imgholi,  x = 5, y = 2.5, width = 2, height = 1.5  )

library(survminer)
model <- coxph( Surv(time, status) ~ sex + rx + adhere,  data = colon ) # Fit Cox prop hazards model
ggforest(model)
# lifefit <- survfit( Surv( padslife$life, status) ~ padslife$make , data = padslife )
modelpad  <- coxph( Surv( life, status) ~ make , data = padslife)
ggforest(modelpad)


# Facet by one grouping variables: rx  
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = "Component")   #,    palette = "jco", pval = TRUE
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = c("Component", "adhere") ) #,   palette = "jco", pval = TRUE)   # Facet by two grouping

##################################################################################
##################################################################################







library(ggfortify)
library(survival)
empaddays  %>% View()
fit <- survfit( Surv(lifedays , empaddays$status) ~ empaddays$make, data = empaddays)  # fit <- survfit(Surv(time, status) ~ sex, data = lung)
autoplot(fit)

##################################################################################
















##################################################################################
##################################################################################
##################################################################################


#####  lead lag function ########

## lag function   eg    dt %>% group_by(location) %>% mutate(lvar = lag(var))
## seems group_by(name) %>% mutate(next.score = lead(score, order_by=name), before.score = lag(score, order_by=name) )
# from tidyverse fn lead or lag
# lead(x, n = 1L, default = NA, order_by = NULL, ...)
# x	 is vector of values,  n is number of positions to lead or lag by
# default is value to  use for non-existent rows. Defaults to NA.
# order_by   override the default ordering to use another vector



TKDpafit <- survfit( Surv(life) ~ Make , data = TKDlifeLfinal )

ggsurvplot(
    TKDpafit,                     # survfit object with calculated statistics.
    # fun = "event",              # fun = "cumhaz", # fun = function(y) y*100 ,
    # linetype = "strata",        # change line type by groups
    size = 2,                     # change line size
    data = TKDlifeLfinal,         # data used to fit survival curves.
    #linetype = "strata",          # Change line type by groups
    conf.int = TRUE,              # confidence intervals for  point estimates.
    
    #pval = TRUE,                 # show p-value of log-rank test.
    #pval = 0.03
    #pval = "The hot p-value is: 0.031"
    #pval.coord = c(0, 0.03)
    #pval.size = 4,
    #pval.method = TRUE,
    #pval.method.size = 3,
    #log.rank.weights = "1",
    #conf.int.style = "ribbon",
    #conf.int.alpha = 0.2,
    # ?conf.int.fill = "blue",
    # palette = c("#E7B800", "#2E9FDF"), # custom color palette, match varibles
    palette = "Dark2",
    xlim = c(0,2000),         # present narrower X axis, but not affect survival estimates.
    xlab = "Failure Time in Days",   # customize X axis label.
    break.time.by = 90,     # break X axis in time intervals by 500.
    #ggtheme = theme_light(), # customize plot and risk table with a theme.
    ggtheme = theme_bw() ,
    #censor.shape="|", 
    #censor.size = 4,
    # ncensor.plot = TRUE,      # plot the number of censored subjects at time t
    # ncensor.plot.height = 0.25,
    # conf.int.style = "step",  # customize style of confidence intervals
    
    #font.main = c(16, "bold", "darkblue"),
    #font.x = c(14, "bold.italic", "red"),
    #font.y = c(14, "bold.italic", "darkred"),
    #font.tickslab = c(12, "plain", "darkgreen"),
    # legend = "bottom", 
    #legend = c(0.2, 0.2),
    #legend.title = "Sex",
    #legend.labs = c("Male", "Female"),
    
    surv.median.line = "v",  # add the median survival pointer. c("none", "hv", "h", "v")
    # legend = "bottom" , 
    # legend.labs =      c("Male", "Female"),    # change legend labels.
    
    risk.table = TRUE,       # show risk table.
    # tables.theme = theme_cleantable(),
    risk.table.col = "strata" , 
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.height = 0.25 #, # the height of the risk table
    #risk.table.y.text = FALSE # show bars instead of names in text annotations in legend of risk table.
)    -> ggsurv

ggsurv  #+ draw_image("https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png", x = 5, y = 2.5, width = 2, height = 1.5    )

#########################


######### Changing Labels %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(     title    = "The Power assembly failures over time since commissioning in TKD shed",                     
                                       subtitle = "Based on TKD shed actual data as on 30/03/19 and life given in days" ,  
                                       caption  = "TRANSLOCO liners failes withing a year ! "          )  #+
#draw_image("https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png", x = 5, y = 2.5, width = 2, height = 1.5    )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = " TKD Liners in service after so many days and thier Survival table",          
                                       subtitle = " Population in Locos and time to fail in days", 
                                       caption  = " TKD data as on 30/03/2019"        )

##### Labels for ncensor plot  #######
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(   title    = "censured data", 
                                                     subtitle = "over the time.",
                                                     caption  = "TKD data "  )

ggsurv


############################################################################
# Customise 
ggsurv$plot <- ggsurv$plot + labs(     title    = "The Power assembly failurs over time in TKD shed",                     
                                       subtitle = "Based on limited data at TKD on 30/03/19",  
                                       caption  = "CC lines are the worst"          )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = "Failure of liners in Locos - Survival table",          
                                       subtitle = "and data is evedent that CC make lines failing too soon.", 
                                       caption  = "TKD data"        )

# Labels for ncensor plot 
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(   title    = "censured data", 
                                                     subtitle = "over the time.",
                                                     caption  = "TKD data "  )

# Changing the font size, style and color   to all the components of ggsurv:
ggsurv <- ggpar(  
    ggsurv,
    font.title    = c(16, "bold", "darkblue"),         
    font.subtitle = c(15, "bold.italic", "purple"), 
    font.caption  = c(14, "plain", "orange"),        
    font.x        = c(14, "bold.italic", "red"),          
    font.y        = c(14, "bold.italic", "darkred"),      
    font.xtickslab = c(12, "plain", "darkgreen"),
    legend = "top"
   )

ggsurv

# specific for risk table and ncensor plots #########

# Font for Risk Table
ggsurv$table <- ggpar(      ggsurv$table,
                            font.title    = c(10, "bold.italic", "green"),
                            font.subtitle = c(10, "bold", "pink"),
                            font.caption  = c(8, "plain", "darkgreen"),
                            font.x        = c(8, "bold.italic", "orange"),
                            font.y        = c(8, "bold.italic", "darkgreen"),
                            font.xtickslab = c(8, "bold", "red")
                       )


# Font for ncensor plot
ggsurv$ncensor.plot <- ggpar(
    ggsurv$ncensor.plot,
    font.title    = c(13, "bold.italic", "green"),
    font.subtitle = c(15, "bold", "pink"),
    font.caption  = c(11, "plain", "darkgreen"),
    font.x        = c(8, "bold.italic", "orange"),
    font.y        = c(11, "bold.italic", "darkgreen"),
    font.xtickslab = c(9, "bold", "red")
                     )

ggsurv

print(ggsurv)

######## graphs ############
ggplot(TKDpa1, aes(x = Loco1 , y = lifemonths)) +   geom_col() +   rotate_x_text(angle = 45)

ggplot(TKDpa1,   aes( x = reorder(Loco1, Loco1) , y = lifemonths)  ) +
    geom_segment(  aes(x = Loco1, xend = Loco1, 
                       y = 0,     yend = lifemonths , col = Make) , 
                       alpha = 0.8, size =5   ) + 
    geom_point( aes(color = Make), size = 3 ) +
    geom_text( aes(label = lifemonths), nudge_y = 3) + 
    #scale_color_viridis_d() +
    theme_pubclean() +
    rotate_x_text(45) +
    coord_flip() +
    theme_bw()  #theme_cowplot()

########### Graphs above  ################
# levels(factor(df$col))    OR    unique(df$col)
# distinct(uloc) %>% View()

# inner_join(df1, df2),   left_join(df1, df2), full_join(df1, df2)
# semi_join(df1, df2) #keep only observations in df1 that match in df2.
# anti_join(df1, df2) #drops all observations in df1 that match in df2.    
# bind_rows, 
# union()  merge from both data frames but keep only the distinct (unique) rows  
# ‘intersect’, return only the duplicated rows among the data frames
# ‘setdiff’, which would return only the rows that are not duplicated.

# spread() distributes a pair of key:value columns into a field of cells. The unique values of the key column become the column names of the field of cells.
spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE,  sep = NULL)
# fill missing values, convert type, drop facotr ordeing, sep keymane-keyvalue
gather(table4, "myKeyCol", "myValueCol", colomestocollaps )  # 3:5 or c("cols", "cols3",  "etc") or -coltoExclude
# na.rm = FALSE, convert = FALSE, factor_key = FALSE ) # factor_key preserve ording
# separate() and unite()
# separate(table3, rate, into = c("cases", "population"), sep = "/") #sep = "/", 1, "", 
# remove = FALSE, convert = FALSE, extra = drop/merge/NA
# unite_(mtcars, "vs_am", c("vs","am"))     # joinsep = "_", "/", "", etc

# if datefail == na , fill with next record date
#apply(Ages, 2, median)  # apply( subsetdata, c(1 for row, 2 for colomns), Funtion you want to apply )

# subetting df[1:4, 3:4]  or c("Name", "Surname") 



###############################
# Customise

#### Changing Labels %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(     title    = "The Power assembly failurs over time in TKD shed",                     
                                       subtitle = "Based on limited data at TKD on 30/03/19",  
                                       caption  = "CC lines are the worst"          )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = "Failure of liners in Locos - Survival table",          
                                       subtitle = "and data is evedent that CC make lines failing too soon.", 
                                       caption  = "TKD data"        )

# Labels for ncensor plot 
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(   title    = "censured data", 
                                                     subtitle = "over the time.",
                                                     caption  = "TKD data "  )

ggsurv

# Changing the font size, style and color
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Applying the same font style to all the components of ggsurv:
# survival curves, risk table and censor part

ggsurv <- ggpar(  
    ggsurv,
    font.title     = c(16, "bold", "darkblue"),         
    font.subtitle  = c(15, "bold.italic", "purple"), 
    font.caption   = c(14, "plain", "orange"),        
    font.x         = c(14, "bold.italic", "red"),          
    font.y         = c(14, "bold.italic", "darkred"),      
    font.xtickslab = c(12, "plain", "darkgreen"),
    legend = "top"
)

ggsurv


# Using specific fonts for risk table and ncensor plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Font for Risk Table
ggsurv$table <- ggpar(      ggsurv$table,
                            font.title    = c(13, "bold.italic", "green"),
                            font.subtitle = c(15, "bold", "pink"),
                            font.caption  = c(11, "plain", "darkgreen"),
                            font.x        = c(8, "bold.italic", "orange"),
                            font.y        = c(11, "bold.italic", "darkgreen"),
                            font.xtickslab = c(9, "bold", "red")
)


# Font for ncensor plot
ggsurv$ncensor.plot <- ggpar(
    ggsurv$ncensor.plot,
    font.title    = c(13, "bold.italic", "green"),
    font.subtitle = c(15, "bold", "pink"),
    font.caption  = c(11, "plain", "darkgreen"),
    font.x        = c(8, "bold.italic", "orange"),
    font.y        = c(11, "bold.italic", "darkgreen"),
    font.xtickslab = c(9, "bold", "red")
)

ggsurv

print(ggsurv)


############################

# Facet by one grouping variables: rx
TKDpa1
TKDpafit <- survfit(Surv(lifemonths) ~ Make , data = TKDpa1 )
lifemonths
TKDpafit <- survfit( Surv(lifemonths) ~ Make, data = TKDpa1  )
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = "Component")   #,    palette = "jco", pval = TRUE
ggsurvplot_facet(TKDpafit, TKDpa1, facet.by = c("Component", "adhere") ) #,   palette = "jco", pval = TRUE)   # Facet by two grouping
#####################################################
#####################################################

# ggsurvplot(), ggsurvplot_list() , ggsurvplot_facet(), ggsurvplot_group_by(), ggsurvplot_add_all(), ggsurvplot_combine()

# Fit a Cox proportional hazards model
surv_object
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps,   data = ovarian)
ggforest(fit.coxph, data = ovarian)

ggsurvplot(fit2 )   # most basic plot of fitted data
ggsurvplot_facet(fit2 , data = colon ,  facet.by = "adhere") + theme_bw()  


fit <- list(PFS = fit1, OS = fit2)
ggsurvplot_combine(fit, lung)  ####### Combine curves 
############################################################
ggsurvplot_facet(fit2, data = colon, facet.by = "adhere",  palette = "jco", pval = TRUE) + theme_bw()
ggsurvplot_facet(fit2 , data = colon ,  facet.by = "adhere") + theme_bw()  


############################################################

ggsurvplot()          #: Draws survival curves with the 'number at risk' table, the cumulative number of events table and the cumulative number of censored subjects table.
arrange_ggsurvplots() #: Arranges multiple ggsurvplots on the same page.
ggsurvevents()        #: Plots the distribution of event's times.
surv_summary()        #: Summary of a survival curve. Compared to the default summary() function, surv_summary() creates a data frame containing a nice summary from survfit results.
surv_cutpoint()       #: Determines the optimal cutpoint for one or multiple continuous variables at once. Provides a value of a cutpoint that correspond to the most significant relation with survival.
pairwise_survdiff()   #: Multiple comparisons of survival curves. Calculate pairwise comparisons between group levels with corrections for multiple testing.
# Diagnostics of Cox Model
ggcoxzph()           #: Graphical test of proportional hazards. Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve using ggplot2. Wrapper around plot.cox.zph().
ggcoxdiagnostics()   #: Displays diagnostics graphs presenting goodness of Cox Proportional Hazards Model fit.
ggcoxfunctional()    #: Displays graphs of continuous explanatory variable against martingale residuals of null cox proportional hazards model. It helps to properly choose the functional form of continuous variable in cox model.

ggforest()           #: Draws forest plot for CoxPH model  # Summary of Cox Model
ggadjustedcurves()   #: Plots adjusted survival curves for coxph model.
ggcompetingrisks()   # Competing Risks, Plots cumulative incidence curves for competing risks.

# more at http://www.sthda.com/english/rpkgs/survminer/, and check out the documentation and usage 

###################################################################

# Facet by one grouping variables: rx
fit <- survfit( Surv(time, status) ~ sex, data = colon )

ggsurvplot(fit, data = colon,
           fun = "cumhaz", conf.int = TRUE,
           risk.table = TRUE, risk.table.col="strata",  ggtheme = theme_bw())
ggsurvplot_facet(fit, colon, facet.by = "rx",    palette = "jco", pval = TRUE)
ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),   palette = "jco", pval = TRUE)  # Facet by two grouping variables: rx and adhere

# Another fit
fit2 <- survfit( Surv(time, status) ~ sex + rx,  data = colon )
ggsurvplot_facet(fit2, colon, facet.by = "adhere",  palette = "jco", pval = TRUE)



ggsurv$plot + facet_grid(rx ~ adhere)  # Faceting survival curves


# # Generate risk table for each facet plot item

ggsurv$table + facet_grid(rx ~ adhere, scales = "free")+  theme(legend.position = "none")

tbl_facet <- ggsurv$table + facet_grid(.~ adhere, scales = "free") # Generate risk table each facet columns
tbl_facet + theme(legend.position = "none")

# # Arrange faceted survival curves and risk tables
g2 <- ggplotGrob(curv_facet)
g3 <- ggplotGrob(tbl_facet)
min_ncol <- min(ncol(g2), ncol(g3))
g <- gridExtra::rbind.gtable(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
g$widths <- grid::unit.pmax(g2$widths, g3$widths)
grid::grid.newpage()
grid::grid.draw(g)
# 
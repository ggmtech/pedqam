# EMPads_CleanDataGS.R
# Main Data cleaing files for saving to curated file # EMPadDM  , # WarrentyPosted  # EMpadFD  # 
rm(list = ls())
if (!require(devtools)) install.packages("devtools") # devtools::install_github("boxuancui/DataExplorer")  #, ref = "develop"

packages <- c("tidyverse",   "magrittr",  "here",
              "lubridate", "anytime",
              "googlesheets4", "readxl",
              "DataExplorer", "inspectdf", "summarytools", "skimr",
              "broom", 
              "knitr",
              
              "coefplot", "cowplot", "drat",
              
              "survival", "survminer", "ggfortify",
              "ggfortify", "DT", 
              "gridExtra","plotly", "timetk", "ggforce", "ggraph", 
              #"ggplot2", "dplyr", "tidyr", "magrittr", "stringr","forcats","reshape2",
              
              "scales", "ggpubr","GGally", "ggrepel", "ggridges",
              
              "ggthemes", #"themr",
              "viridis", "viridisLite",  
              "magick",
              "here", "devtools",
              "naniar", "prismatic"
            )
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
lapply(packages, library, character.only = TRUE) #%>% invisible()

# Get Data raw ###################################################################
# googlesheets4::sheet_add() sheet_append() sheet_copy() sheet_delete() sheet_write() sheet_properties() sheet_rename()
# googlesheets4::read_sheet(ss, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, or "cidDl"
#                          skip = 0, na = "", trim_ws = TRUE, n_max = Inf, guess_max = min(1000, n_max),.name_repair = "unique" )

########## EM pads supplies data tidy ######################### 
# myfile = read_file(myuri) 


# EM Pads supplies data Google Sheet
ss = "https://docs.google.com/spreadsheets/d/1lshFtdQf87ONyGdMHbwDRvRtWPM8B_bRPtuuq67P6xE" #
sheet_names(ss)  # see sheets names

EMpadsupplies <-  googlesheets4::read_sheet(ss, sheet = "EMpadsupplied" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies %>% tibble::glimpse() #View()


WarrentyPosted <-  googlesheets4::read_sheet(ss, sheet = "WarrentyPosted" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
WarrentyPosted %>% filter( FAILED > 0 ) %>% group_by(Rly) %>%  tibble::glimpse() 

WarrentyPosted %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED) ) %>%  filter( Qfailed > 0 ) %>%
                   tidyr::pivot_wider( id_cols     = Rly, names_sort = FALSE, names_sep = "_",
                                      names_from  = c( VENDOR , FY ) , 
                                      values_from = Qfailed ,    values_fill = NULL,
                                      values_fn   = sum    # NULL, mean
                                     )       %>%
                    #View()
                    googlesheets4::sheet_write(ss, sheet =  "postedyearmakerly")

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
                     # View()
                   googlesheets4::sheet_write(ss , sheet = "vendoryearwarrenty")
  
# Now Rly wise
WarrentyPosted %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED) ) %>%  filter( Qfailed > 0 ) %>%
                   tidyr::pivot_wider( id_cols     = Rly, 
                                       names_sort = FALSE, 
                                       names_sep = "_",
                                       names_from  =  FY,   # c( Rly , FY ) , 
                                       values_from = Qfailed ,    values_fill = NULL,
                                       values_fn   = sum    # NULL, mean
                                     )       %>%
                    # View()
                    googlesheets4::sheet_write(ss, sheet = "Rlywarrenty")     


WarrentyPosted %>% # View()
                    dplyr::mutate( FAILED = as.numeric(FAILED)  ) %>%
                    dplyr::filter(!is.na(FAILED), FAILED > 0)  %>%    
                    #View() 
                    googlesheets4::sheet_write(ss, sheet = "warrentypost")

##### Supplies data cleaning ##################################################
EMpadsupplies    %>%  dplyr::select( Period, Railway, Firm, PO , POdate, DM, DMdate, Qty, Value) %>%    #str() #   View()
                      dplyr::mutate(  Qty = as.numeric(Qty), 
                                    Value = as.numeric(Value)    ) %>%
                      dplyr::filter( !is.na(Qty))   %>%     
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

EMPadQAM %>% View() #glimpse()

#sslife <- "https://docs.google.com/spreadsheets/d/1elSHjPakhrMHJsyGuP74FPm0NirIYPnT-DyQome48Lo" ## EMPads failure google sheet  "EMPadFailureZonalRailways"
ss
EMPadQAM  %>%  googlesheets4::sheet_write( ss = ss , sheet = "EMpadDM")


########### Get curated supply data now from google sheet     ########################################
EMPadDM  <-  googlesheets4::read_sheet( ss = sslife , sheet = "EMpadDM", col_names = TRUE, trim_ws = TRUE,
                                        col_types = "cccDcDccdd"  , skip = 0,  na = "") # col_types = "cDDcccildd" 
# Rly	Make	Period	POdate	DM	DMdate	Qtr	PO	Qty	Value
EMPadDM  %>%  # View() 
              # glimpse()
               dplyr::mutate(  Qty =  as.numeric(Qty) ) %>%  
               tidyr::pivot_wider( id_cols     = c(Rly, Qtr), 
                      names_sort = FALSE, 
                      names_sep = "_",
                      names_from  =  Make,   # c( Rly , FY ) , 
                      values_from = Qty ,    values_fill = NULL,
                      values_fn   = sum    # NULL, mean
               )       %>%
            # View()
            googlesheets4::sheet_write(ss, sheet = "Qtrwisesupply")     







##### Supply details plot
# also plot # geom_line()
EMPadDM %>%   #filter(  year(DMdate ) == "2019" ) %>%   # less supplies in 2019 check data
  ggplot() + 
  geom_point(aes(x = DMdate, y = Qty, size = Qty),  color = "darkblue", alpha = 0.2) +
  #geom_col(aes(x = DMdate, y = Qty), width = 0.15,  color = "#09557f", alpha = 0.6, size = 0.6) +
  #geom_jitter(aes(x = DMdate, y = Qty), width = 0.15,  color = "#09557f", alpha = 0.6, size = 0.6) +
  scale_x_date(limits = as.Date(c("2016-01-01", "2021-04-01"))  ) +
  
  labs(x = "Date",   y = "EM Pad Supplies ",  title = "EM Pad supplies", subtitle = "as per DM" , caption = "(c) RDSO data Year 2016-21" ) +  theme_minimal()







####### EM pads failure data tidy #########
sslife <- "https://docs.google.com/spreadsheets/d/1elSHjPakhrMHJsyGuP74FPm0NirIYPnT-DyQome48Lo" ## EMPads failure google sheet  "EMPadFailureZonalRailways"

EMPadfailuresWR   <-  googlesheets4::read_sheet(sslife, sheet = "WR"       , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
#EMPadfailuresWR   <-  googlesheets4::read_sheet(sslife, sheet = "WRtidy"       , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresWR %>% glimpse()

EMPadfailuresNR   <-  googlesheets4::read_sheet(sslife, sheet = "NRgktest" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresSECR <-  googlesheets4::read_sheet(sslife, sheet = "SECR_Failure" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 
EMPadfailuresCR   <-  googlesheets4::read_sheet(sslife, sheet = "CR" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 

ssCR  = "https://docs.google.com/spreadsheets/d/11A5lwOWa-G9SyCCgpkkxC4FFoUzD7Hc3yoYlPEohd0c"
EMPadfailuresCR2   <-  googlesheets4::read_sheet(ssCR, sheet = "B" , col_names = TRUE,  col_types = "c"  , skip = 2, trim_ws = TRUE, na = "")  # col_types = "cDDcccildd" 


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

tmp %>%  mutate(MakeDate =  lubridate::parse_date_time(paste( "01-", str_squish(`Date of manufacturing` ) ), orders = c("dmy" ),  tz = "UTC" ) ) %>% View()
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

# Causes reported by Rly  
EMpadlife  %>%     count( Rly, Reason ) %>% 
                   arrange( desc(n) )   %>% 
                   pivot_wider( names_from = Rly, values_from = n)  %>% 
                   # View()
                   googlesheets4::sheet_write(ss, sheet = "CausesbyRly")

# Makewisefailure  reported
EMpadlife  %>%     filter( !is.na(MakeDate )  )              %>%
                   filter( FailDate < dmy("01/04/2021")  )   %>%
                   filter( MakeDate > dmy("31/03/2013")  )   %>%   # count() #16426 #19230 >2013
                   count(Rly, Make)                          %>%   #  View()
                   pivot_wider( values_from = n , names_from = Rly) %>% 
                   # View()
                   googlesheets4::sheet_write(ss, sheet = "Makewisefailure")


# RlyMakeReason 
EMpadlife  %>%     count(Rly, Make, Reason) %>% 
                   pivot_wider( values_from = n , names_from = Rly) %>% 
                   # View()
                   googlesheets4::sheet_write(ss, sheet = "RlyMakeReason")  


# Tresh data Discarded data as too old 2013 etc
EMpadlife  %>%     filter( is.na(MakeDate )  | FailDate > dmy("01/04/2021")  | MakeDate < dmy("31/03/2013")  )   %>%   
                   count(Rly, Make) %>% 
                   pivot_wider( values_from = n , names_from = c(Rly) ) %>% 
                   View()     

EMpadlife  %>%     filter(  FailDate > dmy("01/04/2021") | MakeDate < dmy("31/03/2015")  )  %>% 
                   View()  




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
inspectdf::inspect_imb(EMpadlife)
inspectdf::inspect_na(EMpadlife)
inspectdf::inspect_types(EMpadlife)
#devtools::install_github("alastairrushworth/inspectdf")# library(inspectdf)

inspectdf::inspect_cat(EMpadlife)

inspectdf::inspect_imb(EMpadlife)
inspectdf::inspect_na(EMpadlife)
inspectdf::inspect_types(EMpadlife)


inspectdf::inspect_mem(EMpadlife) %>% show_plot()  # good 
inspectdf::inspect_na(EMpadlife) %>% show_plot()
inspectdf::inspect_types(EMpadlife) %>% show_plot()
inspectdf::inspect_num(EMpadlife) %>% show_plot()
inspectdf::inspect_imb(EMpadlife) %>% show_plot()
inspectdf::inspect_cat(EMpadlife) %>% show_plot()
inspectdf::inspect_cor(EMpadlife) %>% show_plot()
inspectdf::inspect_cat(EMpadlife) %>% show_plot()

inspectdf::inspect_cat(EMpadlife) %>% show_plot()

inspectdf::inspect_cat
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
# show_plot = TRUE is deprecated and will be removed in a future version. The show_plot()



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



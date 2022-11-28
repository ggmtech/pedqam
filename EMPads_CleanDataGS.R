# EMPads_CleanDataGS.R    Write everything in ss1 = EMpadsmaster
# Main Data cleaing files for saving to curated file # EMPadDM  , # WarrentyPosted  # EMpadFD  # 
# rm(list = ls())
# if (!require(devtools)) install.packages("devtools") # devtools::install_github("boxuancui/DataExplorer")  #, ref = "develop"


# mutate(date = make_date(year, month, day))
# numbers_1 %>% mutate(number = parse_number(number))
# flights %>% select(ends_with("hour"))
# flights %>% select(contains("hour"))
mutate(origin = case_when(
  (origin == "EWR") & dep_delay >  20 ~ "Newark International Airport - DELAYED",
  (origin == "EWR") & dep_delay <= 20 ~ "Newark International Airport - ON TIME DEPARTURE", ) ) %>%

mutate(origin = str_replace_all(origin, c("^EWR$" = "Newark International",    
                                          "^JFK$" = "John F. Kennedy International"
             ))    )    %>%
# 9. Filter groups without making a new column
# flights_top_carriers <- flights %>%  group_by(carrier) %>% filter(n() >= 10000) %>% ungroup() 
  
#  str_detect("^Am")) 
# All comb
  crossing(
    customer_channel = c("Bus", "Car"),
    customer_status = c("New", "Repeat"),
    spend_range = c("$0-$10", "$10-$20", "$20-$50", "$50+"))






  
###############################################################################################
###############################################################################################
packages <- c("tidyverse",   "magrittr",  "here", "lubridate", "anytime",  "googlesheets4", "readxl", "janitor",
              "DT", "DataExplorer", "inspectdf", "summarytools", "skimr", "here", "devtools", "naniar",
              "scales",    "coefplot", "cowplot", "drat",
             "knitr", "gridExtra","plotly", "timetk", "ggforce", "ggraph", 
             "survival" , "survminer", "ggpubr","GGally", "ggrepel", "ggridges",
              "viridis", "viridisLite",  "ggthemes", #"themr",  #  "ggfortify",  "ggfortify", 
             "magick", "ggfx" ,  "prismatic"
            )
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
lapply(packages, library, character.only = TRUE) #%>% invisible()

###############################################################################################
###############################################################################################

# Get Data raw ###################################################################
# googlesheets4::sheet_add() sheet_append() sheet_copy() sheet_delete() sheet_write() sheet_properties() sheet_rename()
# googlesheets4::read_sheet(ss, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, or "cidDl"
#                          skip = 0, na = "", trim_ws = TRUE, n_max = Inf, guess_max = min(1000, n_max),.name_repair = "unique" )
# myfile = read_file(myuri) 
########## EM pads supplies data tidy ######################### 
ss01  = "https://docs.google.com/spreadsheets/d/1lshFtdQf87ONyGdMHbwDRvRtWPM8B_bRPtuuq67P6xE" # "EM pads supply 2017to2021"
sheet_names(ss01)  # see sheets names

# x EMpadsupplies01 <-  googlesheets4::read_sheet(ss01, sheet = "EMpadsupplied" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
# EMpadsupplies01 %>% distinct() %>% group_by(Period ) %>%  summarise( supp = sum(as.numeric(Qty) ), n = n() )


# ensure that google date are dmy
#supply 17-18
EMpadsupplies1718 <-  googlesheets4::read_sheet(ss01, sheet = "supply 17-18" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies1718
EMpadsupplies1819 <-  googlesheets4::read_sheet(ss01, sheet = "supply 18-19" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies1819
EMpadsupplies1920 <-  googlesheets4::read_sheet(ss01, sheet = "supply 19-20" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies1920
EMpadsupplies2021 <-  googlesheets4::read_sheet(ss01, sheet = "supply 20-21" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies2021 %>% View()

#EMpadsupplies2021j <-  googlesheets4::read_sheet(ss01, sheet = "supply 20-21jan" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
#EMpadsupplies2021m <-  googlesheets4::read_sheet(ss01, sheet = "supply 20-21mar" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"

EMpadsupplies2021 

#EMpadsupplies2122 <-  googlesheets4::read_sheet(ss01, sheet = "supply 21-22" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"

#
#EMpadsupplies2122 %>%   summarise( supp = sum( as.numeric(.$'Qty. Delivered') , na.rm = TRUE ),  n = n() )

# Combile all year data of supplies to yearwiseDM
yearwiseDM <-  NULL
yearwiseDM <-  EMpadsupplies1718
yearwiseDM <-  yearwiseDM        %>% full_join( EMpadsupplies1819  )
yearwiseDM <-  yearwiseDM        %>% full_join( EMpadsupplies1920  )
yearwiseDM <-  yearwiseDM        %>% full_join( EMpadsupplies2021  )


#yearwiseDM <-  yearwiseDM        %>% full_join( EMpadsupplies2122  )
yearwiseDM %>% distinct() %>%  count(Railway) %>% View()  # n = 158
yearwiseDM %>% glimpse()
#yearwiseDM %>%   group_by(year(`Date of Issue of DM`) ) %>% count( `Date of Issue of DM`) #%>% View()  # n = 158

##### Supplies data cleaning ##################################################
yearwiseDM       %>%  
                      janitor::clean_names()  %>%   janitor::remove_empty()  %>% 
                      #glimpse()
                     # dplyr::select(  railway, firm_name,  dm_no, date_of_issue_of_dm, qty_delivered, value_of_goods_inspected ) %>% 
                      dplyr::mutate(  Qty =  as.numeric(qty_delivered) ,
                                      ValueDM = as.numeric(value_of_goods_inspected) ,
                                      Make = as_factor(firm_name),
                                      Rly = as_factor(railway),
                                      #DMdate =  lubridate::dmy(date_of_issue_of_dm)  ,
                                      DMdate =  lubridate::parse_date_time(date_of_issue_of_dm, orders = c("dmy", "dmY"))  ,
                                      #POdate =  lubridate::dmy(POdate)  ,
                                      # DMdated2 = round_date( DMdatedt, unit = "month" ) 
                                      QtrDM    = lubridate::quarter(DMdate, with_year = TRUE, fiscal_start = 4),
                                      YearDM   = lubridate::year(DMdate),
                                      DMdatefloor = lubridate::floor_date( DMdate, unit = "month" )  , 
                                      
                                      )  %>% 
                      dplyr::filter( !is.na(Qty))   %>%          #View() # glimpse()  #1801
                 # Now First clean  vendor names as Make
                 # Make ARL ARYAN BASANT BONY CALCAST FAS HFL MGM MONDEEP Others PRAG TAYAL VRC
                     dplyr::mutate( Make = fct_collapse(Make, 
                                     VRC = c("VRC Continental (Unit of BESCO Ltd )",  "VRC"),
                                     ARL = c( "Avadh Rail Infra Ltd.",  "ARL", "ARL"),
                                     ARYAN = c( "Aryan Exporters Pvt Ltd" , "ARYAN", "Aryan Exporters" , "Aryan" , "ARYN", "ARIREN"), 
                                     TAYAL = c( "Tayal & Co.",  "TC"),
                                     BASANT = c("Basant Rubber Factory Pvt. Ltd.","BASANT", "BASAANT", 
                                                "BRC", "Basant", "BASAN", "BASNT", "BASAT"  ),
                                     FAS = c( "Frontier Alloy Steels Ltd.", "FAS", "fas", "FASL"),
                                     HFL = c("Howrah Forgings Ltd",   "HFL", "Howrah Forgings, Ltd"),
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
                       dplyr::mutate( Rly = fct_collapse( Rly,  # values( "CR", "ER", "ECR", "NR","NCR", "NWR", "NER", "WR", "WCR", "SR", "SCR", "SWR", )
                                     CR = c("Central Railway", "CENTRAL RAILWAY" , "C.RLY", "CR"), 
                                     ER = c("Eastern Railway", "ER"),
                                     ECR = c( "East Central Rly.", "East Central Railway","E.C.RLY",  "ECR" ),
                                     ECoR = c("East Coast Railway", "ECOR", "ECoR"),
                                     NR = c("Northern Railway", "NORTHERN RAILWAY", "Northern Rly.",  "NR", "N. RLY"),
                                     NCR = c("North Central Railway","N.C.RLY", "NCR"),
                                     NER = c("North Eastern Railway", "NORTH EAST RAILWAY", "NER"),
                                     NFR = c("North Frontier Railway", "NORTH EAST FRONTIER RAILWAY",
                                             "N.F.RLY",  "N. F. RLY"),
                                     NWR = c("North Western Railway", "NORTH WESTERN RAILWAY", "N.W.RLY", "NWR" ),
                                     WR = c("Western Railway", "WR", "WR/W.Call"),
                                     WCR = c("West Central Railway", "WEST CENTRAL RAILWAY", "W. C. RLY", "WCR"),
                                     SECR = c("South East Central Railway",   "SECR"),
                                     SR = c("Southern Railway", "SOUTHERN RAILWAY", "SR", "S.RLY"),
                                     SER = c("South Eastern Railway", "SOUTH EASTERN RAILWAY", "S.E.RLY.",
                                             "S.E.RLY", "SER"),
                                     SECR = c("S.E.C.RLY"),
                                     SCR = c("South Central Railway", "SOUTH CENTRAL RAILWAY", "S.C.RLY", "SCR"),
                                     SWR = c("South Western Railway",  "S.W.RLY", "SWR"),
                                     KRCL = c("KRCL", "Konkan Railway"),
                                     Warrenty = c( "WARRANTY REPLACEMENT", "SCR,WCR,WR,SER,ER,NR,ECOR,SECR,NWR,",
                                                   "ECoR/Warranty Call", "NCR/WARRANTY CALL",   
                                                   "SCR /Warranty Call", "SCR/Warranty Call" ,
                                                   "CR/WARRANTY CALL", "CR/W.Call" ,  "ECR/Warranty Call" , "CR/Warranty Call",
                                                   "NR/Warranty" , "NR/Warranty Call","NR/WARRANTY CALL", "NR/W.Call",
                                                   "NCR/Warranty Call",
                                                   "NWR/Warranty Call",  "NWR/Warranty Call" , "NWR/WarrantyCall",
                                                   "SER/Warranty" , "NFR/Warranty Call" , "SER/Warranty Call",
                                                   "SECR/Warranty Call",
                                                   "WR/Warranty Call", "WR/WARRANTY CALL", "WR/Warranty",
                                                   "WCR/Warranty Call",  "WCR/WARRANTY CALL", "WCR/ Warranty Call"),
                                     #Jupitor = c("JUPITER WAGONS LIMITED,Kolkata", "M/S JUPITER ALLOYS & STEEL (INDIA) LTD."),
                                     #Oriental = c("Oriental Foundry Pvt. Ltd., Kutch, Gujarat", "Oriental Foundry Pvt. Ltd.", "ORIENTAL FOUNDRY PVT. LTD. Kutch, Gujarat"),
                                     #BrandAlloy = c("BRAND ALLOYS PVT. LTD., Hooghly (W.B.)"),
                                     #Siena    =      c("Siena Engineering Pvt. Ltd., Pithampur", "SIENA Engg. Pvt. Ltd."),
                                     #Braithwaith = c("BRAITHWAITE & CO. LIMITED,Kolkata"),
                                     #Texmeco = c("Texmaco Rail & Engineering Limited, Raipur"),
                                     #Besco = c("BESCO LIMITED (FOUNDRY DIVISION) , Kolkata"),
                                     other_level = "OEM"  
                                     )
                                                  )              %>%
                  filter(!is.na(Rly), !is.na(Make))           %>%           #distinct( .keep_all = TRUE) %>%
                 # View()  # all ok so far
                  #glimpse()
                  dplyr::distinct(  ) %>%
                  dplyr::select( Rly, Make,  dm_no, DMdate,  Qty, ValueDM )  -> EMPadDM  


EMPadDM %>% glimpse()


###########################
 


###############################################################################################
###########################

EMPadDM  %>%  filter(!is.na(DMdate))  %>% group_by(DMdate) %>% summarise( supp = sum( Qty ), n = n() )
EMPadDM  %>%  janitor::tabyl(Make)   %>%   janitor::adorn_pct_formatting(digits = 2, affix_sign = TRUE)

EMPadDM %>%  dplyr::distinct( .keep_all = TRUE )  %>% janitor::get_dupes()  %>% View()
EMPadDM %>%  dplyr::distinct( .keep_all = TRUE )   # %>%   janitor::get_dupes()  %>% View()

dplyr::distinct()  # %>% distinct(Sepal.Length, .keep_all = TRUE)
janitor::excel_numeric_to_date(41103)
janitor::remove_empty()
janitor::adorn_title()


# add the quarter and year detaisl column

# Further get the filter for total failures and warrenty reporting data

# Get year wise failures & warrenty failires percentage of total supplies of that period


# Get all data from NR by deputing officer 

# Analise CR data


# Now write to  googlesheet  "EMpadsmaster" ss1 
ss1  = "https://docs.google.com/spreadsheets/d/1YUM-_wDrWpsG57imr2TG0J7QzV9atq28DgFcT5C5RKE" # EMpadsmaster2021
EMPadDM   %>%  dplyr::distinct( .keep_all = TRUE ) %>% 
               #View()
               googlesheets4::sheet_write( ss = ss1 , sheet = "EMpadDM")


######################################################################################
######################################################################################
######################################################################################


#################################################################################################
#################################################################################################
#################################################################################################
########################## Data of warranty posting #############################################

#ss01 = "https://docs.google.com/spreadsheets/d/1lshFtdQf87ONyGdMHbwDRvRtWPM8B_bRPtuuq67P6xE" # GS = "EM pads supply 2017to2021"
#sheet_names(ss01)  # see sheets names

# WarrentyPosted01 <-  googlesheets4::read_sheet(ss01, sheet = "WarrentyPosted" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
# WarrentyPosted01 %>% filter( FAILED > 0 ) %>% group_by(Rly) %>%  View() #tibble::glimpse()  # 896 
# WarrentyPosted01 %>% dplyr::mutate(  Qfailed =  as.numeric(FAILED),
#                                      Qfrom = ymd(parse_date_time(Qfrom, orders = c("dmy") ) ),
#                                      
#                                      QtrFloor = lubridate::quarter( Qfrom, with_year = TRUE, fiscal_start = 1  ) ,
#                                      Qtrmonth = month(Qfrom),
#                                      Qyear = year(Qfrom )   ) %>%  
#                         filter( Qfailed > 0 ) %>%
#                     select(Rly,Qfrom,  QtrFloor, VENDOR, Qfailed , Qyear, Qtrmonth)  %>%
#                    #View() 
#                    #glimpse()
#                    # ss1  = "https://docs.google.com/spreadsheets/d/1YUM-_wDrWpsG57imr2TG0J7QzV9atq28DgFcT5C5RKE" # EMpadsmaster2021
#                    googlesheets4::sheet_write(ss1, sheet =  "postedyearmakerly")

# Now write to  googlesheet  "EMpadsmaster" ss1 
# ss1  = "https://docs.google.com/spreadsheets/d/1YUM-_wDrWpsG57imr2TG0J7QzV9atq28DgFcT5C5RKE" # EMpadsmaster2021
# WarrentyPosted01 <-  googlesheets4::read_sheet(ss01, sheet = "WarrentyPosted" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
########## Read warrenty posted from EMpadmaster sheet ####################################################

################## Raw year wise Data of warranty posting #############################################
ss01 = "https://docs.google.com/spreadsheets/d/1lshFtdQf87ONyGdMHbwDRvRtWPM8B_bRPtuuq67P6xE" # GS = "EM pads supply 2017to2021"
sheet_names(ss01)  # see sheets names
# # Rly	Period From	Period To	F/C Year	Item	Vendor	EQUIPMENT IN SERVICER	Equipment Failed	Status	Entry Date	Post Date


WarrentyPost2021 <-  googlesheets4::read_sheet(ss01, sheet = "warranty 20-21" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
WarrentyPost1920 <-  googlesheets4::read_sheet(ss01, sheet = "warranty 19-20" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
WarrentyPost1819 <-  googlesheets4::read_sheet(ss01, sheet = "warranty 18-19" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
WarrentyPost1718 <-  googlesheets4::read_sheet(ss01, sheet = "warranty 17-18" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
#WarrentyPost1617 <-  googlesheets4::read_sheet(ss01, sheet = "warranty 16-17" , col_names = TRUE,  col_types = "c"  , skip = 0, trim_ws = TRUE, na = "")  # col_types = "ccilDD"

WarrentyPost2021
WarrentyPost2021 %>% View()  #  %>% count() #413
WarrentyPost1920 %>% View()  #%>% count() #268
WarrentyPost1819 %>% View()  #%>% count() #178
WarrentyPost1718 %>% View()  #%>% count() #167

WarrentyPostedAll <- NULL
WarrentyPostedAll <- WarrentyPost1718  
WarrentyPostedAll <- WarrentyPostedAll   %>%   full_join( WarrentyPost1819  ) #var
WarrentyPostedAll <- WarrentyPostedAll   %>%   full_join( WarrentyPost1920  )
WarrentyPostedAll <- WarrentyPostedAll   %>%   full_join( WarrentyPost2021  )
#WarrentyPostedAll <- WarrentyPostedAll   %>%   full_join( WarrentyPost1819  )
WarrentyPostedAll  %>% View()

WarrentyPostedAll  %>% DataExplorer::create_report()

WarrentyPostedAll %>% #filter(!is.na(zone)) %>% 
                      #glimpse()
                     # count(VENDOR)
                      # View()
                      # str()
                    dplyr::distinct() %>%
                     # glimpse() #View()

                   mutate( wqty = as.numeric( `EQUIPMENT FAILED`) ,
                           qtr_from = parse_date_time(`Period from`, orders = c("dmy", "dmY") ),
                           dqtr = lubridate::quarter( qtr_from, with_year = TRUE) ,
                           cqtr = as.character(dqtr) ,
                           Make = as_factor( VENDOR  )
                           ) %>%   
                    # glimpse()
                    mutate(Make = fct_collapse(Make,
                               # Make ARL ARYAN BASANT BONY CALCAST FAS HFL MGM MONDEEP Others PRAG TAYAL VRC
                               ARL =c("ARL"),
                               ARYAN = c("ARYAN") ,
                               BASANT  = c("BASANT"),
                               BONY   = c("BONY", "BPL"),
                               CALCAST = c("CALCAST", "CFL"),
                               FAS    = c("FAS"),
                               HFL  = c("HFL"),
                               MGM   = c("MGM"),
                               MONDEEP   = c("MONDEEP"),
                               Others  = c("Others"),
                               PRAG  = c("PRAG"),
                               TAYAL   = c("TAYAL", "TC"),
                               VRC  = c("VRC")
                                                )
                           ) %>%
                  
                    #glimpse() #    View()
                    #dplyr::select(cqtr,Make, wqty)   %>%  # -> wclaim #
                    googlesheets4::write_sheet(ss1, sheet = "wclaim")

wclaim
wclaim  ->  WarrentyPostedAll
    
         #       tidyr::pivot_wider( id_cols     = zone , names_sort = FALSE, names_sep = "_",
         #                               names_from  = c( VENDOR ) , 
         #                               values_from = qtywarrenty ,    values_fill = NULL,
         #                               values_fn   = sum    # NULL, mean
         #                               )       %>%
         # View()
         # googlesheets4::sheet_write(ss1, sheet =  "Rlywarrenty")  # EMpadsmaster2021


# ss1  = "https://docs.google.com/spreadsheets/d/1YUM-_wDrWpsG57imr2TG0J7QzV9atq28DgFcT5C5RKE" # EMpadsmaster2021








         
         
         
         
################################### Dm data vs Failure data in warranty ###############################      
##########################################################################################################
##########################################################################################################
##########################################################################################################


##########################################################################################################
##########################################################################################################
##########################################################################################################

# NOW
# Rlyreported in make/qtr  = supplies
# Qtr V   Make->       # from 

EMPadDM %>% # glimpse()
            mutate(  DMqtr = lubridate::quarter( DMdate, with_year = TRUE, fiscal_start = 1  ),
                     DMqtr = as.character( DMqtr)    ) %>%
            group_by(Rly, Make, DMqtr) %>%
            summarise( Qtyq = sum(Qty)   )  -> DMdata
             View()
  
DMdata    %>% View()         
             
            
WarrentyPostedAll %>%  glimpse() # View()
                   mutate( FailQtr = lubridate::quarter( Qfrom , with_year = TRUE, fiscal_start = 1  ) ,
                           FailQtr = as.character(FailQtr)    ) %>%
                   dplyr::select( Rly,  FailQtr,  Qfailed,  VENDOR  )    -> RlyWrnty
                   glimpse()
RlyWrnty %>% View()

WarrentyPostedAll -> RlyWrnty # glimpse()






FailFractionlj <- left_join(DMdata, RlyWrnty,  by = c( "DMqtr" = "cqtr", 
                                                        #"Rly" = "Rly", 
                                                        "Make" = "Make")   )  # , suffix = c(".x", ".y") , na_matches = c("na", "never") )
       
FailFraction  <-  full_join(DMdata, RlyWrnty,  by = c( "DMqtr" = "cqtr", 
                                              #"Rly" = "Rly", 
                                              "Make" = "Make") )  # , suffix = c(".x", ".y") , na_matches = c("na", "never") )


FailFraction %>% # mutate( rel = Qfailed/Qtyq*100 )  %>%
               summarise()
               View()
  
FailFraction %>% group_by(Make, DMqtr) %>%
                summarise( qtyt = sum(Qtyq) , qf = sum(Qfailed)   ) %>% 
                mutate(  DMqtr = as.numeric(DMqtr) )   %>% 
                ungroup() %>%  
                arrange( Make, DMqtr, -qf,-qtyt )  %>%  # View()


               ggplot( aes(x= DMqtr, y = qf, fill = Make) ) + 
                 scale_x_continuous(minor_breaks = 4 ) +
                 geom_point( aes(colour = Make )   ) +  
                 #geom_col( aes(colour = Make ) ) +
                 geom_line(aes(colour = Make )  )   +
  
                 geom_line(aes( y = qtyt , colour = Make)   ) +
                 geom_bar(aes( y =  after_stat(count)) , colour = qtyt)   +
                 geom_point(aes( y = qtyt , colour = Make)  , size =3 )
  
  
  
ggplot(mpg, aes(displ)) +  geom_histogram(aes(y = after_stat(count / max(count))))
# Use a transparent version of colour for fill
mpg
mpg %>% ggplot( aes(class, hwy)) + 
     geom_boxplot(  aes(colour = class,  fill = after_scale(   alpha(colour, 0.4)  )  )   )



# Dodge ar Note we convert the cyl variable to a factor here in order to fill by cylinder
ggplot(mpg) +  geom_bar( aes( x = class, fill = factor(cyl)   ),
                         position = position_dodge(preserve = 'single')        )

# geom_bar(position="stack", stat="identity")
# geom_bar(stat = “identity”, position = position_dodge(), alpha = 0.75) gives the side by side bar graphs
#   geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  

# geom_text(  aes(label = Freq), fontface = "bold", vjust = 1.5, position = position_dodge(.9), size = 4) +

# position="fill" fraction

mpg %>% group_by(class) %>% summarise(hwy_mpg = mean(hwy)) %>% 
     ggplot() + geom_bar(  aes(x = class, y = hwy_mpg),  stat = 'identity'  )
# else stat = 'count' is default and ggplot automatically populate the y-axis with a count of our data
# else use geom_col Instead of using geom_bar with stat = 'identity', you can simply use the geom_col 
mpg %>% group_by(class) %>% summarise(hwy_mpg = mean(hwy)) %>%  ggplot() + 
        # same below geom_bar(  aes(x = class, y = hwy_mpg),  stat = 'identity'  )
        geom_col(aes(x = class, y = hwy_mpg))

# facet_grid(. ~Player) 

######## boxplot to correct
FailFraction %>% group_by(Make, DMqtr) %>%
  summarise( qtyt = sum(Qtyq) , qf = sum(Qfailed)   ) %>% 
  mutate(  DMqtr = as.numeric(DMqtr) )   %>% 
  ungroup() %>%  
  arrange( Make, DMqtr, -qf,-qtyt ) %>% #-> tmp  
  View()

tmp  %>%
  ggplot( aes(x=  Make, y = qf)  ) + 
  geom_boxplot(  aes(   colour = Make ,   
                        fill = after_scale(   alpha( colour , 0.4)  ) 
                     )    ) +
  theme_bw()



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
  googlesheets4::sheet_write(ss , sheet = "vendoryearwarrenty")


WarrentyPost2021 %>%  janitor::clean_names() %>% #View()
                       mutate( qty = as.numeric(equipment_failed) ) %>%
                       tidyr::pivot_wider( id_cols = zone, 
                                           names_from  =  vendor,
                                           # names_from  =  zone, for sum
                                           values_from = qty,
                                           values_fn   = sum  ) %>% 
                        View()
# not yet rightly done
WarrentyPost2021 %>%  janitor::clean_names() %>% #View()
  mutate( qty = as.numeric(equipment_failed) )  %>% group_by(zone) %>% summarise( qty)
  ggplot(aes(zone, qty))   + 
  geom_point(zone)
  
WarrentyPost2021 %>% janitor::clean_names() %>%  janitor::tabyl(zone, vendor, )  # %>%   janitor::adorn_pct_formatting(digits = 2, affix_sign = TRUE)



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
  googlesheets4::sheet_write(ss, sheet = "Rlywarrenty")     


WarrentyPosted %>%  glimpse() #View()
  dplyr::mutate( FAILED = as.numeric(FAILED) ,
                 Qtr = parse_date_time(Qfrom, orders = c("dmy")  ),
                 Qtr = quarter(  Qtr, with_year = FALSE, fiscal_start = 1  )   ) %>%
  dplyr::filter( !is.na(FAILED), FAILED > 0 )  %>% 
  select(Rly, )
  View() 
  googlesheets4::sheet_write(ss, sheet = "warrentypost")

# mutate(text = gsub("\\.", " ",text)) %>%  mutate(value = round(as.numeric(value),0)) %>%

WarrentyPosted %>% 
    mutate( Qfrom  = parse_date_time(Qfrom, orders = c("dmy")  ),
            year = year(Qfrom) ,
            FAILED = as.numeric(FAILED) ) %>%  #glimpse()
       filter(!is.na(FAILED), !is.na(VENDOR)) %>%
    plot_time_series(Qfrom, FAILED,
                     .facet_vars   = c(VENDOR, year), # add groups/facets
                     #.color_var    = year,            # color by year
                     #.facet_ncol   = 4,
                     #.facet_scales = "free",
                     .interactive  = FALSE)
  
  

###############################  Timetk
  
  plot_time_series(
    .data,         # tibble or data.frame
    .date_var,     # Date or datetime variable
    .value,        # num column , accepts transformations  like ggplot2
    .color_var = NULL,  # categorical column
    .facet_vars = NULL,
    .facet_ncol = 1,
    .facet_scales = "free_y",  # facet x & y-axis ranges:  "fixed", "free", "free_y", "free_x"
    .facet_collapse = TRUE,
    .facet_collapse_sep = " ",
    .line_color = "#2c3e50",
    .line_size = 0.5,
    .line_type = 1,
    .line_alpha = 1,
    .y_intercept = NULL,   # Value for a y-intercept on the plot
    .y_intercept_color = "#2c3e50",
    .smooth = TRUE,        # trendline smoother loess
    .smooth_period = "auto",
    .smooth_message = FALSE,
    .smooth_span = NULL,
    .smooth_degree = 2,
    .smooth_color = "#3366FF",
    .smooth_size = 1,
    .smooth_alpha = 1,
    .legend_show = TRUE,
    .title = "Time Series Plot",
    .x_lab = "",
    .y_lab = "",
    .color_lab = "Legend",
    .interactive = TRUE, #  Plotly else ggplot if FALSE
    .plotly_slider = FALSE  # returns a plotly date range slider.
  )
  
# group_by() - If groups are detected, multiple facets are returned
# plot_time_series(.facet_vars) - You can manually supply facets as well.
  
  
  
  

####### EM pads failure data tidy ###########################
#sslife <- "https://docs.google.com/spreadsheets/d/1elSHjPakhrMHJsyGuP74FPm0NirIYPnT-DyQome48Lo" ## EMPads failure google sheet  "EMPadFailureZonalRailways"
ss1  = "https://docs.google.com/spreadsheets/d/1YUM-_wDrWpsG57imr2TG0J7QzV9atq28DgFcT5C5RKE" # EMpadsmaster2021
  
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
library(tidyverse)
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
library(vtree)

vtree::vtree(EMpadFD, "Make YearFail",sameline=T)
vtree::vtree(EMpadFD, "Make YearFail YearMade", sameline=T)

vtree(FakeData,"Severity Sex",sameline=T)

# install.packages("gtsummary")
library(gtsummary)

trial2 <- trial %>% select(trt, age, grade)
EMpadFD %>% tbl_summary()%>%    add_n() # %>% add_overall() 

EMpadFD %>% tbl_summary() %>% add_p()
trial2 <- trial %>% select(trt, age, grade)
trial2
trial2 %>%  tbl_summary(
    by = trt,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)" ),
    digits = all_continuous() ~ 2,
    label = grade ~ "Tumor Grade",
    missing_text = "(Missing)"
  )


library(gt)
library(tidyverse)
library(glue)
EMpadFD %>% glimpse()
EMpadFD %>% gt() %>%  tab_header(   title = "S&P 500",  subtitle = "dlkfjdlakjsdfjk"  ) #glue("{MakeDate} to {FailDate}")


# NOT RUN {
require("survival")
require("survminer")
model <- coxph( Surv(time, status) ~ sex + rx + adhere,          
                data = colon )
ggforest(model)

colon <- within(colon, {
  sex <- factor(sex, labels = c("female", "male"))
  differ <- factor(differ, labels = c("well", "moderate", "poor"))
  extent <- factor(extent, labels = c("submuc.", "muscle", "serosa", "contig."))
})
bigmodel <- coxph(Surv(time, status) ~ sex + rx + adhere + differ + extent + node4,
        data = colon )
ggforest(bigmodel)


model <- coxph( Surv(time, status) ~ sex + rx + adhere,          
                data = colon )
ggforest(model)


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



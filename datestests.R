## learning 

# importing from excel
## Dates stored in excel in char
dates <- c("May 27 1984", "July 7 2005")
betterDates <- as.Date(dates,   format = "%B %d %Y")


## Dates stored in excel in numbers, provide origin
# from Windows Excel: origin is = "1899-12-30"  # excel writer mistake as 1900 not leap
dates <- c(30829, 38540)
betterDates <- as.Date(dates,   origin = "1899-12-30")

# from Mac Excel: the origin is "1904-01-01"  1 jan 1904 !!
dates <- c(29367, 37078)
betterDates <- as.Date(dates,   origin = "1904-01-01")

#change output formats
format(betterDates,    "%a %b %d")
#[1] "Sun May 27"     "Thu Jul 07"


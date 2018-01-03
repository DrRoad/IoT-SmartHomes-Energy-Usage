##### This code is still !!!!!WORK IN PROCESS !!!!!!
#
# R-Pipeline-Filtering-TimeSeries
#
##########################################################################################################
# Housekeeping
#################################

###  rm(list = ls())   # Clear all variables from R
rm(list = ls(all.names = TRUE)) # Clear all variables from R and removes hidden objects as well
###- other ways to clear memory 
##-- makeActiveBinding("refresh", function() { system(paste0(R.home(),"/bin/i386/R")); q("no") }, .GlobalEnv)
##-- paste0(R.home(),"/bin/i386/R --no-save") #-- will not save workspace
##-- paste0(R.home(),"/bin/i386/R --save")    #-- will save workspace
#####  .rs.restartR()  ## this command will clear R memory completely

#####  -- set garbage collector
gc(verbose = getOption("verbose"), reset = FALSE)
gcinfo(verbose)
gc() #- run garbage collector now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x, i)
gcinfo(verbose = FALSE) #-- don't show it anymore
gc(TRUE)   #FALSE
gc(reset = TRUE)

#### --  Set working directory
getwd()
setwd("C:/Users/AntoninaPearl/Downloads/Rutgers Data Analytics/Deep Analytics and Visualization")
dir()

##########################################################################################################
## Install and load packages
#####################################
install.packages("lubridate") ## work with dates
install.packages("dplyr")     ## data manipulation (filter, summarize, mutate)
install.packages("tidyr")
install.packages("ggplot2")
install.packages("forecast")
install.packages("labeling")
#####library(gridExtra)       ## not found, used to combine plots
require(lubridate)  ## object is masked from 'package:base':date
require(dplyr)    ## objects are masked from 'package:lubridate': intersect, setdiff, union
                  ##                    from 'package:stats': filter, lag
                  ##                    from 'package:base': intersect, setdiff, setequal, union
require(tidyr)
require(ggplot2)
require(forecast)
require(labeling)

###########################################################################################################
# Load dataset 
###############

hhpwr <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = T,
        quote = "\"", dec = ".", fill = TRUE, comment.char = "")
class(hhpwr)
str(hhpwr)

## ------ Evaluate NA values ----------##
any(is.na(hhpwr)) 

# -- Count the number of values = NA --## 
sum(is.na(hhpwr)) # 259779 NAs
sum(is.na(hhpwr$Date)) 
sum(is.na(hhpwr$Time))
sum(is.na(hhpwr$Sub_metering_1))
sum(is.na(hhpwr$Sub_metering_2))
sum(is.na(hhpwr$Sub_metering_3))  # 259779 NAs
sum(is.na(hhpwr$Global_active_power))
sum(is.na(hhpwr$Global_reactive_power))
sum(is.na(hhpwr$Voltage))
sum(is.na(hhpwr$Global_intensity))

###########################################################################################################
# Pre-process DS 
##################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
hhpwrDT <- hhpwr %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()
# convert DateTime to POSIXct
##  hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
##                                format = "%d/%m/%Y %H:%M:%S",  ## %y for 2 digit, %Y for 4 digit year
##                                 "America/New_York")  ### produced NA for one hour in the sprig for each year 
                                                        ### due to the day light saving time
hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%d/%m/%Y %H:%M:%S","GMT")  

class(hhpwrDT$DateTime) #[1] "POSIXct" "POSIXt" 
tz(hhpwrDT$DateTime) ## check what time zone 

# convert Date to as.Date
hhpwrDT$Date <- as.Date(hhpwrDT$Date, "%d/%m/%Y")

str(hhpwrDT)

## ------ Evaluate NA values ----------##
# Are there any NAs in df?
any(is.na(hhpwrDT)) 

# -- Count the number of values = NA --## 
sum(is.na(hhpwrDT)) #25979 NAs
sum(is.na(hhpwrDT$DateTime)) 
sum(is.na(hhpwrDT$Date)) 
sum(is.na(hhpwrDT$Time))
sum(is.na(hhpwrDT$Sub_metering_1))
sum(is.na(hhpwrDT$Sub_metering_2))
sum(is.na(hhpwrDT$Sub_metering_3))  #25979 NAs
sum(is.na(hhpwrDT$Global_active_power))
sum(is.na(hhpwrDT$Global_reactive_power))
sum(is.na(hhpwrDT$Voltage))
sum(is.na(hhpwrDT$Global_intensity))

##------- Change data types---------##
# Note: Understand the difference between as.numeric(as.character()) and as.numeric()

hhpwrDT$Global_active_power <- as.numeric(as.character(hhpwr$Global_active_power))     # NAs introduced by coercion
hhpwrDT$Global_reactive_power <- as.numeric(as.character(hhpwr$Global_reactive_power)) # NAs introduced by coercion
hhpwrDT$Voltage <- as.numeric(as.character(hhpwr$Voltage))                             # NAs introduced by coercion
hhpwrDT$Global_intensity <- as.numeric(as.character(hhpwr$Global_intensity))           # NAs introduced by coercion
hhpwrDT$Sub_metering_1 <- as.numeric(as.character(hhpwr$Sub_metering_1))               # NAs introduced by coercion
hhpwrDT$Sub_metering_2 <- as.numeric(as.character(hhpwr$Sub_metering_2))               # NAs introduced by coercion             
### hhpwr$Sub_metering_3 is already numeric, doesn't need  reformatting
str(hhpwrDT)

## ------ Evaluate NA values ----------##
# Are there any NAs in df?
any(is.na(hhpwrDT)) 

# -- Count the number of values = NA --## 
sum(is.na(hhpwrDT))                       # 181853 NAs
sum(is.na(hhpwrDT$DateTime))
sum(is.na(hhpwrDT$Date)) 
sum(is.na(hhpwrDT$Time))
sum(is.na(hhpwrDT$Sub_metering_1))        # 25979 NAs
sum(is.na(hhpwrDT$Sub_metering_2))        # 25979 NAs
sum(is.na(hhpwrDT$Sub_metering_3))        # 25979 NAs
sum(is.na(hhpwrDT$Global_active_power))   # 25979 NAs    
sum(is.na(hhpwrDT$Global_reactive_power)) # 25979 NAs
sum(is.na(hhpwrDT$Voltage))               # 25979 NAs
sum(is.na(hhpwrDT$Global_intensity))      # 25979 NAs
hhpwrDT[is.na(hhpwrDT$DateTime),1:9]  ### show 9 columns of no data values
## -------- Save pre-processed dataset --------##
write.csv(hhpwrDT, "household_power_consumption_DT.csv")

#########################################################################################################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): filter for time interval (Yr/Mo/Day) using lubridate w/in dplyr mutate() to create col.
# 2. dplyr::filter(): select cols to filter by; full ds + col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select desired time interval to subset
# 3. dplyr::summarize(): select which vars and any calculations for the vars
# 4. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 5. dplyr::filter() to remove any NA or narrow data ranges 
# 6. Leave in wide format and write all data as csv for use in Tableau or R for plotting

#################################################################################################################
## First create sum per each day of winter 2007-2010 and then average by week day
####################################################

wint.sum.day.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime)) %>%  # Add Year, Month, Day columns
  filter((Year==2007 | Year==2008 | Year==2009 | Year==2010),  
         ( Month == 01 | Month == 02 | Month == 12))%>% 
  group_by(Year, Month, Day) %>%  
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            BAL = round(sum(Global_active_power/60, na.rm = TRUE),3)-SM1 - SM2 - SM3,
            ALL = round(sum(Global_active_power/60, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

wint.sum.day.wide

wint.avg.wday.wide <- wint.sum.day.wide %>%
  mutate(wDay = wday(DateTime,1)) %>%  # Add col week day
  group_by(wDay) %>%  # Group data by week day
  summarize(SM1 = round(mean(SM1, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(SM2, na.rm = TRUE),3), 
            SM3 = round(mean(SM3, na.rm = TRUE),3),
            BAL = round(mean(BAL, na.rm = TRUE),3),
            ALL = round(mean(ALL, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

wint.avg.wday.wide

tally(wint.avg.wday.wide)
names(wint.avg.wday.wide)
class(wint.avg.wday.wide)  
any(is.na(wint.avg.wday.wide))    ##  FALSE, if not, uncomment below and check for NA
# sum(is.na(wint.avg.day.wide$wDay))

plot.ts(wint.avg.wday.wide)
plot.ts(wint.avg.wday.wide$SM1)
plot.ts(wint.avg.wday.wide$SM2)
plot.ts(wint.avg.wday.wide$SM3)

wint.avg.day.wide$DateTime <- NULL

ggplot(wint.avg.wday.wide, aes(x = wDay)) + 
  geom_line(aes(y = SM1), group = interaction("SM1", "wDay"),colour= "black") + 
  geom_line(aes(y = SM2), group = interaction("SM2", "wDay"),colour = "blue") + 
  geom_line(aes(y = SM3), group = interaction("SM3", "wDay"),colour = "red") + 
  geom_line(aes(y = BAL), group = interaction("ALL", "wDay"),colour = "green") +
  geom_line(aes(y = ALL), group = interaction("BAL", "wDay"),colour = "yellow") +
  ylab(label="Sub Meters") + 
  xlab("Weekday")

##### convert to long format
wint.avg.wday.long <- gather(wint.avg.wday.wide, id = c("SM1","SM2","SM3","ALL","BAL") )

tally(wint.avg.wday.long)
names(wint.avg.wday.long)
class(wint.avg.wday.long)  
any(is.na(wint.avg.wday.long))    ##  FALSE, if not, uncomment below and check for NA
# sum(is.na(wint.avg.day.long$wDay))

#### plot long format
ggplot(wint.avg.wday.long, aes(wDay, value, group = key, colour=key,  ylim=0)) + 
  #geom_line(position=position_dodge(width=0.7))+  ## to separate overlaping lines use jitter or dodge or
                                                   ## may also try "linetype = key" in aes section
  geom_line()+   ##position=position_jitter(w=0.1, h=0))+ ##use this code if lines overlap
  geom_point() + geom_rug(col="darkblue") + 
  ggtitle("Winter Daily Average for 2007-2009 ") +
  ylab(label="kWt") + 
  xlab("Weekday")

##----------------  save ds, and use it in Tablaue -----####
write.csv(wint.avg.wday.wide, "Household_Power_Wint_Avg.csv")

####################################################
## Create hourly TOTALS and then AVG in January of 2010
####################################################
Jan.sum.day.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime), Hour = hour(DateTime)) %>%  # Add col week day
  filter((Year==2010),  
         ( Month == 01))%>% 
  group_by(Day, Hour) %>%  # Group data by week day
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            BAL = round(sum(Global_active_power/60, na.rm = TRUE),3)-SM1 - SM2 - SM3,
            ALL = round(sum(Global_active_power/60, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

Jan.sum.day.wide

hour.avg.day.wide <- Jan.sum.day.wide %>%
  mutate(Hour = hour(DateTime)) %>%  # Add col week day
  group_by(Hour) %>%  # Group data by week day
  summarize(SM1 = round(mean(SM1, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(SM2, na.rm = TRUE),3), 
            SM3 = round(mean(SM3, na.rm = TRUE),3),
            BAL = round(mean(BAL, na.rm = TRUE),3),
            ALL = round(mean(ALL, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

hour.avg.day.wide
tally(hour.avg.day.wide)
names(hour.avg.day.wide)
class(hour.avg.day.wide)  
any(is.na(hour.avg.day.wide))    ##  FALSE, if not, uncomment below and check for NA
# sum(is.na(hour.avg.day.wide$Hour))

plot.ts(hour.avg.day.wide)
plot.ts(hour.avg.day.wide$SM1)
plot.ts(hour.avg.day.wide$SM2)
plot.ts(hour.avg.day.wide$SM3)
plot.ts(hour.avg.day.wide$BAL)
plot.ts(hour.avg.day.wide$ALL)

hour.avg.day.wide$DateTime <- NULL

##convert to long format
hour.avg.day.long <- gather(hour.avg.day.wide, id = c("SM1", "SM2", "SM3", "BAL", "ALL"))

tally(hour.avg.day.long)
names(hour.avg.day.long)
class(hour.avg.day.long)  
any(is.na(hour.avg.day.long))    ##  FALSE, if not, uncomment below and check for NA
# sum(is.na(hour.avg.day.long$Hour))

### plot long format
ggplot(hour.avg.day.long, aes(Hour, value, group = key, colour=key, ylim=0)) + geom_line()+ 
  geom_point() + geom_rug(col="darkblue",alpha=.1)+
  ggtitle("Hourly Average for January 2010") +
  ylab(label="kWt") + 
  xlab("Hour")

##----------------  save ds, and use it in Tablaue -----####
write.csv(hour.avg.day.wide, "Household_Power_Wint_Avg.csv")

#################################################################################################################
## Yearly Totals 2007-2009
####################################################
year.sum.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col week day
  filter((Year==2007 | Year==2008 | Year==2009))%>% ### 
  group_by(Year) %>%  # Group data by week day
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),0), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),0), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),0),
            BAL = round(sum(Global_active_power/60, na.rm = TRUE),0)-SM1 - SM2 - SM3,
            ALL = round(sum(Global_active_power/60, na.rm = TRUE),0),
            DateTime = first(DateTime))   # To verify date of first instance

year.sum.wide
tally(year.sum.wide)
names(year.sum.wide)
class(year.sum.wide)  
any(is.na(year.sum.wide))    ##  FALSE, if not, uncomment below and check for NA
# sum(is.na(year.sum.wide$Year))
year.sum.wide$DateTime <- NULL

plot.ts(year.sum.wide)

##### convert to long format
year.sum.long <- gather(year.sum.wide, id = c("SM1","SM2","SM3","BAL","ALL" ) )
tally(year.sum.long)
names(year.sum.long)
class(year.sum.long)  
any(is.na(year.sum.long)) 
# sum(is.na(year.sum.long$Year))  ##  FALSE, if not, uncomment below and check for NA

## plor long format
ggplot(year.sum.long, aes(key, factor(value)), fill = Year, colour=key) +
  geom_col(fill="darkblue") + geom_rug(col="darkred",alpha=.1) + 
  facet_wrap("Year", nrow = 1, scales = "free_x") +
  scale_fill_grey(start = 500, na.value = "red") +
  theme_bw(base_size = 10) +  ggtitle("Yearly Totals 2007-2009") +
  xlab("Meters") +
  ylab(label="kWt")

##----------------  save ds, and use it in Tablaue -----#### fill=rainbow(15)
write.csv(year.sum.wide, "Household_Power_Yearly_Sum.csv")

##------------------------------------------------------------###
##  create DS for SM3 yearly totals for 2007-2009
##------------------------------------------------------------###
SM3.year.sum.wide <- year.sum.long %>%
    filter((Year==2007| Year==2008 | Year==2009),key=="SM3")
SM3.year.sum.wide

##------------------------------------------------------------###
##  create DS for SM3 monthly totals for 2007-2009
##------------------------------------------------------------###

SM3.mo.sum.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime)) %>%  # Add Year, Month
  filter(Date >="2007-01-01" & Date <= "2010-10-31")%>%  
  group_by(Year, Month) %>%  
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),1),
            DateTime = first(DateTime))   # To verify date of first instance

SM3.mo.sum.wide
class(SM3.mo.sum.wide)  
##----------------  create a folder for saving plots -----####
jpeg("myplot.jpg", width=350, height=420)

##-------- save plots commands  -------###
###dev.off("SM1geompoint")
## or
###ggsave("SM1geompoint")

##--------------------------------------------------------###
## yearly forcast for 2010-2011 for SM3
##--------------------------------------------------------###
SM3.year.sum.wide$key <- NULL
SM3.year.sum.wide$Year <- NULL

SM3.year.ts<- ts(SM3.year.sum.wide,start = 2007, end = 2009)
class(SM3.year.ts)

SM3.year.ts
str(SM3.year.ts)
plot.ts(SM3.year.ts)
forecast(SM3.year.sum.wide$value,h=10,level = 4)
forecast(SM3.year.ts,h=5,level = 4)


decmpSM3yr = decompose(SM3.year.ts)
head(decmpSM3yr,1)
tail(decmpSM3yr,1)
str(decmpSM3yr)


######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
###
###    the code below is there just because it has useful info on plots and subselections of ds
###            that might still be used
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

barplot(all.sum.wide$SM1, main="SM1", names.arg = all.sum.wide$DateTime,las=1)


ggplot(all.sum.wide,aes(x=Qtr,y=SM1)) + geom_col(fill=heat.colors(1433)) + ggtitle("2007-2009 Sub-Meter1 by Qtr")
ggplot(all.sum.wide,aes(x=Qtr,y=SM2)) + geom_col(fill=terrain.colors(1433)) + ggtitle("2007-2009 Sub-Meter2 by Qtr")
ggplot(all.sum.wide,aes(x=Qtr,y=SM3)) + geom_col(fill=topo.colors(1433)) + ggtitle("2007-2009 Sub-Meter3 by Qtr")
ggplot(all.sum.wide,aes(x=Qtr,y=BAL)) + geom_col(fill=cm.colors(1433)) + ggtitle("2007-2009 Balance by Qtr")
ggplot(all.sum.wide,aes(x=Qtr,y=ALL)) + geom_col(fill=rainbow(1433)) + ggtitle("2007-2009 All Power by Qtr")

ggplot(all.sum.wide,aes(x=Year,y=SM1)) + geom_col(col="royalblue3") + ggtitle("2007-2009 Sub-Meter1 by Year")
ggplot(all.sum.wide,aes(x=Year,y=SM2)) + geom_col(col="blue") + ggtitle("2007-2009 Sub-Meter2 by Year")
testcolors <- ifelse(all.sum.wide$SM3 >= 10, "blue", "red")
ggplot(all.sum.wide,aes(x=Year,y=SM3)) + geom_col(col=testcolors) + ggtitle("2007-2009 Sub-Meter3 by Year")

ggplot(all.sum.wide,aes(x=DateTime,y=ALL),scale_x_date(labels=date_format("%m-%y"))) + geom_col(fill=rainbow(1433)) + ggtitle("2007-2009 All Power by Year")

Daycount <- table(all.sum.wide$Day)
plot(Daycount)
hist(all.sum.wide$SM1)
hist(all.sum.wide$SM1, breaks = 6, main="choose your title", col = "blue", xlab = "SM1", ylab="frequeny")

qplot(SM1, data=all.sum.wide, binwidth=6)  ## bandwidth (bw) is the standard deviation of the kernel
qplot(SM2, data=all.sum.wide, binwidth=6)
qplot(SM3, data=all.sum.wide, binwidth=6)
qplot(BAL, data=all.sum.wide, binwidth=6)
qplot(ALL, data=all.sum.wide, binwidth=6)
###  or 
ggplot(all.sum.wide, aes(x=ALL)) + geom_histogram(binwidth=6)

#rug plot
SM1geompoint <- ggplot(all.sum.wide,aes(x=all.sum.wide$DateTime,y=all.sum.wide$SM1))  + geom_point() + geom_rug(col="darkred",alpha=.1)


##----------------  create a folder for saving plots -----####
jpeg("myplot.jpg", width=350, height=420)
##-------- save plots commands  -------###
dev.off("SM1geompoint")
## or
ggsave("SM1geompoint")

#----------------  save ds, and use it in Tablaue -----####
write.csv(all.sum.wide, "Household_Power_All_Sum.csv")

################################
## AVG: YEAR, MONTH by sub meter, global power and global minus sub meters (balance power usage), 2007-2009
################################

yrmo.avg.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime)) %>%  # Add col Year and Month
  filter((Year==2007 | Year==2008 | Year==2009 ),
         (!is.na(Sub_metering_1)),   
         (!is.na(Sub_metering_2)),
         (!is.na(Sub_metering_3)),
         (!is.na(Global_active_power))) %>%  ### filter NA
  group_by(Year, Month) %>%  
  summarize(SM1 = round(mean(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(mean(Sub_metering_3 / 1000, na.rm = TRUE),3),
            BAL = round(mean(Global_active_power/60, na.rm = TRUE),3)-SM1 - SM2 - SM3,
            ALL = round(mean(Global_active_power/60,na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance
yrmo.avg.wide
names(yrmo.avg.wide)
class(yrmo.avg.wide)  
any(is.na(yrmo.avg.wide))

ggplot(yrmo.avg.wide,aes(x=Month,y=SM1)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Sub-Meter1 by Month")
ggplot(yrmo.avg.wide,aes(x=Month,y=SM2)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Sub-Meter2 by Month")
ggplot(yrmo.avg.wide,aes(x=Month,y=SM3)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Sub-Meter3 by Month")
ggplot(yrmo.avg.wide,aes(x=Month,y=BAL)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Balance by Month")
ggplot(yrmo.avg.wide,aes(x=Month,y=ALL)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 All Power by Month")


ggplot(yrmo.avg.wide,aes(x=Year,y=SM1)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Sub-Meter1 by Year")
ggplot(yrmo.avg.wide,aes(x=Year,y=SM2)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Sub-Meter2 by Year")
ggplot(yrmo.avg.wide,aes(x=Year,y=SM3)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Sub-Meter3 by Year")
ggplot(yrmo.avg.wide,aes(x=Year,y=BAL)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 Balance by Year")
ggplot(yrmo.avg.wide,aes(x=Year,y=ALL)) + geom_col(fill=rainbow(36)) + ggtitle("2007-2009 All Power by Year")

##plot.ts(yrmo.avg.wide)
plot.ts(yrmo.avg.wide$SM1)
plot.ts(yrmo.avg.wide$SM2)
plot.ts(yrmo.avg.wide$SM3)
plot.ts(yrmo.avg.wide$BAL)
plot.ts(yrmo.avg.wide$ALL)


#####################
## Sum one DAY 15 min blocks; random day selected
#####################

min.sum.wide <- hhpwrDT %>%
  filter(Date == '2009-10-25') %>% 
  group_by(DateTime = cut(DateTime, breaks="15 min")) %>%
  summarize(SM1 = round(mean(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(mean(Sub_metering_3 / 1000, na.rm = TRUE),3),
            BAL = round(mean(Global_active_power/60, na.rm = TRUE),3)-SM1 - SM2 - SM3,
            ALL = round(mean(Global_active_power/60,na.rm = TRUE),3),
            Time = first(Time))
min.sum.wide
names(min.sum.wide)
class(min.sum.wide)
any(is.na(min.sum.wide))

plot.ts(min.sum.wide)
plot.ts(min.sum.wide$SM1,xlab="15 Mintues Aggregation", ylab="kWt", main="2009-10-25 Sub-Meter1",las=1)
plot.ts(min.sum.wide$SM2,xlab="15 Mintues Aggregation", ylab="kWt", main="2009-10-25 Sub-Meter2",las=1)
plot.ts(min.sum.wide$SM3,xlab="15 Mintues Aggregation", ylab="kWt", main="2009-10-25 Sub-Meter3",las=1)
plot.ts(min.sum.wide$BAL,xlab="15 Mintues Aggregation", ylab="kWt", main="2009-10-25 All Minus Submeters Power Used",las=1)
plot.ts(min.sum.wide$ALL,xlab="15 Mintues Aggregation", ylab="kWt", main="2009-10-25 All  Power Used",las=1)

ggplot(min.sum.wide, aes(x=Time, y=SM1)) + geom_point() + ggtitle("SUB METER 1 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=Time, y=SM2)) + geom_point() + ggtitle("SUB METER 2 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=Time, y=SM3)) + geom_point() + ggtitle("SUB METER 3 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=Time, y=BAL)) + geom_point() + ggtitle("BALANCE POWER BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=Time, y=ALL)) + geom_point() + ggtitle("ALL POWER 1 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=SM1), ylim=0) + geom_line() + ggtitle("SUB METER 1 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=SM2), ylim=0) + geom_line() + ggtitle("SUB METER 2 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=SM3), ylim=0) + geom_line() + ggtitle("SUB METER 3 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=BAL), ylim=0) + geom_line() + ggtitle("BALANCE POWER BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=ALL), ylim=0) + geom_line() + ggtitle("ALL POWER BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=SM1)) + geom_line() + geom_point() + ggtitle("SUB METER 1 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=SM2)) + geom_line() + geom_point() + ggtitle("SUB METER 2 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=SM3)) + geom_line() + geom_point() + ggtitle("SUB METER 3 BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=BAL)) + geom_line() + geom_point() + ggtitle("BALANCE POWER BY 15 MINUTES")
ggplot(min.sum.wide, aes(x=as.numeric(DateTime), y=ALL)) + geom_line() + geom_point() + ggtitle("ALL POWER BY 15 MINUTES")

ggplot(min.sum.wide, aes(x=factor(SM1))) + geom_bar(fill=rainbow(9))

barplot(min.sum.wide$SM1,main="Sub Meter1 15 Min Blocks", names.arg = min.sum.wide$Time)
barplot(min.sum.wide$SM2,main="Sub Meter2 15 Min Blocks", names.arg = min.sum.wide$Time)
barplot(min.sum.wide$SM3,main="Sub Meter3 15 Min Blocks", names.arg = min.sum.wide$Time)
barplot(min.sum.wide$BAL,main="All Minus Submitters 15 Min Blocks", names.arg = min.sum.wide$Time)
barplot(min.sum.wide$ALL,main="All Power 15 Min Blocks", names.arg = min.sum.wide$Time)


###############################################
###  TS: daily sum 2007-2009
###############################################

mo.avg.wide <- all.sum.wide %>%
  ##mutate(wDay = wday(DateTime,1)) %>%  # Add col week day
  ##filter(Month==12 | Month==01 | Month==02) %>%
  group_by(Month) %>%  # Group data by week day
  summarize(SM1 = round(mean(SM1),3), # Total kWh per hour
            SM2 = round(mean(SM2),3),
            SM3 = round(mean(SM3),3),
            BAL = round(mean(BAL),3),
            ALL = round(mean(ALL),3))
mo.avg.wide          
plot(mo.avg.wide$SM1)
ggplot(mo.avg.wide, aes(x = Month)) + 
  geom_line(aes(y = SM1), colour="blue") + 
  geom_line(aes(y = SM2), colour = "grey") + 
  geom_line(aes(y = SM3), colour = "black") + 
  geom_line(aes(y = BAL), colour = "green") +
  geom_line(aes(y = ALL), colour = "red") +
  ylab(label="SM") + 
  xlab("Month")
###mo.avg.wide$Month <- NULL
mots<- ts(mo.avg.wide[, c("Month","SM1","SM2","SM3")],frequency = 12)
mots<- ts(mo.avg.wide[, c("SM1")],frequency = 12)
mots
str(mots)
plot.ts(mo.avg.wide)



decmots = decompose(mots)
head(decmots,1)
tail(decmots,10)
str(decmots)


## allts<- ts(all.sum.wide,frequency = 365, start=c(2007,1),end = c(2009,364))
#  2008 was a leap year, but there were 2 days with no data (4/29/07 and 6/14/09) that's why "364" as end
### don't need the logic above since ds is already prefiltered 

allts<- msts(all.sum.wide, ts.frequency = 1440,525960) #, start = c(2006,4,12,16))
options(max.print=1000000) 
# even by overriding print default, Rstudio's default doesn't show the entire ts
# check it by looking at head and tail
head(allts,10)
tail(allts,10)
str(allts$NULL)
plot.ts(allts, aes(x=NULL, y=SM1)) + geom_point() + ggtitle("2007-2009 Sub-Meter1 Daily")
decallts = decompose(allts)
head(decallts,1)
tail(decallts,10)
str(decallts)

monthts<- ts(all.sum.wide,frequency = 12)
options(max.print=1000000) 
# even by overriding print default, Rstudio's default doesn't show the entire ts
# check it by looking at head and tail
head(monthts,100)
tail(monthts,100)
str(monthts)
plot.ts(allts)
decallts = decompose(allts)
head(decallts,1)
tail(decallts,10)
str(decallts)

allts1 <- ts(hhpwrDT, frequency=1440, start=c(2007,1),end = c(2009,364))
head(allts1,100)
tail(allts1,100)
str(allts1)
plot.ts(allts1)
decallts = decompose(allts1)
head(decallts1,1)
tail(decallts1,10)
str(decallts1)

###############################################
###  TS: monthly avg 2007-2009
###############################################
####   for SM1
motsSM1<- ts(yrmo.avg.wide[3],frequency = 12)  ##, start=c(2007,1),end = c(2009,12))
motsSM1
str(motsSM1)
plot.ts(motsSM1)
decmots = decompose(motsSM1)
motsSM1

###############################################
###  TS: Wint.sum for weekdays 2007-2009 Dec-Feb
###############################################
####   for SM1
wintsum_ts_SM1<- ts(wint.sum.wide[2],frequency = 7)    ###, start=c(2007,1),end = c(2009,12))
wintsum_ts_SM1
str(wintsum_ts_SM1)
plot.ts(wintsum_ts_SM1)

hwintersWintsum.mean <-  HoltWinters(wint.sum.wide, alpha = 0.2, beta = FALSE, gamma = FALSE)
hwintersWintsum.predict<- predict(hwintersWintsum.mean, n.ahead = 7, prediction.interval = TRUE)
##### code from the tutuorail   #######################################################
plotHoltWintersForecast <- function(wintsum_ts_SM1, n.ahead=4, CI=.95, error.ribbon='green', line.size=1) {
  # Calculate a forecast based on the Holt-Winters algorithm
  hw_object <- HoltWinters(wintsum_ts_SM1);
  forecast <- predict(hw_object, n.ahead=3, prediction.interval=T, level=CI);
  
  # Retrieve forecast, fitted and actual values
  for_values    <- data.frame(time=round(time(forecast), 3),
                              value_forecast=as.data.frame(forecast)$fit,
                              dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit);
  fitted_values <- data.frame(time=round(time(hw_object$fitted),3),
                              value_fitted=as.data.frame(hw_object$fitted)$xhat);
  actual_values <- data.frame(time=round(time(hw_object$x), 3),
                              Actual=c(hw_object$x));
  
  # Generate graphset to plot the values
  graphset <- merge(actual_values,  fitted_values,  by='time',  all=TRUE);
  graphset <- merge(graphset,  for_values,  all=TRUE,  by='time');
  graphset[is.na(graphset$dev),  ]$dev<-0;
  graphset$Fitted <- c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),
                       fitted_values$value_fitted,  for_values$value_forecast);
  graphset.melt <- melt(graphset[, c('time', 'Actual', 'Fitted')], id='time');
  
  # Create plot object and return it
  plot <- ggplot(graphset.melt, aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + geom_line(aes(colour=variable), size=line.size) + geom_vline(x=max(actual_values$time),  lty=2) + xlab('Time') + ylab('Value') + theme(legend.position='bottom') + scale_colour_hue('');
  return(plot);
}
##################33 end of code from the tutoral   ############################################3
##------------------------- working section ---------------##

mots<- ts(data=NA, frequency = 12, start=c(2007,1), end = c(2009,12), deltat = 1, ts.eps = getOption("ts.eps"),
          +        class=if(nseries > 1)c("mts","ts","matrix") else "ts", names = if(!is.null(dimnames(yrmp.avg.wide))) 
            +        colnames(hhpwrDT) else paste("Series",seq(nseries)))

min.sum.ts <- ts(min.sum.wide)  ## don't know
min.sum.ts

wint.avg.ts <- ts(wint.avg.wide) #,frequency=7)  ##doesn't work
wint.avg.ts

yrmo.avg.ts <- ts(yrmo.avg.wide,frequency=12)  
yrmo.avg.ts

##--------------------------------------------------------##
tslm(formula, data, subset, lambda = NULL, biasadj = FALSE)
###tslm and decomposed models
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
#fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

########################################
# Train and test model on train ds
########################################
ctreefit1 <- train(brand~., data = trainSet, method = "ctree", trControl = fitControl) 
ctreefit1

####old mean by wday incorrect version
wint.avg.day.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), wDay = wday(DateTime,1)) %>%  # Add col week day
  filter((Year==2007 | Year==2008 | Year==2009 | Year==2010),  
         ( Month == 01 | Month == 02 | Month == 12),
         (!is.na(Sub_metering_1)),  
         (!is.na(Sub_metering_2)),
         (!is.na(Sub_metering_3)),
         (!is.na(Global_active_power)))%>% ### filter NA
  group_by(wDay) %>%  # Group data by week day
  summarize(SM1 = round(mean(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(mean(Sub_metering_3 / 1000, na.rm = TRUE),3),
            BAL = round(mean(Global_active_power/60, na.rm = TRUE),3)-SM1 - SM2 - SM3,
            ALL = round(mean(Global_active_power/60, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

wint.avg.day.wide

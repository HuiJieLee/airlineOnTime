# load libraries
library(zoo)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(lubridate)
library(ggplot2)

# Batch import files
files <- list.files(pattern = ".csv$")
ontime <- lapply(files, read.csv)
ontime <- do.call(rbind,ontime)

summary(ontime)
str(ontime) 

# parse date
ontime$FL_DATE <- ymd(ontime$FL_DATE)
# remove NA values in ARR_TIME and CRS_DEP_TIME
good <- complete.cases(ontime$ARR_DELAY, ontime$CRS_DEP_TIME)
ontime<- ontime[good,]

# format CRS_DEP_TIME
ontime$CRS_DEP_TIME <- sprintf(ontime$CRS_DEP_TIME, fmt = "%04d")
ontime$CRS_DEP_TIME <- parse_date_time(ontime$CRS_DEP_TIME, orders = "%H%M")

# Select only ORD to NY data
ORDtoNY <- ontime[(ontime$ORIGIN == "ORD") & (ontime$DEST == "JFK" | ontime$DEST == "LGA"),]
ORDtoNY2 <- ORDtoNY
# Make delay time < 0 (i.e. on time) = 0
ORDtoNY2$ARR_DELAY[ORDtoNY2$ARR_DELAY < 0] <- 0
# Average arrival delay minutes by airline
ByAirline <- ddply(ORDtoNY2, .(DEST, UNIQUE_CARRIER), summarize, avgDelay = mean(ARR_DELAY), sdDelay = sd(ARR_DELAY), minDelay = min(ARR_DELAY), maxDelay = max(ARR_DELAY), medianDelay = median(ARR_DELAY))

# Plot average arrival delay mintes by airline and by LGA/JFK
pdf("airline.pdf")
ggplot(ByAirline, aes(x = UNIQUE_CARRIER, y = avgDelay, fill = DEST)) + geom_bar(stat = "identity") + facet_wrap(~DEST) + labs(x = "Airline", y = "Average delay (mins)", title = "Flight from Chicago to New York City in 2014")
dev.off()


# Specify weekday
ORDtoNY2$day <- weekdays(ORDtoNY2$FL_DATE)
# Specify time in a day
time <- hour(ORDtoNY2$CRS_DEP_TIME)
time_breaks <- hour(hm("00:00", "12:00", "17:00", "21:00", "23:59"))
time_labels <- c("Morning", "Afternoon", "Evening", "Night")
ORDtoNY2$times <- cut(hour(ORDtoNY2$CRS_DEP_TIME), breaks = time_breaks, labels = time_labels)
ByTime <- ddply(ORDtoNY2, .(day, times), summarize, avgDelay = mean(ARR_DELAY), sdDelay = sd(ARR_DELAY), minDelay = min(ARR_DELAY), maxDelay = max(ARR_DELAY), medianDelay = median(ARR_DELAY))

# Plot average delay as a function of day and time
pdf("time.pdf")
ggplot(ByTime, aes(x = day, y = times)) + geom_tile(aes(fill = avgDelay), color = "white") + scale_fill_gradient(low = "white", high = "steelblue", name = "Average Delay (mins)") + labs(x = "Day", y = "Times", title = "Average delay from Chicago to New York City by day and time in 2014")
dev.off()


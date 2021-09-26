
# Test to Github

#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages(c('R6', 'jsonlite'))
#install.packages(c('vscDebugger'))

#install.packages("reshape")

library(readxl)
library(dplyr)
library(reshape)
library(ggplot2)
#library(xlsx)
#library(vscDebugger)

download.file('https://www.ssm.gov.mo/docs/stat/apt/RNA010.xlsx', 'RNA010.xlsx', method='curl' )
rdf <- read_excel("RNA010.xlsx", sheet="20210804", na = "---")
rdf1 <- rdf %>% select(c(2,4:ncol(rdf))) %>% slice(4:nrow(rdf))
rdf <- read_excel("RNA010.xlsx", sheet="20210805", na = "---")
rdf2 <- rdf %>% select(c(2,4:ncol(rdf))) %>% slice(3:nrow(rdf))
rdf <- read_excel("RNA010.xlsx", sheet="20210806", na = "---")
rdf3 <- rdf %>% select(c(2,4:ncol(rdf))) %>% slice(3:nrow(rdf))
rdf <- read_excel("RNA010.xlsx", sheet="20210807", na = "---")
rdf4 <- rdf %>% select(c(2,4:ncol(rdf))) %>% slice(3:nrow(rdf))
rdf1$ReservationDate <- "2021-08-04"
rdf2$ReservationDate <- "2021-08-05"
rdf3$ReservationDate <- "2021-08-06"
rdf4$ReservationDate <- "2021-08-07"
df <- rbind(rdf1,rdf2,rdf3,rdf4)
df$ReservationTime <- substr(df$預約時段,1,5)
df$ReservationDateTime <- paste (df$ReservationDate, df$ReservationTime)
df <- df[, 2:ncol(df)]
data <- as.data.frame(df)
dfm <- melt(data, id = c("ReservationDateTime", "ReservationDate", "ReservationTime"))
#dfm[is.na(dfm)] <- 0
#dfn <- filter(dfm, variable == "澳門大學")
#, value < quantile(dfm$value, 0.90, na.rm=TRUE))


# View by 24 hours
png(file="SSM-hr.png", width = 2150, height = 900)
g <- ggplot(dfm, aes(ReservationTime,value,colour = ReservationDate, group = 1))
g + geom_point() + #geom_line() +
  facet_wrap(~variable, nrow = 6, ncol = 7) +
  ggtitle("View by 24 hours") + xlab("Hours in a day") + ylab("Number of Reservations") +
  theme(text=element_text(size=18, family="Serif"))
dev.off()


# View by days
png(file="SSM-day.png", width = 2150, height = 900)
g <- ggplot(dfm, aes(y=value,colour = ReservationDate))
g + geom_boxplot() +
  facet_wrap(~variable, nrow = 6, ncol = 7) +
  ggtitle("View by 4 days") + xlab("1st to 4th day") + ylab("Reservation Variance") +
  theme(text=element_text(size=18, family="Serif"))
dev.off()


library(flux)
library(readxl)
library(dplyr)
library(reshape)
library(ggplot2)
library(stringr)
library(tidyr)
library(gridExtra)
library(ggmap)
library(plotly)
library(GGally)

filelist <- dir('aptmon-scraping/20211004', full.names=TRUE)
scrp <- data.frame()
for (file in filelist) {
    filetimestr <- sub(".csv", "",sub(".*-", "", file))
    filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
    temp <- read.csv(file, na = "---")
    temp$DateTime <- as.POSIXlt(filetime)
    scrp <- rbind(scrp,as.data.frame(temp))
}
str(scrp)

tail(scrp)

attr(scrp$DateTime, "tzone") <- "GMT"
scrp$DateTimeRound <- round(scrp$DateTime, "30mins")
scrp$WaitingQueue <- as.numeric(as.character(sub("*�?", "", scrp$輪候人�?)))
scrp$WaitingMinutes <- as.numeric(as.character(sub("分鐘", "",sub(".*>", "", scrp$等候時�?))))
scrp$StationCount <- rowSums(scrp[ ,c("口採樣點", "鼻採樣點")], na.rm=TRUE)
scrp$HourNumber <- 
sapply(strsplit(substr(scrp$DateTimeRound,12,16),":"),
  function(x) {
    x <- as.numeric(x)
    x[1]+x[2]/60
    }
)

filter(scrp, is.na(scrp$DateTime) == TRUE)


out_path <- rmarkdown::render("markdown/SSM-NAT-EDA-Oct/SSM-NAT-EDA-Oct.rmd")
browseURL(out_path)

getwd()
setwd( "D:/Documents/GitHub/ssm-rna-test/")

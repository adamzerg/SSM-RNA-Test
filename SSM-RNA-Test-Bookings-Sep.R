
#install.packages("XML")
#install.packages("RSelenium")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages(c('R6', 'jsonlite'))
#install.packages(c('vscDebugger'))
#install.packages("reshape")


library(rvest)
library(data.table)
library(httr)
library(XML)
library(RSelenium)
mybrowser <- rsDriver(browser = 'firefox', verbose = TRUE, port=4569L,)

link <- "https://eservice.ssm.gov.mo/aptmon/aptmon/ch"
mybrowser$client$navigate(link)

#mybrowser$client$findElement(using = 'id', "tblist")$getElementText()
html.table.0 <-  mybrowser$client$findElement(using = 'id', "tblist")
webElem5txt.0 <- html.table.0$getElementAttribute("outerHTML")[[1]]

#read_html(webElem5txt.0) %>% html_nodes('div.hidden-xs') %>% html_text(trim = TRUE)
df.table.0 <-  read_html(webElem5txt.0) %>% html_table() %>% data.frame(.)
df.table.0$地點 <- read_html(webElem5txt.0) %>% html_nodes('div.hidden-xs') %>% html_text(trim = TRUE)
station <- df.table.0[,c(1:4,7,5,6)]

#head(station)
#summary(station)

write.csv(station,paste("station-",format(Sys.time(), "%Y%m%d%H%M%S"),".csv", sep = ""), row.names = TRUE)
mybrowser$server$stop()






library(readxl)
library(dplyr)
library(reshape)
library(ggplot2)
library(stringr)
#library(xlsx)

download.file('https://www.ssm.gov.mo/docs/stat/apt/RNA010.xlsx', 'RNA010.xlsx', method='curl' )

## Sept 25

### Extract first row for location list
cnames <- read_excel("RNA010.xlsx", sheet="20210925A", n_max = 0, na = "---") %>% names()
lls1 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf1 <- read_excel("RNA010.xlsx", sheet="20210925A", na = "---", skip = 2) #skip 2 because there exists a hidden row 1 in this spreadsheet
sdf1 <- rdf1 %>% select(c(1:2,6:ncol(rdf1))) %>% slice(2:nrow(rdf1)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls1, each = nrow(sdf1) * 2)
### Set date
sdf1$預約日期 <- as.Date("2021-09-25")
### Melt to pivot
sdf1 <- as.data.frame(sdf1)
mdf1 <- melt(sdf1, id = c("預約日期", "預約時段"))
### Combine Location with dataset
df1 <- cbind(Location,mdf1)
### Clean away column names with ...
df1$variable <- sub("\\....*", "", df1$variable)


## Sept 26

### Extract first row for location list
cnames <- read_excel("RNA010.xlsx", sheet="20210926A", n_max = 0, na = "---") %>% names()
lls2 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf2 <- read_excel("RNA010.xlsx", sheet="20210926A", na = "---", skip = 1)
sdf2 <- rdf2 %>% select(c(1:2,6:ncol(rdf2))) %>% slice(2:nrow(rdf2)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls2, each = nrow(sdf2) * 2)
### Set date
sdf2$預約日期 <- as.Date("2021-09-26")
### Melt to pivot
sdf2 <- as.data.frame(sdf2)
mdf2 <- melt(sdf2, id = c("預約日期", "預約時段"))
### Combine Location with dataset
df2 <- cbind(Location,mdf2)
### Clean away column names with ...
df2$variable <- sub("\\....*", "", df2$variable)


## Sept 27

### Extract first row for location list
cnames <- read_excel("RNA010.xlsx", sheet="20210927A", n_max = 0, na = "---") %>% names()
lls3 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf3 <- read_excel("RNA010.xlsx", sheet="20210927A", na = "---", skip = 1)
sdf3 <- rdf3 %>% select(c(1:2,6:ncol(rdf3))) %>% slice(2:nrow(rdf3)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls3, each = nrow(sdf3) * 2)
### Set date
sdf3$預約日期 <- as.Date("2021-09-27")
### Melt to pivot
sdf3 <- as.data.frame(sdf3)
mdf3 <- melt(sdf3, id = c("預約日期", "預約時段"))
### Combine Location with dataset
df3 <- cbind(Location,mdf3)
### Clean away column names with ...
df3$variable <- sub("\\....*", "", df3$variable)


## Sept 28

### Extract first row for location list
cnames <- read_excel("RNA010.xlsx", sheet="20210928A", n_max = 0, na = "---") %>% names()
lls4 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf4 <- read_excel("RNA010.xlsx", sheet="20210928A", na = "---", skip = 1)
sdf4 <- rdf4 %>% select(c(1:2,6:ncol(rdf4))) %>% slice(2:nrow(rdf4)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls4, each = nrow(sdf4) * 2)
### Set date
sdf4$預約日期 <- as.Date("2021-09-28")
### Melt to pivot
sdf4 <- as.data.frame(sdf4)
mdf4 <- melt(sdf4, id = c("預約日期", "預約時段"))
### Combine Location with dataset
df4 <- cbind(Location,mdf4)
### Clean away column names with ...
df4$variable <- sub("\\....*", "", df4$variable)

# df4[c(1:62),]
# filter(df4, Location == "中葡職業技術學校體育館")

df <- rbind(df1,df2,df3,df4)
df$ReservationDateTime <- as.POSIXlt(paste(df$預約日期, substr(df$預約時段,1,5)))
df$ReservationCalendarTime <- as.POSIXct(paste(df$預約日期, substr(df$預約時段,1,5)))
df$ReservationTime <- 
sapply(strsplit(substr(df$預約時段,1,5),":"),
  function(x) {
    x <- as.numeric(x)
    x[1]+x[2]/60
    }
)


station$Location <- station$地點
sdf <- station[c(8,1,3,4,5), ]
mdf <- merge(sdf, df, by = "Location")
mdf$ReservationPerStation <- ifelse(mdf$variable == "口咽拭",mdf$value/ifelse(mdf$口採樣點==0,1,mdf$口採樣點),mdf$value/ifelse(mdf$鼻採樣點==0,1,mdf$鼻採樣點))
fdf <- filter(mdf, ReservationDateTime <= Sys.time())
summary(fdf)

#summary(df)
#summary(mdf)
#filter(df, Location == "北安客運碼頭")
#filter(mdf, Location == "北安客運碼頭")
#head(df)
#df[is.na(df)] <- 0


gdf <- df %>% group_by(ReservationTime,variable) %>%
      summarise(value.mean = mean(value, na.rm = TRUE))
ggplot(gdf,aes(x = ReservationTime, y = value.mean)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_grid(variable~. )


gdf <- df %>% group_by(ReservationTime,預約日期,variable) %>%
      summarise(value.sum = sum(value, na.rm = TRUE))
ggplot(gdf,aes(x = ReservationTime, y = value.sum)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_grid(預約日期~. )


gdf <- mdf %>% group_by(Location,ReservationTime,variable) %>%
      summarise(value.mean = mean(ReservationPerStation, na.rm = TRUE))
ggplot(gdf,aes(x = ReservationTime, y = value.mean)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_wrap(~Location, nrow = 6, ncol = 8) +
ggtitle("RNA test per sampling method by location") + xlab("24 hours") + ylab("Average test per station")


gdf <- mdf %>% group_by(Location,預約日期,variable) %>%
      summarise(value.mean = mean(ReservationPerStation, na.rm = TRUE))
ggplot(gdf,aes(x = 預約日期, y = value.mean)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_wrap(~Location, nrow = 6, ncol = 8) +
ggtitle("RNA test per sampling method by location") + xlab("4 days") + ylab("Average test per station")


ggplot(mdf,aes(x = ReservationCalendarTime, y = ReservationPerStation)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_wrap(~Location, nrow = 6, ncol = 8) +
ggtitle("RNA test per sampling method by location") + xlab("All intervals") + ylab("Average test per station")


gdf <- mdf %>% group_by(Location,ReservationCalendarTime,variable) %>%
      summarise(value.sum = sum(value, na.rm = TRUE))
ggplot(gdf,aes(x = ReservationCalendarTime, y = value.sum)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_wrap(~Location, nrow = 6, ncol = 8) +
ggtitle("RNA test per sampling method by location") + xlab("All intervals") + ylab("Total test")

top_n(gdf, 5)
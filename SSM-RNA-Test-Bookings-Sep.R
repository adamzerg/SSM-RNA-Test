#install.packages("XML")
#install.packages("RSelenium")
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
library(stringr)
library(gridExtra)

scrp1 <- read.csv("aptmon-scraping/station-20210926202025.csv", na = "---")
scrp2 <- read.csv("aptmon-scraping/station-20210926230837.csv", na = "---")
scrp3 <- read.csv("aptmon-scraping/station-20210927093840.csv", na = "---")
scrp4 <- read.csv("aptmon-scraping/station-20210927195730.csv", na = "---")

scrp1$DateTime <- as.POSIXct(strptime("20210926202025","%Y%m%d%H%M%S"))
scrp2$DateTime <- as.POSIXct(strptime("20210926230837","%Y%m%d%H%M%S"))
scrp3$DateTime <- as.POSIXct(strptime("20210927093840","%Y%m%d%H%M%S"))
scrp4$DateTime <- as.POSIXct(strptime("20210927195730","%Y%m%d%H%M%S"))

scrp <- rbind(scrp1,scrp2,scrp3,scrp4)

#head(scrp)
#filter(scrp, 地點 == "街坊會聯合總會社區服務大樓")

station <- scrp %>% group_by(序號,地點,類別) %>%
      summarise(
            口採樣點.mean = mean(口採樣點, na.rm = TRUE),
            鼻採樣點.mean = mean(鼻採樣點, na.rm = TRUE),
            口採樣點.median = median(口採樣點, na.rm = TRUE),
            鼻採樣點.median = median(鼻採樣點, na.rm = TRUE),
            口採樣點.min = min(口採樣點, na.rm = TRUE),
            鼻採樣點.min = min(鼻採樣點, na.rm = TRUE),
            口採樣點.max = max(口採樣點, na.rm = TRUE),
            鼻採樣點.max = max(鼻採樣點, na.rm = TRUE)
      ) %>% as.data.frame()

colnames(station)[which(names(station) == "地點")] <- "Location"
head(station)



# download.file('https://www.ssm.gov.mo/docs/stat/apt/RNA010.xlsx', 'RNA010.xlsx', method='curl' )

## Sept 25

### Extract first row for location list
cnames <- read_excel("RNA010-0928934.xlsx", sheet="20210925A", n_max = 0, na = "---") %>% names()
lls1 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf1 <- read_excel("RNA010-0928934.xlsx", sheet="20210925A", na = "---", skip = 2) #skip 2 because there exists a hidden row 1 in this spreadsheet
sdf1 <- rdf1 %>% select(c(1:2,6:ncol(rdf1))) %>% slice(2:nrow(rdf1)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls1, each = nrow(sdf1) * 2)
### Set date
sdf1$預約日期 <- as.Date("2021-09-25")
### Melt to pivot
sdf1 <- as.data.frame(sdf1)
mdf1 <- reshape::melt(sdf1, id = c("預約日期", "預約時段"))
### Combine Location with dataset
df1 <- cbind(Location,mdf1)
### Clean away column names with ...
df1$variable <- sub("\\....*", "", df1$variable)


## Sept 26

### Extract first row for location list
cnames <- read_excel("RNA010-0928934.xlsx", sheet="20210926A", n_max = 0, na = "---") %>% names()
lls2 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf2 <- read_excel("RNA010-0928934.xlsx", sheet="20210926A", na = "---", skip = 1)
sdf2 <- rdf2 %>% select(c(1:2,6:ncol(rdf2))) %>% slice(2:nrow(rdf2)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls2, each = nrow(sdf2) * 2)
### Set date
sdf2$預約日期 <- as.Date("2021-09-26")
### Melt to pivot
sdf2 <- as.data.frame(sdf2)
mdf2 <- reshape::melt(sdf2, id = c("預約日期", "預約時段"))
### Combine Location with dataset
df2 <- cbind(Location,mdf2)
### Clean away column names with ...
df2$variable <- sub("\\....*", "", df2$variable)


## Sept 27

### Extract first row for location list
cnames <- read_excel("RNA010-0928934.xlsx", sheet="20210927A", n_max = 0, na = "---") %>% names()
lls3 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf3 <- read_excel("RNA010-0928934.xlsx", sheet="20210927A", na = "---", skip = 1)
sdf3 <- rdf3 %>% select(c(1:2,6:ncol(rdf3))) %>% slice(2:nrow(rdf3)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls3, each = nrow(sdf3) * 2)
### Set date
sdf3$預約日期 <- as.Date("2021-09-27")
### Melt to pivot
sdf3 <- as.data.frame(sdf3)
mdf3 <- reshape::melt(sdf3, id = c("預約日期", "預約時段"))
### Combine Location with dataset
df3 <- cbind(Location,mdf3)
### Clean away column names with ...
df3$variable <- sub("\\....*", "", df3$variable)


## Sept 28

### Extract first row for location list
cnames <- read_excel("RNA010-0928934.xlsx", sheet="20210928A", n_max = 0, na = "---") %>% names()
lls4 <- cnames[seq(6, length(cnames), 3)]
### Extract data from 2nd row 
rdf4 <- read_excel("RNA010-0928934.xlsx", sheet="20210928A", na = "---", skip = 1)
sdf4 <- rdf4 %>% select(c(1:2,6:ncol(rdf4))) %>% slice(2:nrow(rdf4)) %>% select(-contains("總人次"))
### Repeat Location info for number of rows
Location <- rep(lls4, each = nrow(sdf4) * 2)
### Set date
sdf4$預約日期 <- as.Date("2021-09-28")
### Melt to pivot
sdf4 <- as.data.frame(sdf4)
mdf4 <- reshape::melt(sdf4, id = c("預約日期", "預約時段"))
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


mdf <- merge(station[1:5], df, by = "Location")
mdf$ReservationPerStation <- ifelse(
      mdf$variable == "口咽拭",
      mdf$value/mdf$口採樣點.mean,
      mdf$value/mdf$鼻採樣點.mean
      )
summary(mdf)


#fdf <- filter(mdf, ReservationDateTime <= Sys.time())
#filter(df, Location == "北安客運碼頭")
#filter(mdf, Location == "北安客運碼頭")
#head(df)
#df[is.na(df)] <- 0



#1. 截至結束前六小時，總預約數  
sum(mdf$value,na.rm = TRUE)

#2. 口咽拭約為鼻咽拭的兩倍  
p1 <- mdf %>% group_by(variable) %>% summarise(value.sum = sum(value, na.rm = TRUE))

ggplot(data=p1, aes(x = variable, y = value.sum)) +
  geom_bar(stat="identity", fill="steelblue") + coord_flip() +
  theme_minimal() +
  ggtitle("Total RNA test by sampling method") + xlab("Sampling method") + ylab("Total bookings")

#3. 預約總數的採樣點排名  
p2 <- mdf %>% group_by(Location) %>% tally(value)

ggplot(data=p2, mapping = aes(x = reorder(Location,n), n)) +
  geom_bar(stat="identity", fill="steelblue") + coord_flip() +
  theme_minimal() +
  ggtitle("Total RNA test by location") + xlab("Location") + ylab("Total bookings")

#4. 四日內預約情況看，高峰集中在第二日  
p3 <- mdf %>% group_by(ReservationTime,預約日期) %>% summarise(value.sum = sum(value, na.rm = TRUE))

ggplot(p3,aes(x = ReservationTime, y = value.sum, color = value.sum)) +
geom_line() + facet_grid(預約日期~. )

#5. 預約單日的排名  
p4 <- mdf %>% group_by(Location,預約日期) %>% tally(value) %>% top_n(1) %>% as.data.frame()

ggplot(data=p4, mapping = aes(x = reorder(paste(Location,預約日期),n), n)) +
  geom_bar(stat="identity", fill="steelblue") + coord_flip() +
  theme_minimal() +
  ggtitle("Total RNA test by location") + xlab("Location top day") + ylab("Top day daily handled bookings")




pairs(mdf[c(4,5,9,12)], main = "RNA Bookings", pch = 21)

gdf <- mdf %>% group_by(Location,ReservationTime,variable) %>%
      summarise(value.sum = sum(value, na.rm = TRUE))
ggplot(gdf,aes(x = ReservationTime, y = value.sum)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_wrap(~Location, nrow = 6, ncol = 8) +
ggtitle("RNA test per sampling method by location") + xlab("All intervals") + ylab("Total test")

p5 <- mdf %>% group_by(Location,ReservationTime,variable) %>%
      summarise(value.mean = mean(ReservationPerStation, na.rm = TRUE))
ggplot(p5,aes(x = ReservationTime, y = value.mean)) +
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

tail(p5)

p5 <- mdf %>%
      group_by(Location,ReservationCalendarTime,variable) %>%
      summarise(value.sum = sum(value, na.rm = TRUE))
ggplot(p5,aes(x = ReservationCalendarTime, y = value.sum)) +
geom_line(stat = "identity", aes(color = variable)) +
facet_wrap(~Location, ncol = 2) 

#facet_wrap(~Location, nrow =2, ncol = 2) +
#geom_line(stat = "identity", aes(color = Location)) +
#geom_smooth(stat = "identity", aes(color = Location)) +
#facet_grid(Location~.) +
#ggtitle("RNA test per sampling method by location") + xlab("All intervals") + ylab("Total test")




p5 <- filter(mdf, Location %in% c("澳門威尼斯人","工人體育場","澳門保安部隊高等學校","青洲坊活動中心")) %>%
      group_by(Location,ReservationTime) %>%
      summarise(value.sum = sum(value, na.rm = TRUE))
p6 <- filter(mdf, Location %in% c("澳門威尼斯人","工人體育場","澳門保安部隊高等學校","青洲坊活動中心")) %>%
      group_by(Location,ReservationTime) %>%
      summarise(ReservationPerStation = mean(ReservationPerStation, na.rm = TRUE))

g1 <- ggplot(p5,aes(x = ReservationTime, y = value.sum, color = Location)) +
geom_point() + geom_smooth(formula = "y ~ x", alpha = 0.3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d()
g2 <- ggplot(p6,aes(x = ReservationTime, y = ReservationPerStation, color = Location)) +
geom_point() + geom_smooth(formula = "y ~ x", alpha = 0.3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

grid.arrange(g1,g2,nrow=2)

p7 <- filter(mdf, Location %in% c("澳門威尼斯人","工人體育場","澳門保安部隊高等學校","青洲坊活動中心")) %>%
      group_by(Location,ReservationCalendarTime) %>%
      summarise(value.sum = sum(value, na.rm = TRUE))
p8 <- filter(mdf, Location %in% c("澳門威尼斯人","工人體育場","澳門保安部隊高等學校","青洲坊活動中心")) %>%
      group_by(Location,ReservationCalendarTime) %>%
      summarise(ReservationPerStation.mean = mean(ReservationPerStation, na.rm = TRUE))

g1 <- ggplot(p7,aes(x = ReservationCalendarTime, y = value.sum, color = Location)) +
geom_point() + geom_smooth(formula = "y ~ x", alpha = 0.3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d()
g2 <- ggplot(p8,aes(x = ReservationCalendarTime, y = ReservationPerStation.mean, color = Location)) +
geom_point() + geom_smooth(formula = "y ~ x", alpha = 0.3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d()

grid.arrange(g1,g2,nrow=2)
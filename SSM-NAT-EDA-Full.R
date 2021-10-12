
#install.packages("XML")
#install.packages("RSelenium")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages(c('R6', 'jsonlite'))
#install.packages(c('vscDebugger'))
#install.packages("reshape")
#install.packages("flux")
#install.packages('ggmap')
#install.packages('plotly')
#install.packages("GGally")

# this sets your google map for this session
# register_google(key = "AIzaSyD9z90fvzxmOhRzoNbxbwOmuIXI6CVKcTE")
# ggmap_hide_api_key()

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
library("GGally")



### Loop to ingest all scraped data
filelist <- dir('aptmon-scraping/20211004', full.names=TRUE)
scrp <- data.frame()
for (file in filelist) {
    filetimestr <- sub(".csv", "",sub(".*-", "", file))
    filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
    temp <- read.csv(file, na = "---")
    temp$DateTime <- as.POSIXlt(filetime)
    scrp <- rbind(scrp,as.data.frame(temp))
}

### Date and time transformation
attr(scrp$DateTime, "tzone") <- "GMT"
scrp$DateTimeRound <- round(scrp$DateTime, "30mins")
scrp$WaitingQueue <- as.numeric(as.character(sub("*人", "", scrp$輪候人數)))
scrp$WaitingMinutes <- as.numeric(as.character(sub("分鐘", "",sub(".*>", "", scrp$等候時間))))
scrp$StationCount <- rowSums(scrp[ ,c("口採樣點", "鼻採樣點")], na.rm=TRUE)
scrp$HourNumber <- 
sapply(strsplit(substr(scrp$DateTimeRound,12,16),":"),
  function(x) {
    x <- as.numeric(x)
    x[1]+x[2]/60
    }
)

#summary(scrp)
#tail(scrp)
#scrp[scrp$Location == "街坊會聯合總會社區服務大樓", ]

### Prepare for Location adding lon and lat
LonLat <- unique(scrp[c("Location")])
LonLat$MapLoc <- ifelse(LonLat$Location == "科大體育館","Macao, 澳門科技大學室內體育館 Gymnasium",LonLat$Location)
LonLat[grep(".*工人體育場", LonLat$Location, perl=T), ]$MapLoc <- "Macao, 工人體育場館"
LonLat[grep(".*1樓", LonLat$Location, perl=T), ]$MapLoc <- "望廈體育中心 Centro Desportivo Mong-Há"
LonLat <- mutate_geocode(LonLat, MapLoc)
LonLat$area <- ifelse(LonLat$lat>=22.17,'macao','cotai,macao')

#install.packages('revgeo')
#library(revgeo)
#revgeo(LonLat$lon, LonLat$lat, provider='google', API='API key here', output='hash', item = 'country')

# Missing DateTimeRound
# 2021-10-05 06:00:00
# 2021-10-05 08:30:00
# 2021-10-05 09:00:00


station <- scrp %>% group_by(序號,Location,類別,DateTimeRound,HourNumber) %>%
      summarise(
            StationCount.mean = mean(StationCount, na.rm = TRUE),
            StationCount.median = median(StationCount, na.rm = TRUE),
            口採樣點.mean = mean(口採樣點, na.rm = TRUE),
            鼻採樣點.mean = mean(鼻採樣點, na.rm = TRUE),
            口採樣點.median = median(口採樣點, na.rm = TRUE),
            鼻採樣點.median = median(鼻採樣點, na.rm = TRUE),
            WaitingQueue.mean = mean(WaitingQueue, na.rm = TRUE),
            WaitingMinutes.mean = mean(WaitingMinutes, na.rm = TRUE),
            WaitingQueue.median = median(WaitingQueue, na.rm = TRUE),
            WaitingMinutes.median = median(WaitingMinutes, na.rm = TRUE),
      ) %>% as.data.frame()

#str(station)
#nrow(station)
#station[station$Location == "奧林匹克體育中心-運動場-室內體育館", ]


ggplot(station,aes(x = DateTimeRound, y = WaitingMinutes.mean, color = 類別)) +
geom_line() +
#geom_point() + geom_smooth(formula = "y ~ x", alpha = 0.3) +
#scale_color_viridis_d(option = 'magma', alpha = .7) +
facet_wrap(~Location, nrow = 6, ncol = 8)

ggplot(station,aes(x = HourNumber, y = WaitingMinutes.mean, color = 類別)) +
geom_line() +
#geom_point() + geom_smooth(formula = "y ~ x", alpha = 0.3) +
#scale_color_viridis_d(option = 'magma', alpha = .7) +
facet_wrap(~Location, nrow = 6, ncol = 8)




# download.file('https://www.ssm.gov.mo/docs/stat/apt/RNA010.xlsx', 'RNA010.xlsx', method='curl' )


filelist2 <- dir('RNA010/20211004', full.names=TRUE)
file2 <- tail(filelist2, 1)

sheets <- c("20211004A","20211005A","20211006A","20211007A")
df <- data.frame()
for (sheetname in sheets) {
    #sheetname <- "20211004A"

    ### Extract first row for location list
    cnames <- read_excel(file2, sheet = sheetname, n_max = 0, na = "---") %>% names()
    lls1 <- sub(".*?-", "",cnames[seq(6, length(cnames), 3)])
    ### Extract data from 2nd row
    rdf1 <- read_excel(file2, sheet=sheetname, na = "---", skip = ifelse(sheetname == "20211004A", 2, 1)) # skip 2 because there exists a hidden row 1 in this spreadsheet
    ### Set date
    rdf1$TestDate <- as.Date(strptime(str_remove(sheetname, "A"),"%Y%m%d"))
    rdf1$TestTime <- substr(rdf1$預約時段,1,5)
    ### select columns and rows
    sdf1 <- rdf1 %>% select(c(6:ncol(rdf1))) %>% slice(2:nrow(rdf1)) %>% select(-contains("總人次"))
    ### Repeat Location info for number of rows
    Location <- rep(lls1, each = nrow(sdf1) * 2)
    ### Melt to pivot
    sdf1 <- as.data.frame(sdf1)
    mdf1 <- reshape::melt(sdf1, id = c("TestDate", "TestTime"))
    ### Combine Location with dataset
    df1 <- cbind(Location,mdf1)
    ### Clean away column names with ...
    df1$variable <- sub("\\....*", "", df1$variable)
    df <- rbind(df,as.data.frame(df1))
}

# filter(df, Location == "奧林匹克體育中心-運動場-室內體育館")

#tail(df)
pdf <- df %>% pivot_wider(names_from = variable, values_from = value)
pdf <- as.data.frame(pdf)
pdf$TestCount <- rowSums(pdf[ ,c("口咽拭", "鼻咽拭")], na.rm=TRUE)
pdf$DateTimeRound <- as.POSIXlt(paste(pdf$TestDate, pdf$TestTime))
attr(pdf$DateTimeRound, "tzone") <- "GMT"
pdf$HourNumber <- 
sapply(strsplit(pdf$TestTime,":"),
  function(x) {
    x <- as.numeric(x)
    x[1]+x[2]/60
    }
)

#tail(station)

#pctl <- mdf %>% mutate(TestPerStation.percentile = percent_rank(TestPerStation))

ldf <- merge(LonLat, pdf)
mdf <- merge(station, pdf, by = c("Location","DateTimeRound","HourNumber"))
mdf <- mdf %>%
mutate(TestPerStation = mdf$TestCount / ifelse(is.na(mdf$StationCount.mean) | mdf$StationCount.mean == 0, 1, mdf$StationCount.mean),
      TestPerStation.ntile = ntile(TestPerStation, 4), 
      MouthPerStation = mdf$口咽拭 / ifelse(is.na(mdf$口採樣點.mean) | mdf$口採樣點.mean == 0 , 1, mdf$口採樣點.mean),
      NosePerStation = mdf$鼻咽拭 / ifelse(is.na(mdf$鼻採樣點.mean) | mdf$鼻採樣點.mean == 0, 1, mdf$鼻採樣點.mean))
lldf <- merge(LonLat, mdf)


summary(mdf)
str(lldf)
nrow(station) # station data has rows up to end of day 2
nrow(pdf) # booking data has more rows after day 2
nrow(mdf)
nrow(lldf)
unique(station[c("Location")])
unique(pdf[c("Location")]) # booking data does not contain A/C type station

#fdf <- filter(mdf, ReservationDateTime <= Sys.time())
#filter(df, Location == "北安客運碼頭")
#filter(mdf, Location == "工人體育場")
#head(df)
#df[is.na(df)] <- 0


#1. 總預約數  
sum(pdf$TestCount,na.rm = TRUE)

#1. 口咽拭約為鼻咽拭的兩倍  
p1 <- df %>% group_by(variable) %>% summarise(value.sum = sum(value, na.rm = TRUE))
ggplot(data=p1, aes(x = variable, y = value.sum, fill = variable, alpha = .9)) +
  geom_bar(stat="identity") + coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  ggtitle("Total RNA test by sampling method") + xlab("Sampling method") + ylab("Total bookings")

#1. 澳門半島約為氹仔路環的兩倍半  
p1 <- ldf %>% group_by(area) %>% tally(TestCount)
ggplot(data=p1, aes(x = area, y = n, fill = area, alpha = .9)) +
  geom_bar(stat="identity") + coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  ggtitle("Total RNA test by area") + xlab("Macao / Cotai") + ylab("Total bookings")


#2. 取自B類採樣點的預約排名, 總數排名中雖威尼斯人排第一，氹仔僅佔三站點。看單日分佈預約均聚集於四日中第二日

top <- pdf %>% group_by(Location) %>% tally(TestCount) %>% top_n(10) # print(n=40)
merge(top, LonLat)

p2 <- pdf %>% group_by(Location, TestDate) %>% tally(TestCount)

ggplot(data=p2, mapping = aes(x = reorder(Location,n), n, fill = factor(TestDate), alpha = .9)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  ggtitle("Total RNA test by location") + xlab("Location") + ylab("Total bookings")


#3. 四分位的24時分佈

tilevalue <- c(max(filter(lldf, TestPerStation.ntile == 1)$TestPerStation),
max(filter(lldf, TestPerStation.ntile == 2)$TestPerStation),
max(filter(lldf, TestPerStation.ntile == 3)$TestPerStation),
max(filter(lldf, TestPerStation.ntile == 4)$TestPerStation))
#, labeller = labeller(setNames(tilevalue, sort(unique(lldf$TestPerStation.ntile))))

fw1 <- ggplot(lldf) + geom_point(aes(x = StationCount.mean, y = TestCount, color = factor(TestDate), alpha = .7)) +
scale_color_viridis_d(labels = sort(unique(lldf$TestDate)), option = 'magma', alpha = .7) +
theme_minimal() + facet_wrap(~TestPerStation.ntile, nrow = 4)
#guides(color = FALSE, alpha = FALSE)
fw2 <- ggplot(lldf, aes(x = HourNumber, color = factor(TestDate), fill = factor(TestDate))) +
geom_histogram(binwidth = 1, alpha = .5) +
geom_hline(linetype = "dotted", yintercept = 45, color = "#fc8961") +
scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
facet_wrap(~TestPerStation.ntile, nrow = 4) +
theme_minimal() #+ theme(legend.position = "none")
grid.arrange(fw1,fw2,ncol=2)

#4. 高峰聚集於第二日，24時內各採樣點測試數的動態分佈
p4 <- lldf %>% filter(TestPerStation.ntile == 4 & TestDate == '2021-10-05')
# Histogram, same result as bubble animation, for check only
ggplot(p4, aes(x = HourNumber, y = TestCount, color = TestCount, fill = TestCount)) +
geom_bar(stat = "identity", binwidth = 1, alpha = .5) +
scale_color_viridis_c(option = 'magma') + scale_fill_viridis_c(option = 'magma') +
facet_wrap(~Location)

# p4 %>% top_n(100)
# p4 %>% filter(Location == "澳門東亞運體育館A館")

plot <- ggmap(get_map(location = "taipa, macao", zoom = 12), darken = .5, 
base_layer = ggplot(data = p4, aes(x = lon, y = lat, frame = HourNumber, ids = Location))) +
geom_point(data = p4, aes(color = TestCount, size = TestCount, alpha = .5)) +
#geom_text(data = p4, aes(label=paste(Location, "平均等待", WaitingQueue.mean,"人")), color = 'white', size = 3, check_overlap = T) +
scale_size(range = c(0, 12)) +
scale_color_viridis_c(option = "magma")
ggplotly(plot)

#5. 散點圖中可以發現基於每測試站測試數的四分位，和隊伍每人等待時間成比。基本可以代表測試站壓力。
ggpairs(lldf[c("WaitingQueue.mean","WaitingMinutes.mean","TestCount","StationCount.mean")], aes(color = factor(lldf$TestPerStation.ntile), alpha = .3))
#scale_color_viridis_d(option = 'magma')

str(lldf)





cols <- character(nrow(lldf))
cols[] <- "#b73779"
cols[lldf$TestDate == "2021-10-04"] <- "#000004"
cols[lldf$TestDate == "2021-10-05"] <- "#51127c"
cols[lldf$TestDate == "2021-10-06"] <- "#fc8961"
cols[lldf$TestDate == "2021-10-07"] <- "#fcfdbf"
pairs(lldf[c("WaitingQueue.mean","WaitingMinutes.mean","TestCount","StationCount.mean")], main = "RNA Bookings", pch = 21, col = cols)



ggplot(lldf) +
geom_boxplot(aes(y = TestPerStation, color = Location))

ggplot(lldf) +
geom_boxplot(aes(y = TestPerStation, color = area))

lldf %>% summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            TestPerStation = quantile(TestPerStation, c(0.25, 0.5, 0.75)))

lldf %>% summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            TestPerStation = quantile(TestPerStation, c(0.25, 0.5, 0.75)))
q25 <- lldf %>% filter(TestPerStation <= 26.52632)
q50 <- lldf %>% filter(TestPerStation > 26.52632 & TestPerStation <= 33.42857)
q75 <- lldf %>% filter(TestPerStation > 33.42857 & TestPerStation <= 42)
q100 <- lldf %>% filter(TestPerStation > 42)

g25<- ggplot(q25) + geom_point(aes(x = StationCount.mean, y = TestCount, color = TestDate, alpha = .7)) +
xlim(min(lldf$StationCount.mean),max(lldf$StationCount.mean)) + ylim(min(lldf$TestCount),max(lldf$TestCount)) + 
scale_color_viridis_c(option = 'magma', alpha = .7)
g50<- ggplot(q50) + geom_point(aes(x = StationCount.mean, y = TestCount, color = TestDate, alpha = .7)) +
xlim(min(lldf$StationCount.mean),max(lldf$StationCount.mean)) + ylim(min(lldf$TestCount),max(lldf$TestCount)) + 
scale_color_viridis_c(option = 'magma', alpha = .7)
g75<- ggplot(q75) + geom_point(aes(x = StationCount.mean, y = TestCount, color = TestDate, alpha = .7)) +
xlim(min(lldf$StationCount.mean),max(lldf$StationCount.mean)) + ylim(min(lldf$TestCount),max(lldf$TestCount)) + 
scale_color_viridis_c(option = 'magma', alpha = .7)
g100<- ggplot(q100) + geom_point(aes(x = StationCount.mean, y = TestCount, color = TestDate, alpha = .7)) +
xlim(min(lldf$StationCount.mean),max(lldf$StationCount.mean)) + ylim(min(lldf$TestCount),max(lldf$TestCount)) + 
scale_color_viridis_c(option = 'magma', alpha = .7)

g25h<- ggplot(q25, aes(x = HourNumber, color = area, fill = area)) + geom_histogram(alpha = .5) + 
xlim(min(lldf$HourNumber),max(lldf$HourNumber)) + ylim(0,80) + 
scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma')
g50h<- ggplot(q50, aes(x = HourNumber, color = area, fill = area)) + geom_histogram(alpha = .5) + 
xlim(min(lldf$HourNumber),max(lldf$HourNumber)) + ylim(0,80) + 
scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma')
g75h<- ggplot(q75, aes(x = HourNumber, color = area, fill = area)) + geom_histogram(alpha = .5) + 
xlim(min(lldf$HourNumber),max(lldf$HourNumber)) + ylim(0,80) + 
scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma')
g100h<- ggplot(q75, aes(x = HourNumber, color = area, fill = area)) + geom_histogram(alpha = .5) + 
xlim(min(lldf$HourNumber),max(lldf$HourNumber)) + ylim(0,80) + 
scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma')
grid.arrange(g100,g100h,g75,g75h,g50,g50h,g25,g25h,ncol=2)





ggplot(mdf) +
geom_line(aes(x = DateTimeRound, y = TestPerStation, color = TestDate)) +
geom_line(aes(x = DateTimeRound, y = TestCount, color = TestDate)) +
geom_line(aes(x = DateTimeRound, y = StationCount.mean, color = TestDate)) +
facet_wrap(~Location, nrow = 6, ncol = 8) +
ggtitle("RNA test per sampling method by location") + xlab("All intervals") + ylab("Average test per station")



# install.packages('ggmap')
# this sets your google map for this session
# register_google(key = "AIzaSyD9z90fvzxmOhRzoNbxbwOmuIXI6CVKcTE")
# ggmap_hide_api_key()

#install.packages('plotly')
#install.packages('dash')

library(ggmap)
library(grid)
library(gridExtra)
library(dplyr)
library(reshape)
library(ggplot2)
library(stringr)
library(plotly)


paths <- dir('aptmon-scraping/', full.names=TRUE)
locationfilename <- tail(paths, 1)
filetimestr <- sub(".csv", "",sub(".*-", "", locationfilename))
filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
filectime <- tail(file.info(paths)$ctime, 1)

scrp <- read.csv(locationfilename, na = "---")
scrp$MapLoc <- ifelse(scrp$Location == "科大體育館","Macao, 澳門科技大學室內體育館 Gymnasium",scrp$Location)
scrp[grep(".*工人體育場", scrp$Location, perl=T), ]$MapLoc <- "Macao, 工人體育場館"
scrp[grep(".*1樓", scrp$Location, perl=T), ]$MapLoc <- "望廈體育中心 Centro Desportivo Mong-Há"
scrp$DateTime <- as.POSIXct(filetime)
scrp$WaitingQueue <- as.numeric(as.character(sub("*人", "", scrp$輪候人數)))
scrp$WaitingMinutes <- as.numeric(as.character(sub("分鐘", "",sub(".*>", "", scrp$等候時間))))
LonLat <- mutate_geocode(scrp, MapLoc)
LonLat$area <- ifelse(LonLat$lat>=22.17,'macao','cotai,macao')


plot <- ggmap(get_map(location = "taipa, macao", zoom = 12), darken = .5, 
base_layer = ggplot(data = LonLat, aes(x = lon, y = lat, label=paste(Location,WaitingQueue,"人")))) +
geom_point(aes(colour = 類別, size = WaitingMinutes, alpha = .2), data = LonLat) +
geom_text(colour = 'white', size = 4, check_overlap = T) +
scale_size(range = c(0, 12), trans="reverse") +
scale_color_viridis_d(option = "magma")

#title <- paste("Macau 3rd Citywide NAT - waiting minutes as of",filectime)

ggplotly(plot)

# macao <- filter(LonLat, area == 'macao')
# plot1 <- ggmap(get_map(location = "macao", zoom = 14), darken = .5, legend = "topleft", 
# base_layer = ggplot(data = macao, aes(x = lon, y = lat, label=paste(Location,WaitingQueue,"人")))) +
# geom_point(aes(colour = 類別, size = WaitingMinutes, alpha = .2), data = macao) +
# geom_text(colour = 'white', size = 4, check_overlap = T) +
# scale_size(range = c(0, 18), trans="reverse") +
# scale_color_viridis_d(option = "magma")

# cotai <- filter(LonLat, area == 'cotai,macao')
# plot2 <- ggmap(get_map(location = "cotai,macao", zoom = 14), darken = .5, legend = "topleft",
# base_layer = ggplot(data = cotai, aes(x = lon, y = lat, label=paste(Location,WaitingQueue,"人")))) +
# geom_point(aes(colour = 類別, size = WaitingMinutes, alpha = .2), data = cotai) +
# geom_text(colour = 'white', size = 4, check_overlap = T) +
# scale_size(range = c(0, 18), trans="reverse") +
# scale_color_viridis_d(option = "magma")


# png(file = paste("NAT-Oct-ggmap ",filetimestr,".png",sep=""), width = 700, height = 1400)
# title <- paste("Macau 3rd Citywide NAT - waiting minutes as of",filectime)
# grid.arrange(plot1, plot2, nrow = 2, ncol = 1, top = textGrob(title))
# dev.off()



library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
    htmlDiv(
        list(
            dccGraph(figure=plot) 
        )
     )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)
library(rvest)
library(data.table)
library(httr)
library(XML)
library(RSelenium)
library(netstat)
library(httpuv)

port <- randomPort(min = 1024L, max = 49151L, host = "127.0.0.1", n = 20)
mybrowser <- rsDriver(browser = 'firefox', verbose = TRUE, port = port)
Sys.sleep(10)

link <- "https://eservice.ssm.gov.mo/aptmon/aptmon/ch"
mybrowser$client$navigate(link)
Sys.sleep(60)

#mybrowser$client$findElement(using = 'id', "tblist")$getElementText()
html.table.0 <-  mybrowser$client$findElement(using = 'id', "tblist")
webElem5txt.0 <- html.table.0$getElementAttribute("outerHTML")[[1]]

#read_html(webElem5txt.0) %>% html_nodes('div.hidden-xs') %>% html_text(trim = TRUE)
df.table.0 <-  read_html(webElem5txt.0) %>% html_table() %>% data.frame(.)
Sys.sleep(5)

df.table.0$Location <- read_html(webElem5txt.0) %>% html_nodes('div.hidden-xs') %>% html_text(trim = TRUE)
station <- df.table.0

#head(station)
#summary(station)

Sys.sleep(10)

write.csv(station,paste("D:/Documents/GitHub/ssm-rna-test/aptmon-scraping/station-",format(Sys.time(), "%Y%m%d%H%M%S"),".csv", sep = ""), row.names = TRUE)

Sys.sleep(10)

# to close the browser
mybrowser$client$close()
# to close the server
mybrowser$server$stop()

#rm(mybrowser)
#kill = 'for /f "tokens=5" %a in (\'netstat -aon ^| findstr ":4548" ^| findstr "LISTENING"\') do taskkill /f /pid %a'
#shell(kill, ignore.stderr = TRUE, ignore.stdout = TRUE)

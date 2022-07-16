---
title: "An Exploratory Data Analysis of Macau's 3rd city-wide Nucleic Acid Testing"
date: "2021-12-03"
author:
- name: Adam Zheng
output: 
  html_document:
    keep_md: true
    css: ../bootstrap3/assets/css/bootstrap.min.css
---





## Background  

Under COVID-19 pandemic in 2021, when new imported cases is reported, Macau SAR will launch one round of city-wide nucleic tests to prevent the corona-virus from spreading to local communities.  
This is an exploratory data analysis of Macau's 3rd time city-wide NAT over the period of October 4th 9pm - 7th 9pm.  
This time Macau Government and the Health Bureau has shorten the overall period, targeting to complete process for over 600k citizens within 48 hours.  
(There were 8 locations kept swab station working as usual for continuing after the first 48 hour)  

The questions we seek here are:  
- What time is the most busy time during day?  
- If any location is over stressed or too free?  
- If overall performance is well balanced among each location?  


## Data Sources  
  
1. [City-wide Nucleic Acid Test Booking Stats](https://www.ssm.gov.mo/docs/stat/apt/RNA010.xlsx)  
As the sources claims the "Number of booking will only be counted when it is valid",
therefore the number is treated as the fact number of swab done in our analysis.  
A backup of the data source is available upon requirement:  
[https://github.com/adamzerg/SSM-RNA-Test/blob/main/RNA010/20211004.7z]  

![PreventCovid-image-tag](../../image/PreventCOVID-19.PNG)  

2. [Special webpage against Epidemics - Near real-time waiting stats at each swab station location](https://eservice.ssm.gov.mo/aptmon/aptmon/ch)  
The source is refreshed about every 10 minutes, source including number of swab stations by nasal / mouth, locations by category, waiting time.  
A backup of the data source is available upon requirement:  
[https://github.com/adamzerg/SSM-RNA-Test/blob/main/aptmon-scraping/20211004.7z]

![Station-image-tag](../../image/aptmon.PNG)  

3. Expected Formula  
Load balance at one location =  
  Source 1：Number of Swab per 30 minutes / Source 2: (Nasal Swab Desks + Mouth Swab Desks)  


## Data Prep  

1. Source 1 is Excel, preparation including  
- Select the latest record only  
- Set a loop to extract information from the specific 4 spreadsheets  
- Pivot for nasal / mouth swab counts based on granularity time (every 30 minutes)  

![RNA010-image-tag](../../image/RNA010.PNG)  






2. Source 2 is web scraped by setting schedule, preparation including  
- Batch load in once for all scraped histories  
- Add for time format for convenience  






## Data Wrangling  

1. Since Source 2 are scraped from near real time, need to:  
- Deduplicate for 30 minutes duration, and determine count / measures with their median value  
- Backfill 3 missing timeframe at Oct-05 6am / 8:30 am / 9am  



2. Extract geolocation information  



3. Merge two sources into one, only category B remains after merged  




## Preliminary Analysis   

1. Fact Ratios  

- Total bookings is 627,865 from source 1, i.e. Category B (General Station) only  



- The mouth swab total is about 2.3 times than nasal swab  
- Total swab in Macau peninsula is about 2.8 times than Taipa plus Coloane  

![](SSM-NAT-EDA-Oct-EN_files/figure-html/base-ratio-1.png)<!-- -->

2. When time is the most busiest time in a day?  

- Set quartile value based on number of swab per station  



- The 1st tile has most count overnight, and the 2nd and 3rd mostly appears by daytime  
- Note for the 4th tile, 3 peaks goes to 7am / 1pm / 8pm, each appears for more than 45 times  
- The 3 peak times matched to people's peak working hour during a day  
- Should consider to increase number of stations / swab desks for the specific time frame  

![](SSM-NAT-EDA-Oct-EN_files/figure-html/quartile-24hour-1.png)<!-- -->

3. Location stats  

- From total bookings top 5, there are Venetian Cotai Arena, Workers Stadium, Keang Peng Middle School, Kiang Wu Hospital Auditorium, Macao Cultural Centre  
- The distribution appears mostly tend to day 1  

![](SSM-NAT-EDA-Oct-EN_files/figure-html/daily-booking-ranking-1.png)<!-- -->

- Number of swab desks varies  
- Outlier can be seen for Worker Stadium, once increased from 10 to 23  
- The adjustment down at Keang Peng Middle School, Kiang Wu Hospital Auditorium  

![](SSM-NAT-EDA-Oct-EN_files/figure-html/avgdesk-value-1.png)<!-- -->

4. Station load balance  



- Sort heatmap on the 5th tile swab count proportion by number of desks  
- The 5tile is set against each location, to number of desks  
- Why for taipa most swab were done at the 5th tile? e.g. Pac On Ferry Terminal, Venetian Cotai Arena, East Asian Games Dome  

![](SSM-NAT-EDA-Oct-EN_files/figure-html/prop-heatmap-1.png)<!-- -->



- Preview set of 4 stations in Macau over the complete 48 hour (some runs over 48 hr)  
- At Mong-Ha Sports Centre, Keang Peng Middle School, Worker Stadium, no matter which swab method there are increase and decrease adjustment in swab desk along with swab counts  
- Macau Forum however has fluctuated swab counts, but rarely has any adjustment in swab desks  

![](SSM-NAT-EDA-Oct-EN_files/figure-html/explore-location-set1-1.png)<!-- -->

- Preview set of 4 stations in Taipa over the complete 48 hour (some runs over 48 hr)  
- There are darker and lighter intervals one after each in number of swab desks vs swab done, means they were not in a consist ratio  
- Some spikes appears for Venetian Cotai Arena and East Asian Games Dome but in different time frame, could consider supporting each other in due time  
- The 2nd day overall has less stressed than the 1st in Taipa, which explained why the distribution appears mostly tend to be in day 1, should consider for supporting Macau, or make redirection for the crowd go Taipa by day 2  

![](SSM-NAT-EDA-Oct-EN_files/figure-html/explore-location-set2-1.png)<!-- -->

5. Map  

- An animation for the 4th quartile swab per desk to each location within 48 hours  
- Bubble size representing swab per desk  
- Reflecting well the peak time by the 10th / 16th hour (Oct-06 7am / 1pm)  
- The 31st -36th hour crowd appears only in Macau peninsula (Oct-07 4am - 9am)  
- A notable crowd appears right by the 47th hour (Oct-07 8pm)  


```{=html}
<div id="htmlwidget-f6df337334cbdb429060" style="width:672px;height:480px;" class="plotly html-widget"></div>
```


By Venetian Cotai Arena, 2nd day is less stressed comparing to the 1st day  

![Venetian-image-tag](../../image/20211006_124845.jpg)  

There are about 20 swab desks, by 2nd day noon only 5-6 is operating and there is nearly no waiting queue  

![Venetian-image2-tag](../../image/20211006_124848.jpg)  


## Summary  
- We can use Swab per desk by every 30 minutes as a measure to review the load balance to each station, ideally this number is under 33  
- Potential improvement can be made, if we can increase the swab desks in station during peak rush hours  
- Relocating more / less staff depending on the next hour booking numbers  
- Limit for number of bookings for an upcoming time in case if shortage, also avoid relocation of staffs  


## Reference
[8個常規核酸檢測站在全民核酸檢測結束後將重新開放並延長服務時間](https://www.ssm.gov.mo/docs/20257/20257_113648d57b4a4aa3b1cc4265ec112902_000.pdf)  
[今(4)日晚上9時至10月7日晚上9時進行第三次全民核酸檢測](https://www.ssm.gov.mo/docs/20291/20291_f2153a77511c40619660cc7c7764a661_000.pdf)  
[第 3 次較第 2 次全民核檢首 3 小時採樣人數多 各個採樣站點的輪候情況理想](https://www.ssm.gov.mo/docs/file/20318/)  


![qr-tag](../../image/qr.jpg)  


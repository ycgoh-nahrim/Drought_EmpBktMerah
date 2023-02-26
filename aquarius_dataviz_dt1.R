
###########################
#DATA TESTING


# load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(scattermore)
library(ggrepel)
library(plotly)
library(scales)
library(ggpubr) # ggarrange
library(gghighlight)


library(ggforce)

library(extrafont)
library(openxlsx)

library(viridis)


# INPUT
area_name <- "Bukit Merah"
a_name <- "BktMerah"
cnt_yr <- 347


#from rainfall database
rain_dt <- fread(file="J:/Backup_main/2022/20220607_Bkt_Merah_drought/Data/Aq_RF_1d_Perak_raw.csv",
                 header = TRUE, sep=",", stringsAsFactors = F)

#rain_dt <- df


#check field name, type, format
head(rain_dt, 3)

str(rain_dt)



# check values

#remove duplicates
raindata <- unique(rain_dt)
str(raindata)

stn_list <- data.frame(unique(raindata$Stn_no))

# add status column for incomplete data
#trial <- raindata[Stn_no == 2615131]
#trial <- raindata[Depth == "-999"]
#trial[, status:= fifelse(Depth == "-999", 0, fifelse(str_detect(Depth, "\\?"), 40, 100))]

raindata[, Complete := fifelse(Depth == "-999", 0, fifelse(str_detect(Depth, "\\?"), 40, 100))]

#replace value '-999' with NA
raindata[raindata == "-999"] <- NA

#replace ? with blank
raindata$Depth = gsub("\\?", "", raindata$Depth)

negative_values <- raindata[Depth < 0]


# format date

raindata$Datetime <- as.POSIXct(ymd_hms(raindata$Datetime), format = "%Y-%m-%d %H:%M:%S")
#raindata$Datetime <- as.POSIXct(raindata$Datetime, format = "%d/%m/%Y %H:%M:%S")
#raindata$Datetime <- as.Date(raindata$Datetime, format = "%Y-%m-%d %H:%M:%S")

na_dt <- raindata[is.na(Datetime)]

## set row ID to check NA dates later
#raindata[, ID := .I] 
#rain_dt[, ID := .I]

## filter to check original dates

ori_dt_check <- rain_dt[ID %in% na_dt$ID]

## replace date string
setDT(ori_dt_check)[, Datetime := str_replace(Datetime, "31/12/1981 24:00:00", "1982-01-01 00:00:00")]
ori_dt_check$Datetime <- as.POSIXct(dmy_hms(ori_dt_check$Datetime), format = "%d/%m/%Y %H:%M:%S")

dmy_hms(ori_dt_check$Datetime)

str(raindata)



#format column from character to numeric
raindata$Depth <- as.numeric(as.character(raindata$Depth))

#format column from numeric to character  
raindata$Stn_no <- as.integer(raindata$Stn_no)


#data summary for checking
max_depth <- raindata[, .(max= max(Depth, na.rm = T)), by = Stn_no]


### checking
test <- raindata[Stn_no == 4907422]
test <- raindata[Stn_no == 5007421]

### filter data
raindata <- raindata[Stn_no != 4907422]
raindata <- raindata[Stn_no != 5007421]


#####################
#write to csv
write.csv(raindata,"J:/Backup_main/2022/20220607_Bkt_Merah_drought/Data/Aq_RF_1d_Perak_clean1.csv",
          row.names = FALSE, quote = FALSE)


###########################
# DATA AGGREGATION

# load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(scattermore)
library(ggrepel)
library(plotly)
library(scales)


# from rainfall database
rain_dt <- fread(file="J:/Backup_main/2022/20220607_Bkt_Merah_drought/Data/Aq_RF_1d_Perak_clean1.csv",
                 header = TRUE, sep=",", stringsAsFactors = F)
raindata <- rain_dt


str(rain_dt)


# list stations
stn_list <- data.frame(unique(rain_dt$Stn_no))


# add selected stations data (HP1 2021 KL Selangor stations)
#stn_dt <- fread(file="J:/Backup_main/2022/20220330_Statistik_hujan/Analysis/TIDEDA/HP1_2021_stn_list_KL-Sel.csv",
#                header = TRUE, sep=",", stringsAsFactors = F)


## join rainfall database with selected stations only
#raindata <- rain_dt[stn_dt, on = "Stn_no"]

## delete columns
str(raindata)
#raindata[, ':=' (No = NULL, Stn_name = NULL)]



## format date
#raindata$Datetime <- as.POSIXct(ymd_hms(raindata$Datetime), format = "%Y-%m-%d %H:%M:%S")
#raindata$Datetime <- as.Date(raindata$Datetime, format = "%Y-%m-%d") # failed to get year()
raindata$Datetime <- as.POSIXct(raindata$Datetime, format = "%Y-%m-%d")

## change column names
colnames(raindata) <- c("Date", "Stn_no", "Depth")
str(raindata)

## check datetime
na_dt <- raindata[is.na(Date)]



## add Date column
#setDT(raindata)[, Date:= date(Datetime) ]



###data summary for checking
raindata_max <- raindata[, .(max= max(Depth, na.rm = T)), by = .(Stn_no, year(Date))]
max(raindata_max$max)


## find outliers (>200mm/hr)
raindata_100 <- raindata[Depth > 100]

#write to csv
write.csv(raindata_100,paste0("Aq_RF_1d_", a_name, "_100.csv"),
          row.names = FALSE, quote = FALSE)


# calculate the sum precipitation for each day

#raindata_day <- raindata[, .(Depth_day = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))),
#                           by = c("Stn_no", "Date")]



###########################
# DATA EXPLORATION

#RECORD HEATMAP (count daily data by year)

## annual count

### add year column
raindata2 <- setDT(raindata)[, Year:= year(Date) ]
raindata2$Year <- as.numeric(as.character(raindata2$Year))

RF_day_yr <- raindata2[,.(Depth_yr = sum(Depth, na.rm = T), 
                     cnt = sum(!is.na(Depth))), 
                 by = c("Stn_no", "Year")]

max(RF_day_yr$cnt) #wrong if more than 366 

gg_RF_cnt_day_matrix <- RF_day_yr %>% 
  ggplot(aes(x = Year, y = factor(Stn_no), fill = cnt)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       breaks = c(0, 100, 200, 300, 366), 
                       labels = c(0, 100, 200, 300, 366), 
                       limits = c(0, 366),
                       name = "Annual count") +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station no."
                   #breaks = seq(1, 12, by = 1), 
                   #minor_breaks = NULL, 
                   #expand = c(0, 0)
                   #labels = "Stn_no"
  ) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(1970, 2022, by = 5),
                     minor_breaks = NULL) +
  theme(text=element_text(family = "Roboto", size = 6,
                          color = "grey20"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        #legend.text = element_text(size = 5),
        #legend.margin = margin(l = -70),
        legend.position="top") +
  labs(title = "Daily Rainfall Data (Aquarius) Availability",
       subtitle = paste0(area_name, " stations")) +
  guides(fill = guide_colorbar(label.position = "top", title.hjust = .6, 
                               barwidth = unit(10, "lines"), barheight = unit(.5, "lines")))
#coord_cartesian(expand = F)
#coord_fixed(ratio = 1.5)

gg_RF_cnt_day_matrix

#print last plot to file
ggsave(paste0("Aq_RF_1d_", a_name, "_annual_cnt.jpg"), dpi = 300,
       width = 12, height = 6, units = "in")






###########################
# VALUES

# daily data

## set start of x-axis
lims <- c(as_date('2000-01-01'), 
          as_date('2023-01-01'))

## set date format
raindata2$Date <- as.Date(as.character(raindata2$Date), format = "%Y-%m-%d")
str(raindata2)


gg_rainfall <- raindata2 %>% 
  #filter(RF_stn != "Tiada Data") %>%  # filter if got extreme outliers
  ggplot(aes(x = Date, y = Depth)) +
  #geom_point(aes(shape = ".", alpha = 0.5, color = Stn_no), na.rm = T) +
  geom_scattermore(pointsize = 0, na.rm = T, alpha = 0.8, color = "steelblue") +
  #geom_text_repel(aes(label = ifelse(Depth > 750, Stn_no, "")), 
  #                segment.size  = 0.2,
  #                max.overlaps = 20, size = 2) +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Year", date_labels = "%Y",
               #date_breaks = "5 year",
               breaks = seq(as.Date("2000-01-01"), as.Date("2023-01-01"), by = "2 year"),
               limits = lims,
               minor_breaks = NULL) +
  scale_y_continuous(name= paste("Daily Rainfall (mm)"),
                     breaks = seq(0, 200, by = 20),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Daily Rainfall (Aquarius)",
       subtitle = paste0(area_name, " stations"))

gg_rainfall

#print last plot to file

ggsave(paste0("Aq_RF_1d_", a_name, "_scatter.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


###########################
# CUMMULATIVE ANNUAL RAINFALL


# check annual rainfall values

## get years with complete* data except 2022 for station 5006021
raindata_yr_full <- RF_day_yr[cnt > cnt_yr]
raindata_yr_full <- RF_day_yr[cnt > cnt_yr | Year == 2022 & Stn_no = 5006021]
raindata_yr_full <- RF_day_yr[cnt > cnt_yr | Year == 2022 & cnt != 0]


## list stations with complete data
stn_list2 <- data.frame(unique(raindata_yr_full$Stn_no))

### write to csv
write.csv(stn_list2, paste0("Aq_RF_", a_name, "_stn_list_sel.csv"),
          row.names = FALSE, quote = FALSE)


## plot

gg_rainfall_yr <- raindata_yr_full %>% 
  ggplot(aes(x = Year, y = Depth_yr)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = Stn_no), na.rm = T) +
  geom_text_repel(aes(label = ifelse(Depth_yr > 3500 | Depth_yr < 1000, 
                                     paste0(Stn_no, " (", Year, ")"), 
                                     "")), 
                  segment.size  = 0.2,
                  max.overlaps = 20, size = 2) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                   #expand = c(0, 0),
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual Rainfall (mm)"),
                     breaks = seq(0, 6000, by = 500),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Annual Rainfall (Aquarius)",
       subtitle = paste0(area_name, " stations"))

gg_rainfall_yr

### print last plot to file

ggsave(paste0("Aq_RF_", a_name, "_annual_notclean.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall_yr, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


## clean annual rainfall by filter

### visualize problematic data

prob_stn <- 5006021

raindata_yr_full_test <- raindata[Stn_no == prob_stn & Year == 2022]

gg_test <- raindata_yr_full_test %>% 
  ggplot(aes(x = Date, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5), color = "steelblue", na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Date", date_labels = "%b",
               date_breaks = "1 month",
               #expand = c(0, 0),
               minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Daily Rainfall (mm)"),
                     #breaks = seq(0, 600, by = 100),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Daily Rainfall (Aquarius)",
       subtitle = paste0("Perak stations - ", prob_stn))

gg_test

### print last plot to file
ggsave(paste0("Aq_RF_1d_5006021_y2022.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")


### visualize problematic data 2
raindata_yr_full_test2 <- raindata[Stn_no == prob_stn]

#### create fake date column to plot with same year
raindata_yr_full_test2[, fake_date := paste0('2000-', month(Date), '-', day(Date))]

#### format date
str(raindata_yr_full_test2)
raindata_yr_full_test2$fake_date <- as.Date(raindata_yr_full_test2$fake_date, format = "%Y-%m-%d")

gg_test2 <- raindata_yr_full_test2 %>% 
  ggplot(aes(x = fake_date, y = Depth, color = factor(Year))) +
  geom_point(aes(shape = ".", alpha = 0.5), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Datetime", date_labels = "%b",
                   date_breaks = "1 month",
                   #expand = c(0, 0),
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Hourly Rainfall (mm)"),
                     breaks = seq(0, 200, by = 20),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Daily Rainfall (Aquarius)",
       subtitle = paste0("Perak stations - ", prob_stn))

gg_test2

##### only 1 year has abnormally high values compared to other years, 
##### therefore remove this year (this station) from analysis


##### check data
raindata_yr_2021 <- raindata_yr_full[Year == 2021]


# clean data
#raindata_yr_full_clean <- raindata_yr_full[Depth_yr %between% c(1000, 5000)]
raindata_yr_full_clean <- raindata_yr_full[Depth_yr > 0]

# remove data based on stations & year with >200 mm/hr multiple times in that year
#raindata_yr_full_clean2 <- raindata_yr_full_clean1[Stn_no != 2818110 & Year != 2018]


### plot

gg_rainfall_yr2 <- raindata_yr_full_clean %>% 
  ggplot(aes(x = Year, y = Depth_yr)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = as.factor(Stn_no)), na.rm = T) +
  #geom_text_repel(aes(label = ifelse(Depth_yr > 5000 | Depth_yr < 1000, Stn_no, "")), 
  #                segment.size  = 0.2, max.overlaps = 20, size = 2) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                     #expand = c(0, 0),
                     breaks = seq(2000, 2022, by = 2),
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual Rainfall (mm)"),
                     breaks = seq(0, 5000, by = 500),
                     limits = c(0, 5000),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Annual Rainfall (Aquarius)",
       subtitle = paste0(area_name, " stations"),
       color = "Stn no") +
  guides(alpha = "none", shape = "none")

gg_rainfall_yr2


### print last plot to file

ggsave(paste0("Aq_RF_", a_name, "_annual_clean.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")



###########################
# SUM CUMULATIVE ANNUAL RAINFALL


## join tables for years with complete data
raindata_day_yr_full <- raindata2[raindata_yr_full_clean, on = c("Stn_no", "Year")]

## delete columns
str(raindata_day_yr_full)
raindata_day_yr_full[, ':=' (cnt = NULL, Depth_yr = NULL)]

## cum sum
raindata_day_cumsum <- raindata_day_yr_full[, Depth_cum := cumsum(Depth), by = .(Stn_no, Year)]

## create fake date column to plot with same year
raindata_day_cumsum[, fake_date := paste0('2000-', month(Date), '-', day(Date))]

## format date
str(raindata_day_cumsum)
raindata_day_cumsum$fake_date <- as.Date(raindata_day_cumsum$fake_date, format = "%Y-%m-%d")


#write to csv daily data with full years
write.csv(raindata_day_yr_full, paste0("Aq_RF_1d_", a_name, "_yr_full.csv"),
          row.names = FALSE, quote = FALSE)



## plot # or don't plot

gg_rf_cumsum_ln <- raindata_day_cumsum %>% 
  ggplot(aes(x = fake_date, y = Depth_cum, color = as.character(Stn_no), #group = 1,
             text = paste0("Station no: ", Stn_no)
  )
  ) +
  geom_line(size = 0.25, alpha = 0.5, na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Year", date_labels = "%b", date_breaks = "month", minor_breaks = NULL) + #x axis format
  #scale_x_continuous(name= "Year", 
                     #breaks = seq(0, max(Rain_sum_yr_cum$num, na.rm = T), by = 5000), 
  #                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Cummulative annual rainfall (mm)"),
                     #labels = comma,
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color="grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(title = "Cummulative annual rainfall (Aquarius)",
       subtitle = "Perak stations")

gg_rf_cumsum_ln


### print last plot to file

ggsave(paste0("Aq_RF_1d_Prk_annual_cumsum_try1.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")




## find percentiles


pct_rf_day <- as.data.frame(raindata_day_cumsum) %>% 
  group_by(fake_date) %>% 
  summarise(min = min(Depth_cum, na.rm = T),
            q01 = quantile(Depth_cum, probs = .01, na.rm = T),
            q05 = quantile(Depth_cum, probs = .05, na.rm = T),
            q10 = quantile(Depth_cum, probs = .1, na.rm = T),
            q50 = quantile(Depth_cum, probs = .5, na.rm = T),
            q90 = quantile(Depth_cum, probs = .9, na.rm = T),
            q95 = quantile(Depth_cum, probs = .95, na.rm = T),
            q99 = quantile(Depth_cum, probs = .99, na.rm = T),
            max = max(Depth_cum, na.rm = T))


### find 2022 rainfall
sel_yr <- 2022
sel_stn <- 5006021
rf_day_2022_avg <- as.data.frame(raindata_day_cumsum) %>% 
  filter(Year == sel_yr & month(Date) < 7) %>%  # remove weird Dec data
  group_by(fake_date) %>%
  summarise(r2022_avg = mean(Depth_cum))

rf_day_2022_sel <- as.data.frame(raindata_day_cumsum) %>% 
  filter(Year == sel_yr & Stn_no == sel_stn & month(Date) < 7) %>% 
  group_by(fake_date) %>%
  summarise(r2022 = Depth_cum)

#### combine df
rf_day_2022 <- merge(rf_day_2022_avg, rf_day_2022_sel, by = "fake_date")


### join percentile with 2022 rainfall 

## fill missing dates, NA for year 2022
rf_day_2022 <- rf_day_2022 %>%
  ungroup %>% 
  complete(fake_date = seq.Date(min(fake_date), as.Date("2000-12-31"), by = "day"))

pct_rf_day_2022 <- merge(x = pct_rf_day, y = rf_day_2022, by = "fake_date")



## plot 

gg_rf_cumsum <- pct_rf_day_2022 %>% 
  ggplot(aes(x = fake_date, #y = Depth, color = as.character(Quan) #group = 1,
             #text = paste0("Station no: ", Stn_no)
  )
  ) +
  geom_line(aes(y = max), color = "steelblue3", linetype = "dashed", size = 0.5, alpha = 1, na.rm = T) +
  geom_line(aes(y = min), color = "steelblue3", linetype = "dashed", size = 0.5, alpha = 1, na.rm = T) +
  geom_ribbon(aes(ymin = q01, ymax = q99), fill = "lightskyblue", alpha = 0.6) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue2", alpha = 0.6) +
  geom_ribbon(aes(ymin = q10, ymax = q90), fill = "steelblue3", alpha = 0.6) +
  geom_line(aes(y = q50), color = "white", size = 0.5, alpha = 1, na.rm = T) +
  geom_line(aes(y = r2022), color = "black", size = 1, alpha = 1, na.rm = T) +
  geom_line(aes(y = r2022_avg), color = "grey30", size = 1, alpha = 1, na.rm = T) +
  annotate(geom = "text", x = as.Date("2000-12-01"), y = 2300, label = "Median", 
           hjust = "left", color = "white") +
  annotate(geom = "text", x = as.Date("2000-09-01"), y = 3800, label = "Historical maximum", 
           hjust = "left", color = "steelblue3") +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 700, label = "Historical minimum", 
           hjust = "left", color = "steelblue3") +
  annotate(geom = "text", x = as.Date("2000-06-10"), y = 300, 
           label = paste0(sel_yr, " (", sel_stn, " Kolam Air Bkt Merah)"), 
           hjust = "left", color = "black") +
  annotate(geom = "text", x = as.Date("2000-06-10"), y = 800, 
           label = paste0("Average ", sel_yr), 
           hjust = "left", color = "grey30") +
  geom_segment(aes(x = as.Date("2000-02-01"), y = 4000, xend = as.Date("2000-03-01"), yend = 4000), 
               size = 3, color = "lightskyblue") +
  annotate(geom = "text", x = as.Date("2000-03-05"), y = 4000, label = "1 to 99 percentiles", 
           hjust = "left", color = "grey40") +
  geom_segment(aes(x = as.Date("2000-02-01"), y = 3800, xend = as.Date("2000-03-01"), yend = 3800), 
               size = 3, color = "steelblue2") +
  annotate(geom = "text", x = as.Date("2000-03-05"), y = 3800, label = "5 to 95 percentiles", 
           hjust = "left", color = "grey40") +
  geom_segment(aes(x = as.Date("2000-02-01"), y = 3600, xend = as.Date("2000-03-01"), yend = 3600), 
               size = 3, color = "steelblue3") +
  annotate(geom = "text", x = as.Date("2000-03-05"), y = 3600, label = "10 to 90 percentiles", 
           hjust = "left", color = "grey40") +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Month", date_labels = "%b", date_breaks = "month", 
               expand = c(0, 0),
               #limits = c(as.Date("2000-01-01"), as.Date("2000-12-31")),
               minor_breaks = NULL) + #x axis format
  #scale_x_continuous(name= "Year", 
  #breaks = seq(0, max(Rain_sum_yr_cum$num, na.rm = T), by = 5000), 
  #                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Cummulative rainfall (mm)"),
                     breaks = seq(0, 5000, by = 500),
                     labels = comma,
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color="grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(title = "Cummulative rainfall (Aquarius) (2000-2020)",
       subtitle = paste0(area_name, " stations"))

gg_rf_cumsum


### print last plot to file

ggsave(paste0("Aq_RF_", a_name, "_annual_cumsum_", sel_stn, "-", sel_yr, ".jpg"), dpi = 300,
       width = 10, height = 5, units = "in")



#write to csv
write.csv(pct_rf_day_2022,paste0("Aq_RF_1d_", a_name, "_annual_cumsum.csv"),
          row.names = FALSE, quote = FALSE)



###########################
#   ROLLING AVERAGE OF ANNUAL RAINFALL

# packages for rolling mean
library(zoo)

## rolling mean for 5 years, ignore missing years
raindata_yr_rm <- raindata_yr_full_clean[, 
                                         Depth_rm := rollapplyr(Depth_yr, FUN = mean, width = 10, 
                                                                align = "right", partial = TRUE), 
                                         by = Stn_no]

##### write to csv
write.csv(raindata_yr_rm, paste0("Aq_RF_", a_name, "rm_10yr.csv"))


## plot

gg_rf_rm <- raindata_yr_rm %>% 
  ggplot(aes(x = Year, y = Depth_rm, color = as.character(Stn_no), #group = 1,
             text = paste0("Station no: ", Stn_no))
         ) +
  geom_line(size = 0.25, alpha = 1, na.rm = T) +
  geom_point(size = 0.75) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                     breaks = seq(1970, 2020, by = 5), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual rainfall (mm)"),
                     #labels = comma,
                     breaks = seq(0, 4000, by = 500), 
                     limits = c(0, 4000),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color="grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0.5),
        legend.position = "right") +
  labs(title = "10-year moving average annual rainfall (Aquarius)",
       subtitle = paste0(area_name, " stations"),
       color = "Stn no")

gg_rf_rm


### print last plot to file

ggsave(paste0("Aq_RF_", a_name, "_annual_rm10yr.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

###########################
# STATISTICS

#from rainfall database (restart)
raindata_day_yr_full <- fread(file="J:/Backup_main/2022/20220607_Bkt_Merah_drought/Analysis/Aquarius/R3/Aq_RF_1d_BktMerah_yr_full.csv",
                              header = TRUE, sep=",", stringsAsFactors = F)
raindata_day_yr_full$Date <- as.Date(as.character(raindata_day_yr_full$Date), format = "%Y-%m-%d")
raindata_day_yr_full$fake_date <- as.Date(as.character(raindata_day_yr_full$fake_date), format = "%Y-%m-%d")
str(raindata_day_yr_full)


# add columns for data aggregation
## add a month column 
raindata_day_yr_full2 <- setDT(raindata_day_yr_full)[, Month:= month(Date) ]
raindata_day_yr_full2 <- setDT(raindata_day_yr_full2)[, Day:= day(Date) ]
raindata_day_yr_full2$Month <- as.integer(as.character(raindata_day_yr_full2$Month))
raindata_day_yr_full2$Day <- as.integer(as.character(raindata_day_yr_full2$Day))

str(raindata_day_yr_full2)

###########################

# SINGLE MASS CURVE FOR DAILY DATA

#dataset: clean 2

#cumulative sum
Rain_day_cum <- raindata_day_yr_full2[, Depth_cum_all := cumsum(Depth), by = .(Stn_no)]
Rain_day_cum <- Rain_day_cum[, num := rowidv(Stn_no)]



rain_mass_single_day <- Rain_day_cum %>% 
  ggplot(aes(x = num, y = Depth_cum_all, color = as.character(Stn_no), #group = 1,
             text = paste0("Station no: ", Stn_no)
  )
  ) +
  geom_line(size = 0.25, alpha = 1, na.rm = T) +
  theme_bw(base_size = 10) +
  #scale_x_date(name= "Year", date_labels = "%Y", date_breaks = "year", minor_breaks = NULL) + #x axis format
  scale_x_continuous(name= "Day", 
                     breaks = seq(0, max(Rain_day_cum$num, na.rm = T), by = 500), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Cummulative daily rainfall (mm)"),
                     labels = comma,
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color="grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  labs(title = "Single mass curves for daily rainfall",
       subtitle = paste0(area_name, " stations"),
       color = "Station no")

rain_mass_single_day

#print last plot to file
ggsave(paste0("Aq_RF_", a_name, "_mass_curve_single_daily.jpg"), dpi = 300,
       width = 30, height = 20, units = "cm")


#plotly
ggplotly(rain_mass_single_day, tooltip = "text",
         width = 1200, height = 800)

###########################
#boxplot label function

font_family <- "Roboto"

ggplot_box_legend <- function(family = font_family){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x, 
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile", 
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text", 
                       list(size = 2.8, 
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label", 
                       list(size = 2.8, 
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.25,
                 color = "grey40") +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values), 
                 width = 0.3, 
                 #alpha = 0.3,
                 #fill = "lightgrey",
                 color = "steelblue3",
                 outlier.colour="red") +
    # number of observations
    #geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    #geom_text(aes(x = 1.17, y = 950,
    #              label = "Number of values"),
    #          fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    # mean point
    geom_point(aes(x = 1, y = 390),
               shape = 4, size = 2.5) + 
    geom_text(aes(x = 1.2, 
                  y =  390, 
                  label = "Mean"), 
              vjust = 0.5, fontface = "bold") +
    # quartiles lines
    geom_segment(aes(x = 2.3, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["75th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    # interquartile label
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]), 
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.3) +
    # whiskers lines
    geom_segment(aes(x = 1, xend = 1, 
                     y = ggplot_output[["upper_whisker"]], 
                     yend = ggplot_output[["lower_whisker"]]),
                 color = "grey58") +
    # whiskers label
    geom_text(aes(x = c(1.17,1.17), 
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]), 
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    # outlier labels
    geom_text(aes(x = c(1.17), 
                  y =  ggplot_output[["lower_dots"]], 
                  label = "Outlier"), 
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(1.55), 
                  y =  ggplot_output[["lower_dots"]], 
                  label = ": Value is >1.5 times and"), 
              vjust = 0.5) +
    geom_text(aes(x = 1.17, 
                  y = ggplot_output[["lower_dots"]], 
                  label = "<3 times the interquartile range\nbeyond either end of the box"), 
              vjust = 1.5) +
    # quartiles labels
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]]+1, 
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.6,0.4), 
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 11)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")
  
  return(explain_plot) 
  
}

legend_plot <- ggplot_box_legend()


###########################

###########################
# ANNUAL RAINFALL

#boxplot

Rain_sum_yr_box <- as.data.frame(raindata_yr_full_clean) %>%
  filter(Year != 2022) %>% 
  ggplot(aes(x = Year, y = Depth_yr, group = Year
             #alpha = 0.8
             #color = as.factor(month)
  )) +
  
  geom_boxplot(outlier.colour="red", 
               outlier.shape=20,
               outlier.size=3,
               alpha = 0.5,
               #fill = "white", 
               color = "steelblue3") +
  geom_point(size = 0.9, alpha = 0.7, color = "grey80") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2000, 2022, by = 2), 
                     #limits = c(2000, 2021),
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual rainfall (mm)"),
                     #breaks = seq(0, 350, by = 50), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  stat_summary(fun=mean, aes(shape = "Mean"), #mean value
               geom="point", size=2.5, show.legend = F) +
  stat_boxplot(geom ='errorbar', show.legend = F, width = 0.5,
               color = "grey58") +
  scale_shape_manual(name =NULL, values=c("Mean"=4)) +
  #scale_color_brewer(palette="Set3") +
  theme(text=element_text(
    #family=font_family, 
    color="grey20"),
    panel.grid.major.x = element_blank()) +
  labs(title = "Annual rainfall variability",
       subtitle = paste0(area_name, " stations"))

Rain_sum_yr_box


#arrange chart and tables in 1 page
rain_annual_box <- ggarrange(Rain_sum_yr_box, legend_plot,
                             ncol = 2, nrow = 1, widths = c(3,1))



#print last plot to file
ggsave(paste0("Aq_RF_", a_name, "_rain_boxplot_annual.jpg"), 
       dpi = 300, width = 320, height = 210, units = "mm",
       rain_annual_box)


ggplotly(Rain_sum_yr_box, #tooltip = "text",
         width = 1000, height = 500) 


###########################
# MONTHLY RAINFALL VARIABILITY



# SUMMARIZE DATA FOR MONTH


# calculate the total rain (mm) for each month, each year
Rain_mth_cnt <- raindata_day_yr_full2[, .(Depth_mth = sum(Depth, na.rm = T),
                                          cnt = sum(!is.na(Depth))), 
                                      by = c("Stn_no", "Year", "Month")]

# select months with more than 28 days
Rain_mth_cnt_sel <- Rain_mth_cnt[cnt >= 28] 


# long term monthly average
Rain_sum_mth_lt <- Rain_mth_cnt_sel[, .(Depth_mth_lt = mean(Depth_mth)),
                                    by = c("Stn_no", "Month")] 




###########

#plot boxplot
Rain_sum_mth_box <- Rain_mth_cnt_sel %>%
  ggplot(aes(x = Month, y = Depth_mth, group = Month
             #alpha = 0.8
             #color = as.factor(month)
  )) +
  geom_boxplot(outlier.colour="red", 
               outlier.shape=20,
               outlier.size=3,
               fill = NA, 
               color = "steelblue3") +
  #geom_point(size = ., alpha = 0.5, color = "grey80") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     labels=month.abb) + #x axis format
  scale_y_continuous(name= paste("Monthly rainfall (mm)"),
                     #breaks = seq(0, 900, by = 100), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  stat_summary(fun = mean, aes(shape = "Mean"), #mean value
               geom="point", size=2.5, show.legend = F) +
  stat_boxplot(geom ='errorbar', show.legend = F, width = 0.5,
               color = "grey58") +
  scale_shape_manual(name = NULL, values = c("Mean" = 4)) +
  #scale_color_brewer(palette="Set3") +
  theme(text=element_text(
    #family=font_family, 
    color = "grey20"),
    panel.grid.major.x = element_blank()) +
  labs(title = "Monthly rainfall variability",
       subtitle = paste0(area_name, " stations"))

Rain_sum_mth_box


#arrange chart and tables in 1 page
rain_mth_box <- ggarrange(Rain_sum_mth_box, legend_plot,
                          ncol = 2, nrow = 1, widths = c(3,1))



#print last plot to file
ggsave(paste0("Aq_RF_", a_name, "_rain_boxplot_mth.jpg"), 
       dpi = 300, width = 297, height = 210, units = "mm",
       rain_mth_box)


ggplotly(Rain_sum_mth_box, #tooltip = "text",
         width = 1000, height = 500) 

###########################
# MONTHLY RAINFALL - FACET


# station list
stn_dt <- fread(file="J:/Backup_main/2022/20220607_Bkt_Merah_drought/Data/JPS_RF_stn_Perak.csv",
                 header = TRUE, sep=",", stringsAsFactors = F)

str(stn_dt)

## change column names
colnames(stn_dt) <- c("Stn_no", "Stn_name")


# join station name (with monthly rainfall including incomplete month, Rain_mth_cnt)
Rain_mth_cnt_sel2 <- Rain_mth_cnt[stn_dt, on = "Stn_no", nomatch = 0]


#convert df to named characters for labelling
labels_stn <- setNames(as.character(stn_dt$Stn_name), stn_dt$Stn_no)

# highlight year
yr_hl <- max(Rain_mth_cnt_sel2$Year)
yr_hl <- 2016
yr_hl <- 2020


#plot
Rain_mth_facet <- Rain_mth_cnt_sel2 %>%
  #filter(Stn_no == stn_no_sel) %>%
  ggplot(aes(x = Month, y = Depth_mth, group = Year  
  )) +
  geom_line(
    color = "steelblue", size = 0.7
  ) +
  #geom_text(aes(label = STATION_NA)) +
  #annotate("text", x= 6, y = 15, label =unique(Rain_avg_mth_all2$STATION_NA)) +
  gghighlight(Year == yr_hl, label_key = Year,
              #unhighlighted_colour = alpha("grey60", 0.7),
              #use_direct_label = TRUE,
              label_params = list(alpha = 0.5),
              calculate_per_facet = T) +
  facet_wrap(Stn_no~. ,
             labeller = labeller(Stn_no = labels_stn),
             strip.position = "top",
             #space = "free",
             #scales = "free_y",
             nrow = 4
  ) +
  theme_bw(base_size = 10) +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(name = "Monthly Total Rainfall (mm)",
                     #breaks = seq(0, 350, by = 50), 
                     minor_breaks = NULL) + #y axis format
  #labs(colour = "Year") + #legend title
  theme(text=element_text(family = "Roboto", size = 10,
                          color = "grey20"),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill="grey88", color="white"#, size=0, linetype="solid"
        )) +
  labs(title = "Monthly Total Rainfall by Year and Station",
       subtitle = paste0(area_name, " stations"))

Rain_mth_facet

#print last plot to file
ggsave(paste0("Aq_RF_", a_name, "_mth_facet_", yr_hl, ".jpg"), dpi = 300, 
       width = 20, height = 30, units = "cm")


###########################
#ANNUAL RAINFALL - FACET

# average annual rainfall by station
raindata_yr_full_clean2 <- raindata_yr_full_clean[cnt > cnt_yr]
raindata_yr_full_lt <- raindata_yr_full_clean[, .(Depth_yr_lt = mean(Depth_yr)),
                                                  by = c("Stn_no")]

raindata_yr_full_clean2 <- raindata_yr_full_clean[raindata_yr_full_lt, on = "Stn_no", nomatch = 0]


Rain_yr_facet <- raindata_yr_full_clean2 %>%
  ggplot(aes(x = Year, y = Depth_yr)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(min(raindata_yr_full_clean2$Year), 
                                  max(raindata_yr_full_clean2$Year), by = 5),
                     minor_breaks = NULL) +
  scale_y_continuous(name= "Annual Rainfall (mm)",
                     #breaks = seq(0, 3500, by = 500), 
                     minor_breaks = NULL) + #y axis format
  facet_wrap(Stn_no~. ,
             labeller = labeller(Stn_no = labels_stn),
             strip.position = "top",
             #space = "free",
             #scales = "free_y",
             nrow = 10) +
  geom_hline(
    #data = avg_Rain_yr,
    aes(yintercept = Depth_yr_lt), 
    color = "black", 
    alpha = 0.3, size = 1) + #avg line
  geom_text(aes(min(Year), Depth_yr_lt, label = round(Depth_yr_lt, 0), vjust = -0.5)) +
  theme(text=element_text(family="Roboto", color="grey20", size = 10),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Annual Rainfall",
       subtitle = paste0(area_name, " stations"))

Rain_yr_facet

#print last plot to file
ggsave(paste0("Aq_RF_", a_name, "_yr_facet.jpg"), dpi = 300, 
       width = 15, height = 30, units = "cm")

###########################

###########################


###############################################################################
#OUTPUT
###############################################################################


#list all dataframe
list_worksheet <- list("Annual" = Rain_sum_yr3,
                       "LTAnnual" = Rain_sum_yr_avg_coord,
                       "Record" = raindata_cnt_clean2,
                       "Monthly" = Rain_sum_mth_all2,
                       "LTMthly" = Rain_sum_mth_lt)


# Create a blank workbook
wb <- createWorkbook()

# Loop through the list of split tables as well as their names
#   and add each one as a sheet to the workbook
Map(function(data, name){
  addWorksheet(wb, name)
  writeData(wb, name, data)
  setColWidths(wb, name, cols = 1:3, widths = "auto")
}, list_worksheet, names(list_worksheet))



###########################

# Save workbook to working directory
saveWorkbook(wb, file = paste0("Aq_RF_", a_name, "_output.xlsx"), 
             overwrite = TRUE)


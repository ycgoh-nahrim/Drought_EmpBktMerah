
# MANIPULATE AQUARIUS CSV FILES

# load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2022/20220607_Bkt_Merah_drought/Data/HIDROLOGI DATA PERAK')

#import raw data from Infobanjir
#raw_data = read.csv("KEDAH BulkExport-6 Locations-20220328120637.csv", header = T, sep = ",")



# REARRANGE DATA

#list all files in working directory
filenames <- list.files()

# get all csv files data
all <- lapply(filenames, function(i) 
  cbind(read.csv(i, header = FALSE, skip = 1))
  )

# data list
data_list1 <- list()
data_list <- list()

# loop through all csv files

i = 1

for (i in 1:length(all)) {
  
  #i=3
  
  # number of rows and columns
  n_row = nrow(all[[i]]) # data from 5 to n_row
  n_col = ncol(all[[i]]) # data from 3 to n_col
  
  j = 1
  
  # loop through single csv file data
  for (j in 1:(n_col-2)) {
    
    raw_data <- all[[i]]
    
    # rearrange data from raw_data to new dataframe
    datetime_df = raw_data[5:n_row, 1]
    stn_data_df = raw_data[5:n_row, j + 2] # 3 to n_col
    stn_name = raw_data[2, j + 2] # 3 to n_col
    
    datetime_stn_data_df = cbind(datetime_df, stn_name, stn_data_df)
    
    # add in list combined single stn data
    data_list1[[j]] <- datetime_stn_data_df
  }
  
  # add in list combined csv data
  #data_list[[i]] <- data_list1
  data_list <- append(data_list, data_list1)
  data_list1 <- list()
  
}

#combine into dataframe/data table
data_df <- do.call(rbind.data.frame, data_list)

data_dt <- rbindlist(lapply(data_list, as.data.table))

str(data_dt)

colnames(data_dt) <- c("Datetime", "name", "Depth")

# check stn names
name_df <- data.frame(unique(data_dt$name))

trial <- data_list[[1]]


# split name into stn name and no
data_df2 <- extract(data_df, name, into = c("Stn_name", "Stn_no"), "([^(]+)\\s+\\(([0-9]+).")

data_dt2 <- extract(data_dt, name, into = c("Stn_name", "Stn_no"), "([^(]+)\\s+\\(([0-9]+).")

str(data_dt2)

#check data type, columns
head(data_df2, 6)

check_values <- data.frame(sort(unique(data_dt2$Depth)))
check_values2 <- data.frame(sort(unique(data_dt2$Stn_no)))

check_value3 <- data_dt2[Stn_no == 4324001]

#####
# parse datetime for multiple formats #all failed, fix the source file (do not edit csv in Excel)
library(anytime)

anytime(check_value3$Datetime)

#parse_date_time(check_value3$Datetime, c("%Y-%m-%d %H:%M:%S", "%d/%m/%y %H:%M"), exact = TRUE) #failed
parse_date_time(data_dt2$Datetime, c("dmY HM", "Ymd HMS"), truncated = 3)

str(check_value3)

#####


#replace value '-9999', - with NA
#raw_data[raw_data == "-9999.0"] <- NA
data_dt2[data_dt2 == "NaN"] <- NA



#format date
#data_dt2$Datetime <- as.POSIXct(data_dt2$Datetime, format = "%Y-%m-%d %H:%M:%S") #fwrite will convert date to weird format
data_dt2$Depth <- as.numeric(as.character(data_dt2$Depth))
data_dt2$Stn_no <- as.integer(as.character(data_dt2$Stn_no))

# remove Stn_name column
stn_list <- unique(data_dt2, by = c("Stn_no", "Stn_name"))
data_dt2 <- data_dt2[,':='(Stn_name = NULL)]

str(data_dt2)

#remove duplicates

duplicates_no <- sum(duplicated(data_dt2))
duplicates_df <- data.frame(duplicated(data_dt2))


  
data_dt2 <- unique(data_dt2)

#####################
#write to csv
write.csv(data_dt2, "Aq_RF_1d_Perak_raw.csv", row.names = FALSE, quote = FALSE)

## stn list
write.csv(stn_list, "Aq_RF_1d_Perak_stn_list.csv", row.names = FALSE, quote = FALSE)

#fwrite(data_dt2, "RF_hr_PM_raw.csv", row.names = FALSE, quote = FALSE)

########################################

# load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2022/20220607_Bkt_Merah_drought/Analysis/Aquarius')


#from rainfall database
rain_dt <- fread(file="J:/Backup_main/2022/20220607_Bkt_Merah_drought/Data/Aq_RF_1d_Perak_raw.csv",
                 header = TRUE, sep=",", stringsAsFactors = F)

na_df <- rain_dt[is.na(Datetime)]

test <- rain_dt[Stn_no == 2025001]

#check field name, type, format
head(rain_dt, 3)

str(rain_dt)

#format date
rain_dt$Datetime <- as.POSIXct(rain_dt$Datetime, format = "%Y-%m-%d %H:%M:%S")


#remove negative and NA
RF_data_negative <- rain_dt[Depth < 0]


#add a date column to data table
setDT(rain_dt)[, Date:= date(Datetime) ]

str(rain_dt)

##################################################
# VISUALIZATION



# daily count

RF_daily <- rain_dt[,.(Depth_day = sum(Depth, na.rm = T),cnt = sum(!is.na(Depth))), 
                    by = c("Stn_no", "Date")]

max(RF_daily$cnt) #wrong if more than 24 per day

#check if more than 24 per day
suspicious <- RF_daily[cnt > 24]



#########################

#RECORD HEATMAP (count hourly data by year)

# annual count

setDT(rain_dt)[, Year:= year(Datetime) ]
rain_dt$Year <- as.numeric(as.character(rain_dt$Year))

RF_yr <- rain_dt[,.(Depth_yr = sum(Depth, na.rm = T),cnt = sum(!is.na(Depth))), 
                 by = c("Stn_no", "Year")]

max(RF_yr$cnt) #wrong if more than 366

gg_RF_cnt_hr_matrix <- RF_yr %>% 
  ggplot(aes(x = Year, y = factor(Stn_no), fill = cnt)) +
  geom_tile(aes(text = paste0(Year, '<br>', Stn_no,
                              '<br>Record: <b>', cnt, '</b>'))
            #height = 0.2
            ) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       breaks = c(0, 100, 200, 300, 366), 
                       labels = c(0, 100, 200, 300, 366), 
                       limits = c(0, 366),
                       name = "Annual count") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station no."
                     #breaks = seq(1, 12, by = 1), 
                     #minor_breaks = NULL, 
                     #expand = c(0, 0)
                     #labels = "Stn_no"
                     ) +
  #scale_x_date(name = "Date", breaks = "1 year", date_labels = "%Y",
  #             expand = c(0, 0), minor_breaks = NULL) + 
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
  labs(title = "Daily Rainfall Data Availability (Perak)") +
  guides(fill = guide_colorbar(label.position = "top", title.hjust = .6, 
                               barwidth = unit(10, "lines"), barheight = unit(.5, "lines")))
  #coord_cartesian(expand = F)
  #coord_fixed(ratio = 1.5)

gg_RF_cnt_hr_matrix

#print last plot to file
ggsave(paste0("Aq_RF_1d_Perak_annual_cnt.jpg"), dpi = 300,
       width = 8, height = 20, units = "in")





# annual count for daily data count = 24

RF_yr_day_full <- RF_daily[cnt == 24]
setDT(RF_yr_day_full)[, Year:= year(Date) ]
RF_yr_day <- RF_yr_day_full[,.(Depth_yr = sum(Depth_day, na.rm = T),cnt_yr = sum(!is.na(Depth_day))), 
                            by = c("Stn_no", "Year")]


#RECORD HEATMAP (count daily (24 full) data by year)

gg_RF_cnt_day_matrix <- RF_yr_day %>% 
  ggplot(aes(x = Year, y = factor(Stn_no), fill = cnt_yr)) +
  geom_tile(aes(text = paste0(Year, '<br>', Stn_no,
                              '<br>Record: <b>', cnt_yr, '</b>'))
            #height = 0.2
  ) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       limits = c(0, 366),
                       name = "Annual count") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station no."
                   #breaks = seq(1, 12, by = 1), 
                   #minor_breaks = NULL, 
                   #expand = c(0, 0)
                   #labels = "Stn_no"
  ) +
  #scale_x_date(name = "Date", breaks = "1 year", date_labels = "%Y",
  #             expand = c(0, 0), minor_breaks = NULL) + 
  scale_x_continuous(name = "Year", 
                     breaks = seq(2000, 2022, by = 2),
                     minor_breaks = NULL) +
  theme(text=element_text(family = "Roboto", size = 6,
                          color = "grey20"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        #legend.text = element_text(size = 5),
        #legend.margin = margin(l = -70),
        legend.position="top") +
  labs(title = "Daily Rainfall Data Availability for Each Year (Peninsular Malaysia)") +
  guides(fill = guide_colorbar(label.position = "top", title.hjust = .6, 
                               barwidth = unit(10, "lines"), barheight = unit(.5, "lines")))
#coord_cartesian(expand = F)
#coord_fixed(ratio = 1.5)

gg_RF_cnt_day_matrix

#print last plot to file
ggsave(paste0("PM_RF_annual_cnt_daily.jpg"), dpi = 300,
       width = 8, height = 20, units = "in")



# 2022 DATA

RF_2022_dt <- rain_dt[Year > 2021]

# aggregate data to monthly
setDT(RF_2022_dt)[, Month:= month(Datetime) ]
setDT(RF_2022_dt)[, Day:= day(Datetime) ]
RF_2022_dt$Month <- as.numeric(as.character(RF_2022_dt$Month))
RF_2022_dt$Day <- as.numeric(as.character(RF_2022_dt$Day))

RF_2022_cnt_dt <- RF_2022_dt[,.(Depth_day = sum(Depth, na.rm = T),cnt_day = sum(!is.na(Depth))), 
                            by = c("Stn_no", "Date", "Year", "Month", "Day")]


#RECORD HEATMAP (count daily (24 full) data by year)

gg_RF_cnt_2022_matrix <- RF_2022_cnt_dt %>% 
  ggplot(aes(x = Date, y = factor(Stn_no), fill = cnt_day)) +
  geom_tile(aes(text = paste0(Month, '<br>', Stn_no,
                              '<br>Record: <b>', cnt_day, '</b>'))
            #height = 0.2
  ) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       limits = c(0, 24),
                       name = "Daily count") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station no."
                   #breaks = seq(1, 12, by = 1), 
                   #minor_breaks = NULL, 
                   #expand = c(0, 0)
                   #labels = "Stn_no"
  ) +
  scale_x_date(name = "Date", breaks = "1 day", date_labels = "%b %e",
               expand = c(0, 0), minor_breaks = NULL) + 
  #scale_x_continuous(name = "Date", 
  #                   #breaks = seq(2000, 2022, by = 2),
  #                   minor_breaks = NULL) +
  theme(text=element_text(family = "Roboto", size = 6,
                          color = "grey20"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        #legend.text = element_text(size = 5),
        #legend.margin = margin(l = -70),
        legend.position="top") +
  labs(title = "Hourly Rainfall Data Availability for 2022 (Peninsular Malaysia)") +
  guides(fill = guide_colorbar(label.position = "top", title.hjust = .6, 
                               barwidth = unit(10, "lines"), barheight = unit(.5, "lines")))
#coord_cartesian(expand = F)
#coord_fixed(ratio = 1.5)

gg_RF_cnt_2022_matrix

#print last plot to file
ggsave(paste0("PM_RF_2022_cnt_daily.jpg"), dpi = 300,
       width = 10, height = 20, units = "in")


#########################
#VALUES CHART

library(scattermore)


# disable scientific notation
options(scipen = 999) 


# aggregate data to monthly

setDT(rain_dt)[, Month:= month(Datetime) ]
rain_dt$Month <- as.numeric(as.character(rain_dt$Month))

str(rain_dt)

trial <- rain_dt[Depth > 200]

rain_dt2 <- rain_dt[Depth < 1000]

# check max
max(rain_dt$Depth, na.rm = T)


#write to csv
write.csv(trial, "RF_hr_PM_200mm.csv", row.names = FALSE, quote = FALSE)


## plot all by month



gg_RF_all <- trial %>% 
  ggplot(aes(x = Month, y = Depth)) +
  geom_scattermore(pointsize = 0) +
  theme_bw(base_size = 10) 

gg_RF_all <- rain_dt %>% 
  ggplot(aes(x = Month, y = Depth)) +
  geom_scattermore(pointsize = 0, na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Month", breaks = seq(1, 12, by = 1))

gg_RF_all

gg_rainfall <- rain_dt %>% 
  ggplot(aes(x = Datetime, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = Stn_no), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%Y",
               date_breaks = "1 year",
               #date_minor_breaks = "1 day",
               minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Hourly rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Hourly Rainfall for Peninsular Malaysia")

gg_rainfall

#print last plot to file
ggsave(paste0("PM_RFvalues_hr_all_scatter.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


## hourly

#combine columns
RF_data_hr2 <- RF_data_hr %>% 
  mutate(hour2 = paste0(hour, ":00")) %>% 
  unite(Datetime, c(Date, hour2), sep = " ")

#date format
RF_data_hr2$Datetime <- as.POSIXct(RF_data_hr2$Datetime, format = "%Y-%m-%d %H:%M")

gg_rainfall_hr <- RF_data_hr2 %>% 
  filter(Datetime >= "2021-12-17" & Datetime <= "2021-12-20") %>% 
  ggplot(aes(x = Datetime, y = Depth_hr)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = RF_stn), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%b %d",
                   date_breaks = "1 day",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Hourly Rainfall (mm)"),
                     breaks = seq(0, 100, by = 10),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Hourly Rainfall (December 2021)")

gg_rainfall_hr

#print last plot to file
ggsave(paste0("IB_KLSel_RFvalueshr_dec18.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall_hr, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


## daily

#date format
RF_data_day$Date <- as.POSIXct(RF_data_day$Date, format = "%Y-%m-%d")

gg_rainfall_day <- RF_data_day %>% 
  ggplot(aes(x = Date, y = Depth_day)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = RF_stn), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Date", date_labels = "%b %d",
                   date_breaks = "1 day",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Daily Rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Daily Rainfall (December 2021)")

gg_rainfall_day

#print last plot to file
ggsave(paste0("IB_KLSel_RFvaluesday.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_rainfall_day, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


#clean data

RF_data2 <- RF_data %>% 
  filter(RF_stn != "MOCKUPWPKL")

######
# WATER LEVEL DATA

#import data
WL_data = read.csv("IB_WL_202112_clean.csv", header = T, sep = ",")

#set format
str(WL_data)
WL_data$Datetime <- as.POSIXct(WL_data$Datetime, format = "%Y-%m-%d %H:%M")


#remove duplicates, recount RF_data_hr

duplicates_no <- sum(duplicated(WL_data))

WL_data2 <- unique(WL_data)

#check hourly count

WL_data_hr <- WL_data2 %>% 
  mutate(hour = hour(Datetime), Date = date(Datetime)) %>% 
  group_by(WL_stn, Station.Name, Date, hour) %>% 
  summarise(Level_hr = sum(Level, na.rm = T), cnt = sum(!is.na(Level)))

max(WL_data_hr$cnt) #wrong if more than 4 per hour

#check if more than 4 per hour
suspicious <- WL_data_hr %>% 
  filter(cnt > 4)
suspicious2 <- WL_data %>% 
  filter(RF_stn == 3013002)

#check daily count
WL_data_day <- WL_data2 %>% 
  mutate(Date = date(Datetime)) %>% 
  group_by(WL_stn, Station.Name, Date) %>% 
  summarise(Level_day = sum(Level, na.rm = T), cnt = sum(!is.na(Level)))

max(WL_data_day$cnt) #wrong if more than 24*4=96 per day

#check if more than 96 per day
suspicious <- WL_data_day %>% 
  filter(cnt > 96)



#RECORD HEATMAP

gg_WL_cnt_matrix <- WL_data_day %>% 
  ggplot(aes(x = Date, y = Station.Name, fill = cnt)) +
  geom_tile(aes(text = paste0(Date, '<br>', Station.Name,
                              '<br>Record: <b>', cnt, '</b>'))) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       limits = c(0, 96),
                       name = "Daily count") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_y_discrete(name = "Station Name"
                   #breaks = seq(1, 12, by = 1), 
                   #minor_breaks = NULL, 
                   #expand = c(0, 0)
                   #labels=Station.Name
  ) +
  scale_x_date(name = "Date",
               breaks = "1 day", 
               date_labels = "%b %d",
               expand = c(0, 0),
               minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", size = 10,
                          color = "grey20"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "Data Availability") 
#coord_fixed(ratio = 1.5)

gg_WL_cnt_matrix

#print last plot to file
ggsave(paste0("IB_KLSel_WLcount_day.jpg"), dpi = 300,
       width = 8, height = 10, units = "in")

#plotly
ggplotly(gg_WL_cnt_matrix, tooltip = "text",
         height = 1000,
         width = 900
) %>% 
  layout(legend = list(orientation = "h",
                       y = 1, x = 1))


#VALUES CHART

gg_waterlevel <- WL_data2 %>% 
  ggplot(aes(x = Datetime, y = Level)) +
  geom_line(aes(alpha = 0.5, color = WL_stn), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Datetime", date_labels = "%b %d",
                   date_breaks = "1 day",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Water Level (m)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Water Level (December 2021)")

gg_waterlevel

#print last plot to file
ggsave(paste0("IB_KLSel_WLvalues15min.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")

ggplotly(gg_waterlevel, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


#clean data

WL_data2 <- WL_data %>% 
  filter(WL_stn != "MOCKUPWPKL")



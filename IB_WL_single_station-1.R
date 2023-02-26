# load packages
library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)
library(openxlsx)
library(gghighlight)


# set strings as factors to false
options(stringsAsFactors = FALSE)

###############################################################################
#INPUT
###############################################################################

#select stations by state
state_sel <- "Bukit Merah"
notes_caption <- "NOTE: Year(s) with incomplete data is excluded.\nSource: JPS"
#source_caption <- "Source: JPS"
compare_yr <- 2022


#set working directory
#set filename
filename2 <- paste0("WL_", state_sel)
# get current working directory
working_dir <- getwd()
dir.create(filename2)
setwd(filename2)


###########################
##OPTIONS (streamflow station data or from database) #############
# average daily stage data


#from database
stage_db <- read.csv(file = "E:/Backup_main/2022/20220607_Bkt_Merah_drought/Analysis/Dam/IB/IB_WL_KABM_clean1.csv",
                    header = TRUE, sep=",")

str(stage_db)




###########################
#SELECT STATION LIST BY STATE


# join database with station list

## select data
stagedata_sel <- stage_db %>%
  select(WL_stn, Datetime, Level)

str(stagedata_sel)

#change column name
colnames(stagedata_sel) <- c("Stn_no", "Datetime", "Stage")


## format date
stagedata_sel$Datetime <- as.POSIXct(ymd_hms(stagedata_sel$Datetime), format = "%Y-%m-%d %H:%M:%S")

## check datetime
na_dt <- stagedata_sel %>% 
  filter(is.na(Datetime))


stagedata_sel <- unique(stagedata_sel)


## add Date column
stagedata_sel <- stagedata_sel %>% 
  mutate(Date = date(Datetime))


###data summary for checking
max(stagedata_sel$Stage)
min(stagedata_sel$Stage)


str(stagedata_sel)


#if date is not in date format
stagedata_sel$Date <- as.Date(stagedata_sel$Date, format = "%Y-%m-%d")
#format column from character to numeric
stagedata_sel$Stage <- as.numeric(as.character(stagedata_sel$Stage))





# add columns for data aggregation
## add a year column to data.frame
stagedata_sel <- stagedata_sel %>%
  mutate(Year = year(Date))
## add a month column to data.frame
stagedata_sel <- stagedata_sel %>%
  mutate(Month = month(Date))
## add a day column to data.frame
stagedata_sel <- stagedata_sel %>%
  mutate(Day = day(Date))


###############################################################################
#DATA CLEANING
#if skip data cleaning, then
stagedata_sel2 <- stagedata_sel
###############################################################################

#skip if skip cleaning

# remove years with missing data
## count non-missing data
yr_cnt <- 347
mth_cnt <- 28
day_cnt <- 1

stagedata_cnt <- stagedata_sel %>%
  group_by(Year, Month, Day, Date) %>%
  summarise(WL_day = mean(Stage, na.rm = T), cnt = sum(!is.na(Stage)))


### check max count per day
max(stagedata_cnt$cnt) #should not be more than 24

### check distribution of count per day
hist(stagedata_cnt$cnt, breaks = 20)


## select days based on count
stagedata_cnt_day_full <- stagedata_cnt %>% 
  filter(cnt > day_cnt)


## count by month
stagedata_cnt_mth_full <- stagedata_cnt_day_full %>%
  group_by(Year, Month) %>%
  summarise(WL_mth = mean(WL_day, na.rm = T), cnt = sum(!is.na(WL_day)))


# check data count by month 

gg_cnt_mth_matrix <- stagedata_cnt_mth_full %>% 
  ggplot(aes(x = Month, y = Year, fill = cnt)) +
  geom_tile(aes(text = paste0(month.abb[Month], ' ', Year,
                              '<br>Record: <b>', cnt, '</b>'))) +
  #scale_fill_gradient(low="white", high="turquoise4") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "Daily values per month") +
  #scale_fill_viridis(name = "Daily values", limits = c(0, 31), direction = -1, option = "plasma") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     expand = c(0, 0),
                     labels=month.abb) +
  scale_y_continuous(name = "Year",
                     breaks = seq(2010, 2022, by = 1), 
                     expand = c(0, 0),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family="Roboto", 
                          color="grey20"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="top") +
  labs(title = "Data Availability",
       subtitle = state_sel) +
  guides(fill = guide_colorbar(label.position = "top", title.hjust = .6, 
                               barwidth = unit(10, "lines"), barheight = unit(.5, "lines")))


gg_cnt_mth_matrix

#print last plot to file
ggsave(paste0("IB_BktMerah_WL_cnt_mth.jpg"), dpi = 300,
       width = 10, height = 8, units = "in")


#####################
# ANNUAL WATER LEVEL


## create fake date column to plot with same year
stagedata_day_full <- stagedata_cnt_day_full %>% 
  mutate(fake_date = paste0('2000-', month(Date), '-', day(Date)))


## format date
str(stagedata_day_full)
stagedata_day_full$fake_date <- as.Date(stagedata_day_full$fake_date, format = "%Y-%m-%d")






## fill missing dates, NA
stagedata_day_full2 <- stagedata_day_full %>%
  ungroup %>% 
  #mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day"))

## create fake date column to plot with same year
stagedata_day_full2 <- stagedata_day_full2 %>% 
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>% 
  mutate(fake_date = paste0('2000-', month(Date), '-', day(Date)))


### write to csv
write.csv(stagedata_day_full2,"E:/Backup_main/2022/20220607_Bkt_Merah_drought/Analysis/Dam/IB_BktMerah_WL_day1_full.csv",
          row.names = FALSE, quote = FALSE)

### read csv
stagedata_day_full2 <- read.csv(file = "J:/Backup_main/2022/20220607_Bkt_Merah_drought/Analysis/Dam/IB_BktMerah_WL_day1_full.csv",
                                header = TRUE, sep=",")

  
## format date
str(stagedata_day_full2)
stagedata_day_full2$fake_date <- as.Date(stagedata_day_full2$fake_date, format = "%Y-%m-%d")


gg_stage_yr_all <- stagedata_day_full2 %>%
  #filter(Stn_no == stn_no_sel) %>%
  ggplot(aes(x = fake_date, y = WL_day, color = as.character(Year))) +
  geom_line(size = 0.7) +
  geom_line(data = filter(stagedata_day_full2,Year == 2022), size = 1.2, color = "#e25435") +
  annotate("point", x = as.Date("2000-05-27"), y = 6.1, colour = "blue", size = 3, shape = 4) +
  annotate(geom = "text", x = as.Date("2000-05-30"), y = 6.1, label = "Irrigation water stopped at 6.1m (27 May 2022)", 
           hjust = "left", color = "grey40") +
  geom_hline(yintercept = 5.18, color ="#990000", linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = 6.1, color ="#990000", linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = 6.41, color ="#990000", linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = 6.71, color ="#990000", linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = 7.01, color ="#990000", linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = 7.62, color ="#990000", linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = 9.15, color ="#990000", linetype = "dashed", alpha = 0.25) +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 5.23, label = "Paras Operasi Bekalan Air (LAP) Dihentikan", 
           hjust = "left", color = "grey40") +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 6.15, label = "Paras Operasi Pengairan Dihentikan", 
           hjust = "left", color = "grey40") +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 6.46, label = "Paras Kritikal Pengairan (Tahap 3)", 
           hjust = "left", color = "grey40") +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 6.76, label = "Paras Kritikal Pengairan (Tahap 2)", 
           hjust = "left", color = "grey40") +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 7.06, label = "Paras Kritikal Pengairan (Tahap 1)", 
           hjust = "left", color = "grey40") +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 7.67, label = "Paras Berjaga-jaga Pengairan", 
           hjust = "left", color = "grey40") +
  annotate(geom = "text", x = as.Date("2000-10-01"), y = 9.2, label = "Paras Bahaya Banjir", 
           hjust = "left", color = "grey40") +
  #geom_text(aes(label = STATION_NA)) +
  #annotate("text", x= 6, y = 15, label =unique(Stage_avg_mth_all2$STATION_NA)) +
  #gghighlight(Year == max(Year), label_key = Year) +
              #unhighlighted_colour = alpha("grey60", 0.7),
              #use_direct_label = TRUE,
              #label_params = list(size = 5),
              #calculate_per_facet = T) +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Month", 
               date_labels = "%b", 
               date_breaks = "month", 
               expand = c(0, 0),
               minor_breaks = NULL) +
  scale_y_continuous(name = "Dam Water Level (m)",
                     breaks = seq(5, 9.5, by = .5), 
                     limits = c(5, 9.5),
                     minor_breaks = NULL) + #y axis format
  scale_color_manual(name = "Year",
                     breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022),
                     values = c("#bef7ff", "#b0e7f3", "#a2d7e8", "#93c6dc", "#85b6d0", "#ffdc1d", 
                                "#6996b9", "#5b86ad", "#4d76a1", "#f57b22", "#30558a", "#e25435")) +
  theme(text=element_text(family="Roboto", 
                          color="grey20"),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill="grey88", color="white"#, size=0, linetype="solid"
        )) +
  labs(title = "Bukit Merah Reservoir Water Level",
       #subtitle = state_sel,
       caption = "Note: Gaps indicate missing data")

gg_stage_yr_all

# generated color
## c("#bef7ff", "#b0e7f3", "#a2d7e8", "#93c6dc", "#85b6d0", "#77a6c4", "#6996b9", "#5b86ad", "#4d76a1", "#3e6595", "#30558a", "#22457e")

#print last plot to file
ggsave(paste0("IB_BktMerah_WL_annual5_hl.jpg"), dpi = 300, 
       width = 40, height = 22, units = "cm")







stagedata_cnt2 <- stagedata_cnt %>%
  mutate(status = ifelse(cnt >= data_cnt, "Complete", "Incomplete")) 
stagedata_cnt2 %>%
  ggplot(aes(x = Year, y = cnt, fill = status)) +
  geom_bar(stat = "identity") +
  facet_grid(Stn_no~.) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                     #breaks = seq(min(stagedata_cnt2$Year),max(stagedata_cnt2$Year),by = 1), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= "Count",
                     breaks = seq(0, 400, by = 50), 
                     limits = c(0, 400),
                     minor_breaks = NULL) + #y axis format
  scale_fill_manual(values=c("steelblue", #color
                             "orange2"),
                    name="Legend", 
                    labels = c("Complete",
                               "Incomplete")) +
  theme(text=element_text(family="Roboto", color="grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0.5)) 
#print last plot to file
ggsave(paste0(filename2, "_cnt_yr.jpg"), dpi = 300, 
       width = 20, height = 50, units = "cm")

###########################
# DATA SUMMARY

#summarise data

# calculate the avg stage (m) for each year
Stage_avg_yr <- stagedata_sel2 %>%
  select(Stn_no, Year, Stage) %>%
  group_by(Stn_no, Year) %>%
  summarise(avg_Stage = mean(Stage)) 

# calculate the avg stage (m) for each month, each year
Stage_avg_mth_all <- stagedata_sel2 %>%
  select(Stn_no, Year, Month, Stage) %>%
  group_by(Stn_no, Year, Month) %>%
  summarise(avg_Stage = mean(Stage))

Stage_avg_mth <- Stage_avg_mth_all %>%
  group_by(Stn_no, Month) %>%
  summarise(mth_Stage = mean(avg_Stage))

# calculate the avg stage (m) for long term
Stage_avg_lt <- stagedata_sel2 %>%
  select(Stn_no, Stage) %>%
  group_by(Stn_no) %>%
  summarise(avg_Stage_lt = mean(Stage, na.rm = T)) 


#remove na
Stage_avg_mth_all2 <- Stage_avg_mth_all %>% 
  filter(!is.na(avg_Stage))

# join data
Stage_avg_mth_all2 <- Stage_avg_mth_all2 %>%
  inner_join(stn_sel, by = c("Stn_no" = "STATION_NO")) %>%
  select(Stn_no, STATION_NA, Year, Month, avg_Stage)

# format station no
#Stage_avg_mth_all2$Stn_no <- as.character(Stage_avg_mth_all2$Stn_no)



###########################
#CHARTS - FACETS


#line chart, monthly stage (m), facet, gghighlight
labels_stn <- Stage_avg_mth_all2 %>%
  ungroup() %>%
  select(Stn_no, STATION_NA) %>%
  unique()
#convert df to named characters for labelling
labels_stn <- setNames(as.character(labels_stn$STATION_NA), labels_stn$Stn_no)
#plot
Stage_avg_mth_all2 %>%
  #filter(Stn_no == stn_no_sel) %>%
  ggplot(aes(x = Month, y = avg_Stage, group = Year  
             )) +
  geom_line(
    color = "steelblue", size = 0.7
    ) +
  #geom_text(aes(label = STATION_NA)) +
  #annotate("text", x= 6, y = 15, label =unique(Stage_avg_mth_all2$STATION_NA)) +
  gghighlight(Year == max(Year), label_key = Year,
              #unhighlighted_colour = alpha("grey60", 0.7),
              #use_direct_label = TRUE,
              #label_params = list(size = 5),
              calculate_per_facet = T) +
  facet_wrap(Stn_no~. ,
             labeller = labeller(Stn_no = labels_stn),
             strip.position = "top",
             #space = "free",
             scales = "free_y",
             nrow = 5
             ) +
  theme_bw(base_size = 10) +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(name = "Monthly Average Water Level (m)",
                     #breaks = seq(0, 350, by = 50), 
                     minor_breaks = NULL) + #y axis format
  #labs(colour = "Year") + #legend title
  theme(text=element_text(family="Roboto", 
                          color="grey20"),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill="grey88", color="white"#, size=0, linetype="solid"
        )) +
  labs(title = "Monthly Average Water Level by Year and Station",
       subtitle = state_sel)
#print last plot to file
ggsave(paste0(filename2, "_all_mth_highlight_gg.jpg"), dpi = 300, 
       width = 70, height = 30, units = "cm")


#bar chart, annual avg water level (m) - facet
#long term mean water level
#avg_Stage_yr <- Stage_avg_yr %>%
#  group_by(Stn_no) %>%
#  summarise(avg_Stage_lt = mean(avg_Stage, na.rm = T))

#remove na
Stage_avg_yr2 <- Stage_avg_yr %>% 
  filter(!is.na(avg_Stage)) %>%
  inner_join(Stage_avg_lt, by = "Stn_no")

Stage_avg_yr2 %>%
  ggplot(aes(x = as.factor(Year), y = avg_Stage, group = 1)) +
  #geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(color = "steelblue", size = 1) +
  theme_bw(base_size = 10) +
  scale_x_discrete(name= "Year") + #x axis format
  scale_y_continuous(name= "Annual Average Water Level (m)",
                     #breaks = seq(0, 3500, by = 500), 
                     minor_breaks = NULL) + #y axis format
  facet_wrap(Stn_no~. ,
             labeller = labeller(Stn_no = labels_stn),
             strip.position = "top",
             #space = "free",
             scales = "free_y",
             nrow = 5
  ) +
  geom_hline(
    #data = avg_Stage_yr,
    aes(yintercept = avg_Stage_lt), 
    color="black", 
    alpha=0.3, 
    size=1) + #avg line
  theme(text=element_text(family="Roboto", color="grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Annual Average Water Level",
       subtitle = paste0(state_sel))
#print last plot to file
ggsave(paste0(filename2, "_Stage_annual_all.jpg"), dpi = 300,
       width = 70, height = 50, units = "cm")


###########################
#CHARTS - SINGLE

#selected stations list, with stn no and name
stn_sel2 <- Stage_avg_lt %>%
  inner_join(stn_db, by = c(Stn_no = "STATION_NO")) %>%
  select(Stn_no, "STATION_NA")


for (i in 1:nrow(stn_sel2)) {
  
  stn_no_sel <- stn_sel2[i, 1]
  stn_name_sel <- stn_sel2[i, 2]
  
  #line chart, monthly stage (m), single
  
  Stage_avg_mth_all2 %>%
    filter(Stn_no == stn_no_sel) %>%
    ggplot(aes(x = Month, y = avg_Stage, 
               group = Year  
    )) +
    geom_line(
      color = "steelblue", size = 0.7
    ) +
    gghighlight(Year == max(Year)) +
    theme_bw(base_size = 10) +
    scale_x_discrete(limits = month.abb) +
    scale_y_continuous(name = "Monthly Average Water Level (m)",
                       #breaks = seq(0, 350, by = 50), 
                       minor_breaks = NULL) + #y axis format
    #labs(colour = "Year") + #legend title
    theme(text=element_text(family="Roboto", 
                            color="grey20"),
          panel.grid.major.x = element_blank()) +
    labs(title = "Monthly Average Water Level by Year",
         subtitle = paste(state_sel, ":", stn_no_sel, stn_name_sel),
         caption = notes_caption)
  
  #print last plot to file
  ggsave(paste0(filename2, "WL_mth_", stn_no_sel, ".jpg"), dpi = 300)
  
  
  #bar chart, annual avg water level (m) - no missing years
  #long term average water level (m)
  #avg_Stage_yr <- mean(Stage_avg_yr$avg_Stage)
  Stage_avg_yr2 %>%
    filter(Stn_no == stn_no_sel) %>%
    ggplot(aes(x = as.factor(Year), y = avg_Stage, group = 1)) +
    #geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(color = "steelblue", size = 1) +
    theme_bw(base_size = 10) +
    scale_x_discrete(name= "Year") + #x axis format
    scale_y_continuous(name= expression(paste("Annual Average Water Level (m)")),
                       #breaks = seq(0, 3500, by = 500), 
                       #expand = c(0, 0),
                       #limits = c(0, NA),
                       minor_breaks = NULL) + #y axis format
    geom_hline(aes(yintercept = mean(avg_Stage)), 
               color = "black", 
               alpha = 0.3, 
               size = 1) + #avg line
    geom_text(aes(x = 1, y = mean(avg_Stage),
                  label = paste("Average =", round(mean(avg_Stage), digits = 2), "m")),
              #parse = TRUE, #for superscript
              vjust = -0.2,
              hjust = 0) +
    theme(text=element_text(family="Roboto", color="grey20"),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0.5))  +
    labs(title = "Annual Average Water Level",
         subtitle = paste(state_sel, ":", stn_no_sel, stn_name_sel),
         caption = notes_caption)
  #print last plot to file
  ggsave(paste0(filename2, "WL_yr_", stn_no_sel, "_nomissing.jpg"), dpi = 300)
  
  
}



###############################################################################
#OUTPUT
###############################################################################


#list all dataframe
list_worksheet <- list("Monthly" = Stage_avg_mth_all2,
                       "Annual" = Stage_avg_yr2)


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
saveWorkbook(wb, file = paste0(filename2, "_output.xlsx"), 
             overwrite = TRUE)
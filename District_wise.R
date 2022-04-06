rm(list=ls(all=TRUE))

#dis=read.csv(file = "https://api.covid19india.org/csv/latest/districts.csv",header = T)
dis = read.csv(file = "https://data.covid19bharat.org/csv/latest/districts.csv", header = T)

library(tidyverse)
library(zoo)
dis <- as_tibble(dis)
#arrange according to State and District
dis$Date <- as.Date(dis$Date)
dis <- dis %>%
arrange(State, District) %>%
filter(District != "Unknown" & District != "Airport Quarantine" & District != "Foreign Evacuees" & District != "Other State" & District != "Railway Quarantine")

dist_bengal <- dis %>% filter(State == "West Bengal") %>% 
  group_by(District) %>% 
  mutate(Confirmed_daily = c(Confirmed[1], diff(Confirmed))) %>%
  mutate(death_daily = c(Deceased[1], diff(Deceased))) %>%
  mutate(tested_daily = c(Tested[1], diff(Tested))) %>%
  #filter(Date != Sys.Date())  %>% 
  mutate(confirmed_5da = rollmean(Confirmed_daily, k = 5, fill = NA, align = "right")) %>%
  filter(!is.na(confirmed_5da)) %>%
  mutate(death_5da = rollmean(death_daily, k = 5, fill = NA, align = "right")) %>%
  filter(!is.na(death_5da)) %>% 
  mutate(tested_7da = rollmean(tested_daily, k = 7, fill = NA, align = "right")) %>%
  #filter(!is.na(tested_7da)) %>%
  mutate(cum_Confirmed = cumsum(confirmed_5da)) %>%
  mutate(r = confirmed_5da/cum_Confirmed) %>%
  mutate(cum_death = cumsum(death_5da)) %>%
  mutate(TPR = confirmed_5da/tested_7da) %>% 
  mutate(cum_tested = cumsum(tested_7da)) %>%
  #filter(r != 1) %>%
  ungroup()






dist <- dis %>% 
  group_by(District) %>% 
  mutate(Confirmed_daily = c(Confirmed[1], diff(Confirmed))) %>%
  mutate(death_daily = c(Deceased[1], diff(Deceased))) %>%
  mutate(tested_daily = c(Tested[1], diff(Tested))) %>%
filter(Date != Sys.Date())  %>% 
mutate(confirmed_5da = rollmean(Confirmed_daily, k = 5, fill = NA, align = "right")) %>%
filter(!is.na(confirmed_5da)) %>%
  mutate(death_5da = rollmean(death_daily, k = 5, fill = NA, align = "right")) %>%
  filter(!is.na(death_5da)) %>% 
  #mutate(tested_7da = rollmean(tested_daily, k = 7, fill = NA, align = "right")) %>%
  #filter(!is.na(tested_7da)) %>%
  mutate(cum_Confirmed = cumsum(confirmed_5da)) %>%
  mutate(r = confirmed_5da/cum_Confirmed) %>%
  mutate(cum_death = cumsum(death_5da)) %>%
  #mutate(TPR = confirmed_5da/tested_7da) %>% 
 # mutate(cum_tested = cumsum(tested_7da)) %>%
  #filter(r != 1) %>%
ungroup()



major_dis <- dist %>% filter(District == "Lucknow" | District == "Delhi" | District == "Mumbai" | District == "Pune" | District  == "Bengaluru Urban" | District == "Kolkata" | District == "Chennai" | District == "Nagpur"| District == "Kozhikode") #filter a district

major_dis%>% filter(Date  >= as.Date("2021-10-31")) %>% 
  ggplot(aes(Date, confirmed_5da, colour = District)) +
  geom_line() +
  # geom_line(aes(Date, death_7da),linetype = 2, show.legend = TRUE) +
  #scale_y_log10() +
  scale_x_date(date_breaks = "2 week", date_labels = "%d/%b/%y") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases in  Major City of India"),subtitle = "Data:covid19bharat.org")




lko <- major_dis %>% filter(Date  >= as.Date("2021-01-31")) %>% 
  filter(District == "Lucknow") 
lko %>% 
ggplot(aes(Date, confirmed_5da)) +
  geom_line() +
 geom_line(aes(Date, death_5da),col = 'red',show.legend = TRUE) +
 #scale_y_log10() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b/%y") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases in  Lucknow"),subtitle = "Data:covid19india.org")



dist %>% filter(Date  >= as.Date("2021-01-31")) %>% 
  filter(State == "Uttar Pradesh") %>% 
  filter(District == "Lucknow" |  District == "Varanasi"| District == "Meerut" | District == "Prayagraj" | District == "Kanpur Nagar" | District  == "Gorakhpur" | District == "Agra" | District == "Ghaziabad" | District == "Gautam Buddha Nagar"| District == "Saharanpur") %>% 
  ggplot() +
  geom_line(aes(x =Date, y = confirmed_5da),col = 'blue') +
  geom_line(aes(x =Date, y = death_5da*50),col = 'red') +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Daily Death")) +
  facet_wrap(vars(District), ncol = 3) +
  #scale_y_log10() +
  facet_wrap(~District, scales = "free_y")+
  theme(axis.text.x = element_text(face="bold", size=8, angle=90),
     axis.text.y = element_text(face="bold", color="blue",size=8), 
    axis.text.y.right = element_text(color = "red"), legend.position = "bottom", legend.title = element_blank(),
   plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  scale_color_identity(breaks = c( "red", "blue"),
                       labels = c("Daily Death", "Daily Confirmed"),
                       guide = "legend") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b/%y") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases(in blue) and Daily Death (in red): UP", Sys.Date(), sep = " "), subtitle = "Data:covid19bharat.org")



####Kerala
dist %>% filter(Date  >= as.Date("2020-12-31")) %>% 
  filter(State == "Kerala") %>% 
  ggplot(aes(Date)) +
  geom_line(aes(y = confirmed_5da),col = 'blue') +
  geom_line(aes(y = death_5da*100),col = 'red') +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Daily Death")) +
  #facet_wrap(vars(District), ncol = 3) +
  #scale_y_log10() +
  facet_wrap(~District, scales = "free_y")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b/%y") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases(in blue) and death(red): Kerala"),subtitle = "Data:covid19india.org")




####West Bengal
dist %>% filter(Date  >= as.Date("2020-12-31")) %>% 
  filter(State == "West Bengal") %>% 
  ggplot(aes(Date)) +
  geom_line(aes(y = confirmed_5da),col = 'blue') +
  geom_line(aes(y = death_5da*30),col = 'red') +
  scale_y_continuous(sec.axis = sec_axis(~./30, name = "Daily Death")) +
  #facet_wrap(vars(District), ncol = 3) +
  #scale_y_log10() +
  facet_wrap(~District, scales = "free_y")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases(in blue) and death(red): West Bengal"),subtitle = "Data:covid19india.org")


require(scales)
dist %>% #filter(Date  >= as.Date("2020-12-31")) %>% 
  filter(District == "Lucknow") %>% 
    mutate(Doubling_Time = 0.699/r) %>% 
  ggplot() +
  geom_line(aes(Date, r), col='red') +
  geom_line(aes(Date, Doubling_Time),col = "blue", linetype = 2, show.legend = TRUE) +
   # scale_y_log10(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases in  Lucknow"),subtitle = "Data:covid19india.org")
  scale_x_date(date_breaks = "1 month", date_labels = "%b/%y") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases in  Lucknow"),subtitle = "Data:covid19india.org")
  
  
#####Code for Death Vs CFR ## in UP#########
up <- dis %>% filter(State == "Uttar Pradesh") %>%
  filter(Date == Sys.Date() - 1) %>%
  mutate(cfr = Deceased/Confirmed) %>%
  mutate(Doubling_Time = 0.699/r)

up %>% arrange(desc(Confirmed)) %>% 
  filter(District == Lucknow) -> up 

###########Plot for Death Vs CFR in UP###############
up %>% filter(Deceased>200) %>% 
ggplot(aes(Deceased,cfr)) +
  geom_point(aes(size=cum_Confirmed, col = District)) +
  theme(legend.position = "none") +
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.02) +
  scale_x_log10(breaks=c(0,1,2,5,10,20,30,50,100,200,300,500, 1000, 2000)) +
  scale_size(range = c(0,11), guide = FALSE) +
  labs(x = "Cumulative Death", y = "Case Fatility Rate", title = paste("Death Vs CFR in UP"), subtitle = "Bubble size represents Total Confirmed Cases, Data:covid19india.org")

############DT Vs cum_Confirmed_ in UP##################
ggplot(up,aes(Confirmed_7da, Doubling_Time)) +
  geom_point(aes(size = cum_Confirmed, col = District)) +
  theme(legend.position = "none") +
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.02) +
  scale_x_log10(breaks = c(10, 25, 50, 100, 200, 400, 800, 1500, 5000)) +
  scale_size(range = c(0,10)) +
  labs(x = "Daily Confirmed(7 days average)", y = "Doubling Time(in Days)", title = paste("Confirmed Cases Vs Doubling Time in UP Dated", Sys.Date(), sep = " " ), subtitle = "Bubble size represents Total Cases, Data:covid19india.org", caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com") 

#####Tested vs Confirmed
ggplot(up,aes(Tested, cum_Confirmed))+
geom_point(aes(size = r, col = District))+
  theme(legend.position = "none")+
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.005)+
  scale_x_log10(breaks = c(10000,20000,30000,40000,50000,60000,100000,150000,200000,350000))+
  scale_size(range = c(0,10), guide = FALSE) +
  scale_y_log10(breaks = c(2^(-1:4) * 1000, 30000))+
  labs(x = "Cumulative Tested", y = "Cumulative Confirmed", title = paste("Confirmed Cases Vs Tested in UP Dated", Sys.Date(), sep = " " ), subtitle = "Bubble size represents infection rate,\n Data:covid19india.org", x = "Tested", y = "Confirmed" ,caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com")
 
######### Up major
up_major <- dis %>% filter(State == "Uttar Pradesh") %>%
  filter(Date == Sys.Date() - 1) %>%
  mutate(cfr = Deceased/Confirmed) %>%
  mutate(Doubling_Time = 0.699/r) %>% 
 # mutate(month = mday(Date)) %>% 
  filter(District == "Lucknow" | District == "Kanpur Nagar"  | District ==  "Gautam Buddha Nagar" | District == "Ghaziabad" | District ==  "Prayagraj" | District == "Varanasi" | District == "Gorakhpur"| District == "Agra" | District == "Meerut") 


up_major %>% 
  ggplot(mapping = aes(x = Date, y = Confirmed_7da, col = District)) +
  geom_line() +
  facet_wrap(vars(District), ncol = 3)



#####Function Plotting
gap_plot <- function(data){
  ggplot(aes(Tested, cum_Confirmed))+
    geom_point(aes(size = r, col = District))+
    theme(legend.position = "none")+
    geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.005)+
    scale_x_log10(breaks = c(10000,20000,30000,40000,50000,60000,100000,150000,200000,350000))+
    scale_size(range = c(0,10), guide = FALSE) +
    scale_y_log10(breaks = c(2^(-1:4) * 1000, 30000))+
    labs(x = "Cumulative Tested", y = "Cumulative Confirmed", title = paste("Confirmed Cases Vs Tested in UP Dated", Sys.Date(), sep = " " ), subtitle = "Bubble size represents infection rate,\n Data:covid19india.org", x = "Tested", y = "Confirmed" ,caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com")
}
up_gap_week <- up_major %>% 
  filter(month %% 30 == 0) %>% 
  group_by(Date) 
by_week <- up_gap_week %>%  group_split() 
by_week[[1]]

month <- up_gap_week %>% group_keys() %>% pull(mday)
titles <- paste0("Month:", month)

plots <- map2(by_week, titles, gap_plot)


###############India Major#############
###############
###############

ind_major_dis <- dis %>% #filter(State == "Uttar Pradesh") %>%
  filter(Date == Sys.Date() - 1) %>%
  filter(Confirmed > 25000) %>% 
  select(District)

ind_major<- dis %>% 
  mutate(cfr = Deceased/Confirmed) %>%
  mutate(Doubling_Time = 0.699/r) %>% 
  arrange(desc(cum_Confirmed)) %>% 
  filter(District %in% ind_major_dis$District) 

#plot for tested vs confirmed #####
ind_major %>% filter(Date == Sys.Date() - 1) %>%
ggplot(aes(Tested, cum_Confirmed))+
  geom_point(aes(size = r, col = District))+
  theme(legend.position = "none")+
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.005)+
  scale_x_log10(breaks = c(10000,20000,30000,40000,60000,100000,150000,200000,350000, 500000, 1000000, 2000000))+
  scale_size(range = c(0,10), guide = FALSE) +
  scale_y_log10(breaks = c(2^(-1:8) * 1000))+
  labs(x = "Cumulative Tested", y = "Cumulative Confirmed", title = paste("Confirmed Cases Vs Tested in Top District Dated", Sys.Date(), sep = " " ), subtitle = "Bubble size represents infection rate,\n Data:covid19india.org", x = "Tested", y = "Confirmed" ,caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com")

###plot for Doubling Time Vs ######################
ind_major %>% filter(Date == Sys.Date() - 1) %>%
ggplot(aes(Confirmed_7da, Doubling_Time)) +
  geom_point(aes(size = cum_Confirmed, col = District)) +
  theme(legend.position = "none") +
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.02) +
  scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800, 1000,1500, 3000, 5000, 7000)) +
  scale_size(range = c(0,10)) +
  labs(x = "Daily Confirmed(7 days average)", y = "Doubling Time(in Days)", title = paste("Confirmed Cases Vs Doubling Time in District where Cumulative Confirmed > 10000  Dated", Sys.Date(), sep = " " ), subtitle = "Bubble size represents Total Cases, Data:covid19india.org", caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com") 

# plot of Death vs cfr in India Distrct
ind_major %>% filter(Date == Sys.Date() - 1) %>% arrange(desc(Deceased)) %>% 
ggplot(aes(Deceased,cfr)) +
  geom_point(aes(size=cum_Confirmed, col = District)) +
  theme(legend.position = "none") +
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.02) +
  scale_x_log10(breaks=c(10,50,100,200,400,800,1600,3200,5000, 8000)) +
  scale_size(range = c(0,10), guide = FALSE) +
  labs(x = "Cumulative Death", y = "Case Fatility Rate", title = paste("Death Vs CFR in in District where Cumulative Confirmed > 10000", Sys.Date(), sep = " "), subtitle = "Bubble size represents Total Confirmed Cases, Data:covid19india.org")

#plot for tested vs confirmed for 6 major state #####
dis %>% filter(State == "Maharashtra"|State == "Andhra Pradesh"|State == "Karnataka"|State == "Tamil Nadu"|State == "Uttar Pradesh" |State == "Odisha")  %>%
  filter(Date == Sys.Date() - 1) %>%
  mutate(cfr = Deceased/Confirmed) %>%
  mutate(Doubling_Time = 0.699/r) %>% 
  ggplot(aes(Deceased, cfr))+
  geom_point(aes(size = r, col = District)) +
  theme(legend.position = "none") +
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.005) +
  scale_x_log10() +
 # scale_size(range = c(0,10), guide = FALSE) +
  facet_wrap(vars(State), ncol = 3) +
  labs(x = "Cumulative Death", y = "Case Fatility Rate", title = paste("Death Vs CFR in District of 6 States of India", Sys.Date(), sep = " "), subtitle = "Bubble size represents Total Confirmed Cases, Data:covid19india.org")

dis %>% filter(State == "Maharashtra"|State == "Andhra Pradesh"|State == "Karnataka"|State == "Tamil Nadu"|State == "Uttar Pradesh" |State == "Odisha")  %>%
  filter(Date == Sys.Date() - 1) %>%
  mutate(cfr = Deceased/Confirmed) %>%
  mutate(Doubling_Time = 0.699/r) %>% 
  ggplot(aes(Tested, cum_Confirmed))+
  geom_point(aes(size = r, col = District)) +
  theme(legend.position = "none") +
  geom_text(aes(label = District), check_overlap = TRUE, size= 3, hjust = 0, nudge_x = 0.005) +
  scale_x_log10() +
  scale_y_log10() +
  # scale_size(range = c(0,10), guide = FALSE) +
 facet_wrap(vars(State), ncol = 3, scales = "free", strip.position = "top")+
  labs(x = "Tested", y = "Cumulative Confirmed", title = paste("Tested Vs Total Confirmed in District of 6 States of India", Sys.Date(), sep = " "), subtitle = "Bubble size represents infection incidence, Data:covid19india.org",caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com")


rm(list = ls(all = TRUE))
d = read.csv(file = "https://data.covid19bharat.org/csv/latest/state_wise_daily.csv",header = T)
library(tidyverse)
library(zoo)
library(geofacet) # facets plotting
d <- as_tibble(d)
d$Date = as.Date(d$Date,"%d-%b-%y")

dlong <- d %>% pivot_longer(cols = TT:UN, names_to = "State", values_to = "Cases") %>%
  pivot_wider(names_from = Status, values_from = Cases) %>% 
  arrange(State)

#dlong <-  dlong %>% filter(Date  >= as.Date("2021-12-1"))
#Calculation of r #########
dlong <- dlong %>% filter(State != "UN") %>% 
  group_by(State) %>% 
  mutate(Confirmed_7da = rollmean(Confirmed, k = 3, fill = NA, align = "right")) %>%
  mutate(Death_7da = rollmean(Deceased, k = 3, fill = NA, align = "right")) %>%
filter(!is.na(Confirmed_7da)) %>%
  mutate(cum_Confirmed = cumsum(Confirmed_7da)) %>%
  mutate(r=Confirmed_7da/cum_Confirmed) %>%
  filter(r!=1) %>%
  ungroup()

#################
dlong %>% filter(Date  >= as.Date("2021-1-31")) %>% 
  #filter(State == "KE") %>% 
  mutate(State = fct_reorder(State, Death_7da, .fun = mean)) %>% #reordering state as per deaths
  ggplot() +
  geom_line(aes(x = Date, y = Confirmed_7da,color = 'blue')) +
  geom_line(aes(x = Date, y = Death_7da*100,color = 'red')) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Daily Death")) +
  #facet_wrap(vars(District), ncol = 3) +
  #scale_y_log10() +
 # theme_minimal()+
  facet_wrap(~State, scales = "free_y") +
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=90),
        axis.text.y = element_text(face="bold", color="blue", 
                                   size=8), 
        axis.text.y.right = element_text(color = "red"), legend.position="top", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  scale_color_identity(breaks = c( "red", "blue"),
                       labels = c("Daily Death", "Daily Confirmed"),
                       guide = "legend") +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases & Deaths (7 days Moving average): States/UT of India"),subtitle = "Data: www.covid19india.org",caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com")

########Calculation of CFR####################
dlong <- dlong %>% 
  group_by(State) %>% 
 mutate(cum_Death = cumsum(Death_7da)) %>%
  mutate(cfr=cum_Death/cum_Confirmed) %>%
  ungroup()
######################################
dlong %>% filter(Date  >= as.Date("2020-3-31")) %>% 
 #filter(State == "UP") %>% 
  ggplot(aes(Date)) +
  geom_line(aes(y = Confirmed_7da),col = 'blue') +
  geom_line(aes(y = Death_7da*50),col = 'red') +
  scale_y_continuous(sec.axis = sec_axis(~./50, name = "Daily Death")) +
  facet_wrap(~State, scales = "free_y")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=90),
        axis.text.y = element_text(face="bold", color="blue", 
                                   size=8), 
        axis.text.y.right = element_text(color = "red"), legend.position="top", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  #scale_y_log10() +
  labs(x = "Month", y = "Daily New Cases", title = paste("Daily New Covid Cases (Blue Line) & Death (Red line) in  States"),subtitle = "Data:covid19india.org", caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com") 
 

######################################

#########################################
dlongt <- dlong %>%  filter(State != "TT" & Date == Sys.Date() - 1) 
ggplot(dlongt,aes(cum_Death,cfr, col = State)) +
  geom_point(aes(size = cum_Confirmed)) +
  theme(legend.position = "none") +
  geom_text(aes(label = State), check_overlap = TRUE, size = 3, hjust = 0, nudge_x = 0.05) + 
  scale_x_log10(breaks=c(0, 1,5,10,24,100,500,2500, 10000, 15000, 28000, 55000)) +
  labs(title = paste("Death Vs CFR by COVID-19 among different State of India dated", Sys.Date(), sep = " " ), subtitle = "Bubble size represents total confirmed cases", x = "Total Death", y = "Case Fatility Rate" ,caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com")
  
######Doubling Time################ 
dlongt %>%  mutate(Doubling_Time = 0.699/r) %>% 
  ggplot(aes(Confirmed_7da, Doubling_Time))+
  geom_point(aes(size = cum_Confirmed, col = State))+
  theme(legend.position = "none")+
  geom_text(aes(label = State), check_overlap = TRUE, , size= 3, hjust = 0, nudge_x = 0.05)+
  scale_x_log10(breaks=c(0, 1,5,10,24,100,230, 1500,5000,10000,15000))+
  scale_size(range = c(0,10)) +
  labs(title = paste("Daily Cased Vs Doubling Time of  COVID-19 cases among different State of India dated", Sys.Date(), sep = " " ), subtitle = "Bubble size represents total confirmed cases", x = "Daily Confirmed(7 day Moving Average)", y = "Doubling Time(in Days)" ,caption = "Dr. Anup Kumar, SGPGIMS, Lucknow \n anup.stats@gmail.com")

######################
require(scales)
dlong %>% 
  group_by(State) %>% 
 filter(Date  >= as.Date("2021-12-01")) %>% 
  filter(Date  <= as.Date("2022-01-10")) %>% 
  #mutate(Doubling_Time = 0.699/r) %>% 
ggplot(mapping = aes(Date, Confirmed)) +
  geom_col() +
  geom_line(mapping = aes(Date, Confirmed_7da), col = "red") +
  facet_wrap(~State,scales = "free_y" ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b/%y") +
  theme(legend.position = "none") 

 
  

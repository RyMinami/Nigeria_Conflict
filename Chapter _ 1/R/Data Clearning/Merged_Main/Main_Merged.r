library(dbplyr)
library(tidyverse)
library(cowplot)
library(RColorBrewer)
install.packages("ggspatial")
library(ggspatial)

##Full data
full_conf_df <- read.csv("/Users/ryuto/Dropbox/Conflict/Analysis/Dissertation_R/Doctor/Conflict_data/ACRED Full.csv")
view(full_conf_df)


##Extracting the data for the analysis, only Nigeria
Nigeria_ACREDA <- full_conf_df |> 
                  filter(COUNTRY == "Nigeria", YEAR >= 2009, YEAR <= 2019) 
view(Nigeria_ACREDA)

##Extracting the data for the analysis based on Event type
df_Event <- Nigeria_ACREDA |> 
                  filter(EVENT_TYPE == "Violence against civilians" | EVENT_TYPE == "Battles" | EVENT_TYPE == "Explosions/Remote violence")
view(df_Event)

##Converting the Event_Type variable into a numarical variable (Event_All_1)
df_Event <- df_Event |> 
            mutate(Event_All_1 = case_when(EVENT_TYPE == "Violence against civilians" ~ 1,
                                    EVENT_TYPE == "Battles" ~ 1,
                                    EVENT_TYPE == "Explosions/Remote violence" ~ 1,
                                    TRUE ~ 0))
view(df_Event)

conflict_sf <- st_as_sf(Nigeria_ACREDA, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

##Creatinh Fatality rate 2003-2019
Fatality_1 <- full_conf_df |> 
                  filter(COUNTRY == "Nigeria", YEAR >= 2003, YEAR <= 2019) |>
                  filter(EVENT_TYPE == "Violence against civilians" | EVENT_TYPE == "Battles" | EVENT_TYPE == "Explosions/Remote violence") |>
                  mutate(YEAR = factor(YEAR, levels = c(2003:2019)))
view(Fatality_1)

##Mapping
library(sf)
N_map <- st_read("/Users/ryuto/Desktop/Doctoral/Spatial Data/Nigeria_Geo_Shape/gadm41_NGA_1.shp")
View(N_map)


N_map_2 <- st_read("/Users/ryuto/Desktop/Doctoral/Spatial Data/Nigeria_Geo_Shape/gadm41_NGA_2.shp")
View(N_map_2)

##try mapping
ggplot(N_map_2) +
  geom_sf(alpha = 0.5)

##Change the name of the variable (ADIN1 -> NAME_1)
Nigeria_ACREDA <- Nigeria_ACREDA|>
                    rename(NAME_1 = ADMIN1)
view(Nigeria_ACREDA)

conflict_summary <- conflict_sf |>
  st_join(N_map_2) |>
  group_by(NAME_2) |>
  summarize(total_events = n(),
            total_fatalities = sum(S_Fatalities, na.rm = TRUE))

conflict_summary_2 <- conflict_sf |>
  st_join(N_map_2) |>
  group_by(NAME_2) |>
  summarize(total_events = n(),
            total_event = sum(Event_by_state, na.rm = TRUE))


conflict_summary <- N_map_2 |>
  st_join() |>
  group_by(NAME_2) |>
  summarize(total_events = n(),
            total_fatalities = sum(S_Fatalities, na.rm = TRUE))


##ggplot Fatality 2003-2019
ggplot(Fatality_1, aes(x = YEAR, y = FATALITIES)) +
    stat_summary(fun = "sum", geom = "bar") +
    scale_y_continuous(breaks = seq(0,15000,length = 4),limits = c(0,12000)) +
    xlab("Year") +
    ylab("Event") +
    theme_classic() + 
    theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 20))+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 14))
ggsave("~/Desktop/plot.png")

##PLOT 
ggplot(N_map_2) + 
      geom_sf() + 
      geom_point(data = Nigeria_ACREDA, 
      aes(x = LONGITUDE, y = LATITUDE))
ggsave("~/Desktop/plot.png")

ggplot(N_map_2) + 
      geom_sf() + 
      geom_point(data = df_Boko, aes(x = LONGITUDE , y = LATITUDE))

##Showing the number of fatalities by state
Nigeria_ACREDA |> 
  group_by(NAME_1) |>
  summarise(FATALITIES = sum(FATALITIES)) |>
  arrange(desc(FATALITIES)) |>
print(n = 38)

##Creating a new variable for the number of fatalities by state
Nigeria_ACREDA <- Nigeria_ACREDA |> 
                  group_by(NAME_1) |>
                  mutate(Fatalities_by_state = sum(FATALITIES)) |>
                  arrange(desc(Fatalities_by_state)) |>
                  ungroup()
view(Nigeria_ACREDA)

left_join(N_map_2,)

##mappinng the number of fatalities by state
ggplot(N_map_2) +
  geom_sf(aes(fill = S_Fatalities), alpha = 0.5)

####Creating the variable for the number of fatalities by state in map data
N_map_2 <- N_map_2 |> 
          mutate(S_Fatalities = case_when(NAME_1 == "Borno" ~ 27381,
                                          NAME_1 == "Yobe" ~ 2240,
                                          NAME_1 == "Adamawa" ~ 3351,
                                          NAME_1 == "Gombe" ~ 520,
                                          NAME_1 == "Taraba" ~ 1598,
                                          NAME_1 == "Bauchi" ~ 537,
                                          NAME_1 == "Plateau" ~ 3977,
                                          NAME_1 == "Kaduna" ~ 3808,
                                          NAME_1 == "Kano" ~ 927,
                                          NAME_1 == "Katsina" ~ 673,
                                          NAME_1 == "Zamfara" ~ 2476,
                                          NAME_1 == "Sokoto" ~ 234,
                                          NAME_1 == "Kebbi" ~ 48,
                                          NAME_1 == "Jigawa" ~ 43,
                                          NAME_1 == "Niger" ~ 334,
                                          NAME_1 == "Kogi" ~ 391,
                                          NAME_1 == "Kwara" ~ 70,
                                          NAME_1 == "Nasarawa" ~ 1032,
                                          NAME_1 == "Benue" ~ 2794,
                                          NAME_1 == "Oyo" ~ 52,
                                          NAME_1 == "Osun" ~ 87,
                                          NAME_1 == "Ondo" ~ 74,
                                          NAME_1 == "Ekiti" ~ 49,
                                          NAME_1 == "Ogun" ~ 238,
                                          NAME_1 == "Lagos" ~ 381,
                                          NAME_1 == "Rivers" ~ 588,
                                          NAME_1 == "Bayelsa" ~ 289,
                                          NAME_1 == "Delta" ~ 1839,
                                          NAME_1 == "Edo" ~ 220,
                                          NAME_1 == "Anambra" ~ 3351,
                                          NAME_1 == "Enugu" ~ 149,
                                          NAME_1 == "Ebonyi" ~ 273,
                                          NAME_1 == "Imo" ~ 125,
                                          NAME_1 == "Abia" ~ 354,
                                          NAME_1 == "Akwa Ibom" ~ 270,
                                          NAME_1 == "Cross River" ~ 500,
                                          NAME_1 == "Benue" ~ 2794,
                                          NAME_1 == "Federal Capital Territory" ~ 16
                                          ))
view(N_map)

ggplot(N_map_2) +
    geom_sf(aes(fill = S_Fatalities)) +
    labs(title = "No of Fatality Case by State") +
    scale_fill_continuous(high = "#3e0dbb", low = "#f2f0f7") +
    theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5)
    ) +
    annotation_scale(
        location = "bl",
        width_hint = 0.5,
        pad_x = unit(3, "cm")
    )
 ggsave("~/Desktop/plot.pdf")


##mappinng the number of fatalities by state
  ggplot(N_map) +
  geom_sf(aes(fill = S_Fatalities)) +
  labs(
    title    = "Fatalities by State",
    subtitle = "2009-2019")  +
  scale_fill_continuous(high="#54278f", low="#f2f0f7") +
  theme_bw() +
  annotation_scale(
    location = "bl",
    width_hint = 0.5,
    pad_x = unit(3, "cm")
  )
  ggsave("~/Desktop/plot.png")

##Showing the number of events by state
df_Event|> 
  group_by(ADMIN1) |>
  summarise(Event_by_state = sum(Event_All_1)) |>
  arrange(desc(Event_by_state)) |>
  print(n = 38)

df_Event<- df_Event |>
  rename(NAME_1 = ADMIN1) 
view(df_Event)

N_map_2 <- N_map_2 |> 
          mutate(Event_by_state = case_when(NAME_1 == "Borno" ~ 2411,
                                          NAME_1 == "Yobe" ~ 288,
                                          NAME_1 == "Adamawa" ~ 362,
                                          NAME_1 == "Gombe" ~ 77,
                                          NAME_1 == "Taraba" ~ 274,
                                          NAME_1 == "Bauchi" ~ 111,
                                          NAME_1 == "Plateau" ~ 503,
                                          NAME_1 == "Kaduna" ~ 393,
                                          NAME_1 == "Kano" ~ 953,
                                          NAME_1 == "Katsina" ~ 118,
                                          NAME_1 == "Zamfara" ~ 247,
                                          NAME_1 == "Sokoto" ~ 66,
                                          NAME_1 == "Kebbi" ~ 1,
                                          NAME_1 == "Jigawa" ~ 24,
                                          NAME_1 == "Niger" ~ 97,
                                          NAME_1 == "Kogi" ~ 138,
                                          NAME_1 == "Kwara" ~ 32,
                                          NAME_1 == "Nasarawa" ~ 181,
                                          NAME_1 == "Benue" ~ 412,
                                          NAME_1 == "Oyo" ~ 66,
                                          NAME_1 == "Osun" ~ 57,
                                          NAME_1 == "Ondo" ~ 84,
                                          NAME_1 == "Ekiti" ~ 61,
                                          NAME_1 == "Ogun" ~ 89,
                                          NAME_1 == "Lagos" ~ 192,
                                          NAME_1 == "Rivers" ~ 304,
                                          NAME_1 == "Bayelsa" ~ 185,
                                          NAME_1 == "Delta" ~ 321,
                                          NAME_1 == "Edo" ~ 149,
                                          NAME_1 == "Anambra" ~ 94,
                                          NAME_1 == "Enugu" ~ 95,
                                          NAME_1 == "Ebonyi" ~ 67,
                                          NAME_1 == "Imo" ~ 41,
                                          NAME_1 == "Abia" ~ 65,
                                          NAME_1 == "Akwa Ibom" ~ 97,
                                          NAME_1 == "Cross River" ~ 149,
                                          NAME_1 == "Federal Capital Territory" ~ 16
                                          ))
view(N_map)


ggplot(N_map_2) +
    geom_sf(aes(fill = Event_by_state)) +
    labs(title = "No of Conflict Event by State") +
    scale_fill_continuous(high = "#bb0d0d", low = "#f2f0f7") +
    theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5)
    ) +
    annotation_scale(
        location = "bl",
        width_hint = 0.5,
        pad_x = unit(3, "cm")
    )

ggplot(N_map) +
  geom_sf(aes(fill = Event_by_state)) +
  scale_fill_gradient(low='#ffffffff', high='#e60808') +
  labs(
    title    = "Event by State",
    subtitle = "2009-2019")  +
  theme_bw() +
  annotation_scale(
    location = "bl",
    width_hint = 0.5,
    pad_x = unit(3, "cm")
  )
  ggsave("~/Desktop/plot.png")
  
##Showing the Fatalities_by_state grouped by NAME_1
Nigeria_ACREDA |> 
  group_by(NAME_1) |>
  summarise(Fatalities_by_state = sum(FATALITIES)) |>
  arrange(desc(Fatalities_by_state)) |>
  print(n = 38)

##Converting the Event_Type variable into a numarical variable (Event_All)
Nigeria_ACREDA <- Nigeria_ACREDA |> 
            mutate(Event_All = case_when(
              EVENT_TYPE == "Violence against civilians" ~ 1,
              EVENT_TYPE == "Battles" ~ 1,
              EVENT_TYPE == "Explosions/Remote violence" ~ 1,
              EVENT_TYPE == "Riots/Protests" ~ 1,
              EVENT_TYPE == "Strategic developments" ~ 1,
              TRUE ~ 0))
view(Nigeria_ACREDA)


##Showing the number of events by state
Nigeria_ACREDA |> 
  group_by(ADMIN1) |>
  summarise(EVENT_by_state = sum(Event_All)) |>
  arrange(desc(EVENT_by_state)) |>
  print(n = 38)


##Conflict data by year
##Filtering the data by year
df_2009_2010 <- full_conf_df |> 
                  filter(COUNTRY == "Nigeria", YEAR >= 2009, YEAR <= 2010) 
view(df_2009_2010)

##converting the Event_Type variable into a numarical variable (Event_N)
df_2009_2010 <- df_2009_2010 |> 
            mutate(Event_N = case_when(
              EVENT_TYPE == "Violence against civilians" ~ 1,
              EVENT_TYPE == "Battles" ~ 1,
              EVENT_TYPE == "Explosions/Remote violence" ~ 1,
              EVENT_TYPE == "Riots/Protests" ~ 1,
              EVENT_TYPE == "Strategic developments" ~ 1,
              TRUE ~ 0))
df_2009_2010

df_2009_2010 |> 
  group_by(ADMIN1) |>
  summarise(EVENT_by_state = sum(Event_N)) |>
  arrange(desc(EVENT_by_state)) |>
print(n = 38)

##creating the number of events by state(2009_2010)
N_map <- N_map |> 
          mutate(EVENT_2009_10 = case_when(NAME_1 == "Borno" ~ 43,
                                          NAME_1 == "Yobe" ~ 12,
                                          NAME_1 == "Adamawa" ~ 2,
                                          NAME_1 == "Gombe" ~ 5,
                                          NAME_1 == "Taraba" ~ 3,
                                          NAME_1 == "Bauchi" ~ 19,
                                          NAME_1 == "Plateau" ~ 72,
                                          NAME_1 == "Kaduna" ~ 9,
                                          NAME_1 == "Kano" ~ 6,
                                          NAME_1 == "Katsina" ~ 1,
                                          NAME_1 == "Zamfara" ~ 2,
                                          NAME_1 == "Sokoto" ~ 3,
                                          NAME_1 == "Kebbi" ~ 5,
                                          NAME_1 == "Jigawa" ~ 4,
                                          NAME_1 == "Niger" ~ 6,
                                          NAME_1 == "Kogi" ~ 4,
                                          NAME_1 == "Kwara" ~ 2,
                                          NAME_1 == "Nasarawa" ~ 4,
                                          NAME_1 == "Benue" ~ 2,
                                          NAME_1 == "Oyo" ~ 12,
                                          NAME_1 == "Osun" ~ 6,
                                          NAME_1 == "Ondo" ~ 7,
                                          NAME_1 == "Ekiti" ~ 3,
                                          NAME_1 == "Ogun" ~ 8,
                                          NAME_1 == "Lagos" ~ 17,
                                          NAME_1 == "Rivers" ~ 48,
                                          NAME_1 == "Bayelsa" ~ 47,
                                          NAME_1 == "Delta" ~ 59,
                                          NAME_1 == "Edo" ~ 18,
                                          NAME_1 == "Anambra" ~ 3,
                                          NAME_1 == "Enugu" ~ 10,
                                          NAME_1 == "Ebonyi" ~ 4,
                                          NAME_1 == "Imo" ~ 5,
                                          NAME_1 == "Abia" ~ 16,
                                          NAME_1 == "Akwa Ibom" ~ 9,
                                          NAME_1 == "Cross River" ~ 13,
                                          NAME_1 == "Federal Capital Territory " ~ 16,
                                          ))
N_map

##mappinng the number of fatalities by state 2012-2013
df_2012_2013 <- full_conf_df |> 
                  filter(COUNTRY == "Nigeria", YEAR >= 2012, YEAR <= 2013) 
view(df_2012_2013)
##converting the Event_Type variable into a numarical variable (Event_N)
df_2012_2013 <- df_2012_2013 |> 
            mutate(Event_N = case_when(
              EVENT_TYPE == "Violence against civilians" ~ 1,
              EVENT_TYPE == "Battles" ~ 1,
              EVENT_TYPE == "Explosions/Remote violence" ~ 1,
              EVENT_TYPE == "Riots/Protests" ~ 1,
              EVENT_TYPE == "Strategic developments" ~ 1,
              TRUE ~ 0))
df_2012_2013

df_2012_2013 |> 
  group_by(ADMIN1) |>
  summarise(EVENT_by_state = sum(Event_N)) |>
  arrange(desc(EVENT_by_state)) |>
print(n = 38)

##creating the number of events by state(2012_2013)
N_map <- N_map |> 
          mutate(Event_2012_13 = case_when(NAME_1 == "Borno" ~ 420,
                                          NAME_1 == "Yobe" ~ 111,
                                          NAME_1 == "Adamawa" ~ 44,
                                          NAME_1 == "Gombe" ~ 27,
                                          NAME_1 == "Taraba" ~ 23,
                                          NAME_1 == "Bauchi" ~ 35,
                                          NAME_1 == "Plateau" ~ 117,
                                          NAME_1 == "Kaduna" ~ 75,
                                          NAME_1 == "Kano" ~ 110,
                                          NAME_1 == "Katsina" ~ 7,
                                          NAME_1 == "Zamfara" ~ 30,
                                          NAME_1 == "Sokoto" ~ 10,
                                          NAME_1 == "Kebbi" ~ 0,
                                          NAME_1 == "Jigawa" ~ 6,
                                          NAME_1 == "Niger" ~ 18,
                                          NAME_1 == "Kogi" ~ 16,
                                          NAME_1 == "Kwara" ~ 11,
                                          NAME_1 == "Nasarawa" ~ 23,
                                          NAME_1 == "Benue" ~ 43,
                                          NAME_1 == "Oyo" ~ 9,
                                          NAME_1 == "Osun" ~ 4,
                                          NAME_1 == "Ondo" ~ 8,
                                          NAME_1 == "Ekiti" ~ 9,
                                          NAME_1 == "Ogun" ~ 18,
                                          NAME_1 == "Lagos" ~ 37,
                                          NAME_1 == "Rivers" ~ 23,
                                          NAME_1 == "Bayelsa" ~ 27,
                                          NAME_1 == "Delta" ~ 37,
                                          NAME_1 == "Edo" ~ 22,
                                          NAME_1 == "Anambra" ~ 26,
                                          NAME_1 == "Enugu" ~ 23,
                                          NAME_1 == "Ebonyi" ~ 7,
                                          NAME_1 == "Imo" ~ 7,
                                          NAME_1 == "Abia" ~ 2,
                                          NAME_1 == "Akwa Ibom" ~ 5,
                                          NAME_1 == "Cross River" ~ 21,
                                          NAME_1 == "Benue" ~ 43,
                                          NAME_1 == "Federal Capital Territory " ~ 17
                                          ))
view(N_map)

##mappinng the number of fatalities by state 2015-2016
df_2015_2016 <- full_conf_df |> 
                  filter(COUNTRY == "Nigeria", YEAR >= 2015, YEAR <= 2016) 
view(df_2015_2016)
##converting the Event_Type variable into a numarical variable (Event_N)
df_2015_2016 <- df_2015_2016 |> 
            mutate(Event_N = case_when(
              EVENT_TYPE == "Violence against civilians" ~ 1,
              EVENT_TYPE == "Battles" ~ 1,
              EVENT_TYPE == "Explosions/Remote violence" ~ 1,
              EVENT_TYPE == "Riots/Protests" ~ 1,
              EVENT_TYPE == "Strategic developments" ~ 1,
              TRUE ~ 0))
df_2015_2016

df_2015_2016 |> 
  group_by(ADMIN1) |>
  summarise(EVENT_by_state = sum(Event_N)) |>
  arrange(desc(EVENT_by_state)) |>
print(n = 38)

##creating the number of events by state(2015_2016)
N_map <- N_map |> 
          mutate(Event_2015_16 = case_when(NAME_1 == "Borno" ~ 613,
                                          NAME_1 == "Yobe" ~ 68,
                                          NAME_1 == "Adamawa" ~ 75,
                                          NAME_1 == "Gombe" ~ 22,
                                          NAME_1 == "Taraba" ~ 45,
                                          NAME_1 == "Bauchi" ~ 19,
                                          NAME_1 == "Plateau" ~ 59,
                                          NAME_1 == "Kaduna" ~ 50,
                                          NAME_1 == "Kano" ~ 110,
                                          NAME_1 == "Katsina" ~ 7,
                                          NAME_1 == "Zamfara" ~ 30,
                                          NAME_1 == "Sokoto" ~ 2,
                                          NAME_1 == "Kebbi" ~ 3,
                                          NAME_1 == "Jigawa" ~ 4,
                                          NAME_1 == "Niger" ~ 8,
                                          NAME_1 == "Kogi" ~ 25,
                                          NAME_1 == "Kwara" ~ 11,
                                          NAME_1 == "Nasarawa" ~ 23,
                                          NAME_1 == "Benue" ~ 43,
                                          NAME_1 == "Oyo" ~ 8,
                                          NAME_1 == "Osun" ~ 4,
                                          NAME_1 == "Ondo" ~ 8,
                                          NAME_1 == "Ekiti" ~ 9,
                                          NAME_1 == "Ogun" ~ 16,
                                          NAME_1 == "Lagos" ~ 49,
                                          NAME_1 == "Rivers" ~ 93,
                                          NAME_1 == "Bayelsa" ~ 35,
                                          NAME_1 == "Delta" ~ 68,
                                          NAME_1 == "Edo" ~ 21,
                                          NAME_1 == "Anambra" ~ 26,
                                          NAME_1 == "Enugu" ~ 17,
                                          NAME_1 == "Ebonyi" ~ 15,
                                          NAME_1 == "Imo" ~ 8,
                                          NAME_1 == "Abia" ~ 2,
                                          NAME_1 == "Akwa Ibom" ~ 17,
                                          NAME_1 == "Cross River" ~ 33,
                                          NAME_1 == "Benue" ~ 101,
                                          NAME_1 == "Federal Capital Territory " ~ 23
                                          ))
view(N_map)

##mappinng the number of fatalities by state 2018-2019
df_2018_2019 <- full_conf_df |> 
                  filter(COUNTRY == "Nigeria", YEAR >= 2018, YEAR <= 2019)

df_2018_2019 <- df_2018_2019 |> 
            mutate(Event_N = case_when(
              EVENT_TYPE == "Violence against civilians" ~ 1,
              EVENT_TYPE == "Battles" ~ 1,
              EVENT_TYPE == "Explosions/Remote violence" ~ 1,
              EVENT_TYPE == "Riots/Protests" ~ 1,
              EVENT_TYPE == "Strategic developments" ~ 1,
              TRUE ~ 0))
df_2018_2019

df_2018_2019 |> 
  group_by(ADMIN1) |>
  summarise(EVENT_by_state = sum(Event_N)) |>
  arrange(desc(EVENT_by_state)) |>
print(n = 38)

##creating the number of events by state(2018_2019)
N_map <- N_map |> 
          mutate(Event_2018_19 = case_when(NAME_1 == "Borno" ~ 768,
                                          NAME_1 == "Yobe" ~ 40,
                                          NAME_1 == "Adamawa" ~ 121,
                                          NAME_1 == "Gombe" ~ 22,
                                          NAME_1 == "Taraba" ~ 152,
                                          NAME_1 == "Bauchi" ~ 19,
                                          NAME_1 == "Plateau" ~ 132,
                                          NAME_1 == "Kaduna" ~ 166,
                                          NAME_1 == "Kano" ~ 110,
                                          NAME_1 == "Katsina" ~ 94,
                                          NAME_1 == "Zamfara" ~ 186,
                                          NAME_1 == "Sokoto" ~ 48,
                                          NAME_1 == "Kebbi" ~ 3,
                                          NAME_1 == "Jigawa" ~ 4,
                                          NAME_1 == "Niger" ~ 55,
                                          NAME_1 == "Kogi" ~ 71,
                                          NAME_1 == "Kwara" ~ 11,
                                          NAME_1 == "Nasarawa" ~ 102,
                                          NAME_1 == "Benue" ~ 172,
                                          NAME_1 == "Oyo" ~ 8,
                                          NAME_1 == "Osun" ~ 4,
                                          NAME_1 == "Ondo" ~ 40,
                                          NAME_1 == "Ekiti" ~ 9,
                                          NAME_1 == "Ogun" ~ 41,
                                          NAME_1 == "Lagos" ~ 69,
                                          NAME_1 == "Rivers" ~ 115,
                                          NAME_1 == "Bayelsa" ~ 50,
                                          NAME_1 == "Delta" ~ 112,
                                          NAME_1 == "Edo" ~ 69,
                                          NAME_1 == "Anambra" ~ 38,
                                          NAME_1 == "Enugu" ~ 35,
                                          NAME_1 == "Ebonyi" ~ 36,
                                          NAME_1 == "Imo" ~ 30,
                                          NAME_1 == "Abia" ~ 24,
                                          NAME_1 == "Akwa Ibom" ~ 53,
                                          NAME_1 == "Cross River" ~ 54,
                                          NAME_1 == "Benue" ~ 172,
                                          NAME_1 == "Federal Capital Territory " ~ 26
                                          ))
view(N_map)

ggplot(N_map) +
  geom_sf(aes(fill =Event_2018_19)) +
  labs(
    title    = "Conflict Event by State",
    subtitle = "2018-2019")  +
  scale_fill_continuous(high="#378f27", low="#f2f0f7") +
  theme_bw() +
  annotation_scale(
    location = "bl",
    width_hint = 0.5,
    pad_x = unit(3, "cm")
  )
  ggsave("~/Desktop/plot_2019.png")

ggplot(N_map) +
  geom_sf(aes(fill = Event_2015_16)) +
  labs(
    title    = "Conflict Event by State",
    subtitle = "2015-2016")  +
  scale_fill_continuous(high="#378f27", low="#f2f0f7") +
  theme_bw() +
  annotation_scale(
    location = "bl",
    width_hint = 0.5,
    pad_x = unit(3, "cm")
  )
ggsave("~/Desktop/plot_2016.png")

ggplot(N_map) +
  geom_sf(aes(fill = Event_2012_13)) +
  labs(
    title    = "Conflict Event by State",
    subtitle = "2012-2013")  +
  scale_fill_continuous(high="#378f27", low="#f2f0f7") +
  theme_bw() +
  annotation_scale(
    location = "bl",
    width_hint = 0.5,
    pad_x = unit(3, "cm")
  )
ggsave("~/Desktop/plot_2013.png")

ggplot(N_map) +
  geom_sf(aes(fill = EVENT_2009_10)) +
  labs(
    title    = "Conflict Event by State",
    subtitle = "2009-2010")  +
  scale_fill_continuous(high="#378f27", low="#f2f0f7") +
  theme_bw() +
  annotation_scale(
    location = "bl",
    width_hint = 0.5,
    pad_x = unit(3, "cm"))
ggsave("~/Desktop/plot_2010.png")


# Rename education level column
data <- data |> rename(level_edu_comp = q02c06)

# ECCE (Early Childhood Care and Education) variable
data <- data |> mutate(ECCE_C = case_when(
  level_edu_comp == 0 ~ 1,
  level_edu_comp <= 5 ~ 1,
  level_edu_comp >= 6 ~ 0
))

# Primary Completed
data <- data |> mutate(PrimaryC = case_when(
  level_edu_comp >= 6 & level_edu_comp <= 8 ~ 1,
  TRUE ~ 0
))

# Lower Secondary Completed
data <- data |> mutate(L_SecondaryC = case_when(
  level_edu_comp >= 9 & level_edu_comp <= 11 ~ 1,
  level_edu_comp == 13 ~ 1,
  TRUE ~ 0
))

# Upper Secondary Completed
data <- data |> mutate(U_SecondaryC = case_when(
  level_edu_comp == 12 ~ 1,
  level_edu_comp == 14 ~ 1,
  TRUE ~ 0
))


df_Event |>
 group_by (YEAR) |>
 summarize(conflict_number = sum (FATALITIES))|>
 ungroup() |>
 print (n=28)



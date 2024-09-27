
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Data Processing ---------------------------------------------------------

df_w <- read.csv(here::here("Road_Weather_Information_Stations_20240926.csv")) %>%
  mutate(date = mdy_hms(DateTime),
         date = ceiling_date(date, "hour")) %>%
  filter(StationName == "AuroraBridge") %>%
  group_by(date) %>%
  summarize(temp_surf = mean(RoadSurfaceTemperature),
            temp_air = mean(AirTemperature)) %>%
  ungroup

df_b <- read.csv(here::here("Fremont_Bridge_Bicycle_Counter_20240926.csv")) %>%
  mutate(date = mdy_hms(Date)) %>%
  filter(date >= min(df_w$date) & date <= max(df_w$date))
names(df_b) <- c("Date", "total", "northbound", "southbound", "date")

df <- left_join(df_b, df_w, by="date") %>%
  subset(!is.na(temp_surf) & !is.na(temp_air))

openxlsx::write.xlsx(df, here::here("kids_on_bikes.xlsx"))

# Model -------------------------------------------------------------------



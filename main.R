
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lme4)


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
  subset(!is.na(temp_surf) & !is.na(temp_air)) %>%
  mutate(day_of_week = factor(weekdays(date),
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         temp_surf_m = mean(temp_surf),
         temp_air_m = mean(temp_air),
         temp_surf_a = temp_surf - temp_surf_m,
         temp_air_a = temp_air - temp_air_m,
         hour = factor(hour(date),levels=0:24),
         weekday = ifelse(day_of_week %in% c("Saturday", "Sunday"), 0, 1),
         diff = southbound - northbound)

# Model -------------------------------------------------------------------

# Expected total bikes 
m <- glmer(total ~ (1|hour) + weekday + temp_air_a, data = df, family=poisson)
df$total_pred <- predict(m, newdata=df, type="response")


# Expected difference between north/soundbound traffic
## the bigger the number, the more people going downtown
m_diff <- lmer(diff ~ total * (weekday + (1|hour)), data = df)
df$total_diff <- predict(m_diff, newdata=df, type="response")


# Calculate leverage
out <- influence(m, obs=TRUE)
df$cooks_distance <- cooks.distance(out)


openxlsx::write.xlsx(df, here::here("kids_on_bikes.xlsx"))

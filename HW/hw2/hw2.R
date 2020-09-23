library(tidyverse)

setwd("~/Documents/JHU/macroeconomic_forecasting/HW/hw2")

# read in personal consumption expenditures csv
pce <- read.csv("NA000349Q.csv") %>%
  rename("value" = "NA000349Q",
         "date" = "DATE") %>%
  mutate(date = as.Date(date),
         value = value/1000000)

# limit data to show seasonality
pce_seasonal <- read.csv("NA000349Q.csv") %>%
  rename("value" = "NA000349Q",
         "date" = "DATE") %>%
  mutate(date = as.Date(date),
         value = value/1000000) %>%
  slice(-c(1:271))

# plot pce data
pce_chart <- ggplot() +
  geom_line(data = pce, aes(x = date, y = value), color = "dark blue") +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10), plot.caption = element_text(hjust = 0)) +
  labs(x = "Year", y = "U.S.Dollars (Trillions)", 
       title = "Personal Consumption Expenditures",
       subtitle = "Frequency = Quarterly",
       caption = "Not Seasonally Adjusted
       Source = FRED") +
  scale_x_date(breaks = as.Date(c("1950-01-01", "1960-01-01", "1970-01-01", 
                                "1980-01-01", "1990-01-01", "2000-01-01",
                                "2010-01-01", "2020-01-01")), date_labels = "%Y") +
  scale_y_continuous(limits = c(0,4))

# show seasonality in data plot
seasonal_plot <- ggplot() +
  geom_line(data = pce_seasonal, aes(x = date, y = value), color = "dark red") +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10), plot.caption = element_text(hjust = 0)) +
  labs(x = "Year", y = "U.S. Dollars (Trillions)",
       title = "Personal Consumption Expenditures",
       subtitle = "Frequency = Quarterly",
       caption = "Not Seasonally Adjusted
       Source = FRED") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(2.5,4))

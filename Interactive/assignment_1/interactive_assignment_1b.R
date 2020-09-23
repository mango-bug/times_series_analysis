library("tidyverse")
library("readxl")
library("janitor")
library("lubridate")

setwd("~/Documents/JHU/macroeconomic_forecasting/Interactive")

libor <- read_excel("USD3MTD156N.xls") %>%
  rename("Date" = "FRED Graph Observations", "Rate" = "...2") %>%
  slice(-c(1:10)) %>%
  drop_na() %>%
  mutate(date = excel_numeric_to_date(as.numeric(Date))) %>%
  mutate(libor_rate = as.numeric(Rate)) %>%
  select(-c(Date, Rate))

treas_3m <- read.csv("DTB3.csv") %>%
  mutate(date = mdy(DATE), treas_3m_rate = DTB3) %>%
  select(-c(DATE, DTB3)) %>%
  drop_na() 

graph <- ggplot() +
  geom_line(data = libor, aes(x = date, y = libor_rate), color = "blue") +
  geom_line(data = treas_3m, aes(x = date, y = treas_3m_rate), color = "orange") +
  geom_hline(yintercept = 0) +
  theme_classic() + 
  labs(x = "Year", y = "%", title = "3-Month USD LIBOR vs 3-Month T-Bill Rate",
       subtitle = "Source: Federal Reserve Bank of St. Louis (FRED), https://fred.stlouisfed.org)",
       caption = "Note: The data is based on US Dollar, daily 
                  frequency, and not seasonally adjusted.
                  Date Range: 1/2/2009 - 5/21/2020") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0,3)) +
  annotate("text", x = as.Date("2019-01-01"), y = 3, label = "LIBOR") +
  annotate("text", x = as.Date("2019-01-01"), y = 2, label = "T-Bill")

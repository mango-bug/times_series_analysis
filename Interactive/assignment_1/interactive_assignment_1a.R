library("tidyverse")
library("readxl")
library("janitor")

setwd("~/Documents/JHU/macroeconomic_forecasting/Interactive")

libor <- read_excel("USD3MTD156N.xls") %>%
  rename("Date" = "FRED Graph Observations", "Rate" = "...2") %>%
  slice(-c(1:10)) %>%
  drop_na() %>%
  mutate(Date = excel_numeric_to_date(as.numeric(Date))) %>%
  mutate(Rate = as.numeric(Rate))

libor_graph <- ggplot() +
  geom_line(data = libor, aes(x = Date, y = Rate), color = "blue") +
  geom_hline(yintercept = 0) +
  theme_classic() + 
  labs(x = "Year", y = "%", title = "3-Month USD LIBOR",
       subtitle = "Source: Federal Reserve Bank of St. Louis (FRED), https://fred.stlouisfed.org)",
       caption = "Note: The data is based on US Dollar, daily 
                  frequency, and not seasonally adjusted.") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0,3))

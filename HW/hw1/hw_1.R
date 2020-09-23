library("tidyverse")
library("readxl")
library("janitor")

setwd("~/Documents/JHU/macroeconomic_forecasting/HW/hw1")

# read in and wrangle data
dollar_euro <- read_excel("hmwk1data.xls") %>%
  rename("date" = "...1", "ex_rate" = "fm") %>%
  slice(-c(1:3)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(ex_rate = as.numeric(ex_rate)) %>%
  mutate(ex_diff = (ex_rate - lag(ex_rate))/ex_rate) %>%
  #mutate(ex_diff = ((ex_rate/1.18)-1)) %>%
  mutate(diff_lag = lag(ex_diff))

# export df to regress using stata
write.csv(dollar_euro, "hw1_1.csv", row.names = F, col.names = T)

# 1a
dollar_euro_graph <- ggplot() +
  geom_line(data = dollar_euro, aes(x = date, y = ex_rate), color = "blue") +
  geom_hline(yintercept = 0) +
  theme_classic() + 
  labs(x = "Year", y = "Dollar/Euro", title = "Dollar-Euro Exchange Rate") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(.8,1.8)) 

# 1b (% change in exchange rate)
perct_chg <- ggplot() +
  geom_line(data = dollar_euro, aes(x = date, y = ex_diff), color = "dark green") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  labs(x = "Year", y = "Change", title = "Percent Change in Dollar-Euro Exchange Rate") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent)

# 1c (% change lagged 1 period)
lagged_chg <- ggplot() +
  geom_point(data = dollar_euro, aes(x = diff_lag, y = ex_diff), color = "dark red") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_classic() +
  labs(x = "% change lagged one period", y = "% change in current period", 
       title = "Daily Dollar-Euro Ex. Rate % Change vs Lagged % Change") +
  scale_x_continuous(limits = c(-.04, .04), labels = scales::percent) +
  scale_y_continuous(limits = c(-.04, .04), labels = scales::percent)
  
# 1d (run regression on pct change series on lagged pct change series)
reg <- lm(ex_diff ~ diff_lag, data = dollar_euro)
residuals <- resid(reg)
summary(reg)

residuals <- as.data.frame(residuals) %>%
  mutate(time = row_number())

resid_plot <- ggplot() +
  geom_line(data = residuals, aes(x = time, y = residuals))

# 1e (residual plot)
residual_plot <- ggplot(lm(ex_diff ~ diff_lag, data = dollar_euro)) +
  geom_point(aes(x = .fitted, y = .resid), color = "purple") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  theme_classic() +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 15)) +
  labs(x = "Fitted", y = "Residuals", title = "Residual Plot % Change vs Lagged % Change Regression") 



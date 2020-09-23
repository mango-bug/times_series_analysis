library("tidyverse")
library("tseries")

setwd("~/Documents/JHU/macroeconomic_forecasting/HW")

# read in .dat and create log of exchange rate and lagged log of exchange rate
hun_dollar <- read.delim("hmwk week 1.dat") %>%
  mutate(time = row_number(),
         log_exch = log(exchrate),
         lagged_log = lag(log_exch),
         log_diff = log_exch - lag(log_exch)) %>%
  relocate(time, .before = exchrate)

# 1_2a (take log and produce time series plot of log of $/Ft exchange rate)
log_chart <- ggplot() +
  geom_line(data = hun_dollar, aes(x = time, y = log_exch), color = "dark red") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Days", y = "Log of Exchange Rate", title = "Log of Dollar-Forint Exchange Rate (Time Series)") +
  scale_x_continuous(breaks = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650))

# 1_2b (chart ln($/ft) against lagged ln($/ft))
lagged_chrt <- ggplot() +
  geom_point(data = hun_dollar, aes(x = log_exch, y = lagged_log), color = "dark green") +
  #geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 0) +
  theme_classic() +
  labs(x = "lagged ln($/ft)", y = "ln($/ft)", title = "Log vs Lagged Log of $/FT Exchange Rate") +
  scale_x_continuous(limits = c(-1,4)) +
  scale_y_continuous(limits = c(-1,4))

# 1_2c (create change in ln($/ft).  test for normality)
log_diff_norm <- ggplot(data = hun_dollar, aes(x = log_diff)) +
  geom_histogram() +
  theme_classic() +
  labs(x = "log difference", y = "count", title = "Change in ln($/ft) Distribution") +
  scale_x_continuous(limits = c(-2,2)) +
  scale_y_continuous(limits = c(0,400))

# jacque-bera test
jarque_log_diff <- as.data.frame(hun_dollar$log_diff) %>%
  drop_na() %>%
  rename("log_diff" = "hun_dollar$log_diff")

diff_data <- jarque_log_diff$log_diff

jarque_test <- jarque.bera.test(diff_data)

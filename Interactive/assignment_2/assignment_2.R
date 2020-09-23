library(tidyverse)
library(readxl)
library(janitor)
library(forecast)

setwd("~/Documents/JHU/macroeconomic_forecasting/Interactive/assignment_2")

# bring in china imports data and clean
china_imports <- read_excel("Interactive unit 2_1.xls") %>%
  slice(-c(1:17)) %>%
  rename("date" = "Title:",
         "value" = "Goods, Value of Imports for China©") %>%
  mutate(date = excel_numeric_to_date(as.numeric(date)),
         value = as.numeric(value)/1000000000,
         log_value = log(value),
         time = row_number(),
         feb = as.numeric(as.numeric(month(ymd(china_imports$date))) == 2),
         mar = as.numeric(as.numeric(month(ymd(china_imports$date))) == 3),
         apr = as.numeric(as.numeric(month(ymd(china_imports$date))) == 4),
         may = as.numeric(as.numeric(month(ymd(china_imports$date))) == 5),
         jun = as.numeric(as.numeric(month(ymd(china_imports$date))) == 6),
         jly = as.numeric(as.numeric(month(ymd(china_imports$date))) == 7),
         aug = as.numeric(as.numeric(month(ymd(china_imports$date))) == 8),
         sep = as.numeric(as.numeric(month(ymd(china_imports$date))) == 9),
         oct = as.numeric(as.numeric(month(ymd(china_imports$date))) == 10),
         nov = as.numeric(as.numeric(month(ymd(china_imports$date))) == 11),
         dec = as.numeric(as.numeric(month(ymd(china_imports$date))) == 12),
         early = as.numeric(as.numeric(year(ymd(china_imports$date))) <= 1998),
         febe = as.numeric(feb == 1 & early == 1),
         mare = as.numeric(mar == 1 & early == 1),
         apre = as.numeric(apr == 1 & early == 1),
         maye = as.numeric(may == 1 & early == 1),
         june = as.numeric(jun == 1 & early == 1),
         jlye = as.numeric(jly == 1 & early == 1),
         auge = as.numeric(aug == 1 & early == 1),
         sepe = as.numeric(sep == 1 & early == 1),
         octe = as.numeric(oct == 1 & early == 1),
         nove = as.numeric(nov == 1 & early == 1),
         dece = as.numeric(dec == 1 & early == 1)) %>%
  relocate(time, .before = date)

# create initial chart of china imports
imports_graph <- ggplot() +
  geom_line(data = china_imports, aes(x = date, y = value), color = "dark red") +
  #geom_line(data = china_imports, aes(x = date, y = log_value, color = "green")) + 
  geom_smooth(method = "lm", formula = log(value) ~ date, se = F) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10), plot.caption = element_text(hjust = 0)) +
  labs(x = "Year", y = "U.S. Dollars (Billions)", title = "Value of China Imports",
       subtitle = "Frequency = Monthly",
       caption = "
       Data not seasonally adjusted
       Source: IMF") +
  #scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0,200))

# linear regression of china imports and residual plots
exp_model <- lm(log(value) ~ time, data = china_imports)
quad_model <- lm(value ~ time + I(time^2), data = china_imports)
log_model <- lm(value ~ exp(time), data = china_imports)

summary(exp_model)
summary(quad_model)

# find residuals
china_imports$exp_resid <- exp(resid(exp_model))
china_imports$quad_resid <- resid(quad_model)

# find predicted (fitted) values
china_imports$exp_predict <- exp(fitted((exp_model)))
china_imports$quad_predict <- predict(quad_model)

# exponential fit plot
exp_fit_plot <- ggplot() + 
  geom_line(data = china_imports, aes(x = date, y = value), color = "dark red")  
  geom_line(data = china_imports, aes(x = date, y = exp_predict), color = "dark green")

# quadratic fit plot
quad_fit_plot <- ggplot() + 
  geom_line(data = china_imports, aes(x = date, y = value), color = "dark red")+ 
  geom_line(data = china_imports, aes(x = date, y = quad_predict), color = "dark green")

# exponential residual plot
exp_resid_plot <- ggplot() +
  geom_line(data = china_imports, aes(x = date, y = exp_resid), color = "dark blue") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Exponential Trend Residual Plot")

# quadratic residual plot
quad_resid_plot <- ggplot() +
  geom_line(data = china_imports, aes(x = date, y = quad_resid), color = "dark green") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Quadratic Trend Residual Plot")



# use exponential model on data
scatter.smooth(x = china_imports$time, y = china_imports$value,
               xlab = "Time", ylab = "Value of Imports")

# calculate AIC and SIC(BIC)
AIC(log_model)
AIC(quad_model)

BIC(log_model)
BIC(quad_model)


# model with seasonality (monthly)
# linear regression of china imports and residual plots
exp_seasonal_model <- lm(log(value) ~ time + feb + mar + jun + jly + aug + sep + oct + nov + dec, data = china_imports)
quad_seasonal_model <- lm(value ~ time + I(time^2) + feb + mar + jun + jly + aug + sep + oct + nov + dec, data = china_imports)
log_seasonal_model <- lm(value ~ exp(time) + feb + mar + jun + jly + aug + sep + oct + nov + dec, data = china_imports)

summary(exp_seasonal_model)
summary(quad_seasonal_model)

# find residuals
china_imports$exp_seasonal_resid <- exp(resid(exp_seasonal_model))
china_imports$quad_seasonal_resid <- resid(quad_seasonal_model)

# find predicted (fitted) values
china_imports$exp_seasonal_predict <- exp(fitted((exp_seasonal_model)))
china_imports$quad_seasonal_predict <- predict(quad_seasonal_model)

# exponential fit plot
exp_seasonal_fit_plot <- ggplot() + 
  geom_line(data = china_imports, aes(x = date, y = value), color = "dark red") + 
  geom_line(data = china_imports, aes(x = date, y = exp_seasonal_predict), color = "dark green")+
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Exponential Trend and Seasonality Fit Plot")

# quadratic fit plot
quad_seasonal_fit_plot <- ggplot() + 
  geom_line(data = china_imports, aes(x = date, y = value), color = "dark red")+ 
  geom_line(data = china_imports, aes(x = date, y = quad_seasonal_predict), color = "dark green") +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Quadratic Trend and Seasonality Fit Plot")

# exponential residual plot
exp_seasonal_resid_plot <- ggplot() +
  geom_line(data = china_imports, aes(x = date, y = exp_seasonal_resid), color = "dark blue") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Exponential Trend and Seasonality Residual Plot")

# quadratic residual plot
quad_seasonal_resid_plot <- ggplot() +
  geom_line(data = china_imports, aes(x = date, y = quad_seasonal_resid), color = "dark green") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Quadratic Trend and Seasonality Residual Plot")


# calculate AIC and SIC(BIC)
AIC(log_seasonal_model)
AIC(quad_seasonal_model)

BIC(log_seasonal_model)
BIC(quad_seasonal_model)

# model with seasonal and change in seasonality dummy variables
# linear regression of china imports and residual plots
exp_seasonal_chg_model <- lm(log(value) ~ early + time + feb + mar + jun + jly + aug + sep + oct + nov + dec
                             + febe + mare + apre + maye + june + jlye + auge + sepe + octe + nove + dece, data = china_imports)
quad_seasonal_chg_model <- lm(value ~ early + time + I(time^2) + feb + mar + jun + jly + aug + sep + oct + nov + dec
                              + febe + mare + apre + maye + june + jlye + auge + sepe + octe + nove + dece, data = china_imports)
log_seasonal_chg_model <- lm(value ~ early + exp(time) + feb + mar + jun + jly + aug + sep + oct + nov + dec
                             + febe + mare + apre + maye + june + jlye + auge + sepe + octe + nove + dece, data = china_imports)

summary(exp_seasonal_chg_model)
summary(quad_seasonal_chg_model)

# find residuals
china_imports$exp_seasonal_chg_resid <- exp(resid(exp_seasonal_chg_model))
china_imports$quad_seasonal_chg_resid <- resid(quad_seasonal_chg_model)

# find predicted (fitted) values
china_imports$exp_seasonal_chg_predict <- exp(fitted((exp_seasonal_chg_model)))
china_imports$quad_seasonal_chg_predict <- predict(quad_seasonal_chg_model)

# exponential fit plot
exp_seasonal_chg_fit_plot <- ggplot() + 
  geom_line(data = china_imports, aes(x = date, y = value), color = "dark red") + 
  geom_line(data = china_imports, aes(x = date, y = exp_seasonal_chg_predict), color = "dark green")+
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Value of Goods Imports from China w/ Change in Seasonality",
       subtitle = "Exponential Trend")

# quadratic fit plot
quad_seasonal_chg_fit_plot <- ggplot() + 
  geom_line(data = china_imports, aes(x = date, y = value), color = "dark red")+ 
  geom_line(data = china_imports, aes(x = date, y = quad_seasonal_chg_predict), color = "dark green") +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Value of Goods Imports from China w/ Change in Seasonality",
       subtitle = "Quadratic Trend")

# exponential residual plot
exp_seasonal_chg_resid_plot <- ggplot() +
  geom_line(data = china_imports, aes(x = date, y = exp_seasonal_chg_resid), color = "dark blue") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Value of Goods Imports from China w/ Change in Seasonality Residuals",
       subtitle = "Exponential Trend")

# quadratic residual plot
quad_seasonal_chg_resid_plot <- ggplot() +
  geom_line(data = china_imports, aes(x = date, y = quad_seasonal_chg_resid), color = "dark green") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "US Dollar (Billions)", title = "Value of Goods Imports from China w/ Change in Seasonality Residuals",
       subtitle = "Quadratic Trend")


# calculate AIC and SIC(BIC)
AIC(log_seasonal_chg_model)
AIC(quad_seasonal_chg_model)

BIC(log_seasonal_chg_model)
BIC(quad_seasonal_chg_model)


# export df to regress using stata
write.csv(china_imports, "ia_2.csv", row.names = F, col.names = T)
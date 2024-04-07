install.packages("ggplot2")
install.packages("tidyverse")

library(ggplot2)
library(tidyverse)

df_assets <- read.csv("../projeto-estat/assets.csv")
df_assets <- na.omit(df_assets)

nivelConfianca <- 0.95
alpha <- 1 - nivelConfianca

mean_petr <- mean(df_assets$PETR4.SA);
median_petr <- median(df_assets$PETR4.SA)
devPad_petr <- sd(df_assets$PETR4.SA)
var_petr <- var(df_assets$PETR4.SA)
mode_petr <- as.numeric(names(sort(-table(df_assets$PETR4.SA)))[1])

# WEG
mean_wege <- mean(df_assets$WEGE3.SA);
median_wege <- median(df_assets$WEGE3.SA)
devPad_wege <- sd(df_assets$WEGE3.SA)
var_wege <- var(df_assets$WEGE3.SA)
mode_wege <- as.numeric(names(sort(-table(df_assets$WEGE3.SA)))[1])

# Ambev
mean_abev <- mean(df_assets$ABEV3.SA);
median_abev <- median(df_assets$ABEV3.SA)
devPad_abev <- sd(df_assets$ABEV3.SA)
var_abev <- var(df_assets$ABEV3.SA)
mode_abev <- as.numeric(names(sort(-table(df_assets$ABEV3.SA)))[1])

# Vale
mean_vale <- mean(df_assets$VALE3.SA);
median_vale <- median(df_assets$VALE3.SA)
devPad_vale <- sd(df_assets$VALE3.SA)
var_vale <- var(df_assets$VALE3.SA)
mode_vale <- as.numeric(names(sort(-table(df_assets$VALE3.SA)))[1])

calc_returns = function(data) {
  for(i in 2:nrow(data)) {
    data$return[i] = log(data$close[i] / data$close[i-1])
  }
  
  clean_data = data[-1,]
  
  return (clean_data)
}

df_petr = data.frame(df_assets$PETR4.SA)
colnames(df_petr) = ("close")
df_petr$return = 0
df_petr = calc_returns(df_petr)

df_wege = data.frame(df_assets$WEGE3.SA)
colnames(df_wege) = ("close")
df_wege$return = 0
df_wege = calc_returns(df_wege)

df_abev = data.frame(df_assets$ABEV3.SA)
colnames(df_abev) = ("close")
df_abev$return = 0
df_abev = calc_returns(df_abev)

df_vale = data.frame(df_assets$VALE3.SA)
colnames(df_vale) = ("close")
df_vale$return = 0
df_vale = calc_returns(df_vale)

df_returns = data.frame(df_petr$return, df_wege$return, df_abev$return, df_vale$return)
colnames(df_returns) = c("petr", "wege", "abev", "vale")

cor_returns = cor(df_returns)
cov_returns = cov(df_returns)

cov_returns_by_year = cov(df_returns) * 252

petr_mean_return = mean(df_returns$petr) * 252
wege_mean_return = mean(df_returns$wege) * 252
abev_mean_return = mean(df_returns$abev) * 252
vale_mean_return = mean(df_returns$vale) * 252

volatility_returns = sqrt(cov_returns_by_year)
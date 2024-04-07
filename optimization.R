install.packages("ggplot2");
install.packages("tidyverse");
install.packages("languageserver")

install.packages("quadprog")
install.packages("tidyquant")

library(quadprog)
library(tidyquant)

library(ggplot2);
library(tidyverse);
library(readr);


df_assets <- read.csv("assets.csv")


nivelConfianca <- 0.95;
alpha <- 1 - nivelConfianca;

z <- qnorm(1 - alpha/2);

calculaIntervaloDeConfianca <- function(mediaAm, devPad, numElementos, valorCritico) {
  return (list(mediaAm - valorCritico*devPad/(sqrt(numElementos)), mediaAm + valorCritico*devPad/(sqrt(numElementos))));
}


# Análise quantitativa:
# Petrobras
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

# Boxplots:
# Petrobras
ggplot(df_assets, aes(x = "PETR4.SA", y = df_assets$PETR4.SA)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Petrobras", x = "Asset", y = "Value") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("petrobras_boxplot.png")

# WEG
ggplot(df_assets, aes(x = "WEGE3.SA", y = df_assets$WEGE3.SA)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "WEG", x = "Asset", y = "Value") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("weg_boxplot.png")

# Ambev
ggplot(df_assets, aes(x = "ABEV3.SA", y = df_assets$ABEV3.SA)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Ambev", x = "Asset", y = "Value") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("ambev_boxplot.png")

# Vale
ggplot(df_assets, aes(x = "VALE3.SA", y = df_assets$VALE3.SA)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Vale", x = "Asset", y = "Value") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("vale_boxplot.png")


# Histogramas:
#Petrobras
ggplot(df_assets, aes(x = PETR4.SA)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Petrobras", x = "Value", y = "Frequência") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("petrobras_histogram.png")


# WEG
ggplot(df_assets, aes(x = WEGE3.SA)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "WEG", x = "Value", y = "Frequência") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("weg_histogram.png")

# Ambev
ggplot(df_assets, aes(x = ABEV3.SA)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Ambev", x = "Value", y = "Frequência") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("ambev_histogram.png")

# Vale
ggplot(df_assets, aes(x = VALE3.SA)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Vale", x = "Value", y = "Frequência") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("vale_histogram.png")


# Markowitz weights analysis
weights <- c(0.25, 0.25, 0.25, 0.25)  # Example weights for the assets
portfolio_return <- sum(weights * c(mean_petr, mean_wege, mean_abev, mean_vale), na.rm = TRUE)
portfolio_volatility <- sqrt(sum(weights^2 * c(var_petr, var_wege, var_abev, var_vale), na.rm = TRUE))

# Print portfolio return and volatility
cat("Portfolio Return:", portfolio_return, "\n")
cat("Portfolio Volatility:", portfolio_volatility, "\n")

# Calculate the efficient frontier
cov_matrix <- cov(df_assets[, c("PETR4.SA", "WEGE3.SA", "ABEV3.SA", "VALE3.SA")])
portfolios <- portfolio.optim(covmat = cov_matrix, pm = mean(df_assets[, c("PETR4.SA", "WEGE3.SA", "ABEV3.SA", "VALE3.SA")]), 
                              pmTarget = seq(0.01, 0.1, by = 0.01), short = FALSE)

# Plot the efficient frontier
plot(portfolios$ps, portfolios$pm, type = "l", xlab = "Volatility", ylab = "Return", main = "Efficient Frontier")
points(portfolio_volatility, portfolio_return, col = "red", pch = 16)
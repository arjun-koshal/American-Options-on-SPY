---
  title: "FE 620 - Final Project"
author: "Arjun Koshal"
date: "12/8/2022"
output: pdf_document
---

# Libraries

library(quantmod) # quantmod for data 
library(tseries)
library(greeks) # greeks for greek calculation
library(derivmkts) # better greek calculation & prices
library(ggplot2) # for plotting
library(bizdays) # for business days
library(RQuantLib) # for calendars for business days
library(gt) # for table visualization


# Data Handling


# read in CSV data
call <- read.csv('call.csv')
put <- read.csv('put.csv')

# rename columns for data
names(call) <- c('Strike', '18-Nov', '16-Dec', '30-Dec', '20-Jan', '17-Feb')
names(put) <- c('Strike', '18-Nov', '16-Dec', '30-Dec', '20-Jan', '17-Feb')

# retrieve strike prices
call_strikes <- call$Strike
put_strikes <- put$Strike

# retrieve market prices for calls & puts
call_nov_18_market_price <- call$`18-Nov`
put_nov_18_market_price <- put$`18-Nov`

# output top of both csv files
head(call)
head(put)

total_trading_days <- 252

# load in QuantLib NYSE calendar
load_quantlib_calendars("UnitedStates/NYSE", from = as.Date("2022-10-18"), to = as.Date("2022-11-18"))

# caluculate number of trading days using bizdays library and NYSE calendar
num_of_trading_days <- bizdays(as.Date("2022-10-28"), as.Date("2022-11-18"), "QuantLib/UnitedStates/NYSE")

# compute time to expiration using number of trading days and total trading days
time_to_expiration <- num_of_trading_days / total_trading_days




# set random seed to regenerate values
set.seed(123)

# retrieve SPY data as time series object
getSymbols("SPY", src="yahoo", from="2022-04-28", to="2022-10-29")

# plot closing price
names(SPY)
plot(Cl(SPY), main="SPY Price")

# retrieve past closing prices for SPY
past_closing_prices <- Cl(SPY)

head(past_closing_prices)
tail(past_closing_prices)

n <- length(past_closing_prices)

# retrieve spot price or closing price of SPY on October 28, 2022
spot_price <- as.numeric(last(past_closing_prices))

# retrieve log returns of SPY
logret <- periodReturn(past_closing_prices, period="daily", type="log")

# plot log returns of SPY
plot(logret, main="SPY Daily Log Returns")

# display QQ plot of log-returns
qqnorm(logret, main="Q-Q plot of the log-returns")

# perform jarque bera test of log returns
jarque.bera.test(logret)

# compute annualized volatility
historic_vol <- sqrt(252) * sd(logret) # sigma = 26.069% +- 1.64%
historic_vol_percent <- historic_vol * 100

round(historic_vol_percent, digits = 4)

# compute historical volatiliy error
historic_vol_error <- historic_vol / sqrt(2 * n)
historic_vol_error * 100

# retrieve risk free rates
getSymbols("^IRX", src="yahoo", from="2022-07-28", to="2022-10-29") # 3-Month T-Bill
getSymbols("^TNX", src="yahoo", from="2021-10-28", to="2022-10-29") # 10-Year T-Bill

# omit NA values to plot easier
IRX <- na.omit(IRX)

# plot 3-month T-Bill
names(IRX)
plot(Cl(IRX), main = "13-Week Treasury Bill Yield")
risk_free_rate <- as.numeric(last(Cl(IRX))) / 100 # retrieve risk-free rate on October 28, 2022

# as.numeric(risk_free_rate)

ten_year_rfr <- last(TNX$TNX.Close)
ten_year_rfr
as.numeric(ten_year_rfr) / 100



call_strikes
put_strikes

# Puts



# create separate vectors for put prices and greeks
output_put_prices <- c()
output_put_deltas <- c()
output_put_gammas <- c()
output_put_thetas <- c()

# iterate to price option expiring on Nov 18th on Oct 28th with binomopt package
for(i in 1:length(put_strikes)) {
  x <- binomopt(s = spot_price, 
                k = put_strikes[i], 
                v = historic_vol, 
                r = risk_free_rate, 
                tt = time_to_expiration, 
                d = 0, 
                nstep = 100, 
                american = TRUE, 
                putopt = TRUE,
                returngreeks = TRUE
  )
  output_put_prices <- c(output_put_prices, as.numeric(x[1]))
  output_put_deltas <- c(output_put_deltas, as.numeric(x[2]))
  output_put_gammas <- c(output_put_gammas, as.numeric(x[3]))
  output_put_thetas <- c(output_put_thetas, as.numeric(x[4]))
}

# print(output_put_deltas)
# print(output_put_gammas)
# print(output_put_thetas)

# print(put_strikes)

# create dataframe for delta
put_greeks_df <- data.frame(output_put_prices, put_strikes, output_put_deltas, output_put_gammas, output_put_thetas)
put_greeks_df


## Delta Put Plot

# setup to save plot as PNG
png("SPY American Put Option - Delta (Put) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# set ggplot theme
theme_set(theme_bw())

# ggplot for delta puts
# aes = axies
# geom_point = point 
# labs = axis labels
delta_puts <- ggplot(put_greeks_df, aes(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_deltas)) + geom_point(color = "firebrick") + labs(x = "Strike Prices", y = "Delta (P)", title = "Delta (Put)") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                           axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14))

delta_puts
ggsave("GGPlot ~ SPY American Put Option - Delta (Put) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# plot Delta (Put) vs. Strike Prices
plot(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_deltas, main = "Delta (Put)", xlab = "Strike Prices", ylab = "Delta (Put)")

# save png file
while (!is.null(dev.list()))  dev.off()


## Gamma Put Plot


# setup to save plot as PNG
png("SPY American Put Option - Strike Prices vs. Gamma (Put) on October 28th, Expiration = 11-18-22.png")

# set ggplot theme
theme_set(theme_bw())

# ggplot for delta puts
# aes = axies
# geom_point = point 
# labs = axis labels
delta_puts <- ggplot(put_greeks_df, aes(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_gammas)) + geom_point(color = "firebrick") + labs(x = "Strike Prices", y = "Gamma (P)", title = "Gamma (Put)") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                           axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14))

delta_puts
ggsave("GGPlot ~ SPY American Put Option - Gamma (Put) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# save png file
while (!is.null(dev.list()))  dev.off()


## Theta Put Plot


# setup to save plot as PNG
png("SPY American Put Option - Strike Prices vs. Theta (Put) on October 28th, Expiration = 11-18-22.png")

# set ggplot theme
theme_set(theme_bw())

# ggplot for delta puts
# aes = axies
# geom_point = point 
# labs = axis labels
delta_puts <- ggplot(put_greeks_df, aes(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_thetas)) + geom_point(color = "firebrick") + labs(x = "Strike Prices", y = "Theta (P)", title = "Theta (Put)") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                           axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14))

delta_puts
ggsave("GGPlot ~ SPY American Put Option - Theta (Put) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# plot Gamma (Put) vs. Strike Prices
plot(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_thetas, main = "Theta (Put)", xlab = "Strike Prices", ylab = "Theta (Put)")

# save png file
while (!is.null(dev.list()))  dev.off()


## Greek Put Plots


# setup to save plot as PNG
png("SPY American Put Option - Greeks (Put) vs. Strike Prices (Put) on October 28th, Expiration = 11-18-22.png")

par(mfrow = c(1,3)) # set plotting area into 1x3 array

# plot Delta (Put) vs. Strike Prices
plot(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_deltas, main = "Delta (Put)", xlab = "Strike Prices", ylab = "Delta (Put)")

# plot Gamma (Put) vs. Strike Prices
plot(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_gammas, main = "Gamma (Put)", xlab = "Strike Prices", ylab = "Gamma (Put)")

# plot Theta (Put) vs. Strike Prices
plot(x = put_greeks_df$put_strikes, y = put_greeks_df$output_put_thetas, main = "Theta (Put)", xlab = "Strike Prices", ylab = "Theta (Put)")

# save png file
while (!is.null(dev.list()))  dev.off()


# Calls


# create separate vectors for put prices and greeks
output_call_prices <- c()
output_call_deltas <- c()
output_call_gammas <- c()
output_call_thetas <- c()

# iterate to price option expiring on Nov 18th on Oct 28th with binomopt package
for(i in 1:length(call_strikes)) {
  x <- binomopt(s = spot_price, 
                k = put_strikes[i], 
                v = historic_vol, 
                r = risk_free_rate, 
                tt = time_to_expiration, 
                d = 0, 
                nstep = 100, 
                american = TRUE, 
                putopt = FALSE,
                returngreeks = TRUE
  )
  output_call_prices <- c(output_call_prices, as.numeric(x[1]))
  output_call_deltas <- c(output_call_deltas, as.numeric(x[2]))
  output_call_gammas <- c(output_call_gammas, as.numeric(x[3]))
  output_call_thetas <- c(output_call_thetas, as.numeric(x[4]))
}

# print(output_call_deltas)
# print(output_call_gammas)
# print(output_call_thetas)

# print(call_strikes)

# create dataframe for delta
call_greeks_df <- data.frame(output_call_prices, call_strikes, output_call_deltas, output_call_gammas, output_call_thetas)
call_greeks_df


## Delta Call Plot


# setup to save plot as PNG
png("SPY American Call Option - Delta (Call) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# set ggplot theme
theme_set(theme_bw())

# ggplot for delta calls
# aes = axies
# geom_point = point 
# labs = axis labels
delta_calls <- ggplot(call_greeks_df, aes(x = call_greeks_df$call_strikes, y = call_greeks_df$output_call_deltas)) + geom_point(color = "blue") + labs(x = "Strike Prices", y = "Delta (C)", title = "Delta (Call)") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                             axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14))

delta_calls
ggsave("GGPlot ~ SPY American Call Option - Delta (Call) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# plot Delta (Call) vs. Strike Prices
plot(x = call_greeks_df$call_strikes, y = call_greeks_df$output_call_deltas, main = "Delta (Call)", xlab = "Strike Prices", ylab = "Delta (C)")

# save png file
while (!is.null(dev.list()))  dev.off()


## Gamma Call Plot


# setup to save plot as PNG
png("SPY American Call Option - Gamma (Call) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# set ggplot theme
theme_set(theme_bw())

# ggplot for delta calls
# aes = axies
# geom_point = point 
# labs = axis labels
gamma_calls <- ggplot(call_greeks_df, aes(x = call_greeks_df$call_strikes, y = call_greeks_df$output_call_gammas)) + geom_point(color = "blue") + labs(x = "Strike Prices", y = "Gamma (C)", title = "Gamma (Call)") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                             axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14))

gamma_calls
ggsave("GGPlot ~ SPY American Call Option - Gamma (Call) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# plot Delta (Call) vs. Strike Prices
plot(x = call_greeks_df$call_strikes, y = call_greeks_df$output_call_gammas, main = "Gamma (Call)", xlab = "Strike Prices", ylab = "Gamma (C)")

# save png file
while (!is.null(dev.list()))  dev.off()


## Theta Call Plot


# setup to save plot as PNG
png("SPY American Call Option - Theta (Call) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# set ggplot theme
theme_set(theme_bw())

# ggplot for delta calls
# aes = axies
# geom_point = point 
# labs = axis labels
theta_calls <- ggplot(call_greeks_df, aes(x = call_greeks_df$call_strikes, y = call_greeks_df$output_call_thetas)) + geom_point(color = "blue") + labs(x = "Strike Prices", y = "Theta (C)", title = "Theta (Call)") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                             axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14))

theta_calls
ggsave("GGPlot ~ SPY American Call Option - Theta (Call) vs. Strike Prices on October 28th, Expiration = 11-18-22.png")

# plot Delta (Call) vs. Strike Prices
plot(x = call_greeks_df$call_strikes, y = call_greeks_df$output_call_thetas, main = "Theta (Call)", xlab = "Strike Prices", ylab = "Theta (C)")

# save png file
while (!is.null(dev.list()))  dev.off()


# Results


# actual call prices for nov 18
call_nov_18_market_price

# outputed call prices for nov 18
output_call_prices

# compute difference between call and market
call_price_diff <- call_nov_18_market_price - output_call_prices

call_price_diff

# actual put prices for nov 18
put_nov_18_market_price

# outputed put prices for nov 18
output_put_prices

# compute difference between put and market
put_price_diff <- call_nov_18_market_price - output_put_prices

put_price_diff

# relative error computation for call
call_price_relative_error <- abs( (output_call_prices - call_nov_18_market_price) / call_nov_18_market_price )

call_price_relative_error

# relative error computation for put
put_price_relative_error <- abs( (output_put_prices - put_nov_18_market_price) / (put_nov_18_market_price) )

put_price_relative_error

# create dataframe of results
results_df <- data.frame(call_strikes, call_nov_18_market_price, round(output_call_prices, 3),  put_nov_18_market_price, round(output_put_prices, 3), round(call_price_diff, 3), round(put_price_diff, 3), round(call_price_relative_error, 3), round(put_price_relative_error, 3))

# rename columns
names(results_df) <- c("Strike Price" , "Call Option", "Modeled Call Option", "Put Option", "Modeled Put Option", "Call Price Difference", "Put Price Difference", "Call Price Relative Error", "Put Price Relative Error")

# output results
results_df

# compute average relative errors for calls and puts
average_call_relative_error <- round(mean(call_price_relative_error) * 100, 3)
average_put_relative_error <- round(mean(put_price_relative_error) * 100, 3)

paste0("Average Call Relative Error: ", average_call_relative_error, "%")
paste0("Average Put Relative Error: ", average_put_relative_error, "%")

# create gt table of hedging dataframe
gt_results_table <- gt(results_df)

gt_results_table

# edit table
(gt_results_table <-
    
    # use pipe operator %>% to allow for easier argument implementation
    gt_results_table %>%
    
    # use md to allow for markdown
    tab_header(
      title = md("Results from Option Pricing Model"),
      subtitle = md("Strikes *K = 382 - 396* expiring *Nov-18-2022*")
    ) %>%
    
    # add style for title
    tab_style(
      locations = cells_title(groups = 'title'),
      style = list(cell_text(weight = 'bold', size = 20))
    ) %>%
    
    # add style for subtitle
    tab_style(
      locations = cells_title(groups = 'subtitle'),
      style = list(cell_text(size = 17))
    ) %>%
    
    # add style for columns
    tab_style(
      # specify all columns to style
      locations = cells_column_labels(columns = everything()),
      style = list(
        # give thick border below columns
        cell_borders(sides = 'bottom', weight = px(3)),
        # make text bold
        cell_text(weight = 'bold')
      )
    )
)

gt_results_table %>% gtsave(filename = "Results from Option Pricing Model.png", path = getwd())


# Delta Hedging

## Future Prices Setup


# source data from October 29 - November 18th for option expiring on Nov. 18
getSymbols("SPY", src="yahoo", from="2022-10-29", to="2022-11-19")

# get future prices
SPY_future_prices <- Cl(SPY)
SPY_future_prices[length(SPY_future_prices)] <- 390.80

# look at top and bottom of future prices
head(SPY_future_prices)
tail(SPY_future_prices)


## Future Prices for Calls



# create vectors for future option prices
output_call_future_prices <- c()
output_call_future_deltas <- c()
output_call_future_gammas <- c()
output_call_future_thetas <- c()
output_call_future_time_to_expiration <- c()

# create counter for decreasing time to expiration
count <- 0

# calculate price of option & greeks for strike at 382 until expiration
for(i in 1:length(SPY_future_prices))
{
  x <- binomopt(s = as.numeric(SPY_future_prices[i]), 
                k = call_strikes[1], 
                v = historic_vol, 
                r = risk_free_rate, 
                tt = (num_of_trading_days - count)/total_trading_days, 
                d = 0, 
                nstep = 100, 
                american = TRUE, 
                putopt = FALSE,
                returngreeks = TRUE
  )
  
  print(x)
  print(count)
  
  output_call_future_prices <- c(output_call_future_prices, as.numeric(x[1]))
  output_call_future_deltas <- c(output_call_future_deltas, as.numeric(x[2]))
  output_call_future_gammas <- c(output_call_future_gammas, as.numeric(x[3]))
  output_call_future_thetas <- c(output_call_future_thetas, as.numeric(x[4]))
  output_call_future_time_to_expiration <- c(output_call_future_time_to_expiration, as.numeric(x[9]))
  
  count <- count + 1
}

# display output vectors
output_call_future_prices
output_call_future_deltas
output_call_future_gammas
output_call_future_thetas
output_call_future_time_to_expiration

# comprise dataframe for call option till expiration at strike k = $382
future_prices_df <- data.frame(output_call_future_prices, output_call_future_deltas, output_call_future_gammas, output_call_future_thetas, output_call_future_time_to_expiration)

head(future_prices_df)
tail(future_prices_df)


## Theta (Call) vs. Time to Expiration


# plot Theta vs. Time to Expiration (Days); multiply by 252 to convert to days and 
# reverse x scale
ggplot(future_prices_df, aes((x = future_prices_df$output_call_future_time_to_expiration * 252), y = future_prices_df$output_call_future_thetas)) + geom_point(color = "blue") + labs(x = "Time to Expiration (Days)", y = "Theta (C)", title = "Theta (Call) vs. Time to Expiration") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                                                                                               axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14)) + scale_x_reverse() + scale_y_reverse()

ggsave("GGPlot ~ SPY American Call Option - Theta (Call) vs. Time to Expiration on October 28th, Expiration = 11-18-22.png")


## Delta (Call) vs. Time to Expiration


# plot Delta vs. Time to Expiration (Days); multiply by 252 to convert to days and 
# reverse x scale
ggplot(future_prices_df, aes((x = future_prices_df$output_call_future_time_to_expiration * 252), y = future_prices_df$output_call_future_deltas)) + geom_point(color = "blue") + labs(x = "Time to Expiration (Days)", y = "Delta (C)", title = "Delta (Call) vs. Time to Expiration") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                                                                                               axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14)) + scale_x_reverse()

ggsave("GGPlot ~ SPY American Call Option - Delta (Call) vs. Time to Expiration on October 28th, Expiration = 11-18-22.png")


## Gamma (Call) vs. Time to Expiration


# plot Delta vs. Time to Expiration (Days); multiply by 252 to convert to days and 
# reverse x scale
ggplot(future_prices_df, aes((x = future_prices_df$output_call_future_time_to_expiration * 252), y = future_prices_df$output_call_future_gammas)) + geom_point(color = "blue") + labs(x = "Time to Expiration (Days)", y = "Gamma (C)", title = "Gamma (Call) vs. Time to Expiration") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                                                                                               axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14)) + scale_x_reverse()

ggsave("GGPlot ~ SPY American Call Option - Gamma (Call) vs. Time to Expiration on October 28th, Expiration = 11-18-22.png")


## Future Prices for Put


# create vectors for future option prices
output_put_future_prices <- c()
output_put_future_deltas <- c()
output_put_future_gammas <- c()
output_put_future_thetas <- c()
output_put_future_time_to_expiration <- c()

# create counter for decreasing time to expiration
count <- 0

# calculate price of option & greeks for strike at 382 until expiration
for(i in 1:length(SPY_future_prices))
{
  x <- binomopt(s = as.numeric(SPY_future_prices[i]), 
                k = put_strikes[1], 
                v = historic_vol, 
                r = risk_free_rate, 
                tt = (num_of_trading_days - count)/total_trading_days, 
                d = 0, 
                nstep = 100, 
                american = TRUE, 
                putopt = TRUE,
                returngreeks = TRUE
  )
  
  # print(x)
  # print(count)
  
  output_put_future_prices <- c(output_put_future_prices, as.numeric(x[1]))
  output_put_future_deltas <- c(output_put_future_deltas, as.numeric(x[2]))
  output_put_future_gammas <- c(output_put_future_gammas, as.numeric(x[3]))
  output_put_future_thetas <- c(output_put_future_thetas, as.numeric(x[4]))
  output_put_future_time_to_expiration <- c(output_put_future_time_to_expiration, as.numeric(x[9]))
  
  count <- count + 1
}

# display output vectors
output_put_future_prices
output_put_future_deltas
output_put_future_gammas
output_put_future_thetas
output_put_future_time_to_expiration

# append to dataframe for call option till expiration at strike k = $382
future_prices_df <- cbind(future_prices_df, output_put_future_prices, output_put_future_deltas, output_put_future_gammas, output_put_future_thetas, output_put_future_time_to_expiration)

future_prices_df


## Theta (Put) vs. Time to Expiration


# plot Theta vs. Time to Expiration (Days); multiply by 252 to convert to days and 
# reverse x scale
ggplot(future_prices_df, aes((x = future_prices_df$output_put_future_time_to_expiration * 252), y = future_prices_df$output_put_future_thetas)) + geom_point(color = "red") + labs(x = "Time to Expiration (Days)", y = "Theta (P)", title = "Theta (Put) vs. Time to Expiration") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                                                                                           axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14)) + scale_x_reverse() + scale_y_reverse()

ggsave("GGPlot ~ SPY American Call Option - Theta (Put) vs. Time to Expiration on October 28th, Expiration = 11-18-22.png")


## Delta (Put) vs. Time to Expiration


# plot Delta vs. Time to Expiration (Days); multiply by 252 to convert to days and 
# reverse x scale
ggplot(future_prices_df, aes((x = future_prices_df$output_put_future_time_to_expiration * 252), y = future_prices_df$output_put_future_deltas)) + geom_point(color = "red") + labs(x = "Time to Expiration (Days)", y = "Delta (P)", title = "Delta (Put) vs. Time to Expiration") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                                                                                           axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14)) + scale_x_reverse() + scale_y_reverse()

ggsave("GGPlot ~ SPY American Call Option - Delta (Put) vs. Time to Expiration on October 28th, Expiration = 11-18-22.png")


## Gamma (Put) vs. Time to Expiration


# plot Delta vs. Time to Expiration (Days); multiply by 252 to convert to days and 
# reverse x scale
ggplot(future_prices_df, aes((x = future_prices_df$output_put_future_time_to_expiration * 252), y = future_prices_df$output_put_future_gammas)) + geom_point(color = "red") + labs(x = "Time to Expiration (Days)", y = "Gamma (P)", title = "Gamma (Put) vs. Time to Expiration") + theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                                                                                                                                           axis.title.y = element_text(vjust = 2, size = 13)) + theme(plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14)) + scale_x_reverse()

ggsave("GGPlot ~ SPY American Call Option - Gamma (Put) vs. Time to Expiration on October 28th, Expiration = 11-18-22.png")


## Hedging Strategy


SPY_future_prices

SPY_future_prices_df <- data.frame(index(SPY_future_prices), as.numeric(SPY_future_prices))
names(SPY_future_prices_df) <- c("Date", "SPY Closing Price")

SPY_future_prices_df

gt_spy_table <- gt(SPY_future_prices_df)

# create gt table of hedging dataframe
gt_hedge_table <- gt(hedge_df)

# edit table
(gt_spy_table <-
    
    # use pipe operator %>% to allow for easier argument implementation
    gt_spy_table %>%
    
    # use md to allow for markdown
    tab_header(
      title = md("Closing Prices of SPY"),
      subtitle = md("From *Oct-31-2022* to *Nov-18-2022*")
    ) %>%
    
    # add style for title
    tab_style(
      locations = cells_title(groups = 'title'),
      style = list(cell_text(weight = 'bold', size = 20))
    ) %>%
    
    # add style for subtitle
    tab_style(
      locations = cells_title(groups = 'subtitle'),
      style = list(cell_text(size = 17))
    ) %>%
    
    # add style for columns
    tab_style(
      # specify all columns to style
      locations = cells_column_labels(columns = everything()),
      style = list(
        # give thick border below columns
        cell_borders(sides = 'bottom', weight = px(3)),
        # make text bold
        cell_text(weight = 'bold')
      )
    )
)

gt_spy_table %>% gtsave(filename = "Closing Prices of SPY.png", path = getwd())

The closing prices of SPY for the first 2 weeks of November 2022 are as follows.
The call option which is in-the-money (ITM) on November 11th, with strike K = 396, ends up out-of-the-money on November 18th, when the closing price for SPY is 394.03. How can we hedge it?
  
  Hedging strategy for put option on SPY with expiration November 18th and strike K = 396. The last two columns show the daily price change of the option (unhedged) and of the option hedged with $\Delta$ shares of stock.


# retrieve indices of xts (time series) object to append to dataframe
future_prices_indices <- index(SPY_future_prices)

# create vectors for future option prices for strike = 396
output_put_future_prices_strike396 <- c()
output_put_future_deltas_strike396 <- c()
output_put_future_gammas_strike396 <- c()
output_put_future_thetas_strike396 <- c()
output_put_future_time_to_expiration_strike396 <- c()

# create counter for decreasing time to expiration
count <- 0

# calculate price of option & greeks for put with strike at 396 until expiration
for(i in 1:length(SPY_future_prices))
{
  x <- binomopt(s = as.numeric(SPY_future_prices[i]), 
                k = put_strikes[length(put_strikes)], 
                v = historic_vol, 
                r = risk_free_rate, 
                tt = (num_of_trading_days - count)/total_trading_days, 
                d = 0, 
                nstep = 100, 
                american = TRUE, 
                putopt = TRUE,
                returngreeks = TRUE
  )
  
  # print(x)
  # print(count)
  
  output_put_future_prices_strike396 <- c(output_put_future_prices_strike396, as.numeric(x[1]))
  output_put_future_deltas_strike396 <- c(output_put_future_deltas_strike396, as.numeric(x[2]))
  output_put_future_gammas_strike396 <- c(output_put_future_gammas_strike396, as.numeric(x[3]))
  output_put_future_thetas_strike396 <- c(output_put_future_thetas_strike396, as.numeric(x[4]))
  output_put_future_time_to_expiration_strike396 <- c(output_put_future_time_to_expiration_strike396, as.numeric(x[9]))
  
  count <- count + 1
}

# display output vectors
output_put_future_prices_strike396
output_put_future_deltas_strike396
output_put_future_gammas_strike396
output_put_future_thetas_strike396
output_put_future_time_to_expiration_strike396

# append to dataframe for call option till expiration at strike k = $396
future_prices_df_strike396 <- data.frame(output_put_future_prices_strike396, output_put_future_deltas_strike396, output_put_future_gammas_strike396, output_put_future_thetas_strike396, output_put_future_time_to_expiration_strike396)

future_prices_df_strike396

output_put_future_prices_strike396

# compute P(t) - P(t-1) or daily price change of the unhedged option using diff()
unhedged_price_diff <- diff(output_put_future_prices_strike396)

# add NA value to make number of rows even
unhedged_price_diff <- c(NA, unhedged_price_diff)

hedged_price_diff <- c(NA, 0.1082, 3.8923, 2.738, -2.034, -1.082, 1.082, 6.102, -4.532, -1.028, 0.859, -1.2934, 0.2934, -0.1234, 1.592)

# create vector for the time till expiration for option
time_till_expiration <- c()

# compute number of rows of dataframe for incrementing
nrow(hedge_df)

# iterate downward to 0, starting at 14
for(i in (nrow(hedge_df) - 1):0)
{
  time_till_expiration <- c(time_till_expiration, paste0(i, "/252"))
}

# turn last day into NA since that's when option expires
time_till_expiration[length(time_till_expiration)] <- NA

# output expiration dates
time_till_expiration

# compute hedge dataframe with all information including delta hedging
hedge_df <- data.frame(time_till_expiration, SPY_future_prices, round(output_put_future_prices_strike396, 3), round(output_put_future_deltas_strike396, 4), round(unhedged_price_diff, 4), round(hedged_price_diff, 4))

hedge_df

# rename rows of dataframe
names(hedge_df) <- c("Maturity", "Spot Price", "Put Price", "Delta", "P(t) - P(t-1)", "D(t) - D(t-1)")

# create gt table of hedging dataframe
gt_hedge_table <- gt(hedge_df)

# edit table
(gt_hedge_table <-
    
    # use pipe operator %>% to allow for easier argument implementation
    gt_hedge_table %>%
    
    # use md to allow for markdown
    tab_header(
      title = md("Hedging Strategy for Put Option on SPY"),
      subtitle = md("Expiration *Nov-18-2022* and *K = 396*")
    ) %>%
    
    # add style for title
    tab_style(
      locations = cells_title(groups = 'title'),
      style = list(cell_text(weight = 'bold', size = 20))
    ) %>%
    
    # add style for subtitle
    tab_style(
      locations = cells_title(groups = 'subtitle'),
      style = list(cell_text(size = 17))
    ) %>%
    
    # add style for columns
    tab_style(
      # specify all columns to style
      locations = cells_column_labels(columns = everything()),
      style = list(
        # give thick border below columns
        cell_borders(sides = 'bottom', weight = px(3)),
        # make text bold
        cell_text(weight = 'bold')
      )
    )
)

# display gt hedge table
gt_hedge_table

# save gt hedge table to current working directory
gt_hedge_table %>% gtsave(filename = "Hedging Strategy for Put Option on SPY.png", path = getwd())


Testing Binomopt


s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; nstep=15

binomopt(s, k, v, r, tt, d, nstep, american=TRUE, putopt=TRUE)

binomopt(s, k, v, r, tt, d, nstep, american=TRUE, putopt=TRUE,
         returnparams=TRUE, returntrees = TRUE)

## matches Fig 10.8 in 3rd edition of Derivatives Markets
x <- binomopt(110, 100, .3, .05, 1, 0.035, 3, american=TRUE,
              returntrees=TRUE, returnparams=TRUE)
print(x$oppricretree)
print(x$delta)
print(x$bond)

binomplot(s, k, v, r, tt, d, nstep, american=TRUE, putopt=FALSE, plotarrows = TRUE, returnprice = TRUE)

binomplot(s, k, v, r, tt, d, nstep, american=FALSE, putopt=TRUE)

binomplot(s, k, v, r, tt, d, nstep, american=TRUE, putopt=FALSE, plotarrows = TRUE, returnprice = TRUE)

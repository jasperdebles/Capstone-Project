# Advanced Business & Economic Forecasting 
# Capstone Project 
# Jasper P de Bles 

# Download packages 
suppressMessages(library(xts))
suppressMessages(library(astsa))
suppressMessages(library(stargazer))
suppressMessages(library(dynlm))
suppressMessages(library(sandwich))
suppressMessages(library(lmtest))
suppressMessages(library(HH))
suppressMessages(library(forecast))
suppressMessages(library(aTSA))
suppressMessages(library(quantmod))
suppressMessages(library(tseries))
suppressMessages(library(timeSeries))
suppressMessages(library(basicTrendline))


# Section I 
# Import the data 
table1 = read.csv("capstone1.csv")
table1$date = as.yearqtr(table1$date, "%Y:%q")
realgdp.xts = as.xts(table1$realgdp, order.by = table1$date)
plot.xts(realgdp.xts)

Y.xts = log(realgdp.xts)
plot.xts(Y.xts)

gdpgrowth.xts = 400 * diff(Y.xts)
plot.xts(gdpgrowth.xts)

tbillrate.xts = as.xts(table1$tbillrate, order.by = table1$date)
plot.xts(tbillrate.xts)

deltaR.xts = diff(tbillrate.xts)
plot.xts(deltaR.xts)

# Question a 
gdpgrowth.select = gdpgrowth.xts["1955/2004"]
AR.1 = sarima(gdpgrowth.select, 1, 0, 0)
stargazer(AR.1, type = "latex")

# Question b
gdpgrowth_ts = ts(gdpgrowth.xts, 
               start = c(1955,1), 
               end = c(2004, 4), 
               frequency = 4)

deltaR_ts = ts(deltaR.xts, 
                  start = c(1955,1), 
                  end = c(2004, 4), 
                  frequency = 4)

ADL1.4data = ts.union(gdpgrowth_ts, deltaR_ts)
ADL1.4 = dynlm(gdpgrowth_ts ~ L(gdpgrowth_ts) + L(deltaR_ts) + L(deltaR_ts, 2)
               + L(deltaR_ts, 3) + L(deltaR_ts, 4),
               start = c(1955, 1), end = c(2004, 4))
coeftest(ADL1.4, vcov. = sandwich)
stargazer(ADL1.4, type = "latex")



# Sectoin II 
# Import the data 
table2 = read.csv("capstone2.csv")
table2$date = as.yearqtr(table2$date, "%m/%d/%Y")
swedish.gdp.xts = as.xts(table2$swrgdp, order.by = table2$date)
plot.xts(swedish.gdp.xts)
swedish.gdp.xts = swedish.gdp.xts["1993/2005"]

# Linear trend model 
ltm = lm(swrgdp ~ date, data = table2)
plot(ltm$model)
summary(ltm)
stargazer(ltm, type = "text")

swedish.gdp.ts = ts(table2$swrgdp, frequency = 4, start = c(1993, 1), end = c(2005,4))
ts.stl = stl(swedish.gdp.ts,"periodic")
ts.sa = seasadj(ts.stl)
plot(swedish.gdp.ts, type = "l")
plot(ts.sa, type = "l")
seasonplot(ts.sa, 4, col = rainbow(4), year.labels = TRUE)

# Create dummy variables 
DF = data.frame(Date = seq(as.Date("1993-01-01"), length.out = 62, by = "3 months"))
dummytable = data.table::dcast(DF, Date ~ paste0("Q", lubridate::quarter(DF$Date)), length, 
                  value.var = "Date")

table2$Q2 = dummytable$Q2
table2$Q3 = dummytable$Q3 
table2$Q4 = dummytable$Q4
table2 = table2[-(53:62),]

# Linear trend model with 3 seasonal dummy variables 
ltmseasonal = lm(swrgdp ~ date + Q2 + Q3 + Q4, data = table2)
summary(ltmseasonal)
stargazer(ltmseasonal, type = "text")


# HAC
robust.se.1 = list(sqrt(diag(vcovHAC(ltm, type = "HAC1"))), 
                 sqrt(diag(vcovHAC(ltmseasonal, type = "HAC1"))))

# Make one table for comparison 
stargazer(ltm,
          ltmseasonal, 
          se = robust.se.1,
          type = "latex", intercept.bottom = FALSE, 
          column.labels = c("Linear Trend Model", "Seasonally Adjusted"),
          title = "Results Linear Trend Models Swedish GDP", 
          digits = 2, 
          dep.var.caption  = "Swedish GDP", 
          covariate.labels = c("Constant", "Trend", "Quarter 2", "Quarter 3",
                               "Quarter 4"))

ltm2 = tslm(swedish.gdp.ts ~ trend)
summary(ltm2) 
ltmseasonal2 = tslm(swedish.gdp.ts ~ trend + table2$Q2 + table2$Q3 + table2$Q4)
summary(ltmseasonal2)

decomposedRes <- decompose(swedish.gdp.ts, type="additive")
plot (decomposedRes) # see plot below
stlRes <- stl(swedish.gdp.ts, s.window = "periodic")
stlRes

# Forecast

capstone2.forecast = read.csv("capstone2forecast.csv")
capstone2.forecast$date = as.yearqtr(capstone2.forecast$date, "%m/%d/%Y")
Swedish.GDP.xts = as.xts(capstone2.forecast$swrgdp, order.by = capstone2.forecast$date)
LTMforecast.xts = as.xts(capstone2.forecast$forecast..1., order.by = capstone2.forecast$date)
SAforecast.xts = as.xts(capstone2.forecast$forecast..2., order.by = capstone2.forecast$date)
plot.xts(merge.xts(Swedish.GDP.xts, LTMforecast.xts, SAforecast.xts), 
         lwd = 0.1, legend.loc = "bottom", main = "Sample vs Forecast")
Swedish.GDP.xts = Swedish.GDP.xts["2006/2008"]
LTMforecast.xts = LTMforecast.xts["2006/2008"]
SAforecast.xts = SAforecast.xts["2006/2008"]


# Section III 
# Import the data 
table3 = read.csv("capstone3.csv")
table3$date = as.Date(table3$date, "%Y-%m-%d")
crudeoil.xts = as.xts(table3$oilprice, order.by = table3$date)
plot.xts(crudeoil.xts)

logoil.xts = log(crudeoil.xts)
plot.xts(logoil.xts)

oilgrowth.xts = 1200 * diff(logoil.xts)
plot.xts(oilgrowth.xts)

oilgrowth.xts = na.omit(oilgrowth.xts)
acf(oilgrowth.xts)

acf.pacf.plot(oilgrowth.xts)
acf.pacf.plot(crudeoil.xts)

ARIMA3.1.1 = sarima(crudeoil.xts, 3, 1, 1, no.constant = TRUE)
?sarima
stargazer(ARIMA3.1.1, type = "latex")

auto.arima(crudeoil.xts, ic = "aic")


# Section IV 
# Import the data 
stockreturn = read.csv("Stock_Returns_1931_2002.csv")
stockreturn$X = 1
stockreturn$date = as.Date(with(stockreturn, paste(time, Month, X, sep="-")), "%Y-%m-%d")
excessreturn.xts = as.xts(stockreturn$ExReturn, order.by = stockreturn$date)
plot.xts(excessreturn.xts, lwd = 0.1)

return.ts = as.ts(stockreturn$ExReturn, order.by = stockreturn$date)

adf.test(excessreturn.xts)

# Run the AR models 
AR.1.stock = dynlm(return.ts ~ L(return.ts))
summary(AR.1.stock)
coeftest(AR.1.stock, vcov. = vcovHAC)

AR.2.stock = dynlm(return.ts ~ L(return.ts) + L(return.ts, 2))
summary(AR.2.stock)
coeftest(AR.2.stock, vcov. = vcovHAC)

AR.4.stock = dynlm(return.ts ~ L(return.ts) + L(return.ts, 2) + 
                     L(return.ts, 3) + L(return.ts, 4))
summary(AR.4.stock)
coeftest(AR.4.stock, vcov. = vcovHAC)

robust.se = list(sqrt(diag(vcovHAC(AR.1.stock, type = "HAC1"))), 
                 sqrt(diag(vcovHAC(AR.2.stock, type = "HAC1"))), 
                 sqrt(diag(vcovHAC(AR.4.stock, type = "HAC1"))))

# Create a table like in the book 
stargazer(AR.1.stock,
          AR.2.stock, 
          AR.4.stock,
          se = robust.se,
          type = "latex", intercept.bottom = FALSE, 
          column.labels = c("AR(1)", "AR(2)", "AR(4)"),
          title = "Autoregressive models of monthly excess stock returns", 
          digits = 3, 
          dep.var.caption  = "excess return t", 
          covariate.labels = c("Constant", "excess return t-1", "excess return t-2",
                               "excess return t-3", "excess return t-4"))

acf(AR.1.stock$residuals,main="Residuals plot")

# Forecast 
forecast.return = read.csv("returnsforecast.csv")
excessreturn.select = excessreturn.xts["1983/"]
forecast.return$X = 1
forecast.return$date = as.Date(with(forecast.return, paste(time, Month, X, sep="-")), "%Y-%m-%d")
forecast1.xts = as.xts(forecast.return$AR1, order.by = forecast.return$date)
forecast2.xts = as.xts(forecast.return$AR2, order.by = forecast.return$date)
forecast4.xts = as.xts(forecast.return$AR4, order.by = forecast.return$date)
plot.xts(merge.xts(forecast1.xts, forecast2.xts, forecast4.xts, excessreturn.select), 
         lwd = 0.1, legend.loc = "bottom", main = "Sample vs Forecast")
plot.xts(merge.xts(forecast1.xts, forecast2.xts, forecast4.xts), 
         lwd = 0.1, legend.loc = "bottom", main = "Forecast")


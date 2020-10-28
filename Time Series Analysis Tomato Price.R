#Libraries
library('ggplot2')
library('ggfortify')
library('forecast')
library('tseries')
#https://rpubs.com/palominoM/series


# load data
datos = read.csv('tomato_price.csv', header=TRUE)
head(datos)

# data Transform to TimeSeries (January, 1 2016 to June, 1 2020)
myts <- ts(datos$rate, start=c(2012, 1), end=c(2020, 6), frequency=12, class="ts", name = "Tomato Rate")

# Chart Time Series
autoplot(myts, ts.colour = 'green', ts.linetype = 'dashed')

# Test de Normalidad: Histogram, Kolmorov-Smirnov, Shapiro
hist(datos$rate, col='grey')
ks.test(datos$rate, "pnorm", mean=mean(datos$rate), sd=sd(datos$rate))
shapiro.test(datos$rate)

# Serie Estacionaria
autoplot(acf(myts, plot = FALSE))
autoplot(stl(myts, s.window = "periodic"), ts.colour = "blue")
# Test Dickey-Fuller
adf.test(datos$rate, alternative = "stationary")

# Descomponiendo serie por año
price_ma = ts(na.omit(datos$rate), frequency=12)
decomp = stl(price_ma, s.window="periodic")
deseasonal_ma <- seasadj(decomp)
plot(decomp)

n_lags = 0
# Autocorrelacion (No tomar en cuenta para valor para x=0)
a <-(acf(datos$rate,plot = TRUE, main =""))
# convert to data frame
a <-(pacf(datos$rate, plot = TRUE, main =""))
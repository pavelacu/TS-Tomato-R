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
autoplot(myts, ts.colour = '#1f618d', ts.linetype = 'dashed')+theme_light()

# Test de Normalidad: Histogram, Kolmorov-Smirnov, Shapiro
hist(datos$rate, col='#1f618d')
qplot(datos$rate, geom="histogram") 
ggplot(data=datos, aes(datos$rate)) + geom_histogram(color="white", fill="#85c1e9")+theme_light()
ks.test(datos$rate, "pnorm", mean=mean(datos$rate), sd=sd(datos$rate))
shapiro.test(datos$rate)

# Serie Estacionaria
# Metodo 1
autoplot(acf(myts, plot = FALSE))+theme_light()
autoplot(stl(myts, s.window = "periodic"), ts.colour = "#1f618d")
# Test Dickey-Fuller
adf.test(datos$rate, alternative = "stationary")

# Metodo 2
# Descomponiendo serie por año
price_tomato = ts(na.omit(datos$rate), frequency=12)
decomp = stl(price_tomato, s.window="periodic")
deseasonal_ma <- seasadj(decomp)
plot(decomp)

# Determinar el grado de arima
autoplot(acf(myts, plot = FALSE))+theme_light()
autoplot(pacf(myts, plot = FALSE))+theme_light()

#Modelado de la Serie
ndiffs(myts)

nsdiffs(myts)

diff.myts<-autoplot(diff(myts), ts.linetype = "dashed", ts.colour = "darkmagenta")
diff.myts

autoplot(acf(diff(myts), plot = FALSE))

monthplot(diff(myts), col = "midnightblue")

diffco2<-diff(myts)
boxplot(diffco2~cycle(diffco2))

diff.myts.12<-diff(myts, lag = 12)
autoplot(diff.myts.12, ts.colour = "darkorange4", ts.linetype = "dashed")

adf<-adf.test(diff.myts.12)
adf$p.value

kpss<-kpss.test(diff.myts.12)
kpss$p.value

autoplot(acf(diff.myts.12, plot = FALSE))

autoplot(pacf(diff.myts.12, plot = FALSE))

arima1<- Arima(myts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(myts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(myts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(myts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(myts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(myts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(myts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))

#AIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
AIC(arima1,arima3,arima4,arima5,arima6,arima7)
#BIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
BIC(arima1,arima3,arima4,arima5,arima6,arima7)

autoplot(acf(arima1$residuals, plot = FALSE))

autoplot(pacf(arima1$residuals, plot = FALSE))

ggtsdiag(arima1)

bp <- Box.test(arima1$residuals) # Test de Box-Pierce
bp$p.value

jb <- jarque.bera.test(arima1$residuals) # Test de Jarque-Bera
jb$p.value

sht<-shapiro.test(arima1$residuals) $ # Test de Shapiro-Wilk
sht$p.value1

(auto.arima(myts, stepwise = FALSE, approximation = FALSE))




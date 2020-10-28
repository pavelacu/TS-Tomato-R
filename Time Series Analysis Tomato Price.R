#Libraries
library('ggplot2')
library('forecast')
library('tseries')
datos = read.csv('tomato_price.csv', header=TRUE)
head(datos)



plot(datos$month, datos$rate, main="Semana",ylab="Ventas")

myts <- ts(datos$rate, start=c(2012, 1), end=c(2020, 6), frequency=12)
head(myts)
plot(myts)
desc(myts)
ggplot(data = datos, aes(x = month, y = rate))+geom_line(color = "#00AFBB", size = 2)



n_lags = 3
# Autocorrelacion (No tomar en cuenta para valor para x=0)
a <-(acf(datos$ventas, lag = n_lags, plot = TRUE))
# convert to data frame
a <- data.frame(lag=a$lag, acf=a$acf)
# use data frame for ggplot
# 95% confidence interval limits
ci <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(datos)))  
ggplot(a, aes(lag, acf)) + geom_area(fill="grey") +  geom_hline(yintercept=c(ci, -ci), linetype="dashed") +
  theme_bw()
library(readxl)
BBNI_JK <- read_excel("..\Dataset\BBNI.JK.xlsx")
View(BBNI_JK)
dim(BBNI_JK)
str(BBNI_JK)

#Missing value
sapply(BBNI_JK, function(x) sum(is.na(x)))
library(visdat)
vis_miss(BBNI_JK)
summary(BBNI_JK)
Close = ts(BBNI_JK[,5])
Close
plot.ts(Close)

#Cek Statistioneritas Ragam
library(astsa)
library(tseries)
library(forecast)
library(TSA)
bc = BoxCox.lambda(Close)
bc

#Cek Statistioneritas Rataan
adf.test(Close)

#Cek ACF dan PACF
acf2(Close)

#melakukan differencing terhadap tren
diff1.Close = diff(Close)
plot.ts(diff1.Close)
diff2.Close = diff(diff1.Close)
plot(diff2.Close)

#Melihat plot dalam bentuk matriks
par(mfrow=c(3,1))
plot.ts(Close)
plot.ts(diff1.Close, col = "blue")
plot.ts(diff2.Close, col = "red")

#Transformasi
AP = Close
log.AP = log(AP)
par(mfrow = c(2,1))
plot.ts(AP)
plot.ts(log.AP, col = 'blue')

#Melakulan Differencing pada log AP
par(mfrow = c(3,1))
plot(log.AP)
diff1.log.AP = diff(log.AP)
plot.ts(diff1.log.AP)
diff2.log.AP =diff(diff1.log.AP)
plot.ts(diff2.log.AP)
library(tidyverse)
library(broom)
library(car)
library(pdfetch)
library(magrittr)
library(rpart)
library(tsibble)
library(stargazer)
library(readxl)

data1 <- read_excel("C:/Users/james/Desktop/5253/5223 PROJECT/google trends/DATA1.xlsx")

summary(data1)

data1 %<>% mutate(logAAPL.V = log(AAPL.V),logBRKB.V = log(BRKB.V),logFB.V = log(FB.V),
              logJNJ.V = log(JNJ.V), logXOM.V = log(XOM.V))
data1 %<>% mutate(logAAPL.PRICE = log(AAPL.PRICE),logBRKB.PRICE = log(BRKB.PRICE),
                  logFB.PRICE = log(FB.PRICE),logJNJ.PRICE = log(JNJ.PRICE),
                  logXOM.PRICE = log(XOM.PRICE))

                
data1 %<>% mutate(lagAAPL.INDEX = lag(AAPL.INDEX),lagBRKB.INDEX = lag(BRKB.INDEX),
                  lagFB.INDEX = lag(FB.INDEX),lagJNJ.INDEX = lag(JNJ.INDEX),
                  lagXOM.INDEX = lag(XOM.INDEX))
ggplot(data1,aes(Week, AAPL.INDEX))+ geom_line()
ggplot(data1,aes(Week, BRKB.INDEX))+ geom_line()
ggplot(data1,aes(Week, XOM.INDEX))+ geom_line()
ggplot(data1,aes(Week, JNJ.INDEX))+ geom_line()
ggplot(data1,aes(Week, FB.INDEX))+ geom_line()

aaplv <- lm(logAAPL.V ~ AAPL.INDEX + lag(AAPL.INDEX,1), data = data1)
aaplp <- lm(logAAPL.PRICE ~ AAPL.INDEX + lag(AAPL.INDEX,1), data = data1)

brkbv <- lm(logBRKB.V ~ BRKB.INDEX + lag(BRKB.INDEX,1), data = data1)
brkbp <- lm(logBRKB.PRICE ~ BRKB.INDEX + lag(BRKB.INDEX,1), data = data1)

xomv <- lm(logXOM.V ~ XOM.INDEX + lag(XOM.INDEX,1), data = data1)
xomp <- lm(logXOM.PRICE ~ XOM.INDEX + lag(XOM.INDEX,1), data = data1)

jnjv <- lm(logJNJ.V ~ JNJ.INDEX + lag(JNJ.INDEX,1), data = data1)
jnjp <- lm(logJNJ.PRICE ~ JNJ.INDEX + lag(JNJ.INDEX,1), data = data1)

fbv <- lm(logFB.V ~ FB.INDEX + lag(FB.INDEX,1),data = data1)
fbp <- lm(logFB.PRICE ~ FB.INDEX + lag(FB.INDEX), data = data1)
summary(aaplv)
stargazer(aaplv,brkbv,xomv,jnjv,fbv)
stargazer(aaplp,brkbp,xomp,jnjp,fbp)



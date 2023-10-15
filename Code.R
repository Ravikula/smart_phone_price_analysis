rm(list=ls())

library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)

setwd("C:/Users/Buddhi Perera/Desktop/Assignment_2")
df <- read_csv("phone_price.csv")

df %>% summary()

brand_perc <- df$Brand %>% table()%>% prop.table()*100
brand_perc %>% barplot(main = "Percentage of Smartphone Brands",ylab = "Percent", ylim=c(0,20) , xlab="Brand", cex.lab = 1.2, cex.axis =1.2, las=2)

os_perc <- df$OS %>% table()%>% prop.table()*100
os_perc %>% barplot(main = "Percentage of Smartphone OS",ylab = "Percent", ylim=c(0,100) , xlab="OS", cex.lab = 1.2, cex.axis =1.2)

ram_perc <- df$RAM %>% table()%>% prop.table()*100
ram_perc %>% barplot(main = "Percentage of Smartphone RAM",ylab = "Percent", ylim=c(0,40) , xlab="RAM (GB)", cex.lab = 1.2, cex.axis =1.2,las=1)

tab1 <- table(df$Brand,df$RAM)
tab1
tab2 <- tab1 %>% prop.table(margin=2)
tab2

df %>% summarise(Min = min(RAM,na.rm = TRUE),
                 Q1 = quantile(RAM,probs = .25,na.rm = TRUE),
                 Median = median(RAM, na.rm = TRUE),
                 Q3 = quantile(RAM,probs = .75,na.rm = TRUE), 
                 Max = max(RAM,na.rm = TRUE),
                 Mean = mean(RAM, na.rm = TRUE),
                 SD = sd(RAM, na.rm = TRUE),
                 n = n(),
                 Missing = sum(is.na(RAM)))



df %>% summarise(Min = min(`Price ()`,na.rm = TRUE),
                 Q1 = quantile(`Price ()`,probs = .25,na.rm = TRUE),
                 Median = median(`Price ()`, na.rm = TRUE),
                 Q3 = quantile(`Price ()`,probs = .75,na.rm = TRUE), 
                 Max = max(`Price ()`,na.rm = TRUE),
                 Mean = mean(`Price ()`, na.rm = TRUE),
                 SD = sd(`Price ()`, na.rm = TRUE),
                 n = n(),
                 Missing = sum(is.na(`Price ()`)))

boxplot(df$`Price ()`,main="Box plot of Smart Phone Price Ranges  ",ylab= "Price ($)")

boxplot(df$`Battery Capacity (mAh)`,main="Box plot of Battery Capacities  ",ylab= "Battery Capacity (mAh)")

df %>% boxplot(`Price ()`~Brand ,data = ., main="Box Plot of Smart Phone Prices by Band", 
               ylab="Price ($)", xlab="Brand",horizontal=FALSE, col = "skyblue",las=2)


abc <- df %>% boxplot(`Price ()`~Brand ,data = ., main="Box Plot of Smart Phone Prices by Band", 
               ylab="Price ($)", xlab="Brand",horizontal=FALSE, col = "skyblue",las=2)
summary(abc$stats)

`breaks <- c(0, 500, 1000, 1500, Inf)
labels <- c("< 500", "500 - 1000", "1000-1500", "1500 <")
df$price_ranges <- cut(df$`Price ()`, breaks = breaks, labels = labels, right = FALSE)

tab1 <- table(df$Brand,df$price_ranges)
tab1
tab2 <- tab1 %>% prop.table(margin=1)*100
tab2

cor(df$`Price ()`,df$Storage)
cor(df$`Price ()`,df$RAM)
cor(df$`Price ()`,df$`Battery Capacity (mAh)`)

plot(df$Storage,df$`Price ()`,xlab="Storage (GB)",ylab="Price ($)",main='Storage Vs Price')
plot(df$RAM,df$`Price ()`,xlab="RAM (GB)",ylab="Price ($)",main='RAM Vs Price')
plot(df$`Battery Capacity (mAh)`,df$`Price ()`,xlab="Battery Capacity (mAh)",ylab="Price ($)",main='Battery Capacity Vs Price')
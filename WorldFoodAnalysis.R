#CS356
#Unit 3
#Assignment Submission 2

# Objective:
# Big data processing using Mapreduce aggregation to
# generated descriptive statistics, data visualizations, and statistical models by
# determining the top 10 fastest growing countries for food production,
# and by determining top 10 countries with the fastest food consumptions.

library("readr") # data input
library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('ggplot2') # visualization
install.packages("ggthemes")
library('ggthemes') # visualization
install.packages("corrplot")
library('corrplot') # visualization
library('lubridate') # date and time
library('purrr') # data manipulation
install.packages("cowplot")
library('cowplot')# visualization
library('maps')# visualization
install.packages("viridis")
install.packages("viridisLite")
library('viridisLite')
library('viridis')# visualization
install.packages('treemap')
library('treemap')
install.packages("leaflet")
library('leaflet')
install.packages("dygraphs")
library('dygraphs')
library('graphics')
install.packages("forecast")
library('forecast')
library('xts')
install.packages("IRdisplay")
library('IRdisplay')
options(scipen = 999)
options(warn = -1)

##set destination
setwd("C:/Users/rafsan bhuiyan/Documents/PREVIOUS COURSES CTU/CTU SEMESTER JULY TO SEP 2019/Foundation of Big Data Analytics/Unit 3")

#read csv file
faoData <- read.csv("FAO.csv")

#print the summary of fao dataset
summary(faoData)

str(faoData)

#Renaming few columns
colnames(faoData)[1] <- "areaAbb"
colnames(faoData)[2] <- "areaCode"
colnames(faoData)[3] <- "area"
colnames(faoData)[4] <- "itemCode"
colnames(faoData)[5] <- "item"
colnames(faoData)[6] <- "elementCode"
colnames(faoData)[7] <- "element"

#Print faoData
head(faoData)

#Determine the fastest growing countries for food production
#Simulate a graph using ggplot
options(repr.plot.width=10, repr.plot.height =4)
faoData <- mutate(faoData, Total=apply(faoData[11:63], 1, sum, na.rm = T))
faoData <- mutate(faoData, last5=apply(faoData[58:63], 1, sum, na.rm = T))
fastestGrowingCountries <- faoData %>% group_by(areaAbb, element) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(areaAbb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightblue", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.5, size = 3)+ theme_bw()+ xlab("Fastest Growing Countries for food production") + ylab("Food production since 1961")

#Top 10 countries with the fastest food consumptions
#Create a plot
top10FastFoodConsumptions <- faoData %>% group_by(areaAbb, element) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(areaAbb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightblue", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.5, size =3)+ theme_bw()+ xlab("Top 10 Countries with fastest food Consumptions") + ylab("Food Consumption since 2008-13")

plot_grid(fastestGrowingCountries, top10FastFoodConsumptions, align = "h")


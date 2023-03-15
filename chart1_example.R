library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

checkOutData <- read_csv("2017-2023-10-Checkouts-SPL-Data.csv")

graphData <- dplyr::filter(checkOutData, MaterialType %in% c('MUSIC', 'EBOOK', 'MOVIE', 'AUDIOBOOK', 'SONG'))

newdata <- dplyr::filter(checkOutData, MaterialType %in% c("MOVIE"))

digitalAverages <- graphData %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(Checkouts = mean(Checkouts))

colnames(digitalAverages)[1]<-"Entertainment"

digitalAverages %>%
  ggplot(aes(CheckoutYear, Checkouts, colour = Entertainment))+
  geom_point(size = 2, alpha = 1)+
  geom_smooth(method="lm", se=FALSE, size=1)+
  xlab("Year")+
  labs(title = "Digital Entertainment Checkouts per Year")

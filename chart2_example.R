library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

checkOutData <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv")

graphData <- dplyr::filter(checkOutData, MaterialType %in% c('MUSIC', 'EBOOK', 'MOVIE', 'AUDIOBOOK', 'SONG'))

newdata <- dplyr::filter(checkOutData, MaterialType %in% c("MOVIE"))

bookComparison <- dplyr::filter(checkOutData, MaterialType %in% c('AUDIOBOOK', "BOOK", "EBOOK"))

bookAverages <- bookComparison %>%
  group_by(MaterialType, CheckoutMonth) %>%
  summarize(Checkouts = mean(Checkouts))

colnames(bookAverages)[1]<-"Book Type"

bookAverages %>%
  ggplot(aes(CheckoutMonth, Checkouts, colour = `Book Type`))+
  geom_point(size = 2, alpha = 1)+
  geom_smooth(method="lm", se=FALSE, size=1)+
  scale_x_discrete(name = "Month", limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  labs(title = "Average Book Checkouts By Month Between 2017 and 2023")

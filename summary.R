library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

checkOutData <- read.csv("a3-spl-checkouts-seanmorris1-main/2017-2023-10-Checkouts-SPL-Data.csv")

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

publishers <- checkOutData %>%
  group_by(Publisher, Checkouts, CheckoutYear = 2022) %>%
  summarize(mean_summarry = mean(Checkouts))

summarise_each(publishers, funs(max(., na.rm=TRUE)))

publishers <- arrange(publishers, desc(Checkouts))

finalPublishers <- publishers[c(1,2,3,4,6),]

finalPublishers %>% 
  ggplot(aes(x = reorder(Publisher, -Checkouts), y = Checkouts))+
  geom_bar(stat="identity")+
  xlab("Publishers")+
  theme_classic()+
  labs(title = "Top 5 Publishers of 2022")

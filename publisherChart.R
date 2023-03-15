library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

checkOutData <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv")

graphData <- dplyr::filter(checkOutData, MaterialType %in% c('MUSIC', 'EBOOK', 'MOVIE', 'AUDIOBOOK', 'SONG'))

newdata <- dplyr::filter(checkOutData, MaterialType %in% c("MOVIE"))

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
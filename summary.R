library(dplyr)
library(readr)
library(tidyverse)

checkOutData <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv")

checkOutAverages <- checkOutData %>%
  group_by(MaterialType) %>%
  summarize(Checkouts = mean(Checkouts))

highestMonth <- checkOutData %>%
  group_by("Normal people : a novel / Sally Rooney.", CheckoutMonth) %>%
  summarise(max = max(Checkouts, na.rm=TRUE), min = min(Checkouts, na.rm=TRUE))

highestEbook <- checkOutData %>%
  group_by("EBOOK", CheckoutYear) %>%
  summarize(max = max(Checkouts, na.rm=TRUE))

printBooks <- checkOutData %>%
  group_by("BOOK", CheckoutYear, "PHYSICAL") %>%
  summarise(max = max(Checkouts, na.rm=TRUE))

laptopAverage <- checkOutData %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarise(max = max(Checkouts, na.rm=TRUE))

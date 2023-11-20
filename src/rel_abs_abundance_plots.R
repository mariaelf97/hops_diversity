library(tidyverse)
library(data.table)
# read data
growth_data <- fread("/Users/maryam/mnt/home/Growth vs N and P.csv")
n_fixing_data <-fread("/Users/maryam/mnt/home/n_fixing.csv")
# change origin and variety value to factor data type
growth_data$Origin <- as.factor(growth_data$Origin)
growth_data$Variety <- as.factor(growth_data$Variety)
# Normalize the RGR and DIN so they are both in the same ranges
growth_data$RGR <-scale(growth_data$RGR)
growth_data$`[DIN] 10/17` <- scale(growth_data$`[DIN] 10/17`)
growth_data<-growth_data%>%select(Origin,Variety,`[DIN] 10/17`,RGR)%>%
  group_by(Variety,Origin)%>% summarise(mean_nitrogen = mean(`[DIN] 10/17`),
                                        mean_RGR=mean(RGR))
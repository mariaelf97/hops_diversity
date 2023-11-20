library(tidyverse)
library(data.table)
library(reshape2)
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
# function to read relative abundance files and sum the abundance values
read_preprocess_file <- function(genus_name) {
  relative_abundance_data<-as.data.frame(fread(
    paste("/Users/maryam/mnt/home/",
    "zr13074.16S_231019.zymo/02...US.vs.NONUS.Bac16Sv34",
    "/Taxa2ASV_Decomposer/Genus/","g__",
    genus_name,"/seq.relative.abun.transpose.csv", sep = "")))
  colsum<- colSums(relative_abundance_data[,-1],na.rm=TRUE)
  colsum$genus<-genus_name
  return(colsum)
}
# apply the function
relative_abundance_files <- mapply(read_preprocess_file,
                                   n_fixing_data$s , SIMPLIFY = FALSE)
# simplofy the mapply output
relative_abundance_files_unzipped <- as.data.frame(
  do.call(rbind,relative_abundance_files))
# unlist the output
rel_abun_unlisted<-as.data.frame(unnest(relative_abundance_files_unzipped,
                                        colnames(relative_abundance_files_unzipped)))
# move genus column to row names
rownames(rel_abun_unlisted)<-rel_abun_unlisted$genus

melt_rel_abun<-melt(rel_abun_unlisted[,-22])
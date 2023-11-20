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

# change data to long format to be able to merge other datasets
melt_rel_abun<-melt(rel_abun_unlisted[,-22])
# function to read absolute abundance files
read_preprocess_file_absolute <- function(genus_name) {
  abs_abundance_data<-as.data.frame(fread(paste("/Users/maryam/mnt/home",
  "/zr13074.16S_231019.zymo/02...US.vs.NONUS.Bac16Sv34",
  "/Taxa2ASV_Decomposer/Genus/","g__", genus_name,
  "/seq.counts.csv", sep = "")))
  rowsum<- rowSums(abs_abundance_data[,-1],na.rm=TRUE)
  rowsum$genus<-genus_name
  return(rowsum)
}
# apply the function
absolute_abundance_files <- mapply(read_preprocess_file_absolute,
                                   n_fixing_data$s , SIMPLIFY = FALSE)
# simplify the output
absolute_abundance_files_unzipped <- as.data.frame(
  do.call(rbind,absolute_abundance_files))
# unlist the output
abs_abun_unlisted<-as.data.frame(unnest(
  absolute_abundance_files_unzipped,colnames(absolute_abundance_files_unzipped)))
# change dataframe column names
colnames(abs_abun_unlisted)<-c("Control.CSUSM","Zeus.CSUSM",
                               "Brewers.Gold.CSUSM","Neo1.CSUSM",
                               "Comet.CSUSM","Columbus.CSUSM",
                               "Saaz72.CSUSM","Sorachi.Ace.CSUSM",
                               "Southern.Cross.CSUSM","Hallertauer.CSUSM",
                               "Fuggle.UK","genus")
# move gneus column to row names
rownames(abs_abun_unlisted)<-abs_abun_unlisted$genus
# change data to long format
melt_abs_abun<-melt(abs_abun_unlisted[,-23])
# join dataframes
rel_abun_growth<-melt_rel_abun%>%
  inner_join(growth_data, by= c("variable"="Variety"))
rel_abun_growth_nfix <- rel_abun_growth %>%
  inner_join(n_fixing_data,by=c("genus"="s"))
rel_abun_growth_nfix_abs_abun<-rel_abun_growth_nfix%>%
  inner_join(melt_abs_abun,by= c("genus","variable"))
# change column names
colnames(rel_abun_growth_nfix_abs_abun)<-c("genus","cultivar","genus_rel_abun",
                                           "origin","mean_DIN","mean_RGR",
                                           "n_fixing","notes","genus_abs_abun")
# scale data to run regression analyses
rel_abun_growth_nfix_abs_abun$genus_abs_abun<-scale(
  rel_abun_growth_nfix_abs_abun$genus_abs_abun)
rel_abun_growth_nfix_abs_abun$genus_rel_abun<-scale(
  rel_abun_growth_nfix_abs_abun$genus_rel_abun)
rel_abun_growth_nfix_abs_abun$mean_DIN<-scale(
  rel_abun_growth_nfix_abs_abun$mean_DIN)
rel_abun_growth_nfix_abs_abun$mean_RGR<-scale(
  rel_abun_growth_nfix_abs_abun$mean_RGR)



### Plate Challenge Updated -  7/4/22
rm(list = ls())
closeAllConnections()

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plater)
library(tibble)
library(stats)
source('~/Desktop/Plate_ReadWrite/convert_384..R')
source('~/Desktop/Plate_ReadWrite/Format_checks.R')

#' @param annotated_path This string is the location of the .csv file that contains the sample's unique IDs (unique_ID) and meta data. 
#' @param platemap_path This string is the location of the .csv file that contains the plate map. See plater format requirements (e.g. rows are labeled A:H, columns are labeled 1:12, and there is one empty row between wells)
#' @param IQue_path This string is the location of the .csv file contains the IQUE data in long format (not plate format) 
#' @param duplicate_dir This boolean is set to TRUE when duplicates are pipetted horizontally and FALSE when duplicates are pipetted vertically
#' @param measurements This list contains the measurements of interest from the IQue data (e.g. "Median BL2-H of 1")
#' @plate_names This is a vector of strings containing the plate names
#' @exp_name This is a string describing the experiment name
#' @param num_386 This is a numeric describing the total # of 386 well plates used in the experiment (that show up in the IQUE data file)
#' @param num_96 This is a numeric describing the total # of 96 well plates used in the experiment (that show up in the IQUE data file)
#' @param save_path This is a string that denotes where the final combined data will be saved
#' @param QC_check This is a numeric that is used as the minimum bead count threshold  

#' Inputs 1 - Biana
annotated_path <- '/Users/maanasa/Downloads/biana_fake_annotated_data.csv'
platemap_path <- '/Users/maanasa/Downloads/PlateLayoutLuminexBiana.csv'
IQue_path <- '/Users/maanasa/Downloads/0526_luminexmicrobiomeVG_CHIM.csv'
duplicate_dir <- TRUE 
measurements <- c("Median.BL2.H.of.12.Rotarix","Count.of.26.LTA.s.pyogenes")
plate_names <- c('P1_TotIgG','P2_IgA1')
num_384 <- 2
num_96 <- 0
exp_name <- "Biana_exp_1"
save_path <- '/Users/maanasa/Downloads/plate_results/'
measurements <- c("Median.BL2.H.of.12.Rotarix","Count.of.26.LTA.s.pyogenes")
QC_check <- 40

#' Inputs 2 - Mock Data Unique Plate
annotated_path <- '/Users/maanasa/Downloads/Annotated1.csv'
platemap_path <- '/Users/maanasa/Downloads/Plate_w_controls1.csv'
IQue_path <- '/Users/maanasa/Downloads/0526_luminexmicrobiomeVG_CHIM.csv'
duplicate_dir <- TRUE 
measurements <- c("Median.BL2.H.of.12.Rotarix","Count.of.26.LTA.s.pyogenes")
plate_names <- c('P1_TotIgG','P2_IgA1')
num_384 <- 2
num_96 <- 0
exp_name <- "Biana_exp_1_unique"
save_path <- '/Users/maanasa/Downloads/plate_results/'
measurements <- c("Median.BL2.H.of.12.Rotarix","Count.of.26.LTA.s.pyogenes")
QC_check <- 40

#' Formatting Checks
check_annotated_data(annotated_path)
check_exp_data(IQue_path)
check_plater_format(platemap_path)

#' Get Annotated Meta Data
sample_meta_data <- read_csv(annotated_path) 
colnames(sample_meta_data) <- tolower(colnames(sample_meta_data))
anno_col <- ncol(sample_meta_data) - 1
sample_meta_data$unique_id <- as.character(sample_meta_data$unique_id)

#' Read IQue data from csv file (exclude first two rows which are the experiment and plate name respectively and continue to exclude 2 rows thereafter because of header and plate name rows)
#' Keep track of plate number
exp_list <- list()
plate_list <- list()
for (i in c(1:num_384))
  {idx = 2 + 386*(i-1) 
  exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE,nrows=384)
  plate_list[[i]] <- rep(i,384)}
#' Combine IQue data from multiple plates (note: do.call combines elements of list using rbind without having to list out each individual element)
exp_df <- do.call(rbind,exp_list)
Plate <- unlist(plate_list)
#' Add Plate # as a new column to the experimental data dataframe
exp_df <- exp_df %>% add_column(Plate,.after = 1)

#' Reads plate data using plater and converts it into the tidyr format (columns: Well, Plate, Unique_ID)
plate_data <- read_plate(file = platemap_path,sep = ',')
colnames(plate_data) <- c('Well.ID',(1:ncol(plate_data)))
plate_data_tidy <- plate_data %>% pivot_longer(names_to = "Plate",values_to = "unique_id",cols = -Well.ID)
num_plates <- ncol(plate_data) - 1
num_wells <- nrow(plate_data)

#' producing the masked experimental data using QC parameter
Count_col <- exp_df[,which(grepl("Count",colnames(exp_df)))]
Median_col <- exp_df[,which(grepl("Median",colnames(exp_df)))]
mask <- ifelse(Count_col[,] > QC_check, TRUE, FALSE)
edited_Median_col <- replace(Median_col, !mask, NA)
QC_exp_data <- cbind(Plate = exp_df$Plate,Well.ID = exp_df$Well.ID,edited_Median_col)

#' If 96 well plates are used in the plate map, 2xnum_384 96 well plates must be provided in the platemap
if (num_wells == 96 & num_plates != 2*num_384) 
  {stop(paste('Insufficient number of 96 well plates were provided in platemap.','# of 96 well plates found: ',num_plates,'.# of 384 well plates used experimentally: ',num_384))}
#' If 384 well plates are used in the plate map, num_384 386 plates must be provided
if (num_wells == 384 & num_plates != num_384) 
  {stop(paste('Insufficient number of 384 well plates were provided in platemap.','# of 384 well plates found: ',num_plates,'.# of 384 well plates used: ',num_384))}
#' If the # of 384 well plates detected from the platemap is equal to the number of 384 well plates specified by the user, we assume that duplicates are accounted for
if (num_wells == 384 & num_plates == num_384) 
  #' Well IDs and Sample Meta data are combined with left join to ensure controls are included within the combined data even if they are not found in the annotated meta data
  {pre_combined_data <- left_join(plate_data_tidy,sample_meta_data,by = "unique_id")
  combined_data <- merge(pre_combined_data,exp_df,by = c("Plate","Well.ID"))
  QC_masked_data <- merge(pre_combined_data,QC_exp_data,by = c("Plate","Well.ID"))}
#' If the # of 96 well plates detected from the platemap is equal to twice the number of 384 well plates specified by the user, we assume that we need to account for duplicates using convert_to()
if (num_wells == 96 & num_plates == 2*num_384) 
  #' Getting a new plate ID list which includes duplicates when mapping well IDs to unique sample IDs by looping over each 384 well plate
  {plate_ID_list <- list()
  for(i in c(1:num_384))
      {plate_ID_list[[i]] <- convert_to(plate_data_tidy,i,plate_names[i],(i*2-1),i*2,duplicate_dir,save_path)}
    new_plate_ID_list <- do.call(rbind,plate_ID_list)
    #' New plate well ID list and Sample Meta data are combined with left join to ensure controls are included within the combined data even if they are not found in the annotated meta data
    pre_combined_data <- left_join(new_plate_ID_list,sample_meta_data,by = "unique_id")  
    combined_data <- merge(pre_combined_data,exp_df,by = c("Plate","Well.ID"))
    QC_masked_data <- merge(pre_combined_data,QC_exp_data,by = c("Plate","Well.ID"))}
#' extracting selected measurements to get abbreviated combined data
if (length(measurements) != 0)
  {combined_data_abbr <- select(combined_data,"unique_id","Plate","Well.ID",measurements)
  write.csv(combined_data_abbr, file=paste0(save_path,'combined_data_abbr_',exp_name,'.csv'))}

#' Calculate median, mean, and standard deviation for the experimental data columns
#' Note: if the controls are labeled with the same identifier, they will be averaged across despite being from different plates
summary_stats <- QC_masked_data %>% group_by(unique_id) %>% summarise(across(c((anno_col + 3):(ncol(QC_masked_data)-1)),funs(mean, median, sd)))
summary_stats_df <- right_join(sample_meta_data,summary_stats,by = "unique_id") 

#' Saving combined data, QC masked, and summary stats data files
write.csv(combined_data, file=paste0(save_path,'combined_data_',exp_name,'.csv'))
write.csv(QC_masked_data, file=paste0(save_path,'QC_masked_combined_data_',exp_name,'.csv'))
write.csv(summary_stats_df,file=paste0(save_path,'summary_stats',exp_name,'.csv'))

#' Issues to fix still
#' 1) Summary stats for the rows corresponding to the controls are not properly being calculated. It returns NA maybe due to the different group sizes?
#' 2) In general, how to deal with controls in the summary stats 
#' 3) Add functionality for 96 well plates and 48 well plate maps, which might be useful for functional assays?
#' 4) Decide on whether to make duplicated unique_ids in annotated data an error or a warning
#' 6) Decide on whether to read in the plate names and # of 384 plates from the experimental data during format checks or not (currently this functionality is commented out)
#' 5) Gather and make unit testing datasets to evaluate this
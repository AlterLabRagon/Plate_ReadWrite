### Plate Challenge - Revamped 
rm(list = ls())
closeAllConnections()

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plater)
library(platetools)
library(viridis)
library(stats)
source('~/Desktop/Plate_ReadWrite/convert_384..R')
source('~/Desktop/Plate_ReadWrite/Format_checks.R')

#' @param annotated_file_path This string is the location of the .csv file that contains the sample's unique IDs (unique_ID) and meta data. 
#' @param plate_map_path This string is the location of the .csv file that contains the plate map. See plater format requirements (e.g. rows are labeled A:H, columns are labeled 1:12, and there is one empty row between wells)
#' @param IQue_path This string is the location of the .csv file contains the IQUE data in long format (not plate format) 
#' @param duplicate_dir This boolean is set to TRUE when duplicates are pipetted horizontally and FALSE when duplicates are pipetted vertically
#' @param measurements This list contains the measurements of interest from the IQue data (e.g. "Median BL2-H of 1")
#' @param plate_names This is a vector of strings containing the plate names
#' @param num_386 This is a numeric describing the total # of 386 well plates used in the experiment (that show up in the IQUE data file)
#' @param SAVE_PATH This is a string that denotes where the final combined data will be saved

#' Inputs
biana_IQue_path <- '/Users/maanasa/Downloads/0526_luminexmicrobiomeVG_CHIM.csv'
biana_plate_path <- '/Users/maanasa/Downloads/PlateLayoutLuminexBiana.csv'
num_384 <- 2
biana_annotated_data_path <- '/Users/maanasa/Downloads/biana_fake_annotated_data.csv'
biana_plate_names <- c('P1_TotIgG','P2_IgA1')
SAVE_PATH <- '/Users/maanasa/Downloads/results-plate/' 
measurements <- c("Median.BL2.H.of.12.Rotarix","Count.of.26.LTA.s.pyogenes")
QC_parameter <- 20

#' Formatting Checks
check_annotated_meta_data_format(biana_annotated_data_path)
plates <- check_exp_data_format(biana_IQue_path)
check_plater_format(biana_plate_path)

#' Get Annotated Meta Data
biana_meta_data <- read_csv(biana_annotated_data_path) 
unique_IDs <- biana_meta_data[ , grepl( "Unique_ID|unique_ID|unique_id|Unique_id" , names(biana_meta_data) ) ]

#' Reads plate data using plateR which converts it into the tidy format (num_wells X num_plates)
biana_plate_data <- read_plate(file = biana_plate_path,sep = ',')
num_plates <- ncol(biana_plate_data) - 1
num_wells <- nrow(biana_plate_data)

#' If 96 well plates are used in the plate map, 2xnum_384 must be provided (ncol = num_384*2)
#' If 384 well plates are used in the map, num_384 plates must be provided (ncol = num_384)
if (num_wells == 96 & num_plates != 2*num_384) 
{stop(paste('Insufficient number of 96 well plates were provided in platemap.','# of 96 well plates found: ',num_plates,'.# of 384 well plates used experimentally: ',num_384))}
if (num_wells == 384 & num_plates != num_384) 
{stop(paste('Insufficient number of 384 well plates were provided in platemap.','# of 384 well plates found: ',num_plates,'.# of 384 well plates used: ',num_384))}

#' generating the names of the experimental, plate_ID, combined data, and combined data abbreviated dataframes
exp_df_names = c()
edited_plate_ID_list = c()
combined_data_names = c()
exp_df_abbr_names =c()
combined_data_abbr_names = c()

for (i in c(1:num_384))
{new_exp_data <- paste('exp_data',i,sep='_')
new_plate_ID <- paste('plate_ID_list',i,sep="_")
new_combined_data <- paste('combined_data',i,sep="_")
new_combined_data_abbr <- paste('combined_data_abbr',i,sep="_")
new_exp_data_abbr <- paste('exp_data_abbr',i,sep="_")

exp_df_names <- append(exp_df_names,new_exp_data)
edited_plate_ID_list <- append(edited_plate_ID_list,new_plate_ID)
combined_data_names <- append(combined_data_names,new_combined_data)
combined_data_abbr_names <- append(combined_data_abbr_names,new_combined_data_abbr)
exp_df_abbr_names <- append(exp_df_abbr_names,new_exp_data_abbr)}

#' assigning the experimental data to the names
for (i in c(1:num_384))
  #' if there are 3 lines before the start of the IQue data, 2 + 386(i-1)
{idx = 2 + 386*(i-1) 
assign(exp_df_names[i],read.csv(biana_IQue_path, skip=idx, header=TRUE,nrows=384))
assign(exp_df_abbr_names[i],select(get(exp_df_names[i]),"Well.ID", measurements)) }

for(i in c(1:num_384))
{assign(edited_plate_ID_list[i],convert_to_384(biana_plate_data,biana_plate_names[i],1+i,2+i,TRUE,SAVE_PATH))
assign(combined_data_names[i],merge(get(edited_plate_ID_list[i]),get(exp_df_names[i]),by.x = "plate_well_ID",by.y = "Well.ID")) 
assign(combined_data_names[i],merge(biana_meta_data,get(combined_data_names[i]),by.x = colnames(biana_meta_data)[grepl( "Unique_ID|unique_ID|unique_id|Unique_id" , names(biana_meta_data))],by.y="plate_ID_list"))
# Getting abbreviated combined data
assign(combined_data_abbr_names[i],merge(get(edited_plate_ID_list[i]),get(exp_df_abbr_names[i]),by.x = "plate_well_ID",by.y = "Well.ID")) 
assign(combined_data_abbr_names[i],merge(biana_meta_data,get(combined_data_abbr_names[i]),by.x = colnames(biana_meta_data)[grepl( "Unique_ID|unique_ID|unique_id|Unique_id" , names(biana_meta_data))],by.y="plate_ID_list"))

# Maybe should save as different sheets in 1 xlsx file instead of separate files?
write.csv(get(combined_data_names[i]), file=paste0(SAVE_PATH,'combined_data_',biana_plate_names[i],'.csv'))
write.csv(get(combined_data_abbr_names[i]), file=paste0(SAVE_PATH,'combined_data_abbr_',biana_plate_names[i],'.csv'))}


##### Random QC attempts
group_mean <- aggregate(combined_data_1[,6:ncol(combined_data_1)], list(combined_data_1[,1]), mean)
group_median <- aggregate(combined_data_1[,6:ncol(combined_data_1)], list(combined_data_1[,1]), median)
group_sd <- aggregate(combined_data_1[,6:ncol(combined_data_1)], list(combined_data_1[,1]), sd)
group_mean2 <- aggregate(combined_data_1[,6:ncol(combined_data_1)], list(combined_data_1[,1]), mean,subset = )

### Helpful to get counts data alone?
Counts_data <- combined_data_1[,which(grepl("Count",colnames(combined_data_1)))]

# unique_id_name <- colnames(biana_meta_data)[grepl( "Unique_ID|unique_ID|unique_id|Unique_id" , names(biana_meta_data))]

### masking in R
### Got bead count columns
### Got other columns
### subsetting in R cmt[cmt,] > 20
### need to have it as a mtatrix
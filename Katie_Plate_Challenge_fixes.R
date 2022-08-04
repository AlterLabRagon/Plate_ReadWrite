### Plate Challenge Updated -  8/1/22

# Change how the QC is done but there are fewer count cols than medians which is interesting
# Add correlation plots -- looping through and producing correlation coefficients for the unique IDs group + Same plate (okay assumption?)
# Maybe diff antigens to the bead and diff secondaries can be incorporated in the plate read phase

rm(list = ls())
closeAllConnections()

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plater)
library(tibble)
library(stats)

#' @wd_path wd_path This is the path of the working directory 
#' @param annotated_name This string is the name of the .csv file that contains the sample's unique IDs (unique_ID) and meta data. 
#' @param platemap_name This string is the name of the .csv file that contains the plate map. See plater format requirements (e.g. rows are labeled A:H, columns are labeled 1:12, and there is one empty row between wells)
#' @param plate_to_ique This string is the name of the .csv file that contains the beadmap, describing the mapping between the experimental plates, platemap, beadtype, and secondaries used
#' @param IQue_name This string is the name of the .csv file contains the IQUE data in long format (not plate format) 
#' @param duplicate_dir This boolean is set to TRUE when duplicates are pipetted horizontally and FALSE when duplicates are pipetted vertically
#' @param measurements This list contains the measurements of interest from the IQue data (e.g. "Median BL2-H of 1")
#' @plate_names This is a vector of strings containing the plate names
#' @exp_name This is a string describing the experiment name
#' @param num_386 This is a numeric describing the total # of 386 well plates used in the experiment (that show up in the IQUE data file)
#' @param num_96 This is a numeric describing the total # of 96 well plates used in the experiment (that show up in the IQUE data file)
#' @param num_well_in_platemap This is a numeric describing the # of wells used in the platemap (options are 96 and 384)
#' @param save_path This is a string that denotes where the final combined data will be saved
#' @param QC_check This is a numeric that is used as the minimum bead count threshold  

source('/Users/maanasa/Desktop/Plate_ReadWrite_new/Plate_ReadWrite/convert_384..R')
source('/Users/maanasa/Desktop/Plate_ReadWrite_new/Plate_ReadWrite/Format_checks.R')

# Set Working Directory
wd_path <- '/Users/maanasa/Desktop/Plate_ReadWrite_new/Plate_ReadWrite/Katie/'
setwd(wd_path) 
## Example of working directory path that works is /Users/maanasa/Desktop/Plate_ReadWrite_new/Plate_ReadWrite/ (no quotation marks needed; is the slash needed?)


#' Inputs : Katie
annotated_name <- 'katie_fake_annotated_data.csv'
platemap_name <- 'Katie_Lum_plate_IgM.csv'
plate_to_ique <- 'bead_map?.csv'
IQue_name <- 'edited_data/20220722_Dheda_TB_Luminex_IgM_complete.csv'

duplicate_dir <- TRUE 
measurements <- c()
plate_names <- c(1:8)
num_384 <- 8
num_96 <- 0
num_well_in_platemap <- 384
exp_name <- "20220722_Dheda_TB_Luminex_IgM"
save_path <- '/Users/maanasa/Desktop/Plate_ReadWrite_new/Plate_ReadWrite/Katie/new_results/'
QC_check <- 40

#' Get full file paths
annotated_path <- paste0(wd_path,annotated_name)
platemap_path <- paste0(wd_path,platemap_name)
IQue_path <- paste0(wd_path,IQue_name)
plate_to_IQue_path <- paste0(wd_path,plate_to_ique)

#' Formatting Checks
check_annotated_data(annotated_path) # Double check warning
check_exp_data(IQue_path,measurements)
check_plater_format(platemap_path)
check_data_key(plate_to_IQue_path)
# will add in a check beadmap_path if that is okay...?

# Get beadmap data -- add formatting checks (plate, platemap, beadtype / beat sets, and secondary) + also need to check / add warning if plate names in num_exp is not unique
beadmap_data <- read_csv(plate_to_IQue_path)
colnames(beadmap_data) <- tolower(colnames(beadmap_data))
num_exp <- length(unique(beadmap_data$plate))
num_platemap <- length(unique(beadmap_data$platemap))
num_beadtype <- length(unique(beadmap_data$set))
num_secondary <- length(unique(beadmap_data$secondary))
### Various situations : 96 + 96 ; 96 + 384 ; 384 + 384

if (!num_well_in_platemap %in% c(96, 384))
  {stop('The number of wells in the platemap must be 96 or 384')}

# Check that the # of different places specified by the data key matches the # of plates run in the experiment
if (num_well_in_platemap == 384 & num_exp != num_platemap | num_well_in_platemap == 96 & num_exp != num_platemap*2)
  {if (num_well_in_platemap == 384)
    {check_mapping <- (num_exp == num_platemap * num_beadtype * num_secondary)
    if (check_mapping == TRUE) 
      {platemap_list <- beadmap_data$platemap}
    else
      {stop(paste0('The number of platemaps provided does not match number of experimental plates, considering the different bead sets and secondaries used.','\n Number of beadtypes: ',num_beadtype,'\n Number of secondaries: ',num_secondary,'\n Number of plates in the platemap: ',num_platemap,'\n Number of plates used experimentally: ',num_exp))}}
  if (num_well_in_platemap == 96)  
    {check_mapping <- (num_exp == num_platemap * num_beadtype * num_secondary * 2) 
    if (check_mapping == TRUE)
      {platemap_list <- beadmap_data$platemap}
    else
      {stop(paste0('The number of platemaps provided does not match number of experimental plates, considering the different bead sets and secondaries used.','\n Number of beadtypes: ',num_beadtype,'\n Number of secondaries: ',num_secondary,'\n Number of plates in the platemap: ',num_platemap,'\n Number of plates used experimentally: ',num_exp))}
    } 
  }

## Actual code
if (num_well_in_platemap == 384 & num_exp != num_platemap | num_well_in_platemap == 96 & num_exp != num_platemap*2)
{if (num_well_in_platemap == 384)
{check_mapping <- (num_exp == num_platemap * num_beadtype * num_secondary)
if (check_mapping == TRUE) 
{platemap_list <- beadmap_data$platemap
bead_names <- paste0('.',beadmap_data$set)
secondary_names <- paste0('.',beadmap_data$secondary)}
else
{stop(paste0('The number of platemaps provided does not match number of experimental plates, considering the different bead sets and secondaries used.','\n Number of beadtypes: ',num_beadtype,'\n Number of secondaries: ',num_secondary,'\n Number of plates in the platemap: ',num_platemap,'\n Number of plates used experimentally: ',num_exp))}
}
  if (num_well_in_platemap == 96)  
  {check_mapping <- (num_exp == num_platemap * num_beadtype * num_secondary * 2) 
  if (check_mapping == TRUE)
  {bead_names <- paste0('.',beadmap_data$set)
  secondary_names <- paste0('.',beadmap_data$secondary)}
  else
  {stop(paste0('The number of platemaps provided does not match number of experimental plates, considering the different bead sets and secondaries used.','\n Number of beadtypes: ',num_beadtype,'\n Number of secondaries: ',num_secondary,'\n Number of plates in the platemap: ',num_platemap,'\n Number of plates used experimentally: ',num_exp))}
  } 
}



#' Reads plate data using plater and converts it into the tidyr format (columns: Well, Plate, Unique_ID)
plate_data <- read_plate(file = platemap_path,sep = ',')
num_plates <- ncol(plate_data) - 1
num_wells <- nrow(plate_data) 
# Adds in the # of platemap plates needed for the number of experimental plates present
if (num_well_in_platemap == 384)
  {plate_data_cols <- rep(paste0('values.',c((num_plates + 1):num_384)))} 
if (num_well_in_platemap == 96)
  {plate_data_cols <- rep(paste0('values.',c((num_plates + 1):2*num_384)))}

## Adds in empty rows for the platemaps that should be there but are not
for(i in plate_data_cols)
  plate_data[,i] <- NA
# Adds the platemaps used to make each plate into a list
platemaps <- list()
for (i in platemap_list)
  platemaps <- append(platemaps,plate_data[i+1])
# Create new unique ids that include the bead set and secondary name
for (i in c(1:(num_384)))
  {plate_data[, i + 1] <- rep(paste0(platemaps[[i]],bead_names[i],secondary_names[i],sep = '.')) }

# Convert platedata into a tidyr format
colnames(plate_data) <- c('Well.ID',(1:ncol(plate_data)))
plate_data_tidy <- plate_data %>% pivot_longer(names_to = "Plate",values_to = "unique_id_edit",cols = -Well.ID)

#' Get Annotated Meta Data
sample_meta_data <- read_csv(annotated_path) 
colnames(sample_meta_data) <- tolower(colnames(sample_meta_data))
anno_col <- ncol(sample_meta_data) - 1
sample_meta_data$unique_id <- as.character(sample_meta_data$unique_id)

#' A more robust way to read in IQue data
plate.indic <- "Plate: "
# read in the raw file
lines <- readLines(IQue_path)
# find the start of the new plates using the "Plate: " pattern
plate.start <- grep(plate.indic, lines)
if (num_384 + num_96 != length(plate.start))
{warning(paste0('The # of plates identified in the IQue data is not equal to the # of plates specified by the user.',' Number of plates in IQue data: ',length(plate.start),". Number of user-entered 384 well plates is ",num_384,' & user entered 96 well plates is ',num_96))}
# get plate names listed in the IQue file
lines2 <- gsub('(^"|"$|,)', "", lines)
plate_names <- lines2[plate.start]

#' Read IQue data from csv file (exclude first two rows which are the experiment and plate name respectively and continue to exclude 2 rows thereafter because of header and plate name rows)
#' Keep track of plate number
exp_list <- list()
plate_list <- list()
plate_names_list <- list()

if (num_384 + num_96 < length(plate.start))
  {for (i in c(1:num_384))
    {idx = plate.start[i] 
    exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE,nrows=(plate.start[i+1] - plate.start[i] - 2))
    plate_list[[i]] <- rep(i,plate.start[i+1] - plate.start[i] - 2)} }

## Need to add in functionality specific to num_96 or num_384
if (num_384 == length(plate.start))
  for (i in c(1:num_384))
  {if (i != num_384)
  {idx = plate.start[i] 
  exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE,nrows=(plate.start[i+1] - plate.start[i] - 2))
  plate_list[[i]] <- rep(i,plate.start[i+1] - plate.start[i] - 2)
  plate_names_list[[i]] <- rep(plate_names[i],plate.start[i+1] - plate.start[i] - 2)}
    else
    {idx = plate.start[i] 
    exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE)
    plate_list[[i]] <- rep(i,dim(exp_list[[i]])[1])
    plate_names_list[[i]] <- rep(plate_names[i],dim(exp_list[[i]])[1])}}

if (num_96 == length(plate.start))
  for (i in c(1:num_96))
  {if (i != num_96)
  {idx = plate.start[i] 
  exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE,nrows=(plate.start[i+1] - plate.start[i] - 2))
  plate_list[[i]] <- rep(i,plate.start[i+1] - plate.start[i] - 2)
  plate_names_list[[i]] <- rep(plate_names[i],plate.start[i+1] - plate.start[i] - 2)}
    else
    {idx = plate.start[i] 
    exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE)
    plate_list[[i]] <- rep(i,dim(exp_list[[i]])[1])
    plate_names_list[[i]] <- rep(plate_names[i],dim(exp_list[[i]])[1])}}

if (num_384 + num_96 > length(plate.start))
  warning(paste0('Error: The number of plates entered by the user is not equal to the # of plates found in the data. ',
                 'User entered plates: ',num_384 + num_96,' Plates identified experimentally: ',length(plate.start)))

#' Combine IQue data from multiple plates (note: do.call combines elements of list using rbind without having to list out each individual element)
exp_df <- do.call(rbind,exp_list)
Plate <- unlist(plate_list)
Plate_names <- unlist(plate_names_list)
#' Add Plate # as a new column to the experimental data dataframe
exp_df <- exp_df %>% add_column(Plate,.after = 1)
exp_df <- exp_df %>% add_column(Plate_names,.after = 2)

if (num_well_in_platemap == 384) 
  #' Well IDs and Sample Meta data are combined with left join to ensure controls are included within the combined data even if they are not found in the annotated meta data
{plate_data_tidier <- data.frame(do.call('rbind', strsplit(as.character(plate_data_tidy$unique_id_edit), '.', fixed=TRUE)))
colnames(plate_data_tidier) <- c('unique_id','beadset','secondary')
plate_data_tidyr <- cbind(plate_data_tidy,plate_data_tidier)
plate_data_tidyr <- plate_data_tidyr %>% dplyr::filter(unique_id != 'NA')
pre_combined_data <- left_join(plate_data_tidyr,sample_meta_data,by = "unique_id")
combined_data <- merge(pre_combined_data,exp_df,by = c("Plate","Well.ID")) }

#' Producing the masked experimental data using QC parameter
Count_col <- exp_df[,which(grepl("Count",colnames(exp_df)))]
Median_col <- exp_df[,which(grepl("Median",colnames(exp_df)))]
if (dim(Count_col)[2] == dim(Median_col)[2])
  {mask <- ifelse(Count_col[,] > QC_check, TRUE, FALSE)
  edited_Median_col <- replace(Median_col, !mask, NA)
  QC_exp_data <- cbind(Plate = exp_df$Plate,Well.ID = exp_df$Well.ID,edited_Median_col)
  QC_masked_data <- merge(pre_combined_data,QC_exp_data,by = c("Plate","Well.ID"))
} else 
{warning(paste0('The number of bead count and median measurements provided are not equal, so a QC output will not be generated.','\n Number of median columns: ',dim(Median_col)[2],'\n Number of bead count columns: ', dim(Count_col)[2])) }


#' If the # of 96 well plates detected from the platemap is equal to twice the number of 384 well plates specified by the user, we assume that we need to account for duplicates using convert_to()
if (num_well_in_platemap == 96 & num_384 != 0) 
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
edited_Median_col <- cbind(Plate = exp_df$Plate,Well.ID = exp_df$Well.ID,Median_col)
Median_only_data <- merge(plate_data_tidyr,edited_Median_col,by = c("Plate","Well.ID"))

# Need to add plate # Or Well ID ? Does that make sense (PLATE IS NEEDED HERE)
summary_stats <- Median_only_data %>% group_by(unique_id,Plate) %>% summarise(across(c((anno_col + 3):(ncol(Median_only_data)-2)),list(mean = mean, median = median, sd = sd)))
summary_stats <- summary_stats %>% arrange(Plate)
#summary_stats_df <- right_join(sample_meta_data,summary_stats,by = "unique_id") 

# Create a pdf of correlation plots?
p1<-c()
i<-0

# Initialize parameters
idlist <- unique(Median_only_data$unique_id_edit)
platelist <- unique(Median_only_data$Plate)
library(gridExtra)

# for each feature
for (plate_l in platelist){
  plotting_data <- Median_only_data[Median_only_data$Plate == plate_l,]
  for (curfeat2 in idlist){
    curdata <- plotting_data[plotting_data$unique_id_edit==curfeat2,]
    curdata2 <- pivot_longer(curdata,-c(1:6),names_to = 'measurement',values_to = 'value')
    curdata3 <- pivot_wider(curdata2,names_from = "Well.ID", values_from = "value")
    colnames(curdata3)[(ncol(curdata3)-1):ncol(curdata3)] <- c('duplicate_1','duplicate_2')
    i <- i+1
    p1[i] <- lapply(curfeat2, function(curfeat2) {  
      ggplot(data = curdata3, aes(x = duplicate_1, y = duplicate_2, colour = measurement)) + 
        geom_point(size=2) + geom_smooth(method = "lm", se=FALSE)  + xlim(0,6000) + ylim(0,6000) +
       stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
       stat_regline_equation(label.y = 350, aes(label = ..rr.label..)) + 
        ggtitle(paste0("Unique_id: ",curfeat2, "     Plate: ",plate_l)) +
        theme(text = element_text(size= 10, hjust=0.5),
            title = element_text(size= 10, hjust=0.5),
            line = element_line(size=1),
            plot.title = element_text(hjust = 0.5)) +
        theme(legend.position="none",plot.title = element_text(hjust = 0.5)) } ) } 
}


curfilname<-paste0("/Users/maanasa/Desktop/Plate_ReadWrite_new/Plate_ReadWrite/Katie/results/corrplots.pdf")

# set row and column numbers and title
ml <- marrangeGrob(p1, nrow=3, ncol=4, top="ptc")
ggsave(curfilname, ml, width = 20, height = 10, dpi = 300, limitsize=FALSE)

dev.off()




#' Saving combined data, QC masked, and summary stats data files
write.csv(combined_data, file=paste0(save_path,'combined_data_',exp_name,'.csv'))
write.csv(Median_only_data, file=paste0(save_path,'QC_masked_combined_data_',exp_name,'.csv'))
write.csv(summary_stats,file=paste0(save_path,'summary_stats',exp_name,'.csv'))

#' Issues to fix still
#' 1) Summary stats for the rows corresponding to the controls are not properly being calculated. It returns NA maybe due to the different group sizes?
#' 2) In general, how to deal with controls in the summary stats 
#' 3) Add functionality for 96 well plates and 48 well plate maps, which might be useful for functional assays?
#' 4) Decide on whether to make duplicated unique_ids in annotated data an error or a warning
#' 6) Decide on whether to read in the plate names and # of 384 plates from the experimental data during format checks or not (currently this functionality is commented out)
#' 5) Gather and make unit testing datasets to evaluate this
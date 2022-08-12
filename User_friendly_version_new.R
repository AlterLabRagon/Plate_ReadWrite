rm(list = ls())
closeAllConnections()
options(warn=1)

### INPUTS TO CHANGE
##########################################################################
##########################################################################
##########################################################################

### Required parameters (to change)
##########################################################################
duplicate_dir <- TRUE 
measurements <- NULL
num_384 <- 4
num_96 <- 0
num_well_in_platemap <- 96
exp_name <- 'Bead_party'
save_path <- '/Users/maanasa/Desktop/Plate_ReadWrite_new/Plate_ReadWrite/Example_markdown/'
QC_check <- 40
platemap_long <- FALSE
  
### Required files (leave as NUlL to be prompted with a file pop up)
##########################################################################
annotated_path <- 'Example/Data/annotated_data.csv'
platemap_path <- 'Example/Data/platemap_small.csv'
datakey_path <- 'Example/Data/beadmap_small.csv'
IQue_path <- 'Example/Data/ique_data.csv'
  
### Optional parameters (to change)
##########################################################################
QC_check <- NULL
plate_names <- NULL
corr_plot <- NULL
platemap_long_format <- NULL

### Information about parameters
##########################################################################
#' @param annotated_path This string is the path of the .csv file that contains the sample's unique IDs (unique_ID) and meta data. 
#' @param platemap_path This string is the path of the .csv file that contains the plate map. See plater format requirements (e.g. rows are labeled A:H, columns are labeled 1:12, and there is one empty row between wells)
#' @param datakey_path This string is the path of the .csv file that contains the beadmap, describing the mapping between the experimental plates, platemap, beadtype, and secondaries used
#' @param IQue_path This string is the path of the .csv file contains the IQUE data in long format (not plate format) 
#' @param duplicate_dir This boolean is set to TRUE when duplicates are pipetted horizontally and FALSE when duplicates are pipetted vertically
#' @param measurements This list contains the measurements of interest from the IQue data (e.g. "Median BL2-H of 1"). It can be empty.
#' @plate_names This is a vector of strings containing the plate names
#' @param num_386 This is a numeric describing the total # of 386 well plates used in the experiment (that show up in the IQUE data file)
#' @param num_96 This is a numeric describing the total # of 96 well plates used in the experiment (that show up in the IQUE data file)
#' @param num_well_in_platemap This is a numeric describing the # of wells used in the platemap (options are 96 and 384)
#' @param save_path This is a string that denotes where the final combined data will be saved
#' @param exp_name This is a string that describes the name of the experiment
#' @param QC_check This is a numeric that is used as the minimum bead count threshold  
#' @param corr_plot This is a boolean that is set to TRUE when you want to plot correlation for duplicates for each plate and unique_id 
#' @param platemap_long_format This is a boolean that is set to TRUE when your platemap is already in long format with Well IDs (A01) mapped to the samples

# Prompting a file pop up to get the annotated, platemap, datakey, and IQue data
if(is.null(annotated_path))
  {annotated_path = rstudioapi::selectFile(caption = "Select Annotated Data File",label = "Select Annotated Data File",filter = "CSV Files (*.csv)",existing = TRUE)}
if(is.null(platemap_path))
  {platemap_path <- rstudioapi::selectFile(caption = "Select Platemap File",label = "Select Platemap File",filter = "CSV Files (*.csv)",existing = TRUE)}
if(is.null(datakey_path))
  {datakey_path <- rstudioapi::selectFile(caption = "Select Data Key File",label = "Select Data Key File",filter = "CSV Files (*.csv)",existing = TRUE)}
if(is.null(IQue_path))
  {IQue_path <- rstudioapi::selectFile(caption = "Select iQue Data File",label = "Select iQue Data File",filter = "CSV Files (*.csv)",existing = TRUE)}
# If no plate names are provided, integer names will be assigned to the plates
if(is.null(plate_names))
  {actual_num_plate <- ifelse(num_96 == 0, num_384, num_96)
  plate_names <- c(1:actual_num_plate)}
# If no QC check is provided, a minimum bead count threshold of 20 is set
if(is.null(QC_check))
  {QC_check <- 20}
# If no option is provided for plotting correlation plot, it is set to FALSE
if(is.null(corr_plot))
  {corr_plot <- FALSE}
# If no user input is provided for whether the platemap is in long format or not, we assume that it is not in long format
if(is.null(platemap_long_format))
  {platemap_long_format <- FALSE}


### CODE TO RUN
##########################################################################
##########################################################################
##########################################################################

### Libraries
##########################################################################
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plater)
library(tibble)
library(stats)
library(devtools)

source('Src/Format_checks2.R')
source('Src/convert_384..R')

### Create an Error and Warning Log Folder and File
dir.create(file.path(save_path,'Log'))
log_path <- file.path(save_path,'Log')

get_log_file_name <- function(file_name="log_file")
  {file_name <- paste(file_name,format(Sys.time(), "%Y%m%d"),sep="_")
  file_name <- paste(log_path, paste(file_name,"log", sep="."), sep="/")
  return(file_name)}

logger <- file(get_log_file_name(), open = "a")
sink(logger, append = TRUE, type="message")

### Perform Formatting Checks
##########################################################################
check_annotated_data(annotated_path)
check_exp_data(IQue_path,measurements,num_384,num_96)
check_plater_format(platemap_path)
check_data_key(datakey_path)

### Get Beadmap Data
##########################################################################
beadmap_data <- read_csv(datakey_path)
colnames(beadmap_data) <- tolower(colnames(beadmap_data))
platemap_col <- grep('platemap',colnames(beadmap_data))
num_exp <- length(unique(beadmap_data$plate))
num_platemap <- length(unique(beadmap_data$platemap))
num_beadtype <- length(unique(beadmap_data$set))
num_secondary <- length(unique(beadmap_data$secondary))

if (!num_well_in_platemap %in% c(96, 384))
  {stop('The number of wells in the platemap must be 96 or 384')}

### Check that the # of different plates specified by the data key matches the # of plates run in the experiment
##########################################################################
if (num_well_in_platemap == 384 & num_exp != num_platemap | num_well_in_platemap == 96 & num_exp != num_platemap*2 |num_well_in_platemap == 96 & num_exp != num_platemap)
  {check_mapping <- (num_exp == num_platemap * num_beadtype * num_secondary)
  check_mapping_w_d <- (num_exp == (num_platemap * num_beadtype * num_secondary) / 2)
    if (any(c(check_mapping,check_mapping_w_d) == TRUE))
      {platemap_list <- beadmap_data$platemap
      bead_names <- paste0('.',beadmap_data$set)
      secondary_names <- paste0('.',beadmap_data$secondary)}
    else
      {stop(paste0('The number of platemaps provided does not match number of experimental plates, considering the different bead sets and secondaries used.','\n Number of beadtypes: ',num_beadtype,'\n Number of secondaries: ',num_secondary,'\n Number of plates in the platemap: ',num_platemap,'\n Number of plates used experimentally: ',num_exp))}}

### Get Annotated Meta Data
##########################################################################
sample_meta_data <- read_csv(annotated_path) 
colnames(sample_meta_data) <- tolower(colnames(sample_meta_data))
anno_col <- ncol(sample_meta_data) - 1
sample_meta_data$unique_id <- as.character(sample_meta_data$unique_id)

### Get Experimental Data
##########################################################################
# Setting the pattern that comes before the start of a new plate
plate.indic <- "Plate: "
# Read in the raw file
lines <- readLines(IQue_path)
# Find the start of the new plates using the plate.indic pattern
plate.start <- grep(plate.indic, lines)
# Get plate names that are listed in the experimental data file (1 plate name per row)
lines2 <- gsub('(^"|"$|,)', "", lines)
plate_names <- lines2[plate.start]
# Initialize the lists that will have the experiments, plate #, and plate names
exp_list <- list()
plate_list <- list()
plate_names_list <- list()
# Read experimental data from csv file when the number of plates the user is interested in is less than the # of plates found
if (num_384 + num_96 < length(plate.start))
  {actual_num_plate <- ifelse(num_96 == 0, num_384, num_96)
  for (i in c(1:actual_num_plate))
    {idx = plate.start[i] 
    exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE,nrows=(plate.start[i+1] - plate.start[i] - 2))
    plate_list[[i]] <- rep(i,plate.start[i+1] - plate.start[i] - 2)
    plate_names_list[[i]] <- rep(plate_names[i],plate.start[i+1] - plate.start[i] - 2) } }
# Read experimental data from csv file when the number of plates the user is interested in is equal to the # of plates 
if (num_384 + num_96 == length(plate.start))
  {actual_num_plate <- ifelse(num_96 == 0, num_384, num_96)
  for (i in c(1:actual_num_plate))
    {if (i != actual_num_plate)
      {idx = plate.start[i] 
      exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE,nrows=(plate.start[i+1] - plate.start[i] - 2))
      plate_list[[i]] <- rep(i,plate.start[i+1] - plate.start[i] - 2)
      plate_names_list[[i]] <- rep(plate_names[i],plate.start[i+1] - plate.start[i] - 2)}
    else
      {idx = plate.start[i] 
      exp_list[[i]] <- read.csv(IQue_path, skip=idx, header=TRUE)
      plate_list[[i]] <- rep(i,dim(exp_list[[i]])[1])
      plate_names_list[[i]] <- rep(plate_names[i],dim(exp_list[[i]])[1])}}}
# Outputs error when the number of plates the user is interested in is greater than the # of plates found
if (num_384 + num_96 > length(plate.start))
  stop(paste0('Error: The number of plates entered by the user is greater than the # of plates found in the data. ',
                 'User entered plates: ',num_384 + num_96,' Plates identified experimentally: ',length(plate.start)))
# Combine experimental data from multiple plates
exp_df <- do.call(rbind,exp_list)
Plate <- unlist(plate_list)
Plate_names <- unlist(plate_names_list)
#Add Plate # as a new column to the experimental data frame
exp_df <- exp_df %>% add_column(Plate,.after = 1)
exp_df <- exp_df %>% add_column(Plate_names,.after = 2)

### Reads plate data using plater and converts it into the tidyr format
##########################################################################
plate_data <- read_plate(file = platemap_path,sep = ',')
num_plates <- ncol(plate_data) - 1
num_wells <- nrow(plate_data) 

# We are creating column names to account for the platemaps that are not represented in the platemap file due to different secondaries, beadsets, or duplicates
if (num_well_in_platemap == 384)
  {plate_data_cols <- rep(paste0('values.',c((num_plates + 1):num_384)))} 
if (num_well_in_platemap == 96 & num_384 != 0)
  {plate_data_cols <- rep(paste0('values.',c((num_plates + 1):(num_384*2))))}
if (num_well_in_platemap == 96 & num_96 != 0)
  {plate_data_cols <- rep(paste0('values.',c((num_plates + 1):num_96)))}

# Adds in empty columns to account for the plates that should be present
for(i in plate_data_cols)
  plate_data[,i] <- NA
# Initializes platemaps list and adds platemap data in an order that corresponds to the data key file
platemaps <- list()
for (i in platemap_list)
  platemaps <- append(platemaps,plate_data[i+1])

# Create new unique ids that include the bead set and secondary names. The various conditions account for the # of plates that were run experimentally if there were 96, 384 without duplicates, or 384 with duplicates
if (num_well_in_platemap == 384)
  {for (i in c(1:(num_384)))
    {plate_data[, i + 1] <- rep(paste0(platemaps[[i]],bead_names[i],secondary_names[i],sep = '.')) } }
if (num_well_in_platemap == 96 & num_96 != 0)
  {for (i in c(1:(num_96)))
    {plate_data[, i + 1] <- rep(paste0(platemaps[[i]],bead_names[i],secondary_names[i],sep = '.')) } }
if (num_well_in_platemap == 96 & num_384 != 0)
  {for (i in c(1:(2*num_384)))
    {plate_data[, i + 1] <- rep(paste0(platemaps[[i]],bead_names[i],secondary_names[i],sep = '.')) } }

# Accounts for duplicates by producing a tidy version of the sample to plate and well id mappings, taking into account duplicates
if (num_well_in_platemap == 96 & num_384 != 0)
  {colnames(plate_data) <- c('Well.ID',(1:ncol(plate_data)))
  plate_data_tidy <- plate_data %>% pivot_longer(names_to = "Plate",values_to = "unique_id",cols = -Well.ID)
  plate_ID_list <- list()
  for(i in c(1:(num_384)))
    {plate_ID_list[[i]] <- convert_to(plate_data_tidy,i,plate_names[i],(i*2-1),i*2,duplicate_dir,save_path)}
  new_plate_ID_list <- do.call(rbind,plate_ID_list)
  colnames(new_plate_ID_list) <- c("Plate","Well.ID","unique_id_edit")
}


# Convert plate data into a tidyr format if there are 384 wells in the platemap
if (num_well_in_platemap == 384)
  {colnames(plate_data) <- c('Well.ID',(1:ncol(plate_data)))
  plate_data_tidy <- plate_data %>% pivot_longer(names_to = "Plate",values_to = "unique_id_edit",cols = -Well.ID)}

### Merge the plate data with the meta data and experimental data
##########################################################################
if (num_well_in_platemap == 384 | num_well_in_platemap == 96 & num_96 != 0) 
  { # Split the unique_id_edit to get the unique_id, beadset, and secondary
  plate_data_tidier <- data.frame(do.call('rbind', strsplit(as.character(plate_data_tidy$unique_id_edit), '.', fixed=TRUE)))
  colnames(plate_data_tidier) <- c('unique_id','beadset','secondary')
  plate_data_tidyr <- cbind(plate_data_tidy,plate_data_tidier)
  # Filter out unique_id that are NA (we have to do this because of how the edited unique IDs are constructed)
  plate_data_tidyr <- plate_data_tidyr %>% dplyr::filter(unique_id != 'NA')
  # Well IDs and Sample Meta data are combined with left join to ensure controls are included within the combined data even if they are not found in the annotated meta data
  pre_combined_data <- left_join(plate_data_tidyr,sample_meta_data,by = "unique_id")
  combined_data <- merge(pre_combined_data,exp_df,by = c("Plate","Well.ID")) }

if (num_well_in_platemap == 96 & num_384 !=0) 
  { # Split the unique_id_edit to get the unique_id, beadset, and secondary
  plate_data_tidier <- data.frame(do.call('rbind', strsplit(as.character(new_plate_ID_list$unique_id_edit), '.', fixed=TRUE)))
  colnames(plate_data_tidier) <- c('unique_id','beadset','secondary')
  plate_data_tidyr <- cbind(new_plate_ID_list,plate_data_tidier)
  # Filter out unique_id that are NA (we have to do this because of how the edited unique IDs are constructed)
  plate_data_tidyr <- plate_data_tidyr %>% dplyr::filter(unique_id != 'NA')
  # Well IDs and Sample Meta data are combined with left join to ensure controls are included within the combined data even if they are not found in the annotated meta data
  pre_combined_data <- left_join(plate_data_tidyr,sample_meta_data,by = "unique_id")
  combined_data <- merge(pre_combined_data,exp_df,by = c("Plate","Well.ID")) }

### Getting Data that passes the QC Checks
##########################################################################
# Getting the count and median columns separately
Count_col <- exp_df[,which(grepl("Count",colnames(exp_df)))]
Median_col <- exp_df[,which(grepl("Median",colnames(exp_df)))]
# Extracting the bead region # from the count and median columns
count_int <- gsub('.*Count.of.',"",colnames(Count_col))
median_int <- gsub('.*Median.BL2.H.of.',"",colnames(Median_col))
# Reordering the count columns so that it matches the ordering of the bead regions in the median columns
reorder_Count_col <- Count_col[,match(count_int,median_int)]
# Creating a logical matrix based on whether the bead counts are higher than the QC check provided by the user
mask <- ifelse(reorder_Count_col[,] > QC_check, TRUE, FALSE)
# Extracting the MFI values that have bead counts over the thresholds
edited_Median_col <- replace(Median_col, !mask, NA)
# Merging the MFI values that passed QC checks with the Plate # and Well IDs
QC_exp_data <- cbind(Plate = exp_df$Plate,Well.ID = exp_df$Well.ID,edited_Median_col)
# Merging the experimental data that passes QC checks with the annotated data
QC_masked_data <- merge(pre_combined_data,QC_exp_data,by = c("Plate","Well.ID"))

### Extracting particular measurements of interest
##########################################################################
if (length(measurements) != 0)
  {combined_data_abbr <- select(combined_data,"unique_id","Plate","Well.ID",measurements)
  write.csv(combined_data_abbr, file=paste0(save_path,'combined_data_abbr_',exp_name,'.csv'))}

### Calculating summary statistics for the experimental data that passes QC checks
##########################################################################
summary_stats <- QC_masked_data %>% group_by(unique_id,Plate) %>% summarise(across(c((anno_col + 5):(ncol(QC_masked_data) -2)),list(mean = mean, median = median, sd = sd)))
summary_stats_edited <- merge(plate_data_tidyr,summary_stats,by = c('unique_id','Plate'))
summary_stats_edited <- summary_stats_edited %>% dplyr::arrange(Plate) %>% dplyr::arrange(Well.ID)
summary_stats_edited <- summary_stats_edited %>% dplyr::select(-unique_id_edit)

### Performing dimensionality checks on the data
##########################################################################
check_dim_combined(plate_data_tidyr,pre_combined_data,combined_data,save_path,sample_meta_data,exp_df,anno_col,QC_exp_data,QC_masked_data,summary_stats,summary_stats_edited)

### Saving combined data, QC masked, and summary stats data files
##########################################################################
write.csv(combined_data, file=paste0(save_path,'combined_data_',exp_name,'.csv'))
write.csv(QC_masked_data, file=paste0(save_path,'QC_masked_combined_data_',exp_name,'.csv'))
write.csv(summary_stats_edited,file=paste0(save_path,'summary_stats',exp_name,'.csv'))

sink(type = "message")
close(logger)
readLines(get_log_file_name())

### Getting correlation plots 
##########################################################################
# Defining a useful helper function
eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

# Getting the correlation plots
if(corr_plot == TRUE) 
 {library(gridExtra)
  QC_exp_data_with_ids <- merge(plate_data_tidyr,QC_exp_data,by = c("Plate","Well.ID"))
  # Create a pdf of correlation plots
  p1<-c()
  i<-0
  # Initialize parameters
  platelist <- unique(QC_exp_data_with_ids$Plate)
  # for each feature
  for (plate_l in platelist){
    plotting_data <- QC_exp_data_with_ids[QC_exp_data_with_ids$Plate == plate_l,]
    idlist <- unique(plotting_data$unique_id)
    for (curfeat2 in idlist){
      curdata <- plotting_data[plotting_data$unique_id==curfeat2,]
      curdata2 <- pivot_longer(curdata,-c(1:6),names_to = 'measurement',values_to = 'value')
      curdata3 <- pivot_wider(curdata2,names_from = "Well.ID", values_from = "value")
      colnames(curdata3)[(ncol(curdata3)-1):ncol(curdata3)] <- c('duplicate_1','duplicate_2')
      if(all(is.na(c(curdata3$duplicate_1))) == FALSE & all(is.na(c(curdata3$duplicate_2))) == FALSE) 
      {i <- i+1
      p1[i] <- lapply(curfeat2, function(curfeat2) {  
        ggplot(data = curdata3, aes(x = duplicate_1, y = duplicate_2, colour = measurement)) + 
          geom_point(size=2) + geom_smooth(method = "lm", formula = duplicate_2 ~ duplicate_1, se=FALSE) + 
          geom_text(x = mean(range(curdata3$duplicate_1,na.rm = TRUE)), y = max(curdata3$duplicate_2,na.rm = TRUE), label = eq(curdata3$duplicate_1,curdata3$duplicate_2), parse = TRUE, colour = "black",size = 3 ) +
          theme(line = element_line(size=1),
                plot.title = element_text(hjust = 0.5)) +
          ggtitle(paste0("Unique_id: ",curfeat2, "     Plate: ",plate_l)) +
          theme(text = element_text(size= 10, hjust=0.5),
                title = element_text(size= 10, hjust=0.5),
                line = element_line(size=1),
                plot.title = element_text(hjust = 0.5)) +
          theme(legend.position="none",plot.title = element_text(hjust = 0.5)) } ) }  }
  }
  
  # Set filename of plot
  curfilname<-paste0(save_path,'correlation_plot.pdf')
  # Set row and column numbers and title
  ml <- marrangeGrob(p1, nrow=3, ncol=4, top="ptc")
  ggsave(curfilname, ml, width = 20, height = 10, dpi = 300, limitsize=FALSE)
  dev.off() }


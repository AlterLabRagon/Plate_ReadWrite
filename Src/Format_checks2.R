### File formatting requirements
### Annotated MetaData - file must exist, CSV, not empty, contain unique_id column, unique_id column must not be empty, there must be no duplicates, all Unique_IDs must be in platemap (but not all platemap IDs need to be in annotated data?)
### Experimental Data - file must exist, CSV, not empty, be in long format (contains a Well.ID column), must match num_384 well plates specified
### Platemap Data - plater function
### Data key Data - file must exist, CSV, not empty, contain plate, platemap, set, and secondary columns

### Helper Functions for all formatting checks
################################################################################
check_file_path <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(paste0("Sorry, can't find your file '", file, "'."), call. = FALSE) }
}

check_that_file_is_non_empty <- function(file) {
  if (length(read_lines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."), call. = FALSE)
  }
}

check_csv <- function(file) {
  if (!(grepl("[Cc][Ss][Vv]$", file))) {
    stop(paste0("Sorry, '", file, "' doesn't have a proper CSV file extension."), call. = FALSE) }
}

check_column_name <- function(file,filename,column_names) {
  data_key <- read.csv(file,header=TRUE)
  if(!any(column_names %in% tolower(colnames(data_key))))
  {stop(paste0(filename," must have columns named ",paste(column_names,collapse = ', '), ". Current columns include: ", paste(colnames(data_key),collapse = ', ')))} }

### Helper functions specifically for checking annotated_data
################################################################################
check_duplicated_unique <- function(file) {
  meta_data <- read.csv(file)
  unique_IDs <- meta_data[ , grepl( "unique_id" , tolower(names(meta_data)) ) ]
  if(anyDuplicated(unique_IDs)){
    warning(paste('The IDs in the unique_id column must be unique; repeated IDs include:', paste(unique_IDs[duplicated(unique_IDs)],collapse = ', ')))}
}

### Check annotated data
################################################################################
check_sample_annotation_data <- function(file) {
  check <- function(description, f) {
    message(paste0("* ", description, " ... ", collapse = ""), appendLF = FALSE)
    result <- tryCatch(
      expr = f(), 
      error = function(e) {
        # new line
        message("problem.")
        stop(e)
      })
    message("good!")
    result
  }
  
  check("Checking sample annotation data file path", function() check_file_path(file))
  check("Checking that sample annotation data is a csv file", function() check_csv(file))
  check("Checking that file is not empty", function() check_that_file_is_non_empty(file))
  
  check("Checking valid sample annotation data column labels", function() check_column_name(file,'Annotated Data','unique_id'))
  
  check("Checking whether there are duplicate IDs in the unique_id column",function() check_duplicated_unique(file))
  
  message("Success!")
}


### Helper functions specifically for checking experimental data
################################################################################
check_Well_ID <- function(file,num_384,num_96) {
  exp.indic <- "Well ID"
  lines <- readLines(file)
  exp.start <- grep(exp.indic, lines)
  lines2 <- gsub('(^"|"$|,)', "", lines)
  cur_columns <- read.csv(file, skip=(exp.start[1]-1), header=FALSE,nrows=1)
  if(length(exp.start) == 0)
    {cur_columns <- lines2[3] 
    stop(paste('Experimental data must be in long format with a column named "Well.ID"; current columns include:', paste(cur_columns,collapse = ', '))) }
  if(length(exp.start) != num_384 | num_96)
    {warning(paste0('Number of plates identified in the experimental data is not equal to the # of plates entered by the user.','\n','Number of plates identified in data: ',length(exp.start),'\n','Number of 384 well plates entered by user: ',num_384,'\n','Number of 96 well plates entered by user: ',num_96))}
  return (cur_columns)
  }

check_measurements <- function(column_names,measurements) {
  if(!length(measurements) == 0) {
    if(!all(as.logical(measurements %in% column_names)))
    {warning(paste('Selected measurements are not included in experimental data; selected measurements include: ', paste(measurements,collapse = ', ')))} 
    }
  }

### Check experimental data
################################################################################
check_exp_data <- function(file,measurements,num_384,num_96) {
  check <- function(description, f) {
    message(paste0("* ", description, " ... ", collapse = ""), appendLF = FALSE)
    result <- tryCatch(
      expr = f(), 
      error = function(e) {
        # new line
        message("problem.")
        stop(e)
      })
    message("good!")
    result
  }
  
  check("Checking experimental data file path", function() check_file_path(file))
  check("Checking that experimental data file is a csv", function() check_csv(file))
  check("Checking that experimental data file is not empty", function() check_that_file_is_non_empty(file))
  columns <- check("Checking that experimental data is in long format using the 'Well.ID' column and that the number of user entered 384 and 96 well plates matches the # of plates identified in the experimental data", function() check_Well_ID(file,num_384,num_96))
  check("Checking that measurements are included in the experimental data", function() check_measurements(columns,measurements))
  
  message("Success!")
}

### Check formatting of data key file
################################################################################
check_plate_annotation <- function(file) {
  check <- function(description, f) {
    message(paste0("* ", description, " ... ", collapse = ""), appendLF = FALSE)
    result <- tryCatch(
      expr = f(), 
      error = function(e) {
        # new line
        message("problem.")
        stop(e)
      })
    message("good!")
    result
  }
  
  check("Checking plate annotation file path", function() check_file_path(file))
  check("Checking that plate annotation file is a csv", function() check_file_path(file))
  check("Checking that plate annotation file is not empty", function() check_that_file_is_non_empty(file))
  columns <- check("Checking plate annotation has valid column labels", function() check_column_name(file,'Data key',c("plate","platemap","set","secondary")))
  
  message("Success!")
}

### Helper Functions specifically for checking output files
################################################################################
check_anno_dimensions <-  function(plate_data_tidyr,pre_combined,anno_col) {
  num_annotated_final <- dim(pre_combined_data)[2] - dim(plate_data_tidyr)[2]
  if (anno_col != num_annotated_final)
  {warning(
      cat(paste0('The number of annotated columns changed while merging the plate data with the annotated data.','\n',
                 'Number of original annotated data columns: ',anno_col,'\n','Number of annotated data columns in the merged data: ',num_annotated_final))) }
}

check_exp_dimensions <-  function(exp_df,pre_combined,combined) {
  num_exp_final <- dim(combined)[2] - dim(pre_combined_data)[2] + 2
  num_exp_actual <- dim(exp_df)[2]
  if (num_exp_actual != num_exp_final)
  {warning(
    cat(paste0('The number of experimental data columns changed while merging the experimental data with the pre combined data.','\n',
               'Number of original experimental data columns: ',num_exp_actual,'\n','Number of experimental data columns in the merged data: ',num_exp_final))) }
}

check_merge_columns <- function(plate_data_tidyr,meta_data,pre_combined,exp_df)
  {first_merge <- all( intersect(colnames(plate_data_tidyr),colnames(meta_data)) %in% "unique_id") == T
  second_merge <- all( intersect(colnames(pre_combined_data),colnames(exp_df)) %in% c("Plate","Well.ID")) == T
  if (first_merge == FALSE)
      {warning('There was an issue with merging the tidy plate data and metadata together')}
  if (second_merge == FALSE)
      {warning('There was an issue with merging the pre-combined and experimental data together')}
  }

check_dropped_rows <- function(pre_combined,combined,save_path)
  {if(nrow(pre_combined) != nrow(combined))
    {if(nrow(pre_combined)  > nrow(combined))
    {warning(paste('IDs dropped or not found in experimental data!', '\n','Number of dropped IDs: ',length(pre_combined_data$unique_id_edit[ !pre_combined_data$unique_id_edit%in%combined$unique_id_edit]),'\n','Check the well and plate numbers of dropped IDs in the dropped ID csv file'))
     dropped_ids <- as.data.frame(pre_combined_data$unique_id_edit[ !pre_combined_data$unique_id_edit%in%combined$unique_id_edit, 1:3],colnames = c('Plate','Well.ID','unique_id_edit'))
      write.csv(dropped_ids,file=paste0(save_path,'dropped_IDs','.csv'))}
     }
  }
                   
                   
check_annotated_merge_rows <- function(plate_data_tidyr,pre_combined)
  {if(nrow(plate_data_tidyr) != nrow(pre_combined_data))
    {warning(paste('The number of rows in the merged pre combined data is not equal to the # of plates in the tidy plate data','\n','Number of rows in the tidy plate data',nrow(plate_data_tidyr),'\n','Number of rows in the pre-combined rows',nrow(pre_combined_data)))
    }
  }

### Helper functions for checking QC and Summary Statistics merging
################################################################################
check_merge_columns_QC <- function(pre_combined,QC_exp_data)
  # d1 is pre_combined and d2 is QC_exp_data 
  {first_merge <- all( intersect(colnames(pre_combined),colnames(QC_exp_data)) %in% c('Plate','Well.ID')) == T
  if (first_merge == FALSE)
    {warning('There was an issue with merging the pre-combined and QC passed experimental data together')} }

check_merge_columns_ss <- function(plate_data_tidyr,summary_stats)
  # d1 is plate_data_tidyr and d2 is summary statistics data
  {first_merge <- all( intersect(colnames(plate_data_tidyr),colnames(summary_stats)) %in% c('unique_id','Plate')) == T
  if (first_merge == FALSE)
    {warning('There was an issue with merging the plate data and summary statistics together')} }

check_dropped_rows_qc <- function(pre_combined,QC_masked_data,dropped_ids,save_path)
  # d1 is pre_combined and d2 is QC_masked_data
  {if(nrow(pre_combined) != nrow(QC_masked_data))
    {if(nrow(pre_combined)  > nrow(QC_masked_data))
      {warning(paste('Samples dropped or not found in QC experimental data!', '\n','Number of dropped IDs: ',(nrow(pre_combined) - nrow(QC_masked_data)),'\n','You can check the well and plate numbers of dropped IDs in the dropped ID QC csv file if they are different from the other dropped IDs'))
      dropped_ids_QC <- as.data.frame(pre_combined[ !pre_combined$unique_id_edit %in% QC_masked_data$unique_id_edit, 1:3],colnames = c('Plate','Well.ID','unique_id_edit'))
      if(nrow(dropped_ids_QC) != nrow(dropped_ids))
        {write.csv(dropped_ids_QC,file=paste0(save_path,'dropped_IDs_QC','.csv'))}
            }
          }
        }

check_dropped_rows_ss <- function(pre_combined,summary_stats_edited,dropped_ids,save_path)
  # d1 is pre_combined and d2 is QC_masked_data
{if(nrow(pre_combined) != nrow(summary_stats_edited))
  {if(nrow(pre_combined)  > nrow(summary_stats_edited))
    {warning(paste('Samples dropped or not found in summary stats data!', '\n','Number of dropped IDs: ',(nrow(pre_combined) - nrow(summary_stats_edited)),'\n','You can check the well and plate numbers of dropped IDs in the dropped ID summary stats csv file if they are different from the other dropped IDs'))
    dropped_ids_ss <- as.data.frame(pre_combined[ !pre_combined$unique_id_edit %in% summary_stats_edited$unique_id_edit, 1:3],colnames = c('Plate','Well.ID','unique_id_edit'))
    if(nrow(dropped_ids_ss) != nrow(dropped_ids))
      {write.csv(dropped_ids_ss,file=paste0(save_path,'dropped_IDs_ss','.csv'))}
    }
  }
}

### Checking formatting of output merged files
################################################################################
check_dim_combined <- function(plate_data_tidyr,pre_combined,combined,save_path,meta_data,exp_df,anno_col,QC_exp_data,QC_masked_data,summary_stats,summary_stats_edited) {
  check <- function(description, f) {
    message(paste0("* ", description, " ... ", collapse = ""), appendLF = FALSE)
    result <- tryCatch(
      expr = f(), 
      error = function(e) {
        # new line
        message("problem.")
        stop(e)
      })
    message("good!")
    result
  }
  
  check("Checking whether the correct columns were merged to create the combined data", function() check_merge_columns(plate_data_tidyr,meta_data,pre_combined,exp_df))
  check("Checking whether the dimensions of the annotated data changed", function() check_anno_dimensions(plate_data_tidyr,pre_combined,anno_col))
  check("Checking whether the dimensions of the experimental data changed", function() check_exp_dimensions(exp_df,pre_combined,combined))
  dropped_ids <- check("Checking whether any rows of experimental data were dropped", function() check_dropped_rows(pre_combined,combined,save_path))
  check("Checking whether any rows of data were added while merging annotated data", function() check_annotated_merge_rows(plate_data_tidyr,pre_combined))
  check("Checking whether the correct columns were merged to create the QC masked data", function() check_merge_columns_QC(pre_combined,QC_exp_data))
  check("Checking whether any rows of QC experimental data were dropped", function() check_dropped_rows_qc(pre_combined,QC_masked_data,dropped_ids,save_path))
  check("Checking whether the correct columns were merged to create the summary statistics data", function() check_merge_columns_ss(plate_data_tidyr,summary_stats))
  check("Checking whether any rows of summary statistics data were dropped", function() check_dropped_rows_ss(pre_combined,summary_stats_edited,dropped_ids,save_path))
  
  message("Success!")
}



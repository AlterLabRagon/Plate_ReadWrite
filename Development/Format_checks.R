### File formatting requirements
### Annotated MetaData - file must exist, CSV, not empty, contain Unique_ID column, Unique_ID column must not be empty, there must be no duplicates, all Unique_IDs must be in platemap (but not all platemap IDs need to be in annotated data?)
### Experimental Data - file must exist, CSV, not empty, be in long format (contains a Well.ID column), must match num_384 well plates specified, 
### PlateMap Data - plateR

### Checking Annotated MetaData
check_file_path <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(paste0("Sorry, can't find your file '", file, "'."), call. = FALSE)
  }
  
  if (!(grepl("[Cc][Ss][Vv]$", file))) {
    stop(paste0("Sorry, '", file, "' doesn't have a proper CSV file extension."),
         call. = FALSE)
  }
}


check_Unique_ID_column_name <- function(file) {
  meta_data <- read.csv(file)
  if(!any(c('unique_id','Unique_id','Unique_ID','unique_ID','unique_Id','Unique_Id') %in% colnames(meta_data)))
  {stop(paste('Annotated_file_path must contain a column named "Unique_ID","unique_id","Unique_id",or"unique_ID" ; current columns include:', paste(colnames(meta_data),collapse = ', ')))}
  
  unique_IDs <- meta_data[ , grepl( "Unique_ID|unique_ID|unique_id|Unique_id" , names(meta_data) ) ]
  if (all(is.null(unique_IDs)) || all(unique_IDs == "")) {
    stop("Sorry, the Unique_ID column must not be NULL or an empty string.",
         call. = FALSE)
  }
}

# to.lower
check_duplicated_unique <- function(file) {
  meta_data <- read.csv(file)
  unique_IDs <- meta_data[ , grepl( "Unique_ID|unique_ID|unique_id|Unique_id" , names(meta_data) ) ]
  if(anyDuplicated(unique_IDs)){
    warning(paste('The IDs in Unique_ID must be unique; repeated IDs include:', paste(unique_IDs[duplicated(unique_IDs)],collapse = ', ')))}
}

check_that_file_is_non_empty <- function(file) {
  if (length(read_lines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."), call. = FALSE)
  }
}

check_annotated_data <- function(file) {
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
  
  check("Checking annotated meta data file path", function() check_file_path(file))
  
  check("Checking that file is not empty", function() check_that_file_is_non_empty(file))
  
  check("Checking whether there are duplicate IDs in the unique_ID column",function() check_duplicated_unique(file))
  check("Checking valid annotated meta data column labels", 
        function() check_Unique_ID_column_name(file))

  message("Success!")
}


### Checking Experimental Data
check_well_ID_column_name <- function(file) {
  exp_data <- read.csv(file, skip=2, header=TRUE,nrows=384)
  #' if there are 3 lines before the start of the IQue data, skip parameter needs to change
  if(!any(c('Well.ID','Well ID') %in% colnames(exp_data)))
  {stop(paste('Experimental data must be in long format with a column named "Well.ID"; current columns include:', paste(colnames(exp_data),collapse = ', ')))} }

# move or remove (do at once)
check_num_384_plates <- function(file) {
  exp_data <- readLines(file,n=-1)
  num_384_plates <- (length(exp_data) - 1) / 386 
  print(paste('Number of 384 well plates identified from experimental data file is',num_384_plates)) 
  return(num_384_plates)}

# move or remove (do at once)
check_plate_names <- function(file,num_384_plates) {
  plates = c()
  for (i in c(1:num_384_plates))
  {idx = 1 + 386*(i-1)
  plate_name <- read.csv(file, skip=idx, header=FALSE,nrows=1)
  plates <- append(plates,plate_name)}
  print(paste('Plate names are',plates,collapse = ', '))
  return(plates)}

check_exp_data <- function(file) {
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
  
  check("Checking that experimental data file is not empty", 
        function() check_that_file_is_non_empty(file))
  
  check("Checking experimental data valid column labels", 
        function() check_well_ID_column_name(file))
  
  num_384 <- check_num_384_plates(file)
  plates <- check_plate_names(file,num_384)
  
  message("Success!")
  return(plates)
}



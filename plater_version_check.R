library(pryr)

check_meta_format <- function(file) {
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
  
  check("Checking file path", function() check_file_path(file))
  
  check("Checking that file is not empty", 
        function() check_that_file_is_non_empty(file))
  
  true <- check("Checking valid column labels", 
        function() check_Unique_ID_column_name(file))
  
  message("Success!")
}


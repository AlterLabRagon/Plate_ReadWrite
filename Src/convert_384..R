convert_to <- function(plate_data,plate_num,plate_name,m,n,duplicate_dir,SAVE_PATH)
{#' This function accounts for duplicates when moving from a 96 to 384 well palte
  #'@plate_data <- data frame containing the platemap read into plater format and modified into a long format
  #'@m and @n <- numeric that are indices of the 96 well plates that are combined together in 1 384 well plate (e.g. 2 and 3)
  #'@plate_num <- numeric that is the 384 well plate number
  #'@plate_name <- a string that identifies the 384 well plate names
  #'@duplicate_dir <- boolean that describes if replicates were conducted horizontally or not (TRUE or FALSE)
  plate1 <- plate_data %>% filter(Plate == m)
  plate2 <-  plate_data %>% filter(Plate == n)
  
  plate1_w_d <- c(rbind(as.character(as.matrix(plate1[,3])),as.character(as.matrix((plate1[,3])))))
  print(paste('Plate 1',length(plate1_w_d)))
  plate2_w_d <- c(rbind(as.character(as.matrix(plate2[,3])),as.character(as.matrix((plate2[,3])))))
  print(paste('Plate 2',length(plate2_w_d)))
  
  big_well_plate <- matrix(c(plate1_w_d,plate2_w_d), byrow=duplicate_dir, nrow = 16)
  #' This generates a matrix of the plate IDs of a 386 well plate (A01 - P24)
  rownames(big_well_plate) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
  colnames(big_well_plate) <- c('01','02','03','04','05','06','07','08','09',10:24)
  
  #' Generates intermediate platemap file
  write.csv(big_well_plate, file=paste0(SAVE_PATH,'rev_plate_map_',plate_name,'.csv'))
  big_well_plate_ID <- outer(rownames(big_well_plate), colnames(big_well_plate), paste, sep="")
  
  #' This generates a 386x1 data frame of the plate IDs
  plate_well_ID <- as.vector(t(big_well_plate_ID))
  plate_well_ID <- data.frame(plate_well_ID)
  #' This generates a 386x1 dataframe with the 2 plates and duplicates 
  plate_ID_list <- as.vector(t(big_well_plate))
  plate_ID_list <- cbind(plate_well_ID,plate_ID_list)
  Plate <- rep(plate_num,384)
  plate_ID_list <- cbind(Plate,plate_ID_list)
  colnames(plate_ID_list) <- c("Plate","Well.ID","unique_id")
  return(plate_ID_list)}

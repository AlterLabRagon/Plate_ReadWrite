

library(tibble)

biana_plate_data <- read_plate(file = biana_plate_path,sep = ',')
colnames(biana_plate_data) <- c('Wells',(1:ncol(biana_plate_data)))

biana_plate_data_tidy <- biana_plate_data %>% pivot_longer(names_to = "Plate",values_to = "Unique_ID",cols = -Wells)

exp_list <- list()
plate_list <- list()

for (i in c(1:num_384))
{idx = 2 + 386*(i-1) 
exp_list[[i]] <- read.csv(biana_IQue_path, skip=idx, header=TRUE,nrows=384)
plate_list[[i]] <- rep(i,384)
}

exp_df <- do.call(rbind,exp_list)
Plate <- unlist(plate_list)
exp_df <- exp_df %>% add_column(Plate,.after = 1)

new_plate_ID_list <- convert_to(biana_plate_data_tidy,1,'random',1,2,TRUE,'/Users/maanasa/Downloads/results-plate/')

data_merge1 <- merge(new_plate_ID_list,exp_df,by = c("Plate","Well.ID"))


my_csv[[i]] <- read.csv()

#' assigning the experimental data to the names
for (i in c(1:num_384))
  #' if there are 3 lines before the start of the IQue data, 2 + 386(i-1)
{exp_df['Plate'] <- rep(i,384)
  idx = 2 + 386*(i-1) 
assign(exp_df_names[i],read.csv(biana_IQue_path, skip=idx, header=TRUE,nrows=384))
assign(exp_df_abbr_names[i],select(get(exp_df_names[i]),"Well.ID", measurements)) }


subsetting
ifelse(mat[, 'two'] == 7 & mat[, 'three'] == 12, "both", "not both")
# Generates boolean matrix
rand4 <- ifelse(Count_col[,] < 100, TRUE, FALSE, drop=FALSE)

rand41 <- ifelse(Count_col[,] < 100, TRUE, NA)

rand5 <- as.matrix(Median_col[rand41],drop=FALSE)
  
rand5 <- lapply(apply(rand4, 1, which), function(i) Median_col[,i,drop=FALSE])

rand51 <- Median_col[Median_col[rand4,drop=FALSE]]

Median_col <- data_merge1[,which(grepl("Median",colnames(data_merge1)))]

vm <- as.vector(Median_col)
vi <- as.vector(rand41)
new_v <- ifelse(vi, vm, NA)
new_mat <- matrix(new_v, nrow = nrow(Median_col), ncol=ncol(Median_col))


apply(Median_col, c(1, 2), sum)

zeta <- function(x)
{for (i in nrow(rand41))
  {for (n in ncol(rand41))
  {x <- ifelse(rand41[i,n] == TRUE,x,rand41[i,n])
    print(x)}
} }

#### 
Median_col <- data_merge1[,which(grepl("Median",colnames(data_merge1)))]

rand4 <- ifelse(Count_col[,] < 100, TRUE, FALSE)
edited_Median_col <- replace(Median_col, !rand4, NA)

# Step 1: Check the structure of your data
str(vert)

# Step 2: Convert list columns to character or another appropriate type
vert <- lapply(vert, function(x) {
  if (is.list(x)) {
    return(as.character(x))  # Convert list to character
  }
  return(x)
})

# Convert the list of columns back to a data frame
vert <- as.data.frame(vert)

# Step 3: Replace infinite values with NA
vert[sapply(vert, is.infinite)] <- NA

# Step 4: Run missForest imputation
library(missForest)
noi <- missForest::missForest(vert, verbose = TRUE)


# Convert list columns to character or factor
vert <- lapply(vert, function(x) {
  if (is.list(x)) {
    return(as.character(x))  # Convert lists to character
  }
  return(x)
})

# Convert the list of columns back to a data frame
vert <- as.data.frame(vert)

# Replace infinite values with NA in numeric columns
numeric_columns <- names(vert)[sapply(vert, is.numeric)]
vert[numeric_columns] <- lapply(vert[numeric_columns], function(x) {
  x[is.infinite(x)] <- NA
  return(x)
})

# Convert character columns to factors
character_columns <- names(vert)[sapply(vert, is.character)]
vert[character_columns] <- lapply(vert[character_columns], as.factor)

# Run missForest imputation
library(missForest)
noi <- missForest::missForest(vert, verbose = TRUE)


library(readxl)
X20240826_IMP <- read_excel("C:/Users/med-an7/Work Folders/Desktop/Fredrik/MassSpec/Test runs/Run 3 DIA-NN/20240826_IMP.xlsx", 
                            col_types = c("text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(X20240826_IMP)


str(sdd)
# Convert all columns in sdd to numeric, if possible
sdd <- as.data.frame(lapply(sdd, function(x) as.numeric(as.character(x))))

# Alternatively, remove non-numeric columns
sdd <- sdd[sapply(sdd, is.numeric)]
library(pheatmap)

# Assuming `heatmap_colors` and `annotation` are already defined
pheatmap(sdd, scale = "row", 
         cluster_rows = TRUE,
         clustering_method = "ward.D2",
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean",
         show_rownames = FALSE,
         show_colnames = TRUE,
         annotation = annotation,
         annotation_names_col = FALSE,
         treeheight_row = 0, 
         angle_col = 45,
         fontsize = 5,
         color = heatmap_colors)

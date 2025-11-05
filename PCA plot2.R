#Libraries used
library("readxl")
library("tidyverse")
library("openxlsx")
library("missForest")
library("writexl")
library("pheatmap")
library("dplyr")
library("ggplot2")

#Import dataset into Environment. This can be a file prepared in Excel. (I will try later, to get also a file where the missing values are not yet removed)
read_excel("C:/Users/med-an7/Work Folders/Desktop/Fredrik/MassSpec/MS runs/Runs 3 and 4 DIA-NN combined/Combined search/raw_data/valid_raw.xlsx",
           sheet = "Sheet1",
           col_names = TRUE)
#Rename the data set. This term "res" can be replaced with anything that comes to mind, there is no function here.
res <- read_excel("C:/Users/med-an7/Work Folders/Desktop/Fredrik/MassSpec/MS runs/Runs 3 and 4 DIA-NN combined/Combined search/raw_data/valid_raw.xlsx",
                  sheet = "Sheet1",
                  col_names = TRUE)
view(res) 
#Set gene ID (symbol) as row names. This changes that column 1 disappears and the gene or protein names are listed.
rer <- res%>%select(-Genes)
rownames(rer) <- res$Genes
view(rer)

#transpose the data
vert <- t(rer)
view(vert)
head(vert)
#check structure of data
str(vert)

#Check for Any Remaining Non-Numeric Values
non_numeric_columns <- sapply(vert, function(x) !is.numeric(x))
print(non_numeric_columns)

# Check for infinite values
any(is.infinite(vert))
vert[is.na(vert)] <- colMeans(vert, na.rm = TRUE)

# Replace NA values in each column with the respective column mean
for(i in 1:ncol(vert)) {
  vert[is.na(vert[, i]), i] <- mean(vert[, i], na.rm = TRUE)
}

## Perform PCA (conditions as rows, proteins as features)
pca_result <- prcomp(vert, scale. = TRUE)

## Extract PCA scores for plotting (for the first two principal components)
pca_scores <- data.frame(pca_result$x)

#Combine PCA scores with original group data for plotting
pca_scores_with_group <- cbind(vert, pca_scores)

# Create a vector for group assignments
groups <- data.frame(Condition = rownames(vert),  # Assuming rownames in 'vert' are the conditions
                     Group = c(rep("PACS", 46), rep("Control", 13)))

# Combine the group information with PCA scores
pca_scores_with_group <- cbind(groups, pca_scores)

# Ensure that 'Group' is treated as a factor
pca_scores_with_group$Group <- as.factor(pca_scores_with_group$Group)

# Create a PCA plot with different colors for PACS and Control
ggplot(pca_scores_with_group, aes(x = PC1, y = PC2, color = Group)) +
  geom_point(size = 4) +
  labs(title = "PCA Plot: PACS vs Control", x = "PC 1", y = "PC2") +
  theme_minimal() +
  scale_color_manual(values = c("PACS" = "cyan", "Control" = "magenta"))  









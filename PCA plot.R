library(ggplot2)
library(dplyr)
test_pca
glimpse(test_pca)

numeric_data <- test_pca %>%
  select(Feature1:Feature6)
pca_result <- prcomp(numeric_data, scale.=T)
summary(pca_result)
pca_scores <- data.frame(pca_result$x)
pca_scores_with_group <- cbind(test_pca, pca_scores)
p2 <- ggplot(pca_scores_with_group, aes(x=PC1, y=PC2))+
  geom_point(size=4)+
  labs(title="PCA Plot", x = "PC1", y= "PC2")+
  theme_minimal()
plot(p2)

library(readxl)
data <- read_xlsx("C:/Users/med-an7/Work Folders/Desktop/Fredrik/MassSpec/MS runs/Runs 3 and 4 DIA-NN combined/Combined search/raw_data/valid_raw.xlsx")
head(data)

##

#important here is to set the first column into characters, so that they go on the side. See for that script for heatmap. Otherwise it will later count into a column for calculations

##


data_transposed <- t(data)
head(data_transposed)
pca_result <- prcomp(data_transposed, scale. =T)

# Check the structure of the data
str(data_transposed)

# Identify non-numeric columns
non_numeric_columns <- sapply(data_transposed, function(x) !is.numeric(x))
print(non_numeric_columns)
data_numeric <- data_transposed[, sapply(data_transposed, is.numeric)]
data_transposed[] <- lapply(data_transposed, function(x) {
  if (is.factor(x) || is.character(x)) {
    as.numeric(as.factor(x))
  } else {
    x
  }
})
pca_result <- prcomp(data_transposed, scale. =T)
class(data_transposed)
data_transposed <- as.data.frame(data_transposed)

class(data_transposed)
# Check for non-numeric columns again
non_numeric_columns <- sapply(data_transposed, function(x) !is.numeric(x))
print(non_numeric_columns)

data_transposed <- as.data.frame(lapply(data_transposed, function(x) {
  as.numeric(as.character(x))
}))
any(is.na(data_transposed))
data_transposed <- na.omit(data_transposed)
data_transposed[is.na(data_transposed)] <- colMeans(data_transposed, na.rm = TRUE)

pca_result <- prcomp(data_transposed, scale. = TRUE)

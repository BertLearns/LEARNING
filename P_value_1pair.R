setwd("C:/Users/ebert/OneDrive/Desktop/LEARNING")
data1 <- read.csv("ADORA2.csv")
pos1 <- data1[,2:3]
data2 <- read.csv("ARG1.csv")
pos2 <- data2[,2:3]
results_df <- data.frame(Gene1 = character(), Gene2 = character(), Correlation = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

#BIN data1 and data2
library(ggplot2) 
ggplot(data = pos1) + geom_bin_2d(aes(x=X,y=Y), bins = 140)
ggplot(data = pos2) + geom_bin_2d(aes(x=X,y=Y), bins= 140)

gene1_binned <- ggplot(data = pos1) + stat_bin_2d(aes(x=X,y=Y), bins = 140)
gene2_binned <- ggplot(data = pos2) + stat_bin_2d(aes(x=X,y=Y), bins = 140)


# Extract binned data from gene1_binned and gene2_binned
gene1_data <- ggplot_build(gene1_binned)$data[[1]]
gene2_data <- ggplot_build(gene2_binned)$data[[1]]

# Extract the X and Y coordinates
gene1_x <- gene1_data$x
gene1_y <- gene1_data$y
gene2_x <- gene2_data$x
gene2_y <- gene2_data$y

# Calculate the 2D pairwise correlation
correlation <- cor(gene2_x,gene2_y)

# Print the correlation matrix
print(correlation)

correlation <- cor(gene1_binned, gene2_binned, use = "pairwise.complete.obs", method = c("pearson"))
cor()

 <- bin_counts(gene1_expression, bin_size)
gene2_binned <- bin_counts(gene2_expression, bin_size)
stat_bin_2d(
  mapping = NULL,
  data = pos1,
  geom = "tile",
  position = "identity",
  bins = 30,
  binwidth = NULL,
  drop = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
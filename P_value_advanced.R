# Example data
setwd("C:/Users/ebert/OneDrive/Desktop/LEARNING")
data <- read.csv("transcripts_test_for_r.csv") #export from Verascape with Gene, X, Y, Z, CellID
library(ggplot2)
bin_size <- 25  # 25um 

# Import gene names from CSV file
gene_names <- read.csv("gene_names.csv")$GeneNames #all unique gene names decoded

# Create an empty data frame to store the gene-pairs, correlation coefficient, p-values results
results_df <- data.frame(Gene1 = character(), Gene2 = character(), Correlation = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Calculate correlation and p-value for every combination of 2 genes
combinations <- combn(gene_names, 2, simplify = FALSE) #every combination of gene pairs to find r, p-value

#for loop to iterate through all gene-pairs, resulting in r, p-value
#for (i in 1:length(combinations)) {
  #gene_pair <- combinations[[1]]
  gene1_name <- ADORA2
  gene2_name <- ARG1
  
  WorkingData1 <- read.csv("transcripts_test_for_r.csv", nrows=gene1_name)
  print(WorkingData1)
  
  print(gene1_name)
  print('hello')
  # Print the current gene pair being processed - fails to print
  cat("Processing gene pair:", gene1_name, "-", gene2_name)
  
  # Check if the gene names are correct - fails to print
  cat("Gene1:", gene1_name, "\n")
  cat("Gene2:", gene2_name, "\n")
  
  working_data <- read.csv.sql("transcripts_test_for_r.csv",
                     sql = "select * from file where `Gene` = gene1_name", eol = "\n")
  
  
  # Binning and correlation calculation for the specified gene pair
  gene1_expression <- data[, gene1_name]
  gene2_expression <- data[, gene2_name]
  print(head(gene1_name))

  
  # Check if the gene expression data is correct
  print(head(gene1_expression))
  print(head(gene2_expression))
  
  gene1_binned <- bin_counts(gene1_expression, bin_size)
  gene2_binned <- bin_counts(gene2_expression, bin_size)
  
  print(head(gene1_binned))
  print(head(gene2_binned))
  
  correlation <- cor(gene1_binned, gene2_binned)
  
  # Calculate two-tailed p-value
  sample_size <- length(gene1_binned)
  degrees_of_freedom <- sample_size - 2
  t_stat <- correlation * sqrt((sample_size - 2)/(1 - correlation^2))
  p_value <- 2 * (1 - pt(abs(t_stat), df = degrees_of_freedom))
  
  # Print the correlation coefficient and p-value for the gene pair
  cat("Correlation coefficient:", correlation, "\n")
  cat("Two-tailed p-value:", p_value, "\n")
  cat("\n")
  
  # Store the results in the data frame
  result_row <- data.frame(
    Gene1 = gene1_name,
    Gene2 = gene2_name,
    Correlation = correlation,
    P_Value = p_value,
    stringsAsFactors = FALSE
  )
  results_df <- rbind(results_df, result_row)
}

# Print the results
print(results_df)

# Export the results to a CSV file
write.csv(results_df, "p_values.csv", row.names = FALSE)

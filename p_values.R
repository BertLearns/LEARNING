# Example data
data <- read.csv("transcripts_test_for_r.csv")  # Replace "your_data_file.csv" with your actual data file
bin_size <- 25  # Set the desired bin size

# Import gene names from CSV file
gene_names <- read.csv("gene_names.csv")$GeneNames  # Replace "gene_names.csv" with your gene names CSV file

# Create an empty list to store the results
results <- list()

# Calculate correlation and p-value for every combination of 2 genes
combinations <- combn(gene_names, 2, simplify = FALSE)
for (i in 1:length(combinations)) {
  gene_pair <- combinations[[i]]
  gene1_name <- gene_pair[1]
  gene2_name <- gene_pair[2]
  
  # Binning and correlation calculation for the specified gene pair
  gene1_expression <- data[, gene1_name]
  gene2_expression <- data[, gene2_name]
  
  gene1_binned <- bin_counts(gene1_expression, bin_size)
  gene2_binned <- bin_counts(gene2_expression, bin_size)
  
  correlation <- cor(gene1_binned, gene2_binned)
  
  # Calculate two-tailed p-value
  sample_size <- length(gene1_binned)
  degrees_of_freedom <- sample_size - 2
  t_stat <- correlation * sqrt((sample_size - 2)/(1 - correlation^2))
  p_value <- 2 * (1 - pt(abs(t_stat), df = degrees_of_freedom))
  
  # Print the correlation coefficient and p-value for the gene pair
  cat("Pair:", gene1_name, "-", gene2_name, "\n")
  cat("Correlation coefficient:", correlation, "\n")
  cat("Two-tailed p-value:", p_value, "\n")
  cat("\n")
  
  # Store the results in the list
  result <- data.frame(
    Gene1 = gene1_name,
    Gene2 = gene2_name,
    Correlation = correlation,
    P_Value = p_value,
    stringsAsFactors = FALSE
  )
  results[[i]] <- result
}

# Combine the results into a single data frame
results_df <- do.call(rbind, results)

# Print the results
print(results_df)

# Export the results to a CSV file
write.csv(results_df, "p_values.csv", row.names = FALSE)

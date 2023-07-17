# Define the directory path
directory <- "/Users/ebert/OneDrive/Desktop/LEARNING"

# Check if the directory exists
if (dir.exists(directory)) {
  # Check the permissions of the directory
  permissions <- file.access(directory, 2)  # 2 represents write permission
  
  if (permissions == 0) {
    cat("You have write permission to the directory.")
  } else {
    cat("You do not have write permission to the directory.")
  }
} else {
  cat("The directory does not exist.")
}
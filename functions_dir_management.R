# functions_dir_management.R

# Function to get all folders within a base directory
get_all_folders <- function(base_dir) {
  # List all directories and subdirectories
  folders <- list.dirs(base_dir, full.names = TRUE, recursive = TRUE)
  
  # Removing the base directory from the list as list.dirs includes it by default
  folders <- folders[folders != base_dir]
  
  return(folders)
}

# Function to get all files and folders within a base directory
get_all_files_and_folders <- function(base_dir) {
  # List all directories including subdirectories
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = TRUE)
  
  # List all files including those in subdirectories
  files <- list.files(base_dir, full.names = TRUE, recursive = TRUE)
  
  # Combine the list of directories and files
  all_contents <- c(dirs, files)
  
  # Optionally, remove the base directory from the list if not needed
  all_contents <- all_contents[all_contents != base_dir]
  
  return(all_contents)
}

# Function to get all files in a specific folder within a base directory
get_all_files_in_specific_folder <- function(base_dir, folder_name) {
  # List all directories including subdirectories
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = TRUE)
  
  # Filter directories to include only those with the specified name
  target_dirs <- grep(folder_name, dirs, value = TRUE)
  
  # Initialize a vector to hold all files in the target directories
  all_files <- c()
  
  # Loop through each target directory and list all files
  for (dir in target_dirs) {
    files_in_dir <- list.files(dir, full.names = TRUE, recursive = TRUE)
    all_files <- c(all_files, files_in_dir)
  }
  return(all_files)
}

# Function to group files by their containing folder
get_files_grouped_by_folder <- function(base_dir, folder_name) {
  # List all directories including subdirectories
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = TRUE)
  
  # Filter directories to include only those with the specified name
  target_dirs <- grep(folder_name, dirs, value = TRUE)
  
  # Initialize a list to hold all file names grouped by folder
  files_grouped <- list()
  
  # Loop through each target directory and list all file names
  for (dir in target_dirs) {
    full_paths <- list.files(dir, full.names = TRUE, recursive = TRUE)
    file_names <- basename(full_paths)  # Extract file names
    files_grouped[[dir]] <- file_names
  }
  return(files_grouped)
}

# Function to check if the directory exists and create one if it does not exist
create_dir <- function(path_to_create){
  if (!dir.exists(path_to_create)) {
    dir.create(path_to_create, recursive = TRUE)
    paste("This path is created:", path_to_create)
  }
}

# Define functions to convert file sizes to adaptive units (B, KB, MB, GB)
format_size <- function(size_in_bytes) {
  if (size_in_bytes >= 1024^3) {
    return(paste0(round(size_in_bytes / 1024^3, 2), " GB"))
  } else if (size_in_bytes >= 1024^2) {
    return(paste0(round(size_in_bytes / 1024^2, 2), " MB"))
  } else if (size_in_bytes >= 1024) {
    return(paste0(round(size_in_bytes / 1024, 2), " KB"))
  } else {
    return(paste0(size_in_bytes, " B"))
  }
}

# Define function to get file type icon for various file types
get_file_type_icon <- function(file_name, is_directory) {
  if (is_directory) {
    return("[DIR]")
  } else {
    ext <- tolower(tools::file_ext(file_name))  # 获取文件扩展名并转换为小写
    return(switch(ext,
                  "txt" = "[TXT]",
                  "jpg" = "[IMG]",
                  "png" = "[IMG]",
                  "pdf" = "[PDF]",
                  "R" = "[R Script]",
                  "csv" = "[CSV]",
                  "rdata" = "[RData]",
                  "rproj" = "[RProj]",
                  "gz" = "[GZ Archive]",
                  "doc" = "[Word]",
                  "docx" = "[Word]",
                  "xlsx" = "[Excel]",
                  "zip" = "[ZIP Archive]",
                  "tar" = "[TAR Archive]",
                  "ppt" = "[PowerPoint]",
                  "pptx" = "[PowerPoint]",
                  "[FILE]"))  # 如果扩展名不匹配，返回默认 [FILE]
  }
}

#' Print Directory Tree Structure
#'
#' This function recursively prints the directory structure of the specified path.
#' It displays the file names, file sizes, last modified date, and file type icons.
#'
#' @param dir_path A string. The directory path to be listed. Defaults to the current directory.
#' @param prefix A string. Internal parameter used for recursion (not to be set manually by users).
#' @param show An integer. Controls how the directory structure is printed.
#'   \itemize{
#'     \item \code{1}: Displays a simple directory structure.
#'     \item \code{2}: Displays directory structure with file sizes in bytes.
#'     \item \code{3}: Displays directory structure with file sizes in B, KB, MB, or GB.
#'   }
#' @param sort_by A string. The sorting method for files: "name", "size", "date". Defaults to "name".
#' @return No return value. The function prints the directory structure.
#' @examples
#' # Simple directory structure, sorted by name
#' print_tree("path/to/directory", show = 1, sort_by = "name")
#'
#' # Directory structure with file sizes in bytes, sorted by size
#' print_tree("path/to/directory", show = 2, sort_by = "size")
#'
#' # Directory structure with file sizes in human-readable units, sorted by date
#' print_tree("path/to/directory", show = 3, sort_by = "date")
#'
#' @export
print_tree <- function(dir_path = ".", prefix = "", show = 1, sort_by = "name") {

  # Get list of files and directories in the specified path
  files <- list.files(dir_path, full.names = TRUE)

  # Get file information (size, modification time, etc.)
  file_info <- file.info(files)

  # Sort files based on the 'sort_by' argument
  if (sort_by == "size") {
    files <- files[order(file_info$size, decreasing = TRUE)]
  } else if (sort_by == "date") {
    files <- files[order(file_info$mtime, decreasing = TRUE)]
  } else {
    files <- files[order(basename(files))]
  }

  for (i in seq_along(files)) {
    file_name <- basename(files[i])
    file_size <- file_info$size[i]
    file_mtime <- format(file_info$mtime[i], "%Y-%m-%d %H:%M:%S")  # Format modification date
    is_directory <- file_info$isdir[i]
    formatted_size <- format_size(file_size)
    file_icon <- get_file_type_icon(file_name, is_directory)  # Get file type icon
    is_last <- (i == length(files))

    # Print the directory tree based on the 'show' argument
    if (show == 1) {
      if (is_last) {
        cat(prefix, "└──", file_icon, file_name, "(", file_mtime, ")\n", sep = "")
      } else {
        cat(prefix, "├──", file_icon, file_name, "(", file_mtime, ")\n", sep = "")
      }
    } else if (show == 2) {
      if (is_last) {
        cat(prefix, "└──", file_icon, file_name, "(", format(file_size, big.mark = ",", scientific = FALSE), " bytes, ", file_mtime, ")\n", sep = "")
      } else {
        cat(prefix, "├──", file_icon, file_name, "(", format(file_size, big.mark = ",", scientific = FALSE), " bytes, ", file_mtime, ")\n", sep = "")
      }
    } else if (show == 3) {
      if (is_last) {
        cat(prefix, "└──", file_icon, file_name, "(", formatted_size, ", ", file_mtime, ")\n", sep = "")
      } else {
        cat(prefix, "├──", file_icon, file_name, "(", formatted_size, ", ", file_mtime, ")\n", sep = "")
      }
    }

    # Recursively print subdirectories
    if (is_directory) {
      new_prefix <- if (is_last) paste0(prefix, "    ") else paste0(prefix, "│   ")
      print_tree(files[i], new_prefix, show, sort_by)
    }
  }
}

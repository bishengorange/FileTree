# File R/tree.R

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

#' Print Directory Tree Structure
#'
#' This function recursively prints the directory structure of the specified path.
#' It can display either a simple tree, a tree with file sizes in bytes, or a tree
#' with file sizes formatted in human-readable units (B, KB, MB, GB).
#'
#' @param dir_path A string. The directory path to be listed. Defaults to the current directory.
#' @param prefix A string. Internal parameter used for recursion (not to be set manually by users).
#' @param show An integer. Controls how the directory structure is printed.
#'   \itemize{
#'     \item \code{1}: Displays a simple directory structure.
#'     \item \code{2}: Displays directory structure with file sizes in bytes.
#'     \item \code{3}: Displays directory structure with file sizes in B, KB, MB, or GB.
#'   }
#' Defaults to 1.
#' @return No return value. The function prints the directory structure.
#' @examples
#' # Simple directory structure
#' print_tree("path/to/directory", show = 1)
#'
#' # Directory structure with file sizes in bytes
#' print_tree("path/to/directory", show = 2)
#'
#' # Directory structure with human-readable file sizes
#' print_tree("path/to/directory", show = 3)
#'
#' @export
print_tree <- function(dir_path = ".", prefix = "", show = 1) {
  files <- list.files(dir_path, full.names = TRUE)

  for (i in seq_along(files)) {
    file_name <- basename(files[i])
    file_size <- file.info(files[i])$size
    formatted_size <- format_size(file_size)
    is_last <- (i == length(files))

    if (show == 1) {
      if (is_last) {
        cat(prefix, "└──", file_name, "\n", sep = "")
      } else {
        cat(prefix, "├──", file_name, "\n", sep = "")
      }
    } else if (show == 2) {
      if (is_last) {
        cat(prefix, "└──", file_name, "(", format(file_size, big.mark = ",", scientific = FALSE), " bytes)\n", sep = "")
      } else {
        cat(prefix, "├──", file_name, "(", format(file_size, big.mark = ",", scientific = FALSE), " bytes)\n", sep = "")
      }
    } else if (show == 3) {
      if (is_last) {
        cat(prefix, "└──", file_name, "(", formatted_size, ")\n", sep = "")
      } else {
        cat(prefix, "├──", file_name, "(", formatted_size, ")\n", sep = "")
      }
    }

    if (file.info(files[i])$isdir) {
      new_prefix <- if (is_last) paste0(prefix, "    ") else paste0(prefix, "│   ")
      print_tree(files[i], new_prefix, show)
    }
  }
}

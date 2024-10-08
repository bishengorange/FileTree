% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_tree.R
\name{print_tree}
\alias{print_tree}
\title{Print Directory Tree Structure}
\usage{
print_tree(dir_path = ".", prefix = "", show = 1, sort_by = "name")
}
\arguments{
\item{dir_path}{A string. The directory path to be listed. Defaults to the current directory.}

\item{prefix}{A string. Internal parameter used for recursion (not to be set manually by users).}

\item{show}{An integer. Controls how the directory structure is printed.
\itemize{
\item \code{1}: Displays a simple directory structure.
\item \code{2}: Displays directory structure with file sizes in bytes.
\item \code{3}: Displays directory structure with file sizes in B, KB, MB, or GB.
}}

\item{sort_by}{A string. The sorting method for files: "name", "size", "date". Defaults to "name".}
}
\value{
No return value. The function prints the directory structure.
}
\description{
This function recursively prints the directory structure of the specified path.
It displays the file names, file sizes, last modified date, and file type icons.
}
\examples{
# Simple directory structure, sorted by name
print_tree("path/to/directory", show = 1, sort_by = "name")

# Directory structure with file sizes in bytes, sorted by size
print_tree("path/to/directory", show = 2, sort_by = "size")

# Directory structure with file sizes in human-readable units, sorted by date
print_tree("path/to/directory", show = 3, sort_by = "date")

}

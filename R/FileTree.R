#' Display a Tree View of a Directory
#'
#' Recursively display a visual file tree with icons, sizes, and colored levels.
#'
#' @param Path Root directory path to display. Defaults to current directory.
#' @param Prefix (internal) tree line prefix.
#' @param Size Logical. Whether to show file sizes.
#' @param Depth Maximum recursion depth. Use Inf for unlimited.
#' @param Level (internal) current recursion level.
#' @param Output Optional output file path.
#' @param Append Logical. Whether to append output to file.
#'
#' @return No return. Outputs text to console or file.
#' @export
FileTree <- function(Path = ".", Prefix = "", Size = TRUE, Depth = Inf,
                     Level = 0, Output = NULL, Append = FALSE) {

  FormatSize <- function(SizeValue) {
    if (is.na(SizeValue)) return("0 B")
    if (SizeValue < 1024) return(paste(SizeValue, "B"))
    Units <- c("KB", "MB", "GB", "TB")
    Power <- min(floor(log(SizeValue, 1024)), length(Units))
    sprintf("%.1f %s", SizeValue / (1024^Power), Units[Power])
  }

  GetIcon <- function(FileName, IsDir, IsLast) {
    Ext <- tolower(tools::file_ext(FileName))
    if (IsDir) return("📂")
    if (Ext %in% c("r", "py", "js", "cpp", "java")) return("📜")
    if (Ext %in% c("rproj")) return("📘")
    if (Ext %in% c("rdata", "rda")) return("💾")
    if (Ext %in% c("txt", "md", "doc", "docx", "rtf")) return("📝")
    if (Ext %in% c("csv", "tsv", "xlsx", "xls")) return("📊")
    if (Ext %in% c("png", "jpg", "jpeg", "gif", "bmp", "svg")) return("🖼️")
    if (Ext == "pdf") return("📕")
    if (Ext %in% c("zip", "gz", "tar", "7z", "rar")) return("📦")
    if (Ext %in% c("ppt", "pptx", "odp")) return("📽️")
    if (Ext %in% c("mp4", "avi", "mov", "mkv")) return("🎞️")
    if (Ext %in% c("mp3", "wav", "flac", "ogg")) return("🎵")
    if (Ext %in% c("html", "htm", "css")) return("🌐")
    if (IsLast) return("📃")
    return("📄")
  }

  Red    <- crayon::make_style("#F1441D")
  Orange <- crayon::make_style("#FFA500")
  Yellow <- crayon::make_style("#F7D94C")
  Green  <- crayon::make_style("#20894D")
  Cyan   <- crayon::make_style("#22A2C3")
  Blue   <- crayon::make_style("#2775B6")
  Purple <- crayon::make_style("#813C85")

  PrintLine <- function(Line) {
    if (!is.null(Output)) {
      cat(Line, file = Output, sep = "\n", append = TRUE)
    } else {
      cat(Line, "\n")
    }
  }

  if (Level == 0) {
    if (!is.null(Output) && !Append) file.create(Output)
    AllFiles <- list.files(Path, full.names = TRUE, recursive = TRUE)
    if (length(AllFiles) > 0) {
      FileInfo <- file.info(AllFiles)
      FileCount <- sum(!FileInfo$isdir, na.rm = TRUE)
      TotalSize <- sum(FileInfo$size, na.rm = TRUE)
    } else {
      FileCount <- 0
      TotalSize <- 0
    }

    PrintLine(paste0(Blue$bold("📂 Files Path: "), crayon::bold(Purple(normalizePath(Path)))))
    PrintLine(paste0(Blue$bold("📦 Total Files:"), " ", FileCount))
    PrintLine(paste0(Blue$bold("📏 Total Size:"), " ", FormatSize(TotalSize), "\n"))
  }

  Items <- list.files(Path, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  N <- length(Items)

  for (I in seq_along(Items)) {
    IsLast <- (I == N)
    Branch <- if (IsLast) "╰───" else "├───"
    FilePath <- Items[I]
    Name <- basename(FilePath)
    Info <- file.info(FilePath)
    if (is.na(Info$isdir)) next

    IsDir <- Info$isdir
    SizeStr <- ""

    if (Size) {
      if (IsDir) {
        All <- list.files(FilePath, full.names = TRUE, recursive = TRUE)
        Total <- if (length(All) > 0) sum(file.info(All)$size, na.rm = TRUE) else 0
      } else {
        Total <- Info$size
      }
      SizeStr <- FormatSize(Total)
    }

    Icon <- GetIcon(Name, IsDir, IsLast)

    ColoredName <- switch(
      as.character(Level),
      "0" = Red(Name),
      "1" = Orange(Name),
      "2" = Yellow(Name),
      "3" = Green(Name),
      "4" = Cyan(Name),
      "5" = Blue(Name),
      "6" = Purple(Name),
      Name
    )

    Line <- paste0(Prefix, Branch, " ", Icon, " ", ColoredName, " (", SizeStr, ")")
    PrintLine(Line)

    if (IsDir && Level < Depth) {
      NewPrefix <- paste0(Prefix, if (IsLast) "     " else "│    ")
      FileTree(FilePath, NewPrefix, Size, Depth, Level + 1, Output, Append = TRUE)
    }
  }
}

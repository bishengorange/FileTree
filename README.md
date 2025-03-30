# 📂 FileTree

**Visualize and export directory tree structures with icons, file sizes, and colored depth levels in R.**

![status](https://img.shields.io/badge/status-active-brightgreen)

------------------------------------------------------------------------

## 🚀 Installation

This is a development version. You can install it with:

``` r
# Clone or download the package locally, then:
devtools::install("D:/Rproject/FileTree")
```

Or load it directly in development mode:

``` r
devtools::load_all("D:/Rproject/FileTree")
```

## 🌲 Example Usage

``` r
library(FileTree)

# Show current working directory as tree
FileTree()

# Show 2 levels only
FileTree(Depth = 2)

# Output to a text file
FileTree(Path = ".", Output = "tree.txt")
```

Example output:

```         
📂 Files Path: D:/Rproject/FileTree 
📦 Total Files: 7 
📏 Total Size: 6.3 KB

├─── 📄 DESCRIPTION ( 540 B)
├─── 📘 FileTree.Rproj ( 425 B)
├─── 📂 man (2.1 KB)
│    ╰─── 📃 FileTree.Rd (2.1 KB)
├─── 📄 NAMESPACE (75 B)
├─── 📄 LICENSE (1.1 KB)
╰─── 📂 R (2.1 KB)
     ╰─── 📜 FileTree.R (2.1 KB)
```

## 🎨 Features

✅ Recursive directory traversal ✅ File sizes in human-readable units ✅ Color-coded output by tree level ✅ Emoji icons based on file type ✅ Supports output to plain text or Markdown ✅ Customizable depth and output target

## 📄 License

MIT License © Bi Sheng Orange

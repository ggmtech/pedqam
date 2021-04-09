# test mindr
library('mindr')
minexample(outline)
example(markmap)
example(markmapOption)
markmap
input <- c("# Chapter 1", "## Section 1.1", "## Section 1.2", "# Chapter 2")
mm(from = input, type = "text", root = "mindr")

### The outline of the book Learning R

input <- system.file("examples/xuer/xuer.md", package = "mindr")
list.files("examples/xuer/xuer.md")
mm(from = input, type = "file", root = "Learning R", to = "learningr.mm")

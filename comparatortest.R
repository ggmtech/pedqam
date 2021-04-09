#install.packages("comparator")
# install.packages("devtools")
# Levenshtein(): Levenshtein distance/similarity
# DamerauLevenshtein() Damerau-Levenshtein distance/similarity
# Hamming(): Hamming distance/similarity
# OSA(): Optimal String Alignment distance/similarity
# LCS(): Longest Common Subsequence distance/similarity
# Jaro(): Jaro distance/similarity
# JaroWinkler(): Jaro-Winkler distance/similarity
# Hybrid token-character:
#     MongeElkan(): Monge-Elkan similarity
#     FuzzyTokenSet(): Fuzzy Token Set distance
# Other:
#     InVocabulary(): Compares strings using a reference vocabulary. Useful for comparing names.
#     Lookup(): Retrieves distances/similarities from a lookup table
#     BinaryComp(): Compares strings based on whether they agree/disagree exactly.
# Numeric comparators:
#     Euclidean(): Euclidean (L-2) distance
#     Manhattan(): Manhattan (L-1) distance
#     Chebyshev(): Chebyshev (L-∞) distance
#     Minkowski(): Minkowski (L-p) distance

#devtools::install_github("ngmarchant/comparator")
library(comparator)
#A comparator is instantiated by calling its constructor function
comparator <- Levenshtein(similarity = TRUE, normalize = TRUE, ignore_case = TRUE)
#We can apply the comparator to character vectors element-wise as follows:
x <- c("John Doe", "Jane Doe")
y <- c("jonathon doe", "jane doe")
# elementwise(comparator, x, y)     #> [1] 0.6666667 1.0000000
comparator(x, y)  # shorthand for above
# also lists 
   
# syntax for Pairwise compare each string in x with each string in y and return a similarity matrix
pairwise(comparator, x, y, return_matrix = TRUE)


pairwise(comparator, x, return_matrix = TRUE) # compare strings in x pairwise and return a similarity matrix

#
# Script to read in the data file
#

## Filenames
meta_fn <- "06577-0001-Record_layout.txt"
data_fn <- "06577-0001-Data.txt"
# Reading metadata file: Skip the first 10 lines
meta <- readLines(meta_fn)[11:415]
# 1. Extract feature names
features <- unlist(lapply(strsplit(meta, ""), function(s) paste(s[c(12:19)], collapse="")))
features <- unlist(lapply(strsplit(features, " "), function(s) s[1])) # Get rid of whitespaces 
starts <- unlist(lapply(strsplit(meta, ""), function(s) as.integer(paste(s[c(25:27)], collapse=""))))
ends <- unlist(lapply(strsplit(meta, ""), function(s) as.integer(paste(s[c(33:35)], collapse=""))))
pos <- cbind(starts, ends)
# 3. Split each line in data file at starts/ends positions
data <- readLines(data_fn)
data_split_bytes <- strsplit(data, "")
result <- data.frame(matrix(NA, length(data), nrow(pos)))
colnames(result) <- features
for (i in 1:length(data)) {
    result[i,] <- apply(pos, 1, 
                    function(r) as.integer(paste(data_split_bytes[[i]][seq(r[1], r[2])], collapse="")))
}
write.csv(result, file = "HarvardCAS93.csv", quote = FALSE, row.names = FALSE)

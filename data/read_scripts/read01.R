#
# Script to read in the data file
#

## Filenames
meta_fn <- "04291-0001-Record_layout.txt"
data_fn <- "04291-0001-Data.txt"
# Reading metadata file: Skip the first 9 lines
meta <- readLines(meta_fn)[10:492]
# 1. Extract feature names
features <- unlist(lapply(strsplit(meta, ""), function(s) paste(s[c(14:21)], collapse="")))
features <- unlist(lapply(strsplit(features, " "), function(s) s[1])) # Get rid of whitespaces 
# 2. Extract types (this is a year with exception type)
types <- unlist(lapply(strsplit(meta, ""), function(s) paste(s[42:48], collapse="")))
var_exception <- which(types != "Numeric")
# 3. Extract start/end columns
starts <- unlist(lapply(strsplit(meta, ""), function(s) as.integer(paste(s[c(27:29)], collapse=""))))
ends <- unlist(lapply(strsplit(meta, ""), function(s) as.integer(paste(s[c(35:37)], collapse=""))))
pos <- cbind(starts, ends)[-var_exception,]
# 4. Split each line in data file at starts/ends positions
data <- readLines(data_fn)
data_split_bytes <- strsplit(data, "")
result <- data.frame(matrix(NA, length(data), nrow(pos)))
colnames(result) <- features[-var_exception]

for (i in 1:length(data)) {
    result[i,] <- apply(pos, 1, 
                    function(r) as.numeric(paste(data_split_bytes[[i]][seq(r[1], r[2])], collapse="")))
}
# 5. Date columns are appended at the end for convenience
result$G12B <- unlist(lapply(data_split_bytes, function(s) 
    paste(s[ seq(starts[var_exception], ends[var_exception]) ], collapse = "")))
# It actually looks like this column is totally missing for this year
write.csv(result, file = "HarvardCAS01.csv", quote = FALSE, row.names = FALSE)

library(tidyverse)
library(jsonlite)
library(optparse)
source("readOptions.R")
source("completeness-field-definitions.R")
source("draw2.R")

jsonOutputDir <- 'json2'
startTime <- proc.time()

path <- getFile(opt$inputFile)
if (!file.exists(path)) {
  stop(paste('path:', path, "does not exist"))
}
print(paste('path:', path, "Start", "@", format(Sys.time(), "%H:%M:%OS3")))
parts <- unlist(strsplit(path, '/', fixed = TRUE))
file <- parts[length(parts)]
id <- getId(file)

print(paste(path, 'draw completeness graph:', opt$drawCompletenessGraph))
print(paste(path, 'draw entropy graph:', opt$drawEntropyGraph))
print(paste(path, 'calculate existence:', opt$calculateExistence))
print(paste(path, 'calculate cardinalities:', opt$calculateCardinalities))
print(paste(path, 'calculate frequency tables:', opt$calculateFrequencyTables))
print(paste(path, 'calculate basic statistics:', opt$calculateBasicStatistics))
print(paste(path, 'calculate histograms:', opt$calculateHistograms))
print(paste(path, 'produce JSON files:', opt$produceJson))

print(paste(rep('=', 30), collapse=''))

print("read_csv")
qa <- read_csv(path, col_types = all_types, col_names = all_fields);
print("/read_csv")

print(dim(qa))
sum <- nrow(qa)
print(paste(path, 'total records:', sum))

for (name in cardinality_fields) {
  print(name)
  frequencies <- qa %>% 
    select(name) %>% 
    unlist(use.names = FALSE) %>% 
    table() %>% 
    as.data.frame()
  names(frequencies) <- c('label', 'count')
  frequencies$label <- as.numeric(as.character(frequencies$label))

  zeros <- frequencies %>% filter(label == 0) %>% nrow()
  ones <- frequencies %>% filter(label == 1) %>% nrow()
  if (zeros == 0) {
    frequencies[nrow(frequencies) + 1,] = list(0, 0)
  }
  if (ones == 0) {
    frequencies[nrow(frequencies) + 1,] = list(1, 0)
  }
  if (zeros == 0 || ones == 0) {
    frequencies <- frequencies %>% 
      arrange(label)
  }

  if (dim(frequencies)[1] >= 10) {
    data <- qa %>% 
      select(name) %>% 
      unlist(use.names = FALSE)
    data <- data[data > 1]

    hist <- data %>% 
      hist(plot = FALSE, breaks = 8)
    breaks <- hist$breaks
    breaks <- breaks[2:length(breaks)]
    counts <- hist$counts

    freq <- frequencies %>% 
      filter(label <= 1)
    freq$label <- as.character(freq$label)
    for (i in 1:length(breaks)) {
      freq[nrow(freq) + 1,] = list(paste0('<', breaks[i]), counts[i])
    }
    frequencies <- freq
  } else {
    frequencies$label <- as.character(frequencies$label)
  }
  frequencies$density <- frequencies$count / sum
  exportJson <- toJSON(frequencies)
  print(exportJson);
}

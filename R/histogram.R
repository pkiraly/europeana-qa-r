library(tidyverse)
library(jsonlite)
library(optparse)
source("readOptions.R")
source("completeness-field-definitions.R")
source("draw2.R")

jsonOutputDir <- '../json2'
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

histograms <- list()

fields_for_histograms <- c(completeness_fields, cardinality_fields, problem_fields)

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
isAnInteger <-
  function(x) length(x[is.wholenumber(x) == FALSE]) == 0

for (name in fields_for_histograms) {
  # if (name != 'total') next
  is_cardinality_field <- (substr(name, 1, 4) == "crd_")
  frequencies <- qa %>% 
    select(name) %>% 
    unlist(use.names = FALSE) %>% 
    table() %>% 
    as.data.frame()
  names(frequencies) <- c('label', 'count')
  frequencies$label <- as.numeric(as.character(frequencies$label))
  isInteger <- isAnInteger(frequencies$label)

  zeros <- frequencies %>% filter(label == 0) %>% nrow()
  ones <- frequencies %>% filter(label == 1) %>% nrow()
  if (zeros == 0) {
    frequencies[nrow(frequencies) + 1,] = list(0, 0)
  }
  if (is_cardinality_field == TRUE && ones == 0) {
    frequencies[nrow(frequencies) + 1,] = list(1, 0)
  }
  if (zeros == 0 || (is_cardinality_field == TRUE && ones == 0)) {
    frequencies <- frequencies %>% 
      arrange(label)
  }
  
  if (dim(frequencies)[1] >= 10) {
    data <- qa %>% 
      select(name) %>% 
      unlist(use.names = FALSE)
    if (is_cardinality_field == TRUE) {
      min_label <- 1
      data <- data[data > 1]
      number_of_bins <- 8
    } else {
      min_label <- 0.0
      data <- data[data > 0.0]
      number_of_bins <- 8
    }
    
    hist <- data %>% 
      hist(plot = FALSE, breaks = number_of_bins)
    breaks <- hist$breaks
    breaks <- breaks[2:length(breaks)]
    counts <- hist$counts

    freq <- frequencies %>% 
      filter(label <= min_label)

    freq$label <- as.character(freq$label)
    for (i in 1:length(breaks)) {
      if (isInteger) {
        if (i == 1) {
          prev <- min_label + 1
        } else {
          prev <- breaks[i-1] + 1
        }
        if (prev < breaks[i]) {
          label <- paste0(prev, '-', breaks[i])
        } else if (prev == breaks[i]) {
          label = breaks[i]
        } else {
          label <- paste0('<', breaks[i])
        }
      } else {
        if (i == 1) {
          prev <- as.numeric(min_label)
        } else {
          prev <- as.numeric(breaks[i-1])
        }
        min <- frequencies %>% filter(label > prev) %>% min()
        if (min < breaks[i]) {
          label <- paste0(min, '-', breaks[i])
        } else {
          label <- breaks[i]
        }
      }
      freq[nrow(freq) + 1,] = list(label, counts[i])
    }

    frequencies <- freq
  } else {
    frequencies$label <- as.character(frequencies$label)
  }
  frequencies$density <- frequencies$count / sum
  histograms[[tolower(name)]] <- frequencies
  # exportJson <- toJSON(frequencies)
  # print(exportJson);
}

exportJson <- toJSON(histograms)
fileName <- paste0(jsonOutputDir, '/', id, '/', id, ".cardinality.histogram.json")
print(fileName)
write(exportJson, fileName)
rm(histograms)

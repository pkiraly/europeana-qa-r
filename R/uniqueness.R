library(tidyverse, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(readr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(pastecs, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(grid, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(gridExtra, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(jsonlite, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(psych, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(optparse, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
source("R/readOptions.R")
source("R/uniqueness-field-definitions.R")
source("R/draw2.R")

jsonOutputDir <- opt$outputDir
if (!file.exists(jsonOutputDir)) {
  stop(paste('output dir:', jsonOutputDir, "does not exist"))
}
startTime <- proc.time()

path <- getFile(opt$inputFile)
if (!file.exists(path)) {
  stop(paste('path:', path, "does not exist"))
}
print(paste('path:', path, "Start", "@", format(Sys.time(), "%H:%M:%OS3")))
parts <- unlist(strsplit(path, '/', fixed = TRUE))
file <- parts[length(parts)]
id <- getId(file)

ifelse(
  !dir.exists(file.path(jsonOutputDir, id)), 
  dir.create(file.path(jsonOutputDir, id)),
  FALSE
)

print(paste(path, 'file:', file))
print(paste(path, 'id:', id))
print(paste(path, 'draw completeness graph:', opt$drawCompletenessGraph))
print(paste(path, 'draw entropy graph:', opt$drawEntropyGraph))
print(paste(path, 'calculate existence:', opt$calculateExistence))
print(paste(path, 'calculate cardinalities:', opt$calculateCardinalities))
print(paste(path, 'calculate frequency tables:', opt$calculateFrequencyTables))
print(paste(path, 'calculate basic statistics:', opt$calculateBasicStatistics))
print(paste(path, 'calculate histograms:', opt$calculateHistograms))
print(paste(path, 'produce JSON files:', opt$produceJson))

print(paste(rep('=', 30), collapse=''))

allbins <- read_csv("uniqueness.allbins.csv")
print(allbins)
for (name in uniqueness_fields) {
  print(name)
  row <- unname(unlist(allbins[allbins$name == name,c('x1', 'x2', 'x20', 'x40', 'x60', 'x80', 'x100')]))
  print(row)
}
stop("stop")

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

histograms <- list()

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
isAnInteger <-
  function(x) length(x[is.wholenumber(x) == FALSE]) == 0

for (name in uniqueness_fields) {
  # if (name != 'total') next
  print(name)
  frequencies <- qa %>% 
    select(name) %>% 
    unlist(use.names = FALSE) %>% 
    table() %>% 
    as.data.frame()
  names(frequencies) <- c('label', 'count')
  print(frequencies)
  
  frequencies$label <- as.numeric(as.character(frequencies$label))
  isInteger <- isAnInteger(frequencies$label)
  
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
    min_label <- 0.0
    data <- data[data > 0.0]
    number_of_bins <- 8

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
fileName <- paste0(jsonOutputDir, '/', id, '/', id, ".uniqueness.histogram.json")
print(paste(path, 'output filename:', fileName))
write(exportJson, fileName)
rm(histograms)

duration <- (proc.time() - startTime)
print(paste(path, "Finished", "@", format(Sys.time(), "%H:%M:%OS3"),
            sprintf("time: %s, user: %s, sys: %s",
                    duration['elapsed'][[1]], duration['user.self'][[1]], duration['sys.self'][[1]])))
rm(list=ls())

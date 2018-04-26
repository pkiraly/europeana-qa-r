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

allbins_row <- as.data.frame(read_csv("uniqueness.allbins.csv"))
allbins <- list()
for (name in uniqueness_fields) {
  allbins[[name]] <- unlist(unname(allbins_row[allbins_row$name == name,c('x1', 'x2', 'x20', 'x40', 'x60', 'x80', 'x100')]))
}

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

histograms <- list()

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
isAnInteger <-
  function(x) length(x[is.wholenumber(x) == FALSE]) == 0

for (name in uniqueness_fields) {
  data <- qa %>% 
    select(name) %>% 
    unlist(use.names = FALSE)

  data <- data[data > 0.0]

  bins <- allbins[[name]]
  frequencies <- data.frame(label=character(), count=numeric(), percent=numeric)
  total <- length(data[data > 0.0])
  for (i in 1:6) {
    label <- ifelse(i == 1, 1, ifelse(i == 6, paste0(bins[i], '-'), paste0(bins[i], '-', bins[i+1] - 1)))
    count <- length(data[data >= bins[i] & data < bins[i+1]])
    percent <- ifelse(total == 0, 0, count / total)
    frequencies <- rbind(frequencies, data.frame(label=label, count=count, percent=percent))
  }

  score_field <- paste0(name, '_score')
  data <- qa %>% 
    select(score_field) %>% 
    unlist(use.names = FALSE)

  result <- list()
  max <- ifelse(total == 0, 0, max(data))
  min <- ifelse(total == 0, 0, min(data))
  qa2 <- as.data.frame(qa %>% select(id, value=score_field))
  recMin <- qa2[qa2$value == min,][1,1]
  recMax <- qa2[qa2$value == max,][1,1]
  result$statistics <- data.frame(
    n=total,
    mean=mean(data), sd=sd(data),
    min=min, recMin=recMin,
    max=max, recMax=recMax
  )
  result$frequencies <- frequencies

  histograms[[tolower(name)]] <- result
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

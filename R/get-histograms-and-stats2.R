library(readr)
library(pastecs)
library(ggplot2)
library(grid)
library(gridExtra)
library(jsonlite)
library(plyr)
library(psych)
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

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

collector <- list()

if (opt$produceJson) {
  count <- c(sum)
  names(count) <- c('count')
  count <- data.frame(count)
  collector[["count"]][['count']] <- sum
  exportJson <- toJSON(count)
  write(exportJson, paste(jsonOutputDir, '/', id, ".count.json", sep=""))
  rm(count)
}

# STARTS field existency process
if (opt$produceJson && opt$calculateExistence) {
  print(paste(path, "calculate field frequencies"))
  frequencies <- read.table(text = '', colClasses = c("character", "numeric", 'numeric'), col.names = c('field', 'count', 'frequency'))

  for (fieldName in has_a_fields) {
    countTable <- table(qa[fieldName])
    countFrame <- data.frame(countTable)
    countVal <-countFrame[countFrame$Var1 == 1,c('Freq')]
    if (length(countVal) == 0) {
      countVal <- 0
    }

    freqFrame <- data.frame(prop.table(countTable))
    freqVal <-freqFrame[freqFrame$Var1 == 1,c('Freq')]
    if (length(freqVal) == 0) {
      freqVal <- 0
    }

    frequencies <- rbind(frequencies, data.frame(field = tolower(fieldName), count = countVal, frequency = freqVal))
  }
  collector[["frequencies"]] <- frequencies
  exportJson <- toJSON(frequencies)
  write(exportJson, paste(jsonOutputDir, '/', id, ".freq.json", sep=""))
  rm(frequencies)
}
# ENDS field existency process

# STARTS field cardinality process
if (opt$produceJson && opt$calculateCardinalities) {
  print(paste(path, "calculate field cardinality"))
  cardinalities <- read.table(text = '',
                            colClasses = c("character", "numeric", 'numeric'),
                            col.names = c('field', 'count', 'sum', 'mean', 'median'))

  for (fieldName in cardinality_fields) {
    xs <- qa[,fieldName];
    fieldCount <- length(xs[xs > 0])
    fieldSum <- sum(xs)
    fieldMean <- mean(xs)
    fieldMedian <- median(xs)
    cardinalities <- rbind(cardinalities, data.frame(
      field = tolower(substring(fieldName, 5)), count = fieldCount, sum = fieldSum, mean = fieldMean, median = fieldMedian))
  }
  collector[["cardinalities"]] <- cardinalities
  exportJson <- toJSON(cardinalities)
  write(exportJson, paste(jsonOutputDir, '/', id, ".cardinality.json", sep=""))
  rm(cardinalities)
}
# ENDS field cardinality process

if (opt$produceJson && opt$calculateFrequencyTables) {
  print(paste(path, "calculate frequency tables"))
  frequencyTable <- list()

  for (field in all_fields) {
    if (field != 'id' && field != 'collection' && field != 'provider') {
      freq <- count(qa, field)
      rows <- list()
      for (i in 1:dim(freq)[[1]]) {
        key <- as.character(freq[i, field])
        rows[[key]] <- freq[i, 'freq'][[1]]
      }
      frequencyTable[[field]] <- rows
    }
  }

  collector[["frequencyTable"]] <- frequencyTable
  exportJson <- toJSON(frequencyTable)
  jsonFileName <- paste0(jsonOutputDir, '/', id, '.frequency.table.json');
  write(exportJson, jsonFileName)
  rm(frequencyTable)
  rm(jsonFileName)
} # frequencyTable

# if (opt$produceJson) {
if (opt$produceJson && opt$calculateBasicStatistics) {
    print(paste(path, "basic statistics"))
  # stat_names <- c(completeness_fields, cardinality_fields, problem_fields, entropy_fields)
  stat_names <- c(completeness_fields, cardinality_fields, problem_fields)
  stats <- round(stat.desc(qa[,stat_names], basic=TRUE), digits=4)
  names(stats) <- tolower(names(stats))

  stats <- stats[!(rownames(stats) %in% c("nbr.val", "nbr.null", "nbr.na", "sum")),]
  stats <- data.frame(t(stats))

  for (name in stat_names) {
    stats <- setMinMaxRecId(stats, qa, name)
  }
  collector[["stats"]] <- stats
  exportJson <- toJSON(stats)
  write(exportJson, paste(jsonOutputDir, '/', id, ".json", sep=""))
  rm(stats)
}

if (opt$produceJson && opt$calculateHistograms) {
  print(paste(path, "histograms"))
  histograms <- list()
  stat_names <- c(completeness_fields, cardinality_fields, problem_fields)
  for (name in stat_names) {
    print(name)
    print('h')
    h <- hist(qa[,c(name)], plot = FALSE, breaks = 8)
    print('/h')
    hist <- read.table(text = '', colClasses = c("character", "numeric", 'numeric'), col.names = c('label', 'count', 'density'),
                       stringsAsFactors = FALSE)
    for (i in 1:length(h$counts)) {
      label <- paste(h$breaks[i], h$breaks[i + 1], sep = " - ")
      density <- h$counts[i] * 100 / sum
      hist[i, ] <- c(label, h$counts[i], density)
    }
    histograms[[tolower(name)]] <- hist
  }
  collector[["histograms"]] <- histograms
  exportJson <- toJSON(histograms)
  write(exportJson, paste(jsonOutputDir, '/', id, ".hist.json", sep=""))
  rm(histograms)

  exportJson <- toJSON(collector)
  write(exportJson, paste(jsonOutputDir, '/', id, ".collector.json", sep=""))
  rm(collector)
}

if (opt$drawCompletenessGraph == TRUE || opt$drawEntropyGraph == TRUE) {
  prepareImageDirectory(id)
}

if (opt$drawCompletenessGraph == TRUE) {
  prepareImageDirectory(id)
  for (i in 1:length(completeness_labels)) {
    print(paste(path, "drawing subdimension", completeness_labels[i]))
    draw(qa, completeness_fields[i], completeness_labels[i])
  }

  for (i in 1:length(problem_fields)) {
    print(paste(path, "drawing problem catalog", problem_labels[i]))
    draw(qa, problem_fields[i], problem_labels[i])
  }

  print(paste(path, "drawing cardinalities"))
  for (fieldName in cardinality_fields) {
    label <- sub('_', ':',
                 sub('Aggregation_', 'Aggregation/',
                     sub('Proxy_', 'Proxy/',
                         sub('Agent_', 'Agent/',
                             sub('Place_', 'Place/',
                                 sub('Timespan_', 'Timespan/',
                                     sub('Concept_', 'Concept/',
                                         sub('ProvidedCHO_', 'ProvidedCHO/',
                                             sub('crd_', '', fieldName)))))))))
    print(paste(path, "drawing cardinality of", label))
    draw(qa, fieldName, label)
  }
  warnings()
}

################################
# drawing entropy fields
################################

if (opt$drawEntropyGraph == TRUE) {
  for (i in 1:length(entropy_fields)) {
    print(paste(path, "drawing", entropy_labels[i]))
    draw(qa, entropy_fields[i], entropy_labels[i])
  }
  warnings()
}

warnings()
# stopQuietly()

duration <- (proc.time() - startTime)
print(paste(path, "Finished", "@", format(Sys.time(), "%H:%M:%OS3"),
            sprintf("time: %s, user: %s, sys: %s",
                    duration['elapsed'][[1]], duration['user.self'][[1]], duration['sys.self'][[1]])))
rm(list=ls())

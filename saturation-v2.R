library(readr)
library(pastecs)
library(ggplot2)
library(grid) # stat.desc
library(gridExtra)
library(jsonlite)
library(plyr)
library(psych)
library(optparse)
source("saturationOptions.R")
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

print(paste(path, 'draw saturation graph:', opt$drawSaturationGraph))
print(paste(path, 'calculate saturation:', opt$calculateSaturation))
print(paste(path, 'produce JSON files:', opt$produceJson))

print(paste(rep('=', 30), collapse=''))

#id_fields <- c('collection')
id_fields <- c('id', 'collection', 'provider')
id_types <- paste(rep('c', length(id_fields)), collapse='')

specific_fields <- c(
  'provider/dc:title/taggedLiterals', 'provider/dc:title/languages', 'provider/dc:title/literalsPerLanguage',
  'europeana/dc:title/taggedLiterals', 'europeana/dc:title/languages', 'europeana/dc:title/literalsPerLanguage',
  'provider/dcterms:alternative/taggedLiterals', 'provider/dcterms:alternative/languages', 'provider/dcterms:alternative/literalsPerLanguage',
  'europeana/dcterms:alternative/taggedLiterals', 'europeana/dcterms:alternative/languages', 'europeana/dcterms:alternative/literalsPerLanguage',
  'provider/dc:description/taggedLiterals', 'provider/dc:description/languages', 'provider/dc:description/literalsPerLanguage',
  'europeana/dc:description/taggedLiterals', 'europeana/dc:description/languages', 'europeana/dc:description/literalsPerLanguage',
  'provider/dc:creator/taggedLiterals', 'provider/dc:creator/languages', 'provider/dc:creator/literalsPerLanguage',
  'europeana/dc:creator/taggedLiterals', 'europeana/dc:creator/languages', 'europeana/dc:creator/literalsPerLanguage',
  'provider/dc:publisher/taggedLiterals', 'provider/dc:publisher/languages', 'provider/dc:publisher/literalsPerLanguage',
  'europeana/dc:publisher/taggedLiterals', 'europeana/dc:publisher/languages', 'europeana/dc:publisher/literalsPerLanguage',
  'provider/dc:contributor/taggedLiterals', 'provider/dc:contributor/languages', 'provider/dc:contributor/literalsPerLanguage',
  'europeana/dc:contributor/taggedLiterals', 'europeana/dc:contributor/languages', 'europeana/dc:contributor/literalsPerLanguage',
  'provider/dc:type/taggedLiterals', 'provider/dc:type/languages', 'provider/dc:type/literalsPerLanguage',
  'europeana/dc:type/taggedLiterals', 'europeana/dc:type/languages', 'europeana/dc:type/literalsPerLanguage',
  'provider/dc:identifier/taggedLiterals', 'provider/dc:identifier/languages', 'provider/dc:identifier/literalsPerLanguage',
  'europeana/dc:identifier/taggedLiterals', 'europeana/dc:identifier/languages', 'europeana/dc:identifier/literalsPerLanguage',
  'provider/dc:language/taggedLiterals', 'provider/dc:language/languages', 'provider/dc:language/literalsPerLanguage',
  'europeana/dc:language/taggedLiterals', 'europeana/dc:language/languages', 'europeana/dc:language/literalsPerLanguage',
  'provider/dc:coverage/taggedLiterals', 'provider/dc:coverage/languages', 'provider/dc:coverage/literalsPerLanguage',
  'europeana/dc:coverage/taggedLiterals', 'europeana/dc:coverage/languages', 'europeana/dc:coverage/literalsPerLanguage',
  'provider/dcterms:temporal/taggedLiterals', 'provider/dcterms:temporal/languages', 'provider/dcterms:temporal/literalsPerLanguage',
  'europeana/dcterms:temporal/taggedLiterals', 'europeana/dcterms:temporal/languages', 'europeana/dcterms:temporal/literalsPerLanguage',
  'provider/dcterms:spatial/taggedLiterals', 'provider/dcterms:spatial/languages', 'provider/dcterms:spatial/literalsPerLanguage',
  'europeana/dcterms:spatial/taggedLiterals', 'europeana/dcterms:spatial/languages', 'europeana/dcterms:spatial/literalsPerLanguage',
  'provider/dc:subject/taggedLiterals', 'provider/dc:subject/languages', 'provider/dc:subject/literalsPerLanguage',
  'europeana/dc:subject/taggedLiterals', 'europeana/dc:subject/languages', 'europeana/dc:subject/literalsPerLanguage',
  'provider/dc:date/taggedLiterals', 'provider/dc:date/languages', 'provider/dc:date/literalsPerLanguage',
  'europeana/dc:date/taggedLiterals', 'europeana/dc:date/languages', 'europeana/dc:date/literalsPerLanguage',
  'provider/dcterms:created/taggedLiterals', 'provider/dcterms:created/languages', 'provider/dcterms:created/literalsPerLanguage',
  'europeana/dcterms:created/taggedLiterals', 'europeana/dcterms:created/languages', 'europeana/dcterms:created/literalsPerLanguage',
  'provider/dcterms:issued/taggedLiterals', 'provider/dcterms:issued/languages', 'provider/dcterms:issued/literalsPerLanguage',
  'europeana/dcterms:issued/taggedLiterals', 'europeana/dcterms:issued/languages', 'europeana/dcterms:issued/literalsPerLanguage',
  'provider/dcterms:extent/taggedLiterals', 'provider/dcterms:extent/languages', 'provider/dcterms:extent/literalsPerLanguage',
  'europeana/dcterms:extent/taggedLiterals', 'europeana/dcterms:extent/languages', 'europeana/dcterms:extent/literalsPerLanguage',
  'provider/dcterms:medium/taggedLiterals', 'provider/dcterms:medium/languages', 'provider/dcterms:medium/literalsPerLanguage',
  'europeana/dcterms:medium/taggedLiterals', 'europeana/dcterms:medium/languages', 'europeana/dcterms:medium/literalsPerLanguage',
  'provider/dcterms:provenance/taggedLiterals', 'provider/dcterms:provenance/languages', 'provider/dcterms:provenance/literalsPerLanguage',
  'europeana/dcterms:provenance/taggedLiterals', 'europeana/dcterms:provenance/languages', 'europeana/dcterms:provenance/literalsPerLanguage',
  'provider/dcterms:hasPart/taggedLiterals', 'provider/dcterms:hasPart/languages', 'provider/dcterms:hasPart/literalsPerLanguage',
  'europeana/dcterms:hasPart/taggedLiterals', 'europeana/dcterms:hasPart/languages', 'europeana/dcterms:hasPart/literalsPerLanguage',
  'provider/dcterms:isPartOf/taggedLiterals', 'provider/dcterms:isPartOf/languages', 'provider/dcterms:isPartOf/literalsPerLanguage',
  'europeana/dcterms:isPartOf/taggedLiterals', 'europeana/dcterms:isPartOf/languages', 'europeana/dcterms:isPartOf/literalsPerLanguage',
  'provider/dc:format/taggedLiterals', 'provider/dc:format/languages', 'provider/dc:format/literalsPerLanguage',
  'europeana/dc:format/taggedLiterals', 'europeana/dc:format/languages', 'europeana/dc:format/literalsPerLanguage',
  'provider/dc:source/taggedLiterals', 'provider/dc:source/languages', 'provider/dc:source/literalsPerLanguage',
  'europeana/dc:source/taggedLiterals', 'europeana/dc:source/languages', 'europeana/dc:source/literalsPerLanguage',
  'provider/dc:rights/taggedLiterals', 'provider/dc:rights/languages', 'provider/dc:rights/literalsPerLanguage',
  'europeana/dc:rights/taggedLiterals', 'europeana/dc:rights/languages', 'europeana/dc:rights/literalsPerLanguage',
  'provider/dc:relation/taggedLiterals', 'provider/dc:relation/languages', 'provider/dc:relation/literalsPerLanguage',
  'europeana/dc:relation/taggedLiterals', 'europeana/dc:relation/languages', 'europeana/dc:relation/literalsPerLanguage',
  'provider/edm:year/taggedLiterals', 'provider/edm:year/languages', 'provider/edm:year/literalsPerLanguage',
  'europeana/edm:year/taggedLiterals', 'europeana/edm:year/languages', 'europeana/edm:year/literalsPerLanguage',
  'provider/edm:userTag/taggedLiterals', 'provider/edm:userTag/languages', 'provider/edm:userTag/literalsPerLanguage',
  'europeana/edm:userTag/taggedLiterals', 'europeana/edm:userTag/languages', 'europeana/edm:userTag/literalsPerLanguage',
  'provider/dcterms:conformsTo/taggedLiterals', 'provider/dcterms:conformsTo/languages', 'provider/dcterms:conformsTo/literalsPerLanguage',
  'europeana/dcterms:conformsTo/taggedLiterals', 'europeana/dcterms:conformsTo/languages', 'europeana/dcterms:conformsTo/literalsPerLanguage',
  'provider/dcterms:hasFormat/taggedLiterals', 'provider/dcterms:hasFormat/languages', 'provider/dcterms:hasFormat/literalsPerLanguage',
  'europeana/dcterms:hasFormat/taggedLiterals', 'europeana/dcterms:hasFormat/languages', 'europeana/dcterms:hasFormat/literalsPerLanguage',
  'provider/dcterms:hasVersion/taggedLiterals', 'provider/dcterms:hasVersion/languages', 'provider/dcterms:hasVersion/literalsPerLanguage',
  'europeana/dcterms:hasVersion/taggedLiterals', 'europeana/dcterms:hasVersion/languages', 'europeana/dcterms:hasVersion/literalsPerLanguage',
  'provider/dcterms:isFormatOf/taggedLiterals', 'provider/dcterms:isFormatOf/languages', 'provider/dcterms:isFormatOf/literalsPerLanguage',
  'europeana/dcterms:isFormatOf/taggedLiterals', 'europeana/dcterms:isFormatOf/languages', 'europeana/dcterms:isFormatOf/literalsPerLanguage',
  'provider/dcterms:isReferencedBy/taggedLiterals', 'provider/dcterms:isReferencedBy/languages', 'provider/dcterms:isReferencedBy/literalsPerLanguage',
  'europeana/dcterms:isReferencedBy/taggedLiterals', 'europeana/dcterms:isReferencedBy/languages', 'europeana/dcterms:isReferencedBy/literalsPerLanguage',
  'provider/dcterms:isReplacedBy/taggedLiterals', 'provider/dcterms:isReplacedBy/languages', 'provider/dcterms:isReplacedBy/literalsPerLanguage',
  'europeana/dcterms:isReplacedBy/taggedLiterals', 'europeana/dcterms:isReplacedBy/languages', 'europeana/dcterms:isReplacedBy/literalsPerLanguage',
  'provider/dcterms:isRequiredBy/taggedLiterals', 'provider/dcterms:isRequiredBy/languages', 'provider/dcterms:isRequiredBy/literalsPerLanguage',
  'europeana/dcterms:isRequiredBy/taggedLiterals', 'europeana/dcterms:isRequiredBy/languages', 'europeana/dcterms:isRequiredBy/literalsPerLanguage',
  'provider/dcterms:isVersionOf/taggedLiterals', 'provider/dcterms:isVersionOf/languages', 'provider/dcterms:isVersionOf/literalsPerLanguage',
  'europeana/dcterms:isVersionOf/taggedLiterals', 'europeana/dcterms:isVersionOf/languages', 'europeana/dcterms:isVersionOf/literalsPerLanguage',
  'provider/dcterms:references/taggedLiterals', 'provider/dcterms:references/languages', 'provider/dcterms:references/literalsPerLanguage',
  'europeana/dcterms:references/taggedLiterals', 'europeana/dcterms:references/languages', 'europeana/dcterms:references/literalsPerLanguage',
  'provider/dcterms:replaces/taggedLiterals', 'provider/dcterms:replaces/languages', 'provider/dcterms:replaces/literalsPerLanguage',
  'europeana/dcterms:replaces/taggedLiterals', 'europeana/dcterms:replaces/languages', 'europeana/dcterms:replaces/literalsPerLanguage',
  'provider/dcterms:requires/taggedLiterals', 'provider/dcterms:requires/languages', 'provider/dcterms:requires/literalsPerLanguage',
  'europeana/dcterms:requires/taggedLiterals', 'europeana/dcterms:requires/languages', 'europeana/dcterms:requires/literalsPerLanguage',
  'provider/dcterms:tableOfContents/taggedLiterals', 'provider/dcterms:tableOfContents/languages', 'provider/dcterms:tableOfContents/literalsPerLanguage',
  'europeana/dcterms:tableOfContents/taggedLiterals', 'europeana/dcterms:tableOfContents/languages', 'europeana/dcterms:tableOfContents/literalsPerLanguage',
  'provider/edm:currentLocation/taggedLiterals', 'provider/edm:currentLocation/languages', 'provider/edm:currentLocation/literalsPerLanguage',
  'europeana/edm:currentLocation/taggedLiterals', 'europeana/edm:currentLocation/languages', 'europeana/edm:currentLocation/literalsPerLanguage',
  'provider/edm:hasMet/taggedLiterals', 'provider/edm:hasMet/languages', 'provider/edm:hasMet/literalsPerLanguage',
  'europeana/edm:hasMet/taggedLiterals', 'europeana/edm:hasMet/languages', 'europeana/edm:hasMet/literalsPerLanguage',
  'provider/edm:hasType/taggedLiterals', 'provider/edm:hasType/languages', 'provider/edm:hasType/literalsPerLanguage',
  'europeana/edm:hasType/taggedLiterals', 'europeana/edm:hasType/languages', 'europeana/edm:hasType/literalsPerLanguage',
  'provider/edm:incorporates/taggedLiterals', 'provider/edm:incorporates/languages', 'provider/edm:incorporates/literalsPerLanguage',
  'europeana/edm:incorporates/taggedLiterals', 'europeana/edm:incorporates/languages', 'europeana/edm:incorporates/literalsPerLanguage',
  'provider/edm:isDerivativeOf/taggedLiterals', 'provider/edm:isDerivativeOf/languages', 'provider/edm:isDerivativeOf/literalsPerLanguage',
  'europeana/edm:isDerivativeOf/taggedLiterals', 'europeana/edm:isDerivativeOf/languages', 'europeana/edm:isDerivativeOf/literalsPerLanguage',
  'provider/edm:isRelatedTo/taggedLiterals', 'provider/edm:isRelatedTo/languages', 'provider/edm:isRelatedTo/literalsPerLanguage',
  'europeana/edm:isRelatedTo/taggedLiterals', 'europeana/edm:isRelatedTo/languages', 'europeana/edm:isRelatedTo/literalsPerLanguage',
  'provider/edm:isRepresentationOf/taggedLiterals', 'provider/edm:isRepresentationOf/languages', 'provider/edm:isRepresentationOf/literalsPerLanguage',
  'europeana/edm:isRepresentationOf/taggedLiterals', 'europeana/edm:isRepresentationOf/languages', 'europeana/edm:isRepresentationOf/literalsPerLanguage',
  'provider/edm:isSimilarTo/taggedLiterals', 'provider/edm:isSimilarTo/languages', 'provider/edm:isSimilarTo/literalsPerLanguage',
  'europeana/edm:isSimilarTo/taggedLiterals', 'europeana/edm:isSimilarTo/languages', 'europeana/edm:isSimilarTo/literalsPerLanguage',
  'provider/edm:isSuccessorOf/taggedLiterals', 'provider/edm:isSuccessorOf/languages', 'provider/edm:isSuccessorOf/literalsPerLanguage',
  'europeana/edm:isSuccessorOf/taggedLiterals', 'europeana/edm:isSuccessorOf/languages', 'europeana/edm:isSuccessorOf/literalsPerLanguage',
  'provider/edm:realizes/taggedLiterals', 'provider/edm:realizes/languages', 'provider/edm:realizes/literalsPerLanguage',
  'europeana/edm:realizes/taggedLiterals', 'europeana/edm:realizes/languages', 'europeana/edm:realizes/literalsPerLanguage',
  'provider/edm:wasPresentAt/taggedLiterals', 'provider/edm:wasPresentAt/languages', 'provider/edm:wasPresentAt/literalsPerLanguage',
  'europeana/edm:wasPresentAt/taggedLiterals', 'europeana/edm:wasPresentAt/languages', 'europeana/edm:wasPresentAt/literalsPerLanguage'
);
# add 'saturation_' prefix
specific_fields <- gsub(':', '_', specific_fields)
specific_fields <- gsub('/', '_', specific_fields)
# print(specific_fields)
# specific_fields <- paste0('saturation_', specific_fields)
specific_types <- paste(rep('n', length(specific_fields)), collapse='')

generic_fields <- c(
  'NumberOfLanguagesPerPropertyInProviderProxy', 'NumberOfLanguagesPerPropertyInEuropeanaProxy',
  'NumberOfLanguagesPerPropertyInObject', 
  'TaggedLiteralsInProviderProxy', 'TaggedLiteralsInEuropeanaProxy',
  'DistinctLanguageCountInProviderProxy', 'DistinctLanguageCountInEuropeanaProxy', 
  'TaggedLiteralsInObject', 'DistinctLanguagesInObject',
  'TaggedLiteralsPerLanguageInProviderProxy', 'TaggedLiteralsPerLanguageInEuropeanaProxy',
  'TaggedLiteralsPerLanguageInObject'
);
generic_types <- paste(rep('n', length(generic_fields)), collapse='')

all_fields <- c(id_fields, specific_fields, generic_fields)
all_types <- paste(id_types, specific_types, generic_types, sep='')

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

field <- 'europeana_dc_title_taggedLiterals'
removable_stats <- c('nbr.val', 'nbr.null', 'nbr.na', 'sum')

# str(qa)
# select(qa, c('NumberOfLanguagesPerPropertyInProviderProxy'))
v <- as.vector(qa[,c('NumberOfLanguagesPerPropertyInProviderProxy')])
str(as.vector(qa[field][field]))
# head(qa)
# stat <- stat.desc(qa[qa[field] > -1, field], basic=TRUE)
# stat
# recMin <- head(qa[qa[field] == stat['min',1], 'id'], 1)
# recMax <- head(qa[qa[field] == stat['max',1], 'id'], 1)
# stat <- round(stat, digits=4)
# str(stat)

# stat[c('min', 'max', 'range', 'sum', 'median', 'mean', 'SE.mean', 'CI.mean.0.95', 'var', 'std.dev', 'coef.var'),]

# row_to_keep = c(F, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T)
# stat[row_to_keep,c('provider_dc_title_taggedLiterals')]
# stat
# stat <- data.frame(stat[!names(stat) %in% removable_stats])

stopQuietly()

if (opt$produceJson) {
  print(paste(path, "basic statistics"))
  # stat_names <- specific_fields #c(specific_fields, top_fields)
  stat_names <- generic_fields #c(specific_fields, top_fields)
  
  removable_stats <- c('nbr.val', 'nbr.null', 'nbr.na', 'sum')
  stats <- read.table(text = "1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1", 
                      colClasses = c('character'), col.names = c('dummy'))
  print(paste(path, "/read.table"))
  for (field in stat_names) {
    print(paste(path, "field:", field))
    stat <- stat.desc(qa[qa[field] > -1, field], basic=TRUE)
    stat
    print(paste(path, "recMin"))
    recMin <- head(qa[qa[field] == stat['min',1], 'id'], 1)
    print(paste(path, "recMin", stat['min',1], recMin))
    recMax <- head(qa[qa[field] == stat['max',1], 'id'], 1)
    print(paste(path, "recMax", stat['max',1], recMax))

    stat <- round(stat, digits=4)
    print(paste(path, "recMin"))
    stat['recMin',1] <- recMin
    print(paste(path, "recMax"))
    stat['recMax',1] <- recMax
    # stat <- data.frame(stat[!names(stat) %in% removable_stats])
    colnames(stat) <- tolower(field)

    print(paste(path, "cbind"))
    stats <- cbind(stats, stat)
  }
  print(paste(path, "/for"))
  stats <- stats[,colnames(stats) != 'dummy']
  stats <- data.frame(t(stats))
  exportJson <- toJSON(stats)
  write(exportJson, paste(jsonOutputDir, '/', id, ".saturation.json", sep=""))
  rm(stats)

  print(paste(path, "histograms"))
  histograms <- list()
  for (name in stat_names) {
    print(paste(path, "histogram", name))
    h <- hist(x = qa[,name], plot = FALSE, breaks = 8)
    print(paste(path, "read.table"))
    hist <- read.table(
      text = '',
      colClasses = c("character", "numeric", 'numeric'),
      col.names = c('label', 'count', 'density'),
      stringsAsFactors = FALSE)
    print(paste(path, "for"))
    for (i in 1:length(h$counts)) {
      label <- paste(h$breaks[i], h$breaks[i + 1], sep = " - ")
      density <- h$counts[i] * 100 / sum
      hist[i, ] <- c(label, h$counts[i], density)
    }
    histograms[[tolower(name)]] <- hist
  }
  exportJson <- toJSON(histograms)
  write(exportJson, paste(jsonOutputDir, '/', id, ".saturation.histogram.json", sep=""))
  rm(histograms)

  print(paste(path, "frequency table"))
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
  exportJson <- toJSON(frequencyTable)
  jsonFileName <- paste0(jsonOutputDir, '/', id, '.saturation.frequency.table.json');
  print(paste(path, 'saving', jsonFileName))
  write(exportJson, jsonFileName)
  rm(frequencyTable)
  rm(jsonFileName)
}

################################
# drawing saturation fields
################################

if (opt$drawSaturationGraph == TRUE) {
  print(paste(path, "drawing saturation"))
  for (fieldName in specific_fields) {
    label <- gsub('_', ':',
                 sub('Aggregation_', 'Aggregation/',
                     sub('Proxy_', 'Proxy/',
                         sub('Agent_', 'Agent/',
                             sub('Place_', 'Place/',
                                 sub('Timespan_', 'Timespan/',
                                     sub('Concept_', 'Concept/',
                                         sub('saturation_', '', fieldName))))))))
    # print(paste(path, "drawing saturation of", label))
    draw(qa, fieldName, label)
  }
  warnings()
}

if (opt$drawTopSaturationGraph == TRUE) {
  print(paste(path, "drawing top saturation"))
  for (fieldName in top_fields) {
    label <- fieldName
    # print(paste(path, "drawing saturation of", label))
    draw(qa, fieldName, label)
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

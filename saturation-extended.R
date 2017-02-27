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

completeness_fields <- c(
  'total', 'mandatory', 'descriptiveness', 'searchability', 'contextualization',
  'identification', 'browsing', 'viewing', 'reusability', 'multilinguality'
)
completeness_labels <- c(
  'All fields', 'Mandatory fields', 'Descriptiveness', 'Searchability', 'Contextualization',
  'Identification', 'Browsing', 'Viewing', 'Reusability', 'Multilinguality'
)
completeness_types <- paste(rep('n', length(completeness_fields)), collapse='')

saturation_fields <- c(
  'saturation:sum:Proxy/dc:title', 'saturation:average:Proxy/dc:title',
  'saturation:normalized:Proxy/dc:title',
  'saturation:sum:Proxy/dcterms:alternative', 'saturation:average:Proxy/dcterms:alternative',
  'saturation:normalized:Proxy/dcterms:alternative', 'saturation:sum:Proxy/dc:description',
  'saturation:average:Proxy/dc:description', 'saturation:normalized:Proxy/dc:description',
  'saturation:sum:Proxy/dc:creator',
  'saturation:average:Proxy/dc:creator', 'saturation:normalized:Proxy/dc:creator',
  'saturation:sum:Proxy/dc:publisher',
  'saturation:average:Proxy/dc:publisher', 'saturation:normalized:Proxy/dc:publisher',
  'saturation:sum:Proxy/dc:contributor',
  'saturation:average:Proxy/dc:contributor', 'saturation:normalized:Proxy/dc:contributor',
  'saturation:sum:Proxy/dc:type',
  'saturation:average:Proxy/dc:type', 'saturation:normalized:Proxy/dc:type',
  'saturation:sum:Proxy/dc:identifier',
  'saturation:average:Proxy/dc:identifier', 'saturation:normalized:Proxy/dc:identifier',
  'saturation:sum:Proxy/dc:language',
  'saturation:average:Proxy/dc:language', 'saturation:normalized:Proxy/dc:language',
  'saturation:sum:Proxy/dc:coverage',
  'saturation:average:Proxy/dc:coverage', 'saturation:normalized:Proxy/dc:coverage',
  'saturation:sum:Proxy/dcterms:temporal',
  'saturation:average:Proxy/dcterms:temporal', 'saturation:normalized:Proxy/dcterms:temporal',
  'saturation:sum:Proxy/dcterms:spatial', 'saturation:average:Proxy/dcterms:spatial',
  'saturation:normalized:Proxy/dcterms:spatial', 'saturation:sum:Proxy/dc:subject',
  'saturation:average:Proxy/dc:subject',
  'saturation:normalized:Proxy/dc:subject', 'saturation:sum:Proxy/dc:date',
  'saturation:average:Proxy/dc:date',
  'saturation:normalized:Proxy/dc:date', 'saturation:sum:Proxy/dcterms:created',
  'saturation:average:Proxy/dcterms:created',
  'saturation:normalized:Proxy/dcterms:created', 'saturation:sum:Proxy/dcterms:issued',
  'saturation:average:Proxy/dcterms:issued', 'saturation:normalized:Proxy/dcterms:issued',
  'saturation:sum:Proxy/dcterms:extent',
  'saturation:average:Proxy/dcterms:extent', 'saturation:normalized:Proxy/dcterms:extent',
  'saturation:sum:Proxy/dcterms:medium',
  'saturation:average:Proxy/dcterms:medium', 'saturation:normalized:Proxy/dcterms:medium',
  'saturation:sum:Proxy/dcterms:provenance', 'saturation:average:Proxy/dcterms:provenance',
  'saturation:normalized:Proxy/dcterms:provenance', 'saturation:sum:Proxy/dcterms:hasPart',
  'saturation:average:Proxy/dcterms:hasPart', 'saturation:normalized:Proxy/dcterms:hasPart',
  'saturation:sum:Proxy/dcterms:isPartOf', 'saturation:average:Proxy/dcterms:isPartOf',
  'saturation:normalized:Proxy/dcterms:isPartOf', 'saturation:sum:Proxy/dc:format',
  'saturation:average:Proxy/dc:format',
  'saturation:normalized:Proxy/dc:format', 'saturation:sum:Proxy/dc:source',
  'saturation:average:Proxy/dc:source',
  'saturation:normalized:Proxy/dc:source', 'saturation:sum:Proxy/dc:rights',
  'saturation:average:Proxy/dc:rights',
  'saturation:normalized:Proxy/dc:rights', 'saturation:sum:Proxy/dc:relation',
  'saturation:average:Proxy/dc:relation',
  'saturation:normalized:Proxy/dc:relation', 'saturation:sum:Proxy/edm:europeanaProxy',
  'saturation:average:Proxy/edm:europeanaProxy', 'saturation:normalized:Proxy/edm:europeanaProxy',
  'saturation:sum:Proxy/edm:year', 'saturation:average:Proxy/edm:year',
  'saturation:normalized:Proxy/edm:year',
  'saturation:sum:Proxy/edm:userTag', 'saturation:average:Proxy/edm:userTag',
  'saturation:normalized:Proxy/edm:userTag',
  'saturation:sum:Proxy/ore:proxyIn', 'saturation:average:Proxy/ore:proxyIn',
  'saturation:normalized:Proxy/ore:proxyIn',
  'saturation:sum:Proxy/ore:proxyFor', 'saturation:average:Proxy/ore:proxyFor',
  'saturation:normalized:Proxy/ore:proxyFor',
  'saturation:sum:Proxy/dcterms:conformsTo', 'saturation:average:Proxy/dcterms:conformsTo',
  'saturation:normalized:Proxy/dcterms:conformsTo', 'saturation:sum:Proxy/dcterms:hasFormat',
  'saturation:average:Proxy/dcterms:hasFormat', 'saturation:normalized:Proxy/dcterms:hasFormat',
  'saturation:sum:Proxy/dcterms:hasVersion', 'saturation:average:Proxy/dcterms:hasVersion',
  'saturation:normalized:Proxy/dcterms:hasVersion', 'saturation:sum:Proxy/dcterms:isFormatOf',
  'saturation:average:Proxy/dcterms:isFormatOf', 'saturation:normalized:Proxy/dcterms:isFormatOf',
  'saturation:sum:Proxy/dcterms:isReferencedBy', 'saturation:average:Proxy/dcterms:isReferencedBy',
  'saturation:normalized:Proxy/dcterms:isReferencedBy', 'saturation:sum:Proxy/dcterms:isReplacedBy',
  'saturation:average:Proxy/dcterms:isReplacedBy', 'saturation:normalized:Proxy/dcterms:isReplacedBy',
  'saturation:sum:Proxy/dcterms:isRequiredBy', 'saturation:average:Proxy/dcterms:isRequiredBy',
  'saturation:normalized:Proxy/dcterms:isRequiredBy', 'saturation:sum:Proxy/dcterms:isVersionOf',
  'saturation:average:Proxy/dcterms:isVersionOf', 'saturation:normalized:Proxy/dcterms:isVersionOf',
  'saturation:sum:Proxy/dcterms:references', 'saturation:average:Proxy/dcterms:references',
  'saturation:normalized:Proxy/dcterms:references', 'saturation:sum:Proxy/dcterms:replaces',
  'saturation:average:Proxy/dcterms:replaces', 'saturation:normalized:Proxy/dcterms:replaces',
  'saturation:sum:Proxy/dcterms:requires', 'saturation:average:Proxy/dcterms:requires',
  'saturation:normalized:Proxy/dcterms:requires', 'saturation:sum:Proxy/dcterms:tableOfContents',
  'saturation:average:Proxy/dcterms:tableOfContents', 'saturation:normalized:Proxy/dcterms:tableOfContents',
  'saturation:sum:Proxy/edm:currentLocation', 'saturation:average:Proxy/edm:currentLocation',
  'saturation:normalized:Proxy/edm:currentLocation', 'saturation:sum:Proxy/edm:hasMet',
  'saturation:average:Proxy/edm:hasMet', 'saturation:normalized:Proxy/edm:hasMet',
  'saturation:sum:Proxy/edm:hasType', 'saturation:average:Proxy/edm:hasType',
  'saturation:normalized:Proxy/edm:hasType', 'saturation:sum:Proxy/edm:incorporates',
  'saturation:average:Proxy/edm:incorporates', 'saturation:normalized:Proxy/edm:incorporates',
  'saturation:sum:Proxy/edm:isDerivativeOf', 'saturation:average:Proxy/edm:isDerivativeOf',
  'saturation:normalized:Proxy/edm:isDerivativeOf', 'saturation:sum:Proxy/edm:isRelatedTo',
  'saturation:average:Proxy/edm:isRelatedTo', 'saturation:normalized:Proxy/edm:isRelatedTo',
  'saturation:sum:Proxy/edm:isRepresentationOf', 'saturation:average:Proxy/edm:isRepresentationOf',
  'saturation:normalized:Proxy/edm:isRepresentationOf', 'saturation:sum:Proxy/edm:isSimilarTo',
  'saturation:average:Proxy/edm:isSimilarTo', 'saturation:normalized:Proxy/edm:isSimilarTo',
  'saturation:sum:Proxy/edm:isSuccessorOf', 'saturation:average:Proxy/edm:isSuccessorOf',
  'saturation:normalized:Proxy/edm:isSuccessorOf', 'saturation:sum:Proxy/edm:realizes',
  'saturation:average:Proxy/edm:realizes', 'saturation:normalized:Proxy/edm:realizes',
  'saturation:sum:Proxy/edm:wasPresentAt', 'saturation:average:Proxy/edm:wasPresentAt',
  'saturation:normalized:Proxy/edm:wasPresentAt', 'saturation:sum:Aggregation/edm:rights',
  'saturation:average:Aggregation/edm:rights', 'saturation:normalized:Aggregation/edm:rights',
  'saturation:sum:Aggregation/edm:provider', 'saturation:average:Aggregation/edm:provider',
  'saturation:normalized:Aggregation/edm:provider', 'saturation:sum:Aggregation/edm:dataProvider',
  'saturation:average:Aggregation/edm:dataProvider',
  'saturation:normalized:Aggregation/edm:dataProvider',
  'saturation:sum:Aggregation/dc:rights', 'saturation:average:Aggregation/dc:rights',
  'saturation:normalized:Aggregation/dc:rights', 'saturation:sum:Aggregation/edm:ugc',
  'saturation:average:Aggregation/edm:ugc',
  'saturation:normalized:Aggregation/edm:ugc', 'saturation:sum:Aggregation/edm:aggregatedCHO',
  'saturation:average:Aggregation/edm:aggregatedCHO',
  'saturation:normalized:Aggregation/edm:aggregatedCHO',
  'saturation:sum:Aggregation/edm:intermediateProvider',
  'saturation:average:Aggregation/edm:intermediateProvider',
  'saturation:normalized:Aggregation/edm:intermediateProvider',
  'saturation:sum:Place/dcterms:isPartOf',
  'saturation:average:Place/dcterms:isPartOf',
  'saturation:normalized:Place/dcterms:isPartOf',
  'saturation:sum:Place/dcterms:hasPart', 'saturation:average:Place/dcterms:hasPart',
  'saturation:normalized:Place/dcterms:hasPart', 'saturation:sum:Place/skos:prefLabel',
  'saturation:average:Place/skos:prefLabel', 'saturation:normalized:Place/skos:prefLabel',
  'saturation:sum:Place/skos:altLabel', 'saturation:average:Place/skos:altLabel',
  'saturation:normalized:Place/skos:altLabel', 'saturation:sum:Place/skos:note',
  'saturation:average:Place/skos:note', 'saturation:normalized:Place/skos:note',
  'saturation:sum:Agent/edm:begin', 'saturation:average:Agent/edm:begin',
  'saturation:normalized:Agent/edm:begin',
  'saturation:sum:Agent/edm:end', 'saturation:average:Agent/edm:end', 'saturation:normalized:Agent/edm:end',
  'saturation:sum:Agent/edm:hasMet', 'saturation:average:Agent/edm:hasMet',
  'saturation:normalized:Agent/edm:hasMet', 'saturation:sum:Agent/edm:isRelatedTo',
  'saturation:average:Agent/edm:isRelatedTo', 'saturation:normalized:Agent/edm:isRelatedTo',
  'saturation:sum:Agent/owl:sameAs', 'saturation:average:Agent/owl:sameAs',
  'saturation:normalized:Agent/owl:sameAs', 'saturation:sum:Agent/foaf:name',
  'saturation:average:Agent/foaf:name', 'saturation:normalized:Agent/foaf:name',
  'saturation:sum:Agent/dc:date', 'saturation:average:Agent/dc:date', 'saturation:normalized:Agent/dc:date',
  'saturation:sum:Agent/dc:identifier', 'saturation:average:Agent/dc:identifier',
  'saturation:normalized:Agent/dc:identifier',
  'saturation:sum:Agent/rdaGr2:dateOfBirth', 'saturation:average:Agent/rdaGr2:dateOfBirth',
  'saturation:normalized:Agent/rdaGr2:dateOfBirth', 'saturation:sum:Agent/rdaGr2:placeOfBirth',
  'saturation:average:Agent/rdaGr2:placeOfBirth', 'saturation:normalized:Agent/rdaGr2:placeOfBirth',
  'saturation:sum:Agent/rdaGr2:dateOfDeath', 'saturation:average:Agent/rdaGr2:dateOfDeath',
  'saturation:normalized:Agent/rdaGr2:dateOfDeath',
  'saturation:sum:Agent/rdaGr2:placeOfDeath', 'saturation:average:Agent/rdaGr2:placeOfDeath',
  'saturation:normalized:Agent/rdaGr2:placeOfDeath', 'saturation:sum:Agent/rdaGr2:dateOfEstablishment',
  'saturation:average:Agent/rdaGr2:dateOfEstablishment',
  'saturation:normalized:Agent/rdaGr2:dateOfEstablishment',
  'saturation:sum:Agent/rdaGr2:dateOfTermination', 'saturation:average:Agent/rdaGr2:dateOfTermination',
  'saturation:normalized:Agent/rdaGr2:dateOfTermination', 'saturation:sum:Agent/rdaGr2:gender',
  'saturation:average:Agent/rdaGr2:gender', 'saturation:normalized:Agent/rdaGr2:gender',
  'saturation:sum:Agent/rdaGr2:professionOrOccupation',
  'saturation:average:Agent/rdaGr2:professionOrOccupation',
  'saturation:normalized:Agent/rdaGr2:professionOrOccupation',
  'saturation:sum:Agent/rdaGr2:biographicalInformation',
  'saturation:average:Agent/rdaGr2:biographicalInformation',
  'saturation:normalized:Agent/rdaGr2:biographicalInformation',
  'saturation:sum:Agent/skos:prefLabel', 'saturation:average:Agent/skos:prefLabel',
  'saturation:normalized:Agent/skos:prefLabel', 'saturation:sum:Agent/skos:altLabel',
  'saturation:average:Agent/skos:altLabel', 'saturation:normalized:Agent/skos:altLabel',
  'saturation:sum:Agent/skos:note', 'saturation:average:Agent/skos:note',
  'saturation:normalized:Agent/skos:note', 'saturation:sum:Timespan/edm:begin',
  'saturation:average:Timespan/edm:begin',
  'saturation:normalized:Timespan/edm:begin', 'saturation:sum:Timespan/edm:end',
  'saturation:average:Timespan/edm:end', 'saturation:normalized:Timespan/edm:end',
  'saturation:sum:Timespan/dcterms:isPartOf', 'saturation:average:Timespan/dcterms:isPartOf',
  'saturation:normalized:Timespan/dcterms:isPartOf', 'saturation:sum:Timespan/dcterms:hasPart',
  'saturation:average:Timespan/dcterms:hasPart', 'saturation:normalized:Timespan/dcterms:hasPart',
  'saturation:sum:Timespan/edm:isNextInSequence', 'saturation:average:Timespan/edm:isNextInSequence',
  'saturation:normalized:Timespan/edm:isNextInSequence', 'saturation:sum:Timespan/owl:sameAs',
  'saturation:average:Timespan/owl:sameAs', 'saturation:normalized:Timespan/owl:sameAs',
  'saturation:sum:Timespan/skos:prefLabel', 'saturation:average:Timespan/skos:prefLabel',
  'saturation:normalized:Timespan/skos:prefLabel', 'saturation:sum:Timespan/skos:altLabel',
  'saturation:average:Timespan/skos:altLabel', 'saturation:normalized:Timespan/skos:altLabel',
  'saturation:sum:Timespan/skos:note', 'saturation:average:Timespan/skos:note',
  'saturation:normalized:Timespan/skos:note', 'saturation:sum:Concept/skos:broader',
  'saturation:average:Concept/skos:broader', 'saturation:normalized:Concept/skos:broader',
  'saturation:sum:Concept/skos:narrower', 'saturation:average:Concept/skos:narrower',
  'saturation:normalized:Concept/skos:narrower', 'saturation:sum:Concept/skos:related',
  'saturation:average:Concept/skos:related', 'saturation:normalized:Concept/skos:related',
  'saturation:sum:Concept/skos:broadMatch', 'saturation:average:Concept/skos:broadMatch',
  'saturation:normalized:Concept/skos:broadMatch', 'saturation:sum:Concept/skos:narrowMatch',
  'saturation:average:Concept/skos:narrowMatch', 'saturation:normalized:Concept/skos:narrowMatch',
  'saturation:sum:Concept/skos:relatedMatch', 'saturation:average:Concept/skos:relatedMatch',
  'saturation:normalized:Concept/skos:relatedMatch', 'saturation:sum:Concept/skos:exactMatch',
  'saturation:average:Concept/skos:exactMatch',
  'saturation:normalized:Concept/skos:exactMatch', 'saturation:sum:Concept/skos:closeMatch',
  'saturation:average:Concept/skos:closeMatch', 'saturation:normalized:Concept/skos:closeMatch',
  'saturation:sum:Concept/skos:notation', 'saturation:average:Concept/skos:notation',
  'saturation:normalized:Concept/skos:notation', 'saturation:sum:Concept/skos:inScheme',
  'saturation:average:Concept/skos:inScheme', 'saturation:normalized:Concept/skos:inScheme',
  'saturation:sum:Concept/skos:prefLabel', 'saturation:average:Concept/skos:prefLabel',
  'saturation:normalized:Concept/skos:prefLabel',
  'saturation:sum:Concept/skos:altLabel', 'saturation:average:Concept/skos:altLabel',
  'saturation:normalized:Concept/skos:altLabel', 'saturation:sum:Concept/skos:note',
  'saturation:average:Concept/skos:note', 'saturation:normalized:Concept/skos:note',
  'saturation_sum', 'saturation_average', 'saturation_normalized'
);
# add 'saturation_' prefix
saturation_fields <- gsub(':', '_', saturation_fields)
saturation_fields <- gsub('/', '_', saturation_fields)
# print(saturation_fields)
# saturation_fields <- paste0('saturation_', saturation_fields)
saturation_types <- paste(rep('n', length(saturation_fields)), collapse='')

all_fields <- c(id_fields, saturation_fields)
all_types <- paste(id_types, saturation_types, sep='')

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

if (opt$produceJson) {
  print(paste(path, "basic statistics"))
  stat_names <- saturation_fields #c(saturation_fields, top_fields)
  
  removable_stats <- c('nbr.val', 'nbr.null', 'nbr.na', 'sum')
  stats <- read.table(text = "1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1", 
                      colClasses = c('character'), col.names = c('dummy'))
  for (field in stat_names) {
    stat <- stat.desc(qa[qa[field] > -1, field], basic=TRUE)
    recMin <- head(qa[qa[field] == stat[['min']], 'id'], 1)
    recMax <- head(qa[qa[field] == stat[['max']], 'id'], 1)
    stat <- round(stat, digits=4)
    stat[['recMin']] <- recMin
    stat[['recMax']] <- recMax
    stat <- data.frame(stat[!names(stat) %in% removable_stats])
    colnames(stat) <- tolower(field)

    stats <- cbind(stats, stat)
  }
  stats <- stats[,colnames(stats) != 'dummy']
  stats <- data.frame(t(stats))
  exportJson <- toJSON(stats)
  write(exportJson, paste(jsonOutputDir, '/', id, ".saturation.json", sep=""))
  rm(stats)

  print(paste(path, "histograms"))
  histograms <- list()
  for (name in stat_names) {
    h <- hist(qa[,c(name)], plot = FALSE, breaks = 8)
    hist <- read.table(
      text = '',
      colClasses = c("character", "numeric", 'numeric'),
      col.names = c('label', 'count', 'density'),
      stringsAsFactors = FALSE)
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
  for (fieldName in saturation_fields) {
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

library(readr)
library(pastecs)
library(ggplot2)
library(grid)
library(gridExtra)
library(jsonlite)
library(plyr)
library(psych)
source("draw2.R")

jsonOutputDir <- 'json2'
startTime <- proc.time()

args <- commandArgs(trailingOnly=TRUE)

path <- getFile(args[1])
if (!file.exists(path)) {
  stop(paste('path:', path, "does not exist"))
}
print(paste('path:', path, "Start", "@", format(Sys.time(), "%H:%M:%OS3")))
parts <- unlist(strsplit(path, '/', fixed = TRUE))
file <- parts[length(parts)]
id <- getId(file)

jsonPath <- getPath(args[1])

drawCompletenessGraph <- as.logical(args[2])
if (is.na(drawCompletenessGraph)) {
  drawCompletenessGraph <- FALSE
}
print(paste(path, 'draw completeness graph:', drawCompletenessGraph))

drawEntropyGraph <- as.logical(args[3])
if (is.na(drawEntropyGraph)) {
  drawEntropyGraph <- FALSE
}
print(paste(path, 'draw entropy graph:', drawEntropyGraph))

doFrequencies <- as.logical(args[4])
if (is.na(doFrequencies)) {
  doFrequencies <- FALSE
}
print(paste(path, 'doFrequencies:', doFrequencies))

doCardinalities <- as.logical(args[5])
if (is.na(doCardinalities)) {
  doCardinalities <- TRUE
}
print(paste(path, 'doCardinalities:', doCardinalities))
print(paste(rep('=', 30), collapse=''))
doFrequencyTables <- TRUE

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

has_a_fields <- c(
  'ProvidedCHO_rdf_about',
  'Proxy_dc_title', 'Proxy_dcterms_alternative', 'Proxy_dc_description', 'Proxy_dc_creator', 'Proxy_dc_publisher',
  'Proxy_dc_contributor', 'Proxy_dc_type', 'Proxy_dc_identifier', 'Proxy_dc_language', 'Proxy_dc_coverage',
  'Proxy_dcterms_temporal', 'Proxy_dcterms_spatial', 'Proxy_dc_subject', 'Proxy_dc_date', 'Proxy_dcterms_created',
  'Proxy_dcterms_issued', 'Proxy_dcterms_extent', 'Proxy_dcterms_medium', 'Proxy_dcterms_provenance',
  'Proxy_dcterms_hasPart', 'Proxy_dcterms_isPartOf', 'Proxy_dc_format', 'Proxy_dc_source', 'Proxy_dc_rights', 
  'Proxy_dc_relation', 'Proxy_edm_isNextInSequence', 'Proxy_edm_type', 'Proxy_edm_europeanaProxy',
  'Proxy_edm_year', 'Proxy_edm_userTag', 'Proxy_ore_ProxyIn', 'Proxy_ore_ProxyFor', 'Proxy_dc_conformsTo',
  'Proxy_dcterms_hasFormat', 'Proxy_dcterms_hasVersion', 'Proxy_dcterms_isFormatOf', 'Proxy_dcterms_isReferencedBy',
  'Proxy_dcterms_isReplacedBy', 'Proxy_dcterms_isRequiredBy', 'Proxy_dcterms_isVersionOf', 'Proxy_dcterms_references',
  'Proxy_dcterms_replaces', 'Proxy_dcterms_requires', 'Proxy_dcterms_tableOfContents', 'Proxy_edm_currentLocation',
  'Proxy_edm_hasMet', 'Proxy_edm_hasType', 'Proxy_edm_incorporates', 'Proxy_edm_isDerivativeOf', 'Proxy_edm_isRelatedTo',
  'Proxy_edm_isRepresentationOf', 'Proxy_edm_isSimilarTo', 'Proxy_edm_isSuccessorOf', 'Proxy_edm_realizes',
  'Proxy_edm_wasPresentAt',
  'Aggregation_edm_rights', 'Aggregation_edm_provider', 'Aggregation_edm_dataProvider', 'Aggregation_edm_isShownAt',
  'Aggregation_edm_isShownBy', 'Aggregation_edm_object', 'Aggregation_edm_hasView', 'Aggregation_dc_rights',
  'Aggregation_edm_ugc', 'Aggregation_edm_aggregatedCHO', 'Aggregation_edm_intermediateProvider', 'Aggregation_rdf_about',
  'Place_wgs84_lat', 'Place_wgs84_long', 'Place_wgs84_alt', 'Place_dcterms_isPartOf', 'Place_wgs84_pos_lat_long',
  'Place_dcterms_hasPart', 'Place_owl_sameAs', 'Place_skos_prefLabel', 'Place_skos_altLabel', 'Place_skos_note',
  'Place_rdf_about',
  'Agent_rdf_about', 'Agent_edm_begin', 'Agent_edm_end', 'Agent_edm_hasMet', 'Agent_edm_isRelatedTo', 'Agent_owl_sameAs',
  'Agent_foaf_name', 'Agent_dc_date', 'Agent_dc_identifier', 'Agent_rdaGr2_dateOfBirth', 'Agent_rdaGr2_placeOfBirth',
  'Agent_rdaGr2_dateOfDeath', 'Agent_rdaGr2_placeOfDeath', 'Agent_rdaGr2_dateOfEstablishment',
  'Agent_rdaGr2_dateOfTermination', 'Agent_rdaGr2_gender', 'Agent_rdaGr2_professionOrOccupation',
  'Agent_rdaGr2_biographicalInformation', 'Agent_skos_prefLabel', 'Agent_skos_altLabel', 'Agent_skos_note',
  'Timespan_rdf_about', 'Timespan_edm_begin', 'Timespan_edm_end', 'Timespan_dcterms_isPartOf', 'Timespan_dcterms_hasPart',
  'Timespan_edm_isNextInSequence', 'Timespan_owl_sameAs', 'Timespan_skos_prefLabel', 'Timespan_skos_altLabel',
  'Timespan_skos_note',
  'Concept_rdf_about', 'Concept_skos_broader', 'Concept_skos_narrower', 'Concept_skos_related', 'Concept_skos_broadMatch',
  'Concept_skos_narrowMatch', 'Concept_skos_relatedMatch', 'Concept_skos_exactMatch', 'Concept_skos_closeMatch',
  'Concept_skos_notation', 'Concept_skos_inScheme', 'Concept_skos_prefLabel', 'Concept_skos_altLabel', 'Concept_skos_note'
);
has_a_types <- paste(rep('n', length(has_a_fields)), collapse='')

cardinality_fields <- c(
  'crd_ProvidedCHO_rdf_about',
  'crd_Proxy_dc_title', 'crd_Proxy_dcterms_alternative', 'crd_Proxy_dc_description', 'crd_Proxy_dc_creator',
  'crd_Proxy_dc_publisher', 'crd_Proxy_dc_contributor', 'crd_Proxy_dc_type', 'crd_Proxy_dc_identifier',
  'crd_Proxy_dc_language', 'crd_Proxy_dc_coverage', 'crd_Proxy_dcterms_temporal', 'crd_Proxy_dcterms_spatial',
  'crd_Proxy_dc_subject', 'crd_Proxy_dc_date', 'crd_Proxy_dcterms_created', 'crd_Proxy_dcterms_issued',
  'crd_Proxy_dcterms_extent', 'crd_Proxy_dcterms_medium', 'crd_Proxy_dcterms_provenance', 'crd_Proxy_dcterms_hasPart',
  'crd_Proxy_dcterms_isPartOf', 'crd_Proxy_dc_format', 'crd_Proxy_dc_source', 'crd_Proxy_dc_rights', 'crd_Proxy_dc_relation',
  'crd_Proxy_edm_isNextInSequence', 'crd_Proxy_edm_type', 'crd_Proxy_edm_europeanaProxy', 'crd_Proxy_edm_year',
  'crd_Proxy_edm_userTag', 'crd_Proxy_ore_ProxyIn', 'crd_Proxy_ore_ProxyFor', 'crd_Proxy_dc_conformsTo',
  'crd_Proxy_dcterms_hasFormat', 'crd_Proxy_dcterms_hasVersion', 'crd_Proxy_dcterms_isFormatOf',
  'crd_Proxy_dcterms_isReferencedBy', 'crd_Proxy_dcterms_isReplacedBy', 'crd_Proxy_dcterms_isRequiredBy',
  'crd_Proxy_dcterms_isVersionOf', 'crd_Proxy_dcterms_references', 'crd_Proxy_dcterms_replaces',
  'crd_Proxy_dcterms_requires', 'crd_Proxy_dcterms_tableOfContents', 'crd_Proxy_edm_currentLocation', 'crd_Proxy_edm_hasMet',
  'crd_Proxy_edm_hasType', 'crd_Proxy_edm_incorporates', 'crd_Proxy_edm_isDerivativeOf', 'crd_Proxy_edm_isRelatedTo',
  'crd_Proxy_edm_isRepresentationOf', 'crd_Proxy_edm_isSimilarTo', 'crd_Proxy_edm_isSuccessorOf', 'crd_Proxy_edm_realizes',
  'crd_Proxy_edm_wasPresentAt',
  'crd_Aggregation_edm_rights', 'crd_Aggregation_edm_provider', 'crd_Aggregation_edm_dataProvider',
  'crd_Aggregation_edm_isShownAt', 'crd_Aggregation_edm_isShownBy', 'crd_Aggregation_edm_object',
  'crd_Aggregation_edm_hasView', 'crd_Aggregation_dc_rights', 'crd_Aggregation_edm_ugc', 'crd_Aggregation_edm_aggregatedCHO',
  'crd_Aggregation_edm_intermediateProvider', 'crd_Aggregation_rdf_about',
  'crd_Place_wgs84_lat', 'crd_Place_wgs84_long', 'crd_Place_wgs84_alt', 'crd_Place_dcterms_isPartOf',
  'crd_Place_wgs84_pos_lat_long', 'crd_Place_dcterms_hasPart', 'crd_Place_owl_sameAs', 'crd_Place_skos_prefLabel',
  'crd_Place_skos_altLabel', 'crd_Place_skos_note', 'crd_Place_rdf_about',
  'crd_Agent_rdf_about', 'crd_Agent_edm_begin', 'crd_Agent_edm_end', 'crd_Agent_edm_hasMet', 'crd_Agent_edm_isRelatedTo',
  'crd_Agent_owl_sameAs', 'crd_Agent_foaf_name', 'crd_Agent_dc_date', 'crd_Agent_dc_identifier',
  'crd_Agent_rdaGr2_dateOfBirth', 'crd_Agent_rdaGr2_placeOfBirth', 'crd_Agent_rdaGr2_dateOfDeath',
  'crd_Agent_rdaGr2_placeOfDeath', 'crd_Agent_rdaGr2_dateOfEstablishment', 'crd_Agent_rdaGr2_dateOfTermination',
  'crd_Agent_rdaGr2_gender', 'crd_Agent_rdaGr2_professionOrOccupation', 'crd_Agent_rdaGr2_biographicalInformation',
  'crd_Agent_skos_prefLabel', 'crd_Agent_skos_altLabel', 'crd_Agent_skos_note',
  'crd_Timespan_rdf_about', 'crd_Timespan_edm_begin', 'crd_Timespan_edm_end', 'crd_Timespan_dcterms_isPartOf',
  'crd_Timespan_dcterms_hasPart', 'crd_Timespan_edm_isNextInSequence', 'crd_Timespan_owl_sameAs',
  'crd_Timespan_skos_prefLabel', 'crd_Timespan_skos_altLabel', 'crd_Timespan_skos_note',
  'crd_Concept_rdf_about', 'crd_Concept_skos_broader', 'crd_Concept_skos_narrower', 'crd_Concept_skos_related',
  'crd_Concept_skos_broadMatch', 'crd_Concept_skos_narrowMatch', 'crd_Concept_skos_relatedMatch',
  'crd_Concept_skos_exactMatch', 'crd_Concept_skos_closeMatch', 'crd_Concept_skos_notation', 'crd_Concept_skos_inScheme',
  'crd_Concept_skos_prefLabel', 'crd_Concept_skos_altLabel', 'crd_Concept_skos_note'
);
cardinality_types <- paste(rep('n', length(cardinality_fields)), collapse='')

problem_fields <- c(
  'long_subject', 'same_title_and_description', 'empty_string'
);
problem_labels <- c(
  'long subject', 'same title and description', 'empty string'
);
problem_types <- paste(rep('n', length(problem_fields)), collapse='')

entropy_fields <- c(
  'entropy_dc_title_sum', 'entropy_dc_title_avg',
  'entropy_dcterms_alternative_sum', 'entropy_dcterms_alternative_avg',
  'entropy_dc_description_sum', 'entropy_dc_description_avg'
);
entropy_labels <- c(
  'dc:title cumulative uniqueness', 'dc:title avarage uniqueness',
  'dcterms:alternative cumulative uniqueness', 'dcterms:alternative average uniqueness',
  'dc:description cumulative uniqueness', 'dc:description average uniqueness'
);
entropy_types <- paste(rep('n', length(entropy_fields)), collapse='')

# all_fields <- c(id_fields, completeness_fields)
# all_fields <- c(id_fields, completeness_fields, has_a_fields, cardinality_fields, problem_fields, entropy_fields)
all_fields <- c(id_fields, completeness_fields, has_a_fields, cardinality_fields, problem_fields)
# all_types <- paste(id_types, completeness_types, sep='')
# all_types <- paste(id_types,  completeness_types,  has_a_types, cardinality_types, problem_types, entropy_types, sep='')
all_types <- paste(id_types,  completeness_types,  has_a_types, cardinality_types, problem_types, sep='')

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

collector <- list()

count <- c(sum)
names(count) <- c('count')
count <- data.frame(count)
collector[["count"]][['count']] <- sum
exportJson <- toJSON(count)
write(exportJson, paste(jsonOutputDir, '/', id, ".count.json", sep=""))
rm(count)

# STARTS field existency process
if (doFrequencies) {
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
if (doCardinalities) {
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

if (doFrequencyTables) {
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

print(paste(path, "histograms"))
histograms <- list()
for (name in stat_names) {
  h <- hist(qa[,c(name)], plot = FALSE, breaks = 8)
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

prepareImageDirectory(id)
if (drawCompletenessGraph == TRUE) {
  for (i in 1:length(completeness_labels)) {
    print(paste(path, "drawing subdimension", completeness_labels[i]))
    draw(qa, completeness_fields[i], completeness_labels[i])
  }

  for (i in 1:length(problem_fields)) {
    print(paste(path, "drawing problem catalog", problem_labels[i]))
    draw(qa, problem_fields[i], problem_labels[i])
  }
  warnings()
}

################################
# drawing entropy fields
################################

if (drawEntropyGraph == TRUE) {
  for (i in 1:length(entropy_fields)) {
    print(paste(path, "drawing", entropy_labels[i]))
    draw(qa, entropy_fields[i], entropy_labels[i])
  }
  warnings()
}

if (doCardinalities == TRUE) {
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
warnings()
# stopQuietly()

duration <- (proc.time() - startTime)
print(paste(path, "Finished", "@", format(Sys.time(), "%H:%M:%OS3"), 
            sprintf("time: %s, user: %s, sys: %s", 
                    duration['elapsed'][[1]], duration['user.self'][[1]], duration['sys.self'][[1]])))
rm(list=ls())

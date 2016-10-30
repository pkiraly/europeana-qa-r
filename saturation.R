library(readr)
library(pastecs)
library(ggplot2)
library(grid)
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
  "proxy_dc_title", "proxy_dcterms_alternative", "proxy_dc_description", "proxy_dc_creator",
  "proxy_dc_publisher", "proxy_dc_contributor", "proxy_dc_type", "proxy_dc_identifier",
  "proxy_dc_language", "proxy_dc_coverage", "proxy_dcterms_temporal", "proxy_dcterms_spatial",
  "proxy_dc_subject", "proxy_dc_date", "proxy_dcterms_created", "proxy_dcterms_issued",
  "proxy_dcterms_extent", "proxy_dcterms_medium", "proxy_dcterms_provenance", "proxy_dcterms_hasPart",
  "proxy_dcterms_isPartOf", "proxy_dc_format", "proxy_dc_source", "proxy_dc_rights",
  "proxy_dc_relation", "proxy_edm_europeanaProxy", "proxy_edm_year", "proxy_edm_userTag",
  "proxy_ore_ProxyIn", "proxy_ore_ProxyFor", "proxy_dc_conformsTo", "proxy_dcterms_hasFormat",
  "proxy_dcterms_hasVersion", "proxy_dcterms_isFormatOf", "proxy_dcterms_isReferencedBy",
  "proxy_dcterms_isReplacedBy", "proxy_dcterms_isRequiredBy", "proxy_dcterms_isVersionOf",
  "proxy_dcterms_references", "proxy_dcterms_replaces", "proxy_dcterms_requires",
  "proxy_dcterms_tableOfContents", "proxy_edm_currentLocation", "proxy_edm_hasMet",
  "proxy_edm_hasType", "proxy_edm_incorporates", "proxy_edm_isDerivativeOf", "proxy_edm_isRelatedTo",
  "proxy_edm_isRepresentationOf", "proxy_edm_isSimilarTo", "proxy_edm_isSuccessorOf",
  "proxy_edm_realizes", "proxy_edm_wasPresentAt",
  "aggregation_edm_rights", "aggregation_edm_provider", "aggregation_edm_dataProvider",
  "aggregation_dc_rights", "aggregation_edm_ugc", "aggregation_edm_aggregatedCHO",
  "aggregation_edm_intermediateProvider", "place_dcterms_isPartOf", "place_dcterms_hasPart",
  "place_skos_prefLabel", "place_skos_altLabel", "place_skos_note", "agent_edm_begin",
  "agent_edm_end", "agent_edm_hasMet", "agent_edm_isRelatedTo", "agent_owl_sameAs", "agent_foaf_name",
  "agent_dc_date", "agent_dc_identifier", "agent_rdaGr2_dateOfBirth", "agent_rdaGr2_placeOfBirth",
  "agent_rdaGr2_dateOfDeath", "agent_rdaGr2_placeOfDeath", "agent_rdaGr2_dateOfEstablishment",
  "agent_rdaGr2_dateOfTermination", "agent_rdaGr2_gender", "agent_rdaGr2_professionOrOccupation",
  "agent_rdaGr2_biographicalInformation", "agent_skos_prefLabel", "agent_skos_altLabel",
  "agent_skos_note",
  "timespan_edm_begin", "timespan_edm_end", "timespan_dcterms_isPartOf", "timespan_dcterms_hasPart",
  "timespan_edm_isNextInSequence", "timespan_owl_sameAs", "timespan_skos_prefLabel",
  "timespan_skos_altLabel", "timespan_skos_note",
  "concept_skos_broader", "concept_skos_narrower", "concept_skos_related", "concept_skos_broadMatch",
  "concept_skos_narrowMatch", "concept_skos_relatedMatch", "concept_skos_exactMatch",
  "concept_skos_closeMatch", "concept_skos_notation", "concept_skos_inScheme", "concept_skos_prefLabel",
  "concept_skos_altLabel", "concept_skos_note");
# add 'saturation_' prefix
saturation_fields <- paste0('saturation_', saturation_fields)
saturation_types <- paste(rep('n', length(saturation_fields)), collapse='')

all_fields <- c(id_fields, saturation_fields)
all_types <- paste(id_types, saturation_types, sep='')

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

if (opt$produceJson) {
  print(paste(path, "basic statistics"))
  stat_names <- c(saturation_fields)
  stats <- round(stat.desc(qa[,stat_names], basic=TRUE), digits=4)
  names(stats) <- tolower(names(stats))
  
  stats <- stats[!(rownames(stats) %in% c("nbr.val", "nbr.null", "nbr.na", "sum")),]
  stats <- data.frame(t(stats))
  
  for (name in stat_names) {
    stats <- setMinMaxRecId(stats, qa, name)
  }

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
    label <- sub('_', ':',
                 sub('aggregation_', 'Aggregation/',
                     sub('proxy_', 'Proxy/',
                         sub('agent_', 'Agent/',
                             sub('place_', 'Place/',
                                 sub('timespan_', 'Timespan/',
                                     sub('concept_', 'Concept/',
                                         sub('saturation_', '', fieldName))))))))
    print(paste(path, "drawing saturation of", label))
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

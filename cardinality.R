library(readr)
library(pastecs)
library(ggplot2)
library(grid)
library(gridExtra)
library(jsonlite)
source("draw.R")

args <- commandArgs(trailingOnly=TRUE)

path <- getFile(args[1])
print(paste('path:', path))
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

#id_fields <- c('collection')
id_fields <- c('provider', 'collection', 'id')
id_types <- paste(rep('c', length(id_fields)), collapse='')

completeness_fields <- c(
  'total', 'mandatory', 'descriptiveness', 'searchability',
  'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
  'multilinguality'
)
completeness_types <- paste(rep('n', length(completeness_fields)), collapse='')

has_a_fields <- c('identifier', 'proxy_dc_title', 'proxy_dcterms_alternative', 
               'proxy_dc_description', 'proxy_dc_creator', 'proxy_dc_publisher', 'proxy_dc_contributor',
               'proxy_dc_type', 'proxy_dc_identifier', 'proxy_dc_language', 'proxy_dc_coverage',
               'proxy_dcterms_temporal', 'proxy_dcterms_spatial', 'proxy_dc_subject', 'proxy_dc_date',
               'proxy_dcterms_created', 'proxy_dcterms_issued', 'proxy_dcterms_extent', 'proxy_dcterms_medium',
               'proxy_dcterms_provenance', 'proxy_dcterms_hasPart', 'proxy_dcterms_isPartOf', 'proxy_dc_format',
               'proxy_dc_source', 'proxy_dc_rights', 'proxy_dc_relation', 'proxy_edm_isNextInSequence',
               'proxy_edm_type', 'aggregation_edm_rights', 'aggregation_edm_provider',
               'aggregation_edm_dataProvider', 'aggregation_edm_isShownAt', 'aggregation_edm_isShownBy',
               'aggregation_edm_object', 'aggregation_edm_hasView');
has_a_types <- paste(rep('n', length(has_a_fields)), collapse='')

cardinality_fields <- c('crd_identifier', 'crd_proxy_dc_title', 'crd_proxy_dcterms_alternative', 
               'crd_proxy_dc_description', 'crd_proxy_dc_creator', 'crd_proxy_dc_publisher', 'crd_proxy_dc_contributor',
               'crd_proxy_dc_type', 'crd_proxy_dc_identifier', 'crd_proxy_dc_language', 'crd_proxy_dc_coverage',
               'crd_proxy_dcterms_temporal', 'crd_proxy_dcterms_spatial', 'crd_proxy_dc_subject', 'crd_proxy_dc_date',
               'crd_proxy_dcterms_created', 'crd_proxy_dcterms_issued', 'crd_proxy_dcterms_extent', 'crd_proxy_dcterms_medium',
               'crd_proxy_dcterms_provenance', 'crd_proxy_dcterms_hasPart', 'crd_proxy_dcterms_isPartOf', 'crd_proxy_dc_format',
               'crd_proxy_dc_source', 'crd_proxy_dc_rights', 'crd_proxy_dc_relation', 'crd_proxy_edm_isNextInSequence',
               'crd_proxy_edm_type', 'crd_aggregation_edm_rights', 'crd_aggregation_edm_provider',
               'crd_aggregation_edm_dataProvider', 'crd_aggregation_edm_isShownAt', 'crd_aggregation_edm_isShownBy',
               'crd_aggregation_edm_object', 'crd_aggregation_edm_hasView');
cardinality_types <- paste(rep('n', length(cardinality_fields)), collapse='')

problem_fields <- c('long_subject', 'same_title_and_description', 'empty_string');
problem_types <- paste(rep('n', length(problem_fields)), collapse='')

entropy_fields <- c('entropy_dc_title_sum', 'entropy_dc_title_avg',
               'entropy_dcterms_alternative_sum', 'entropy_dcterms_alternative_avg',
               'entropy_dc_description_sum', 'entropy_dc_description_avg');
entropy_types <- paste(rep('n', length(entropy_fields)), collapse='')

# all_fields <- c(id_fields, completeness_fields)
all_fields <- c(id_fields, completeness_fields, has_a_fields, cardinality_fields, problem_fields, entropy_fields)
# all_types <- paste(id_types, completeness_types, sep='')
all_types <- paste(id_types,  completeness_types,  has_a_types, cardinality_types, problem_types, entropy_types, sep='')

qa <- read_csv(path, col_types = all_types, col_names = all_fields);

sum <- nrow(qa)

print(paste(path, 'total records:', sum))

# STARTS field existency process
print(paste(path, "calculate field cardinality"))
cardinalities <- read.table(text = '', 
                          colClasses = c("character", "numeric", 'numeric'), 
                          col.names = c('field', 'count', 'sum', 'mean', 'median'))

for (fieldName in cardinality_fields) {
  # str(qa[,fieldName])
  xs <- qa[,fieldName];
  fieldCount <- length(xs[xs > 0])
  fieldSum <- sum(xs)
  fieldMean <- mean(xs)
  fieldMedian <- median(xs)
  # fieldVariance <- variance(qa[,fieldName])
  cardinalities <- rbind(cardinalities, data.frame(
    field = substring(fieldName, 5), count = fieldCount, sum = fieldSum, mean = fieldMean, median = fieldMedian))
}
# collector[["cardinalities"]] <- cardinalities
exportJson <- toJSON(cardinalities)
write(exportJson, paste('json/', id, ".cardinality.json", sep=""))
rm(cardinalities)
# ENDS field existency process

print(paste(path, "Finished"))
rm(list=ls())

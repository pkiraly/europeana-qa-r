library(readr)
library(dplyr)
library(psych)
library(pastecs)
library(jsonlite)
source("draw.R")

file <- getFile(commandArgs(trailingOnly=TRUE))
id <- getId(file)

setMinMaxRecId <- function(stats, recordQuality, field) {
  stats[field,"recMin"] <- head(recordQuality[recordQuality[field] == min(recordQuality[field]),2], 1)
  stats[field,"recMax"] <- head(recordQuality[recordQuality[field] == max(recordQuality[field]),2], 1)
  return(stats)
}

col_names <- c('collection', 'id', 'total', 'mandatory', 'descriptiveness', 'searchability', 
  'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
  'multilinguality')
recordQuality <- read_csv(paste("data/", file, ".csv", sep=""), 
                          col_types = "ccnnnnnnnnnn", col_names = col_names
);

# describe(recordQuality[,c('total', 'mandatory', 'descriptiveness', 'searchability', 
#                           'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
#                           'multilinguality')])

stats <- round(stat.desc(recordQuality[,c('total', 'mandatory', 'descriptiveness', 'searchability', 
                           'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
                           'multilinguality')], basic=TRUE), digits=3)

stats <- stats[!(rownames(stats) %in% c("nbr.val", "nbr.null", "nbr.na", "sum")),]
stats <- data.frame(t(stats))

for (name in col_names) {
  if (name != 'collection' && name != 'id') {
    stats <- setMinMaxRecId(stats, recordQuality, name)
  }
}

exportJson <- toJSON(stats)
write(exportJson, paste(id, ".json", sep=""))


library(readr)
library(dplyr)
library(psych)
library(pastecs)
library(jsonlite)
source("draw.R")

file <- getFile(commandArgs(trailingOnly=TRUE))
id <- getId(file)

recordQuality <- read_csv(paste("data/", file, ".csv", sep=""), 
                          col_types = "ccnnnnnnnnnn", col_names = c(
                            'collection', 'id', 'total', 'mandatory', 'descriptiveness', 'searchability', 
                            'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
                            'multilinguality')
);

# describe(recordQuality[,c('total', 'mandatory', 'descriptiveness', 'searchability', 
#                           'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
#                           'multilinguality')])

stats <- round(stat.desc(recordQuality[,c('total', 'mandatory', 'descriptiveness', 'searchability', 
                           'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
                           'multilinguality')], basic=TRUE), digits=3)

stats <- stats[!(rownames(stats) %in% c("nbr.val", "nbr.null", "nbr.na", "sum")),]
stats <- data.frame(t(stats))

exportJson <- toJSON(stats)
write(exportJson, paste(id, ".json", sep=""))

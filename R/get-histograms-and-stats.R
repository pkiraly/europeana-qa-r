library(readr)
library(pastecs)
library(ggplot2)
library(grid)
library(gridExtra)
library(jsonlite)
source("draw.R")

args <- commandArgs(trailingOnly=TRUE)

path <- getFile(args[1])
if (!file.exists(path)) {
  stop(paste('path:', path, "does not exist"))
}
print(paste('path:', path, "Start", "@", format(Sys.time(), "%H:%M:%OS3")))
parts <- unlist(strsplit(path, '/', fixed = TRUE))
file <- parts[length(parts)]
id <- getId(file)
print(paste('id:', id))

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

#id_fields <- c('collection')
id_fields <- c( 'id', 'provider', 'collection')
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
  'aggregation_edm_object', 'aggregation_edm_hasView'
);
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

collector <- list()

count <- c(sum)
names(count) <- c('count')
count <- data.frame(count)
collector[["count"]][['count']] <- sum
exportJson <- toJSON(count)
write(exportJson, paste('json/', id, ".count.json", sep=""))
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

    frequencies <- rbind(frequencies, data.frame(field = fieldName, count = countVal, frequency = freqVal)) 
  }
  collector[["frequencies"]] <- frequencies
  exportJson <- toJSON(frequencies)
  write(exportJson, paste('json/', id, ".freq.json", sep=""))
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
      field = substring(fieldName, 5), count = fieldCount, sum = fieldSum, mean = fieldMean, median = fieldMedian))
  }
  collector[["cardinalities"]] <- cardinalities
  exportJson <- toJSON(cardinalities)
  write(exportJson, paste('json/', id, ".cardinality.json", sep=""))
  rm(cardinalities)
}
# ENDS field cardinality process

print(paste(path, "basic statistics"))
stat_names <- c(completeness_fields, cardinality_fields, problem_fields, entropy_fields)
stats <- round(stat.desc(qa[,stat_names], basic=TRUE), digits=4)

stats <- stats[!(rownames(stats) %in% c("nbr.val", "nbr.null", "nbr.na", "sum")),]
stats <- data.frame(t(stats))

for (name in stat_names) {
  stats <- setMinMaxRecId(stats, qa, name)
}
collector[["stats"]] <- stats
exportJson <- toJSON(stats)
write(exportJson, paste('json/', id, ".json", sep=""))
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
  histograms[[name]] <- hist
}
collector[["histograms"]] <- histograms
exportJson <- toJSON(histograms)
write(exportJson, paste('json/', id, ".hist.json", sep=""))
rm(histograms)

exportJson <- toJSON(collector)
write(exportJson, paste('json/', id, ".collector.json", sep=""))
rm(collector)

if (drawCompletenessGraph == TRUE) {
  # total
  print(paste(path, "drawing total"))
  bar <- ggplot(qa, aes(total)) + 
    theme(legend.position = "none") +
    geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) + #
    labs(title="Histogram", x="Every fields", y="Density (%)") +
    stat_function(fun=dnorm, args=list(mean=mean(qa$total, na.rm = TRUE), sd=sd(qa$total, na.rm = TRUE)), colour="black", size=1) +
    scale_x_continuous(limits=c(0,1)) + 
    scale_fill_brewer(palette="RdBu") + theme_minimal()

  box <- ggplot(qa, aes(x=collection, y=total)) +
    geom_boxplot() + labs(title="Boxplot", x="Collection", y="Every fields") +
    # geom_jitter(shape=16, position=position_jitter(0.2)) + 
    scale_fill_brewer(palette="RdBu") + theme_minimal() +
    scale_y_continuous(limits=c(0,1))

  qq <- qplot(sample = qa$total) + stat_qq() +
    labs(title="Quantile plot", y="Every fields") +
    scale_fill_brewer(palette="RdBu") + theme_minimal() +
    scale_y_continuous(limits=c(0,1))

  saveImage(id, 'total', bar, box, qq)
}

# mandatory
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing mandatory"))
bar <- ggplot(qa, aes(mandatory)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Mandatory fields", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$mandatory, na.rm = TRUE), sd=sd(qa$mandatory, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=mandatory)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Mandatory fields") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$mandatory) + stat_qq() +
  labs(title="Quantile plot", y="Mandatory fields") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'mandatory', bar, box, qq)
}

# descriptiveness
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing descriptiveness"))
bar <- ggplot(qa, aes(descriptiveness)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Descriptiveness", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$descriptiveness, na.rm = TRUE), sd=sd(qa$descriptiveness, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=descriptiveness)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Descriptiveness") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$descriptiveness) + stat_qq() +
  labs(title="Quantile plot", y="Descriptiveness") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'descriptiveness', bar, box, qq)
}

# searchability
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing searchability"))
bar <- ggplot(qa, aes(searchability)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Searchability", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$searchability, na.rm = TRUE), sd=sd(qa$searchability, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=searchability)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Searchability") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$searchability) + stat_qq() +
  labs(title="Quantile plot", y="Searchability") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'searchability', bar, box, qq)
}

# contextualization
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing contextualization"))
bar <- ggplot(qa, aes(contextualization)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Contextualization", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$contextualization, na.rm = TRUE), sd=sd(qa$contextualization, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=contextualization)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Contextualization") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$contextualization) + stat_qq() +
  labs(title="Quantile plot", y="Contextualization") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'contextualization', bar, box, qq)
}

# identification
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing identification"))
bar <- ggplot(qa, aes(identification)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Identification", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$identification, na.rm = TRUE), sd=sd(qa$identification, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=identification)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Identification") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$identification) + stat_qq() +
  labs(title="Quantile plot", y="Identification") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'identification', bar, box, qq)
}

# browsing
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing browsing"))
bar <- ggplot(qa, aes(browsing)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Browsing", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$browsing, na.rm = TRUE), sd=sd(qa$browsing, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=browsing)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Browsing") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$browsing) + stat_qq() +
  labs(title="Quantile plot", y="Browsing") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'browsing', bar, box, qq)
}

# viewing
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing viewing"))
bar <- ggplot(qa, aes(viewing)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Viewing", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$viewing, na.rm = TRUE), sd=sd(qa$viewing, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=viewing)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Viewing") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$viewing) + stat_qq() +
  labs(title="Quantile plot", y="Viewing") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'viewing', bar, box, qq)
}

# 'reusability'
if (drawCompletenessGraph == TRUE) {
print(paste(path, "drawing 'reusability'"))
bar <- ggplot(qa, aes(reusability)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Reusability", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$reusability, na.rm = TRUE), sd=sd(qa$reusability, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=reusability)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Reusability") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$reusability) + stat_qq() +
  labs(title="Quantile plot", y="Reusability") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'reusability', bar, box, qq)
}

# multilinguality
if (drawCompletenessGraph == TRUE) {
  print(paste(path, "drawing multilinguality"))
  bar <- ggplot(qa, aes(multilinguality)) + 
    theme(legend.position = "none") +
    geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
    labs(title="Histogram", x="Multilinguality", y="Density (%)") +
    stat_function(fun=dnorm, args=list(mean=mean(qa$multilinguality, na.rm = TRUE), sd=sd(qa$multilinguality, na.rm = TRUE)), colour="black", size=1) +
    scale_x_continuous(limits=c(0,1)) + 
    scale_fill_brewer(palette="RdBu") + theme_minimal()

  box <- ggplot(qa, aes(x=collection, y=multilinguality)) +
    geom_boxplot() + labs(title="Boxplot", x="Collection", y="Multilinguality") +
    # geom_jitter(shape=16, position=position_jitter(0.2)) + 
    scale_fill_brewer(palette="RdBu") + theme_minimal() +
    scale_y_continuous(limits=c(0,1))

  qq <- qplot(sample = qa$multilinguality) + stat_qq() +
    labs(title="Quantile plot", y="Multilinguality") +
    scale_fill_brewer(palette="RdBu") + theme_minimal() +
    scale_y_continuous(limits=c(0,1))

  saveImage(id, 'multilinguality', bar, box, qq)
}

################################
# drawing entropy fields
################################

# entropy_dc_title_sum
if (drawEntropyGraph == TRUE) {
print(paste(path, "drawing entropy_dc_title_sum"))
bar <- ggplot(qa, aes(entropy_dc_title_sum)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="dc:title cumulative", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$entropy_dc_title_sum, na.rm = TRUE), sd=sd(qa$entropy_dc_title_sum, na.rm = TRUE)), colour="black", size=1) +
  # scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=entropy_dc_title_sum)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="dc:title cumulative") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$entropy_dc_title_sum) + stat_qq() +
  labs(title="Quantile plot", y="dc:title cumulative") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

saveImage(id, 'entropy_dc_title_sum', bar, box, qq)
}

# entropy_dc_title_avg
if (drawEntropyGraph == TRUE) {
print(paste(path, "drawing entropy_dc_title_avg"))
bar <- ggplot(qa, aes(entropy_dc_title_avg)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="dc:title average", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$entropy_dc_title_avg, na.rm = TRUE), sd=sd(qa$entropy_dc_title_avg, na.rm = TRUE)), colour="black", size=1) +
  # scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=entropy_dc_title_avg)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="dc:title average") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$entropy_dc_title_avg) + stat_qq() +
  labs(title="Quantile plot", y="dc:title average") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

saveImage(id, 'entropy_dc_title_avg', bar, box, qq)
}

# entropy_dcterms_alternative_sum
if (drawEntropyGraph == TRUE) {
print(paste(path, "drawing entropy_dcterms_alternative_sum"))
bar <- ggplot(qa, aes(entropy_dcterms_alternative_sum)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="dcterms:alternative cumulative", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$entropy_dcterms_alternative_sum, na.rm = TRUE), sd=sd(qa$entropy_dcterms_alternative_sum, na.rm = TRUE)), colour="black", size=1) +
  # scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=entropy_dcterms_alternative_sum)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="dcterms:alternative cumulative") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  #scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$entropy_dcterms_alternative_sum) + stat_qq() +
  labs(title="Quantile plot", y="dcterms:alternative cumulative") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

saveImage(id, 'entropy_dcterms_alternative_sum', bar, box, qq)
}

# entropy_dcterms_alternative_avg
if (drawEntropyGraph == TRUE) {
print(paste(path, "drawing entropy_dcterms_alternative_avg"))
bar <- ggplot(qa, aes(entropy_dcterms_alternative_avg)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="dcterms:alternative average", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$entropy_dcterms_alternative_avg, na.rm = TRUE), sd=sd(qa$entropy_dcterms_alternative_avg, na.rm = TRUE)), colour="black", size=1) +
  # scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=entropy_dcterms_alternative_avg)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="dcterms:alternative average") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$entropy_dcterms_alternative_avg) + stat_qq() +
  labs(title="Quantile plot", y="dcterms:alternative average") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

saveImage(id, 'entropy_dcterms_alternative_avg', bar, box, qq)
}

# entropy_dc_description_sum
if (drawEntropyGraph == TRUE) {
print(paste(path, "drawing entropy_dc_description_sum"))
bar <- ggplot(qa, aes(entropy_dc_description_sum)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="dc:description cumulative", y="Density (%)") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$entropy_dc_description_sum, na.rm = TRUE), sd=sd(qa$entropy_dc_description_sum, na.rm = TRUE)), colour="black", size=1) +
  # scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=entropy_dc_description_sum)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="dc:description cumulative") +
  # geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$entropy_dc_description_sum) + stat_qq() +
  labs(title="Quantile plot", y="dc:description cumulative") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()
  # scale_y_continuous(limits=c(0,1))

saveImage(id, 'entropy_dc_description_sum', bar, box, qq)
}

# entropy_dc_description_avg
if (drawEntropyGraph == TRUE) {
  print(paste(path, "drawing entropy_dc_description_avg"))
  bar <- ggplot(qa, aes(entropy_dc_description_avg)) + 
    theme(legend.position = "none") +
    geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
    labs(title="Histogram", x="dc:description average", y="Density (%)") +
    stat_function(fun=dnorm, args=list(mean=mean(qa$entropy_dc_description_avg, na.rm = TRUE), sd=sd(qa$entropy_dc_description_avg, na.rm = TRUE)), colour="black", size=1) +
    # scale_x_continuous(limits=c(0,1)) + 
    scale_fill_brewer(palette="RdBu") + theme_minimal()

  box <- ggplot(qa, aes(x=collection, y=entropy_dc_description_avg)) +
    geom_boxplot() + labs(title="Boxplot", x="Collection", y="dc:description average") +
    # geom_jitter(shape=16, position=position_jitter(0.2)) + 
    scale_fill_brewer(palette="RdBu") + theme_minimal()
    # scale_y_continuous(limits=c(0,1))

  qq <- qplot(sample = qa$entropy_dc_description_avg) + stat_qq() +
    labs(title="Quantile plot", y="dc:description average") +
    scale_fill_brewer(palette="RdBu") + theme_minimal()
    # scale_y_continuous(limits=c(0,1))

  saveImage(id, 'entropy_dc_description_avg', bar, box, qq)
}

print(paste(path, "Finished", "@", format(Sys.time(), "%H:%M:%OS3")))
rm(list=ls())

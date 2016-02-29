library(readr)
library(pastecs)
library(ggplot2)
library(grid)
library(gridExtra)
library(jsonlite)
source("draw.R")

path <- getFile(commandArgs(trailingOnly=TRUE))
parts <- unlist(strsplit(path, '/', fixed = TRUE))
file <- parts[length(parts)]
id <- getId(file)
jsonPath <- getPath(commandArgs(trailingOnly=TRUE))

col_names <- c('collection', 'id', 'total', 'mandatory', 'descriptiveness', 'searchability', 
               'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
               'multilinguality')
qa <- read_csv(path, col_types = "ccnnnnnnnnnn", col_names = col_names);

stats <- round(
          stat.desc(
            qa[,
              c('total', 'mandatory', 'descriptiveness', 'searchability', 
                'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
                'multilinguality')], basic=TRUE), digits=3)

stats <- stats[!(rownames(stats) %in% c("nbr.val", "nbr.null", "nbr.na", "sum")),]
stats <- data.frame(t(stats))

for (name in col_names) {
  if (name != 'collection' && name != 'id') {
    stats <- setMinMaxRecId(stats, qa, name)
  }
}

exportJson <- toJSON(stats)
write(exportJson, paste('json/', id, ".json", sep=""))
rm(stats)

# total
bar <- ggplot(qa, aes(total)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) + #
  labs(title="Histogram", x="Total", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$total, na.rm = TRUE), sd=sd(qa$total, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=total)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Total") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$total) + stat_qq() +
  labs(title="Quantile plot", y="Total") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'total', bar, box, qq)

# mandatory
bar <- ggplot(qa, aes(mandatory)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Mandatory fields", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$mandatory, na.rm = TRUE), sd=sd(qa$mandatory, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=mandatory)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Mandatory fields") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$mandatory) + stat_qq() +
  labs(title="Quantile plot", y="Mandatory fields") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'mandatory', bar, box, qq)

# descriptiveness
bar <- ggplot(qa, aes(descriptiveness)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Descriptiveness", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$descriptiveness, na.rm = TRUE), sd=sd(qa$descriptiveness, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=descriptiveness)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Descriptiveness") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$descriptiveness) + stat_qq() +
  labs(title="Quantile plot", y="Descriptiveness") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'descriptiveness', bar, box, qq)

# searchability
bar <- ggplot(qa, aes(searchability)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Searchability", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$searchability, na.rm = TRUE), sd=sd(qa$searchability, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=searchability)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Searchability") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$searchability) + stat_qq() +
  labs(title="Quantile plot", y="Searchability") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'searchability', bar, box, qq)

# contextualization
bar <- ggplot(qa, aes(contextualization)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Contextualization", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$contextualization, na.rm = TRUE), sd=sd(qa$contextualization, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=contextualization)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Contextualization") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$contextualization) + stat_qq() +
  labs(title="Quantile plot", y="Contextualization") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'contextualization', bar, box, qq)

# identification
bar <- ggplot(qa, aes(identification)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Identification", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$identification, na.rm = TRUE), sd=sd(qa$identification, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=identification)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Identification") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$identification) + stat_qq() +
  labs(title="Quantile plot", y="Identification") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'identification', bar, box, qq)

# browsing
bar <- ggplot(qa, aes(browsing)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Browsing", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$browsing, na.rm = TRUE), sd=sd(qa$browsing, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=browsing)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Browsing") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$browsing) + stat_qq() +
  labs(title="Quantile plot", y="Browsing") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'browsing', bar, box, qq)

# viewing
bar <- ggplot(qa, aes(viewing)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Viewing", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$viewing, na.rm = TRUE), sd=sd(qa$viewing, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=viewing)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Viewing") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$viewing) + stat_qq() +
  labs(title="Quantile plot", y="Viewing") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'viewing', bar, box, qq)

# 'reusability',
bar <- ggplot(qa, aes(reusability)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Reusability", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$reusability, na.rm = TRUE), sd=sd(qa$reusability, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=reusability)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Reusability") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$reusability) + stat_qq() +
  labs(title="Quantile plot", y="Reusability") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'reusability', bar, box, qq)

# multilinguality
bar <- ggplot(qa, aes(multilinguality)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) +
  labs(title="Histogram", x="Multilinguality", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$multilinguality, na.rm = TRUE), sd=sd(qa$multilinguality, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=multilinguality)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Multilinguality") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$multilinguality) + stat_qq() +
  labs(title="Quantile plot", y="Multilinguality") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'multilinguality', bar, box, qq)

rm(list=ls())

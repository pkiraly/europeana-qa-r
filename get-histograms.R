library(readr)
library(pastecs)
library(ggplot2)
library(grid)
library(gridExtra)
source("draw.R")

file <- getFile(commandArgs(trailingOnly=TRUE))
id <- getId(file)

qa <- read_csv(paste("data/", file, ".csv", sep=""),
               col_types = "ccnnnnnnnnnn", col_names = c(
                 'collection', 'id', 'total', 'mandatory', 'descriptiveness', 'searchability', 
                 'contextualization', 'identification', 'browsing', 'viewing', 'reusability',
                 'multilinguality')
);

# total
bar <- ggplot(qa, aes(total)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Total", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$total, na.rm = TRUE), sd=sd(qa$total, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=total)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Total") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$total, stat = "qq") + 
  labs(title="Quantile plot", y="Total") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'total', bar, box, qq)
# png(paste(id, '-total.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# mandatory
bar <- ggplot(qa, aes(mandatory)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Mandatory fields", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$mandatory, na.rm = TRUE), sd=sd(qa$mandatory, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=mandatory)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Mandatory fields") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$mandatory, stat = "qq") + 
  labs(title="Quantile plot", y="Mandatory fields") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'mandatory', bar, box, qq)
# png(paste(id, '-mandatory.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# descriptiveness
bar <- ggplot(qa, aes(descriptiveness)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Descriptiveness", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$descriptiveness, na.rm = TRUE), sd=sd(qa$descriptiveness, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=descriptiveness)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Descriptiveness") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$descriptiveness, stat = "qq") + 
  labs(title="Quantile plot", y="Descriptiveness") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'descriptiveness', bar, box, qq)
# png(paste(id, '-descriptiveness.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# searchability
bar <- ggplot(qa, aes(searchability)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Searchability", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$searchability, na.rm = TRUE), sd=sd(qa$searchability, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=searchability)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Searchability") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$searchability, stat = "qq") + 
  labs(title="Quantile plot", y="Searchability") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'searchability', bar, box, qq)
# png(paste(id, '-searchability.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# contextualization
bar <- ggplot(qa, aes(contextualization)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Contextualization", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$contextualization, na.rm = TRUE), sd=sd(qa$contextualization, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=contextualization)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Contextualization") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$contextualization, stat = "qq") + 
  labs(title="Quantile plot", y="Contextualization") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'contextualization', bar, box, qq)
# png(paste(id, '-contextualization.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# identification
bar <- ggplot(qa, aes(identification)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Identification", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$identification, na.rm = TRUE), sd=sd(qa$identification, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=identification)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Identification") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$identification, stat = "qq") + 
  labs(title="Quantile plot", y="Identification") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'identification', bar, box, qq)
# png(paste(id, '-identification.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# browsing
bar <- ggplot(qa, aes(browsing)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Browsing", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$browsing, na.rm = TRUE), sd=sd(qa$browsing, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=browsing)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Browsing") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$browsing, stat = "qq") + 
  labs(title="Quantile plot", y="Browsing") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'browsing', bar, box, qq)
# png(paste(id, '-browsing.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# viewing
bar <- ggplot(qa, aes(viewing)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Viewing", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$viewing, na.rm = TRUE), sd=sd(qa$viewing, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=viewing)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Viewing") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$viewing, stat = "qq") + 
  labs(title="Quantile plot", y="Viewing") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'viewing', bar, box, qq)
# png(paste(id, '-viewing.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# 'reusability',
bar <- ggplot(qa, aes(reusability)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Reusability", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$reusability, na.rm = TRUE), sd=sd(qa$reusability, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=reusability)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Reusability") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$reusability, stat = "qq") + 
  labs(title="Quantile plot", y="Reusability") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'reusability', bar, box, qq)
# png(paste(id, '-reusability', '.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()

# multilinguality
bar <- ggplot(qa, aes(multilinguality)) + 
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(title="Histogram", x="Multilinguality", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(qa$multilinguality, na.rm = TRUE), sd=sd(qa$multilinguality, na.rm = TRUE)), colour="black", size=1) +
  scale_x_continuous(limits=c(0,1)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

box <- ggplot(qa, aes(x=collection, y=multilinguality)) +
  geom_boxplot() + labs(title="Boxplot", x="Collection", y="Multilinguality") +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

qq <- qplot(sample = qa$multilinguality, stat = "qq") + 
  labs(title="Quantile plot", y="Multilinguality") +
  scale_fill_brewer(palette="RdBu") + theme_minimal() +
  scale_y_continuous(limits=c(0,1))

saveImage(id, 'multilinguality', bar, box, qq)
# png(paste(id, '-multilinguality.png', sep=''), width=920, height=300)
# grid.arrange(bar, box, qq, ncol=3)
# dev.off()


setMinMaxRecId <- function(stats, recordQuality, field) {
  recMin <- head(recordQuality[recordQuality[field] == min(recordQuality[field]),3], 1)
  stats[field,"recMin"] <- recMin
  recMax <- head(recordQuality[recordQuality[field] == max(recordQuality[field]),3], 1)
  stats[field,"recMax"] <- recMax
  return(stats)
}

getFile <- function(args) {
  if (length(args) != 0 && args[1] != "") {
    file <- args[1]
  } else {
    file <- 'data/92027_Ag_EU_TEL_a0429E.xml.csv'
    # file <- 'data/temp.csv'
  }
  return(file)
}

getPath <- function(args) {
  if (length(args) == 2 && args[2] != "") {
    file <- args[2]
  } else {
    file <- 'data'
  }
  return(file)
}

getId <- function(file) {
  pos <- regexpr('_', file)
  if (pos[1] != -1) {
    return(substr(file, 0, (regexpr('_', file) - 1)))
  } else {
    pos <- regexpr('\\.', file)
    if (pos[1] != -1) {
      return(substr(file, 0, (regexpr('.', file, fixed=TRUE) - 1)))
    } else {
      return(file)
    }
  }
}

saveImage <- function(id, name, bar, box, qq) {
  ifelse(!dir.exists(file.path('img', id)), dir.create(file.path('img', id)), FALSE)

  png(paste('img/', id, '/', id, '-', name, '.png', sep=''), width=920, height=300)
  grid.arrange(bar, box, qq, ncol=3)
  # grid.arrange(bar, ncol=1)
  dev.off()
}

draw <- function(qa, fieldName, field) {

  bar <- ggplot(qa, aes(fieldName)) + 
    theme(legend.position = "none") +
    geom_histogram(aes(y=..density..), colour="black", fill="white") +
    labs(x="Total score", y = "Density") +
    stat_function(fun=dnorm, args=list(mean=mean(field, na.rm = TRUE), sd=sd(field, na.rm = TRUE)), colour="black", size=1) +
    scale_x_continuous(limits=c(0,1))

  box <- ggplot(qa, aes(x=collection, y=fieldName)) +
    geom_boxplot() + labs(title="Total score boxplot", x = "Collection", y = "Total score") +
    geom_jitter(shape=16, position=position_jitter(0.2)) + 
    scale_fill_brewer(palette="RdBu") + theme_minimal() +
    scale_y_continuous(limits=c(0,1))
  
  qq <- qplot(sample = field, stat = "qq")
  
  #ggsave(file = paste(imageDirectory,"05 DLF Day 1 Hist.png",sep="/"))
  
  png(paste(id, '-', fieldName, '.png', sep=''), width=920, height=300)
  grid.arrange(bar, box, qq, ncol=3)
  dev.off()
}

stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L),
                                      collapse=" "));
  stop(simpleError(blankMsg));
} # stopQuietly()

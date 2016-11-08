setMinMaxRecId <- function(stats, recordQuality, field) {
  recMin <- head(recordQuality[recordQuality[field] == min(recordQuality[field]),"id"], 1)
  stats[tolower(field),"recMin"] <- recMin
  recMax <- head(recordQuality[recordQuality[field] == max(recordQuality[field]),"id"], 1)
  stats[tolower(field),"recMax"] <- recMax
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

prepareImageDirectory <- function(id) {
  dirName <- file.path('img', id);
  if (!dir.exists(dirName)) {
    dir.create(dirName)
  } else {
    # delete filed in the directory
    unlink(file.path('img', id, '*'))
    # do.call(file.remove, list(list.files(dirName, full.names = TRUE)))
  }
}

saveImage <- function(id, name, hist, box, qq, bar) {
  ifelse(!dir.exists(file.path('img', id)), dir.create(file.path('img', id)), FALSE)
  name <- tolower(name)

  # print(paste(id, "save", "hist", format(Sys.time(), "%H:%M:%OS3")))
  # ggsave(file = paste('img/', id, '/', id, '-', name, '-hist.png', sep=''), plot=hist, width=15, height=15, units="cm")
  
  # print(paste(id, "save", "box", format(Sys.time(), "%H:%M:%OS3")))
  # ggsave(file = paste('img/', id, '/', id, '-', name, '-box.png', sep=''), plot=box, width=15, height=15, units="cm")

  # print(paste(id, "save", "qq", format(Sys.time(), "%H:%M:%OS3")))
  # ggsave(file = paste('img/', id, '/', id, '-', name, '-qq.png', sep=''), plot=qq, width=15, height=15, units="cm")

  # print(paste(id, "save", "bar", format(Sys.time(), "%H:%M:%OS3")))
  # ggsave(file = paste('img/', id, '/', id, '-', name, '-bar.png', sep=''), plot=bar, width=15, height=15, units="cm")

  # TODO: overwrite??
  imgFileName <- paste('img/', id, '/', id, '-', name, '.png', sep='')
  print(paste("save image", imgFileName))
  png(imgFileName, width=1220, height=300)
  grid.arrange(bar, hist, box, qq, ncol=4)
  dev.off()
  # older settings:
  # grid.arrange(hist, box, qq, ncol=3)
  # grid.arrange(bar, ncol=1)
}

draw <- function(qa, fieldName, label) {

  mini <- read.table(text = '', 
                     colClasses = c("character", "numeric"), 
                     col.names = c('x', 'y'), 
                     stringsAsFactors = FALSE)
  for (i in 1:length(qa[3])) {
    mini <- rbind(mini, data.frame(x = "", y = qa[fieldName][[i]]))
  }
  
  freqCount <- count(mini, 'y')
  freqCount$y <- as.factor(freqCount$y)
  freqDimensions <- dim(freqCount)

  if (freqDimensions[1] == 1) { #} || fieldName == 'crd_Proxy_dcterms_hasPart') {
    # print(paste(id, "skip", fieldName))
  } else {
    # print(paste(id, "draw", fieldName))
    
    max <- max(mini$y)
    if (max < 1.0) {
      max <- 1.0
    }
    limits <- c(max * -0.01, max *  1.01)
    
    if (substr(fieldName, 1, 4) == 'crd_') {
      freq_x_label = 'Nr of instances'
    } else {
      freq_x_label = 'list of values'
    }
    # print(paste(id, "draw", fieldName, "barplot", format(Sys.time(), "%H:%M:%OS3")))
    bar <- ggplot(data=freqCount, aes(x=y, y=freq)) +
      theme(legend.position = "none") + 
      geom_bar(stat="identity") +
      labs(title="Frequencies", x=freq_x_label, y="frequencies") +
      #scale_y_continuous(limits = limits) + 
      scale_fill_brewer(palette="RdBu") + theme_minimal()

    # print(paste(id, "draw", fieldName, "histogram", format(Sys.time(), "%H:%M:%OS3")))
    hist <- ggplot(mini, aes(y)) + 
      theme(legend.position = "none") +
      geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01) + #
      labs(title="Histogram", x=label, y="Density (%)") +
      stat_function(
        fun=dnorm, 
        args=list(
          mean=mean(mini$y, na.rm = TRUE), 
          sd=sd(mini$y, na.rm = TRUE)), 
        colour="grey", 
        size=1) +
      scale_y_continuous(limits=c(-1, 100)) + 
      scale_x_continuous(limits = limits) + 
      scale_fill_brewer(palette="RdBu") + theme_minimal()
    
    # print(paste(id, "draw", fieldName, "boxplot", format(Sys.time(), "%H:%M:%OS3")))
    box <- ggplot(mini, aes(x=mini$x, y=mini$y)) +
      geom_boxplot() + 
      labs(title="Boxplot", x="Collection", y=label) +
      # geom_jitter(shape=16, position=position_jitter(0.2)) + 
      scale_fill_brewer(palette="RdBu") + 
      scale_y_continuous(limits=c(0, max)) +
      theme_minimal()
    
    # print(paste(id, "draw", fieldName, "qq plot", format(Sys.time(), "%H:%M:%OS3")))
    qq <- qplot(sample = mini$y) + stat_qq() +
      labs(title="Quantile plot", y=label) +
      scale_fill_brewer(palette="RdBu") + theme_minimal() +
      scale_y_continuous(limits=c(0, max))
    
    saveImage(id, fieldName, hist, box, qq, bar)
    rm(hist, box, qq, bar)
    rm(max, limits, freq_x_label)
  }
  rm(mini, freqCount, freqDimensions)
}

scale_cardinality <- function(cardinality) {
  if (cardinality < 1) {
    scaled <- 0
  } else if (cardinality == 1) {
    scaled <- 1
  } else if (cardinality > 1 && cardinality <= 4) {
    scaled <- 2
  } else if (cardinality > 4 && cardinality <= 10) {
    scaled <- 3
  } else {
    scaled <- 4
  }
  scaled / 4.0
}

stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L),
                                      collapse=" "));
  stop(simpleError(blankMsg));
} # stopQuietly()

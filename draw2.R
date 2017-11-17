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

# saveImage <- function(id, name, hist, box, qq, bar) {
saveImage <- function(id, name, hist, box) {
  ifelse(!dir.exists(file.path('img', id)), dir.create(file.path('img', id)), FALSE)
  name <- tolower(name)
  
  # mode <- 'individual'
  mode <- 'compact'
  
  if (mode == 'individual') {
    print(paste(id, "save", "hist", format(Sys.time(), "%H:%M:%OS3")))
    ggsave(file = paste('img/', id, '/', id, '-', name, '-hist.png', sep=''), plot=hist, width=15, height=15, units="cm")
    
    print(paste(id, "save", "box", format(Sys.time(), "%H:%M:%OS3")))
    ggsave(file = paste('img/', id, '/', id, '-', name, '-box.png', sep=''), plot=box, width=15, height=15, units="cm")
    
    # print(paste(id, "save", "qq", format(Sys.time(), "%H:%M:%OS3")))
    # ggsave(file = paste('img/', id, '/', id, '-', name, '-qq.png', sep=''), plot=qq, width=15, height=15, units="cm")
    
    # print(paste(id, "save", "bar", format(Sys.time(), "%H:%M:%OS3")))
    # ggsave(file = paste('img/', id, '/', id, '-', name, '-bar.png', sep=''), plot=bar, width=15, height=15, units="cm")
  } else {
    imgFileName <- paste('img/', id, '/', id, '-', name, '.png', sep='')
    print(paste("save image", imgFileName))
    png(imgFileName, width=1220, height=300)
    # grid.arrange(bar, hist, box, qq, ncol=4)
    grid.arrange(hist, box, ncol=2)
    dev.off()
    # older settings:
    # grid.arrange(hist, box, qq, ncol=3)
    # grid.arrange(bar, ncol=1)
  }
}

draw <- function(qa, fieldName, label) {

  # mini <- read.table(text = '', 
  #                    colClasses = c("character", "numeric"), 
  #                    col.names = c('x', 'y'), 
  #                    stringsAsFactors = FALSE)
  # for (i in 1:length(qa[3])) {
  #   mini <- rbind(mini, data.frame(x = "", y = qa[fieldName][[i]]))
  # }

  vector <- qa[,c(fieldName)]
  normalizedVector <- normalizeVector(vector, 20)
  mini <- data.frame(x="", y=normalizedVector$vector)
  
  freqCount <- count(mini, 'y')
  freqCount$y <- as.factor(freqCount$y)
  freqDimensions <- dim(freqCount)

  if (freqDimensions[1] == 1) { #} || fieldName == 'crd_Proxy_dcterms_hasPart') {
    print(paste(id, "skip", fieldName))
  } else {
    print(paste(id, "draw", fieldName))
    
    max <- max(mini$y)
    min <- min(mini$y)
    range <- max - min
    #if (max < 1.0) {
    #  max <- 1.0
    #}
    margin <- range * 0.01
    limits <- c(min - margin, max + margin)
    
    if (substr(fieldName, 1, 4) == 'crd_') {
      freq_x_label = 'Nr of instances'
    } else {
      freq_x_label = 'list of values'
    }
    # print(paste(id, "draw", fieldName, "barplot", format(Sys.time(), "%H:%M:%OS3")))
    # bar <- ggplot(data=freqCount, aes(x=y, y=freq)) +
    #   theme(legend.position = "none") + 
    #   geom_bar(stat="identity") +
    #   labs(title="Frequencies", x=freq_x_label, y="frequencies") +
    #   #scale_y_continuous(limits = limits) + 
    #   scale_fill_brewer(palette="RdBu") + theme_minimal()

    # print(paste(id, "draw", fieldName, "histogram with binwith: ", binwidth, format(Sys.time(), "%H:%M:%OS3")))
    hist <- ggplot(mini, aes(y)) + 
      theme(legend.position = "none") +
      geom_histogram(aes(y=..count..), # ..count.., ..density..
                     colour="black", 
                     fill="white", 
                     breaks = normalizedVector$breaks) +
                     # binwidth = binwidth)  + #
      # labs(title="Histogram", x=label, y="Density (%)") +
      labs(title="Histogram", x=label, y="Frequency") +
      # stat_function(
      #   fun=dnorm, 
      #   args=list(
      #     mean=mean(mini$y, na.rm = TRUE), 
      #     sd=sd(mini$y, na.rm = TRUE)), 
      #   colour="grey", 
      #   size=1) +
      # scale_y_continuous(limits=c(min, max)) + 
      # scale_x_continuous(limits = limits) + 
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
    # qq <- qplot(sample = mini$y) + stat_qq() +
    #   labs(title="Quantile plot", y=label) +
    #   scale_fill_brewer(palette="RdBu") + theme_minimal() +
    #   scale_y_continuous(limits=c(0, max))
    
    # print(paste(id, "draw", fieldName, "->save()", format(Sys.time(), "%H:%M:%OS3")))
    saveImage(id, fieldName, hist, box)
    # print(paste(id, "draw", fieldName, "/save()", format(Sys.time(), "%H:%M:%OS3")))
    # rm(hist, box, qq, bar)
    rm(hist, box)
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

getBreaks <- function(vector, bins, margin, boxplot.upper) {
  histogram <- hist(vector, plot = FALSE, breaks = bins)
  breaks <- histogram$breaks
  if (margin < breaks[2]) {
    breaks <- c(0, margin, breaks[2:length(breaks)])
  }
  if (breaks[length(breaks)] > boxplot.upper & breaks[length(breaks) - 1] < boxplot.upper) {
    breaks <- c(breaks[1:length(breaks)-1], boxplot.upper, breaks[length(breaks)])
  }
  return(breaks)
}

normalizeVector <- function(vector, bins) {
  normalized <- list()
  iqr <- IQR(vector)
  margin <- iqr / (bins * 2.5)
  boxplot <- boxplot.stats(vector)
  boxplot.lower <- boxplot$stats[1]
  boxplot.upper <- boxplot$stats[5]
  normalized$normalProcess <- TRUE
  if (boxplot.lower == 0 & boxplot.upper == 0 & length(boxplot$out) > 0) {
    normalized$normalProcess <- FALSE
  }

  if (normalized$normalProcess) {
    normalized$maximum <- boxplot.upper + margin
  } else {
    normalized$maximum <- min(boxplot$out)
    bins <- 2
    normalized$breaks <- c(0, 0.1, (normalized$maximum - 0.1), normalized$maximum)
  }
  vector[vector > boxplot.upper] <- normalized$maximum
  normalized$vector <- vector
  if (normalized$normalProcess) {
    normalized$breaks <- getBreaks(vector, bins, margin, boxplot.upper)
  }
  return(normalized)
}

stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L),
                                      collapse=" "));
  stop(simpleError(blankMsg));
} # stopQuietly()

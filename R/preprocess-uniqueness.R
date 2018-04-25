source("R/uniqueness-field-definitions.R")

fields <- data.frame(field = uniqueness_fields, stringsAsFactors = FALSE)
fields$total <- c(43429608,3192454,26435450)
fields

allbins <- df <- data.frame(name=character(),
                            x1=numeric(),
                            x2=numeric(),
                            x20=numeric(),
                            x40=numeric(),
                            x60=numeric(),
                            x80=numeric(),
                            x100=numeric(),
                            stringsAsFactors=FALSE) 
for (i in 1:dim(fields)[1]) {
  field <- fields[i, 'field']
  total <- fields[i, 'total']
  df <- data.frame(x = seq(1, 100000, 1))
  df$y <- log(1 + (total - df$x + 0.5) / (df$x + 0.5))
  df$z <- (df$y / max(df$y))^2.5
  bins <- data.frame(
    field=field,
    x1=1.0,
    x2=2.0,
    x20=head(df[df$z <= 0.8,c('x')], 1),
    x40=head(df[df$z <= 0.6,c('x')], 1),
    x60=head(df[df$z <= 0.4,c('x')], 1),
    x80=head(df[df$z <= 0.2,c('x')], 1),
    x100=total
  )
  names(bins) <- c('name', 'x1', 'x2', 'x20', 'x40', 'x60', 'x80', 'x100')
  print(bins)
  allbins <- rbind(allbins, bins)
}

allbins
write.csv(allbins, file="uniqueness.allbins.csv", row.names = FALSE)

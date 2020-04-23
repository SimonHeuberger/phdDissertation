
library(magrittr)

# sample prop percent of rows
# for each sampled row, replace all all colNA column values with NAs
own.NA.rows <- function(df, prop, colNA){
  rows <- sample(nrow(df), prop*nrow(df))
  df[rows, colNA] <- NA
  return(df)
}


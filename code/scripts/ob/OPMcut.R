
OPMcut <- function(data, dv, OPMordOut){
    # turn dv into factor variable if it isn't one
  if(is.factor(data[, dv]) == FALSE){
    data[, dv] <- as.factor(data[, dv])
  }
    # store levels of dv and the number of the penultimate level
  lev <- levels(data[, dv])
  penult <- length(lev)-1
    # category span between first and second level
  cat_second_span <- abs(OPMordOut[["int.df"]][1,2] - OPMordOut[["int.df"]][2,2])
    # category span between penultimate and last level
  cat_penult_span <- abs(OPMordOut[["int.df"]][(penult-1),2] - OPMordOut[["int.df"]][penult,2])
    # beginning cutpoint for first level 
  cat_first_cut <- OPMordOut[["int.df"]][1,2] - cat_second_span
    # end cutpoint for last level 
  cat_last_cut <- OPMordOut[["int.df"]][penult,2] + cat_penult_span
   # empty vector to store middle between cutpoints for each level
  midpoints <- c()
    # mid-cutpoint for first level
  midpoints[1] <- (cat_first_cut + OPMordOut[["int.df"]][1,2])/2
    # mid-cutpoints for all except first and last levels 
  for (i in 1:(penult-1)){
    midpoints[i+1] <- (OPMordOut[["int.df"]][i,2] + OPMordOut[["int.df"]][(i+1),2])/2
  }
    # mid-cutpoint for last level 
  midpoints[length(lev)] <- (OPMordOut[["int.df"]][penult,2] + cat_last_cut)/2
    # empty df to store dv level replacements
  tmp <- data.frame(matrix(NA, nrow(data), length(lev)))
    # replace dv levels with mid-cutpoints
  for (x in 1:length(lev)){
    tmp[, x] <- ifelse(data[, dv] == lev[x], midpoints[x], NA)
  }
    # overwrite dv with mid-cutpoints replacement
  data[, dv] <- c(na.omit(c(t(tmp))))
  return(data)
}


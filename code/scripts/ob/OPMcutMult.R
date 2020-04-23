
OPMcutMult <- function(data, dv, NAcols, OPMordMultOut){
  # turn column classes depending on original class
  for(i in 1:length(dv)){
    if(dv[i] %in% NAcols == TRUE){
      stop(paste0("The variable '", dv[i], "' cannot be the DV and a column with NAs at the same time"))
    }
    if(is.factor(data[, dv[i]]) == FALSE){
      # change it to factor and save under current name
      data[, dv[i]] <- as.factor(data[, dv[i]])
    }
    lev <- levels(data[, dv[i]])
    penult <- length(lev)-1
    # category span between first and second level
    cat_second_span <- abs(OPMordMultOut[["int.dfs"]][[dv[i]]][1,2] - OPMordMultOut[["int.dfs"]][[dv[i]]][2,2])
    
    # category span between penultimate and last level
    cat_penult_span <- abs(OPMordMultOut[["int.dfs"]][[dv[i]]][(penult-1),2] - OPMordMultOut[["int.dfs"]][[dv[i]]][penult,2])
    # beginning cutpoint for first level 
    cat_first_cut <- OPMordMultOut[["int.dfs"]][[dv[i]]][1,2] - cat_second_span
    # end cutpoint for last level 
    cat_last_cut <- OPMordMultOut[["int.dfs"]][[dv[i]]][penult,2] + cat_penult_span
    # empty vector to store middle between cutpoints for each level
    midpoints <- c()
    # mid-cutpoint for first level
    midpoints[1] <- (cat_first_cut + OPMordMultOut[["int.dfs"]][[dv[i]]][1,2])/2
    # mid-cutpoints for all except first and last levels 
    
    for (x in 1:(penult-1)){
    midpoints[x+1] <- (OPMordMultOut[["int.dfs"]][[dv[i]]][x,2] + OPMordMultOut[["int.dfs"]][[dv[i]]][(x+1),2])/2
  }
    # mid-cutpoint for last level 
    midpoints[length(lev)] <- (OPMordMultOut[["int.dfs"]][[dv[i]]][penult,2] + cat_last_cut)/2
    # empty df to store dv level replacements
    tmp <- data.frame(matrix(NA, nrow(data), length(lev)))
    # replace dv levels with mid-cutpoints
    for (x in 1:length(lev)){
      tmp[, x] <- ifelse(data[, dv[i]] == lev[x], midpoints[x], NA)
    }
    # overwrite dv with mid-cutpoints replacement
    data[, dv[i]] <- c(na.omit(c(t(tmp))))
  }
  return(data)
}


# library(here)
# save("OPMcutMult", file = here("functions", "OPMcutMult.Rdata"))

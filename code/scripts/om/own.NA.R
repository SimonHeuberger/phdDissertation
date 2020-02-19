
library(magrittr)

# df = data frame
# prop = desired proportion of missing values
# colNA = variables that should have NAs inserted (can be concatenated)
# colDep = variables that the inserted NAs should depend on (can be concatenated, needs to be the same length as colNA)

own.NA <- function(df, prop, colNA, colDep){
  # save original df (might be needed later)
  df.orig <- df
  # add column with ID numbers
  df$id <- as.numeric(rownames(df))
  for(i in 1:length(colDep)){ # i.e. for each variable listed as colDep
      # extract unique levels/values of colDep variable
      uqs <- unique(df[, colDep[i]])
      ids <- lapply(1:length(uqs), function(n){ # for each variable's unique values/levels
            # identify all values in colDep for each level
            ids <- df[which(df[, colDep[i]] == uqs[n]), ] %>% 
              # select only the ID column
              .$id %>% 
              # sample a percentage of the IDs
              sample(., round(length(.)*prop))
            # output the sampled IDs
            ids
          # unlist all IDs for the level, so that they are a vector
          }) %>% unlist
      # replace the colNA values for the unlisted IDs with NA
      df[, colNA[i]][df$id %in% ids] <- NA
  }
  # return the df with NAs
  # df will have prop amount of NAs spread out across each variable's unique values
  return(df)
}


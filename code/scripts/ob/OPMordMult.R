
# The function needs the inputs: data (data frame), dv (character), evs (character or string of characters)
# It outputs data, plr.out, plr.df, int.df, the new education levels, and the new numeric education levels
# The output can be called with $


OPMordMult <- function(data, dv, evs, NAcols){
  library(MASS)
  library(data.table)
  # save original data with all NAs
  data.full.nas <- data
  # use na.omit version of the data in the function
  data <- na.omit(data)
  ord.new.lev <- list()
  ord.new.lev.num <- list()
  int.dfs <- list()
  # turn column classes depending on original class
  for(i in 1:length(dv)){
    if(is.factor(data[, dv[i]]) == TRUE){
      # change it to numeric and save under new numeric name
      data[, paste0(dv[i], ".num")] <- as.numeric(data[, dv[i]])
    }
    if(is.numeric(data[, dv[i]]) == TRUE){
      # save it under new numeric name
      data[, paste0(dv[i], ".num")] <- data[,dv[i]]
      # change it to factor and save under current name
      data[, dv[i]] <- as.factor(data[, dv[i]])
    }
    if(is.character(data[, dv[i]]) == TRUE){
      # change it to numeric and save under new numeric name
      data[, paste0(dv[i], ".num")] <- as.numeric(data[,dv[i]])
      # change it to factor and save under current name
      data[, dv[i]] <- as.factor(data[, dv[i]])
    }  
    if(dv[i] %in% NAcols == TRUE){
      stop(paste0("The variable '", dv[i], "' cannot be the DV and a column with NAs at the same time"))
    }
    variables <- c(dv[i], evs)
    a <- as.formula(data[,variables])
    plr.out <- polr(a, data = data, Hess=TRUE)
    # turn plr.out output into a data frame
    plr.df <- data.frame(coef(summary(plr.out)))
    # empty vector for storage
    empty <- c()
    # loop to fill empty vector with intercept names
    for (x in 1:(length(levels(data[,dv[i]]))-1)){
      empty[x] <- paste0(levels(data[,dv[i]])[x], "|", levels(data[,dv[i]])[x+1])
    }
    # select only rows with intercept names
    int.df <- plr.df[empty,]
    # turn row names into a column
    int.df <- setDT(int.df, keep.rownames = TRUE)[]
    # removes class "data.table", which was added by setDT
    int.df <- data.frame(int.df)
    colnames(int.df) <- c("Intercepts", "Values", "SE", "t-values")
    # factorize intercepts with correctly ordered levels
    int.df$Intercepts <- factor(int.df$Intercepts, levels = int.df$Intercepts)
    # empty df to fill with assigned new cases
    df.cases <- data.frame(matrix(NA, nrow(data), length(levels(data[,dv[i]]))))
    # assign cases that fall underneath the lowest intercept with the respective education category, put results in first column
    df.cases[,1] <- ifelse(plr.out$lp <= int.df$Values[1], levels(data[,dv[i]])[1], NA)
    # assign cases that fall between all intercepts except the lowest and the highest, put results in all but first and last columns
    for(q in 1:(length(levels(data[,dv[i]]))-2)){
      df.cases[,q+1] <- ifelse(plr.out$lp > int.df$Values[q] &
                                 plr.out$lp <= int.df$Values[q+1], levels(data[,dv[i]])[q+1], NA)
    }
    # assign all cases that fall above the highest intercept, put in last column
    df.cases[,length(levels(data[,dv[i]]))] <- ifelse(plr.out$lp > int.df$Values[length(levels(data[,dv[i]]))-1],
                                                      levels(data[,dv[i]])[length(levels(data[,dv[i]]))], NA)
    # combine all columns, omit NAs, add column to df
    data[,paste0(dv[i], ".new")] <- factor(c(na.omit(c(t(df.cases)))))
    # empty small df to extract numbers for the respective re-estimated categories
    df.factors.1 <- data.frame(matrix(NA, length(levels(data[,paste0(dv[i], ".new")])), 2))
    # for each remaining category, extract its name and match it with its respective number, add by rows
    for (w in 1:length(levels(data[,paste0(dv[i], ".new")]))){
      df.factors.1[w,] <- c(unique(data[,paste0(dv[i], ".num")][data[,dv[i]] == levels(data[,paste0(dv[i], ".new")])[w]]),
                            levels(data[,paste0(dv[i], ".new")])[w])
    }
    # empty long df to set up numbers for re-estimated categories for entire data set
    df.factors.2 <- data.frame(matrix(NA, nrow(data), nrow(df.factors.1)))
    # assign numbers, add by columns
    for (n in 1:nrow(df.factors.1)){
      df.factors.2[,n] <- ifelse(data[,paste0(dv[i], ".new")] == df.factors.1[n,2], df.factors.1[n,1], NA)
    }
    # combine all columns, omit NAs, add column to df
    data[,paste0(dv[i], ".new.num")] <- as.numeric(na.omit(c(t(df.factors.2))))
    # refactor levels in the order of the numbers
    data[,paste0(dv[i], ".new")] <- factor(data[,paste0(dv[i], ".new")],
                                           levels = unique(data[,paste0(dv[i], ".new")][order(data[,paste0(dv[i], ".new.num")])]))
    # make numbers for .new.num column start at 1, based on newly refactored .new levels
    data[,paste0(dv[i], ".new.num")] <- as.numeric(data[,paste0(dv[i], ".new")])
    
    int.dfs[[i]] <- int.df
    ord.new.lev[[i]] <- levels(data[,paste0(dv[[i]], ".new")])
    ord.new.lev.num[[i]] = sort(unique(data[,paste0(dv[[i]], ".new.num")]))
  }
  
  names(ord.new.lev) <- names(ord.new.lev.num) <- names(int.dfs) <- dv
  
  # have the function output data with all NAs, na.omit version of data, plr.out, plr.df, int.df, the new education levels, and the new numeric education levels
  output <- list("data.full.nas" = data.full.nas,
                 "data.short.na.omit" = data,
                 # "plr.out" = plr.out,
                 # "plr.df" = plr.df, 
                 "int.dfs" = int.dfs,
                 "ord.new.lev" = ord.new.lev,
                 "ord.new.lev.num" = ord.new.lev.num)
  return(output)
}



## Everything except the int.df and df.factors stuff is very straightforward
## The int.df code assigns the binned cases according to the new thresholds and gives them the respective category names. I had to move the first and the last threshold outside of the loop because it needed different code, since the first falls beneath it and the last above it, but all others fall in-between
## The df.factors stuff is there for one stupid reason: The factor levels for education.new were not ordered the right way, substantively. So I had to come up with a way to order them, but of course R doesn't know that HS grad is supposed to be before Associate. So I needed code to order the levels of whatever categories remained. The only thing I could think of was to add an education.num vector when I set up df, extract the numbers for whatever re-estimated categories remain, create a long vector with those numbers and assign it to the df, and finally reorder the category factor levels based on these numbers. 12 lines of code for such a seemingly simple thing

# library(here)
# save("OPMordMult", file = here("functions", "OPMordMult.Rdata"))
# can be loaded into any R session with load(here("functions", "OPMordMult.Rdata"))

data <- readRDS(here("data", "anes", "anes_cleaned.rds")) 
dv <- c("education", "income")
evs <- c("age", "gender", "race", "occupation", "pid")
NAcols <- c("age", "gender", "race", "occupation", "pid")
fg <- OPMordMult(data = data, dv = dv, evs = evs, NAcols = NAcols)
fg
names(fg)





library(plyr)
library(dplyr)
library(hot.deck)
library(magrittr)
library(data.table)
library(tidyverse)
library(missForest)


load("OPMord.Rdata") 
load("OPMcut.Rdata") 
load("hot.deck.ord.Rdata") 


# Load the framing data
framing <- read.csv("second_framing_experiment.csv")

# Select needed columns
framing <- framing[, c("tb_supp", "tb", "pid", "educ", "inc", "age", "race", "gender", "empl", "interest", "media", "part" )]

# Make all needed columns binary, i.e. numeric/integer (needed for hot.deck())
framing <- mutate(framing,
                  C = ifelse(tb == "C", 1, 0),
                  M_Supp = ifelse(tb == "M_Supp", 1, 0),
                  P_Supp = ifelse(tb == "P_Supp", 1, 0),
                  M_Opp = ifelse(tb == "M_Opp", 1, 0),
                  P_Opp = ifelse(tb == "P_Opp", 1, 0),
                  Dem = ifelse(pid == "Democrat", 1, 0),
                  Rep = ifelse(pid == "Republican", 1, 0),
                  Ind = ifelse(pid == "Independent", 1, 0),
                  SthElse = ifelse(pid == "Something Else", 1, 0),
                  Black = ifelse(race == "Black or African-American", 1, 0),
                  Hisp = ifelse(race == "Hispanic", 1, 0),
                  White = ifelse(race == "White", 1, 0),
                  Female = ifelse(gender == "Female", 1, 0),
                  Male = ifelse(gender == "Male", 1, 0)
)

set.seed(123)
framing <- sample_n(framing, 100)


# Select needed columns and save complete data under a different name
framing_true <- framing <- framing[, c("Dem", "Rep", "Ind", "educ", "inc", "age", "Female", "Male", "Black", "Hisp", "White")]


# variables to insert NAs into. Two are binary, one is nominal, one is continuous
add.nas.columns <- "inc" 
no.nas <- framing_true[,!names(framing_true) %in% add.nas.columns] # separate framing_true into columns with and without NAs
yes.nas <- framing_true[,names(framing_true) %in% add.nas.columns]

prop <- .05 # the proportions of NAs to insert (for the loop)

m <- round_any(prop*nrow(framing_true)*length(add.nas.columns), 10)

methods <- c("hd.ord", "hd.norm", "na.omit")

if(length(add.nas.columns) == 1){
  yes.nas <- data.frame(yes.nas)
  colnames(yes.nas) <- add.nas.columns
  means.hd.norm <- means.hd.ord <- c()
  results.list <- rep(list(c()), length(methods))
  names(results.list) <- methods
  true <- framing_true[, add.nas.columns] %>% mean()
}else{
  means.hd.norm <- means.hd.ord <- data.frame(matrix(NA, m, length(add.nas.columns))) # empty dfs to store variable means for hd.ord and hd.norm
  results.list <- list() # empty list to store results
  true <- sapply(framing_true[, add.nas.columns], mean) # true variable means
}

mc.iterations <- 10000

#results.list <- rep(list(data.frame(matrix(NA, length(methods), length(add.nas.columns)+1))), mc.iterations)
#means.hd <- data.frame(matrix(NA, m, length(add.nas.columns)))
#list.means.hd.norm <- list.means.hd.ord <- rep(list(means.hd), mc.iterations)
#na.omit <- data.frame(matrix(NA, mc.iterations, length(add.nas.columns)))

pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3) # creates the percentage progress bar across the sampled numbers

#mc <- 1
for(mc in 1:mc.iterations){
  
  setTxtProgressBar(pb, mc) # loads the percentage progress bar into the loop
  
  framing.nas <- cbind(no.nas, prodNA(yes.nas, noNA = prop)) # combine columns with NAs and columns without NAs
  na.rows.cols <- which(is.na(framing.nas), arr.ind=TRUE) # which rows and columns have NAs
  m <- na.rows.cols %>% nrow() # how many rows have NAs

  OPMord.frame.more.nas <- OPMord(data = na.omit(framing.nas), dv = "educ", evs = c("Dem", "Rep", "Ind", "inc", "age", "Female", "Male", "Black", "Hisp", "White")) # run ordinal polr() function on data

  OPMcut.frame.more.nas <- OPMcut(data = framing.nas, dv = "educ", OPMordOut = OPMord.frame.more.nas) # run function that replaces ordinal values with mid-cutpoints

  hot.deck.ord.frame.more.nas <- hot.deck.ord(OPMcut.frame.more.nas, m = m, ord = "educ", sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell") # run hot.deck.ord(), which scales the ordinal variable
  
  hot.deck.norm.frame.more.nas <- hot.deck(OPMcut.frame.more.nas, m = m, sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell") # run normal hot.deck()

  na.cols <- na.rows.cols %>% .[,2] %>% unique() # which column numbers have NAs in them
  na.colnames <- names(framing.nas)[na.cols] # names of columns with NAs in them

    if(length(na.colnames) == 1){
      
        for (i in 1:m){
          means.hd.ord[i] <- hot.deck.ord.frame.more.nas$data[[i]] %>% .[, na.colnames] %>% mean()
          means.hd.norm[i] <- hot.deck.norm.frame.more.nas$data[[i]] %>% .[, na.colnames] %>% mean()
        }
      
      results.list[[1]][mc] <- means.hd.ord %>% mean()
      results.list[[2]][mc] <- means.hd.norm %>% mean()
      results.list[[3]][mc] <- na.omit(framing.nas[, na.colnames]) %>% mean()

    }else{

    for (x in 1:length(na.cols)){
      for (i in 1:m){
        means.hd.ord[i,x] <- hot.deck.ord.frame.more.nas$data[[i]] %>% .[, na.colnames[x]] %>% mean()
        means.hd.norm[i,x] <- hot.deck.norm.frame.more.nas$data[[i]] %>% .[, na.colnames[x]] %>% mean()
      }
    }
  
  hd.ord <- sapply(means.hd.ord, mean) # hd.ord variable means
  hd.norm <- sapply(means.hd.norm, mean) # hd.norm variable means
  na.omit <- sapply(na.omit(framing.nas[, na.colnames]), mean) # na.omit variable means
  results <- data.frame(rbind(true, abs(true-hd.ord), abs(true-hd.norm), abs(true-na.omit))) # combine all means in a df
  results[,5] <- c("true", "hd.ord", "hd.norm", "na.omit")
  colnames(results)[5] <- "method"
  rownames(results) <- NULL
  
  results.list[[mc]] <- results # store means for each NA proportion in list

  }

}


# Save files
  if(length(add.nas.columns) == 1){

    saveRDS(results.list, file = paste0("results.", nrow(framing), "n.", mc.iterations, "it.", add.nas.columns,  ".rds"))

  }#else{}

# Analyse files
  if(length(add.nas.columns) == 1){

    vars <- c("Rep", "Dem", "inc", "age")

      for(k in 1:length(vars)){
        assign(paste0(vars[k], ".results.list"), readRDS(paste0("results.", nrow(framing), "n.", mc.iterations, "it.", vars[k],  ".rds")))
      }

    print(Rep.mean <- sapply(Rep.results.list, mean))
    print(Dem.mean <- sapply(Dem.results.list, mean))
    print(inc.mean <- sapply(inc.results.list, mean))
    print(age.mean <- sapply(age.results.list, mean))

    Rep.true <- framing_true[, "Rep"] %>% mean()
    Dem.true <- framing_true[, "Dem"] %>% mean()
    inc.true <- framing_true[, "inc"] %>% mean()
    age.true <- framing_true[, "age"] %>% mean()

    print(Rep.abs <- abs(Rep.mean - Rep.true))
    print(Dem.abs <- abs(Dem.mean - Dem.true))
    print(inc.abs <- abs(inc.mean - inc.true))
    print(age.abs <- abs(age.mean - age.true))

    print(c("Rep", names(Rep.abs)[which.min(Rep.abs)], min(Rep.abs)))
    print(c("Dem", names(Dem.abs)[which.min(Dem.abs)], min(Dem.abs)))
    print(c("inc", names(inc.abs)[which.min(inc.abs)], min(inc.abs)))
    print(c("age", names(age.abs)[which.min(age.abs)], min(age.abs)))

  }#else{}


# According to the means of means, na.omit is the clear winner. That's not all
# that surprising, since we're doing MCAR. With MCAR, deleting observations with
# missing values shouldn't make any difference to the analysis. But hd.ord
# performs better than hd.norm across the board, which is good.





# run <- c("jeff", "CO", "mine")
run <- "mine"
# number of iterations
mc.iterations <- 10

library(plyr)
library(tidyverse)
library(MASS)
library(data.table)
library(reshape2)
library(dplyr)
library(hot.deck)
library(magrittr)
library(rlist)
library(Amelia)
library(caret)

if(run == "mine"){
  library(here)
  load(here("functions", "OPMord.Rdata"))
  load(here("functions", "OPMcut.Rdata"))
  load(here("functions", "hot.deck.ord.Rdata"))
  df_true <- readRDS(here("data", "framing", "framing_1000.rds")) # I took a sample of 1000 rows from the framing data in the separate framing script, making sure it contains all educ levels, then I saved that sample
}
if(run == "jeff"){
  load("OPMord.Rdata") 
  load("OPMcut.Rdata") 
  load("hot.deck.ord.Rdata")
  df_true <- readRDS("framing_1000.rds") 
}
if(run == "CO"){
  load("../data/OPMord.Rdata") 
  load("../data/OPMcut.Rdata") 
  load("../data/hot.deck.ord.Rdata")
  df_true <- readRDS("../data/framing_1000.rds")
}

# identify and discard highly collinear variables
coll.var <- df_true %>% cor() %>% abs() %>% findCorrelation(., cutoff = .6) %>% sort()         
df_true <- df_true[,-c(coll.var)]                                                         
# save name of all EVs
all.evs <- colnames(df_true)[-which(colnames(df_true) == "educ")]                         

# variables to insert NAs into
add.nas.columns <- c("Rep", "Ind", "Black", "Hisp", "Male", "Unempl", "Stud", "Official", 
                     "Media", "Participation", "interest", "inc", "age")

# separate df_true into columns with and without NAs
no.nas <- df_true[,!names(df_true) %in% add.nas.columns]                                  
yes.nas <- df_true[,names(df_true) %in% add.nas.columns]

# the proportions of NAs to insert
prop <- .2
# number of imputations (https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul)
m <- prop*100

# imputation methods
methods <- c("hd.ord", "hd.norm.orig", "amelia", "mice", "na.omit")                                 

# true variable means
true <- sapply(df_true[, add.nas.columns], mean)
# the complete number of education levels
min.lev <- df_true$educ %>% unique() %>% length()

# empty list to store OPMord output
OPMord.few <- list()
OPMord.full <- list()
# set up the percentage progress bar across the sampled numbers
pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3)
for(mc in 1:mc.iterations){
  # load the percentage progress bar into the loop
  setTxtProgressBar(pb, mc)
  # combine columns with NAs and columns without NAs and run ordinal polr() function on data
  data.amp <- cbind(no.nas, ampute(yes.nas, prop = prop, mech = "MAR")$amp)
  if(na.omit(data.amp)  %>% .$educ %>% unique() %>% length() < min.lev){
    OPMord.few[[mc]] <- OPMord(data.amp, dv = "educ", evs = all.evs)
  }else{
    OPMord.full[[mc]] <- OPMord(data.amp, dv = "educ", evs = all.evs)
  }
}


if(length(OPMord.few) != 0){
  OPMord.full <- OPMord.full[-which(sapply(OPMord.full, is.null))]
    if(length(OPMord.full) < 1000){
        stop("Fewer than 1000 data sets with all levels")
    }
}

if(length(OPMord.full) > 1000){
  OPMord.full <- sample(OPMord.full, 1000)
}

OPMord.full %>% length





# empty list of lists of vectors to store results
results.list <- rep(list(rep(list(c()), length(methods))), length(add.nas.columns))
# name first list after variables with NAs
names(results.list) <- add.nas.columns

  # name second lists after methods
  for (t in 1:length(add.nas.columns)){
    names(results.list[[t]]) <- methods
  }



start_time <- Sys.time()
am.orig.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
  for(n in 1:length(OPMord.full)){
    setTxtProgressBar(pb, n)
    # run amelia() on original education values (ps2 sets the console printing)
    am.orig.dat[[n]] <- amelia(OPMord.full[[n]]$data.full.nas, m = m, p2s = 0)
      }
end_time <- Sys.time()
am.time <- difftime(end_time, start_time, units = "mins")


OPMcut.dat <- list()

pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
  for(n in 1:length(OPMord.full)){
    setTxtProgressBar(pb, n)
    # replace ordinal values with mid-cutpoints
    OPMcut.dat[[n]] <- OPMcut(data = OPMord.full[[n]]$data.full.nas,
                           dv = "educ", OPMordOut = OPMord.full[[n]])
    }


start_time <- Sys.time()
hd.ord.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
  for(n in 1:length(OPMord.full)){
    setTxtProgressBar(pb, n)
    # run hot.deck.ord() on OPMcut education values, which scales the ordinal variable
    hd.ord.dat[[n]] <- hot.deck.ord(OPMcut.dat[[n]], m = m, ord = "educ",
                                 sdCutoff = sd(OPMcut.dat[[n]]$educ), method = "best.cell")
      }
end_time <- Sys.time()
hd.ord.time <- difftime(end_time, start_time, units = "mins")


start_time <- Sys.time()
hd.norm.orig.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
  for(n in 1:length(OPMord.full)){
    setTxtProgressBar(pb, n)
    # run normal hot.deck() on original education values
    hd.norm.orig.dat[[n]] <- hot.deck(OPMord.full[[n]]$data.full.nas, m = m,
                                   sdCutoff = sd(OPMord.full[[n]]$data.full.nas$educ),
                                   method = "best.cell")
      }
end_time <- Sys.time()
hd.norm.time <- difftime(end_time, start_time, units = "mins")




start_time <- Sys.time()
mice.orig.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
  for(n in 1:length(OPMord.full)){
    setTxtProgressBar(pb, n)
    # run mice() on original education values (print sets the console)
    mice.orig.dat[[n]] <- mice(OPMord.full[[n]]$data.full.nas, m = m, print = FALSE)
      }
end_time <- Sys.time()
mice.time <- difftime(end_time, start_time, units = "mins")


runtime <- cbind(c(hd.ord.time, hd.norm.time, am.time, mice.time),
                   methods[1:4])


if(run == "mine"){
  write.csv(runtime, file = here("scripts", "testing", "om", paste0("runtime.", length(add.nas.columns), "var.",
                          nrow(df_true), "n.", length(OPMord.full), "it.",
                          prop*100, "perc.csv")))
}
if(run == "jeff"){
  write.csv(runtime, paste0("runtime.", length(add.nas.columns), "var.",
                          nrow(df_true), "n.", length(OPMord.full), "it.",
                          prop*100, "perc.csv"))
}
if(run == "CO"){
  write.csv(runtime, paste0("../results/runtime.", length(add.nas.columns), "var.",
                          nrow(df_true), "n.", length(OPMord.full), "it.",
                          prop*100, "perc.csv"))
}



pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
for(n in 1:length(OPMord.full)){
  setTxtProgressBar(pb, n)
  # empty dfs to store variable means
  # means.mice.orig <- means.am.orig <- means.hd.norm.cut <-means.hd.norm.orig <- means.hd.ord <-
  #   data.frame(matrix(NA, m, length(add.nas.columns))) # empty dfs to store variable means
  means.mice.orig <- means.am.orig <- means.hd.norm.orig <- means.hd.ord <-
    data.frame(matrix(NA, m, length(add.nas.columns)))
  # which rows and columns have NAs
  na.rows.cols <- which(is.na(OPMord.full[[n]]$data.full.nas), arr.ind=TRUE)
  # which column numbers have NAs in them
  na.cols <- na.rows.cols %>% .[,2] %>% unique()

  # fill in means dfs
  for (x in 1:length(na.cols)){
    for (i in 1:m){
      means.hd.ord[i,x] <- hd.ord.dat[[n]]$data[[i]] %>% .[, add.nas.columns[x]] %>% mean()
      # means.hd.norm.cut[i,x] <- hd.norm.cut.dat$data[[i]] %>%
      #   .[, add.nas.columns[x]] %>%
      #   mean()
      means.hd.norm.orig[i,x] <- hd.norm.orig.dat[[n]]$data[[i]] %>%
        .[, add.nas.columns[x]] %>%
        mean()
      means.am.orig[i,x] <- am.orig.dat[[n]]$imputations[[i]] %>%
        .[, add.nas.columns[x]] %>%
        mean()
      means.mice.orig[i,x] <- mice::complete(mice.orig.dat[[n]], action = i) %>%
        .[, add.nas.columns[x]] %>%
        mean()
    }
  }

  # take mean of each means df
  hd.ord <- sapply(means.hd.ord, mean)
  # hd.norm.cut <- sapply(means.hd.norm.cut, mean)
  hd.norm.orig <- sapply(means.hd.norm.orig, mean)
  am.orig <- sapply(means.am.orig, mean)
  mice.orig <- sapply(means.mice.orig, mean)
  # take means of variables from data without NAs
  na.omit <- sapply(OPMord.full[[n]]$data.short.na.omit[, add.nas.columns], mean)

  # fill in results.list
  for (ss in 1:length(add.nas.columns)){
    results.list[[ss]][["hd.ord"]][n] <- hd.ord[ss]
    # results.list[[ss]][["hd.norm.cut"]][n] <- hd.norm.cut[ss]
    results.list[[ss]][["hd.norm.orig"]][n] <- hd.norm.orig[ss]
    results.list[[ss]][["amelia"]][n] <- am.orig[ss]
    results.list[[ss]][["mice"]][n] <- mice.orig[ss]
    results.list[[ss]][["na.omit"]][n] <- na.omit[ss]
  }

}


# save results.list
# if(run == "mine"){
#   saveRDS(results.list, file = here("scripts", "testing", "om", paste0("results.", length(add.nas.columns), "var.",
#                                     nrow(df_true), "n.", length(OPMord.full), "it.",
#                                     prop*100, "perc.rds")))
# }
# if(run == "jeff"){
#   saveRDS(results.list, file = paste0("results.", length(add.nas.columns), "var.",
#                                     nrow(df_true), "n.", length(OPMord.full), "it.",
#                                     prop*100, "perc.rds"))
# }
# if(run == "CO"){
#   saveRDS(results.list, file = paste0("../results/results.", length(add.nas.columns), "var.",
#                                     nrow(df_true), "n.", length(OPMord.full), "it.",
#                                     prop*100, "perc.rds"))
# }




results <- data.frame(cbind(
  "method" = rep(c("true", methods), length(add.nas.columns)),
  "variable" = rep(add.nas.columns, each = length(methods)+1),
  "value" = rep(NA, (length(methods)+1)*length(add.nas.columns)),
  "diff" = rep(NA, (length(methods)+1)*length(add.nas.columns))
))
results$value <- as.numeric(results$value)
results$diff <- as.numeric(results$diff)


for (xs in 1:length(add.nas.columns)){
  results$value[results$method == "true" & results$variable == add.nas.columns[xs]] <- true[xs]
  for(vv in 1:length(methods)){
    results$value[results$method == methods[vv] & results$variable == add.nas.columns[xs]] <-
      results.list[[add.nas.columns[xs]]][[methods[vv]]] %>% mean()
  }
}

results[, "value"] <- results[, "value"] %>% round(., digits = 4)


for(vv in 1:length(add.nas.columns)){
  results$diff[results$variable == add.nas.columns[vv]] <-
    results$value[results$variable == add.nas.columns[vv]] -
    results$value[results$variable == add.nas.columns[vv] & results$method == "true"]
}

# turn off scientific (e-04 etc.) notation
results[,3:4] <- format(results[,3:4], scientific = FALSE)

# save as .csv
if(run == "mine"){
  write.csv(results, here("scripts", "testing", "om", paste0("results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.full), "it.",
                                    prop*100, "perc.csv")))
}
if(run == "jeff"){
  write.csv(results, paste0("results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.full), "it.",
                                    prop*100, "perc.csv"))
}
if(run == "CO"){
  write.csv(results, paste0("../results/results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.full), "it.",
                                    prop*100, "perc.csv"))
}














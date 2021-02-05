
# where to run code ("jeff", "CO", "mine")
run <- "jeff"
# which dataset ("anes", "cces", "framing")
ds <- "framing"
# which set of education categories ("an", "op") (just to differentiate the saved files)
ed.set <- "op"
# MAR or MNAR as missing data mechanism
mech <- "MAR"
# DV
dv <- "Educ"
# variables to insert NAs into
add.nas.columns <- c("Democrat", "Male", "Employed", "Income", "Age",
                     "Student", "Conservative", "Black", "Hisp", "Asian")
# imputation methods
methods <- c("hd.ord", "hot.deck", "amelia", "mice", "na.omit")
# number of iterations
mc.iterations <- 1100
# desired number of iterations (in case some levels need dropping)
mc.it.des <- 1000
# to include ".mult" in saved doc names if needed (saved typing it manually
# afterwards). Includes nothing if it's just Educ
if(length(dv) > 1){
  mult <- ".mult"
}else{
  mult <- ""
}

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
library(mice)

if(run == "mine"){
  library(here)
  load(here::here("functions", "OPMord.Rdata"))
  load(here::here("functions", "OPMcut.Rdata")) 
  load(here::here("functions", "hot.deck.ord.Rdata"))
  df_true <- read.csv(here::here("data", "experiment", "op.clean.csv"), stringsAsFactors = TRUE, check.names = FALSE)
}
if(run == "jeff"){
  load("OPMord.Rdata") 
  load("OPMcut.Rdata") 
  load("hot.deck.ord.Rdata") 
  df_true <- read.csv("op.clean.csv", stringsAsFactors = TRUE, check.names = FALSE)
}
if(run == "CO"){
  load("../data/OPMord.Rdata") 
  load("../data/OPMcut.Rdata") 
  load("../data/hot.deck.ord.Rdata")
  df_true <- read.csv("../data/op.clean.csv", stringsAsFactors = TRUE, check.names = FALSE)
}

df_true <- mutate(df_true,
                  Female = ifelse(gender == "Female", 1, 0),
                  Republican = ifelse(pid == "Republican", 1, 0),
                  Independent = ifelse(pid == "Independent", 1, 0),
                  Conservative = ifelse(ideol == "Conservative", 1, 0),
                  Liberal = ifelse(ideol == "Liberal", 1, 0),
                  Black = ifelse(race == "Black or African-American", 1, 0),
                  Hisp = ifelse(race == "Hispanic or Latino", 1, 0),
                  White = ifelse(race == "White", 1, 0),
                  Arab = ifelse(race == "Arab or Middle Eastern", 1, 0),
                  Asian = ifelse(race == "Asian", 1, 0),
                  Educ = ifelse(educ == "High school or lower", 1,
                         ifelse(educ == "Some college", 2,
                         ifelse(educ == "Associate degree", 3,
                         ifelse(educ == "Bachelor", 4, 5)))))
names(df_true)[names(df_true) == "age"] <- "Age"

df_true <- subset(df_true, 
                  select = c("Democrat", "Republican", "Independent", "Conservative", "Liberal",
                             "Black", "Hisp", "White", "Asian", "Male", "Employed", "Unemployed", 
                             "Retired", "Student", "Income", "Age", "Educ"))

nrow(df_true)

# identify and discard highly collinear variables
coll.var <- df_true %>% cor() %>% abs() %>% findCorrelation(., cutoff = .7) %>% sort() # no variables over .7 collinearity
# df_true <- df_true[,-c(coll.var)]                                                         
# extract EVs
all.evs <- colnames(df_true)[-which(colnames(df_true) == dv)]                         

# separate df_true into columns with and without NAs
no.nas <- df_true[,!names(df_true) %in% add.nas.columns]                                  
yes.nas <- df_true[,names(df_true) %in% add.nas.columns]

# the proportions of NAs to insert
prop <- .2
# number of imputations (https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul)
m <- prop*100

# true variable means
true <- sapply(df_true[, add.nas.columns], mean)
# the complete number of education levels
min.lev <- df_true$Educ %>% unique() %>% length()

# empty list to store OPMord output
OPMord.few <- list()
OPMord.full <- list()
# set up the percentage progress bar across the sampled numbers
pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3)
set.seed(123)
for(mc in 1:mc.iterations){
  # load the percentage progress bar into the loop
  setTxtProgressBar(pb, mc)
  # combine columns with NAs and columns without NAs and run ordinal polr() function on data
  data.amp <- cbind(no.nas, ampute(yes.nas, prop = prop, mech = mech)$amp)
  if(na.omit(data.amp)  %>% .$Educ %>% unique() %>% length() < min.lev){
    OPMord.few[[mc]] <- OPMord(data.amp, dv = dv, evs = all.evs)
  }else{
    OPMord.full[[mc]] <- OPMord(data.amp, dv = dv, evs = all.evs)
  }
}


if(length(OPMord.few) != 0){
  OPMord.full <- OPMord.full[-which(sapply(OPMord.full, is.null))]
    if(length(OPMord.full) < mc.it.des){
        stop("Fewer than", paste0(mc.it.des), "data sets with all levels")
    }
}

if(length(OPMord.full) > mc.it.des){
  OPMord.full <- sample(OPMord.full, mc.it.des)
}






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
                           dv = dv, OPMordOut = OPMord.full[[n]])
    }


start_time <- Sys.time()
hd.ord.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
  for(n in 1:length(OPMord.full)){
    setTxtProgressBar(pb, n)
    # run hot.deck.ord() on OPMcut education values, which scales the ordinal variable
    hd.ord.dat[[n]] <- hot.deck.ord(OPMcut.dat[[n]], m = m, ord = dv,
                                 sdCutoff = sd(OPMcut.dat[[n]]$Educ), method = "best.cell")
      }
end_time <- Sys.time()
hd.ord.time <- difftime(end_time, start_time, units = "mins")


start_time <- Sys.time()
hot.deck.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
  for(n in 1:length(OPMord.full)){
    setTxtProgressBar(pb, n)
    # run normal hot.deck() on original education values
    hot.deck.dat[[n]] <- hot.deck(OPMord.full[[n]]$data.full.nas, m = m,
                                   sdCutoff = sd(OPMord.full[[n]]$data.full.nas$Educ),
                                   method = "best.cell")
      }
end_time <- Sys.time()
hot.deck.time <- difftime(end_time, start_time, units = "mins")




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


runtime <- cbind(c(hd.ord.time, hot.deck.time, am.time, mice.time),
                   methods[1:4])


if(run == "mine"){
  write.csv(runtime, file = here::here("data", "experiment", 
                                 paste0(ds, ".", ed.set, ".", tolower(mech),
                                        mult, ".runtime.", 
                                        length(add.nas.columns), "var.",
                                        nrow(df_true), "n.", length(OPMord.full),
                                        "it.", prop*100, "perc.csv")))
}
if(run == "jeff"){
  write.csv(runtime, paste0(ds, ".", ed.set, ".", tolower(mech),
                            mult, ".runtime.", 
                            length(add.nas.columns), "var.",
                            nrow(df_true), "n.", length(OPMord.full),
                            "it.", prop*100, "perc.csv"))
}
if(run == "CO"){
  write.csv(runtime, paste0("../results/", ds, ".", ed.set, ".", tolower(mech),
                            mult, ".runtime.", 
                            length(add.nas.columns), "var.",
                            nrow(df_true), "n.", length(OPMord.full),
                            "it.", prop*100, "perc.csv"))
}



pb <- txtProgressBar(min = 1, max = length(OPMord.full), style = 3)
for(n in 1:length(OPMord.full)){
  setTxtProgressBar(pb, n)
  # empty dfs to store variable means
  # means.mice.orig <- means.am.orig <- means.hot.deck.cut <-means.hot.deck <- means.hd.ord <-
  #   data.frame(matrix(NA, m, length(add.nas.columns))) # empty dfs to store variable means
  means.mice.orig <- means.am.orig <- means.hot.deck <- means.hd.ord <-
    data.frame(matrix(NA, m, length(add.nas.columns)))
  # which rows and columns have NAs
  na.rows.cols <- which(is.na(OPMord.full[[n]]$data.full.nas), arr.ind=TRUE)
  # which column numbers have NAs in them
  na.cols <- na.rows.cols %>% .[,2] %>% unique()

  # fill in means dfs
  for (x in 1:length(na.cols)){
    for (i in 1:m){
      means.hd.ord[i,x] <- hd.ord.dat[[n]]$data[[i]] %>% .[, add.nas.columns[x]] %>% mean()
      # means.hot.deck.cut[i,x] <- hot.deck.cut.dat$data[[i]] %>%
      #   .[, add.nas.columns[x]] %>%
      #   mean()
      means.hot.deck[i,x] <- hot.deck.dat[[n]]$data[[i]] %>%
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
  # hot.deck.cut <- sapply(means.hot.deck.cut, mean)
  hot.deck <- sapply(means.hot.deck, mean)
  am.orig <- sapply(means.am.orig, mean)
  mice.orig <- sapply(means.mice.orig, mean)
  # take means of variables from data without NAs
  na.omit <- sapply(OPMord.full[[n]]$data.short.na.omit[, add.nas.columns], mean)

  # fill in results.list
  for (ss in 1:length(add.nas.columns)){
    results.list[[ss]][["hd.ord"]][n] <- hd.ord[ss]
    # results.list[[ss]][["hot.deck.cut"]][n] <- hot.deck.cut[ss]
    results.list[[ss]][["hot.deck"]][n] <- hot.deck[ss]
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
  write.csv(results, here::here("data", "experiment", 
                          paste0(ds, ".", ed.set, ".", tolower(mech), mult, ".results.", 
                                 length(add.nas.columns), "var.",
                                 nrow(df_true), "n.", length(OPMord.full),
                                 "it.", prop*100, "perc.csv")))
}
if(run == "jeff"){
  write.csv(results, paste0(ds, ".", ed.set, ".", tolower(mech), mult, ".results.", 
                            length(add.nas.columns), "var.",
                            nrow(df_true), "n.", length(OPMord.full),
                            "it.", prop*100, "perc.csv"))
}
if(run == "CO"){
  write.csv(results, paste0("../results/", ds, ".", ed.set, ".", tolower(mech),
                            mult, ".results.", 
                            length(add.nas.columns), "var.",
                            nrow(df_true), "n.", length(OPMord.full),
                            "it.", prop*100, "perc.csv"))
}














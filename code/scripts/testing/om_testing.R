
library(plyr)
library(dplyr)
library(hot.deck)
library(magrittr)
library(data.table)
library(tidyverse)
library(mice)
library(rlist)
library(Amelia)

load("OPMord.Rdata") 
load("OPMcut.Rdata") 
load("hot.deck.ord.Rdata") 

# Load the framing data
framing <- read.csv("second_framing_experiment.csv")

# Select needed columns
framing <- framing[, c("tb_supp", "tb", "pid", "educ", "inc", "age", "race", "gender", "empl", "ideol", "interest", "media", "part" )]
# following public affairs (ordinal, 4 levels), media interest (numeric, accumulative count of activities), participation (numeric, accumulative count of activities)

# Make all needed columns binary, i.e. numeric/integer (needed for hot.deck())
framing <- mutate(framing,
                  Dem = ifelse(pid == "Democrat", 1, 0),
                  Rep = ifelse(pid == "Republican", 1, 0),
                  Ind = ifelse(pid == "Independent", 1, 0),
                  Cons = ifelse(ideol == "Conservative", 1, 0),
                  Lib = ifelse(ideol == "Liberal", 1, 0),
                  Black = ifelse(race == "Black or African-American", 1, 0),
                  Hisp = ifelse(race == "Hispanic", 1, 0),
                  White = ifelse(race == "White", 1, 0),
                  Arab = ifelse(race == "Arabic", 1, 0),
                  Asian = ifelse(race == "Asian", 1, 0),
                  Female = ifelse(gender == "Female", 1, 0),
                  Male = ifelse(gender == "Male", 1, 0),
                  Empl = ifelse(empl == "Employed full time outside of the home" | empl == "Employed part time outside of the home", 1, 0),
                  Unempl = ifelse(empl == "Unemployed", 1, 0),
                  Ret = ifelse(empl == "Retired", 1, 0),
                  Stud = ifelse(empl == "Student", 1, 0)
)

# Select needed columns and save complete data under a different name
# all.cols <- c("Rep", "Cons", "White", "Female", "Empl", "interest", "media", "part", "inc", "age")
all.cols <- c("Dem", "Rep", "Ind", "Cons", "Lib", "Black", "Hisp", "White", "Asian", "Female", "Male", "Empl", "Unempl", "Ret", "interest", "media", "part", "inc", "age")
framing_true <- framing <- framing[, c(all.cols, "educ")]

add.nas.columns <- c("Rep", "inc", "age", "White", "Female", "interest") # variables to insert NAs into
# add.nas.columns <- c("Rep", "inc", "age", "White", "Female") # variables to insert NAs into
no.nas <- framing_true[,!names(framing_true) %in% add.nas.columns] # separate framing_true into columns with and without NAs
yes.nas <- framing_true[,names(framing_true) %in% add.nas.columns]

prop <- .8 # the proportions of NAs to insert
methods <- c("hd.ord", "hd.norm.orig", "amelia", "mice", "na.omit")
# methods <- c("hd.ord", "hd.norm.cut", "hd.norm.orig", "amelia", "mice", "na.omit")

true <- sapply(framing_true[, add.nas.columns], mean) # true variable means
OPMord.frame <- list()

mc.iterations <- 100
pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3) # creates the percentage progress bar across the sampled numbers

  for(mc in 1:mc.iterations){
    setTxtProgressBar(pb, mc) # loads the percentage progress bar into the loop
    framing.nas <- cbind(no.nas, ampute(yes.nas, prop = prop, mech = "MAR")$amp) # combine columns with NAs and columns without NAs
    OPMord.frame[[mc]] <- OPMord(data = framing.nas, dv = "educ", evs = all.cols) # run ordinal polr() function on data
    
      if(OPMord.frame[[mc]]$int.df %>% nrow() < framing_true$educ %>% unique() %>% length - 1){
        print("Fewer than original levels") # prints whenever int.df doesn't have all rows, which means it doesn't have all education levels (6 rows for 7 levels)
      }
  }




empty <- c() # empty vector to store data iterations in

  for (i in 1:mc.iterations){
    empty[i] <- OPMord.frame[[i]]$int.df %>% nrow() # extract number of rows of int.df
  }

table(empty) # show how many int.dfs don't have all education levels

  # remove all data that don't have all education levels and overwrite output
  if(all.equal(length(empty), sum(empty == length(unique(framing_true$educ))-1)) != TRUE){
    OPMord.frame <- list.remove(OPMord.frame, which(empty != length(unique(framing_true$educ))-1)) 
  }else{
    OPMord.frame <- OPMord.frame
  }

results.list <- rep(list(rep(list(c()), length(methods))), length(add.nas.columns)) # empty container to store results (list of lists of vectors)
names(results.list) <- add.nas.columns # name first list after variables with NAs

  for (t in 1:length(add.nas.columns)){
    names(results.list[[t]]) <- methods # name second lists after methods
  }





pb <- txtProgressBar(min = 1, max = length(OPMord.frame), style = 3) # creates the percentage progress bar across the sampled numbers

  for(n in 1:length(OPMord.frame)){
    setTxtProgressBar(pb, n) # loads the percentage progress bar into the loop
    na.rows.cols <- which(is.na(OPMord.frame[[n]]$data.full.nas), arr.ind=TRUE) # which rows and columns have NAs
    m <- na.rows.cols %>% nrow() # how many rows have NAs

#    am.orig.frame <- amelia(OPMord.frame[[n]]$data.full.nas, m = m, p2s = 0) # run amelia on original education values (ps2 sets the console printing)
    mice.orig.frame <- mice(OPMord.frame[[n]]$data.full.nas, m = m, maxit = 1, print = FALSE) # run mice on original education values (print sets the console 
  }


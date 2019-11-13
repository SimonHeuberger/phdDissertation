

library(plyr)
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

# Select needed columns and save complete data under a different name
framing_true <- framing <- framing[, c("Dem", "Rep", "Ind", "educ", "inc", "age", "Female", "Male", "Black", "Hisp", "White")]


# variables to insert NAs into. Two are binary, one is nominal, one is continuous
add.nas.columns <- c("Dem", "Rep", "inc", "age") 
no.nas <- framing_true[,!names(framing_true) %in% add.nas.columns] # separate framing_true into columns with and without NAs
yes.nas <- framing_true[,names(framing_true) %in% add.nas.columns]


prop <- .05 # the proportions of NAs to insert (for the loop)

methods <- c("true", "hd.ord", "hd.norm", "na.omit")

mc.iterations <- 10000

#results.list <- rep(list(data.frame(matrix(NA, length(methods), length(add.nas.columns)+1))), mc.iterations)

#winner.list <- rep(list(data.frame(matrix(NA, length(prop), length(add.nas.columns)))), mc.iterations)

#hot.deck.ord.frame.more.nas <- rep(list(list()), mc.iterations)

m <- round_any(prop*nrow(framing_true)*length(add.nas.columns), 10)
means.hd <- data.frame(matrix(NA, m, length(add.nas.columns)))
list.means.hd.norm <- list.means.hd.ord <- rep(list(means.hd), mc.iterations)

na.omit <- data.frame(matrix(NA, mc.iterations, length(add.nas.columns)))

pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3) # creates the percentage progress bar across the sampled numbers


for(mc in 1:mc.iterations){
  
  setTxtProgressBar(pb, mc) # loads the percentage progress bar into the loop
  
  set.seed(123)
  framing.nas <- cbind(no.nas, prodNA(yes.nas, noNA = prop)) # combine columns with NAs and columns without NAs
  na.rows.cols <- which(is.na(framing.nas), arr.ind=TRUE) # which rows and columns have NAs
  m <- na.rows.cols %>% nrow() # how many rows have NAs

  OPMord.frame.more.nas <- OPMord(data = na.omit(framing.nas), dv = "educ", evs = c("Dem", "Rep", "Ind", "inc", "age", "Female", "Male", "Black", "Hisp", "White")) # run ordinal polr() function on data

  OPMcut.frame.more.nas <- OPMcut(data = framing.nas, dv = "educ", OPMordOut = OPMord.frame.more.nas) # run function that replaces ordinal values with mid-cutpoints

  hot.deck.ord.frame.more.nas <- hot.deck.ord(OPMcut.frame.more.nas, m = m, ord = "educ", sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell") # run hot.deck.ord(), which scales the ordinal variable

  hot.deck.norm.frame.more.nas <- hot.deck(OPMcut.frame.more.nas, m = m, sdCutoff = sd(na.omit(framing.nas$educ)), method = "best.cell") # run normal hot.deck()

  na.cols <- na.rows.cols %>% .[,2] %>% unique() # which column numbers have NAs in them
  na.colnames <- names(framing.nas)[na.cols] # names of columns with NAs in them

    for (x in 1:length(add.nas.columns)){
      for(i in 1:m){
        list.means.hd.ord[[mc]][i,x] <- hot.deck.ord.frame.more.nas$data[[i]] %>% .[, add.nas.columns[x]] %>% mean()
        list.means.hd.norm[[mc]][i,x] <- hot.deck.norm.frame.more.nas$data[[i]] %>% .[, add.nas.columns[x]] %>% mean()
      }
    }

  na.omit[mc,] <- sapply(na.omit(framing.nas[, na.colnames]), mean) # na.omit variable means

}

#saveRDS(results.list, file = "list.ord.cut.mc.results.rds")
#saveRDS(winner.list, file = "list.ord.cut.mc.winner.rds")
#saveRDS(hot.deck.norm.frame.more.nas, file = "hot.deck.norm.frame.more.nas.rds")

saveRDS(list.means.hd.ord, file = "list.means.hd.ord.1000.rds")
saveRDS(list.means.hd.norm, file = "list.means.hd.norm.1000.rds")
saveRDS(na.omit, file = "na.omit.1000.rds")

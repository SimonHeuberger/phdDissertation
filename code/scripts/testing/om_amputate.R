
{ # so I can run this whole 'setup' code with one keystroke
library(dplyr)
library(hot.deck)
library(magrittr)
library(rlist)
library(Amelia)
library(caret)

load("OPMord.Rdata") 
load("OPMcut.Rdata") 
load("hot.deck.ord.Rdata") 

# Load the framing data
framing <- read.csv("second_framing_experiment.csv")

# Make all needed columns numeric/integer (needed for hot.deck())
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
                  Empl = ifelse(empl == "Employed full time outside of the home" |
                                  empl == "Employed part time outside of the home", 1, 0),
                  Unempl = ifelse(empl == "Unemployed", 1, 0),
                  Ret = ifelse(empl == "Retired", 1, 0),
                  Stud = ifelse(empl == "Student", 1, 0)
                  )


# Select needed columns and save complete data under a different name
# interest: following public affairs (ordinal, 4 levels)
# media: media consumption (numeric, accumulative count of activities)
# part: political participation (numeric, accumulative count of activities)
framing_true <- framing <- framing[, c("Dem", "Rep", "Ind", "Cons", "Lib", "Black", "Hisp",         
                                       "White", "Asian", "Female", "Male", "Empl", "Unempl",        
                                       "Ret", "Stud", "interest", "media", "part", "inc",           
                                       "age", "educ")]                                              


# identify and discard highly collinear variables
coll.var <- framing_true %>% cor() %>% abs() %>% findCorrelation(., cutoff = .7) %>% sort()         
framing_true <- framing_true[,-c(coll.var)]                                                         
# save name of all EVs
all.evs <- colnames(framing_true)[-which(colnames(framing_true) == "educ")]                         

# variables to insert NAs into
add.nas.columns <- c("Dem", "inc", "age", "White", "Female", "interest")                            
# separate framing_true into columns with and without NAs
no.nas <- framing_true[,!names(framing_true) %in% add.nas.columns]                                  
yes.nas <- framing_true[,names(framing_true) %in% add.nas.columns]

# the proportions of NAs to insert
prop <- .8     
# number of imputations (https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul)
m <- prop*100

# imputation methods
methods <- c("hd.ord", "hd.norm.orig", "amelia", "mice", "na.omit")                                 
# methods <- c("hd.ord", "hd.norm.cut", "hd.norm.orig", "amelia", "mice", "na.omit")

# true variable means
true <- sapply(framing_true[, add.nas.columns], mean)
}



calculation <- FALSE

if(calculation == TRUE){

# minimum number of rows int.df needs to have (see below) 
min.row <- framing_true$educ %>% unique() %>% length-1
# empty list to store OPMord output
OPMord.frame <- list()
mc.iterations <- 12500
# set up the percentage progress bar across the sampled numbers
pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3)                                       

  for(mc in 1:mc.iterations){
    # load the percentage progress bar into the loop
    setTxtProgressBar(pb, mc)                                                                       
    # combine columns with NAs and columns without NAs
    framing.nas <- cbind(no.nas, ampute(yes.nas, prop = prop, mech = "MAR")$amp)                    
    # run ordinal polr() function on data
    OPMord.frame[[mc]] <- OPMord(data = framing.nas, dv = "educ", evs = all.evs)                    

      # print whenever int.df doesn't have all rows, which means it doesn't have all education levels (6 rows for 7 levels)
      if(OPMord.frame[[mc]]$int.df %>% nrow() < min.row){                                           
        print("Fewer than original levels")                                                         
      }                                                                                       
  }


# empty vector to store numbers rows of int.dfs
empty <- c()
  # extract numbers of rows of int.dfs
  for (i in 1:mc.iterations){                                                                       
    empty[i] <- OPMord.frame[[i]]$int.df %>% nrow()                                           
  }
# show how many int.dfs don't have all education levels
table(empty)                                                                                        


  # remove all data that don't have all education levels and overwrite output
  if(all.equal(length(empty), sum(empty == min.row)) != TRUE){                                      
    OPMord.frame <- list.remove(OPMord.frame, which(empty != min.row)) 
  }else{
    OPMord.frame <- OPMord.frame
  }

# empty list of lists of vectors to store results
results.list <- rep(list(rep(list(c()), length(methods))), length(add.nas.columns))                 
# name first list after variables with NAs
names(results.list) <- add.nas.columns                                                              

  # name second lists after methods
  for (t in 1:length(add.nas.columns)){                                                             
    names(results.list[[t]]) <- methods 
  }



pb <- txtProgressBar(min = 1, max = length(OPMord.frame), style = 3)

  for(n in 1:length(OPMord.frame)){
    setTxtProgressBar(pb, n)
    # replace ordinal values with mid-cutpoints
    OPMcut.frame <- OPMcut(data = OPMord.frame[[n]]$data.full.nas,                                  
                           dv = "educ", OPMordOut = OPMord.frame[[n]]) 
    # run hot.deck.ord() on OPMcut education values, which scales the ordinal variable
    hd.ord.frame <- hot.deck.ord(OPMcut.frame, m = m, ord = "educ", 
                                 sdCutoff = sd(OPMcut.frame$educ), method = "best.cell")            
    # hd.norm.cut.frame <- hot.deck(OPMcut.frame, m = m, sdCutoff = sd(OPMcut.frame$educ), 
    #                               method = "best.cell") # run normal hot.deck() on OPMcut values
    # run normal hot.deck() on original education values
    hd.norm.orig.frame <- hot.deck(OPMord.frame[[n]]$data.full.nas, m = m,                          
                                   sdCutoff = sd(OPMord.frame[[n]]$data.full.nas$educ), 
                                   method = "best.cell") 
    # run amelia() on original education values (ps2 sets the console printing)
    am.orig.frame <- amelia(OPMord.frame[[n]]$data.full.nas, m = m, p2s = 0)                        
    # run mice() on original education values (print sets the console)
    mice.orig.frame <- mice(OPMord.frame[[n]]$data.full.nas, m = m, print = FALSE)                  

    # empty dfs to store variable means
    # means.mice.orig <- means.am.orig <- means.hd.norm.cut <-means.hd.norm.orig <- means.hd.ord <-
    #   data.frame(matrix(NA, m, length(add.nas.columns))) # empty dfs to store variable means
    means.mice.orig <- means.am.orig <- means.hd.norm.orig <- means.hd.ord <-                       
      data.frame(matrix(NA, m, length(add.nas.columns))) 
    # which rows and columns have NAs
    na.rows.cols <- which(is.na(OPMord.frame[[n]]$data.full.nas), arr.ind=TRUE)                     
    # which column numbers have NAs in them
    na.cols <- na.rows.cols %>% .[,2] %>% unique()                                                  
  
      # fill in means dfs
      for (x in 1:length(na.cols)){                                                                 
        for (i in 1:m){
          means.hd.ord[i,x] <- hd.ord.frame$data[[i]] %>% .[, add.nas.columns[x]] %>% mean()
          # means.hd.norm.cut[i,x] <- hd.norm.cut.frame$data[[i]] %>% 
          #   .[, add.nas.columns[x]] %>% 
          #   mean()
          means.hd.norm.orig[i,x] <- hd.norm.orig.frame$data[[i]] %>%
            .[, add.nas.columns[x]] %>%
            mean()
          means.am.orig[i,x] <- am.orig.frame$imputations[[i]] %>%
            .[, add.nas.columns[x]] %>%
            mean()
          means.mice.orig[i,x] <- mice::complete(mice.orig.frame, action = i) %>%
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
    na.omit <- sapply(OPMord.frame[[n]]$data.short.na.omit[, add.nas.columns], mean)                

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

saveRDS(results.list, file = paste0("results.", length(add.nas.columns), "var.",
                                    nrow(framing_true), "n.", length(OPMord.frame), "it.",
                                    prop*100, "perc.rds"))





}else{




##### Results Analysis #####

# output <- readRDS("results.6var.1003n.10566it.80perc.rds")
# prop <- .8
output <- readRDS("results.6var.1003n.12462it.50perc.rds")
prop <- .5
# output <- readRDS("results.6var.1003n.12500it.20perc.rds")
# prop <- .2

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
          output[[add.nas.columns[xs]]][[methods[vv]]] %>% mean()
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
write.csv(results, paste0("results.", prop*100, ".perc.csv"))

}




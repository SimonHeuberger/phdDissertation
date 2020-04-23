
# run <- c("jeff", "CO", "mine")
run <- "mine"
# number of iterations
mc.iterations <- 10

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
  load(here("functions", "own.NA.Rdata"))
  load(here("functions", "own.NA.rows.Rdata"))
  df <- read.csv(here("data", "framing", "second_framing_experiment.csv"))
}
if(run == "jeff"){
  load("OPMord.Rdata") 
  load("OPMcut.Rdata") 
  load("hot.deck.ord.Rdata") 
  load("own.NA.Rdata")
  load("own.NA.rows.Rdata")
  df <- read.csv("second_framing_experiment.csv")
}
if(run == "CO"){
  load("../data/OPMord.Rdata") 
  load("../data/OPMcut.Rdata") 
  load("../data/hot.deck.ord.Rdata") 
  load("../data/own.NA.Rdata")
  load("../data/own.NA.rows.Rdata")
  df <- read.csv("../data/second_framing_experiment.csv")
}



# Make all needed columns numeric/integer (needed for hot.deck())
df <- mutate(df,
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
df_true  <- df[, c("Dem", "Rep", "Ind", "Cons", "Lib", "Black", "Hisp",         
                                       "White", "Asian", "Female", "Male", "Empl", "Unempl",        
                                       "Ret", "Stud", "interest", "media", "part", "inc",           
                                       "age", "educ")]                                              


# identify and discard highly collinear variables
coll.var <- df_true %>% cor() %>% abs() %>% findCorrelation(., cutoff = .7) %>% sort()         
df_true <- df_true[,-c(coll.var)]                                                         
# save name of all EVs and add NAs to columns
add.nas.columns <- all.evs <- colnames(df_true)[-which(colnames(df_true) == "educ")]                         


# the proportions of NAs to insert
prop <- .5
# number of imputations (https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul)
m <- prop*100

# imputation methods
methods <- c("hd.ord", "hd.norm.orig", "amelia", "mice", "na.omit")                                 
# methods <- c("hd.ord", "hd.norm.cut", "hd.norm.orig", "amelia", "mice", "na.omit")
# true variable means
true <- sapply(df_true[, add.nas.columns], mean)



list.own.NA <- list()
# empty list to store OPMord output
OPMord.dat <- list()
# the complete number of education levels
min.lev <- df_true$educ %>% unique() %>% length()
# set up the percentage progress bar across the sampled numbers
pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3)                                       


  for(mc in 1:mc.iterations){
    # load the percentage progress bar into the loop
    setTxtProgressBar(pb, mc)                                                                       
    # combine columns with NAs and columns without NAs
    list.own.NA[[mc]] <- own.NA.rows(df = df_true, prop = prop, colNA = add.nas.columns)
    # run ordinal polr() function on data
    OPMord.dat[[mc]] <- OPMord(data = list.own.NA[[mc]], dv = "educ", evs = all.evs)                    
      # print whenever int.df doesn't have all rows, which means it doesn't have all education levels (6 rows for 7 levels)
      if(OPMord.dat[[mc]]$data.short.na.omit %>% .$educ %>% unique() %>% length() < min.lev){                                           
        print("Fewer than original levels")                                                         
      }                                                                                       
  }


empty <- c()
for (i in 1:mc.iterations){                                                                       
  empty[i] <- OPMord.dat[[i]]$data.short.na.omit %>% .$educ %>% unique() %>% length()
}
# show how many imputed data sets don't have all education levels
table(empty)                                                                                        


# remove all data that don't have all education levels and overwrite output
if(all.equal(length(empty), sum(empty == min.lev)) != TRUE){                                      
  OPMord.dat <- list.remove(OPMord.dat, which(empty != min.lev))
  list.own.NA <- list.remove(list.own.NA, which(empty != min.lev))
}else{
  OPMord.dat <- OPMord.dat
  list.own.NA <- list.own.NA
}

OPMord.dat %>% length()
list.own.NA %>% length()

# save all amputed output
if(run == "mine"){
  saveRDS(list.own.NA, file = here("scripts", "testing", "om", paste0("own.NA.", length(add.nas.columns), "var.",
                                      nrow(df_true), "n.", length(OPMord.dat), "it.",
                                      prop*100, "perc.rds")))
}
if(run == "jeff"){
  saveRDS(list.own.NA, file = paste0("own.NA.", length(add.nas.columns), "var.",
                                     nrow(df_true), "n.", length(OPMord.dat), "it.",
                                     prop*100, "perc.rds"))
}
if(run == "CO"){
  saveRDS(list.own.NA, file = paste0("../results/own.NA.", length(add.nas.columns), "var.",
                                     nrow(df_true), "n.", length(OPMord.dat), "it.",
                                     prop*100, "perc.rds"))
}
  


# empty list of lists of vectors to store results
results.list <- rep(list(rep(list(c()), length(methods))), length(add.nas.columns))                 
# name first list after variables with NAs
names(results.list) <- add.nas.columns                                                              

  # name second lists after methods
  for (t in 1:length(add.nas.columns)){                                                             
    names(results.list[[t]]) <- methods 
  }

# empty list to store data.nas data frames
list.data.nas <- list()

pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)

  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    list.data.nas[[n]] <- OPMord.dat[[n]]$data.full.nas
    # replace ordinal values with mid-cutpoints
    OPMcut.dat <- OPMcut(data = OPMord.dat[[n]]$data.full.nas,                                  
                           dv = "educ", OPMordOut = OPMord.dat[[n]]) 
    # run hot.deck.ord() on OPMcut education values, which scales the ordinal variable
    hd.ord.dat <- hot.deck.ord(OPMcut.dat, m = m, ord = "educ", 
                                 sdCutoff = sd(OPMcut.dat$educ), method = "best.cell")            
    # hd.norm.cut.dat <- hot.deck(OPMcut.dat, m = m, sdCutoff = sd(OPMcut.dat$educ), 
    #                               method = "best.cell") # run normal hot.deck() on OPMcut values
    # run normal hot.deck() on original education values
    hd.norm.orig.dat <- hot.deck(OPMord.dat[[n]]$data.full.nas, m = m,                          
                                   sdCutoff = sd(OPMord.dat[[n]]$data.full.nas$educ), 
                                   method = "best.cell") 
    # run amelia() on original education values (ps2 sets the console printing)
    am.orig.dat <- amelia(OPMord.dat[[n]]$data.full.nas, m = m, p2s = 0)                        
    # run mice() on original education values (print sets the console)
    mice.orig.dat <- mice(OPMord.dat[[n]]$data.full.nas, m = m, print = FALSE)                  

    # empty dfs to store variable means
    # means.mice.orig <- means.am.orig <- means.hd.norm.cut <-means.hd.norm.orig <- means.hd.ord <-
    #   data.frame(matrix(NA, m, length(add.nas.columns))) # empty dfs to store variable means
    means.mice.orig <- means.am.orig <- means.hd.norm.orig <- means.hd.ord <-                       
      data.frame(matrix(NA, m, length(add.nas.columns))) 
    # which rows and columns have NAs
    na.rows.cols <- which(is.na(OPMord.dat[[n]]$data.full.nas), arr.ind=TRUE)                     
    # which column numbers have NAs in them
    na.cols <- na.rows.cols %>% .[,2] %>% unique()                                                  
  
      # fill in means dfs
      for (x in 1:length(na.cols)){                                                                 
        for (i in 1:m){
          means.hd.ord[i,x] <- hd.ord.dat$data[[i]] %>% .[, add.nas.columns[x]] %>% mean()
          # means.hd.norm.cut[i,x] <- hd.norm.cut.dat$data[[i]] %>% 
          #   .[, add.nas.columns[x]] %>% 
          #   mean()
          means.hd.norm.orig[i,x] <- hd.norm.orig.dat$data[[i]] %>%
            .[, add.nas.columns[x]] %>%
            mean()
          means.am.orig[i,x] <- am.orig.dat$imputations[[i]] %>%
            .[, add.nas.columns[x]] %>%
            mean()
          means.mice.orig[i,x] <- mice::complete(mice.orig.dat, action = i) %>%
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
    na.omit <- sapply(OPMord.dat[[n]]$data.short.na.omit[, add.nas.columns], mean)                

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



if(run == "mine"){
  # save results
  saveRDS(results.list, file = here("scripts", "testing", "om", paste0("results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.rds")))
  # save data.nas data frames
  saveRDS(list.data.nas, file = here("scripts", "testing", "om", paste0("data.nas.", length(add.nas.columns), "var.",
                                      nrow(df_true), "n.", length(OPMord.dat), "it.",
                                      prop*100, "perc.rds")))
}
if(run == "jeff"){
  # save results
  saveRDS(results.list, file = paste0("results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.rds"))
  # save data.nas data frames
  saveRDS(list.data.nas, file = paste0("data.nas.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.rds"))
}
if(run == "CO"){
  # save results
  saveRDS(results.list, file = paste0("../results/results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.rds"))
    # save data.nas data frames
  saveRDS(list.data.nas, file = paste0("../results/data.nas.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.rds"))
}




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
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.csv")))
}
if(run == "jeff"){
  write.csv(results, paste0("results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.csv"))
}
if(run == "CO"){
  write.csv(results, paste0("../results/results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.csv"))
}



          
        








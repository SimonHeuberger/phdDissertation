
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
  load(here("data", "cces", "cces_cleaned.Rdata"))
}
if(run == "jeff"){
  load("OPMord.Rdata") 
  load("OPMcut.Rdata") 
  load("hot.deck.ord.Rdata") 
  load("cces_cleaned.Rdata")
}
if(run == "CO"){
  load("../data/OPMord.Rdata") 
  load("../data/OPMcut.Rdata") 
  load("../data/hot.deck.ord.Rdata")
  load("../data/cces_cleaned.Rdata")
}


# birthyr -- age
# gender -- gender
# race -- race
# faminc -- income
# employ -- occupation
# pid3 -- party ID
# educ -- education
# newsint -- follows politics in media
# sexuality -- sexual orientation
# edloan -- student debtor
# internethome -- internet acces at home
# ideo5 -- ideology
# religpew -- religion
# ownhome -- home ownership
# marstat -- marital status


# library(foreign)
# df <- read.dta("/Users/simonheuberger/Downloads/CCES16_Common_OUTPUT_Feb2018_VV.dta")
# df <- dplyr::select(df, birthyr, gender, race, faminc, employ, pid3, educ, newsint, sexuality, 
#                     edloan, internethome, ideo5, religpew, ownhome, marstat)
# save("df", file = here("data", "cces", "cces_cleaned.Rdata"))
# The full CCES file is almost 600 MB. I didn't want to keep syncing and
# transferring that, so I reduced it to the columns I need. The full file is
# outside of Drive in /Downloads


df <- na.omit(df) # 11,000+ NAs in edloan
summary(is.na(df)) # No NAs

# turn birth years into age
df$birthyr <- 2018 - df$birthyr

df <- rename(df, Age = birthyr, gender = gender, race = race, Inc = faminc, occupation = employ,
             pid = pid3, Educ = educ, Interest = newsint, sexuality = sexuality, studLoans = edloan,
             internetHome = internethome, ideology = ideo5, religion = religpew, ownHome = ownhome, 
             marStat = marstat)
nrow(df) # [1] 52393 observations

# remove observations with refused, missing etc.
df <- filter(df, !Inc %in% c("Prefer not to say"),
             !pid %in% c("Not sure"),
             !Interest %in% c("Don't know"),
             !sexuality %in% c("Prefer not to say"),
             !ideology %in% c("Not sure"))

nrow(df) # [1] 42205 observations

# refactor to get rid of unneeded levels
for (i in 1:ncol(df)){
    df[,i] <- factor(df[,i])
  }

df$Age <- df$Age %>% as.character %>% as.numeric

df <- mutate(df,
             Dem = ifelse(pid == "Democrat", 1, 0),
             Rep = ifelse(pid == "Republican", 1, 0),
             Ind = ifelse(pid == "Independent", 1, 0),
             Black = ifelse(race == "Black", 1, 0),
             Hisp = ifelse(race == "Hispanic", 1, 0),
             White = ifelse(race == "White", 1, 0),
             Asian = ifelse(race == "Asian" | race == "Native American", 1, 0),
             Female = ifelse(gender == "Female", 1, 0),
             Male = ifelse(gender == "Male", 1, 0),
             Empl = ifelse(occupation == "Full-time", 1, 0),
             Unempl = ifelse(occupation == "Unemployed", 1, 0),
             Ret = ifelse(occupation == "Retired", 1, 0),
             Stud = ifelse(occupation == "Student", 1, 0),
             Straight = ifelse(sexuality == "Heterosexual / straight", 1, 0),
             Gay = ifelse(sexuality == "Lesbian / gay woman" | sexuality == "Gay man", 1, 0),
             Bisexual = ifelse(sexuality == "Bisexual", 1, 0),
             StudLoans = ifelse(studLoans == "Yes", 1, 0),
             InternetHome = ifelse(internetHome == "Broadband" | internetHome == "Dial-up", 1, 0),
             Liberal = ifelse(ideology == "Very liberal" | ideology == "Liberal", 1, 0),
             Conservative = ifelse(ideology == "Conservative" | ideology == "Very conservative", 1, 0),
             Moderate = ifelse(ideology == "Moderate", 1, 0),
             Religious = ifelse(religion == "Protestant" | religion == "Roman Catholic" | religion == "Mormon" | religion == "Eastern or Greek Orthodox" | 
                                  religion == "Jewish" | religion == "Muslim" | religion == "Buddhist" | religion == "Hindu", 1, 0),
             NotReligious = ifelse(religion == "Atheist"  | religion == "Agnostic" | religion == "Nothing in particular" | religion == "Something else", 1, 0),
             OwnHome = ifelse(ownHome == "Own", 1, 0),
             RentHome = ifelse(ownHome == "Rent", 1, 0),
             Married = ifelse(marStat == "Married" | marStat == "Domestic partnership", 1, 0),
             Separated = ifelse(marStat == "Separated" | marStat == "Divorced" | marStat == "Widowed", 1, 0),
             Single = ifelse(marStat == "Single", 1, 0)
)

df <- mutate(df,Inc = ifelse(Inc == "Less than $10,000", 1,
                      ifelse(Inc == "$10,000 - $19,999", 2,
                      ifelse(Inc == "$20,000 - $29,999", 3,
                      ifelse(Inc == "$30,000 - $39,999", 4,
                      ifelse(Inc == "$40,000 - $49,999", 5,
                      ifelse(Inc == "$50,000 - $59,999", 6,
                      ifelse(Inc == "$60,000 - $69,999", 7,
                      ifelse(Inc == "$70,000 - $79,999", 8,
                      ifelse(Inc == "$80,000 - $99,999", 9,
                      ifelse(Inc == "$100,000 - $119,999", 10,
                      ifelse(Inc == "$120,000 - $149,999", 11, 12))))))))))))

df <- mutate(df,Educ = ifelse(Educ == "No HS", 1,
                       ifelse(Educ == "High school graduate", 2,
                       ifelse(Educ == "Some college", 3,
                       ifelse(Educ == "2-year", 4,
                       ifelse(Educ == "4-year", 5, 6))))))

df <- mutate(df, Interest = ifelse(Interest == "Hardly at all", 1,
                            ifelse(Interest == "Only now and then", 2,
                            ifelse(Interest == "Some of the time", 3, 4))))

df_true <- df[, c("Dem", "Rep", "Black", "Hisp",
                  "White", "Asian", "Female", "Male", "Empl", "Unempl",        
                  #"Ret", # collinear with Age
                  #"Ind", # collinear with Dem
                  "Stud", "Straight", "Gay", "Bisexual", "StudLoans",
                  "InternetHome", "Liberal", "Conservative", "Moderate",
                  "Religious", "NotReligious", "OwnHome", "RentHome",
                  "Married", "Separated", "Single",
                  "Interest", "Inc", "Age", "Educ")]                                              


# save the data for when I want to run things for all observations
# saveRDS(df_true, here("data", "cces", "cces_all.rds"))


# take a sample of 1000 observations, then save it, to try to get around the CO RAM collapses
# set.seed(134)
# df_true$Educ %>% unique %>% length
# cces_1000 <- sample(nrow(df_true), 1000) %>% df_true[.,]
# cces_1000$Educ %>% unique %>% length
# saveRDS(cces_1000, here("data", "cces", "cces_1000.rds"))

# identify and discard highly collinear variables
coll.var <- df_true %>% cor() %>% abs() %>% findCorrelation(., cutoff = .7) %>% sort()         
df_true <- df_true[,-c(coll.var)]                                                         
# save name of all EVs
all.evs <- colnames(df_true)[-which(colnames(df_true) == "Educ")]                         

# variables to insert NAs into
add.nas.columns <- c("Dem", "Inc", "Age", "Male", "Interest")                            
# separate df_true into columns with and without NAs
no.nas <- df_true[,!names(df_true) %in% add.nas.columns]                                  
yes.nas <- df_true[,names(df_true) %in% add.nas.columns]

# the proportions of NAs to insert
prop <- .2
# number of imputations (https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul)
m <- prop*100

# imputation methods
methods <- c("hd.ord", "hot.deck", "amelia", "mice", "na.omit")                                 

# true variable means
true <- sapply(df_true[, add.nas.columns], mean)
# the complete number of education levels
min.lev <- df_true$Educ %>% unique() %>% length()

# empty list to store OPMord output
OPMord.dat <- list()
# set up the percentage progress bar across the sampled numbers
pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3)
for(mc in 1:mc.iterations){
  # load the percentage progress bar into the loop
  setTxtProgressBar(pb, mc)
  # combine columns with NAs and columns without NAs and run ordinal polr() function on data
  OPMord.dat[[mc]] <- cbind(no.nas, ampute(yes.nas, prop = prop, mech = "MAR")$amp) %>%
    OPMord(., dv = "Educ", evs = all.evs)
    # print whenever int.df doesn't have all rows, which means it doesn't have all education levels (6 rows for 7 levels)
    if(OPMord.dat[[mc]]$data.short.na.omit %>% .$Educ %>% unique() %>% length() < min.lev){
      print("Fewer than original levels")
    }
  rm(amputed)
  rm(df.nas)
}


empty <- c()
for (i in 1:mc.iterations){                                                                       
  empty[i] <- OPMord.dat[[i]]$data.short.na.omit %>% .$Educ %>% unique() %>% length()
}
# show how many imputed data sets don't have all education levels
table(empty)


# remove all data that don't have all education levels and overwrite output
if(all.equal(length(empty), sum(empty == min.lev)) != TRUE){                                      
  OPMord.dat <- list.remove(OPMord.dat, which(empty != min.lev))
}else{
  OPMord.dat <- OPMord.dat
}

rm(empty)

OPMord.dat %>% length()

# empty list of lists of vectors to store results
results.list <- rep(list(rep(list(c()), length(methods))), length(add.nas.columns))                 
# name first list after variables with NAs
names(results.list) <- add.nas.columns                                                              

  # name second lists after methods
  for (t in 1:length(add.nas.columns)){                                                             
    names(results.list[[t]]) <- methods 
  }



OPMcut.dat <- list()

pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    # replace ordinal values with mid-cutpoints
    OPMcut.dat[[n]] <- OPMcut(data = OPMord.dat[[n]]$data.full.nas,                                  
                           dv = "Educ", OPMordOut = OPMord.dat[[n]]) 
    }


start_time <- Sys.time()
hd.ord.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    # run hot.deck.ord() on OPMcut education values, which scales the ordinal variable
    hd.ord.dat[[n]] <- hot.deck.ord(OPMcut.dat[[n]], m = m, ord = "Educ", 
                                 sdCutoff = sd(OPMcut.dat[[n]]$Educ), method = "best.cell")
      }
end_time <- Sys.time()
hd.ord.time <- difftime(end_time, start_time, units = "mins")


start_time <- Sys.time()
hot.deck.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    # run normal hot.deck() on original education values
    hot.deck.dat[[n]] <- hot.deck(OPMord.dat[[n]]$data.full.nas, m = m,                          
                                   sdCutoff = sd(OPMord.dat[[n]]$data.full.nas$Educ), 
                                   method = "best.cell") 
      }
end_time <- Sys.time()
hot.deck.time <- difftime(end_time, start_time, units = "mins")


start_time <- Sys.time()
am.orig.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    # run amelia() on original education values (ps2 sets the console printing)
    am.orig.dat[[n]] <- amelia(OPMord.dat[[n]]$data.full.nas, m = m, p2s = 0)
      }
end_time <- Sys.time()
am.time <- difftime(end_time, start_time, units = "mins")


start_time <- Sys.time()
mice.orig.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    # run mice() on original education values (print sets the console)
    mice.orig.dat[[n]] <- mice(OPMord.dat[[n]]$data.full.nas, m = m, print = FALSE)
      }
end_time <- Sys.time()
mice.time <- difftime(end_time, start_time, units = "mins")


runtime <- cbind(c(hd.ord.time, hot.deck.time, am.time, mice.time),
                   methods[1:4])


if(run == "mine"){
  write.csv(runtime, file = here("scripts", "testing", "om", paste0("runtime.", length(add.nas.columns), "var.",
                          nrow(df_true), "n.", length(OPMord.dat), "it.",
                          prop*100, "perc.csv")))
}
if(run == "jeff"){
  write.csv(runtime, paste0("runtime.", length(add.nas.columns), "var.",
                          nrow(df_true), "n.", length(OPMord.dat), "it.",
                          prop*100, "perc.csv"))
}
if(run == "CO"){
  write.csv(runtime, paste0("../results/runtime.", length(add.nas.columns), "var.",
                          nrow(df_true), "n.", length(OPMord.dat), "it.",
                          prop*100, "perc.csv"))
}



pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
for(n in 1:length(OPMord.dat)){
  setTxtProgressBar(pb, n)
  # empty dfs to store variable means
  # means.mice.orig <- means.am.orig <- means.hot.deck.cut <-means.hot.deck <- means.hd.ord <-
  #   data.frame(matrix(NA, m, length(add.nas.columns))) # empty dfs to store variable means
  means.mice.orig <- means.am.orig <- means.hot.deck <- means.hd.ord <-
    data.frame(matrix(NA, m, length(add.nas.columns)))
  # which rows and columns have NAs
  na.rows.cols <- which(is.na(OPMord.dat[[n]]$data.full.nas), arr.ind=TRUE)
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
  na.omit <- sapply(OPMord.dat[[n]]$data.short.na.omit[, add.nas.columns], mean)

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
if(run == "mine"){
  saveRDS(results.list, file = here("scripts", "testing", "om", paste0("results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.rds")))
}
if(run == "jeff"){
  saveRDS(results.list, file = paste0("results.", length(add.nas.columns), "var.",
                                    nrow(df_true), "n.", length(OPMord.dat), "it.",
                                    prop*100, "perc.rds"))
}
if(run == "CO"){
  saveRDS(results.list, file = paste0("../results/results.", length(add.nas.columns), "var.",
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














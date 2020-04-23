
# run <- c("jeff", "CO", "mine")
run <- "mine"
# number of iterations
mc.iterations <- 10

library(readstata13)
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
  df <- read.dta13(here("data", "anes", "anes_timeseries_2016.dta"), generate.factors=T, nonint.factors = TRUE)
}
if(run == "jeff"){
  load("OPMord.Rdata") 
  load("OPMcut.Rdata") 
  load("hot.deck.ord.Rdata") 
  df <- read.dta13("anes_timeseries_2016.dta", generate.factors=T, nonint.factors = TRUE)
}
if(run == "CO"){
  load("../data/OPMord.Rdata") 
  load("../data/OPMcut.Rdata") 
  load("../data/hot.deck.ord.Rdata")
  df <- read.dta13("../data/anes_timeseries_2016.dta", generate.factors=T, nonint.factors = TRUE)
}


# V161267 -- age
# V161342 -- gender
# V161310x -- race
# V161361x -- income
# V161276x -- occupation
# V161155 -- party ID
# V161270 -- education
# V162257 -- follows politics in media

# V161026 -- voted for president in 2016
# V161080, V161080x -- approval of Conggress handling its job
# V161081 -- are things in country on right track
# V161112 -- has health insurance
# V161126 -- ideology
# V161221 -- is global warming happening
# V161241 -- religious
# V161268 -- marital status
# V161326 -- internet use at home
# V161334 -- home ownership
# V162011 -- go to political rally/meetings
# V162014 -- contribuete money to specific candidate campaign

df <- dplyr::select(df, V161267, V161342, V161310x, V161361x, V161276x, V161155, V161270, V162257,
                    V161026, V161080, V161081, V161112, V161126, V161221, V161241, V161268,
                    V161326, V161334, V162011, V162014)

summary(is.na(df)) # No NAs

df <- rename(df, age = V161267, gender = V161342, race = V161310x, income =  V161361x, occupation = V161276x, pid = V161155,
             education = V161270, interest = V162257, votePres2016 = V161026, congressHandleJob = V161080, countryRightTrack = V161081,
             healthInsurance = V161112, ideology = V161126, globalWarming = V161221, religious = V161241, marStat = V161268,
             internetHome = V161326, ownHome = V161334, rally = V162011, donate = V162014)
nrow(df) # [1] 4270 observations


# remove observations with refused, missing etc.
df <- filter(df, 
             !age %in% c("-9. RF (year of birth)", "-8. DK (year of birth, FTF only)"),
             !gender %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !race %in% c("-9. Missing", "6. Other non-Hispanic incl multiple races [WEB: blank 'Other' counted as a race]"),
             !income %in% c("-9. Refused", "-5. Interview breakoff (sufficient partial IW)"),
             !occupation %in% c("-9. Refused employment status"),
             !pid %in% c("-9. Refused", "-8. Don't know (FTF only)", "0. No preference (FTF ONLY)"),
             !education %in% c("95. Other SPECIFY", "-9. Refused"),
             !interest %in% c("-9. Refused", "-8. Don't know", "-7. No post data, deleted due to incomplete IW", "-6. No post-election interview"),
             !votePres2016 %in% c("-9. Refused", "-8. Don't know (FTF only)", "-1. Inap, 1-3 in V161024x"),
             !congressHandleJob %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !countryRightTrack %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !healthInsurance %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !ideology %in% c("-9. Refused", "-8. Don't know (FTF only)", "99. Haven't thought much about this (FTF ONLY: DO NOT PROBE)"),
             !globalWarming %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !religious %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !marStat %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !internetHome %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !ownHome %in% c("-9. Refused", "-8. Don't know (FTF only)", "4. Some other arrangement"),
             !rally %in% c("-9. Refused", "-7. No post data, deleted due to incomplete IW", "-6. No post-election interview"),
             !donate %in% c("-9. Refused", "-8. Don't know", "-7. No post data, deleted due to incomplete IW", "-6. No post-election interview")
             )

# refactor to get rid of unneeded levels
# for (i in 1:ncol(df)){
#     df[,i] <- factor(df[,i])
# }

# df$age <- revalue(df$age, c("90. Age 90 or older" = "90"))
df$age <- as.numeric(as.character(df$age))

df <- mutate(df,
             Dem = ifelse(pid == "1. Democrat", 1, 0),
             Rep = ifelse(pid == "2. Republican", 1, 0),
             Ind = ifelse(pid == "3. Independent", 1, 0),
             Black = ifelse(race == "2. Black, non-Hispanic", 1, 0),
             Hisp = ifelse(race == "5. Hispanic", 1, 0),
             White = ifelse(race == "1. White, non-Hispanic", 1, 0),
             Asian = ifelse(race == "3. Asian, native Hawaiian or other Pacif Islr,non-Hispanic", 1, 0),
             Female = ifelse(gender == "2. Female", 1, 0),
             Male = ifelse(gender == "1. Male", 1, 0),
             Empl = ifelse(occupation == "1. R working now (if also retired, disabled, homemaker or student, working 20 or more hrs/wk)", 1, 0),
             Unempl = ifelse(occupation == "4. R unemployed", 1, 0),
             Ret = ifelse(occupation == "5. R retired (if also working, working <20 hrs/wk)", 1, 0),
             Stud = ifelse(occupation == "8. R student (if also working, working <20 hrs/wk)", 1, 0),
             VotePres2016 = ifelse(votePres2016 == "1. Yes, voted for President", 1, 0),
             CongressJob = ifelse(congressHandleJob == "1. Approve", 1, 0),
             CountryTrack = ifelse(countryRightTrack == "1. Right direction", 1, 0),
             HealthIns = ifelse(healthInsurance == "1. Yes", 1, 0),
             GlobWarm = ifelse(globalWarming == "1. Has probably been happening", 1, 0),
             Religious = ifelse(religious == "1. Important", 1, 0),
             InternetHome = ifelse(internetHome == "1. Yes", 1, 0),
             OwnHome = ifelse(ownHome == "2. Pay mortgage" | ownHome == "3. Own home with no payments due", 1, 0),
             RentHome = ifelse(ownHome == "1. Pay rent", 1, 0),
             Rally = ifelse(rally == "1. Yes", 1, 0),
             Donate = ifelse(donate == "1. Yes", 1, 0),
             Liberal = ifelse(ideology == "1. Extremely liberal" | ideology == "2. Liberal" | ideology == "3. Slightly liberal", 1, 0),
             Conservative = ifelse(ideology == "5. Slightly conservative" | ideology == "6. Conservative" | ideology == "7. Extremely conservative", 1, 0),
             Moderate = ifelse(ideology == "4. Moderate, middle of the road", 1, 0),
             Married = ifelse(marStat == "1. Married: spouse present", 1, 0),
             Separated = ifelse(marStat == "2. Married: spouse absent [FTF ONLY: volunteered]" | marStat == "3. Widowed" | marStat == "4. Divorced" | marStat == "5. Separated", 1, 0),
             Single = ifelse(marStat == "6. Never married", 1, 0)
             )

df <- mutate(df,inc = ifelse(income == "01. Under $5,000", 1,
                      ifelse(income == "02. $5,000-$9,999", 2,
                      ifelse(income == "03. $10,000-$12,499", 3,
                      ifelse(income == "04. $12,500-$14,999", 4,
                      ifelse(income == "05. $15,000-$17,499", 5,
                      ifelse(income == "06. $17,500-$19,999", 6,
                      ifelse(income == "07. $20,000-$22,499", 7,
                      ifelse(income == "08. $22,500-$24,999", 8,
                      ifelse(income == "09. $25,000-$27,499", 9,
                      ifelse(income == "10. $27,500-$29,999", 10,
                      ifelse(income == "11. $30,000-$34,999", 11,
                      ifelse(income == "12. $35,000-$39,999", 12,
                      ifelse(income == "13. $40,000-$44,999", 13,
                      ifelse(income == "14. $45,000-$49,999", 14,
                      ifelse(income == "15. $50,000-$54,999", 15,
                      ifelse(income == "16. $55,000-$59,999", 16,
                      ifelse(income == "17. $60,000-$64,999", 17,
                      ifelse(income == "18. $65,000-$69,999", 18,
                      ifelse(income == "19. $70,000-$74,999", 19,
                      ifelse(income == "20. $75,000-$79,999", 20,
                      ifelse(income == "21. $80,000-$89,999", 21,
                      ifelse(income == "22. $90,000-$99,999", 22,
                      ifelse(income == "23. $100,000-$109,999", 23,
                      ifelse(income == "24. $110,000-$124,999", 24,
                      ifelse(income == "25. $125,000-$149,999", 25,
                      ifelse(income == "26. $150,000-$174,999", 26, 27)))))))))))))))))))))))))))
                                                                                                                                                            
df <- mutate(df,educ = ifelse(education == "1. Less than 1st grade", 1,
                       ifelse(education == "2. 1st, 2nd, 3rd or 4th grade", 2,
                       ifelse(education == "3. 5th or 6th grade", 3,
                       ifelse(education == "4. 7th or 8th grade", 4,
                       ifelse(education == "5. 9th grade", 5,
                       ifelse(education == "6. 10th grade", 6,
                       ifelse(education == "7. 11th grade", 7,
                       ifelse(education == "8. 12th grade no diploma", 8,
                       ifelse(education == "9. High school graduate- high school diploma or equivalent (for example: GED)", 9,
                       ifelse(education == "10. Some college but no degree", 10,
                       ifelse(education == "11. Associate degree in college - occupational/vocational program", 11,
                       ifelse(education == "12. Associate degree in college -- academic program", 12,
                       ifelse(education == "13. Bachelor's degree (for example: BA, AB, BS)", 13,
                       ifelse(education == "14. Master's degree (for example: MA, MS, MENG, MED, MSW, MBA)", 14,
                       ifelse(education == "15. Professional school degree (for example: MD, DDS, DVM, LLB, JD)", 15, 16))))))))))))))))

df <- mutate(df, interest = ifelse(interest == "1. Very closely", 4,
                            ifelse(interest == "2. Fairly closely", 3,
                            ifelse(interest == "3. Not very closely", 2, 1))))



df_true <- df[, c("Dem", "Rep", "Ind", "Black", "Hisp",
                  "White", "Asian", "Female", "Male", "Empl",        
                  "Ret", "Stud", "interest", "inc", "age", "educ",
                  "VotePres2016", "CongressJob", "CountryTrack", "HealthIns",
                  "GlobWarm", "Religious", "InternetHome", "OwnHome",
                  "RentHome", "Rally", "Donate", "Liberal", "Conservative",
                  "Moderate", "Married", "Separated", "Single")]  
# there are only 85 Unempl in the whole data, these are impossible to sample, so I'm not including them


# take a sample of 1000 observations, then save it, to try to get around the CO RAM collapses
set.seed(143)
df_true$educ %>% unique %>% length
anes_1000 <- sample(nrow(df_true), 1000, replace = TRUE) %>% df_true[.,]
anes_1000$educ %>% unique %>% length
saveRDS(anes_1000, here("data", "anes", "anes_1000.rds"))



# identify and discard highly collinear variables
coll.var <- df_true %>% cor() %>% abs() %>% findCorrelation(., cutoff = .7) %>% sort()         
df_true <- df_true[,-c(coll.var)]                                                         
# save name of all EVs
all.evs <- colnames(df_true)[-which(colnames(df_true) == "educ")]                         

# variables to insert NAs into
add.nas.columns <- c("Dem", "inc", "age", "Male", "interest")                            
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
OPMord.dat <- list()
# set up the percentage progress bar across the sampled numbers
pb <- txtProgressBar(min = 1, max = mc.iterations, style = 3)
for(mc in 1:mc.iterations){
  # load the percentage progress bar into the loop
  setTxtProgressBar(pb, mc)
  # combine columns with NAs and columns without NAs and run ordinal polr() function on data
  OPMord.dat[[mc]] <- cbind(no.nas, ampute(yes.nas, prop = prop, mech = "MAR")$amp) %>%
    OPMord(., dv = "educ", evs = all.evs)
    # print whenever int.df doesn't have all rows, which means it doesn't have all education levels (6 rows for 7 levels)
    if(OPMord.dat[[mc]]$data.short.na.omit %>% .$educ %>% unique() %>% length() < min.lev){
      print("Fewer than original levels")
    }
  rm(amputed)
  rm(df.nas)
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
                           dv = "educ", OPMordOut = OPMord.dat[[n]]) 
    }


start_time <- Sys.time()
hd.ord.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    # run hot.deck.ord() on OPMcut education values, which scales the ordinal variable
    hd.ord.dat[[n]] <- hot.deck.ord(OPMcut.dat[[n]], m = m, ord = "educ", 
                                 sdCutoff = sd(OPMcut.dat[[n]]$educ), method = "best.cell")
      }
end_time <- Sys.time()
hd.ord.time <- difftime(end_time, start_time, units = "mins")


start_time <- Sys.time()
hd.norm.orig.dat <- list()
pb <- txtProgressBar(min = 1, max = length(OPMord.dat), style = 3)
  for(n in 1:length(OPMord.dat)){
    setTxtProgressBar(pb, n)
    # run normal hot.deck() on original education values
    hd.norm.orig.dat[[n]] <- hot.deck(OPMord.dat[[n]]$data.full.nas, m = m,                          
                                   sdCutoff = sd(OPMord.dat[[n]]$data.full.nas$educ), 
                                   method = "best.cell") 
      }
end_time <- Sys.time()
hd.norm.time <- difftime(end_time, start_time, units = "mins")


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


runtime <- cbind(c(hd.ord.time, hd.norm.time, am.time, mice.time),
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












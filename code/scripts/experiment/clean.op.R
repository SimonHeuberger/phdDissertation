
library(here)
library(magrittr)
library(tidyverse)
library(RItools)
library(stargazer)
library(xtable)
library(plyr)


### check for attention check fail/pass ### 

df <- read.csv(here::here("data", "experiment", "op.all.csv"), na.strings = "NA")
summary(is.na(df))
nrow(df)

df.fail <- df[!is.na(df$att),] %>% # where there are no att == NA
  filter(., att != 2) 
nrow(df.fail)



### check for unique RIDs ###

df[!duplicated(df$RID), ] %>% nrow # 1,211 unique entries
df[duplicated(df$RID), ] %>% nrow # 160 duplicate entries

df <- df[!duplicated(df$RID), ] # remove the duplicate RIDs
nrow(df) # 1,211 people with unique RIDs

# save unique RIDs to send to Lucid
# write.csv(df$RID, here("data", "experiment", "RIDs.op.csv"), row.names = FALSE)

# After I checked the number of duplicate RIDs for an and op, I had fewer uniques RIDs than the 2,331 I paid for.
# Lucid opened the survey up again so I could collect more. In these (op) data, I have 1,211 unique RIDs.
# In the an data, I have 1,162. That's 2,373 unique RIDs overall.





### Manipulate variables ###

throw.away <- select(df, online.length, study.choice, online.why) # save throw away columns separately
comments <- na.omit(df$com) # save comments separately

discard.cols <- c("online.length", "study.choice", "online.why", "com", "att")
df <- select(df, -one_of(discard.cols)) # remove unneeded columns

df[rowSums(is.na(df)) > 0,] %>% nrow # 108 people with NAs
summary(is.na(df))

df.omit <- na.omit(df)
nrow(df.omit) # 1,103 people overall for me to work with

df.omit$inc.num <- df.omit$inc
cols.fac <- c("educ", "race", "gender", "empl", "inc", "pid", "ideol")
for (i in 1:length(cols.fac)){
    df.omit[,cols.fac[i]] <- factor(df.omit[,cols.fac[i]])
}

questions.path <- "/Users/simonheuberger/dissertation/shiny/questions"
questions.files <- paste0(c("education.op", "demographics1", "demographics2", "pid", "ideol"), ".csv")

varNames <- function(df, varName){
  filter(df, id == varName) %>% 
    select(., choiceNames) %>% 
    .[,1] %>% 
    strsplit(., ",") %>%
    .[[1]]
} # to extract the var names from the .csv question files

varChoices <- function(df, varName){
  filter(df, id == varName) %>% 
    select(., choices) %>% 
    .[,1] %>% 
    strsplit(., ",") %>%
    .[[1]]
} # to extract the numeric var choices from the files

refactor <- function(df, var, varn){
  if(length(levels(df[, var])) == length(varn)){
      levels(df[, var]) <- varn
      df[, var]
      }else{
        stop("Not all levels are present in data. Rename factors by hand!")
      }
} # refactor the numeric levels with the character versions


var <- "educ"
q1 <- read.csv(paste0(questions.path, "/", questions.files[1])) # education.op for educ
varn <- varNames(q1, var)
df.omit[, var] <- refactor(df.omit, var, varn)  

var <- "race"
q2 <- read.csv(paste0(questions.path, "/", questions.files[2])) # dems1 for race, gender
varn <- varNames(q2, var)
df.omit[, var] <- refactor(df.omit, var, varn)  

var <- "gender"
varn <- varNames(q2, var)
df.omit[, var] <- refactor(df.omit, var, varn)

q3 <- read.csv(paste0(questions.path, "/", questions.files[3])) # dems2 for empl, inc
var <- "empl"
varn <- varNames(q3, var)
df.omit[, var] <- refactor(df.omit, var, varn)

var <- "inc"
varn <- varNames(q3, var)
df.omit[, var] <- refactor(df.omit, var, varn)

q4 <- read.csv(paste0(questions.path, "/", questions.files[4])) # pid for pid
var <- "pid"
varn <- varNames(q4, var)
df.omit[, var] <- refactor(df.omit, var, varn)

q5 <- read.csv(paste0(questions.path, "/", questions.files[5])) # ideol for ideol
var <- "ideol"
varn <- varNames(q5, var)
df.omit[, var] <- refactor(df.omit, var, varn)

df.omit$pid.follow <- df.omit$pid.follow %>% as.character # pid.follow needs to be separate, it's more complicated
qdem <- read.csv(paste0(questions.path, "/pid.foll.dem.csv"))
qind <- read.csv(paste0(questions.path, "/pid.foll.ind.else.csv"))
var <- "pid.follow"
varndem <- varNames(qdem, var)
varnind <- varNames(qind, var)

for(i in 1:nrow(df.omit)){
  if(df.omit[i, "pid"] == "Democrat" | df.omit[i, "pid"] == "Republican"){
    df.omit[i, "pid.follow"] <- ifelse(df.omit[i, "pid.follow"] == 1, varndem[1], varndem[2])
  } else {
    df.omit[i, "pid.follow"] <- ifelse(df.omit[i, "pid.follow"] == 1, varnind[1], 
                                  ifelse(df.omit[i, "pid.follow"] == 2, varnind[2], varnind[3]))
  }
}

df.omit$pid.follow <- df.omit$pid.follow %>% as.factor
df.omit$ideol.follow <- df.omit$ideol.follow %>% as.character # ideol.follow needs to be separate, also more complicated
qlib <- read.csv(paste0(questions.path, "/ideol.foll.lib.csv"))
qcons <- read.csv(paste0(questions.path, "/ideol.foll.cons.csv"))
qnei <- read.csv(paste0(questions.path, "/ideol.foll.nei.csv"))
var <- "ideol.follow"
varnlib <- varNames(qlib, var)
varncons <- varNames(qcons, var)
varnnei <- varNames(qnei, var)

for(i in 1:nrow(df.omit)){
  if(df.omit[i, "ideol"] == "Liberal"){
    df.omit[i, "ideol.follow"] <- ifelse(df.omit[i, "ideol.follow"] == 1, varnlib[1], varnlib[2])
  } else if(df.omit[i, "ideol"] == "Conservative"){
    df.omit[i, "ideol.follow"] <- ifelse(df.omit[i, "ideol.follow"] == 1, varncons[1], varncons[2])
  } else {
    df.omit[i, "ideol.follow"] <- ifelse(df.omit[i, "ideol.follow"] == 1, varnnei[1], 
                                    ifelse(df.omit[i, "ideol.follow"] == 2, varnnei[2], varnnei[3]))
  }
}

df.omit$ideol.follow <- df.omit$ideol.follow %>% as.factor

# birthyear input is 1:83. The corresponding birthyears are 2002:1920
# I want to replace the input with the corresponding birthyears
# To get from birthyear input 1 to actual birthyear 2002, you add 2001. To get from 2 to 2001, you add 1999. To get from 3 to 2000, you add 1997. Etc.
# You start with 2001. For every number you go up in birthyear input, you add a number going downward from 2001 by a sequence of 2 to the birthyear input
# Then you have a column of the actual birthyears. To get the age, subtract each actual birthyear from 2020

var <- "birthyear"
years <- varNames(q2, var) %>% as.numeric
year.inp <- varChoices(q2, var) %>% as.numeric
year.seq.start <- max(years) - min(year.inp) # the first number to add (2001) to the lowest birthyear input (1)
year.dist <- max(year.inp)-min(year.inp) # the distance between the highest and lowest birthyear input (82)
year.seq.end <- year.seq.start - (year.dist * 2) # the last number to add (1837) to the highest birthyear input (83)
year.seq <- seq(from = year.seq.end, to = year.seq.start, by = 2) %>% rev # all the numbers to add to each increasing number in birthyear input

for(i in 1:nrow(df.omit)){
  for(x in 1:length(year.inp)){
    if(df.omit[i, "birthyear"] == year.inp[x]){
      df.omit[i, "age"] <- 2020 - (year.inp[x] + year.seq[x])
    }
  }
}

df.omit <- select(df.omit, -one_of("birthyear")) # I tried simply overwriting column birthyear in the loop, but that kept giving really weird numbers. No idea why
df.omit$Democrat <- ifelse(df.omit$pid == "Democrat", 1, 0)
df.omit$Male <- ifelse(df.omit$gender == "Male", 1, 0)
df.omit$mor.all <- (df.omit$mor.suffer + df.omit$mor.care + df.omit$mor.cruel +
                 df.omit$mor.comp + df.omit$mor.anim + df.omit$mor.kill) / 6
df.omit$si.all <- (df.omit$si.white + df.omit$si.care + df.omit$si.kids + 
                df.omit$si.kill + df.omit$si.good + df.omit$si.help) / 6






### Check against census (Lucid benchmark), should be within a couple of percentage points ###

df.omit$gender %>% table %>% prop.table
# census is 49% male, 51% female -- that checks out

df.omit$inc <- as.factor(df.omit$inc)
df.omit$inc %>% table %>% prop.table
# census has different categories, but it roughly evens out

df.omit$race <- as.factor(df.omit$race)
df.omit$race %>% table %>% prop.table
# census: 72% white, 13% Black, 5% Asian, 1% Native American, 9% Other == that checks out

df.omit$age.cats <- ifelse(df.omit$age <= 24, "18-24", 
                      ifelse(df.omit$age > 24 & df.omit$age <= 34, "25-34",
                             ifelse(df.omit$age >34 & df.omit$age <= 44, "35-44",
                                    ifelse(df.omit$age > 44 & df.omit$age <= 54, "45-54",
                                           ifelse(df.omit$age > 54 & df.omit$age <= 64, "55-64",
                                                  "65+")))))
df.omit$age.cats <- as.factor(df.omit$age.cats)
df.omit$age.cats %>% table %>% prop.table     
# census: 13% 18-24, 18% 25-34, 18% 35-44, 19% 45-54, 16% 55-64, 17% 65+
# that all checks out except 18-24

RID.18 <- subset(df.omit, subset = (age == 18))
RID.18$RID %>% length # 266 people in the sample are 18
RID.18$RID %>% length / nrow(df.omit) # that's a whopping 24%

# write.csv(RID.18, file = here("data", "experiment", "RIDs.resp.with.age.18.op.csv"), row.names = FALSE)
# I saved the RIDs for everyone with age 18 and sent it to Lucid. They
# have the real ages of the respondents on file, so I can use those

# read in correct ages and RIDs for everyone in my data (an and op) who is 18
correct.age.RID.18 <- read.csv(here::here("data", "experiment", "RIDs.resp.with.age.18.all.correct.csv"), na.strings = "NA")
df.omit <- merge(df.omit, correct.age.RID.18[, c("RID", "age")], by = "RID", all.x = TRUE) #merge it with df
# replace NAs in new column with values from original column
df.omit$age.y[is.na(df.omit$age.y)] <- df.omit$age.x[is.na(df.omit$age.y)]
df.omit <- select(df.omit, -one_of("age.x")) # remove unneeded columns
df.omit <- dplyr::rename(df.omit, age = age.y) # rename column
# convert to census categories again
df.omit$age.cats <- ifelse(df.omit$age <= 24, "18-24", 
                      ifelse(df.omit$age > 24 & df.omit$age <= 34, "25-34",
                             ifelse(df.omit$age >34 & df.omit$age <= 44, "35-44",
                                    ifelse(df.omit$age > 44 & df.omit$age <= 54, "45-54",
                                           ifelse(df.omit$age > 54 & df.omit$age <= 64, "55-64",
                                                  "65+")))))
df.omit$age.cats <- as.factor(df.omit$age.cats)
df.omit$age.cats %>% table %>% prop.table     
# census: 13% 18-24, 18% 25-34, 18% 35-44, 19% 45-54, 16% 55-64, 17% 65+
# now it all checks out


### Create binary columns for regressions ###

df.omit$Employed <- ifelse(df.omit$empl == "Employed part time" | df.omit$empl == "Employed full time", 1, 0)
df.omit$Unemployed <- ifelse(df.omit$empl == "Unemployed", 1, 0)
df.omit$Retired <- ifelse(df.omit$empl == "Retired", 1, 0)
df.omit$Student <- ifelse(df.omit$empl == "Student", 1, 0)
df.omit$Homemaker <- ifelse(df.omit$empl == "Homemaker", 1, 0)
df.omit$`High school or lower` <- ifelse(df.omit$educ == "High school or lower", 1, 0)
df.omit$`Some college` <- ifelse(df.omit$educ == "Some college", 1, 0)
df.omit$`Associate degree` <- ifelse(df.omit$educ == "Associate degree", 1, 0)
df.omit$Bachelor <- ifelse(df.omit$educ == "Bachelor", 1, 0)
df.omit$`Master or higher` <- ifelse(df.omit$educ == "Master or higher", 1, 0)

df.omit <- plyr::rename(df.omit, replace = c("inc.num" = "Income",
                                             "mor.all" = "Moral conviction",
                                             "si.all" = "Self-interest",
                                             "empl" = "emply")) # this last one is for a later string removal


### Save final data ###

df.omit <- select(df.omit, -one_of(c("RID", "age.cats"))) # remove unneeded columns
df.omit <- df.omit[, c(35, 1:34, 36:ncol(df.omit))] # move age upfront

write.csv(df.omit, here::here("data", "experiment", "op.clean.csv"), row.names = FALSE) # 1,103 observations

file.copy(here::here("data", "experiment", "op.clean.csv"), "/Users/simonheuberger/dissertation/diss/thesis/data/framing/experiment", overwrite = TRUE)

# that gives me 2,165 respondents without NAs overall (1,103 op; 1,062 an)





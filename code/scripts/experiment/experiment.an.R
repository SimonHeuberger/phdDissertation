
library(here)
library(magrittr)
library(tidyverse)
library(RItools)
library(stargazer)
library(xtable)

df.all <- read.csv(here("data", "experiment", "alldata.an.csv"), na.strings = "NA")

summary(is.na(df.all))

df.all.fail <- df.all[!is.na(df.all$att),] %>% # where there are no att == NA
  filter(., att != 2)
nrow(df.all.fail)

nrow(df.all)

throw.away <- select(df.all, online.length, study.choice, online.why) # save throw away columns separately
comments <- na.omit(df.all$com) # save comments separately

discard.cols <- c("online.length", "study.choice", "online.why", "com", "att", "RID")
df.all <- select(df.all, -one_of(discard.cols)) # remove unneeded columns

df.all[rowSums(is.na(df.all)) > 0,] %>% nrow # 3 people with NAs
summary(is.na(df.all))

df.all.omit <- na.omit(df.all)
nrow(df.all.omit) # 25/26 people overall for me to work with

df.all.omit$inc.num <- df.all.omit$inc
cols.fac <- c("educ", "race", "gender", "empl", "inc", "pid", "ideol")
for (i in 1:length(cols.fac)){
    df.all.omit[,cols.fac[i]] <- factor(df.all.omit[,cols.fac[i]])
}

questions.path <- "/Users/simonheuberger/dissertation/shiny/questions"
questions.files <- paste0(c("education.an", "demographics1", "demographics2", "pid", "ideol"), ".csv")

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
q1 <- read.csv(paste0(questions.path, "/", questions.files[1])) # education.an for educ
varn <- varNames(q1, var)
df.all.omit[, var] <- refactor(df.all.omit, var, varn)  
varLev <- levels(df.all.omit[, var])
varLev
varn
df.all.omit[, var] <- fct_recode(df.all.omit[, var],
                                   "7th-8th grade" = "4",
                                   "High school graduate" = "9",
                                   "Some college" = "10",
                                 "Associate degree" = "11",
                                 "Bachelor" = "12",
                                 "Master" = "13",
                                 "Professional degree" = "14")

var <- "race"
q2 <- read.csv(paste0(questions.path, "/", questions.files[2])) # dems1 for race, gender
varn <- varNames(q2, var)
df.all.omit[, var] <- refactor(df.all.omit, var, varn)  
varLev <- levels(df.all.omit[, var])
varLev
varn
df.all.omit[, var] <- fct_recode(df.all.omit[, var],
                                   "White" = "1",
                                   "Black" = "2",
                                   "Hispanic" = "3",
                                 "Asian" = "4")

var <- "gender"
varn <- varNames(q2, var)
df.all.omit[, var] <- refactor(df.all.omit, var, varn)
varLev <- levels(df.all.omit[, var])
varLev
varn
df.all.omit[, var] <- fct_recode(df.all.omit[, var],
                                   "Male" = "1",
                                   "Female" = "2")

q3 <- read.csv(paste0(questions.path, "/", questions.files[3])) # dems2 for empl, inc
var <- "empl"
varn <- varNames(q3, var)
df.all.omit[, var] <- refactor(df.all.omit, var, varn)
varLev <- levels(df.all.omit[, var])
varLev
varn
df.all.omit[, var] <- fct_recode(df.all.omit[, var],
                                   "Employed part time" = "1",
                                   "Employed full time" = "2",
                                 "Retired" = "4",
                                 "Homemaker" = "5",
                                 "Unemployed" = "6")

var <- "inc"
varn <- varNames(q3, var)
df.all.omit[, var] <- refactor(df.all.omit, var, varn)

q4 <- read.csv(paste0(questions.path, "/", questions.files[4])) # pid for pid
var <- "pid"
varn <- varNames(q4, var)
df.all.omit[, var] <- refactor(df.all.omit, var, varn)
varLev <- levels(df.all.omit[, var])
varLev
varn
df.all.omit[, var] <- fct_recode(df.all.omit[, var],
                                   "Democrat" = "1",
                                   "Republican" = "2",
                                 "Independent" = "3")

q5 <- read.csv(paste0(questions.path, "/", questions.files[5])) # ideol for ideol
var <- "ideol"
varn <- varNames(q5, var)
df.all.omit[, var] <- refactor(df.all.omit, var, varn)

df.all.omit$pid.follow <- df.all.omit$pid.follow %>% as.character # pid.follow needs to be separate, it's more complicated
qdem <- read.csv(paste0(questions.path, "/pid.foll.dem.csv"))
qind <- read.csv(paste0(questions.path, "/pid.foll.ind.else.csv"))
var <- "pid.follow"
varndem <- varNames(qdem, var)
varnind <- varNames(qind, var)

for(i in 1:nrow(df.all.omit)){
  if(df.all.omit[i, "pid"] == "Democrat" | df.all.omit[i, "pid"] == "Republican"){
    df.all.omit[i, "pid.follow"] <- ifelse(df.all.omit[i, "pid.follow"] == 1, varndem[1], varndem[2])
  } else {
    df.all.omit[i, "pid.follow"] <- ifelse(df.all.omit[i, "pid.follow"] == 1, varnind[1], 
                                  ifelse(df.all.omit[i, "pid.follow"] == 2, varnind[2], varnind[3]))
  }
}

df.all.omit$pid.follow <- df.all.omit$pid.follow %>% as.factor
df.all.omit$ideol.follow <- df.all.omit$ideol.follow %>% as.character # ideol.follow needs to be separate, also more complicated
qlib <- read.csv(paste0(questions.path, "/ideol.foll.lib.csv"))
qcons <- read.csv(paste0(questions.path, "/ideol.foll.cons.csv"))
qnei <- read.csv(paste0(questions.path, "/ideol.foll.nei.csv"))
var <- "ideol.follow"
varnlib <- varNames(qlib, var)
varncons <- varNames(qcons, var)
varnnei <- varNames(qnei, var)

for(i in 1:nrow(df.all.omit)){
  if(df.all.omit[i, "ideol"] == "Liberal"){
    df.all.omit[i, "ideol.follow"] <- ifelse(df.all.omit[i, "ideol.follow"] == 1, varnlib[1], varnlib[2])
  } else if(df.all.omit[i, "ideol"] == "Conservative"){
    df.all.omit[i, "ideol.follow"] <- ifelse(df.all.omit[i, "ideol.follow"] == 1, varncons[1], varncons[2])
  } else {
    df.all.omit[i, "ideol.follow"] <- ifelse(df.all.omit[i, "ideol.follow"] == 1, varnnei[1], 
                                    ifelse(df.all.omit[i, "ideol.follow"] == 2, varnnei[2], varnnei[3]))
  }
}

df.all.omit$ideol.follow <- df.all.omit$ideol.follow %>% as.factor

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

for(i in 1:nrow(df.all.omit)){
  for(x in 1:length(year.inp)){
    if(df.all.omit[i, "birthyear"] == year.inp[x]){
      df.all.omit[i, "age"] <- 2020 - (year.inp[x] + year.seq[x])
    }
  }
}

df.all.omit <- select(df.all.omit, -one_of("birthyear")) # I tried simply overwriting column birthyear in the loop, but that kept giving really weird numbers. No idea why
df.all.omit$dem <- ifelse(df.all.omit$pid == "Democrat", 1, 0)
df.all.omit$male <- ifelse(df.all.omit$gender == "Male", 1, 0)
df.all.omit$mor.all <- (df.all.omit$mor.suffer + df.all.omit$mor.care + df.all.omit$mor.cruel +
                 df.all.omit$mor.comp + df.all.omit$mor.anim + df.all.omit$mor.kill) / 6
df.all.omit$si.all <- (df.all.omit$si.white + df.all.omit$si.care + df.all.omit$si.kids + 
                df.all.omit$si.kill + df.all.omit$si.good + df.all.omit$si.help) / 6

# write.csv(df.all.omit, file = here("data", "pre-test", "batch.1.only.att.passed.csv"), row.names = FALSE)

hc.lm.out <- lm(hc.likert ~ hc.group + mor.all + si.all + dem + empl + inc, data = df.all.omit)
ev.lm.out <- lm(ev.likert ~ ev.group + mor.all + si.all + dem + empl + inc, data = df.all.omit)

df.all.omit$hc.group.num <- df.all.omit$hc.group %>% as.factor %>% as.numeric
balance <- xBalance(hc.group.num ~ race + gender + empl + inc + pid + educ + age, 
                    data = df.all.omit, 
                    report = c("std.diffs", "z.scores", "adj.means", "adj.mean.diffs", 
                               "adj.mean.diffs.null.sd", "chisquare.test", "p.values"))
listed <- lapply(seq(dim(balance$results)[3]), function(x) balance$results[ , , x])
balance.df <- plyr::ldply(listed, data.frame, .id = NULL)
balance.df$vars <- c(paste0("race", levels(df.all.omit$race)), paste0("gender", levels(df.all.omit$gender)), 
                     paste0("empl", levels(df.all.omit$empl)), paste0("inc", levels(df.all.omit$inc)), 
                     paste0("pid", levels(df.all.omit$pid)), paste0("educ", levels(df.all.omit$educ)), 
                     "age")
balance.df <- balance.df[, c(8,1:7)]

group_by(df.all.omit, hc.group) %>% summarize(count = n())
group_by(df.all.omit, ev.group) %>% summarize(count = n())
stargazer(hc.lm.out, header = FALSE,
          title = "Healthcare Regression Results",
          label = "hc.reg",
          single.row = TRUE)

stargazer(ev.lm.out, header = FALSE,
          title = "Environment Regression Results",
          label = "ev.reg",
          single.row = TRUE)
xtable(balance.df, caption = "Balance Across Covariates") %>% print(., comment=FALSE, include.rownames=FALSE)
xtable(balance$overall, caption = "Chi-squared test") %>% print(., comment = FALSE, include.rownames=FALSE)
plot(balance)


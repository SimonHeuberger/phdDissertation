
##########################################################

# There are 8 different batches of these data. That's because
# I rejected everyone who failed any attention checks and 
# reassigned those tasks to others. I could have downloaded 
# all the batches together in one go, but I wanted to be 
# careful in case I need to know what was in what batch

# After I did the above, Liz told me I mustn't exclude people
# from the analysis if they failed the post-treatment checks.
# Some bollocks about if affecting passage and being potentially
# meaningful for reasons other than attentiveness. I don't buy 
# the second one (the checks were pretty damn clear and easy),
# but whatever. 

# There are thus now two separate analyses: One
# where I collect all the batches and analyze only the ones
# who have passed everything, and another where I analyze 
# only the first batch and include everyone who didn't fail
# the first check. The first is in this .R file. The second
# is in a .Rmd file because it results in a table and plot
# that I send to the committee

##########################################################


library(here)
library(tidyverse)
library(magrittr)


########################################################## Analysis of 7 batches ##########################################################

### Prepare data for batch 1 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.1 <- read.csv(here("data", "pre-test", "batch.1.csv"), na.strings = "NA")
df.all.1[27, "hc.check"] <- 3 # this guy commented that he accidentally clicked the wrong option, so I'm adjusting this and not rejecting him

df.all.1 <- df.all.1[!duplicated(df.all.1$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.1))

df.no.att.na.fail.1 <- df.all.1[!is.na(df.all.1$att),] %>% # where there are no att == NA
  filter(., att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
df.att.na.fail.1 <- df.all.1[is.na(df.all.1$att),] %>% # where there are att == NA
  filter(., hc.check != 3 | ev.check != 5) # everyone who has failed at least one check

df.fail.1 <- rbind(df.no.att.na.fail.1, df.att.na.fail.1)
nrow(df.fail.1) # 94 people failed at least one attention check

df.1 <- filter(df.all.1, !unique.id %in% df.fail.1$unique.id)
nrow(df.1) # 144 people passed all attention checks

throw.away.1 <- select(df.1, .online.length, .study.choice, online.why) # save throw away columns separately
comments.1 <- na.omit(df.1$com) # save comments separately

# I deleted all the .csv files on Dropbox and then used this to reject the corresponding MTurk workers and republish the HIT for others (who get saved afresh on Dropbox)
df.fail.1$unique.id 

discard.cols <- c(".online.length", ".study.choice", "online.why", "com", "att", "ev.check", "hc.check", "unique.id")
df.1 <- select(df.1, -one_of(discard.cols)) # remove unneeded columns

df.1[rowSums(is.na(df.1)) > 0,] %>% nrow # 7 people with NAs
summary(is.na(df.1))

df.omit.1 <- na.omit(df.1)
nrow(df.omit.1) # 137 people overall for me to work with




### Prepare data for batch 2 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.2 <- read.csv(here("data", "pre-test", "batch.2.csv"), na.strings = "NA")
df.all.2 <- df.all.2[!duplicated(df.all.2$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.2))

df.fail.2 <- filter(df.all.2, att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
nrow(df.fail.2) # 48 people failed at least one attention check

df.2 <- filter(df.all.2, !unique.id %in% df.fail.2$unique.id)
nrow(df.2) # 46 people passed all attention checks

throw.away.2 <- select(df.2, .online.length, .study.choice, online.why) # save throw away columns separately
comments.2 <- na.omit(df.2$com) # save comments separately

# I deleted all the .csv files on Dropbox and then used this to reject the corresponding MTurk workers and republish the HIT for others (who get saved afresh on Dropbox)
df.fail.2$unique.id 

df.2 <- select(df.2, -one_of(discard.cols)) # remove unneeded columns

df.2[rowSums(is.na(df.2)) > 0,] %>% nrow # no NAs in the rest of the data
nrow(df.2) # 46 people overall for me to work with



### Prepare data for batch 3 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.3 <- read.csv(here("data", "pre-test", "batch.3.csv"), na.strings = "NA")

df.all.3 <- df.all.3[!duplicated(df.all.3$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.3))

df.fail.3 <- df.all.3[!is.na(df.all.3$att),] %>% # where there are no att == NA
  filter(., att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
# df.all.3[is.na(df.all.3$att),] %>% # where there are att == NA
#   filter(., hc.check != 3 | ev.check != 5) # no one has failed at least one check

nrow(df.fail.3) # 26 people failed at least one attention check

df.3 <- filter(df.all.3, !unique.id %in% df.fail.3$unique.id)
nrow(df.3) # 344 people passed all attention checks

throw.away.3 <- select(df.3, .online.length, .study.choice, online.why) # save throw away columns separately
comments.3 <- na.omit(df.3$com) # save comments separately

# I deleted all the .csv files on Dropbox and then used this to reject the corresponding MTurk workers and republish the HIT for others (who get saved afresh on Dropbox)
df.fail.3$unique.id 

df.3 <- select(df.3, -one_of(discard.cols)) # remove unneeded columns

df.3[rowSums(is.na(df.3)) > 0,] %>% nrow # 2 people with NAs
summary(is.na(df.3))

df.omit.3 <- na.omit(df.3)
nrow(df.omit.3) # 20 people overall for me to work with



### Prepare data for batch 4 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.4 <- read.csv(here("data", "pre-test", "batch.4.csv"), na.strings = "NA")

df.all.4 <- df.all.4[!duplicated(df.all.4$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.4))

df.fail.4 <- filter(df.all.4, att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
nrow(df.fail.4) # 9 people failed at least one attention check

df.4 <- filter(df.all.4, !unique.id %in% df.fail.4$unique.id)
nrow(df.4) # 17 people passed all attention checks

throw.away.4 <- select(df.4, .online.length, .study.choice, online.why) # save throw away columns separately
comments.4 <- na.omit(df.4$com) # save comments separately

# I deleted all the .csv files on Dropbox and then used this to reject the corresponding MTurk workers and republish the HIT for others (who get saved afresh on Dropbox)
df.fail.4$unique.id 

df.4 <- select(df.4, -one_of(discard.cols)) # remove unneeded columns

df.4[rowSums(is.na(df.4)) > 0,] %>% nrow # no NAs
nrow(df.4) # 17 people overall for me to work with




### Prepare data for batch 5 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.5 <- read.csv(here("data", "pre-test", "batch.5.csv"), na.strings = "NA")

df.all.5 <- df.all.5[!duplicated(df.all.5$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.5))

df.fail.5 <- filter(df.all.5, att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
nrow(df.fail.5) # 5 people failed at least one attention check

df.5 <- filter(df.all.5, !unique.id %in% df.fail.5$unique.id)
nrow(df.5) # 4 people passed all attention checks

throw.away.5 <- select(df.5, .online.length, .study.choice, online.why) # save throw away columns separately
comments.5 <- na.omit(df.5$com) # save comments separately

# I deleted all the .csv files on Dropbox and then used this to reject the corresponding MTurk workers and republish the HIT for others (who get saved afresh on Dropbox)
df.fail.5$unique.id 

df.5 <- select(df.5, -one_of(discard.cols)) # remove unneeded columns

df.5[rowSums(is.na(df.5)) > 0,] %>% nrow # no NAs
nrow(df.5) # 4 people overall for me to work with




### Prepare data for batch 6 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.6 <- read.csv(here("data", "pre-test", "batch.6.csv"), na.strings = "NA")

df.all.6 <- df.all.6[!duplicated(df.all.6$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.6))

df.fail.6 <- filter(df.all.6, att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
nrow(df.fail.6) # 4 people failed at least one attention check

df.6 <- filter(df.all.6, !unique.id %in% df.fail.6$unique.id)
nrow(df.6) # 1 people passed all attention checks

throw.away.6 <- select(df.6, .online.length, .study.choice, online.why) # save throw away columns separately
comments.6 <- na.omit(df.6$com) # save comments separately

# I deleted all the .csv files on Dropbox and then used this to reject the corresponding MTurk workers and republish the HIT for others (who get saved afresh on Dropbox)
df.fail.6$unique.id 

df.6 <- select(df.6, -one_of(discard.cols)) # remove unneeded columns

df.6[rowSums(is.na(df.6)) > 0,] %>% nrow # no NAs
nrow(df.6) # 1 people overall for me to work with





### Prepare data for batch 7 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.7 <- read.csv(here("data", "pre-test", "batch.7.csv"), na.strings = "NA")

df.all.7 <- df.all.7[!duplicated(df.all.7$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.7))

df.fail.7 <- filter(df.all.7, att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
nrow(df.fail.7) # 1 people failed at least one attention check

df.7 <- filter(df.all.7, !unique.id %in% df.fail.7$unique.id)
nrow(df.7) # 3 people passed all attention checks

throw.away.7 <- select(df.7, .online.length, .study.choice, online.why) # save throw away columns separately
comments.7 <- na.omit(df.7$com) # save comments separately

# I deleted all the .csv files on Dropbox and then used this to reject the corresponding MTurk workers and republish the HIT for others (who get saved afresh on Dropbox)
df.fail.7$unique.id 

df.7 <- select(df.7, -one_of(discard.cols)) # remove unneeded columns

df.7[rowSums(is.na(df.7)) > 0,] %>% nrow # no NAs
nrow(df.7) # 3 people overall for me to work with





### Prepare data for batch 8 (sort out failed checks, remove the unimportant columns, take out NAs) ###

df.all.8 <- read.csv(here("data", "pre-test", "batch.8.csv"), na.strings = "NA")

df.all.8 <- df.all.8[!duplicated(df.all.8$unique.id), ] # remove duplicate unique.ids (some MTurk error somewhere)
summary(is.na(df.all.8))

df.fail.8 <- filter(df.all.8, att != 2 | hc.check != 3 | ev.check != 5) # everyone who has failed at least one check
nrow(df.fail.8) # no one failed any attention checks

df.8 <- filter(df.all.8, !unique.id %in% df.fail.8$unique.id)
nrow(df.8) # 1 people passed all attention checks

throw.away.8 <- select(df.8, .online.length, .study.choice, online.why) # save throw away columns separately
comments.8 <- na.omit(df.8$com) # save comments separately

df.8 <- select(df.8, -one_of(discard.cols)) # remove unneeded columns

df.8[rowSums(is.na(df.8)) > 0,] %>% nrow # no NAs
nrow(df.8) # 1 people overall for me to work with





### Combine batches, transform variables, run model ###

df <- rbind(df.omit.1, df.2, df.omit.3, df.4, df.5, df.6, df.7, df.8) %>%
  rename(educ = ed.both)
df$inc.num <- df$inc
cols.fac <- c("educ", "race", "gender", "empl", "inc", "pid", "ideol")
for (i in 1:length(cols.fac)){
    df[,cols.fac[i]] <- factor(df[,cols.fac[i]])
  }
levels(df$educ) <- readRDS(here("data", "anes", "anes_education.rds")) %>% 
  .$education.new %>% 
  levels
df$race <- fct_recode(df$race, 
                      "White" = "1", 
                      "Black" = "2",
                      "Arab" = "3",
                      "Hispanic" = "4",
                      "Asian" = "5",
                      "American Indian" = "6",
                      "Other" = "8")
df$gender <- fct_recode(df$gender,
                        "Male" = "1",
                        "Female" = "2",
                        "Other" = "3")
df$empl <- fct_recode(df$empl,
                      "Part time" = "1",
                      "Full time" = "2", 
                      "Student" = "3" ,
                      "Retired" = "4",
                      "Homemaker" = "5",
                      "Unemployed" = "6")
df$inc <- fct_recode(df$inc,
                     "Less than $20,000" = "1",
                     "$20,000 to $39,999" = "2",
                     "$40,000 to $59,999" = "3",
                     "$60,000 to $79,999" = "4",
                     "$80,000 to $99,999" = "5",
                     "$100,000 to $149,999" = "6",
                     "$150,000 or more" = "7")
df$pid <- fct_recode(df$pid,
                     "Democrat" = "1",
                     "Republican" = "2",
                     "Independent" = "3",
                     "Something else" = "4")
df$ideol <- fct_recode(df$ideol,
                       "Liberal" = "1",
                       "Conservative" = "2",
                       "Neither" = "3")

df$pid.follow <- df$pid.follow %>% as.character

for(i in 1:nrow(df)){
  if(df[i, "pid"] == "Democrat" | df[i, "pid"] == "Republican"){
    df[i, "pid.follow"] <- ifelse(df[i, "pid.follow"] == 1, "Strong", "Not very strong")
  } else {
    df[i, "pid.follow"] <- ifelse(df[i, "pid.follow"] == 1, "Closer to the Democratic Party", 
                                  ifelse(df[i, "pid.follow"] == 2, "Closer to the Republican Party", "Neither"))
  }
}

df$pid.follow <- df$pid.follow %>% as.factor
df$ideol.follow <- df$ideol.follow %>% as.character

for(i in 1:nrow(df)){
  if(df[i, "ideol"] == "Liberal"){
    df[i, "ideol.follow"] <- ifelse(df[i, "ideol.follow"] == 1, "Very liberal", "Somewhat liberal")
  } else if(df[i, "ideol"] == "Conservative"){
    df[i, "ideol.follow"] <- ifelse(df[i, "ideol.follow"] == 1, "Very conservative", "Somewhat conservative")
  } else {
    df[i, "ideol.follow"] <- ifelse(df[i, "ideol.follow"] == 1, "Closer to liberals", 
                                    ifelse(df[i, "ideol.follow"] == 2, "Closer to conservatives", "Neither"))
  }
}

df$ideol.follow <- df$ideol.follow %>% as.factor

# birthyear input is 1:83. The corresponding birthyears are 2002:1920
# I want to replace the input with the corresponding birthyears
# To get from birthyear input 1 to actual birthyear 2002, you add 2001. To get from 2 to 2001, you add 1999. To get from 3 to 2000, you add 1997. Etc.
# You start with 2001. For every number you go up in birthyear input, you add a number going downward from 2001 by a sequence of 2 to the birthyear input
# Then you have a column of the actual birthyears. To get the age, subtract each actual birthyear from 2020
year.inp <- 1:83
years <- 2002:1920
year.seq.start <- max(years) - min(year.inp) # the first number to add (2001) to the lowest birthyear input (1)
year.dist <- max(year.inp)-min(year.inp) # the distance between the highest and lowest birthyear input (82)
year.seq.end <- year.seq.start - (year.dist * 2) # the last number to add (1837) to the highest birthyear input (83)
year.seq <- seq(from = year.seq.end, to = year.seq.start, by = 2) %>% rev # all the numbers to add to each increasing number in birthyear input

for(i in 1:nrow(df)){
  for(x in 1:length(year.inp)){
    if(df[i, "birthyear"] == year.inp[x]){
      df[i, "age"] <- 2020 - (year.inp[x] + year.seq[x])
    }
  }
}

df <- select(df, -one_of("birthyear")) # I tried simply overwriting column birthyear in the loop, but that kept giving really weird numbers. No idea why
df$dem <- ifelse(df$pid == "Democrat", 1, 0)
df$male <- ifelse(df$gender == "Male", 1, 0)
df$mor.all <- (df$mor.suffer + df$mor.care + df$mor.cruel +
                 df$mor.comp + df$mor.anim + df$mor.kill) / 6
df$si.all <- (df$si.white + df$si.care + df$si.kids + 
                df$si.kill + df$si.good + df$si.help) / 6

write.csv(df, file = here("data", "pre-test", "all.batches.all.checks.passed.csv"), row.names = FALSE)

lm(hc.likert ~ hc.group + dem + empl + inc, data = df) %>% summary
lm(ev.likert ~ ev.group + dem + empl + inc, data = df) %>% summary

lm(hc.likert ~ hc.group + mor.all + si.all + dem + empl + inc, data = df) %>% summary
lm(ev.likert ~ ev.group + mor.all + si.all + dem + empl + inc, data = df) %>% summary

group_by(df, hc.group) %>% summarize(count = n())
group_by(df, ev.group) %>% summarize(count = n())




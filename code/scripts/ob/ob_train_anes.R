#################### Load and manipulate/prepare data ####################
rm(list=ls())
library(readstata13)
library(plyr)
library(tidyverse)
library(MASS)
library(data.table)
library(reshape2)
library(ggplot2)
library(here)

temp <- read.dta13(here("data", "anes", "anes_timeseries_2016.dta"), generate.factors=T, nonint.factors = TRUE)

# V161267 -- age
# V161342 -- gender
# V161310x -- race
# V161361x -- income
# V161276x -- occupation
# V161155 -- party ID
# V161270 -- education
## EVs for testing ##
# V161082: Do you approve or disapprove of the way Barack Obama is handling his job as President?
# V162192: Should the minimum wage be [raised, kept the same, lowered but not eliminated, or eliminated altogether / eliminated altogether, lowered but not eliminated, kept the same, or raised]?
# V161081: Do you feel things in this country are generally going in the right direction, or do you feel things have pretty seriously gotten off on the wrong track?
# V161087: Feeling thermometer Trump candidate
# V162257: Interest

df <- dplyr::select(temp, V161267, V161342, V161310x, V161361x, V161276x, V161155, V161270, V161082, V162192, V161081, V161087, V162257)
summary(is.na(df)) # No NAs

df <- rename(df, age = V161267, gender = V161342, race = V161310x, income =  V161361x, occupation = V161276x, pid = V161155, education = V161270,
             pres.approv = V161082, min.wage = V162192, country.track = V161081, feel.trump = V161087, interest = V162257)
nrow(df) # [1] 4270 observations


# all variables are factors

# remove observations with refused, missing etc.
df <- filter(df, !age %in% c("-9. RF (year of birth)", "-8. DK (year of birth, FTF only)"),
             !gender %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !race %in% c("-9. Missing", "6. Other non-Hispanic incl multiple races [WEB: blank 'Other' counted as a race]"),
             !income %in% c("-9. Refused", "-5. Interview breakoff (sufficient partial IW)"),
             !occupation %in% c("-9. Refused employment status"),
             !pid %in% c("-9. Refused", "-8. Don't know (FTF only)", "0. No preference (FTF ONLY)"),
             !education %in% c("95. Other SPECIFY", "-9. Refused"),
             !pres.approv %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !min.wage %in% c("-9. Refused", "-8. Don't know", "-7. No post data, deleted due to incomplete IW", "-6. No post-election interview"),
             !country.track %in% c("-9. Refused", "-8. Don't know (FTF only)"),
             !feel.trump %in% c("-99. Refused", "-89. FTF ONLY: Don't recognize ('don't know who this is')",
                                "-88. FTF ONLY: Don't know ('don't know where to rate')"),
             !interest %in% c("-9. Refused", "-8. Don't know", "-7. No post data, deleted due to incomplete IW", "-6. No post-election interview"))

# refactor to get rid of unneeded levels
for (i in 1:ncol(df)){
    df[,i] <- factor(df[,i])
  }

# manipulate data
df$age <- revalue(df$age, c("90. Age 90 or older" = "90"))
df$age <- as.numeric(as.character(df$age))
df$gender <- fct_recode(df$gender, "Male" = "1. Male", "Female" = "2. Female", "Other" = "3. Other")
df$race <- fct_recode(df$race, "White" = "1. White, non-Hispanic", "African-American" = "2. Black, non-Hispanic",
                      "Asian" = "3. Asian, native Hawaiian or other Pacif Islr,non-Hispanic",
                      "Native American" = "4. Native American or Alaska Native, non-Hispanic", 
                      "Hispanic" = "5. Hispanic")
df$occupation <- fct_recode(df$occupation, "Working" = "1. R working now (if also retired, disabled, homemaker or student, working 20 or more hrs/wk)",
                            "Unemployed" = "2. R temporarily laid off", "Unemployed" = "4. R unemployed",
                            "Retired" = "5. R retired (if also working, working <20 hrs/wk)",
                            "Disabled" = "6. R permanently disabled (if also working, working <20 hrs/wk)",
                            "Homemaker" = "7. R homemaker (if also working, working <20 hrs/wk/ incl nonworkg rs both homemaker and student)",
                            "Student" = "8. R student (if also working, working <20 hrs/wk)")
df$pid <- fct_recode(df$pid, "Democrat" = "1. Democrat",
                     "Republican" = "2. Republican",
                     "Independent" = "3. Independent",
                     "Other" = "5. Other party SPECIFY")
df$income <- fct_recode(df$income, "Under $25,000" = "01. Under $5,000", "Under $25,000" = "02. $5,000-$9,999", 
                        "Under $25,000" = "03. $10,000-$12,499", "Under $25,000" = "04. $12,500-$14,999",
                        "Under $25,000" = "05. $15,000-$17,499", "Under $25,000" = "06. $17,500-$19,999",
                        "Under $25,000" = "07. $20,000-$22,499", "Under $25,000" = "08. $22,500-$24,999", 
                        "$25,000-49,999" = "09. $25,000-$27,499", "$25,000-49,999" = "10. $27,500-$29,999",
                        "$25,000-49,999" = "11. $30,000-$34,999", "$25,000-49,999" = "12. $35,000-$39,999",
                        "$25,000-49,999" = "13. $40,000-$44,999", "$25,000-49,999" = "14. $45,000-$49,999", 
                        "$50,000-74,9999" = "15. $50,000-$54,999", "$50,000-74,9999" = "16. $55,000-$59,999",
                        "$50,000-74,9999" = "17. $60,000-$64,999", "$50,000-74,9999" = "18. $65,000-$69,999",
                        "$50,000-74,9999" = "19. $70,000-$74,999",
                        "$75,000-99,999" = "20. $75,000-$79,999", "$75,000-99,999" = "21. $80,000-$89,999",
                        "$75,000-99,999" = "22. $90,000-$99,999", 
                        "$100,000-124,999" = "23. $100,000-$109,999", "$100,000-124,999" = "24. $110,000-$124,999",
                        "$125,000-149,999" = "25. $125,000-$149,999", "$150,000-174,999" = "26. $150,000-$174,999",
                        "$175,000 or more" = "27. $175,000-$249,999", "$175,000 or more" = "28. $250,000 or more")
df$pres.approv <- fct_recode(df$pres.approv, "Approve" = "1. Approve", "Disapprove" = "2. Disapprove")
df$min.wage <- fct_recode(df$min.wage, "Raised" = "1. Raised", "Kept the same" = "2. Kept the same", "Lowered" = "3. Lowered",
                          "Eliminated" = "4. Eliminated")
df$country.track <- fct_recode(df$country.track, "Right direction" = "1. Right direction", "Wrong track" = "2. Wrong track")
df$feel.trump <- as.numeric(as.character(df$feel.trump))

df$education <- fct_recode(df$education, 
                            "Up to 1st" = "1. Less than 1st grade",
                            "1st-4th" = "2. 1st, 2nd, 3rd or 4th grade", 
                            "5th-6th" = "3. 5th or 6th grade",
                            "7th-8th" = "4. 7th or 8th grade", 
                            "9th" = "5. 9th grade", 
                            "10th" = "6. 10th grade",
                            "11th" = "7. 11th grade", 
                            "12th" = "8. 12th grade no diploma",
                            "HS grad" = "9. High school graduate- high school diploma or equivalent (for example: GED)",
                            "HS grad" = "90. Other specify given as: high school graduate",
                            "Some college" = "10. Some college but no degree", 
                            "Associate" = "11. Associate degree in college - occupational/vocational program", 
                            "Associate" = "12. Associate degree in college -- academic program",
                            "Bachelor's" = "13. Bachelor's degree (for example: BA, AB, BS)",
                            "Master's" = "14. Master's degree (for example: MA, MS, MENG, MED, MSW, MBA)",
                            "Professional" = "15. Professional school degree (for example: MD, DDS, DVM, LLB, JD)",
                            "Doctorate" = "16. Doctorate degree (for example: PHD, EDD)")
df$interest <- fct_recode(df$interest,
                          "Very closely" = "1. Very closely",
                          "Fairly closely" = "2. Fairly closely",
                          "Not very closely" = "3. Not very closely",
                          "Not at all" = "4. Not at all")

df$education.num <- as.character(as.numeric(df$education))    # needed for the loop later

# save df to be used to create opm functions (separate .R file)

saveRDS(df, here("data", "anes", "anes_cleaned.rds"))



#################### Run ordered probit model for selected education categories ####################

load(here("functions", "OPMord.RData"))  # created in "opm_create_function.R"

dv <- "education"
evs <- c("age", "gender", "race", "income", "occupation", "pid")
ord.list <- OPMord(data = df, dv = dv, evs = evs)

# save ord.list to be used in other scripts
saveRDS(ord.list, here("data", "anes", "ord_list.rds"))

# plot and save Coefficients/Thresholds visualization with horizontal error bars of SE
ggplot(ord.list$int.df, aes(Values, Intercepts)) + geom_point(size = 1.5) + geom_errorbarh(aes(xmin=Values-SE, xmax=Values+SE), height = .3) +
   ggtitle("Ordered Thresholds for Education Categories")
ggsave(here("plots", "thresholds.pdf"), height=7, width=7, units='in')
# save .csv with Thresholds, Coefficients, SEs, t-values
write.csv(ord.list$int.df, file = here("data", "thresholds.csv"), row.names = FALSE)

# plot and save distribution of original education categories
ggplot(ord.list$data, aes(x = education)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "darkred") + ylab("Percentages") + xlab("Education") + 
    ggtitle("Distribution of Original Education Categories") + theme(plot.title = element_text(hjust = 0.5))
ggsave(here("plots", "barplot_orig.pdf"), height=7, width=7, units='in')
# plot and save distribution of re-estimated education categories
ggplot(ord.list$data, aes(x = education.new)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "darkred") + ylab("Percentages") +
  xlab("Education") + ggtitle("Distribution of OPM Education Categories") + theme(plot.title = element_text(hjust = 0.5))
ggsave(here("plots", "barplot_new.pdf"), height=7, width=7, units='in')
# save data with new education variable
saveRDS(ord.list$data, file = here("data", "anes", "anes_education.rds"))

 ### Code above ###
## Stores .pdf and .csv files to save thresholds
# .pdf file plots coefficient values by education categories, with error bars the length of the SEs around them
#. csv file saves all polr() output for the coefficients (leaving out the EV ones)
## Stores .pdf files to save distribution of categories
# barplot_orig saves the dist of the original categories
# barplot_new saves the dist of the re-estimated categories
## Function stores lots of output if I need to access it later

 


##### April 11 2019 #####

# Spoke to Ryan today. He asked why I used $lp and not $fitted.values, which lists the probabilities of assignment for each observation for each education category. I showed him the categories resulting from using lp
levels(ord.list$data$education.new)
# We then ran 
table(colnames(ord.list$plr.out$fitted.values)[max.col(ord.list$plr.out$fitted.values,ties.method="first")])
# which determines the education category with the highest prob of assignment for each observation and then lists the count in these categories
# The categoriy Associate was gone! The $lp transformation shows 5 categories, but the $fitted show only 4 -- what is going on here? 
# To investigate, Ryan asked me to check the percentages of observations within each of the 5 lp categories and each of the 4 fitted categories
# lp

(twelve.hs <- ord.list$int.df[8,2])
(hs.some <- ord.list$int.df[9,2])
(some.ass <- ord.list$int.df[10,2])
(ass.ba <- ord.list$int.df[11,2])
(ba.ma <- ord.list$int.df[12,2])
(ma.pro <- ord.list$int.df[13,2])

lp <- ord.list$plr.out$lp

library(dplyr)
hs <- length(lp[between(lp, twelve.hs, hs.some)])/length(lp)
some <- length(lp[between(lp, hs.some, some.ass)])/length(lp)
ass <- length(lp[between(lp, some.ass, ass.ba)])/length(lp)
ba <- length(lp[between(lp, ass.ba, ba.ma)])/length(lp)
ma <- length(lp[between(lp, ba.ma, ma.pro)])/length(lp)

lin.pred <- data.frame(rbind(c(ba, hs, ma, some, ass)))
colnames(lin.pred) <- c("Bachelor's", "HS grad", "Master's", "Some college", "Associate")
lin.pred                                                      

# fitted

fit <- prop.table(table(colnames(ord.list$plr.out$fitted.values)[max.col(ord.list$plr.out$fitted.values,ties.method="first")]))
fit

# These resuts are confusing me. `Some college` is lower in `fit`, but everything else is higher. It looks almost as if `Associate` was redistributed amongst the other three categories in `fit`
# Asked Ryan what he makes of this
# He never responded, so I'm leaving this be




library(magrittr)
library(here)
library(blockTools)
library(tidyverse)

# these are the data with the new education levels, created in ob_train_anes.R
# OPMord has already been applied to these data, so I just need to block
df <- readRDS(here("data", "anes", "anes_education.rds")) 

df$id <- 1:nrow(df) # add "id" column for blocking
vars <- c("age", "gender", "race", "income", "occupation", 
          "pid", "pres.approv", "min.wage", "country.track", 
          "feel.trump", "id")
n.tr <- 5
means.an <- c()
means.op <- c()

df.an <- df[, c(vars, "education.num")] %>% 
  rename(education = education.num)
df.op <- df[, c(vars, "education.new.num")] %>% 
  rename(education = education.new.num)

assigned.an <- assignment(block(df.an, id.vars = "id", block.vars = "education", n.tr = n.tr), seed = 123)
assigned.op <- assignment(block(df.op, id.vars = "id", block.vars = "education", n.tr = n.tr), seed = 123)

saveRDS(df.an, here("data", "ob", "df_an.rds"))
saveRDS(df.op, here("data", "ob", "df_op.rds"))
saveRDS(assigned.an, here("data", "ob", "assigned_an.rds"))
saveRDS(assigned.op, here("data", "ob", "assigned_op.rds"))




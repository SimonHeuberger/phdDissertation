
# combine all respondents with age 18 and send the respective RIDs to Lucid. They can send me the real ages
# won't run without the saved 18.op and 18.an .csv files (creation code commented out in experiment.an.R and experiment.op.R)

library(here)

op.18 <- read.csv(here("data", "experiment", "RIDs.resp.with.age.18.op.csv"), as.is = TRUE)
an.18 <- read.csv(here("data", "experiment", "RIDs.resp.with.age.18.an.csv"), as.is = TRUE)

all.18 <- rbind(op.18, an.18)
nrow(all.18)

write.csv(all.18, file = here("data", "experiment", "RIDs.resp.with.age.18.all.csv"), row.names = FALSE)


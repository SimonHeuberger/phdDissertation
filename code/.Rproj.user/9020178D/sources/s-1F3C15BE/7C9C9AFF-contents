#### Editing, running and testing the shiny app ####

## Edit my questions

library(here) # if this shows /shiny, restart the script within code.Rproj
# here() does not work in the usual sense here, since it doesn't play nicely with system(). Better to keep setwd() alongside it
# I'm using here::here() because plyr masks here() - and plyr is loaded when shiny_psych_survey.R is sourced

# Demographic questions
setwd(here::here("shiny", "questions"))
filed <- "instructions"
filed <- "demographics"
filed <- "education"
filed <- "code"
filed <- "pid_foll_dem"
filed <- "pid_foll_rep"
filed <- "pid_foll_ind_else"

temp <- read.csv(paste0(filed,".csv"))
system(paste("open", paste0(filed,".csv"))) 
write.table(temp, paste0(filed,".txt"), sep="\t", quote = FALSE, row.names = F)
system(paste("open", paste0(filed,".txt")))   


# Treatment questions
setwd(here::here("shiny", "questions", "treatment"))
filed <- "mw.control"
filed <- "mw.m.opp"
filed <- "mw.m.supp"
filed <- "mw.p.opp"
filed <- "mw.p.supp"
filed <- "tb.control"
filed <- "tb.m.opp"
filed <- "tb.m.supp"
filed <- "tb.p.opp"
filed <- "tb.p.supp"

temp <- read.csv(paste0(filed,".csv"))
system(paste("open", paste0(filed,".csv"))) 
write.table(temp, paste0(filed,".txt"), sep="\t", quote = FALSE, row.names = F)
system(paste("open", paste0(filed,".txt")))   


## Load original questions from folder /extdata if I want to look at some lines
filee <- "Instructions_Survey"
filee <- "Demographics"
filee <- "Survey_Example"
filee <- "Goodbye"
fil <- system.file("extdata", paste0(filee,".txt"), package = "ShinyPsych")   # read the text straight from the package
inst.df <- read.table(fil, header = TRUE, sep = "\t", stringsAsFactors = FALSE)        # create a data frame named "inst.df"
write.csv(inst.df, file = paste0(filee,".csv"), row.names = F)                # write the data frame to a .csv
system(paste("open", paste0(filee,".csv")))                                   # open the .csv in Excel where I can edit and save it

## Run modified ShinyPsych code to structure setup; create app (.csv files are saved on AU Dropbox)
setwd(here::here("shiny"))
source("shiny_psych_survey.R")
shinyApp(ui = ui, server = server)

### ANY CHANGES TO /survey_experiment/app.R NEED TO BE DONE MANUALLY, NOT WITH CODE -- FOR SAFETY ###


## Test deployed app code after I make any manual changes
setwd(here::here("shiny", "survey_experiment"))
library(shiny)
runApp()

## (Re-)Deploy app
library(rsconnect)
rsconnect::deployApp(here::here("shiny", "survey_experiment"))
Y

#### Data analysis ####

## Run the loadData() function to download/list/unlist the data from Dropbox; read in the .csv; look at the data

library(rdrop2)
library(plyr)
setwd(here::here("shiny"))
drop_auth(rdstoken = "droptoken.rds")
loadData <- function() {                     
  # Load library and set outputDir
  outputDir <- "alldata"              # loads all responses from AU Dropbox /alldata
  # Read in all .csv files
  rdrop2:::drop_is_folder(outputDir)
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  # Turn all files into a list
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data frame
  data <- ldply(data, data.frame)
  # Write data frame to .csv in local folder /alldata
  write.csv(data, file = "alldata/all_data.csv", row.names = F) 
}

setwd(here::here("shiny"))
loadData()
all_data <- read.csv("alldata/all_data.csv")
View(all_data)


## Run drop_download() to download the blocked .RData files, load them, look at the data frame 

setwd(here::here("shiny"))
library(rdrop2)
drop_auth(rdstoken = "droptoken.rds")
drop_download("seqblock/seqmw.RData", overwrite = TRUE)
load("seqmw.RData")
mw <- bdata$x
drop_download("seqblock/seqtb.RData", overwrite = TRUE)
load("seqtb.RData")
tb <- bdata$x
View(mw)
View(tb)




#### Comparing to the original version of shinyPsych ####

## Run original ShinyPsych code; create app
setwd(here::here("shiny"))
source("/Library/Frameworks/R.framework/Versions/3.5/Resources/library/ShinyPsych/shiny-examples/Survey/app.R")
shinyApp(ui = ui, server = server)



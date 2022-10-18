# define user data directory and history file location
local({
  dataDir <- "userdata"
  if (identical(.Platform$OS.type, "windows"))
    username <- Sys.getenv("USERNAME")
  else
    username <-  Sys.getenv("USER")                   
  userDir <- file.path(dataDir, username)
  if (!file.exists(userDir))
    dir.create(userDir, recursive = TRUE)
  userDir <- normalizePath(userDir)
  
  # locate history in user data dir
  Sys.setenv(R_HISTFILE = file.path(userDir, ".Rhistory"))
})

##### Data for the Northman
# set working directory
setwd("~/R-Work/Social Media Analytics/FINAL PROJECT")

#install.packages("rtweet")
library(rtweet)

Northman <- search_tweets("TheNorthman OR #TheNorthman", 
                          lang="en",
                          n = 18000, 
                          retryonratelimit = TRUE
 )

write_as_csv(Northman, "northman_0504.csv")
northman_0504 = read_twitter_csv("northman_0504.csv")
northman_0504$text[1]



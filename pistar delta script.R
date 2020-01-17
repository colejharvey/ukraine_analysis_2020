rm(list=ls())
library(tidyverse)
library(lme4)
library(arm)

###Starting here
electoral.all <- read.csv("C:/Users/Cole/Documents/Python projects/py-scrape-and-download/ukraine elections combined 2002-2014.csv")
electoral.all <- as_tibble(electoral.all)

electoral.all <- electoral.all %>% mutate(electionid = as.factor(paste(year, presidential, round.2, sep="_")))

electoral.all <- electoral.all %>% mutate(east.digit = main_east %% 10)
electoral.all <- electoral.all %>% mutate(west.digit = main_west %% 10)


##Pistar and delta

gd <- function(i) sum(abs(i-mean(i)))/(2*sum(i))  #Delta function


for(i in unique(electoral.all$electionid)){
  election.sub <- subset(electoral.all, electoral.all$electionid == i)
  pstar.delta.values<-matrix(NA, nrow=max(election.sub$regionid, na.rm=TRUE), ncol=6)
  pstar.delta.values <- as_tibble(pstar.delta.values)
  pstar.delta.values[,1] <- i
  for (j in 1:nrow(pstar.delta.values)){
    group<-subset(election.sub, regionid==j)
    #   Model density, e = expected
    e <- rep(.1, 1e1)
    names(e) <- names(d.table1) <- 0:9
    if (nrow(group) > 0){
      table1<-table(group$east.digit)
      table2<-table(group$west.digit)

      d.table1 <- table1/sum(table1)
      d.table2 <- table2/sum(table2)
      
      #   Compute the pi*
      ps.e <- 1 - 1/max(e/d.table1)
      ps.w <- 1 - 1/max(e/d.table2)

      
      #  Compute delta
      
      d.east <- gd(table1)
      d.west <- gd(table2)
      
      pstar.delta.values[j, 2] <- j
      pstar.delta.values[j, 3] <- round(ps.e, 4)
      pstar.delta.values[j, 4] <- round(ps.w, 4)
      pstar.delta.values[j, 5] <- round(d.east, 4)
      pstar.delta.values[j, 6] <- round(d.west, 4)
      
    }
    else{next}
  }
  colnames(pstar.delta.values) <- c("electionid", "regionid", "ps.east", "ps.west", "d.east", "d.west")
  write.csv(pstar.delta.values, paste("ukraine pistar delta estimates election_", i, ".csv"))
  
}

###Rbind the saved files

myMergedData <- 
  do.call(rbind,
          lapply(list.files(pattern = "pistar"), read.csv))

###Create uniqueid in new file

myMergedData <- myMergedData %>% mutate(unique.id = paste(electionid, regionid, sep = "_"))

###Read in main data

main.data <- read.csv("Ukraine main dataset.csv")

###Merge by unique.id

main.data <- merge(main.data, myMergedData, by="unique.id")

###Save results

write.csv(main.data, "Ukraine main dataset.csv")

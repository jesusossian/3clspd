library(tidyverse)
library(stringr)
library(dplyr)
library(xtable)
library(readr)
library(psych)



insttype <- 'bal'
N<-50
T <- 15

#Small
saidamc <- read_delim(paste("barrier/uncap_",insttype,"_mc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)
saidaranddpheurmc <- read_delim(paste("barrier/uncap_",insttype,"_randdpheurmc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)
saidaranddpheur <- read_delim(paste("uncap_",insttype,"_randdpheur_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)

for(N in list(100,200)){
  saidamc <- rbind(saidamc,read_delim(paste("barrier/uncap_",insttype,"_mc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
  saidaranddpheurmc <- rbind(saidaranddpheurmc,read_delim(paste("barrier/uncap_",insttype,"_randdpheurmc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
  saidaranddpheur <- rbind(saidaranddpheur,read_delim(paste("uncap_",insttype,"_randdpheur_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
}

#saida randdpheur columns: bestcost; averagecost; elapsedtime; dptype; dpalpha

saidaranddpheurmc$disablesolver <- NA

saidamc$percfixed <- NA
saidamc$heursol <- NA


##Clean up data 
# remove data in wrong output csv
saidamc <- subset(saidamc,form=="mc")
saidaranddpheurmc <- subset(saidaranddpheurmc,form=="randdpheurmc")
#saidaranddpheur <- subset(saidaranddpheur,form=="randdpheur")
#remove repeated executions for same instance
dflist <- list(saidamc,saidaranddpheur,saidaranddpheurmc)
for (df in dflist){
  df <- distinct(df,instance, .keep_all = TRUE)
}


saida <- rbind(saidamc,saidaranddpheurmc) #merge all dataframes in saida
saida <- saida[ , -which(names(saida) %in% c("disablesolver"))]


saidaranddpheur <- saidaranddpheur[ , -which(names(saidaranddpheur) %in% c("dptype","dpalpha"))]

saidaranddpheur$form <- "randdpheur"

for (i in 1:nrow(saida)){
  if (saida[i,]$bestbound < 0){
    saida[i,]$bestbound <- 0
  }
}

finaltable <- saida[,which(names(saida) %in% c("instance"))] #create table with names of the instances
finaltable <- finaltable[!duplicated(finaltable), ] #remove duplicates

formulations <- unique(saida$form)
for (f in formulations){
  newsaida <- subset(saida, f == form)
  newsaida <- newsaida[ , -which(names(newsaida) %in% c("form"))]
  colnames(newsaida)[2:length(colnames(newsaida))] <- paste(f, colnames(newsaida)[2:length(colnames(newsaida))], sep = "_")
  
  finaltable = merge(x=finaltable,y=newsaida,by="instance")
}

formulations2 <- unique(saidaranddpheur$form)
for (f in c("randdpheur")){
  newsaida <- subset(saidaranddpheur, f == form)
  newsaida <- newsaida[ , -which(names(newsaida) %in% c("form"))]
  colnames(newsaida)[2:length(colnames(newsaida))] <- paste(f, colnames(newsaida)[2:length(colnames(newsaida))], sep = "_")
  
  finaltable = merge(x=finaltable,y=newsaida,by="instance")
}


testeRand <- finaltable


#Rand, Ipercube, Toro
#testeRand <- dplyr::filter(teste, grepl('Rand', instance))

#Grid, Grid_NQ
#testeRand <- dplyr::filter(teste, grepl('Grid', instance))
#testeRand$instance <- gsub('Grid_NQ', 'GridNQ', testeRand$instance)

#Grid_, GridNQ_
#testeRand <- dplyr::filter(testeRand, grepl('Grid_', instance))



#testeRand <- testeRand[ , -which(names(testeRand) %in% c("graphtype"))]

#testeRand[,1]

#testeRand[1,1]


testeRand <- cbind(testeRand, NR = NA)
testeRand <-cbind(testeRand, NT = NA)
testeRand <-cbind(testeRand, NW = NA)
testeRand <-cbind(testeRand, id = NA)
testeRand <- cbind(testeRand, typeF = NA)
testeRand <- cbind(testeRand, typeD = NA)


for (i in 1:nrow(testeRand)){
  
  strtoprocess <- testeRand[i,]$instance
  strtoprocess <- gsub("\\..*","",strtoprocess) #Remove .fvs from last element
  
  
  vec <- as.vector(unlist(str_split(strtoprocess, "/")))
  
  strtoprocess <- vec[length(vec)]
  
  testeRand[i,]$instance <- strtoprocess
  
  if (str_detect(testeRand[i,]$instance,"SF")){
    testeRand[i,]$typeF <- "SF"
  }else{
    testeRand[i,]$typeF <- "DF"
  }
  if (str_detect(testeRand[i,]$instance,"SD")){
    testeRand[i,]$typeD <- "SD"
  }else{
    testeRand[i,]$typeD <- "DD"
  }
  
  if (str_detect(testeRand[i,]$instance,"N50")){
    testeRand[i,]$NR <- 50
  }else if(str_detect(testeRand[i,]$instance,"N100")){
    testeRand[i,]$NR <- 100
  }else{
    testeRand[i,]$NR <- 200
  }
  
  if (str_detect(testeRand[i,]$instance,"T15")){
    testeRand[i,]$NT <- 15
  }else{
    testeRand[i,]$NT <- 30
  }
  
  
  if (str_detect(testeRand[i,]$instance,"W5")){
    testeRand[i,]$NW <- 5
  }else if(str_detect(testeRand[i,]$instance,"W10")){
    testeRand[i,]$NW <- 10
  }else if(str_detect(testeRand[i,]$instance,"W15")){
    testeRand[i,]$NW <- 15
  }else{
    testeRand[i,]$NW <- 20
  }
  
  testeRand[i,]$id <- substr(strtoprocess,nchar(strtoprocess),nchar(strtoprocess))
  
}

#testeRand
testeRand <- testeRand[order(testeRand$NR,testeRand$NT,testeRand$NW,testeRand$typeD,testeRand$typeF,testeRand$id),]


testeRand <-cbind(testeRand, mc_opt = NA)
testeRand <-cbind(testeRand, randdpheurmc_opt = NA)


for (i in 1:nrow(testeRand)){
  testeRand[i,]$mc_opt <- ifelse(100*(testeRand[i,]$mc_bestsol - testeRand[i,]$mc_bestbound)/testeRand[i,]$mc_bestsol < 0.0001,1,0)
  testeRand[i,]$randdpheurmc_opt <- ifelse(100*(testeRand[i,]$randdpheurmc_bestsol - testeRand[i,]$randdpheurmc_bestbound)/testeRand[i,]$randdpheurmc_bestsol < 0.0001,1,0)
  
  #testeRand[i,]$randdpheurmc_opt <- ifelse(testeRand[i,]$randdpheurmc_bestsol - testeRand[i,]$randdpheurmc_bestbound < 0.00001,1,0)
  #testeRand[i,]$flow_opt <- ifelse(testeRand[i,]$flow_bestbound - testeRand[i,]$flow_bestsol < 0.00001,1,0)
  #testeRand[i,]$fvs_opt <- ifelse(testeRand[i,]$fvs_bestbound - testeRand[i,]$fvs_bestsol < 0.00001,1,0)
  #testeRand[i,]$cycle_opt <- ifelse(testeRand[i,]$cycle_bestbound - testeRand[i,]$cycle_bestsol < 0.00001,1,0)
  #testeRand[i,]$directedconnectivity_opt <- ifelse(testeRand[i,]$directedconnectivity_bestbound - testeRand[i,]$directedconnectivity_bestsol < 0.00001,1,0)
}

testeRand <-cbind(testeRand, mc_solvedtime = NA)
testeRand <-cbind(testeRand, randdpheurmc_solvedtime = NA)

for (i in 1:nrow(testeRand)){
  testeRand[i,]$mc_solvedtime <- ifelse(testeRand[i,]$mc_opt == 1 ,testeRand[i,]$mc_time,NA)
  testeRand[i,]$randdpheurmc_solvedtime <- ifelse(testeRand[i,]$randdpheurmc_opt == 1 ,testeRand[i,]$randdpheurmc_time,NA)
}

testeRand <-cbind(testeRand, mc_solvednodes = NA)
testeRand <-cbind(testeRand, randdpheurmc_solvednodes = NA)


for (i in 1:nrow(testeRand)){
  testeRand[i,]$mc_solvednodes <- ifelse(testeRand[i,]$mc_opt == 1 ,testeRand[i,]$mc_numnodes,NA)
  testeRand[i,]$randdpheurmc_solvednodes <- ifelse(testeRand[i,]$randdpheurmc_opt == 1 ,testeRand[i,]$randdpheurmc_numnodes,NA)
}


testeRand <-cbind(testeRand, mc_unsolvedgap = NA)
testeRand <-cbind(testeRand, randdpheurmc_unsolvedgap = NA)


for (i in 1:nrow(testeRand)){
  testeRand[i,]$mc_unsolvedgap <- ifelse(testeRand[i,]$mc_opt != 1 ,100*(testeRand[i,]$mc_bestsol - testeRand[i,]$mc_bestbound)/testeRand[i,]$mc_bestsol,NA)
  testeRand[i,]$randdpheurmc_unsolvedgap <- ifelse(testeRand[i,]$randdpheurmc_opt != 1 ,100*(testeRand[i,]$randdpheurmc_bestsol - testeRand[i,]$randdpheurmc_bestbound)/testeRand[i,]$randdpheurmc_bestsol,NA)
}

testeRand <- cbind(testeRand, randdpheur_bestgap = NA)
for (i in 1:nrow(testeRand)){
  bestvalue <- min(testeRand[i,]$mc_bestsol,testeRand[i,]$randdpheurmc_bestsol)
  testeRand[i,]$randdpheur_bestgap <- 100*(testeRand[i,]$randdpheur_bestcost - bestvalue)/bestvalue
}

output <- testeRand %>% 
  group_by(NR,NT,NW,typeD,typeF) %>% 
  summarize(mc_bestsol = mean(mc_bestsol,na.rm=TRUE),
    mc_opt = sum(mc_opt),
    mc_time = mean(mc_solvedtime,na.rm=TRUE),
    mc_nodes = mean(mc_solvednodes,na.rm=TRUE),
    mc_gap = mean(mc_unsolvedgap,na.rm=TRUE),
    randdpheur_bestsol = mean(randdpheur_bestcost,na.rm=TRUE),
    randdpheur_averagecost = mean(randdpheur_averagecost,na.rm=TRUE),
    randdpheur_time = mean(randdpheur_elapsedtime,na.rm=TRUE),
    randdpheur_bestgap = mean(randdpheur_bestgap,na.rm=TRUE),
    randdpheurmc_bestsol = mean(randdpheurmc_bestsol,na.rm=TRUE),
    randdpheurmc_opt = sum(randdpheurmc_opt),
    randdpheurmc_time = mean(randdpheurmc_solvedtime,na.rm=TRUE),
    randdpheurmc_nodes = mean(randdpheurmc_solvednodes,na.rm=TRUE),
    randdpheurmc_gap = mean(randdpheurmc_unsolvedgap,na.rm=TRUE),
    randdpheurmc_percfixed = mean(randdpheurmc_percfixed,na.rm=TRUE)
  )

output$instancegroup <- as.character(NA)

for (i in 1:nrow(output)){
  output[i,]$instancegroup <- paste(output[i,]$NR,output[i,]$NT,output[i,]$NW,output[i,]$typeD,output[i,]$typeF,sep="_")
}





output <- output[ , -which(names(output) %in% c("NR","NT","NW","typeD","typeF"))]
output <- output[,c("instancegroup",setdiff(names(output),"instancegroup"))]

#Adding average line
output[nrow(output)+1,] <- NA
output[nrow(output),]$instancegroup <- "Average"
output[nrow(output),]$mc_time <- mean(output$mc_time,na.rm = TRUE)
output[nrow(output),]$randdpheurmc_time <- mean(output$randdpheurmc_time,na.rm = TRUE)
output[nrow(output),]$randdpheur_time <- mean(output$randdpheur_time,na.rm = TRUE)
output[nrow(output),]$randdpheur_bestgap <- mean(output$randdpheur_bestgap,na.rm = TRUE)
output[nrow(output),]$mc_nodes <- mean(output$mc_nodes,na.rm = TRUE)
output[nrow(output),]$randdpheurmc_nodes <- mean(output$randdpheurmc_nodes,na.rm = TRUE)
output[nrow(output),]$randdpheurmc_percfixed <- mean(output$randdpheurmc_percfixed,na.rm = TRUE)

#Adding total line
output[nrow(output)+1,] <- NA
output[nrow(output),]$instancegroup <- "Total"
output[nrow(output),]$mc_opt <- sum(output$mc_opt,na.rm = TRUE)
output[nrow(output),]$randdpheurmc_opt <- sum(output$randdpheurmc_opt,na.rm = TRUE)



#write.csv(output,"random.csv", row.names = FALSE)

#print(xtable(output,digits=c(0,0,1,0,1,1,2,1,1,1,1,0,1,1,2,1)),include.rownames = FALSE) #with bestsol without bestgap for randpheur
print(xtable(output,digits=c(0,0,1,0,1,1,2,1,1,1,1,1,0,1,1,2,1)),include.rownames = FALSE) #with bestsol without bestgap for randpheur

#print(xtable(output,digits=c(0,0,0,1,1,2,0,1,1,2,0,1,1,2,0,1,1,2)),include.rownames = FALSE) #without bestsol




library(tidyverse)
library(stringr)
library(dplyr)
library(xtable)
library(readr)
library(psych)

insttype <- 'unbal'
N<-50
T <- 30

#Small
saidastd <- read_delim(paste("uncap_",insttype,"_std_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)
saidabcstd <- read_delim(paste("uncap_",insttype,"_bcstd_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)
saida3level <- read_delim(paste("uncap_",insttype,"_3level_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)
saidabc3level <- read_delim(paste("uncap_",insttype,"_bc3level_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)


for(N in list(100,200)){
  saidastd <- rbind(saidastd,read_delim(paste("uncap_",insttype,"_std_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
  saidabcstd <- rbind(saidabcstd,read_delim(paste("uncap_",insttype,"_bcstd_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
  saida3level <- rbind(saida3level,read_delim(paste("uncap_",insttype,"_3level_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
  saidabc3level <- rbind(saidabc3level,read_delim(paste("uncap_",insttype,"_bc3level_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
}


saida3level$disablesolver <- NA


##Clean up data 
# remove data in wrong output csv
saidastd <- subset(saidastd,form=="std")
saidabcstd <- subset(saidabcstd,form=="bcstd")
saida3level <- subset(saida3level,form=="3level")
saidabc3level <- subset(saidabc3level,form=="bc3level")
#remove repeated executions for same instance
dflist <- list(saidastd,saidabcstd,saida3level,saidabc3level)
for (df in dflist){
  df <- distinct(df,instance, .keep_all = TRUE)
}


  



saida <- rbind(saidastd,saida3level,saidabcstd,saidabc3level) #merge all dataframes in saida
saida <- saida[ , -which(names(saida) %in% c("disablesolver"))]

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


testeRand <-cbind(testeRand, std_opt = NA)
testeRand <-cbind(testeRand, threelevel_opt = NA)
testeRand <-cbind(testeRand, bcstd_opt = NA)
testeRand <-cbind(testeRand, bc3level_opt = NA)

for (i in 1:nrow(testeRand)){
  testeRand[i,]$std_opt <- ifelse(100*(testeRand[i,]$std_bestsol - testeRand[i,]$std_bestbound)/testeRand[i,]$std_bestsol < 0.0001,1,0)
  testeRand[i,]$threelevel_opt <- ifelse(100*(testeRand[i,]$'3level_bestsol' - testeRand[i,]$'3level_bestbound')/testeRand[i,]$'3level_bestsol' < 0.0001,1,0)
  testeRand[i,]$bcstd_opt <- ifelse(100*(testeRand[i,]$bcstd_bestsol - testeRand[i,]$bcstd_bestbound)/testeRand[i,]$bcstd_bestsol < 0.0001,1,0)
  testeRand[i,]$bc3level_opt <- ifelse(100*(testeRand[i,]$bc3level_bestsol - testeRand[i,]$bc3level_bestbound)/testeRand[i,]$bc3level_bestsol < 0.0001,1,0)
  
  #testeRand[i,]$randdpheurmc_opt <- ifelse(testeRand[i,]$randdpheurmc_bestsol - testeRand[i,]$randdpheurmc_bestbound < 0.00001,1,0)
  #testeRand[i,]$flow_opt <- ifelse(testeRand[i,]$flow_bestbound - testeRand[i,]$flow_bestsol < 0.00001,1,0)
  #testeRand[i,]$fvs_opt <- ifelse(testeRand[i,]$fvs_bestbound - testeRand[i,]$fvs_bestsol < 0.00001,1,0)
  #testeRand[i,]$cycle_opt <- ifelse(testeRand[i,]$cycle_bestbound - testeRand[i,]$cycle_bestsol < 0.00001,1,0)
  #testeRand[i,]$directedconnectivity_opt <- ifelse(testeRand[i,]$directedconnectivity_bestbound - testeRand[i,]$directedconnectivity_bestsol < 0.00001,1,0)
}

testeRand <-cbind(testeRand, std_solvedtime = NA)
testeRand <-cbind(testeRand, threelevel_solvedtime = NA)
testeRand <-cbind(testeRand, bcstd_solvedtime = NA)
testeRand <-cbind(testeRand, bc3level_solvedtime = NA)

for (i in 1:nrow(testeRand)){
  testeRand[i,]$std_solvedtime <- ifelse(testeRand[i,]$std_opt == 1 ,testeRand[i,]$std_time,NA)
  testeRand[i,]$threelevel_solvedtime <- ifelse(testeRand[i,]$threelevel_opt == 1 ,testeRand[i,]$'3level_time',NA)
  testeRand[i,]$bcstd_solvedtime <- ifelse(testeRand[i,]$bcstd_opt == 1 ,testeRand[i,]$bcstd_time,NA)
  testeRand[i,]$bc3level_solvedtime <- ifelse(testeRand[i,]$bc3level_opt == 1 ,testeRand[i,]$bc3level_time,NA)
  #testeRand[i,]$flow_solvedtime <- ifelse(testeRand[i,]$flow_opt == 1,testeRand[i,]$flow_cputime,NA)
  #testeRand[i,]$fvs_solvedtime <- ifelse(testeRand[i,]$fvs_opt == 1,testeRand[i,]$fvs_cputime,NA)
  #testeRand[i,]$cycle_solvedtime <- ifelse(testeRand[i,]$cycle_opt == 1,testeRand[i,]$cycle_cputime,NA)
  #testeRand[i,]$directedconnectivity_solvedtime <- ifelse(testeRand[i,]$directedconnectivity_opt == 1,testeRand[i,]$directedconnectivity_cputime,NA)
}

testeRand <-cbind(testeRand, std_solvednodes = NA)
testeRand <-cbind(testeRand, threelevel_solvednodes = NA)
testeRand <-cbind(testeRand, bcstd_solvednodes = NA)
testeRand <-cbind(testeRand, bc3level_solvednodes = NA)


for (i in 1:nrow(testeRand)){
  testeRand[i,]$std_solvednodes <- ifelse(testeRand[i,]$std_opt == 1 ,testeRand[i,]$std_numnodes,NA)
  testeRand[i,]$threelevel_solvednodes <- ifelse(testeRand[i,]$threelevel_opt == 1 ,testeRand[i,]$'3level_numnodes',NA)
  testeRand[i,]$bcstd_solvednodes <- ifelse(testeRand[i,]$bcstd_opt == 1 ,testeRand[i,]$bcstd_numnodes,NA)
  testeRand[i,]$bc3level_solvednodes <- ifelse(testeRand[i,]$bc3level_opt == 1 ,testeRand[i,]$bc3level_numnodes,NA)
}


testeRand <-cbind(testeRand, std_unsolvedgap = NA)
testeRand <-cbind(testeRand, threelevel_unsolvedgap = NA)
testeRand <-cbind(testeRand, bcstd_unsolvedgap = NA)
testeRand <-cbind(testeRand, bc3level_unsolvedgap = NA)


for (i in 1:nrow(testeRand)){
  testeRand[i,]$std_unsolvedgap <- ifelse(testeRand[i,]$std_opt != 1 ,100*(testeRand[i,]$std_bestsol - testeRand[i,]$std_bestbound)/testeRand[i,]$std_bestsol,NA)
  testeRand[i,]$threelevel_unsolvedgap <- ifelse(testeRand[i,]$threelevel_opt != 1 ,100*(testeRand[i,]$'3level_bestsol' - testeRand[i,]$'3level_bestbound')/testeRand[i,]$'3level_bestsol',NA)
  testeRand[i,]$bcstd_unsolvedgap <- ifelse(testeRand[i,]$bcstd_opt != 1 ,100*(testeRand[i,]$bcstd_bestsol - testeRand[i,]$bcstd_bestbound)/testeRand[i,]$bcstd_bestsol,NA)
  testeRand[i,]$bc3level_unsolvedgap <- ifelse(testeRand[i,]$bc3level_opt != 1 ,100*(testeRand[i,]$bc3level_bestsol - testeRand[i,]$bc3level_bestbound)/testeRand[i,]$bc3level_bestsol,NA)
}



names(testeRand)[names(testeRand) == '3level_bestsol'] <- 'threelevel_bestsol'

output <- testeRand %>% 
  group_by(NR,NT,NW,typeD,typeF) %>% 
  summarize(#std_bestsol = mean(std_bestsol,na.rm=TRUE),
            std_opt = sum(std_opt),
            std_time = mean(std_solvedtime,na.rm=TRUE),
            std_nodes = mean(std_solvednodes,na.rm=TRUE),
            std_gap = mean(std_unsolvedgap,na.rm=TRUE),
            #threelevel_bestsol = mean(threelevel_bestsol,na.rm=TRUE),
            threelevel_opt = sum(threelevel_opt),
            threelevel_time = mean(threelevel_solvedtime,na.rm=TRUE),
            threelevel_nodes = mean(threelevel_solvednodes,na.rm=TRUE),
            threelevel_gap = mean(threelevel_unsolvedgap,na.rm=TRUE),
            #bcstd_bestsol = mean(bcstd_bestsol,na.rm=TRUE),
            bcstd_opt = sum(bcstd_opt),
            bcstd_time = mean(bcstd_solvedtime,na.rm=TRUE),
            bcstd_nodes = mean(bcstd_solvednodes,na.rm=TRUE),
            bcstd_gap = mean(bcstd_unsolvedgap,na.rm=TRUE),
            #bc3level_bestsol = mean(bc3level_bestsol,na.rm=TRUE),
            bc3level_opt = sum(bc3level_opt),
            bc3level_time = mean(bc3level_solvedtime,na.rm=TRUE),
            bc3level_nodes = mean(bc3level_solvednodes,na.rm=TRUE),
            bc3level_gap = mean(bc3level_unsolvedgap,na.rm=TRUE)
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
output[nrow(output),]$bcstd_time <- mean(output$bcstd_time,na.rm = TRUE)
output[nrow(output),]$bc3level_time <- mean(output$bc3level_time,na.rm = TRUE)
output[nrow(output),]$std_time <- mean(output$std_time,na.rm = TRUE)
output[nrow(output),]$threelevel_time <- mean(output$threelevel_time,na.rm = TRUE)
output[nrow(output),]$bcstd_gap <- mean(output$bcstd_gap,na.rm = TRUE)
output[nrow(output),]$std_gap <- mean(output$std_gap,na.rm = TRUE)
output[nrow(output),]$bc3level_gap <- mean(output$bc3level_gap,na.rm = TRUE)
output[nrow(output),]$threelevel_gap <- mean(output$threelevel_gap,na.rm = TRUE)
output[nrow(output),]$bcstd_nodes <- mean(output$bcstd_nodes,na.rm = TRUE)
output[nrow(output),]$bc3level_nodes <- mean(output$bc3level_nodes,na.rm = TRUE)
output[nrow(output),]$std_nodes <- mean(output$std_nodes,na.rm = TRUE)
output[nrow(output),]$threelevel_nodes <- mean(output$threelevel_nodes,na.rm = TRUE)


#Adding total line
output[nrow(output)+1,] <- NA
output[nrow(output),]$instancegroup <- "Total"
output[nrow(output),]$std_opt <- sum(output$std_opt,na.rm = TRUE)
output[nrow(output),]$bcstd_opt <- sum(output$bcstd_opt,na.rm = TRUE)
output[nrow(output),]$bc3level_opt <- sum(output$bc3level_opt,na.rm = TRUE)
output[nrow(output),]$threelevel_opt <- sum(output$threelevel_opt,na.rm = TRUE)



#write.csv(output,"random.csv", row.names = FALSE)

#print(xtable(output,digits=c(0,0,1,0,1,1,2,1,0,1,1,2,1,0,1,1,2,1,0,1,1,2)),include.rownames = FALSE) #with bestsol
print(xtable(output,digits=c(0,0,0,1,1,2,0,1,1,2,0,1,1,2,0,1,1,2)),include.rownames = FALSE) #without bestsol


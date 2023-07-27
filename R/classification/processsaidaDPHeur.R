library(dplyr)
library(readr)
library(stringr)

saidaparalearningdpheur <- read_delim("~/Documents/github/threelevelsupply/saidalearning.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)


saida <- saidaparalearningdpheur[ , -which(names(saidaparalearningdpheur) %in% c("elapsedtime","dptype","dpseed","iter","typeF","typeD"))]

saida <- saida[complete.cases(saida), ]

saida$config <- paste("c",saida$dpalpha,saida$dpredperiods,saida$dpcostreductionware,saida$dpcostreductionret, sep = "_")


saida




output <- saida %>% 
  group_by(inst,NR,NT,NW,capacity,balanced) %>% 
    summarize(minbestconf = config[which.min(bestcost)],
              minaverageconf = config[which.min(averagecost)]
          
  )


output$typeF <- ""
output$typeD <- ""


for (i in 1:nrow(output)){
  
  if (str_detect(output[i,]$inst,"SF")){
    output[i,]$typeF <- "SF"
  }else{
    output[i,]$typeF <- "DF"
  }
  if (str_detect(output[i,]$inst,"SD")){
    output[i,]$typeD <- "SD"
  }else{
    output[i,]$typeD <- "DD"
  }

}


unique(saida$inst)

names(saida)


teste <- saida[1,]

teste$config <- paste("c",teste$dpalpha,teste$dpredperiods,teste$dpcostreductionware,teste$dpcostreductionret, sep = "_")

teste







#######################################
##
##  process saida Relax-and-fix
##
######################################



names(alloutputs)

saida <- alloutputs[ , -which(names(alloutputs) %in% c("elapsedtime","maxtimerf"))]


saida <- saida[complete.cases(saida), ]

saida$config <- paste("c",saida$fixsizerf,saida$freeintervalrf, sep = "_")


saida

`%notin%` <- Negate(`%in%`)
saida <- subset(saida,saida$config %notin% c("c_2_2","c_2_3","c_3_2","c_2_4","c_3_3","c_3_4"))

saida




output <- saida %>% 
  group_by(inst,NR,NT,NW,capacity,balanced) %>% 
  summarize(minbestconf = config[which.min(bestsol)],
            minbestcost = min(bestsol)
            
  )


output$typeF <- ""
output$typeD <- ""


for (i in 1:nrow(output)){
  
  if (str_detect(output[i,]$inst,"SF")){
    output[i,]$typeF <- "SF"
  }else{
    output[i,]$typeF <- "DF"
  }
  if (str_detect(output[i,]$inst,"SD")){
    output[i,]$typeD <- "SD"
  }else{
    output[i,]$typeD <- "DD"
  }
  
}

output




bestconfigs <- output %>% 
  group_by(minbestconf) %>% 
  summarize(count = n(),
            perc = 100*n()/nrow(output)
            )

bestconfigs$average = 0.0
for (i in 1:nrow(bestconfigs)){
  saidaaux <- subset(saida,saida$config == bestconfigs[i,]$minbestconf)
  bestconfigs[i,]$average <- mean(saidaaux$bestsol)
}





unique(output$minbestconf)










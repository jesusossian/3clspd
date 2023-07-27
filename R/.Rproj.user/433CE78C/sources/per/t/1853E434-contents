library(tidyverse)
library(stringr)
library(dplyr)
library(xtable)
library(readr)
library(psych)


T <- 15




N<-50
insttype <- 'bal'
#Small
saidamc <- read_delim(paste("barrier/uncap_",insttype,"_mc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)
saidaranddpheurmc <- read_delim(paste("barrier/uncap_",insttype,"_randdpheurmc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE)

insttype <- 'unbal'
#Small
saidamc <- rbind(saidamc,read_delim(paste("barrier/uncap_",insttype,"_mc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
saidaranddpheurmc <- rbind(saidaranddpheurmc,read_delim(paste("barrier/uncap_",insttype,"_randdpheurmc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))


for (insttype in c('bal','unbal')){
  for(N in list(100,200)){
    saidamc <- rbind(saidamc,read_delim(paste("barrier/uncap_",insttype,"_mc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
    saidaranddpheurmc <- rbind(saidaranddpheurmc,read_delim(paste("barrier/uncap_",insttype,"_randdpheurmc_N",N,"_T",T,"_result.csv",sep=""), ";", escape_double = FALSE, trim_ws = TRUE))
  }
}

#saida randdpheur columns: bestcost; averagecost; elapsedtime; dptype; dpalpha

saidaranddpheurmc$disablesolver <- NA


##Clean up data 
# remove data in wrong output csv
saidamc <- subset(saidamc,form=="mc")
saidaranddpheurmc <- subset(saidaranddpheurmc,form=="randdpheurmc")
#saidaranddpheur <- subset(saidaranddpheur,form=="randdpheur")
#remove repeated executions for same instance
#dflist <- list(saidamc,saidaranddpheur,saidaranddpheurmc)
#for (df in dflist){
#  df <- distinct(df,instance, .keep_all = TRUE)
#}




for (i in 1:nrow(saidamc)){
  if (saidamc[i,]$bestbound < 0){
    saidamc[i,]$bestbound <- 0
  }
  if (saidaranddpheurmc[i,]$bestbound < 0){
    saidaranddpheurmc[i,]$bestbound <- 0
  }
}

saidamc$opt <- 0
saidaranddpheurmc$opt <- 0
for (i in 1:nrow(saidamc)){
  saidamc[i,]$opt <- ifelse( 100*(saidamc[i,]$bestsol - saidamc[i,]$bestbound)/saidamc[i,]$bestsol < 0.001,1,0)
  saidaranddpheurmc[i,]$opt <- ifelse( 100*(saidaranddpheurmc[i,]$bestsol - saidaranddpheurmc[i,]$bestbound)/saidaranddpheurmc[i,]$bestsol < 0.001,1,0)
}




#  if (str_detect(testeRand[i,]$instance,"N50")){
#    testeRand[i,]$NR <- 50



mat <- matrix(0, ncol = 4, nrow = 2)
rownames(mat) <- c("MC","DPH-pMC")
colnames(mat) <- c("|R|=50","|R|=100","|R|=200","ALL")

mattime <- matrix(0, ncol = 4, nrow = 2)
rownames(mattime) <- c("MC","DPH-pMC")
colnames(mattime) <- c("|R|=50","|R|=100","|R|=200","ALL")

#Plot percentage of solved instances

types <- c("|R|=50","|R|=100","|R|=200","ALL")



opt.mc.N50 <- 0
opt.mc.N100 <- 0
opt.mc.N200 <- 0
time.mc.N50 <- 0
time.mc.N100 <- 0
time.mc.N200 <- 0
opt.randdpheurmc.N50 <- 0
opt.randdpheurmc.N100 <- 0
opt.randdpheurmc.N200 <- 0
time.randdpheurmc.N50 <- 0
time.randdpheurmc.N100 <- 0
time.randdpheurmc.N200 <- 0




nrows.N50 <- 0
nrows.N100 <- 0
nrows.N200 <- 0




for (i in 1:nrow(saidamc) ){
  
  if (str_detect(saidamc[i,]$instance,"N50")){
    opt.mc.N50 <- opt.mc.N50 + saidamc[i,]$opt
    time.mc.N50 <- time.mc.N50 + saidamc[i,]$time
    opt.randdpheurmc.N50 <- opt.randdpheurmc.N50 + saidaranddpheurmc[i,]$opt
    time.randdpheurmc.N50 <- time.randdpheurmc.N50 + saidaranddpheurmc[i,]$time
    
    nrows.N50 <- nrows.N50 + 1
  }else{
    if(str_detect(saidamc[i,]$instance,"N100")){
      opt.mc.N100 <- opt.mc.N100 + saidamc[i,]$opt
      time.mc.N100 <- time.mc.N100 + saidamc[i,]$time
      opt.randdpheurmc.N100 <- opt.randdpheurmc.N100 + saidaranddpheurmc[i,]$opt
      time.randdpheurmc.N100 <- time.randdpheurmc.N100 + saidaranddpheurmc[i,]$time

      nrows.N100 <- nrows.N100 + 1
    }else{
      if(str_detect(saidamc[i,]$instance,"N200")){
        opt.mc.N200 <- opt.mc.N200 + saidamc[i,]$opt
        time.mc.N200 <- time.mc.N200 + saidamc[i,]$time
        opt.randdpheurmc.N200 <- opt.randdpheurmc.N200 + saidaranddpheurmc[i,]$opt
        time.randdpheurmc.N200 <- time.randdpheurmc.N200 + saidaranddpheurmc[i,]$time

        nrows.N200 <- nrows.N200 + 1
      }
    }
  }
  
  
}



#mc
perc <- 100 * opt.mc.N50/nrows.N50
mat[1,1] <- perc

perc <- 100 * opt.mc.N100/nrows.N100
mat[1,2] <- perc

perc <- 100 * opt.mc.N200/nrows.N200
mat[1,3] <- perc

perc <- 100 * (opt.mc.N50+opt.mc.N100+opt.mc.N200)/(nrows.N50+nrows.N100+nrows.N200)
mat[1,4] <- perc

#randdpheurmc
perc <- 100 * opt.randdpheurmc.N50/nrows.N50
mat[2,1] <- perc

perc <- 100 * opt.randdpheurmc.N100/nrows.N100
mat[2,2] <- perc

perc <- 100 * opt.randdpheurmc.N200/nrows.N200
mat[2,3] <- perc

perc <- 100 * (opt.randdpheurmc.N50+opt.randdpheurmc.N100+opt.randdpheurmc.N200)/(nrows.N50+nrows.N100+nrows.N200)
mat[2,4] <- perc



#time
#mc
avgtime <- time.mc.N50/nrows.N50
mattime[1,1] <- avgtime

avgtime <-  time.mc.N100/nrows.N100
mattime[1,2] <- avgtime

avgtime <-  time.mc.N200/nrows.N200
mattime[1,3] <- avgtime

avgtime <-  (time.mc.N50+time.mc.N100+time.mc.N200)/(nrows.N50+nrows.N100+nrows.N200)
mattime[1,4] <- avgtime

#randdpheurmc
avgtime <-  time.randdpheurmc.N50/nrows.N50
mattime[2,1] <- avgtime

avgtime <-  time.randdpheurmc.N100/nrows.N100
mattime[2,2] <- avgtime

avgtime <-  time.randdpheurmc.N200/nrows.N200
mattime[2,3] <- avgtime

avgtime <-  (time.randdpheurmc.N50+time.randdpheurmc.N100+time.randdpheurmc.N200)/(nrows.N50+nrows.N100+nrows.N200)
mattime[2,4] <- avgtime




size <-T
pdfname <- paste("optimal_mc_",size,".pdf",sep="")

pdf(pdfname)

# plotting settings -------------------------------------------------------
ylim <- c(-1,101)
angle1 <- c(0,36)
density1 <- c(30,100)
col <- 1 # rainbow(7)
#col <- c("tan", "orange1", "magenta", "cyan", "red")

# plot --------------------------------------------------------------------
#opar = par(oma = c(0,0,0,4)) # Large right margin for plot
#opar = par(oma = c(3,3,1,4)) 
opar = par(oma = c(0,0,2,0)) 
barplot(mat, beside=TRUE, ylim=ylim,ylab="fraction in percent solved to optimality", col=col, angle=angle1, density=density1)

if (FALSE){
  for(i in 1:length(as.vector(mat))){
    if(i<=4){
      text(i+0.6, as.vector(mat)[i]+0.3, srt=90, pos=3, cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else if (i>=5 & i<=8){
      text(i+1.6, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else if (i>=9 & i<=12){
      text(i+2.5, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else if (i>=13 & i<=16){
      text(i+3.5, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } 
  }
}

par(opar) # Reset par
par(bg="transparent")
#legend("topright", legend=rownames(mat), ncol=1, fill=TRUE, col=col, angle=angle1, density=density1,cex=0.75)
legend("top", legend=rownames(mat), ncol=2, fill=TRUE, col=col, angle=angle1, density=density1,cex=0.75)
opar = par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)


dev.off()

par(opar)



size <-T
pdfname <- paste("cputime_mc_",size,".pdf",sep="")

mat<- mattime

pdf(pdfname)

# plotting settings -------------------------------------------------------
if(size==15){
  ylim <- c(-1,20)
}else{
  ylim <- c(-1,1200)
}

angle1 <- c(0,36)
density1 <- c(30,100)
col <- 1 # rainbow(7)
#col <- c("tan", "orange1", "magenta", "cyan", "red")

# plot --------------------------------------------------------------------
#opar = par(oma = c(0,0,0,4)) # Large right margin for plot
#opar = par(oma = c(3,3,1,4)) 
opar = par(oma = c(0,0,2,0)) 
barplot(mat, beside=TRUE, ylim=ylim,ylab="average time in seconds", col=col, angle=angle1, density=density1)

if (FALSE){
  for(i in 1:length(as.vector(mat))){
    if(i<=5){
      text(i+0.6, as.vector(mat)[i]+0.3, srt=90, pos=3, cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else if (i>=6 & i<=10){
      text(i+1.6, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else if (i>=11 & i<=15){
      text(i+2.5, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else if (i>=16 & i<=20){
      text(i+3.5, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else if (i>=21 & i<=25){
      text(i+4.5, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    } else {
      text(i+5.5, as.vector(mat)[i]+0.3, srt=90, pos=3,cex=0.5, labels= format(round(as.vector(mat)[i], 1), nsmall = 1))
    }
  }
}

par(opar) # Reset par
par(bg="transparent")
#legend("topright", legend=rownames(mat), ncol=1, fill=TRUE, col=col, angle=angle1, density=density1,cex=0.75)
legend("top", legend=rownames(mat), ncol=2, fill=TRUE, col=col, angle=angle1, density=density1,cex=0.75)
opar = par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)


dev.off()

par(opar)




library(reshape2)
library(ggplot2)

#-----------------------------------------------------------------------------------#
#####################################################
# (1) SET VALUES (alpha, power, "power of interest")
#####################################################
alpha <- 0.05 # define alpha level
power <- c(0.21, 0.75, 0.8, 0.95, 0.99) # define levels of power (change or add values)

# The plot features a dashed line indicating the proportion of true hypotheses for which
# PPV=NPV at a given level of power. You can change this "power of interest" 
# by changing the poi value. The value must be among the power levels defined above!
poi <- 0.75  # set power of interest
#-----------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------#
#############################################
# (2) Do the maths: Run code down to line 87 
# (I know it's clumsy and ugly, I apologise)
#############################################
### create continuous vector for proportion of true hypotheses
H1rate <- c((1:99)/100) 

### create matrices for PPV, NPV, FDR, and FOR to fill with values
PPVmat <- matrix(data=NA, nrow=99, ncol=length(power)+1)
NPVmat <- matrix(data=NA, nrow=99, ncol=length(power)+1)
FDRmat <- matrix(data=NA, nrow=99, ncol=length(power)+1)
FORmat <- matrix(data=NA, nrow=99, ncol=length(power)+1)

colnames(PPVmat) <- c("H1rate", power)  #rename columns of mat
colnames(NPVmat) <- c("H1rate", power)  #rename columns of mat
colnames(FDRmat) <- c("H1rate", power)  #rename columns of mat
colnames(FORmat) <- c("H1rate", power)  #rename columns of mat

PPVmat[,1] <- H1rate
NPVmat[,1] <- H1rate
FDRmat[,1] <- H1rate
FORmat[,1] <- H1rate

### for loop to calculate PPV, NPV, FDR, and FOR for different power levels and fill them into matrices
for(i in 1:length(power)){ 
  for(j in 1:length(H1rate)) {
    truepos <- H1rate[j] * (power[i]) # true positives: proportion of true hypotheses * power
    falseneg <- H1rate[j] * (1-power)[i] # false negatives: proportion of true hypotheses * (1-power)
    falsepos <- (1-H1rate[j]) * alpha # false positives: proportion of false hypotheses * alpha
    trueneg <- (1-H1rate[j]) * (1-alpha) # true negatives: proportion of true hypotheses * (1-alpha)
    
    PPV <- truepos / (truepos + falsepos) #positive predictive value
    FDR <- falsepos / (truepos + falsepos) #false discovery rate
    FOR <- falseneg / (trueneg + falseneg) #false omission rate
    NPV <- trueneg / (trueneg + falseneg) #negative predictive value 
    
    PPVmat[j,(i+1)] <- PPV # fill PPV values into PPVmat
    NPVmat[j,(i+1)] <- NPV # fill NPV values into NPVmat
    FDRmat[j,(i+1)] <- FDR # fill FDR values into FDRmat
    FORmat[j,(i+1)] <- FOR # fill FOR values into FORmat
  }
}

### Turn the four matrices into data frames
PPVdatmat <- as.data.frame(PPVmat)
NPVdatmat <- as.data.frame(NPVmat)
FDRdatmat <- as.data.frame(FDRmat)
FORdatmat <- as.data.frame(FORmat)

### Turn the four data frames into long format
PPVmelt <- melt(PPVdatmat, id.vars="H1rate", variable.name="power", value.name = "PPV")
NPVmelt <- melt(NPVdatmat, id.vars="H1rate", variable.name="power", value.name = "NPV")
FDRmelt <- melt(FDRdatmat, id.vars="H1rate", variable.name="power", value.name = "FDR")
FORmelt <- melt(FORdatmat, id.vars="H1rate", variable.name="power", value.name = "FOR")

### put all four indices into one matrix & melt it for plotting
allmat <- cbind(PPVmelt, NPVmelt$NPV, FDRmelt$FDR, FORmelt$FOR)
colnames(allmat) <- c("H1rate", "power", "PPV", "NPV", "FDR", "FOR")
meltmat <- melt(allmat, id=c("H1rate","power"), variable.name="index")

### To only plot PPV and NPV together, I merged them into one data frame
PVmat <- cbind(PPVmelt, NPVmelt$NPV)
colnames(PVmat) <- c("H1rate", "power", "PPV", "NPV")
PVmat <- melt(PVmat, id=c("H1rate", "power"), variable.name="index")

### calculate the proportion of true hypotheses for which PPV=NPV at power of interest
PVrate <- allmat[which.min(abs(allmat[allmat[,"power"]==poi, "PPV"]-allmat[allmat[,"power"]==poi, "NPV"])), "H1rate"]
#-----------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------#
#####################################################################################
# (3) PLOTTING
# I only included a plot with PPV and NPV combined ("PVplot") and one with all four
# indices ("allplot"), but the code above sets up single dataframes for each index, 
# allowing you to plot them individually if you want.
#####################################################################################
cbPalette <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#F0E442", "#0072B2", "#999999")

### Plot PPV and NPV together
PVplot <- ggplot(PVmat, aes(x=PVmat$H1rate, y=PVmat$value, colour=PVmat$power, linetype=PVmat$index))  +  
  geom_line() +
  geom_hline(aes(yintercept=0.5), colour="#999999") +
  geom_vline(aes(xintercept=0.07), colour="#999999", linetype="dashed") +
  geom_vline(aes(xintercept=PVrate), colour="#999999", linetype="dashed") +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(face="bold")) +
  ggtitle(paste("PPV and NPV for alpha =", alpha)) +
  scale_y_continuous(name="value") +
  scale_x_continuous(name="proportion of true hypotheses tested") +
  scale_linetype_discrete(name="index") +
  scale_colour_discrete(name="power") 
# If you prefer a colourblind-friendly palette, replace the last argument with
# scale_colour_manual(name="power", values=cbPalette) 
PVplot


### plot with all indices (PPV, NPV, FDR, and FOR)
allplot <- ggplot(meltmat, aes(x=meltmat$H1rate, y=meltmat$value, colour=meltmat$power, linetype=meltmat$index))  +  
  geom_line() +
  geom_hline(aes(yintercept=0.5), colour="#999999") +
  geom_vline(aes(xintercept=0.07), colour="#999999", linetype="dashed") +
  geom_vline(aes(xintercept=PVrate), colour="#999999", linetype="dashed") +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(face="bold")) +
  ggtitle(paste("PPV, NPV, FDR, and FOR for alpha =", alpha)) +
  scale_y_continuous(name="value or rate") +
  scale_x_continuous(name="proportion of true hypotheses tested") +
  scale_colour_discrete(name="power")   
# If you prefer a colourblind-friendly palette, replace the last argument with
# scale_colour_manual(name="power", values=cbPalette) 
allplot
#-----------------------------------------------------------------------------------#

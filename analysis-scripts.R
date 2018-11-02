# H5 reader
library("rhdf5")
rtxiH5reader <- function(file,return.trial.num=1){
    # Determine the number of trials and sample dt in ms
    h5labels <- h5ls(file)
    num.trials <- length(which(h5labels$name=="Period (ns)"))
    if (return.trial.num>num.trials){
        print("Check input file and trial number.")
        return
    }
    dt.ndx <- which(h5labels$name=="Period (ns)")
    dt.label <- paste(h5labels$group[dt.ndx[return.trial.num]],h5labels$name[dt.ndx[return.trial.num]],sep="/")
    # dt in ms
    dt <- h5read(file=file,dt.label)*1e-6

    # Get number of data vectors from specified trial
    l1 <- paste("/Trial",return.trial.num,"/","Synchronous Data",sep="")
    numDataVectors <- length(which(h5labels$group==l1))-1
    name.ndx <-  which(h5labels$group==l1)
    name.ndx <- name.ndx[1:length(name.ndx)-1]
    data.names <- gsub(" ","",h5labels$name[name.ndx])
    data.names <- gsub('[0-9]',"",data.names)
    # data array index
    dataNDX <- which(h5labels$dclass=="ARRAY")
    rawVName <- paste(h5labels$group[dataNDX[return.trial.num]],"/",h5labels$name[dataNDX[return.trial.num]],sep="")
    raw.vec <- h5read(file=file,name=rawVName)

    # Check data consistency
    if (length(raw.vec)%%length(data.names)!=0){
        print("Data vector and labels not correctly parsed.")
        return
    }

    ndx.vector <- rep(1:length(data.names),length(raw.vec)/length(data.names))
    dl <- NULL
    dl <- as.list(dl)
    for (i in 1:length(data.names)){
        dl[[i]] <- raw.vec[which(ndx.vector==i)]
    }

    # Change to data frame and add labels
    dl <- as.data.frame(dl)
    # Create time vector
    t.ms <- dt*(0:(length(dl[[1]])-1))
    
    for (i in 1:length(data.names)){
        colnames(dl)[i] <- data.names[i]
    }

    dataset <- cbind(t.ms,dl)
    return(dataset)
}


# Generic plot fucntion
jgpplotter <- function(x,y,mar=c(5.1,5.1,5.1,2.1),lwdpt=0.75,...){
    # convert pt to lwd
    lwd <- lwdpt*(1/0.75)
    par(mar=mar,lwd=lwd)
    plot(x,y,bty="n",...)
}


# Intervals and Beats
# Function takes in an oscillating signal vector as an argument and returns 

apfind <- function(x,minlength=2,dt=0.1){
    # dt defines sampling interval between points: default is 0.1 ms
    # minlength defines the minimum length of AP signal above the mean potential: default is 2 ms
    t.threshold <- round(minlength/dt)
    # AP peaks
    signalNDX <- which(x>mean(x))
    apNUM <- NULL
    first.ndx <- NULL
    last.ndx <- NULL
    
    if (length(signalNDX)==0){
        print("No APs found. Check input.")
        return
    }

    tmp <- 1
    apCount <- 1
    for(i in 2:length(signalNDX)){

        if (signalNDX[i-1]==signalNDX[i]-1 && tmp < t.threshold) {
            # continuous and minimum threshold not met
            tmp <- tmp+1
         } else if (signalNDX[i-1]!=signalNDX[i]-1 && tmp < t.threshold) {
            # discontinuous and minimum threshold not met
            tmp <- 1
        } else if (signalNDX[i-1]==signalNDX[i]-1 && tmp == t.threshold) {
            # continuous and minimum threshold met
            apNUM[apCount] <- apCount
            first.ndx[apCount] <- signalNDX[i-tmp]
            tmp <- tmp+1
            # check to see if last AP
            if (i==length(signalNDX)){
                last.ndx[apCount] <- signalNDX[i]
            }            
        } else if (signalNDX[i-1]!=signalNDX[i]-1 && tmp > t.threshold) {
            # End of AP
            last.ndx[apCount] <- signalNDX[i-1]
            apCount <- apCount+1
            tmp <- 1
        } else if (signalNDX[i-1]==signalNDX[i]-1 && tmp > t.threshold) {
            # continuous and minimum threshold exceeded
            tmp <- tmp+1
            # check to see if last AP
            if (i==length(signalNDX)){
                last.ndx[apCount] <- signalNDX[i]
            }            
        } else {
            # End of recording: AP cut off
            apNUM <- apNUM[1:length(apNUM)-1]
            first.ndx <- first.ndx[1:length(first.ndx)-1]
        }

    }
    # Check identified APs
    if (length(apNUM)==length(first.ndx) && length(apNUM)==length(last.ndx)){
        retdf <- as.data.frame(cbind(apNUM,first.ndx,last.ndx))
        return(retdf)
    }
}

apview <- function(t,y,ndxDF,interactive=FALSE,...){
    # Plots an AP train with identified APs form apfind
    # interactive mode prompts the user to exlude misidentified APs
    jgpplotter(x=t,y=y,...)
    num.of.APs <- nrow(ndxDF)
    peakV <- rep(NA,num.of.APs) 
    peakV.ndx <- rep(NA,num.of.APs)
    
    for (i in 1:num.of.APs){
        # Calculates peakV
        peakV[i] <- max(y[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]])
        peakV.ndx[i] <-min(which(y[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]]==peakV[i]))+ndxDF$first.ndx[i]-1
        # outlines APs default lw is 1pt
        lines(t[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]],y[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]],col="red",lwd=1.333333)
        # label AP
        mtext(text=i,at=t[peakV.ndx[i]],col="red")
    }

    if(interactive){
        print("List the AP number(s) separated by commas.")
        n <- readline(prompt="AP number to exclude:")
        n <- as.integer(strsplit(n[[1]],split=",")[[1]])
        new.ndxDF <- ndxDF[-n,]
        new.nAPs <- 1:nrow(new.ndxDF)
        new.ndxDF$apNUM <- new.nAPs
        dev.off()
        return(new.ndxDF)
    }
}

apVoltages <- function(x,ndxDF,dt=0.1){
    # Calculates the voltages in an AP train 
    # dt: period between samples and defaults to 0.1 ms
    # ndxDF: a dataframe contraining indices from apfind 

    num.of.APs <- nrow(ndxDF)
    
    # Create vectors for AP properties    
    bri <- rep(NA,num.of.APs)
    mdp <- rep(NA,num.of.APs) 
    mdp.ndx <- rep(NA,num.of.APs) 
    peakV <- rep(NA,num.of.APs) 
    peakV.ndx <- rep(NA,num.of.APs)

    for (i in 1:num.of.APs){
        # Calculates peakV
        peakV[i] <- max(x[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]])
        peakV.ndx[i] <-min(which(x[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]]==peakV[i]))+ndxDF$first.ndx[i]
        # Calculates the minium between the first AP and the following AP
        if (i!=num.of.APs){
            mdp[i] <- min(x[ndxDF$first.ndx[i]:ndxDF$first.ndx[i+1]])
            mdp.ndx[i] <- min(which(x[ndxDF$first.ndx[i]:ndxDF$first.ndx[i+1]]==mdp[i]))+ndxDF$first.ndx[i]
        } else { # Calculates the minium between the last AP and end of the AP train
            mdp[i] <- min(x[ndxDF$first.ndx[i]:length(x)])
            mdp.ndx[i] <- min(which(x[ndxDF$first.ndx[i]:length(x)]==mdp[i]))+ndxDF$first.ndx[i]
        }

 
        # Calculates the bri. Last AP is stored as NA
        if (i!=1){
            bri[i-1] <- (peakV.ndx[i]-peakV.ndx[i-1])*dt
        }

    }
        
        rtndf <- as.data.frame(cbind(bri,mdp,mdp.ndx,peakV,peakV.ndx))
        return(rtndf)

}


apDurations <- function(x,apVoltages,dt=0.1,prePV.time=20){
    # x: assumed to be a single dimensional vector of voltage
    # dt: period between samples and defaults to 0.1 ms
    # apVoltages: a dataframe indices from apVoltages

    num.of.APs <- nrow(apVoltages)
    prePV <- rep(NA,num.of.APs)
    first.ndx <- rep(NA,num.of.APs)
    last.ndx <- rep(NA,num.of.APs)
    amp <- rep(NA,num.of.APs)

    ap10 <- rep(NA,num.of.APs)
    ap20 <- rep(NA,num.of.APs)
    ap30 <- rep(NA,num.of.APs)
    ap40 <- rep(NA,num.of.APs)
    ap50 <- rep(NA,num.of.APs)
    ap60 <- rep(NA,num.of.APs)
    ap70 <- rep(NA,num.of.APs)
    ap80 <- rep(NA,num.of.APs)
    ap90 <- rep(NA,num.of.APs)

    apd10.firstndx <- rep(NA,num.of.APs)
    apd20.firstndx <- rep(NA,num.of.APs)
    apd30.firstndx <- rep(NA,num.of.APs)
    apd40.firstndx <- rep(NA,num.of.APs)
    apd50.firstndx <- rep(NA,num.of.APs)
    apd60.firstndx <- rep(NA,num.of.APs)
    apd70.firstndx <- rep(NA,num.of.APs)
    apd80.firstndx <- rep(NA,num.of.APs)
    apd90.firstndx <- rep(NA,num.of.APs)

    apd10.lastndx <- rep(NA,num.of.APs)
    apd20.lastndx <- rep(NA,num.of.APs)
    apd30.lastndx <- rep(NA,num.of.APs)
    apd40.lastndx <- rep(NA,num.of.APs)
    apd50.lastndx <- rep(NA,num.of.APs)
    apd60.lastndx <- rep(NA,num.of.APs)
    apd70.lastndx <- rep(NA,num.of.APs)
    apd80.lastndx <- rep(NA,num.of.APs)
    apd90.lastndx <- rep(NA,num.of.APs)

    apd10 <- rep(NA,num.of.APs)
    apd20 <- rep(NA,num.of.APs)    
    apd30 <- rep(NA,num.of.APs)
    apd40 <- rep(NA,num.of.APs)
    apd50 <- rep(NA,num.of.APs)
    apd60 <- rep(NA,num.of.APs)
    apd70 <- rep(NA,num.of.APs)
    apd80 <- rep(NA,num.of.APs)    
    apd90 <- rep(NA,num.of.APs)
    
    # Calculates the minimal AP window
    minbri <- min(apVoltages$bri[!is.na(apVoltages$bri)])
    apwindow <- as.integer((0.25*minbri)/dt) # dt is in ms/indices
    first.ndx <- apVoltages$peakV.ndx-rep(apwindow,num.of.APs)
    if (length(which(first.ndx<=0))!=0){
        first.ndx[which(first.ndx<=0)] <- rep(1,length(which(first.ndx<=0)))
    }
    last.ndx <- apVoltages$peakV.ndx+rep(apwindow,num.of.APs)
    if (length(which(last.ndx>length(x)))!=0){
        last.ndx[which(last.ndx>length(x))] <- rep(length(x),length(which(last.ndx>length(x))))
    }


        # Calculates the prePV    
        # Calculates APDs
    for (i in 1:num.of.APs){
        prePV[i] <- mean(x[first.ndx[i]:first.ndx[i]+as.integer(prePV.time/dt)])
        amp[i] <- apVoltages$peakV[i]-prePV[i]
        ap10[i] <- prePV[i]+0.9*amp[i]
        ap20[i] <- prePV[i]+0.8*amp[i]
        ap30[i] <- prePV[i]+0.7*amp[i]
        ap40[i] <- prePV[i]+0.6*amp[i]
        ap50[i] <- prePV[i]+0.5*amp[i]
        ap60[i] <- prePV[i]+0.4*amp[i]
        ap70[i] <- prePV[i]+0.3*amp[i]
        ap80[i] <- prePV[i]+0.2*amp[i]
        ap90[i] <- prePV[i]+0.1*amp[i]

        apd10.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap10[i]))+first.ndx[i]
        apd20.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap20[i]))+first.ndx[i]
        apd30.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap30[i]))+first.ndx[i]
        apd40.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap40[i]))+first.ndx[i]
        apd50.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap50[i]))+first.ndx[i]
        apd60.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap60[i]))+first.ndx[i]
        apd70.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap70[i]))+first.ndx[i]
        apd80.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap80[i]))+first.ndx[i]
        apd90.firstndx[i] <- which.min(abs(x[first.ndx[i]:apVoltages$peakV.ndx[i]]-ap90[i]))+first.ndx[i]

        apd10.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap10[i]))+apVoltages$peakV.ndx[i]
        apd20.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap20[i]))+apVoltages$peakV.ndx[i]
        apd30.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap30[i]))+apVoltages$peakV.ndx[i]
        apd40.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap40[i]))+apVoltages$peakV.ndx[i]
        apd50.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap50[i]))+apVoltages$peakV.ndx[i]
        apd60.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap60[i]))+apVoltages$peakV.ndx[i]
        apd70.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap70[i]))+apVoltages$peakV.ndx[i]
        apd80.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap80[i]))+apVoltages$peakV.ndx[i]
        apd90.lastndx[i] <- which.min(abs(x[apVoltages$peakV.ndx[i]:last.ndx[i]]-ap90[i]))+apVoltages$peakV.ndx[i]

        apd10[i] <- (apd10.lastndx[i] - apd10.firstndx[i])*dt
        apd20[i] <- (apd20.lastndx[i] - apd20.firstndx[i])*dt
        apd30[i] <- (apd30.lastndx[i] - apd30.firstndx[i])*dt
        apd40[i] <- (apd40.lastndx[i] - apd40.firstndx[i])*dt
        apd50[i] <- (apd50.lastndx[i] - apd50.firstndx[i])*dt
        apd60[i] <- (apd60.lastndx[i] - apd60.firstndx[i])*dt
        apd70[i] <- (apd70.lastndx[i] - apd70.firstndx[i])*dt
        apd80[i] <- (apd80.lastndx[i] - apd80.firstndx[i])*dt
        apd90[i] <- (apd90.lastndx[i] - apd90.firstndx[i])*dt
        
    }

    rtndf <- as.data.frame(cbind(prePV,amp,apd10,apd20,apd30,apd40,apd50,apd60,apd70,apd80,apd90,
                                 ap10,ap20,ap30,ap40,ap50,ap60,ap70,ap80,ap90,apd10,apd20,apd30,apd40,
                                 apd50,apd60,apd70,apd80,apd90,
                                 apd10.firstndx,apd20.firstndx,apd30.firstndx,
                                 apd40.firstndx,apd50.firstndx,apd60.firstndx,
                                 apd70.firstndx,apd80.firstndx,apd90.firstndx,
                                 apd10.lastndx,apd20.lastndx,apd30.lastndx,
                                 apd40.lastndx,apd50.lastndx,apd60.lastndx,
                                 apd70.lastndx,apd80.lastndx,apd90.lastndx))
    return(rtndf)
    
}



apfind.variantDT <- function(t.ms,y,minlength=2){
    # minlength defines the minimum length of AP signal above the mean potential: default is 2 ms
    # AP peaks
    if (length(y)!=length(t.ms)){
        print("Voltage and Time vectors do not have the same length.")
        return
    }
    signalNDX <- which(y>mean(y))
    apNUM <- NULL
    first.ndx <- NULL
    last.ndx <- NULL
    
    if (length(signalNDX)==0){
        print("No APs found. Check input.")
        return
    }

    tmp <- 1
    apCount <- 1
    apNUM[apCount] <- apCount
    for(i in 2:length(signalNDX)){
        if (signalNDX[i-1]==signalNDX[i]-1) {
            # continuous
            if (tmp==1){
                first.ndx[apCount] <- signalNDX[i-1]
                tmp <- tmp+1
            } else if (i==length(signalNDX)){
                # AP cut off
                last.ndx[apCount] <- signalNDX[i]
            } else {
                tmp <- tmp+1
            }
            

         } else {
             # discontinuous
             tmp <- 1
             if (t.ms[signalNDX[i-1]]-t.ms[first.ndx[apCount]]>=minlength){
                 # previous AP meets the minimum length threshold for AP
                 last.ndx[apCount] <- signalNDX[i-1]
                 apCount <- apCount+1
                 apNUM[apCount] <- apCount
             } else {
                 # previous AP shorter than minimum length
                 first.ndx[apCount] <- signalNDX[i]
             }
         } 
    }
    # Check identified APs
    if (length(apNUM)==length(first.ndx) && length(apNUM)==length(last.ndx)){
        retdf <- as.data.frame(cbind(apNUM,first.ndx,last.ndx))
        return(retdf)
    } else {
        print("Nope.")
    }
}




#plot commands
#plot(x=t.ms/1000,y=mV*1000,type="l",bty="n",xlab="t / s",ylab="mV")
#points(t.ms[ap.properties.mV$mdp.ndx]/1000,mV[ap.properties.mV$mdp.ndx]*1000,col="red")
#points(t.ms[ap.properties.mV$peakV.ndx]/1000,mV[ap.properties.mV$peakV.ndx]*1000,col="blue")
#lines(t.ms[32597:39577]/1000,rep(-0.005512099*1000,length(t.ms[32597:39577])),col="blue")
#lines(t.ms[39577:47256]/1000,rep(-0.008479115,length(t.ms[39577:47256]))*1000,col="blue")
#lines(t.ms[47256:54795]/1000,rep(-0.007018231,length(t.ms[47256:54795]))*1000,col="blue")

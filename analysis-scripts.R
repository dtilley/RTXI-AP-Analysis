
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


apDurations <- function(x,apVoltages,dt=0.1,preThreshold.t=20){
    # x: assumed to be a single dimensional vector of voltage #
    # dt: period between samples and defaults to 0.1 ms #
    # apVoltages: a dataframe indices from apVoltages  #
    # preThreshold.t: calculates the pre-threshod voltage at time(t) before the peak voltage #
    # preThreshold.divisions: uses the preTheshold times at divisions for durations.

    num.of.APs <- nrow(apVoltages)
    preTheshold.V <- rep(NA,num.of.APs)
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
    
    # Calculates preThreshold.divisions #
    preThreshold.ndx <- apVoltages$peakV.ndx-as.integer(preThreshold.t/dt)
    if (length(which(preThreshold.ndx<0))==0){
        first.ndx <- preThreshold.ndx
        last.ndx[1:(length(last.ndx)-1)] <- first.ndx[2:length(first.ndx)]
        last.ndx[length(last.ndx)] <- (2*first.ndx[length(first.ndx)]-first.ndx[(length(first.ndx)-1)])
        if (last.ndx[length(last.ndx)]>length(x)) {
            last.ndx[length(last.ndx)] <- length(x)
        }
    } else if(length(which(preThreshold.ndx<0))==1 && which(preThreshold.ndx<0)==1) {
        preThreshold.ndx[1] <- 1
        first.ndx <- preThreshold.ndx
        last.ndx[1:(length(last.ndx)-1)] <- first.ndx[2:length(first.ndx)]
        last.ndx[length(last.ndx)] <- (2*first.ndx[length(first.ndx)]-first.ndx[(length(first.ndx)-1)])
        if (last.ndx[length(last.ndx)]>length(x)) {
            last.ndx[length(last.ndx)] <- length(x)
        }
    } else {
        print("Check ndxDF. The preTheshold.t indices are not within the bounds.")
        return()
    }
        # Calculates the preTheshold.V    
        # Calculates APDs
    for (i in 1:num.of.APs){
        preTheshold.V[i] <- mean(x[first.ndx[i]:(first.ndx[i]+as.integer(5/dt))]) # Mean V
        amp[i] <- apVoltages$peakV[i]-preTheshold.V[i]
        ap10[i] <- preTheshold.V[i]+0.9*amp[i]
        ap20[i] <- preTheshold.V[i]+0.8*amp[i]
        ap30[i] <- preTheshold.V[i]+0.7*amp[i]
        ap40[i] <- preTheshold.V[i]+0.6*amp[i]
        ap50[i] <- preTheshold.V[i]+0.5*amp[i]
        ap60[i] <- preTheshold.V[i]+0.4*amp[i]
        ap70[i] <- preTheshold.V[i]+0.3*amp[i]
        ap80[i] <- preTheshold.V[i]+0.2*amp[i]
        ap90[i] <- preTheshold.V[i]+0.1*amp[i]

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

    rtndf <- as.data.frame(cbind(preTheshold.V,amp,first.ndx,last.ndx,apd10,apd20,apd30,apd40,apd50,apd60,apd70,apd80,apd90,
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

# Generic plot fucntion with figure publication standards from the JGP
# EPS vector graphics work best for cross platform compatibility with AI and Inkscape.
jgpplotter <- function(x,y,mar=c(5.1,5.1,5.1,2.1),lwdpt=0.75,...){
    # convert pt to lwd
    lwd <- lwdpt*(1/0.75)
    par(mar=mar,lwd=lwd)
    plot(x,y,bty="n",...)
}


#plot commands
#plot(x=t.ms/1000,y=mV*1000,type="l",bty="n",xlab="t / s",ylab="mV")
#points(t.ms[ap.properties.mV$mdp.ndx]/1000,mV[ap.properties.mV$mdp.ndx]*1000,col="red")
#points(t.ms[ap.properties.mV$peakV.ndx]/1000,mV[ap.properties.mV$peakV.ndx]*1000,col="blue")
#lines(t.ms[32597:39577]/1000,rep(-0.005512099*1000,length(t.ms[32597:39577])),col="blue")
#lines(t.ms[39577:47256]/1000,rep(-0.008479115,length(t.ms[39577:47256]))*1000,col="blue")
#lines(t.ms[47256:54795]/1000,rep(-0.007018231,length(t.ms[47256:54795]))*1000,col="blue")

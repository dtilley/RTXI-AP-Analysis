
apVoltages <- function(mV,ndxDF,dt=0.1){
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
        peakV[i] <- max(mV[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]])
        peakV.ndx[i] <-min(which(mV[ndxDF$first.ndx[i]:ndxDF$last.ndx[i]]==peakV[i]))+ndxDF$first.ndx[i]-1
        # Calculates the minium between the first AP and the following AP
        if (i!=num.of.APs){
            mdp[i] <- min(mV[ndxDF$first.ndx[i]:ndxDF$first.ndx[i+1]])
            mdp.ndx[i] <- min(which(mV[ndxDF$first.ndx[i]:ndxDF$first.ndx[i+1]]==mdp[i]))+ndxDF$first.ndx[i]
        } else { # Calculates the minium between the last AP and end of the AP train
            mdp[i] <- min(mV[ndxDF$first.ndx[i]:length(mV)])
            mdp.ndx[i] <- min(which(mV[ndxDF$first.ndx[i]:length(mV)]==mdp[i]))+ndxDF$first.ndx[i]-1
        }

 
        # Calculates the bri. Last AP is stored as NA
        if (i!=1){
            bri[i-1] <- (peakV.ndx[i]-peakV.ndx[i-1])*dt
        }

    }
        
        rtndf <- as.data.frame(cbind(bri,mdp,mdp.ndx,peakV,peakV.ndx))
        return(rtndf)

}


apDurations <- function(t,mV,apVoltages,preThreshold.dt=20){
    ## t: time vector 
    ## mV: membrane voltage vector
    ## preThreshold.dt: the time prior to the peak voltage to calculate diastolic membrane voltage 
    ## apVoltages: a dataframe of indices from apVoltages  

    num.of.APs <- nrow(apVoltages)
    preThreshold.V <- rep(NA,num.of.APs)
    
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

    apd10.t1 <- rep(NA,num.of.APs)
    apd20.t1 <- rep(NA,num.of.APs)
    apd30.t1 <- rep(NA,num.of.APs)
    apd40.t1 <- rep(NA,num.of.APs)
    apd50.t1 <- rep(NA,num.of.APs)
    apd60.t1 <- rep(NA,num.of.APs)
    apd70.t1 <- rep(NA,num.of.APs)
    apd80.t1 <- rep(NA,num.of.APs)
    apd90.t1 <- rep(NA,num.of.APs)

    apd10.t2 <- rep(NA,num.of.APs)
    apd20.t2 <- rep(NA,num.of.APs)
    apd30.t2 <- rep(NA,num.of.APs)
    apd40.t2 <- rep(NA,num.of.APs)
    apd50.t2 <- rep(NA,num.of.APs)
    apd60.t2 <- rep(NA,num.of.APs)
    apd70.t2 <- rep(NA,num.of.APs)
    apd80.t2 <- rep(NA,num.of.APs)
    apd90.t2 <- rep(NA,num.of.APs)

    apd10 <- rep(NA,num.of.APs)
    apd20 <- rep(NA,num.of.APs)    
    apd30 <- rep(NA,num.of.APs)
    apd40 <- rep(NA,num.of.APs)
    apd50 <- rep(NA,num.of.APs)
    apd60 <- rep(NA,num.of.APs)
    apd70 <- rep(NA,num.of.APs)
    apd80 <- rep(NA,num.of.APs)    
    apd90 <- rep(NA,num.of.APs)
    
    ## Calculates first.ndx
    time.firstndx <- t[apVoltages$peakV.ndx]-preThreshold.dt
    for (i in 1:length(time.firstndx)){
        first.ndx[i] <- which.min(abs(t-time.firstndx[i]))
    }
    ## Calculates last.ndx
    last.ndx[1:(length(last.ndx)-1)] <- first.ndx[2:length(first.ndx)]
    last.ndx[length(last.ndx)] <- as.integer(mean(last.ndx[1:(length(last.ndx)-1)]-first.ndx[1:(length(last.ndx)-1)]))+first.ndx[length(first.ndx)]

    ## Check if first.ndx and last.ndx exsist
    if (length(which(first.ndx<=1))!=0 || length(which(first.ndx>=length(t)))!=0) {
        print("First indices are out of range.")
        return()
    } else if ((length(which(last.ndx<=1))!=0 || length(which(last.ndx>=length(t)))!=0)) {
        print("Last indices are out of range.")
        return()
    }

    ## Calculates APDs
    for (i in 1:num.of.APs){
        ## Calculates preThreshold.V over 20 indices forward from first.ndx        
        preThreshold.V[i] <- mean(mV[first.ndx[i]:(first.ndx[i]+20)])
        if (var(mV[first.ndx[i]:(first.ndx[i]+20)])>0.01*preThreshold.V[i]){
            print("Warning: High variance in preThreshold.V.")
        }
        amp[i] <- apVoltages$peakV[i]-preThreshold.V[i]
        ap10[i] <- preThreshold.V[i]+0.9*amp[i]
        ap20[i] <- preThreshold.V[i]+0.8*amp[i]
        ap30[i] <- preThreshold.V[i]+0.7*amp[i]
        ap40[i] <- preThreshold.V[i]+0.6*amp[i]
        ap50[i] <- preThreshold.V[i]+0.5*amp[i]
        ap60[i] <- preThreshold.V[i]+0.4*amp[i]
        ap70[i] <- preThreshold.V[i]+0.3*amp[i]
        ap80[i] <- preThreshold.V[i]+0.2*amp[i]
        ap90[i] <- preThreshold.V[i]+0.1*amp[i]
        
        ## Calculate apd10
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap10[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap10[i])
        apd10.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap10[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap10[i])
        apd10.t2[i] <- t2$y
        apd10[i] <- apd10.t2[i]-apd10.t1[i]

        ## Calculate apd20
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap20[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap20[i])
        apd20.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap20[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap20[i])
        apd20.t2[i] <- t2$y
        apd20[i] <- apd20.t2[i]-apd20.t1[i]

        ## Calculate apd30
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap30[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap30[i])
        apd30.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap30[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap30[i])
        apd30.t2[i] <- t2$y
        apd30[i] <- apd30.t2[i]-apd30.t1[i]

        ## Calculate apd40
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap40[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap40[i])
        apd40.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap40[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap40[i])
        apd40.t2[i] <- t2$y
        apd40[i] <- apd40.t2[i]-apd40.t1[i]

        ## Calculate apd50
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap50[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap50[i])
        apd50.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap50[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap50[i])
        apd50.t2[i] <- t2$y
        apd50[i] <- apd50.t2[i]-apd50.t1[i]

        ## Calculate apd60
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap60[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap60[i])
        apd60.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap60[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap60[i])
        apd60.t2[i] <- t2$y
        apd60[i] <- apd60.t2[i]-apd60.t1[i]

        ## Calculate apd70
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap70[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap70[i])
        apd70.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap70[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap70[i])
        apd70.t2[i] <- t2$y
        apd70[i] <- apd70.t2[i]-apd70.t1[i]

        ## Calculate apd80
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap80[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap80[i])
        apd80.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap80[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap80[i])
        apd80.t2[i] <- t2$y
        apd80[i] <- apd80.t2[i]-apd80.t1[i]

        ## Calculate apd90
        ## pre-peak points to interpolate
        p1.ndx <- min(which(mV[first.ndx[i]:apVoltages$peakV.ndx[i]]>=ap90[i]))+first.ndx[i]-1
        p2.ndx <- p1.ndx-1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t1 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap90[i])
        apd90.t1[i] <- t1$y
        ## post-peak points to interpolate
        p1.ndx <- max(which(mV[apVoltages$peakV.ndx[i]:last.ndx[i]]>=ap90[i]))+apVoltages$peakV.ndx[i]-1
        p2.ndx <- p1.ndx+1
        
        t.interpolate <- t[c(p1.ndx,p2.ndx)]
        mV.interpolate <- mV[c(p1.ndx,p2.ndx)]
        t2 <- approx(x=mV.interpolate,y=t.interpolate,xout=ap90[i])
        apd90.t2[i] <- t2$y
        apd90[i] <- apd90.t2[i]-apd90.t1[i]
                
    }

    rtndf <- as.data.frame(cbind(preThreshold.V,amp,first.ndx,last.ndx,apd10,apd20,apd30,apd40,apd50,apd60,apd70,apd80,apd90,
                                 ap10,ap20,ap30,ap40,ap50,ap60,ap70,ap80,ap90,apd10,apd20,apd30,apd40,
                                 apd50,apd60,apd70,apd80,apd90,
                                 apd10.t1,apd20.t1,apd30.t1,
                                 apd40.t1,apd50.t1,apd60.t1,
                                 apd70.t1,apd80.t1,apd90.t1,
                                 apd10.t2,apd20.t2,apd30.t2,
                                 apd40.t2,apd50.t2,apd60.t2,
                                 apd70.t2,apd80.t2,apd90.t2))
    return(rtndf)
    
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

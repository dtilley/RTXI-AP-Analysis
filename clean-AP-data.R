################################################################################################
# apfind takes a vector and returns a dataframe marking potential APs                          #
# minlength: defines the minimum length of AP signal above the mean potential: default is 2 ms #
# dt: defines sampling interval between points: RTXI default is 0.1 ms                         #
################################################################################################
apfind <- function(x,minlength=2,dt=0.1){ 
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

##################################################################
# apview plots the AP train for a visual check of the APs        #
#           t: time vector                                       #
#           y: voltage vector                                    #
#       ndxDF: the data frame returned from apfind               #
# interactive: mode prompts the user to exlude misidentified APs #
##################################################################

apview <- function(t,y,ndxDF,interactive=FALSE,...){
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

# Generic plot fucntion with figure publication standards from the JGP
# EPS vector graphics work best for cross platform compatibility with AI and Inkscape.
jgpplotter <- function(x,y,mar=c(5.1,5.1,5.1,2.1),lwdpt=0.75,...){
    # convert pt to lwd
    lwd <- lwdpt*(1/0.75)
    par(mar=mar,lwd=lwd)
    plot(x,y,bty="n",...)
}

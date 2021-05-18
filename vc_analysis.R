remove.CapArtifacts <- function(tms, pApF, mV, cap.buffer=0.5, interpolate=FALSE) {
    ## Calculate derivative of voltage w/ respect to time
    dVdt <- diff(mV)/diff(tms)
    dVdt <- c(NA,dVdt)
    ## Assumes constant dt and cap.buffer is in ms
    dt <- diff(tms[1:2])
    cap.buff.length <- round(cap.buffer/dt)
    ramp.buff.length <- round((cap.buffer+1)/dt)
    ## Find when there are voltage changes
    nonZero.ndices <- which(dVdt!=0)
    nonZero.ndices.split <- split(nonZero.ndices,cumsum(c(1,diff(nonZero.ndices) != 1)))
    ## Rename list
    tmp.names <- NULL
    for (i in 1:length(nonZero.ndices.split)) {
        tmp.names[i] <- paste("V",i,sep="")
    }
    names(nonZero.ndices.split) <- tmp.names
    ndices.lengths <- as.vector(unlist(lapply(nonZero.ndices.split,length)))
    ## Assume shortest dVdt is a simple voltage step
    vstep.length <- min(ndices.lengths)
    
    for (i in 1:length(nonZero.ndices.split)) {
        if (ndices.lengths[i]==vstep.length) {
            ## Replace pApF w/ NA
            tmp.ndx <- nonZero.ndices.split[[i]][length(nonZero.ndices.split[[i]])]
            if (interpolate==FALSE){            
                tmp.na <- rep(NA,length(pApF[tmp.ndx:(tmp.ndx+cap.buff.length)]))
                pApF[tmp.ndx:(tmp.ndx+cap.buff.length)] <- tmp.na
            } else {
                m <- (pApF[(tmp.ndx+cap.buff.length)]-pApF[tmp.ndx])/(tms[(tmp.ndx+cap.buff.length)]-tms[tmp.ndx])
                tmp.t <- ((1:length(pApF[tmp.ndx:(tmp.ndx+cap.buff.length)]))-1)*dt
                tmp.y <- m*tmp.t + pApF[tmp.ndx]
                pApF[tmp.ndx:(tmp.ndx+cap.buff.length)] <- tmp.y
            }
        } else {
            tmp.ndx0 <- nonZero.ndices.split[[i]][1]
            tmp.ndx1 <- nonZero.ndices.split[[i]][length(nonZero.ndices.split[[i]])]
            if (interpolate==FALSE){
                ## Beginning of Ramp
                tmp.na <- rep(NA,length(pApF[tmp.ndx0:(tmp.ndx0+ramp.buff.length)]))
                pApF[tmp.ndx0:(tmp.ndx0+ramp.buff.length)] <- tmp.na
                ## End of Ramp
                if (tmp.ndx1 != length(tms)) {
                    tmp.na <- rep(NA,length(pApF[tmp.ndx1:(tmp.ndx1+cap.buff.length)]))
                    pApF[tmp.ndx1:(tmp.ndx1+cap.buff.length)] <- tmp.na
                }
            } else {
                ## Beginning of Ramp
                m <- (pApF[(tmp.ndx0+ramp.buff.length)]-pApF[tmp.ndx0])/(tms[(tmp.ndx0+ramp.buff.length)]-tms[tmp.ndx0])
                tmp.t <- ((1:length(pApF[tmp.ndx0:(tmp.ndx0+ramp.buff.length)]))-1)*dt
                tmp.y <- m*tmp.t + pApF[tmp.ndx0]
                pApF[tmp.ndx0:(tmp.ndx0+ramp.buff.length)] <- tmp.y
                ## End of Ramp
                if (tmp.ndx1 != length(tms)) {
                    m <- (pApF[(tmp.ndx1+cap.buff.length)]-pApF[tmp.ndx1])/(tms[(tmp.ndx1+cap.buff.length)]-tms[tmp.ndx1])
                    tmp.t <- ((1:length(pApF[tmp.ndx1:(tmp.ndx1+cap.buff.length)]))-1)*dt
                    tmp.y <- m*tmp.t + pApF[tmp.ndx1]
                    pApF[tmp.ndx1:(tmp.ndx1+cap.buff.length)] <- tmp.y
                }
            }
        }
    }
    return(pApF)
}

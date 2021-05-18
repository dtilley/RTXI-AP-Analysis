# rtxiH5reader is an R function that reads in RTXI style H5 data files.
# Output is a dataframe with a time and data vector.
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


getTrialTags <- function(file){
    h5labels <- h5ls(file=file)
    if (h5labels$name[1]=="Tags") {
        ## Store Trial Timestamps ##
        timestamp.start.ndices <- which(h5labels$name=="Timestamp Start (ns)")
        timestamp.stop.ndices <- which(h5labels$name=="Timestamp Stop (ns)")
        timestamp.start.labels <- paste(h5labels$group[timestamp.start.ndices],"/",h5labels$name[timestamp.start.ndices],sep="")
        timestamp.stop.labels <- paste(h5labels$group[timestamp.stop.ndices],"/",h5labels$name[timestamp.stop.ndices],sep="")
        timestamp.start <- rep(NA,length(timestamp.start.ndices))
        timestamp.stop <- rep(NA,length(timestamp.stop.ndices))
        for ( i in 1:length(timestamp.start.ndices)){
            timestamp.start[i] <- h5read(file=file,name=timestamp.start.labels[i],bit64conversion='double')
            timestamp.stop[i] <- h5read(file=file,name=timestamp.stop.labels[i],bit64conversion='double')
        }

        ## Store Tag Information ##
        tags.group.ndices <- which(h5labels$group=="/Tags")
        tags.labels <- paste("/Tags/",h5labels$name[tags.group.ndices],sep="")
        tag.no <- rep(NA,length(tags.group.ndices))
        tag.timestamp <- rep(NA,length(tags.group.ndices))
        tag.timestamp.ms <- rep(NA,length(tags.group.ndices))
        tag.text <- rep(NA,length(tags.group.ndices))
        tag.trial.no <- rep(NA,length(tags.group.ndices))
        for ( i in 1:length(tags.group.ndices)) {
            tmp <- h5labels$name[tags.group.ndices[i]]
            tmp <- unlist(strsplit(tmp,split=" "))
            tag.no[i] <- as.numeric(tmp[2])
            tmp <- h5read(file=file,name=tags.labels[i])
            tmp <- unlist(strsplit(tmp,split=","))
            tag.timestamp[i] <- as.numeric(tmp[1])
            tag.text[i] <- tmp[2]
            ## Assign Trial Number to Tag ##
            for (j in 1:length(timestamp.start)){
                if (tag.timestamp[i]>=timestamp.start[j] && tag.timestamp[i]<=timestamp.stop[j]) {
                    tag.trial.no[i] <- j
                    tag.timestamp.ms[i] <- (tag.timestamp[i] - timestamp.start[j])*1e-6
                }
            }
        }
        rtrn.df <- as.data.frame(cbind(tag.no,tag.text,tag.trial.no,tag.timestamp,tag.timestamp.ms))
        return(rtrn.df)
    } else {
        print("Could not identify Tags.")
    }
}

getTrialEvents <- function (file,return.trial.num=1) {

}

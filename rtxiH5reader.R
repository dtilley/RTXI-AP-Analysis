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

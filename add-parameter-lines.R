

addPARAMS.tseries.ms <- function(t.ms,df,col="red"){
    ## addPARAMS.tseries.ms assumes time stamp indices copied from h5 file. ##
    ## addPARAMS.tseries.ms returns a df with the time(s) in ms and R_indices ##
    ndx <- df[[1]]+1
    times <- t.ms[ndx]
    params <- df[[2]]
    abline(v=times,col=col)
    mtext(text=params,side=3,at=times,col=col)
    return(as.data.frame(cbind(ndx,times,params)))
}

addTAGS.tseries.ms <- function(tags.df,trial.no=1,col="red") {
    trial.tags <- subset(tags.df,tag.trial.no==trial.no)
    tags.text <- as.character(levels(trial.tags$tag.text))[trial.tags$tag.text]
    tags.timestamp.ms <- as.numeric(levels(trial.tags$tag.timestamp.ms))[trial.tags$tag.timestamp.ms]
    abline(v=tags.timestamp.ms,col=col)
    mtext(text=tags.text,side=3,at=tags.timestamp.ms,col=col)
}

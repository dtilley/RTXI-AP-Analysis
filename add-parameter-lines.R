

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

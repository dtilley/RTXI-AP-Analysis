

addPARAMS.tseries.ms <- function(df,col="red"){
    t <- df[[1]]/10
    params <- df[[2]]
    abline(v=t,col=col)
    mtext(text=params,side=3,at=t,col=col)
}

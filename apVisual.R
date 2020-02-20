# Generic plot fucntion with figure publication standards from the JGP
# EPS vector graphics work best for cross platform compatibility with AI and Inkscape.
jgpplotter <- function(x,y,mar=c(5.1,5.1,5.1,2.1),lwdpt=0.75,...){
    # convert pt to lwd
    lwd <- lwdpt*(1/0.75)
    par(mar=mar,lwd=lwd)
    plot(x,y,bty="n",...)
}


visualizeAPDs <- function(t,mV,apVoltages,apDurations,...){
    jgpplotter(t,mV,type="l",...)
    abline(v=t[apVoltages$peakV.ndx],col="blue")
    abline(v=t[apDurations$first.ndx],lty=1,col="seagreen")
    abline(v=t[apDurations$last.ndx],lty=2,col="magenta")
    colors <- rainbow(n=9)
    for (i in 1:nrow(apDurations)){
        segments(x0=apDurations$apd10.t1[i],y0=apDurations$ap10[i],x1=apDurations$apd10.t2[i],y1=apDurations$ap10[i],col=colors[1])
        segments(x0=apDurations$apd20.t1[i],y0=apDurations$ap20[i],x1=apDurations$apd20.t2[i],y1=apDurations$ap20[i],col=colors[2])
        segments(x0=apDurations$apd30.t1[i],y0=apDurations$ap30[i],x1=apDurations$apd30.t2[i],y1=apDurations$ap30[i],col=colors[3])
        segments(x0=apDurations$apd40.t1[i],y0=apDurations$ap40[i],x1=apDurations$apd40.t2[i],y1=apDurations$ap40[i],col=colors[4])
        segments(x0=apDurations$apd50.t1[i],y0=apDurations$ap50[i],x1=apDurations$apd50.t2[i],y1=apDurations$ap50[i],col=colors[5])
        segments(x0=apDurations$apd60.t1[i],y0=apDurations$ap60[i],x1=apDurations$apd60.t2[i],y1=apDurations$ap60[i],col=colors[6])
        segments(x0=apDurations$apd70.t1[i],y0=apDurations$ap70[i],x1=apDurations$apd70.t2[i],y1=apDurations$ap70[i],col=colors[7])
        segments(x0=apDurations$apd80.t1[i],y0=apDurations$ap80[i],x1=apDurations$apd80.t2[i],y1=apDurations$ap80[i],col=colors[8])
        segments(x0=apDurations$apd90.t1[i],y0=apDurations$ap90[i],x1=apDurations$apd90.t2[i],y1=apDurations$ap90[i],col=colors[9])
    }

}

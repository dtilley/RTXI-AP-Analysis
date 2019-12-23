# Generic plot fucntion with figure publication standards from the JGP
# EPS vector graphics work best for cross platform compatibility with AI and Inkscape.
jgpplotter <- function(x,y,mar=c(5.1,5.1,5.1,2.1),lwdpt=0.75,...){
    # convert pt to lwd
    lwd <- lwdpt*(1/0.75)
    par(mar=mar,lwd=lwd)
    plot(x,y,bty="n",...)
}


visualize.apDs <- function(t,Vm,apVoltages,apDurations){
    jgpplotter(t.ms,Vm,type="l")
    abline(v=t[apVoltages$peakV.ndx],col="blue")
    abline(v=t[apDurations$first.ndx],lty=1,col="seagreen")
    abline(v=t[apDurations$last.ndx],lty=2,col="seagreen")
    colors <- rainbow(n=9)
    for (i in 1:nrow(apDurations)){
        lines(t[apDurations$apd90.firstndx[i]:apDurations$apd90.lastndx[i]],y=rep(apDurations$ap90[i],length(t[apDurations$apd90.firstndx[i]:apDurations$apd90.lastndx[i]])),col=colors[1])
        lines(t[apDurations$apd80.firstndx[i]:apDurations$apd80.lastndx[i]],y=rep(apDurations$ap80[i],length(t[apDurations$apd80.firstndx[i]:apDurations$apd80.lastndx[i]])),col=colors[2])
        lines(t[apDurations$apd70.firstndx[i]:apDurations$apd70.lastndx[i]],y=rep(apDurations$ap70[i],length(t[apDurations$apd70.firstndx[i]:apDurations$apd70.lastndx[i]])),col=colors[3])
        lines(t[apDurations$apd60.firstndx[i]:apDurations$apd60.lastndx[i]],y=rep(apDurations$ap60[i],length(t[apDurations$apd60.firstndx[i]:apDurations$apd60.lastndx[i]])),col=colors[4])
        lines(t[apDurations$apd50.firstndx[i]:apDurations$apd50.lastndx[i]],y=rep(apDurations$ap50[i],length(t[apDurations$apd50.firstndx[i]:apDurations$apd50.lastndx[i]])),col=colors[5])
        lines(t[apDurations$apd40.firstndx[i]:apDurations$apd40.lastndx[i]],y=rep(apDurations$ap40[i],length(t[apDurations$apd40.firstndx[i]:apDurations$apd40.lastndx[i]])),col=colors[6])
        lines(t[apDurations$apd30.firstndx[i]:apDurations$apd30.lastndx[i]],y=rep(apDurations$ap30[i],length(t[apDurations$apd30.firstndx[i]:apDurations$apd30.lastndx[i]])),col=colors[7])
        lines(t[apDurations$apd20.firstndx[i]:apDurations$apd20.lastndx[i]],y=rep(apDurations$ap20[i],length(t[apDurations$apd20.firstndx[i]:apDurations$apd20.lastndx[i]])),col=colors[8])
        lines(t[apDurations$apd10.firstndx[i]:apDurations$apd10.lastndx[i]],y=rep(apDurations$ap10[i],length(t[apDurations$apd10.firstndx[i]:apDurations$apd10.lastndx[i]])),col=colors[9])
    }

}

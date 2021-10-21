source("~/projects/code-share-112018/jgpplotter.R")
## Reads in action potentials from evaluated models of iPSC EA fits

read.APSet  <- function(file.seq=seq(0,4), file.prefix="cell_2_") {
    if (file.seq[1] != 0) {print("Warning: Sequencing did not begin at 0.")}
    ap.files <- list.files(patter=file.prefix)
    models  <- list()
    for (i in file.seq) {
        ndx.pattern  <- paste("_",as.character(i),".txt",sep="")
        filenames  <- ap.files[grep(pattern=ndx.pattern, x=ap.files)]
        tmp  <- list()
        ## Assign APs to index
        tmp[[1]] <- read.table(file=filenames[grep(pattern="_cntrl_", x=filenames)],header=T)
        tmp[[2]] <- read.table(file=filenames[grep(pattern="_-0.15_ical_", x=filenames)],header=T)
        tmp[[3]] <- read.table(file=filenames[grep(pattern="_0.7_ical_", x=filenames)],header=T)
        tmp[[4]] <- read.table(file=filenames[grep(pattern="_-0.25_ikr_", x=filenames)],header=T)
        tmp[[5]] <- read.table(file=filenames[grep(pattern="_0.9_ikr_", x=filenames)],header=T)
        tmp[[6]] <- read.table(file=filenames[grep(pattern="_-0.9_ito_", x=filenames)],header=T)
        tmp[[7]] <- read.table(file=filenames[grep(pattern="_1.5_ito_", x=filenames)],header=T)
        tmp[[8]] <- read.table(file=filenames[grep(pattern="_10_iks_", x=filenames)],header=T)
        tmp[[9]] <- read.table(file=filenames[grep(pattern="_4_iks_", x=filenames)],header=T)

        models[[(i+1)]]  <- tmp
    }
    return(models)
}

get.APSet.order  <- function() {
    return(c("cntrl","-0.15_ical","0.7_ical","-0.25_ikr","0.9_ikr","-0.9_ito",
             "1.5_ito","10_iks","4_iks"))
}

plot.ModelSet  <- function(modelSet, ap.index=1, cols="black", ref.col="red", ...) {
    ap.label <- get.APSet.order()[ap.index]
    print(paste("AP: ",ap.label,sep=""))
    ap  <- as.data.frame(modelSet[[1]][[ap.index]])
    jgpplotter(ap$t, ap$mV_cell, type="n", axes=FALSE, ...)
    for (i in seq(length(modelSet))) {
        m <- as.data.frame(modelSet[[i]][[ap.index]])
        if (length(cols) == length(modelSet)) {
            lines(m$t, m$mV_simu, col=cols[i])
        } else {
            lines(m$t, m$mV_simu, col=cols)
        }
    }
    lines(ap$t, ap$mV_cell, col=ref.col, lty=2)
}

##  Euclidean Distance from origin
get.euclidean <- function(X, ref=NULL) {
    if (is.null(ref)) { 
        d  <- as.data.frame((apply((X*X), MARGIN=1, FUN=sum))^0.5)
    }
    else if (ncol(X) == length(ref)){
        d <- as.data.frame((apply(((X-ref)^2), MARGIN=1, FUN=sum))^0.5)
    } else {
        stop("Dimensions of X and ref did not match.")
    }
    names(d)  <- c("euclidean")
    return(d)
}

##  Get fitness values from scrs
get.fitness  <- function(scrs) {
    fitness <- as.data.frame(apply(X=scrs, MARGIN=1, FUN=sum))
    names(fitness)  <- c("fitness")
    return(fitness)
}

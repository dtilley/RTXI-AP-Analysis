if("plot3D" %in% rownames(installed.packages()) == FALSE) {
    install.packages("plot3D")
}
if("plot3Drgl" %in% rownames(installed.packages()) == FALSE) {
    install.packages("plot3Drgl")
}
if("rgl" %in% rownames(installed.packages()) == FALSE) {
    install.packages("rgl")
}

library("plot3D")
library("rgl")
library("plot3Drgl")

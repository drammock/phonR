#!/usr/bin/env R

source("../R/phonR.R")
load("../data/indoVowels.rda")

colorby <- rep(c("vowel", "subj", "gender"), each=3)
styleby <- rep(c("vowel", "subj", "gender"), times=3)
grouping <- c("subj", "gender", NA, "gender", NA, "subj", NA, "subj", "gender")


cairo_pdf("test-legend-master.pdf", width=16, height=16, pointsize=12, 
          family="Charis SIL")
    par(mfrow=c(3, 3))
    mapply(FUN=function(cb, sb, gr) {
        title <- paste0("var.col.by=", cb, ", var.sty.by=", sb, ", group=", gr, collapse="")
        plotVowels(indo$f1, indo$f2, indo$vowel, group=indo[[gr]], 
                 var.col.by=indo[[cb]], var.sty.by=indo[[sb]], pretty=TRUE,
                 plot.tokens=TRUE, plot.means=FALSE, ellipse.line=TRUE, 
                 legend.kwd='bottomright', main=title)
        }, colorby, styleby, grouping)
dev.off()

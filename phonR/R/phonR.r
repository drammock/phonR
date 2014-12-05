# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# phonR version 1.0-0-git
# Functions for phoneticians and phonologists
# AUTHOR: Daniel McCloy, drmccloy@uw.edu
# LICENSED UNDER THE GNU GENERAL PUBLIC LICENSE v3.0:
# http://www.gnu.org/licenses/gpl.html
# DEVELOPMENT OF THIS PACKAGE WAS FUNDED IN PART BY NIH-R01DC006014
#
# CHANGELOG:
# v1.0: major refactor of the entire codebase. Enhancements: color and
# style can now be specified either by vowel or by group. Plot dimension
# override via standard "xlim" / "ylim" arguments. New drawing functions
# for repulsive force heatmaps and convex hulls. Hulls, polygons, and
# ellipses can have color fills. Shortcut argument "pretty=TRUE" soothes
# the senses. Smarter handling of additional graphical arguments passed
# via "..." to the appropriate function(s). Diphthongs with arrowheads.
#
# v0.4: bugfixes: poly.order now works with arbitrary labels; bug in
# s-centroid calculation fixed.  Enhancements: added user-override
# arguments for color, shape and linestyle; added support for diphthong
# plotting, argument poly.include eliminated (inferred from elements
# present in poly.order), new argument points.label allows override of
# points label when points='text'.
#
# v0.3 bugfixes: font specification on windows now works for direct-to-
# file output. Enhancements: graphics handling overhauled to use base
# graphics instead of Cairo(). Several new output formats added. Raster
# resolution and font size now specifiable. Improved error handling.
#
# v0.2 bugfixes: points.alpha and means.alpha now work for grayscale
# plots. Plots with polygons or ellipses but no shapes now get proper
# legend type (lines, not boxes). Graphical parameters now captured and
# restored when plotting to onscreen device. Vowels with no variance
# (e.g., single tokens) no longer crash ellipse function. Vowels not in
# default poly.order() no longer go unplotted when points='text'.
# Enhancements: support for custom axis titles (to accommodate pre-
# normalized values), point and mean sizes, and fonts. Custom line types
# added (11 total now).
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# USAGE: source("phonR.r")
# --or-- from command line (replace Xs with version number):
# R CMD install phonR_X.X-X.tar.gz
# Then library(phonR)
# Then call functions as needed

plot.vowels <- function(f1, f2, vowel=NULL, group=NULL,
    plot.tokens=TRUE, pch.tokens=NULL, cex.tokens=NULL,
    plot.means=FALSE, pch.means=NULL, cex.means=NULL,
    hull.line=FALSE, hull.fill=FALSE, hull.col=NULL,
    poly.line=FALSE, poly.fill=FALSE, poly.col=NULL, poly.order=NA,
    ellipse.line=FALSE, ellipse.fill=FALSE, ellipse.conf=0.3173,
    diphthong.smooth=FALSE, diphthong.arrows=FALSE, diphthong.arrow.args=NULL,
    force.heatmap=FALSE, force.colmap=NULL, force.res=50, force.method='default',
    force.legend=NULL, force.labels=NULL, force.label.pos=c(1, 3),
    col.by=NA, style.by=NA, fill.opacity=0.3, legend.kwd=NULL, pretty=FALSE,
    output='screen', ...)
{
    # # # # # # # # #
    # DEPENDENCIES  #
    # # # # # # # # #
    require(plotrix)  # provides color.scale()

    # # # # # # # # # # #
    # HANDLE EXTRA ARGS #
    # # # # # # # # # # #
    exargs <- list(...)
    font.specified <- 'family' %in% names(exargs)
    # two arguments get overridden no matter what
    exargs$ann <- FALSE
    exargs$type <- 'n'
    # Some graphical devices only support inches, so we convert here.
    if ("units" %in% names(exargs)) {
        if (!exargs$units %in% c("in", "cm", "mm", "px")) {
            warning("Unsupported argument value '", units, "': 'units' must be ",
                    "one of 'in', 'cm', 'mm', or 'px'. Using default ('in').")
            exargs$units <- "in"
        }
        if (output %in% c("pdf", "svg", "screen")) {
            if ("width" %in% names(exargs)) {
                if      (exargs$units == "cm") exargs$width <- exargs$width/2.54
                else if (exargs$units == "mm") exargs$width <- exargs$width/2540
                else if (exargs$units == "px") exargs$width <- exargs$width/72
            }
            if ("height" %in% names(exargs)) {
                if      (exargs$units == "cm") exargs$height <- exargs$height/2.54
                else if (exargs$units == "mm") exargs$height <- exargs$height/2540
                else if (exargs$units == "px") exargs$height <- exargs$height/72
            }
        }
    }
    # Some args are only settable by direct par() call (not via plot(), etc).
    # Not strictly true for "family" or "las", but works better this way.
    par.only <- c("ask", "fig", "fin", "las", "lheight", "mai", "mar",
                "mex", "mfcol", "mfrow", "mfg", "new", "oma", "omd", "omi",
                "pin", "plt", "ps", "pty", "usr", "xlog", "ylog", "ylbias")
    if (output == "screen") {
        par.only <- append(par.only, "family")
    } else {
        file.only <- c("filename", "width", "height", "units", "pointsize",
                    "res", "quality", "compression", "family")
        file.args <- exargs[names(exargs) %in% file.only]
        exargs <- exargs[!(names(exargs) %in% file.only)]
    }
    par.args <- exargs[names(exargs) %in% par.only]
    exargs <- exargs[!(names(exargs) %in% par.only)]

    # # # # # # # # # #
    # OUTPUT PARSING  #
    # # # # # # # # # #
    output <- tolower(output)
    if (output=="jpeg") output <- "jpg"
    if (output=="tiff") output <- "tif"
    output.types <- c("pdf", "svg", "jpg", "tif", "png", "bmp", "screen")
    output.raster <- c("jpg", "tif", "png", "bmp", "screen")
    if (!(output %in% output.types)) {
        warning("Unknown argument value '", output, "': 'output' ",
                "must be one of 'pdf', 'svg', 'png', 'tif', 'bmp', ",
                "'jpg', or 'screen'. Using default ('screen').")
        output <- "screen"
    }

    # # # # # # # # # # #
    # LEGEND KWD CHECK  #
    # # # # # # # # # # #
    legend.kwds <- c("left", "right", "top", "bottom", "center", "topleft",
                    "topright", "bottomleft", "bottomright")
    if (!is.null(legend.kwd)) {
        if (!legend.kwd %in% legend.kwds) {
        warning(paste(c("legend.kwd must be one of '",
                        paste(legend.kwds, collapse="', '"), "'."), collapse=""))
    }   }

    # # # # # # # # # # # #
    # DIPHTHONG HANDLING  #
    # # # # # # # # # # # #
    if (is.vector(f1) && is.vector(f2)) {
        diphthong <- FALSE
    } else if (length(dim(f1)) == 1 && length(dim(f2)) == 1) {
        f1 <- as.vector(f1)
        f2 <- as.vector(f2)
        diphthong <- FALSE
    } else {
        if (!all(dim(f1) == dim(f2))) stop('Unequal dimensions for "f1" and "f2".')
        else if (length(dim(f1)) > 2) stop('Argument "f1" has more than two dimensions.')
        else if (length(dim(f2)) > 2) stop('Argument "f2" has more than two dimensions.')
        else if (length(vowel) != dim(f1)[1]) stop('First axis of "f1" does not equal length of "vowel".')
        diphthong <- TRUE
    }
    if (!diphthong) {
        if (length(f2) != length(f1)) stop('Unequal dimensions for "f1" and "f2".')
        else if (length(vowel) != length(f1)) stop('Unequal dimensions for "f1" and "vowel".')
    } else {
        f1d <- f1
        f2d <- f2
        f1 <- f1d[,1]
        f2 <- f2d[,1]
        timepts <- ncol(f2d)
        tokens <- nrow(f2d)
    }

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # PRELIMINARY HANDLING OF GROUPING FACTOR, COLOR, AND STYLE #
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    l <- length(vowel)
    if (is.null(group)) gf <- rep('gf', l)
    else 			    gf <- as.numeric(factor(group))
    # used later to set default polygon color when color varies by vowel
    if (identical(col.by, vowel)) col.by.vowel <- TRUE
    else                          col.by.vowel <- FALSE
    # color.by & style.by
    if (is.na(col.by[1]))    col.by <- rep(1, l)  # default to black
    else					col.by <- as.numeric(factor(col.by))
    if (is.na(style.by[1]))	style.by <- rep(1, l)  # default to solid
    else					style.by <- as.numeric(factor(style.by))
    num.col <- length(unique(col.by))
    num.sty <- length(unique(style.by))
    # misc. plotting defaults
    if (is.null(cex.tokens))    cex.tokens <- par('cex')
    if (is.null(cex.means))      cex.means <- par('cex')

    # # # # # # # #
    # ANNOTATION  #
    # # # # # # # #
    if ("xlab" %in% names(exargs)) xlab <- exargs$xlab
    else if (pretty)               xlab <- "F2"
    else                           xlab <- ""
    if ("ylab" %in% names(exargs)) ylab <- exargs$ylab
    else if (pretty)               ylab <- "F1"
    else                           ylab <- ""
    if ("main" %in% names(exargs)) main <- exargs$main
    else if (pretty)               main <- "Vowels"
    else                           main <- ""
    if ("sub" %in% names(exargs))   sub <- exargs$sub
    else if (pretty)                sub <- "" # "plotted with love using phonR"
    else                            sub <- ""
    exargs$xlab <- NULL
    exargs$ylab <- NULL
    exargs$main <- NULL
    exargs$sub <- NULL

    # # # # # # # # # # # # #
    # DEFAULTS FOR "PRETTY" #
    # # # # # # # # # # # # #
    if (pretty) {
        # if no colors specified, use equally spaced HCL values
        # [-1] avoids duplicate hues 0 and 360
        hue <- seq(0,  360, length.out=1+num.col)[-1]
        chr <- seq(60, 100, length.out=num.col)
        lum <- seq(60,  40, length.out=num.col)
        pretty.col <- hcl(hue, chr, lum, alpha=1)[col.by]
        # PCH: filled / open {circ,tri,squ,diam}, plus, x, inverted open tri
        pretty.pch <- rep(c(16,1,17,2,15,0,18,5,3,4,6), length.out=l)[style.by]
        # LTY: custom linetypes more readily distinguishable
        pretty.lty <- c('solid', '44', 'F4', '4313', 'F3131313', '23F3',
                        '232923', '23258385', '282823B3', '13', '82')[style.by]
        pretty.args <- list(mgp=c(2,0.5,0), xaxs='i', yaxs='i', axes=FALSE,
                            fg=hcl(0,0,40), tcl=-0.25, xpd=NA, asp=1,
                            pch=pretty.pch, lty=pretty.lty, col=pretty.col)
                            # frame.plot=FALSE implied by axes=FALSE
        pretty.par.args <- list(mar=c(1,1,5,5), las=1)  # oma=rep(0.5, 4)
        pretty.arrow.args <- list(length=0.1, angle=20)
        # LET USER-SPECIFIED ARGS OVERRIDE "PRETTY" DEFAULTS
        pretty.args[names(exargs)] <- exargs
        pretty.par.args[names(par.args)] <- par.args
        pretty.arrow.args[names(diphthong.arrow.args)] <- diphthong.arrow.args
        # RE-UNIFY TO AVOID LATER LOGIC BRANCHING
        exargs <- pretty.args
        par.args <- pretty.par.args
        diphthong.arrow.args <- pretty.arrow.args
    }

    # # # # # # # # # #
    # OTHER DEFAULTS  #
    # # # # # # # # # #
    # colors: use default pallete if none specified and pretty=FALSE
    if (!'col' %in% names(exargs)) exargs$col <- palette()[col.by]
    # linetypes
    if (!'lty' %in% names(exargs)) exargs$lty <- style.by
    # plotting characters
    if (!'pch' %in% names(exargs)) exargs$pch <- style.by
    if (is.null(pch.tokens)) pcht <- exargs$pch
    else                     pcht <- pch.tokens
    if (is.null(pch.means))  pchm <- exargs$pch
    else                     pchm <- pch.means
    # transparency
    trans.col <- makeTransparent(exargs$col, fill.opacity)
    trans.fg <- makeTransparent(par('fg'), fill.opacity)
    # ellipse colors
    if (ellipse.line) ellipse.line.col <- exargs$col
    else              ellipse.line.col <- NA[col.by]
    if (ellipse.fill) ellipse.fill.col <- trans.col
    else              ellipse.fill.col <- NA[col.by]
    # hull colors
    if (hull.line) {
        if (!is.null(hull.col)) hull.line.col <- hull.col[col.by]
        else if (col.by.vowel)  hull.line.col <- par("fg")
        else                    hull.line.col <- exargs$col
    } else {                    hull.line.col <- NA[col.by] }
    if (hull.fill) {
        if (!is.null(hull.col)) hull.fill.col <- makeTransparent(hull.col[col.by],
                                                                fill.opacity)
        else if (col.by.vowel)  hull.fill.col <- trans.fg
        else                    hull.fill.col <- trans.col
    } else {                    hull.fill.col <- NA[col.by] }
    # polygon colors
    if (poly.line) {
        if (!is.null(poly.col)) poly.line.col <- poly.col[col.by]
        else if (col.by.vowel)  poly.line.col <- par("fg")
        else                    poly.line.col <- exargs$col
    } else {                    poly.line.col <- NA[col.by] }
    if (poly.fill) {
        if (!is.null(poly.col)) poly.fill.col <- makeTransparent(poly.col[col.by],
                                                                fill.opacity)
        else if (col.by.vowel)  poly.fill.col <- trans.fg
        else                    poly.fill.col <- trans.col
    } else {                    poly.fill.col <- NA[col.by] }

    # # # # # # # # # # # # # # #
    # INITIALIZE OUTPUT DEVICES #
    # # # # # # # # # # # # # # #
    if      (output=='pdf') do.call(cairo_pdf, file.args)
    else if (output=='svg') do.call(svg, file.args)
    else if (output=='jpg') do.call(jpeg, file.args)
    else if (output=='tif') do.call(tiff, file.args)
    else if (output=='png') do.call(png, file.args)
    else if (output=='bmp') do.call(bmp, file.args)
    # FONT HANDLING FOR WINDOWS (RELATED BLOCK AT END OF SCRIPT)
    is.win <- .Platform$OS.type == 'windows'
    if (is.win && font.specified && output %in% output.raster) {
        windowsFonts(phonr=windowsFont(par.args$family))
        par.args$family <- 'phonr'
        if (output=='screen') warning("Font specification may fail if saving ",
                                    "as PDF from onscreen plot window menu. ",
                                    "To ensure PDF font fidelity, run ",
                                    "plot.vowels() with output='pdf'.")
    }
    # INITIAL CALL TO PAR()
    op <- par(par.args)

    # # # # # # # # # # # # # #
    # COLLECT IMPORTANT STUFF #
    # # # # # # # # # # # # # #
    d <- data.frame(v=vowel, gf=factor(gf), m=rep(plot.means, l),
                    color=exargs$col, style=style.by,
                    ellipse.fill.col=ellipse.fill.col,
                    ellipse.line.col=ellipse.line.col,
                    poly.fill.col=poly.fill.col, poly.line.col=poly.line.col,
                    hull.fill.col=hull.fill.col, hull.line.col=hull.line.col,
                    pch.means=pchm, pch.tokens=pcht, tokenid=1:length(vowel),
                    stringsAsFactors=FALSE)
    if (diphthong) {
        e <- d
        d$f2 <- f2d[,1]
        d$f1 <- f1d[,1]
        d$t <- 1
        for(i in 2:timepts) {
            e$f2 <- f2d[,i]
            e$f1 <- f1d[,i]
            e$t <- i
            d <- do.call(rbind, list(d, e))
        }
    } else {
        d$f2 <- f2
        d$f1 <- f1
    }
    dd <- by(d, d[c('v','gf')], identity)
    # CALCULATE VOWEL COVARIANCES (the following line still may yield
    # some NA cov. matrices, due to some vowels having only 1 token)
    s <- lapply(dd, function(i) if (!(is.null(i)))
        with(i[!(is.na(i$f2)) && !(is.na(i$f1)),],
        list(cov(cbind(f2, f1)))))
    s <- do.call(rbind, s)
    # CALCULATE VOWEL MEANS
    mu <- lapply(dd, function(i) { if (!(is.null(i))) {
            with(i[!(is.na(i$f2)) && !(is.na(i$f1)),], list(colMeans(cbind(f2, f1))))
        }})
    mu <- do.call(rbind, mu)
    # COLLECT INTO DATAFRAME
    m <- lapply(dd, function(i) if (!(is.null(i)))
        data.frame(f2=mean(i$f2, na.rm=TRUE), f1=mean(i$f1, na.rm=TRUE),
                v=unique(i$v), gf=unique(i$gf), m=unique(i$m),
                color=i$color[1], style=i$style[1],
                poly.fill.col=i$poly.fill.col[1],
                poly.line.col=i$poly.line.col[1],
                ellipse.fill.col=i$ellipse.fill.col[1],
                ellipse.line.col=i$ellipse.line.col[1],
                pch.means=i$pch.means[1],
                stringsAsFactors=FALSE))
    m <- do.call(rbind, m)
    m$gfn <- as.numeric(factor(m$gf))
    m$mu <- mu
    m$sigma <- s
    m$sigma <- unlist(lapply(m$sigma,
            function(i) ifelse(is.na(i[[1]][1]),
            list(matrix(c(0,0,0,0), nrow=2)),
            list(i))), recursive=FALSE)
    # PLOT EXTREMA
    plot.bounds <- apply(d[,c('f2','f1')], 2, range, finite=TRUE)
    # ELLIPSE EXTREMA
    if (!(is.null(ellipse.fill) && is.null(ellipse.line))) {
        ellipse.args <- apply(m, 1, function(i) list('mu'=i$mu,
                        'sigma'=i$sigma, 'alpha'=ellipse.conf,
                        'draw'=FALSE))
        ellipse.points <- lapply(ellipse.args,
                        function(i) do.call(ellipse, i))
        ellipse.bounds <- lapply(ellipse.points,
                        function(i) apply(i, 2, range, finite=TRUE))
        ellipse.bounds <- apply(do.call(rbind, ellipse.bounds), 2,
                        range, finite=TRUE)
        plot.bounds <- apply(rbind(plot.bounds, ellipse.bounds), 2,
                    range, finite=TRUE)
    }
    if (!'xlim' %in% names(exargs)) exargs$xlim <- rev(plot.bounds[,1])
    if (!'ylim' %in% names(exargs)) exargs$ylim <- rev(plot.bounds[,2])

    if (pretty) {
        # ticks
        xticks <- prettyticks(exargs$xlim)
        yticks <- prettyticks(exargs$ylim)
        exargs$xlim <- rev(range(xticks))
        exargs$ylim <- rev(range(yticks))
        # annotation
        x.args <- list(side=3, line=2, col=par("fg"))
        y.args <- list(side=4, line=3, col=par("fg"))
        t.args <- list(side=3, line=4, col=par("fg"))
        s.args <- list(side=3, line=3, col=par("fg"))
    } else {
        x.args <- list(side=1, line=par("mgp")[1])
        y.args <- list(side=2, line=par("mgp")[1])
        t.args <- list(side=3, line=1, outer=TRUE)
        s.args <- list(side=4, line=par("mgp")[1] + 1)
    }
    do.call(plot, as.list(c(list(NA, NA), exargs)))
    do.call(mtext, as.list(c(xlab, x.args)))
    do.call(mtext, as.list(c(ylab, y.args)))
    do.call(mtext, as.list(c(main, t.args)))
    do.call(mtext, as.list(c(sub, s.args)))
    if (pretty) {
        # axes
        axis(3, at=xticks, col.axis=par('fg'))
        axis(4, at=yticks, col.axis=par('fg'))
        # extend the axis lines to meet at the corner
        if (exargs$xlim[2] != par('usr')[2]) {
            axis(3, at=c(exargs$xlim[2], par('usr')[2]), labels=FALSE,
                col=par('fg'), tcl=0)
        }
        if (exargs$ylim[2] != par('usr')[4]) {
            axis(4, at=c(exargs$ylim[2], par('usr')[4]), labels=FALSE,
                col=par('fg'), tcl=0)
        }
    }

    # # # # # # # # #
    # PLOT HEATMAP  #
    # # # # # # # # #
    if (force.heatmap) {
        if (pretty & is.null(force.colmap)) {
            force.colmap <- color.scale(x=0:100, cs1=c(0, 180), cs2=100,
                                        cs3=c(25, 100), alpha=0.5,
                                        color.spec='hcl')
        }
        force <- with(d, repulsiveForce(f2, f1, v))
        with(d, repulsiveForceHeatmap(f2, f1, force, vowel=v, resolution=force.res,
                                    colormap=force.colmap, method=force.method,
                                    add=TRUE))
        if (is.null(force.legend)) {
            xl <- rep(exargs$xlim[1], 2)
            yl <- exargs$ylim - c(0, diff(exargs$ylim) / 2)
        } else if (!is.na(force.legend)) {
            xl <- force.legend[1:2]
            yl <- force.legend[3:4]
        }
        forceHeatmapLegend(xl, yl, colormap=force.colmap)
        if (!is.null(force.labels)) {
            text(xl, yl, labels=force.labels, pos=force.label.pos, xpd=TRUE)
        }
    }
    # # # # # # #
    # PLOT HULL #
    # # # # # # #
    if (hull.fill || hull.line) {
        hh <- by(d, d$gf, function(i) i[!is.na(i$f2) & !is.na(i$f1),])
        hulls <- lapply(hh, function(i) with(i, i[chull(f2, f1),
                        c('f2', 'f1', 'color', 'hull.fill.col', 'hull.line.col',
                        'style')]))
        lapply(hulls, function(i) with(i, polygon(cbind(f2, f1),
                                                col=hull.fill.col,
                                                border=hull.line.col,
                                                lty=style)))
    }
    # # # # # # # # #
    # PLOT ELLIPSES #
    # # # # # # # # #
    if (ellipse.fill || ellipse.line) {
        lapply(seq_along(ellipse.points),
            function(i) polygon(ellipse.points[[i]], col=m$ellipse.fill.col[i],
                                border=m$ellipse.line.col[i], lty=m$style[i]))
    }
    # # # # # # # # #
    # PLOT POLYGONS #
    # # # # # # # # #
    if (!is.na(poly.order[1]) && (poly.fill || poly.line)) {
        if (length(poly.order) != length(unique(poly.order))) warning(
            'Duplicate entries in "polygon" detected; they will be ignored.')
        poly.order <- unique(as.character(poly.order)) # as.character in case factor
        v <- unique(as.character(m$v))
        if (length(setdiff(poly.order, v)) > 0) {
            warning('There are vowels in "polygon" that are not in ',
                    '"vowel"; they will be ignored.')
            poly.order <- intersect(poly.order, v)
        }
        pp <- m
        pp$v <- factor(pp$v, levels=poly.order)
        pp <- pp[order(pp$v),]
        pp <- split(pp, pp$gf)
        if (poly.fill) {
            bigenough <- sapply(pp, function(i) nrow(i) > 2)
            lapply(pp[bigenough], function(i) with(i, polygon(cbind(f2, f1),
                                                            col=poly.fill.col,
                                                            border=NA)))
        }
        if (poly.line) {
            if (plot.means) type <- 'c'
            else type <- 'l'
            bigenough <- sapply(pp, function(i) nrow(i) > 1)
            invisible(lapply(pp[bigenough], function(i) {
                with(i[i$v %in% poly.order,], points(f2, f1, col=poly.line.col,
                                                    type=type, lty=style,
                                                    cex=1.25*cex.means))
                }))
        }
    }
    # # # # # # # #
    # PLOT TOKENS #
    # # # # # # # #
    if (plot.tokens) {
        if (diphthong) {
            d <- d[order(d$tokenid, d$t),]
            dsp <- split(d, d$tokenid)
            if (is.null(pch.tokens)) {
                pch.tokens <- as.numeric(d$pch.tokens)
                #with(d, points(f2, f1, col=color, pch=pch.tokens, cex=cex.tokens, type="o"))
                diphthong.smooth <- FALSE  # TODO: get this working
                if (diphthong.smooth && timepts > 3) {
                    invisible(lapply(dsp, function(i) {
                        steep <- abs(lm(f1~f2)$coefficients['f2']) > 1
                        if (steep)  pc <- prcomp(i[c("f1", "f2")], center=FALSE, scale.=FALSE)
                        else       pc <- prcomp(i[c("f2", "f1")], center=FALSE, scale.=FALSE)
                        tryCatch({
                            ss <- smooth.spline(pc$x)
                            ssi <- as.matrix(as.data.frame(predict(ss))) %*% solve(pc$rotation) #* pc$scale + pc$center
                            if (steep)  ssi <- ssi[,2:1]
                            #ssi[1,] <- unlist(i[1, c("f2", "f1")])
                            #ssi[nrow(ssi),] <- unlist(i[nrow(i), c("f2", "f1")])
                            if (diphthong.arrows) {
                                end <- nrow(ssi)
                                lines(ssi[1:end-1,], col=i$color)
                                with(as.data.frame(ssi),
                                    do.call(arrows, c(list(x0=f2[end-1], y0=f1[end-1],
                                                            x1=f2[end], y1=f1[end],
                                                            col=i$color), diphthong.arrow.args)))
                            } else {
                                lines(ssi, col=i$color)
                            }
                        },
                        error=function(e){
                            message("warning: could not plot smoother for ",
                                    "diphthong. Plotting connecting segments instead.")
                            message(paste(e, ""))
                            if (diphthong.arrows) {
                                end <- nrow(i)
                                with(i, points(f2[1:end-1], f1[1:end-1], col=color,
                                            pch=pch.tokens, cex=cex.tokens, type="o"))
                                with(i, do.call(arrows, c(list(x0=f2[end-1], y0=f1[end-1],
                                                            x1=f2[end], y1=f1[end],
                                                            col=color), diphthong.arrow.args)))
                            } else {
                                with(i, points(f2, f1, col=color, pch=pch.tokens, cex=cex.tokens, type="o"))
                            }
                        },
                        warning=function(w) message(w), finally={}
                        )
                    }))
                } else {
                    if (diphthong.smooth) {
                        warning("Cannot smooth diphthong traces with fewer than 4 timepoints.",
                                "Plotting connecting segments.")
                    }
                    invisible(lapply(dsp, function(i) {
                        if (diphthong.arrows) {
                            end <- nrow(i)
                            with(i, points(f2[1:end-1], f1[1:end-1], col=color,
                                        pch=pch.tokens, cex=cex.tokens, type="o"))
                            with(i, do.call(arrows, c(list(x0=f2[end-1], y0=f1[end-1],
                                                        x1=f2[end], y1=f1[end],
                                                        col=color), diphthong.arrow.args)))
                        } else {
                            with(i, points(f2, f1, col=color, pch=pch.tokens,
                                        cex=cex.tokens, type="o"))
                        }
                    }))
                }
            } else {
                invisible(lapply(dsp, function(i) {
                    with(i, points(f2, f1, col=color, type="c", cex=1.25*cex.tokens))
                    #with(d, text(f2, f1, labels=pch.tokens, col=color, cex=cex.tokens))
                }))
            }
        } else {
            if (is.null(pch.tokens)) {
                pch.tokens <- as.numeric(d$pch.tokens)
                with(d, points(f2, f1, col=color, pch=pch.tokens, cex=cex.tokens))
            } else {
                with(d, text(f2, f1, labels=pch.tokens, col=color, cex=cex.tokens))
            }
        }
    }
    # # # # # # # #
    # PLOT MEANS  #
    # # # # # # # #
    if (plot.means) {
        if (is.null(pch.means)) {
            pch.means <- as.numeric(factor(colnames(m)))
            with(m, points(f2, f1, col=color, pch=pch.means, cex=cex.means))
        } else {
            with(m, text(f2, f1, labels=pch.means, col=color, cex=cex.means))
        }
    }
    # # # # # #
    # LEGEND  #
    # # # # # #
    if (!is.null(legend.kwd)) {
        if (col.by.vowel)  legend.text <- unique(m$v)
        else              legend.text <- unique(m$gf)
        legend.args <- list(legend.kwd, legend=legend.text, bty="n", seg.len=1)
        # legend lines
        if (hull.line | poly.line | ellipse.line) {
            legend.args <- append(legend.args, list(lty=unique(m$style),
                                                    pch=unique(m$pch.means),
                                                    col=unique(m$color)))
        }
        # legend boxes
        if (hull.fill | poly.fill | ellipse.fill) {
            if (!is.na(m$ellipse.fill.col[1]))   legend.fill <- unique(m$ellipse.fill.col)
            else if (!is.na(m$poly.fill.col[1])) legend.fill <- unique(m$poly.fill.col)
            else legend.fill <- unique(hull.col)
            legend.args$pch <- 22
            legend.args <- append(legend.args, list(pt.bg=legend.fill,
                                                    pt.cex=2.5
                                                    #fill=legend.fill,
                                                    #border=unique(m$color)
                                                    ))
        }
        # draw legend
        do.call(legend, legend.args)
    }

    # # # # # #
    # CLEANUP #
    # # # # # #
    # close file devices
    if (output != "screen") dev.off()
    # reset graphical parameters to defaults
    par(op)
}


prettyticks <- function(lim) {
    axrange <- abs(diff(lim))
    step <- 10^(floor(log(axrange,10)))
    coef <- ifelse(axrange/step < 1, 0.1, ifelse(axrange/step < 2, 0.2,
            ifelse(axrange/step < 5, 0.5, 1)))
    step <- step*coef
    lims <- c(ceiling(max(lim)/step)*step, floor(min(lim)/step)*step)
    if (diff(lims) < 0) {step <- -step}
    seq(lims[1],lims[2],step)
}


ellipse <- function(mu, sigma, alpha=0.05, npoints=250, draw=TRUE, ...) {
    # adapted from the (now-defunct) mixtools package
    es <- eigen(sigma)
    e1 <- es$vec %*% diag(sqrt(es$val))
    r1 <- sqrt(qchisq(1-alpha, 2))
    theta <- seq(0, 2*pi, len=npoints)
    v1 <- cbind(r1*cos(theta), r1*sin(theta))
    pts <- t(mu-(e1 %*% t(v1)))
    if (draw) {
        colnames(pts) <- c('x','y')
        polygon(pts, ...)
    }
    invisible(pts)
}


convexHullArea <- function(x, y, group=NULL) {
    require(splancs)
    if (is.null(group))  group <- "all.points"
    df <- data.frame(x=x, y=y, g=group, stringsAsFactors=FALSE)
    bygrouppts <- by(df, df$g, function(i) i[chull(i$x, i$y),c('x','y')])
    bygrouparea <- sapply(bygrouppts, function(i) {
        areapl(as.matrix(data.frame(x=i$x, y=i$y, stringsAsFactors=FALSE)))
        })
}


repulsiveForce <- function(x, y, type) {
    dmat <- as.matrix(dist(cbind(x, y)))
    force <- sapply(seq_along(type), function(i) {
        sum(1 / dmat[i, !(type %in% type[i])] ^ 2)
        })
}


# OMNIBUS NORMALIZATION FUNCTION (convenience function)
norm.vowels <- function(method, f0=NULL, f1=NULL, f2=NULL, f3=NULL,
                    vowel=NULL, group=NULL, ...) {
    m <- tolower(method)
    methods <- c('bark','mel','log','erb','z','zscore','lobanov',
                'logmean','nearey1','nearey2','scentroid','s-centroid',
                's','wattfabricius','watt-fabricius')
    if (!(m %in% methods)) {
        warning('Method must be one of: bark, mel, log, erb, ',
                'z|zscore|lobanov, logmean|nearey1, nearey2, ',
                's|scentroid|s-centroid|wattfabricius|watt-fabricius.')
    }
    f <- cbind(f0=f0, f1=f1, f2=f2, f3=f3)
    if (m %in% 'bark') return(norm.bark(f))
    else if (m %in% 'mel') return(norm.mel(f))
    else if (m %in% 'log') return(norm.log(f))
    else if (m %in% 'erb') return(norm.erb(f))
    else if (m %in% c('z','zscore','lobanov')) return(norm.lobanov(f, group))
    else if (m %in% c('logmean','nearey1')) return(norm.logmean(f, group, ...))
    else if (m %in% c('nearey','nearey2')) return(norm.nearey(f, group))
    else {
        f <- as.matrix(cbind(f1=f1, f2=f2))
        return(norm.wattfabricius(f, vowel, group))
}	}


# INDIVIDUAL NORMALIZATION FUNCTIONS
norm.bark <- function(f) {
    26.81*f/(1960+f)-0.53
}


norm.log <- function(f) {
    log10(f)
}


norm.mel <- function(f) {
    2595*log10(1+f/700)
}


norm.erb <- function(f) {
    21.4*log10(1+0.00437*f)
}


norm.lobanov <- function(f, group=NULL) {
    if (is.null(group)) {
        return(scale(f))
    } else {
        f <- as.data.frame(f)
        groups <- split(f, group)
        scaled <- lapply(groups, function(x) as.data.frame(scale(x)))
        return(unsplit(scaled, group))
}	}


norm.logmean <- function(f, group=NULL, ...) {
    if (is.null(group)) {
        return(log(f) - rep(colMeans(log(f), ...), each=nrow(f)))
    } else {
        f <- as.data.frame(f)
        groups <- split(f, group)
        logmeans <- lapply(groups,
                    function(x) log(x) - rep(apply(log(x), 2, mean),
                    each=nrow(x)))
        return(unsplit(logmeans, group))
}	}


norm.nearey <- function(f, group=NULL, ...) {
    if (ncol(f) != 4) {
        stop("Missing values: normalization method 'nearey2' ",
            "requires non-null values for f0, f1, f2, and f3).")
    }
    if (is.null(group)) {
        return(log(f) - sum(colMeans(log(f), ...)))
    } else {
        f <- as.data.frame(f)
        groups <- split(f, group)
        logmeans <- lapply(groups, function(x) log(x) - sum(colMeans(log(x), ...)))
        return(unsplit(logmeans, group))
}	}


norm.wattfabricius <- function(f, vowel, group=NULL) {
    if (ncol(f) != 2) {
        warning("Wrong dimensions: s-centroid normalization requires ",
                "an Nx2 matrix or data frame of F1 and F2 values.")
    }
    if (is.null(group)) group <- rep('g', nrow(f))
    subsets <- by(f, list(vowel, group), identity)  # 2D list (group x vowel) of lists (f1,f2)
    means <- matrix(sapply(subsets, function(i) ifelse(is.null(i),
                                                    data.frame(I(c(f1=NA, f2=NA))),
                                                    data.frame(I(colMeans(x))))
                        ), nrow=nrow(subsets), dimnames=dimnames(subsets))
    # TODO: check whether this works with diphthongs
    minima <- apply(means, 2, function(i) data.frame(t(apply(do.call(rbind, i), 2, min, na.rm=TRUE))))
    maxima <- apply(means, 2, function(i) data.frame(t(apply(do.call(rbind, i), 2, max, na.rm=TRUE))))
    min.id <- apply(means, 2, function(i) apply(do.call(rbind,i), 2, which.min))
    max.id <- apply(means, 2, function(i) apply(do.call(rbind,i), 2, which.max))
    if (length(unique(min.id['f1',]))>1) {
        warning('The vowel with the lowest mean F1 value (usually /i/)',
                ' does not match across all speakers/groups. You\'ll ',
                'have to calculate s-centroid manually.')
        print(data.frame(minF1=minima['f1',],
                vowel=dimnames(means)[[1]][min.id['f1',]],
                group=dimnames(means)[[2]]))
        stop()
    } else if (length(unique(max.id['f1',]))>1) {
        warning('The vowel with the highest mean F1 value (usually /a/)',
                ' does not match across all speakers/groups. You\'ll ',
                'have to calculate s-centroid manually.')
        print(data.frame(maxF1=round(maxima['f1',]),
                vowel=dimnames(means)[[1]][max.id['f1',]],
                group=dimnames(means)[[2]]))
        stop()
    }
    lowvowf2 <- do.call(rbind, means.list[unique(max.id['f1',]),])[,'f2']
    centroids <- rbind(f1=(2*minima['f1',] + maxima['f1',])/3, f2=(minima['f2',] + maxima['f2',] + lowvowf2)/3)
    rnames <- rep(rownames(subsets),times=ncol(subsets))
    cnames <- rep(colnames(subsets),each=nrow(subsets))
    f/t(centroids[,group])
}


# pineda's triangle filling algorithm
fillTriangle <- function(x, y, vertices) {
    x0 <- vertices[1,1]
    x1 <- vertices[2,1]
    x2 <- vertices[3,1]
    y0 <- vertices[1,2]
    y1 <- vertices[2,2]
    y2 <- vertices[3,2]
    z0 <- vertices[1,3]
    z1 <- vertices[2,3]
    z2 <- vertices[3,3]
    e0xy <- (x-x0)*(y1-y0)-(y-y0)*(x1-x0)
    e1xy <- (x-x1)*(y2-y1)-(y-y1)*(x2-x1)
    e2xy <- (x-x2)*(y0-y2)-(y-y2)*(x0-x2)
    e0x2 <- (x2-x0)*(y1-y0)-(y2-y0)*(x1-x0)
    e1x0 <- (x0-x1)*(y2-y1)-(y0-y1)*(x2-x1)
    e2x1 <- (x1-x2)*(y0-y2)-(y1-y2)*(x0-x2)
    f0 <- e0xy / e0x2
    f1 <- e1xy / e1x0
    f2 <- e2xy / e2x1
    z <- f0*z2 + f1*z0 + f2*z1
}


repulsiveForceHeatmap <- function(f2, f1, z, vowel=NULL, resolution=50,
                        colormap=NULL, method='default', add=TRUE, ...) {
    require(splancs)  # provides inpip()
    require(deldir)   # provides deldir() and triMat()
    require(plotrix)  # provides color.scale()
    # default to grayscale
    if (is.null(colormap)) colormap <- color.scale(x=0:100, cs1=0, cs2=0,
                                    cs3=c(25,100), alpha=1, color.spec='hcl')
    # create grid encompassing vowel space
    vertices <- data.frame(x=f2, y=f1, z=z, v=vowel)
    vertices <- vertices[!is.na(vertices$x) & !is.na(vertices$y),]
    bounding.rect <- apply(vertices[c('x', 'y')], 2, range, na.rm=TRUE)
    xr <- abs(diff(bounding.rect[,'x']))
    yr <- abs(diff(bounding.rect[,'y']))
    if (xr > yr) {
        xres <- round(resolution * xr / yr)
        yres <- resolution
    } else {
        xres <- resolution
        yres <- round(resolution * yr / xr)
    }
    xgrid <- seq(floor(bounding.rect[1,1]), ceiling(bounding.rect[2,1]), length.out=xres)
    ygrid <- seq(floor(bounding.rect[1,2]), ceiling(bounding.rect[2,2]), length.out=yres)
    grid <- expand.grid(x=xgrid, y=ygrid)
    grid$z <- NA
    # create delaunay triangulation of vowels
    triangs <- with(vertices, triMat(deldir(x, y, suppressMsge=TRUE)))
    triangs <- apply(triangs, 1, function(i) data.frame(x=vertices$x[i], y=vertices$y[i], z=vertices$z[i]))
    # which grid points are inside the vowel space
    grid.indices <- lapply(triangs, function(i) inpip(grid, i, bound=FALSE))
    if (method %in% 'pineda') {
        grid.values <- lapply(seq_along(triangs),
            function(i) fillTriangle(grid[grid.indices[[i]],1],
            grid[grid.indices[[i]],2], triangs[[i]]))
        grid.indices <- do.call(c, grid.indices)
        grid.values <- do.call(c, grid.values)
        grid$z[grid.indices] <- grid.values
        image(xgrid, ygrid, matrix(grid$z, nrow=length(xgrid)),
            col=colormap, add=add)
    } else {
        if (is.null(vowel)) stop('Default method requires non-null values for "vowel".')
        grid.force <- rep(NA, nrow(grid))
        hull <- vertices[chull(vertices),]  # polygon of hull
        subgrid <- grid[inpip(grid[,1:2], hull),]
        # which vowel is closest to each grid point?
        dist.matrix <- apply(subgrid, 1, function(i) apply(as.matrix(vertices[c('x','y')]), 1, function(j) sqrt(sum((j[1:2] - i[1:2])^2))))
        indices <- apply(dist.matrix, 2, function(i) which(i == min(i)))
        indices <- sapply(indices, function(i) i[1])  # if there is a tie of which vowel is closest, pick first one (arbitrarily)
        subgrid.nearest.vowel <- vertices$v[indices]
        subgrid.force <- sapply(seq_along(subgrid.nearest.vowel), function(i) sum(1/dist.matrix[!(vertices$v %in% subgrid.nearest.vowel[i]),i]^2))
        grid.force[inpip(grid[,1:2], hull)] <- log10(subgrid.force)
        image(xgrid, ygrid, matrix(grid.force, nrow=length(xgrid)), col=colormap, add=add, ...)
}	}


forceHeatmapLegend <- function (x, y, smoothness=50, colormap=NULL, lend=2,
                                lwd=6, ...) {
    require(plotrix)
    if (is.null(colormap)) {  # default to grayscale
        colormap <- color.scale(x=0:100, cs1=0, cs2=0, cs3=c(0,100),
                                alpha=1, color.spec='hcl')
    }
    xvals <- seq(x[1], x[2], length.out=smoothness)
    yvals <- seq(y[1], y[2], length.out=smoothness)
    invisible(color.scale.lines(xvals, yvals, col=colormap, lend=lend, lwd=lwd,
                                ...))
}


makeTransparent <- function (opaque.color, opacity) {
    rgba <- t(col2rgb(opaque.color, alpha=TRUE))
    rgba[,4] <- round(255 * opacity)
    colnames(rgba) <- c("red", "green", "blue", "alpha")
    trans.color <- do.call(rgb, c(as.data.frame(rgba), maxColorValue=255))
}


vowelMeansPolygonArea <- function(f1, f2, vowel, talker) {
    df <- data.frame(f1=f1, f2=f2, v=vowel, t=talker)
    bytalker <- as.table(by(df, df$t,
                            function(x) areapl(cbind(tapply(x$f2, x$v, mean),
                                                    tapply(x$f1, x$v, mean)))))
    area <- bytalker[df$t]
}

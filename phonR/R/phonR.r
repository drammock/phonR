# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# phonR version 1.0-0
# Functions for phoneticians and phonologists using R
# AUTHOR: Daniel McCloy, drmccloy@uw.edu
# LICENSED UNDER THE GNU GENERAL PUBLIC LICENSE v3.0:
# http://www.gnu.org/licenses/gpl.html
# DEVELOPMENT OF THIS PACKAGE WAS FUNDED IN PART BY NIH-R01DC006014
#
# CHANGELOG:
# v1.0: major refactor of the entire codebase.
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
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# USAGE: source("phonR.r")
# --or-- from command line (replace Xs with version number): 
# R CMD install phonR_X.X-X.tar.gz
# Then library(phonR)
# Then call functions as needed

plotvowels <- function(f1, f2, vowel=NULL, group=NULL,
	polygon=NA, poly.fill=FALSE, 
	hull.line=NULL, hull.fill=NULL, 
	ellipse.line=NULL, ellipse.fill=NULL, ellipse.conf=0.3173, 
	plot.tokens=TRUE, plot.means=FALSE, 
	pch.tokens=NULL, pch.means=NULL, 
	cex.tokens=NULL, cex.means=NULL, 
	col.by=NA, style.by=NA, axis.labels=NULL, pretty=FALSE,  
	output='screen', units=NULL, ...) 
{
	# XXX TODO 
	# add heatmap function
	# support for diphthongs
	# re-integrate normalization functions
	# XXX TODO 
	# # # # # # # # # #
	# OUTPUT PARSING  #
	# # # # # # # # # #
	output <- tolower(output)
	if (output=='jpeg') output <- 'jpg'
	if (output=='tiff') output <- 'tif'
	output.types <- c('pdf','svg','jpg','tif','png','bmp','screen')
	output.raster <- c('jpg','tif','png','bmp','screen')
	if (!(output %in% output.types)) {
		warning('Unknown argument value \'', output, '\': \'output\' ',
		'must be one of \'pdf\', \'svg\', \'png\', \'tif\', \'bmp\', ',
		'\'jpg\', or \'screen\'. Using default (\'screen\').')
		output <- 'screen'
	}
	if(!(is.null(units))) {
		if (!(units %in% c('in','cm','mm','px'))) {
		  warning('Unknown argument value \'', units, '\': \'units\' ',
		  	'must be one of \'in\', \'cm\', \'mm\', or \'px\'. Using ',
		  	'default (\'in\').')
		  units <- 'in'
		}
		if (output %in% c('pdf','svg','screen')) {
			if (units=='cm') {
				width <- width/2.54
				height <- height/2.54
			} else if (units=='mm') {
				width <- width/2540
				height <- height/2540
			} else if (units=='px') {
				width <- width/72
				height <- height/72
			}		
		}
	}
	
	l <- length(vowel)
	if(is.null(group)) gf <- rep('gf', l)
	else 			   gf <- as.numeric(factor(group))
	# used later to set default polygon color when color varies by vowel
	if(identical(col.by, vowel)) col.by.vowel <- TRUE
	else                         col.by.vowel <- FALSE
	# # # # # # # # # # #
	# HANDLE OTHER ARGS #
	# # # # # # # # # # #
	args <- list(...)
	if(output == 'screen') {
		# args only settable by direct par() call (not via plot(), etc)
		# not strictly true for "family" but works better this way
		par.only <- c("ask", "fig", "fin", "lheight", "mai", "mar", 
			"mex", "mfcol", "mfrow", "mfg", "new", "oma", "omd", "omi", 
			"pin", "plt", "ps", "pty", "usr", "xlog", "ylog", "ylbias",
			"family")
	} else {
		par.only <- c("ask", "fig", "fin", "lheight", "mai", "mar", 
			"mex", "mfcol", "mfrow", "mfg", "new", "oma", "omd", "omi", 
			"pin", "plt", "ps", "pty", "usr", "xlog", "ylog", "ylbias")
		file.only <- c("filename", "width", "height", "units", 
			"pointsize", "res", "quality", "compression", "family")
		file.args <- args[names(args) %in% file.only]
		args <- args[!(names(args) %in% file.only)]
	}
	par.args <- args[names(args) %in% par.only]
	args <- args[!(names(args) %in% par.only)]
	# LET USERS OVERRIDE "PRETTY" SETTINGS
	pretty.par <- list(mar=c(1,1,4,4))
	pretty.par[names(par.args)] <- par.args
	pretty.args <- list(mgp=c(2,0.5,0), las=1, xaxs='i', yaxs='i', 
				   ann=FALSE, fg=hcl(0,0,40), tcl=-0.25, xpd=NA)
	pretty.args[names(args)] <- args
	# OUTPUT DEVICES
	if(output=='pdf')  do.call(cairo_pdf, file.args)
	else if(output=='svg') do.call(svg, file.args)
	else if(output=='jpg') do.call(jpeg, file.args)
	else if(output=='tif') do.call(tiff, file.args)
	else if(output=='png') do.call(png, file.args)
	else if(output=='bmp') do.call(bmp, file.args)
	# INITIAL CALL TO PAR() 
	if(pretty) op <- par(pretty.par)
	else       op <- par(par.args)
	# # # # # # #
	# DEFAULTS  #
	# # # # # # #
	# color.by & style.by
	if(is.na(col.by))     col.by <- rep(1, l)  # default to black
	else                  col.by <- as.numeric(factor(col.by))
	if(is.na(style.by)) style.by <- rep(1, l)  # default to solid
	else                style.by <- as.numeric(factor(style.by))
	num.col <- length(unique(col.by))
	num.sty <- length(unique(style.by))
	# misc. plotting defaults
	if(is.null(axis.labels))  axis.labels <- c('F2','F1')
	if(is.null(cex.tokens))    cex.tokens <- par('cex')
	if(is.null(cex.means))      cex.means <- par('cex')
	# plotting characters
	if(!('pch' %in% names(args))) {
		# TODO: reconcile this with pch.tokens and pch.means
		# filled / open {circ,tri,squ,diam}, plus, x, inverted open tri
		if(pretty) args$pch <- rep(c(16,1,17,2,15,0,18,5,3,4,6), 
							   length.out=l)[style.by]
		else       args$pch <- style.by
	}
	if(is.null(pch.tokens)) pch.tokens <- args$pch
	if(is.null(pch.means))   pch.means <- args$pch
	# linetypes
	if(!('lty' %in% names(args))) {
		if(pretty) args$lty <- c('solid', '44', 'F4', '4313',
								 'F3131313', '23F3', '232923',
								 '23258385', '282823B3', '13', 
								 '82')[style.by]
		else       args$lty <- style.by
	}
	# colors
	if(!('col' %in% names(args))) {
		if(pretty) {
			# if no colors specified, use equally spaced HCL values
			# [-1] avoids duplicate hues 0 and 360
			hue <- seq(0,360,length.out=1+num.col)[-1]
			chr <- seq(60,100,length.out=num.col)
			lum <- seq(60,40,length.out=num.col)
			args$col <- hcl(hue, chr, lum, alpha=1)[col.by]
		} else {
			args$col <- col.by
		}
	}
	# ellipse fill colors
	if(!(is.null(ellipse.line) && is.null(ellipse.fill))) {
		if(pretty) ellipse.col <- hcl(hue, chr, lum, alpha=0.3)
		else       ellipse.col <- rep('#00000099', num.col)
	} else {
		ellipse.col <- NA
	}
	ellipse.col <- ellipse.col[col.by]

	# # # # # # # # # # # # # #
	# COLLECT IMPORTANT STUFF #
	# # # # # # # # # # # # # #
	d <- data.frame(f2=f2, f1=f1, v=vowel, gf=factor(gf), 
		m=rep(plot.means, l), color=args$col, style=style.by, 
		ellipse.col=ellipse.col)
	dd <- by(d, d[c('v','gf')], identity)	
	# CALCULATE VOWEL COVARIANCES (the following line still may yield
	# some NA cov. matrices, due to some vowels having only 1 token)
	s <- lapply(dd, function(i) if(!(is.null(i))) 
		 with(i[!(is.na(i$f2)) && !(is.na(i$f1)),], 
		 list(cov(cbind(f2, f1)))))
	s <- do.call(rbind, s)
	# CALCULATE VOWEL MEANS
	mu <- lapply(dd, function(i) if(!(is.null(i))) 
		  with(i[!(is.na(i$f2)) && !(is.na(i$f1)),], 
		  list(colMeans(cbind(f2, f1)))))
	mu <- do.call(rbind, mu)
	# COLLECT INTO DATAFRAME
	m <- lapply(dd, function(i) if(!(is.null(i))) 
		 data.frame(f2=mean(i$f2, na.rm=TRUE), 
		 f1=mean(i$f1, na.rm=TRUE), v=unique(i$v), gf=unique(i$gf), 
		 m=unique(i$m), color=unique(i$color), style=unique(i$style), 
		 ellipse.col=unique(i$ellipse.col)))
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
	if(!('xlim' %in% names(args))) args$xlim <- rev(plot.bounds[,1])
	if(!('ylim' %in% names(args))) args$ylim <- rev(plot.bounds[,2])

	# FONT HANDLING FOR WINDOWS (RELATED BLOCK AT END OF SCRIPT)
	is.win <- .Platform$OS.type == 'windows'
	if (is.win && 'family' %in% names(args) && output %in% output.raster) {
			oldFont <- windowsFonts()$sans
			windowsFonts(sans=windowsFont(args$family))
			args$family <- 'sans'
			if (output=='screen') warning('Font specification may fail',
				' if saving as PDF from onscreen plot window menu. To ',
				'ensure PDF font fidelity, run plotvowels() with ',
				'output="pdf".')
	}
	if(pretty) {
		xticks <- prettyticks(args$xlim)
		yticks <- prettyticks(args$ylim)
		args$xlim <- rev(range(xticks))
		args$ylim <- rev(range(yticks))
		do.call(plot, as.list(c(list(0, 0, type='n', frame.plot=FALSE, 
				axes=FALSE), args)))
		axis(3, at=xticks, col.axis=par('fg'))
		axis(4, at=yticks, col.axis=par('fg'))
		# EXTEND THE AXIS LINES TO MEET AT THE CORNER AS NEEDED 
		if(args$xlim[2] != par('usr')[2]) axis(3, at=c(args$xlim[2], 
										  par('usr')[2]), labels=FALSE, 
										  col=par('fg'), tcl=0) 
		if(args$ylim[2] != par('usr')[4]) axis(4, at=c(args$ylim[2], 
										  par('usr')[4]), labels=FALSE, 
										  col=par('fg'), tcl=0) 
		# AXIS LABELS
		mtext(axis.labels[1], side=3, line=2, col=par('fg')) 
		mtext(axis.labels[2], side=4, line=2.5, col=par('fg'), las=3) 
	} else {
		do.call(plot, as.list(c(list(0, 0, type='n', ann=FALSE), args)))
	}
	# # # # # # # # #
	# PLOT ELLIPSES #
	# # # # # # # # #
	if (!(is.null(ellipse.fill) && is.null(ellipse.line))) {
		if (is.null(ellipse.line)) lapply(seq_along(ellipse.points), 
								   function(i) polygon(ellipse.points[[i]], 
								   col=m$ellipse.col[i], border=NA))
		else if (is.null(ellipse.fill)) lapply(seq_along(ellipse.points), 
										function(i) polygon(ellipse.points[[i]], 
										col=NA, border=m$color[i], lty=m$style[i]))
		else lapply(seq_along(ellipse.points), 
			 function(i) polygon(ellipse.points[[i]], 
			 col=m$ellipse.col[i], border=m$color[i], lty=m$style[i]))
	}
	# # # # # # # # #
	# PLOT POLYGONS #
	# # # # # # # # #
	if(is.character(polygon)) {
		if(length(polygon) != length(unique(polygon))) warning(
			'Duplicate entries in "polygon" detected; they will be ignored.')
		polygon <- unique(as.character(polygon)) # as.character in case factor
		v <- unique(as.character(m$v))
		if (length(setdiff(polygon, v)) > 0) {
			warning('There are vowels in "polygon" that are not in ',
					'"vowel"; they will be ignored.')
			polygon <- intersect(polygon, v)
		}
		n <- m
		if(col.by.vowel) n$color <- par('fg')
		n$v <- factor(n$v, levels=polygon)
		n <- n[order(n$v),]
		n <- split(n, n$gf)
		invisible(lapply(n, function(i) with(i[i$v %in% polygon,], 
				  points(f2, f1, col=color, type='c', 
				  cex=1.25*cex.means, lty=style))))
	}
	# PLOT TOKENS
	if(plot.tokens) {
		if(is.null(pch.tokens)) pch.tokens <- as.numeric(d$gf)
		with(d, points(f2, f1, col=color, pch=pch.tokens, cex=cex.tokens))
	}
	# PLOT MEANS
	if(plot.means) {
		if(is.null(pch.means)) pch.means <- as.numeric(factor(colnames(m)))
		with(m, points(f2, f1, col=color, pch=pch.means, cex=cex.means))
	}
	# CLOSE FILE DEVICES
	if(output != 'screen') dev.off()
	# RESET GRAPHICAL PARAMETERS
	#par(op)
	# RESET FONT HANDLING FOR WINDOWS
	if (is.win && output %in% output.raster) windowsFonts(sans=oldFont)
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

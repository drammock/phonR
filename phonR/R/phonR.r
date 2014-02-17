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

plot.vowels <- function(f1, f2, vowel=NULL, group=NULL,
	polygon=NA, poly.fill=FALSE, 
	hull.line=NULL, hull.fill=NULL, force.heatmap=FALSE, 
	force.colmap=NULL, force.res=50, force.method='default',
	ellipse.line=NULL, ellipse.fill=NULL, ellipse.conf=0.3173, 
	plot.tokens=TRUE, plot.means=FALSE, 
	pch.tokens=NULL, pch.means=NULL, 
	cex.tokens=NULL, cex.means=NULL, 
	col.by=NA, style.by=NA, axis.labels=NULL, pretty=FALSE,  
	output='screen', units=NULL, ...) 
{
	# XXX TODO 
	# add hull
	# support for diphthongs
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
				'ensure PDF font fidelity, run plot.vowels() with ',
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
	# PLOT HEATMAP  #
	# # # # # # # # #
	if(force.heatmap) {
		force <- with(d, repulsive.force(f2, f1, v))
		with(d, force.heatmap(f2, f1, force, vowel=v, resolution=force.res, 
							  colormap=force.colmap, method=force.method, add=TRUE))
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


repulsive.force <- function(f2, f1, vowel) {
	dmat <- as.matrix(dist(cbind(f2,f1)))
	force <- sapply(seq_along(vowel), 
			 function(i) sum(1/dmat[i, !(vowel %in% vowel[i])]^2))
}


# OMNIBUS NORMALIZATION FUNCTION (convenience function)
norm.vowels <- function(method, f0=NULL, f1=NULL, f2=NULL, f3=NULL, 
					   vowel=NULL, group=NULL) {
	m <- tolower(method)
	methods <- c('bark','mel','log','erb','z','zscore','lobanov',
				 'logmean','nearey1','nearey2','scentroid','s-centroid',
				 's','wattfabricius','watt-fabricius')
	if(!(m %in% methods)) {
		warning('Method must be one of: bark, mel, log, erb, ',
				'z|zscore|lobanov, logmean|nearey1, nearey2, ',
				's|scentroid|s-centroid|wattfabricius|watt-fabricius.')
	}
	f <- as.matrix(cbind(f0=f0, f1=f1, f2=f2, f3=f3))
	if (m %in% 'bark') return(norm.bark(f))
	else if (m %in% 'mel') return(norm.mel(f))
	else if (m %in% 'log') return(norm.log(f))
	else if (m %in% 'erb') return(norm.erb(f))
	else if (m %in% c('z','zscore','lobanov')) return(norm.lobanov(f, group))
	else if (m %in% c('logmean','nearey1')) return(norm.logmean(f, group))
	else if (m %in% c('nearey','nearey2')) return(norm.nearey(f, group))
	else {
		f <- as.matrix(cbind(f1=f1, f2=f2))
		return(norm.wattfabricius(f, vowel, group))
}	}


# INDIVIDUAL NORMALIZATION FUNCTIONS
norm.bark <- function(f) {
	f <- as.matrix(f)
	26.81*f/(1960+f)-0.53
}


norm.log <- function(f) {
	f <- as.matrix(f)
	log10(f)
}


norm.mel <- function(f) {
	f <- as.matrix(f)
	2595*log10(1+f/700)
}


norm.erb <- function(f) {
	f <- as.matrix(f)
	21.4*log10(1+0.00437*f)
}


norm.lobanov <- function(f, group=NULL) {
	f <- as.matrix(f)
	if (is.null(group)) {
		return(as.matrix(as.data.frame(scale(f))))
	} else {
		groups <- split(f, group)
		scaled <- lapply(groups, function(x) as.data.frame(scale(x)))
		return(as.matrix(unsplit(scaled, group)))
}	}


norm.logmean <- function(f, group=NULL) {
	f <- as.matrix(f)
	if (is.null(group)) {
		return(log(f) - rep(colMeans(log(f)), each=nrow(f)))
	} else {
		groups <- split(f, group)
		logmeans <- lapply(groups, 
					function(x) log(x) - rep(apply(log(x), 2, mean), 
					each=nrow(x)))
		return(as.matrix(unsplit(logmeans, group)))
}	}


norm.nearey <- function() {
	f <- as.matrix(f)
	if (ncol(f) != 4) {
		stop('Missing values: normalization method \'nearey2\' ',
			 'requires non-null values for all arguments (f0, f1, f2, ',
			 'and f3).')
	}
	if (is.null(group)) {
		return(log(f) - sum(colMeans(log(f))))
	} else {
		groups <- split(f, group)
		logmeans <- lapply(groups, function(x) log(x) - sum(colMeans(log(x))))
		return(as.matrix(unsplit(logmeans, group)))
}	}


norm.wattfabricius <- function(f, vowel, group=NULL) {
	f <- as.matrix(f)
	if (ncol(f) != 2) {
		warning('Wrong dimensions: s-centroid normalization requires ',
				'an Nx2 matrix or data frame of F1 and F2 values.')
	}
	if(is.null(group)) group <- rep('g', nrow(f))
	subsets <- by(f, list(vowel, group), identity)  # 2D list (group x vowel) of lists (f1,f2)
	means <- matrix(lapply(subsets, colMeans), ncol=ncol(subsets), dimnames=dimnames(subsets))
	minima <- apply(means, 2, function(i) apply(do.call(rbind,i), 2, min)) # TODO: bug here when using diphthongs
	maxima <- apply(means, 2, function(i) apply(do.call(rbind,i), 2, max)) 
	min.id <- apply(means, 2, function(i) apply(do.call(rbind,i), 2, which.min))
	max.id <- apply(means, 2, function(i) apply(do.call(rbind,i), 2, which.max))
	if (length(unique(min.id['f1',]))>1) {
		warning('The vowel with the lowest mean F1 value (usually /i/)',
				' does not match across all speakers/groups. You\'ll ',
				'have to calculate s-centroid manually.')
		data.frame(minF1=minima['f1',], 
				   vowel=dimnames(means)[[1]][min.id['f1',]],
				   group=dimnames(means)[[2]])
		stop()
	} else if (length(unique(max.id['f1',]))>1) {
		warning('The vowel with the highest mean F1 value (usually /a/)',
				' does not match across all speakers/groups. You\'ll ',
				'have to calculate s-centroid manually.')
		data.frame(maxF1=round(maxima['f1',]), 
				   vowel=dimnames(means)[[1]][max.id['f1',]],
				   group=dimnames(means)[[2]])
		stop()
	}
	lowvowf2 <- do.call(rbind, means.list[unique(max.id['f1',]),])[,'f2']
	centroids <- rbind(f1=(2*minima['f1',] + maxima['f1',])/3, f2=(minima['f2',] + maxima['f2',] + lowvowf2)/3)
	rnames <- rep(rownames(subsets),times=ncol(subsets))
	cnames <- rep(colnames(subsets),each=nrow(subsets))
	f/t(centroids[,group])
}


# pineda's triangle filling algorithm
fill.triangle <- function(x, y, vertices) {
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


force.heatmap <- function(f2, f1, z, vowel=NULL, resolution=50, 
						  colormap=NULL, method='default', add=TRUE, ...) {
	require(splancs)  # provides inpip()
	require(deldir)   # provides deldir() and triMat() 
	require(plotrix)  # provides color.scale()
	# default to grayscale
	if(is.null(colormap)) colormap <- color.scale(x=0:100, cs1=0, cs2=0, 
									  cs3=c(25,100), alpha=1, color.spec='hcl')
	# create grid encompassing vowel space
	vertices <- data.frame(x=f2, y=f1, z=z, v=vowel)
	vertices <- vertices[!is.na(vertices$x) & !is.na(vertices$y),]
	bounding.rect <- apply(vertices[c('x', 'y')], 2, range, na.rm=TRUE)
	xr <- abs(diff(bounding.rect[,'x']))
	yr <- abs(diff(bounding.rect[,'y']))
	if(xr > yr) {
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
			function(i) fill.triangle(grid[grid.indices[[i]],1], 
			grid[grid.indices[[i]],2], triangs[[i]]))
		grid.indices <- do.call(c, grid.indices)
		grid.values <- do.call(c, grid.values)
		grid$z[grid.indices] <- grid.values
		image(xgrid, ygrid, matrix(grid$z, nrow=length(xgrid)), 
			  col=colormap, add=add)
	} else {
		if(is.null(vowel)) stop('Default method requires non-null values for "vowel".')
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


vowelHeatmapLegend <- function (x, y, smoothness=50, alpha=1, colormap=NULL, lend=2, lwd=6, ...) {
	require(plotrix)
	if(is.null(colormap)) {  # default to grayscale
		colormap <- color.scale(x=0:100, cs1=0, cs2=0, cs3=c(0,100), alpha=1, color.spec='hcl')
	}
	xvals <- seq(x[1], x[2], length.out=smoothness)
	yvals <- seq(y[1], y[2], length.out=smoothness)
	invisible(color.scale.lines(xvals, yvals, col=colormap, lend=lend, lwd=lwd, ...))
}


# TODO: integrate these four
vowelMeansPolygonArea <- function(f1, f2, vowel, talker) {
	df <- data.frame(f1=f1, f2=f2, v=vowel, t=talker)
	bytalker <- as.table(by(df, df$t, function(x) areapl(cbind(tapply(x$f2, x$v, mean), tapply(x$f1, x$v, mean)))))
	area <- bytalker[df$t]
	return(area)
}

convexHull <- function(f1, f2, talker) {
	df <- data.frame(f1=f1, f2=f2, t=talker)
	bytalkerpts <- by(df, df$t, function(x) x[chull(x$f2, x$f1),c('f2','f1')])
	bytalkerarea <- sapply(bytalkerpts, function(i) areapl(as.matrix(data.frame(x=i$f2, y=i$f1))))
	area <- bytalkerarea[df$t]
	return(area)
}


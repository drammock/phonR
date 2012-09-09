# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# phonR version 0.2
# Functions for phoneticians and phonologists using R
# Daniel McCloy, drmccloy@uw.edu
# LICENSED UNDER THE GNU GENERAL PUBLIC LICENSE v3.0: http://www.gnu.org/licenses/gpl.html
# DEVELOPMENT OF THIS PACKAGE WAS FUNDED IN PART BY THE NATIONAL INSTITUTES OF HEALTH, GRANT NUMBER R01DC006014 TO PAMELA SOUZA
#
# CHANGELOG:
# v0.2 bugfixes: points.alpha and means.alpha now work for grayscale plots. Plots with polygons or ellipses but no shapes now get proper legend type (lines, not boxes). Graphical parameters now captured and restored when plotting to onscreen device. Vowels with no variance (e.g., single tokens) no longer crash ellipse function. Vowels not in default poly.order() no longer go unplotted when points='text'. 
# v0.2 enhancements: support for custom axis titles (to accommodate pre-normalized values), point and mean sizes, and fonts. Custom line types added (11 total now).
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# USAGE: source("phonR.r")
# --or-- R CMD install phonR_0.1.tar.gz (from command line)
# Then library(phonR)
# Then call functions as needed

# NORMALIZATION FUNCTION
normalizeVowels <- function(method, f0=NULL, f1=NULL, f2=NULL, f3=NULL, vowel=NULL, grouping.factor=NULL) {
  m <- tolower(method)
  f <- cbind(f0=as.vector(f0),f1=as.vector(f1),f2=as.vector(f2),f3=as.vector(f3))
  if (is.null(f)) {
    warning('Missing values: at least one of the arguments (f0, f1, f2, or f3) must be supplied.')
  }
  if (!(m %in% c('bark','mel','log','erb','z','zscore','ztransform','lobanov','logmean','nearey1','nearey2','scentroid','s','wattfabricius'))) {
    warning('Method must be one of: bark, mel, log, erb, z|zscore|ztransform|lobanov, logmean|nearey1, nearey2, s|scentroid|wattfabricius')
  }
  if (!is.null(grouping.factor) & !(m %in% c('z','zscore','ztransform','lobanov','logmean','nearey1','nearey2','s','scentroid','wattfabricius'))) {
    warning('Argument \"grouping.factor\" is ignored for normalization method \"',method,'.\"')
  }
  if (!is.null(vowel) & !(m %in% c('s','scentroid','wattfabricius'))) {
    warning('Argument \"vowel\" is ignored for normalization method \"',method,'.\"')
  }
  if ((!is.null(f3) | !is.null(f0)) & !(m %in% c('s','scentroid','wattfabricius'))) {
    warning('f0 and F3 values ignored for s-centroid normalization method. Only F1 and F2 values returned.')
    f <- cbind(as.vector(f1),as.vector(f2))
  }
  if (is.null(grouping.factor)) { grouping.factor <- 'noGroups' }
  if (m=='bark') {
    fn <- 26.81*f/(1960+f)-0.53
  } else if (m=='log') {
    fn <- log10(f)
  } else if (m=='mel') {
    fn <- 2595*log10(1+f/700)
  } else if (m=='erb') {
    fn <- 21.4*log10(0.00437*f+1)
  } else if (m %in% c('s','scentroid','wattfabricius')) {
    subsets <- by(f, list(grouping.factor,vowel), subset) # 2D list (group x vowel) of lists (f1,f2)
    means.list <- matrix(lapply(subsets, function(x){apply(x,2,mean)}), ncol=ncol(subsets), dimnames=dimnames(subsets))
    f1maxima <- apply(means.list,1,function(x){max(matrix(unlist(x),ncol=2,byrow=TRUE)[,1])})
    f1minima <- apply(means.list,1,function(x){min(matrix(unlist(x),ncol=2,byrow=TRUE)[,1])})
    f1max.id <- apply(means.list,1,function(x){which.max(matrix(unlist(x),ncol=2,byrow=TRUE)[,1])})
    f1min.id <- apply(means.list,1,function(x){which.min(matrix(unlist(x),ncol=2,byrow=TRUE)[,1])})
    if (length(unique(f1min.id))>1) {
      warning('The vowel with the lowest mean F1 value (usually /i/) does not match across all speakers/groups. You\'ll have to calculate s-centroid by hand.')
      data.frame(minF1=round(f1minima), vowel=dimnames(means.list)[[2]][f1min.id])
      stop()
    }
    if (length(unique(f1max.id))>1) {
      warning('The vowel with the highest mean F1 value (usually /a/) does not match across all speakers/groups. You\'ll have to calculate s-centroid by hand.')
      data.frame(maxF1=round(f1maxima), vowel=dimnames(means.list)[[2]][f1max.id])
      stop()
    }
    f2lowvow <- apply(means.list,1,function(x){matrix(unlist(x),ncol=2,byrow=TRUE)[unique(f1max.id),2]})
    f2maxima <- apply(means.list,1,function(x){matrix(unlist(x),ncol=2,byrow=TRUE)[unique(f1min.id),2]})
    f2minima <- f1minima
    f1.centroid <- (2*f1minima+f1maxima)/3
    f2.centroid <- (f2minima+f2maxima+f2lowvow)/3
    centroid <- cbind(apply(as.matrix(grouping.factor),1,function(x){f1.centroid[x]}), apply(as.matrix(grouping.factor),1,function(x){f2.centroid[x]}))
    fn <- f/centroid
  } else if (grouping.factor[1]=='noGroups') {
    if (m %in% c('lobanov','ztransform','zscore','z')) {
      fn <- (f - rep(apply(f,2,mean),each=nrow(f))) / rep(apply(f,2,sd),each=nrow(f))
    } else if (m %in% c('nearey1','logmean')) {
      fn <- log(f) - rep(apply(log(f),2,mean),each=nrow(f))
    } else if (m=='nearey2') {
      if (ncol(f) == 4) {
        fn <- log(f) - sum(apply(log(f),2,mean))
      } else {
        warning('Missing values: normalization method \'nearey2\' requires non-null values for all arguments (f0, f1, f2, and f3).')
        stop()
      }
    }
  } else {
    subsets <- lapply(by(f, grouping.factor, subset),as.matrix) # list of matrices
    if (m %in% c('lobanov','ztransform','zscore','z')) {
      means.list <- lapply(subsets, function(x){apply(x,2,mean)})
      stdev.list <- lapply(subsets, function(x){apply(x,2,sd)})
      means.matrix <- t(apply(as.matrix(grouping.factor),1,function(x){means.list[[x]]}))
      stdev.matrix <- t(apply(as.matrix(grouping.factor),1,function(x){stdev.list[[x]]}))
      fn <- (f - means.matrix) / stdev.matrix
    } else if (m %in% c('nearey1','logmean')) {
      logmeans.list <- lapply(subsets, function(x){apply(log(x),2,mean)})
      logmeans.matrix <- t(apply(as.matrix(grouping.factor),1,function(x){logmeans.list[[x]]}))
      fn <- log(f) - logmeans.matrix
    } else if (m=='nearey2') {
      logmeans.list <- lapply(subsets, function(x){apply(log(x),2,mean)})
      logmeans.matrix <- t(apply(as.matrix(grouping.factor),1,function(x){logmeans.list[[x]]}))
      if (ncol(f) == 4) {
        fn <- log(f) - matrix(rep(apply(logmeans.matrix,1,sum),4),ncol=4)
      } else {
        warning('Missing values: normalization method \'nearey2\' requires non-null values for all arguments (f0, f1, f2, and f3).')
        stop()
      }
    }
  }
  return(fn)
}

# VOWEL PLOTTING FUNCTION
plotVowels <- function(vowel, f1, f2, f3=NULL, f0=NULL, grouping.factor=NULL, data=NULL, norm.method='none', match.unit=TRUE, match.axes='absolute', points='text', means='text', points.alpha=0.5, means.alpha=1, points.cex=0.6, means.cex=1.2, ignore.hidden=TRUE, ellipses=TRUE, ellipse.alpha=0.3173, polygon=TRUE, poly.order=NULL, poly.include=NULL, single.plot=TRUE, axis.col='#666666FF', titles='auto', axis.titles='auto', font.family='', grayscale=FALSE, vary.shapes=grayscale, vary.lines=grayscale, uniform.style=!single.plot, legend=single.plot, aspect.ratio=NULL, plot.dims=c(6.5,6.5), plot.unit='in', output='screen') {
	# MAKE CASE-INSENSITIVE, CHECK FOR BOGUS ARGUMENTS, ETC
	norm.method <- tolower(norm.method)
	match.axes <- tolower(match.axes)
	output <- tolower(output)
	points <- tolower(points)
	means <- tolower(means)
	if (output=='jpeg') {output <- 'jpg'}
	if (single.plot & match.axes != 'absolute') {
		match.axes <- 'absolute'
		warning('When \'single.plot\'=TRUE, \'match.axes\' is coerced to \'absolute\'.')
	}
	if (!(match.axes %in% c('absolute','relative','none'))) {
		warning('Unknown argument value \'', match.axes, '\'. \'match.axes\' must be one of \'absolute\', \'relative\', or \'none\'.')
		stop()
	}
	if (!(output %in% c('pdf','jpg','screen'))) {
		warning('Unknown argument value \'', output, '\'. \'output\' must be one of \'pdf\', \'jpg\', or \'screen\'.')
		stop()
	}
	if (!(points %in% c('shape','text','none'))) {
		warning('Unknown argument value \'', points, '\'. \'points\' must be one of \'shape\', \'text\', or \'none\'.')
		stop()
	}
	if (!(means %in% c('shape','text','none'))) {
		warning('Unknown argument value \'', points, '\'. \'points\' must be one of \'shape\', \'text\', or \'none\'.')
		stop()
	}
	if (ellipses & (ellipse.alpha<0 | ellipse.alpha>1)) {
		warning('Ellipse size is measured as an alpha level [0,1], and cannot be a negative number.')
		stop()
	}
	if (!is.null(aspect.ratio)) {
		if (!is.numeric(aspect.ratio) | aspect.ratio <= 0) {
			warning('Aspect ratio (horizontal/vertical) must be a positive number.')
			stop()
		}
	}
	if (!match.unit & norm.method %in% c('s','scentroid','wattfabricius')) {
		warning('Argument \"match.unit\" coerced to TRUE with norm.method \"s-centroid\": plotting Hz on axes is uninformative for linear transforms.')
		match.unit <- TRUE
	}
	if (ellipses & (ellipse.alpha>1 | ellipse.alpha<0)) {
		warning('ellipse.alpha is an alpha level, and must be between 0 and 1 (inclusive)')
	}
	# LOAD DEPENDENCIES
	require(mixtools)
	require(Cairo)
	# FONT HANDLING
	if (output!='screen' & font.family!='') {
	  CairoFonts(regular=paste(font.family,':style=Regular,Book,Roman',sep=''), bold=paste(font.family,':style=Bold',sep=''), italic=paste(font.family,':style=Italic,Oblique',sep=''), bolditalic=paste(font.family,':style=Bold Italic,BoldItalic,Bold Oblique,BoldOblique',sep=''), symbol='Symbol')
	}
	if (.Platform$OS.type == "windows") {
		windowsFonts(phonR=windowsFont(font.family))
		font.family <- 'phonR'
	}
	# DATA PREPROCESSING
	# PREALLOCATE
	group <- NULL
	# GET THE DATA
	if (!is.null(data)) {
		f1 <- data[,match(eval(f1),colnames(data))]
		f2 <- data[,match(eval(f2),colnames(data))]
		vowel <- data[,match(eval(vowel),colnames(data))]
		if (!is.null(f3)) { f3 <- data[,match(eval(f3),colnames(data))] }
		if (!is.null(f0)) { f0 <- data[,match(eval(f0),colnames(data))] }
		if (!is.null(grouping.factor)) {
			group <- data[,match(eval(grouping.factor),colnames(data))]
			group <- factor(group)
		} else {
			group <- rep('noGroupsDefined',length(f1))
		}
	} else {
	  if (!is.null(grouping.factor)) {
  	  group <- factor(grouping.factor)
	  } else {
	    group <- rep('noGroupsDefined',length(f1))
	  }
	}
	df <- data.frame(cbind(f1,f2,f3,f0))
	df$vowel <- vowel
	df$group <- group
	rm(vowel,group)
  # ADJUST poly.order TO INCLUDE VOWELS PRESENT IN THE DATA THAT ARE NOT IN THE DEFAULT SET
  v <- unique(df$vowel)
  if (is.null(poly.order)) {
    poly.order <- c('i','\u026A','e','\u025B','\u00E6','a','\u0251','\u0252','\u0254','o','\u028A','u','\u028C','\u0259') # i ɪ e ɛ æ a ɑ ɒ ɔ o ʊ u ʌ ə
    if (polygon) {
      warning('No vowel order specified for polygon drawing. Polygon(s) may not draw correctly.')
    }
  } else {
    v.match <- v %in% poly.order
    if (sum(v.match) < length(v.match) & polygon) {
      warning('Mismatch between values present in \'vowel\' vector and \'poly.order\'. Polygon(s) may not draw correctly.')
    }
  }
  for (i in 1:length(v)) {
    if (!(v[i] %in% poly.order)) {
      poly.order <- c(poly.order,v[i])
    }
  }
	# SORT THE DATA BY VOWEL, SO THAT POLYGON HAS A CHANCE OF DRAWING IN PROPER ORDER.  SECONDARY ORDERING BY GROUP FOR CONVENIENCE
	df$vowel <- factor(df$vowel, levels=poly.order)
	df$vowel <- factor(df$vowel) # second call drops unused levels
	df <- df[order(df$vowel,df$group),]
	# COMPENSATE FOR MISMATCHES BETWEEN DEFAULT/PROVIDED LEVELS, AND LEVELS ACTUALLY PRESENT IN THE DATA
	if (is.null(poly.include)) {
		poly.include <- length(poly.order)
	} else if(poly.include>length(poly.order)) {
		poly.include <- length(poly.order)
	}
	# SAVE FOR LATER, SINCE THE MAIN f1 AND f2 COLUMNS MAY GET NORMALIZED
	df$f1hz <- df$f1
	df$f2hz <- df$f2
	# EXTRACT LISTS OF VOWEL & GROUP VALUES
	glist <- unique(df$group)
	vlist <- unique(df$vowel)
	# THESE WARNINGS HAVE TO COME AFTER glist HAS BEEN DEFINED
	if (length(titles)==1) {
		if (!single.plot & length(glist)>1 & titles!='none' & titles!='auto') {
			warning('Single title provided when \'single.plot\'=FALSE. All plots will have the same title.')
		}
	} else if (single.plot) {
		warning('Multiple titles provided for a single plot. Only the first title will be used.')
	} else if (length(titles) != length(glist)) {
		warning('Mismatch between number of titles and number of groups. Titles will be recycled or discarded as necessary.')
		titles <- rep(titles, length(glist))
	}
	if (length(axis.titles)>2) {
	  warning('Too many axis titles supplied; only the first two will be used.')
	} else if (length(axis.titles)<2 & axis.titles[1]!='auto') {
	  warning('Too few axis titles supplied; values will be recycled.')
	}
	if (length(glist)>1 & norm.method %in% c('z','zscore','ztransform','lobanov','s','scentroid','wattfabricius') & single.plot & !match.unit) {
		warning('\'match.unit\' coerced to TRUE: cannot draw axes in Hz when normalizing via z-transform or s-centroid with multiple groups.')
		match.unit <- TRUE
	}
	# NORMALIZE THE DATA
	if (norm.method == 'nearey2') {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,f3=df$f3,f0=df$f0,method=norm.method)[,2:3]
	} else if (norm.method %in% c('z','ztransform','zscore','lobanov')) {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,method=norm.method,grouping.factor=df$group)
	} else if (norm.method %in% c('s','scentroid','wattfabricius')) {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,method=norm.method,grouping.factor=df$group,vowel=df$vowel)
	} else if (norm.method != 'none') {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,method=norm.method)
	}
	# DETERMINE AXIS UNITS
	if (norm.method %in% c('log','logmean','nearey1','nearey2')) {
		unit <- 'log'
	} else if (norm.method %in% c('lobanov','ztransform','zscore','z')) {
		unit <- 'st.dev.'
	} else if (norm.method %in% c('s','scentroid','wattfabricius')) {
		unit <- 'Fn/S(Fn)'
	} else if (norm.method == 'mel') {
		unit <- 'Mel'
	} else if (norm.method == 'bark') {
		unit <- 'Bark'
	} else if (norm.method == 'erb') {
		unit <- 'E.R.B.'
	} else {
		unit <- 'Hz'
	}
	# CALCULATE ABSOLUTE EXTREMA & TABLES OF EXTREMA BY SUBJECT
	if (match.unit & norm.method != 'none') { # use normalized values for everything
		x <- df$f2
		y <- df$f1
		xmin.table <- tapply(df$f2,df$group,min)
		xmax.table <- tapply(df$f2,df$group,max)
		ymin.table <- tapply(df$f1,df$group,min)
		ymax.table <- tapply(df$f1,df$group,max)
	} else { # either don't match units or don't normalize, so use Hertz for everything
		x <- df$f2hz
		y <- df$f1hz
		xmin.table <- tapply(df$f2hz,df$group,min)
		xmax.table <- tapply(df$f2hz,df$group,max)
		ymin.table <- tapply(df$f1hz,df$group,min)
		ymax.table <- tapply(df$f1hz,df$group,max)
	}
	if (means != 'none') {
		f2means <- tapply(x,list(df$vowel,df$group),mean) #rownames=vowels, colnames=groups
		f1means <- tapply(y,list(df$vowel,df$group),mean)
	}
	if (ellipses) {
		subsets <- by(cbind(x,y), list(df$vowel,df$group), subset)
		centers <- lapply(subsets[!unlist(lapply(subsets, is.null))], colMeans)
		covars <- lapply(subsets[!unlist(lapply(subsets, is.null))], cov)
		for (i in 1:length(covars)) {
		  covars[[i]][is.na(covars[[i]])] <- 0
		}
		ellipse.args <- apply(cbind('mu'=centers, 'sigma'=covars, 'alpha'=ellipse.alpha, 'draw'=FALSE), 1, c)
		ellipse.points <- lapply(ellipse.args, function(x){ do.call('ellipse',x) })
		# extrema: formant x vowel x group (3-dimensional array)
		ellipse.maxima <- array(unlist(lapply(ellipse.points, function(x){apply(x,2,max)})),dim=c(2,length(vlist),length(glist)),dimnames=list(c('f2','f1'),vlist,glist))
		ellipse.minima <- array(unlist(lapply(ellipse.points, function(x){apply(x,2,min)})),dim=c(2,length(vlist),length(glist)),dimnames=list(c('f2','f1'),vlist,glist))
		# extrema: formant x group (matrix)
		ellipse.maxima.by.group <- apply(ellipse.maxima, c(1,3), max)
		ellipse.minima.by.group <- apply(ellipse.minima, c(1,3), min)
		# overall extrema by formant (vector)
		ellipse.maxima.overall <- apply(ellipse.maxima, 1, max)
		ellipse.minima.overall <- apply(ellipse.minima, 1, min)
	}
	if (match.axes=='absolute') {
		# USE THE EXTREMA FROM THE WHOLE DATA FRAME (ALL AXES IDENTICAL)
		if (ellipses) {
			if (points=='none' & ignore.hidden) {
				# USE ELLIPSES TO CALCULATE EXTREMA
				xmin <- ellipse.minima.overall[1]
				xmax <- ellipse.maxima.overall[1]
				ymin <- ellipse.minima.overall[2]
				ymax <- ellipse.maxima.overall[2]
			} else {
				# COMPARE POINTS & ELLIPSES TO CALCULATE EXTREMA
				xmin <- min(c(ellipse.minima.overall[1], min(x,na.rm=TRUE)))
				xmax <- max(c(ellipse.maxima.overall[1], max(x,na.rm=TRUE)))
				ymin <- min(c(ellipse.minima.overall[2], min(y,na.rm=TRUE)))
				ymax <- max(c(ellipse.maxima.overall[2], max(y,na.rm=TRUE)))
			}
		} else { # !ellipses
			if (points=='none' & ignore.hidden) {
				# USE MEANS TO CALCULATE EXTREMA
				xmin <- min(f2means, na.rm=TRUE)
				xmax <- max(f2means, na.rm=TRUE)
				ymin <- min(f1means, na.rm=TRUE)
				ymax <- max(f1means, na.rm=TRUE)
			} else {
				# USE POINTS TO CALCULATE EXTREMA
				xmin <- min(xmin.table, na.rm=TRUE)
				xmax <- max(xmax.table, na.rm=TRUE)
				ymin <- min(ymin.table, na.rm=TRUE)
				ymax <- max(ymax.table, na.rm=TRUE)
			}
		}
	} else if (match.axes=='relative') {
		# FIGURE OUT WHICH GROUP HAS THE LARGEST POINT RANGE & PAD THE OTHER GROUPS' EXTREMA TO MAKE UP THE DIFFERENCE
		ranges.by.group <- rbind('f2'=xmax.table-xmin.table, 'f1'=ymax.table-ymin.table)
		range.shortfall <- matrix(rep(apply(ranges.by.group,1,max), each=length(glist)), ncol=length(glist), byrow=TRUE) - ranges.by.group
		point.maxima.by.group <- rbind('f2'=xmax.table,'f1'=ymax.table) + range.shortfall/2
		point.minima.by.group <- rbind('f2'=xmin.table,'f1'=ymin.table) - range.shortfall/2
		#     grp1  grp2  grp3  grp4  ...
		# f2   x     x     x     x
		# f1   y     y     y     y
		if (ellipses) {
			# FIGURE OUT WHICH GROUP HAS THE LARGEST ELLIPSE RANGE & PAD THE OTHER GROUPS' EXTREMA TO MAKE UP THE DIFFERENCE
			ellipse.ranges.by.group <- ellipse.maxima.by.group - ellipse.minima.by.group
			ellipse.range.shortfall <- matrix(rep(apply(ellipse.ranges.by.group,1,max), each=length(glist)), ncol=length(glist), byrow=TRUE) - ellipse.ranges.by.group
			ellipse.maxima.by.group <- ellipse.maxima.by.group + ellipse.range.shortfall/2
			ellipse.minima.by.group <- ellipse.minima.by.group - ellipse.range.shortfall/2
			if (points=='none' & ignore.hidden) {
				# USE ELLIPSES TO CALCULATE EXTREMA
				xmin <- ellipse.minima.by.group[1,] # vector by group
				xmax <- ellipse.maxima.by.group[1,]
				ymin <- ellipse.minima.by.group[2,]
				ymax <- ellipse.maxima.by.group[2,]
			} else {
				# COMPARE POINTS & ELLIPSES TO CALCULATE EXTREMA
				xmin <- apply(rbind(point.minima.by.group[1,], ellipse.minima.by.group[1,]), 2, min) # vector by group
				xmax <- apply(rbind(point.maxima.by.group[1,], ellipse.maxima.by.group[1,]), 2, max)
				ymin <- apply(rbind(point.minima.by.group[2,], ellipse.minima.by.group[2,]), 2, min)
				ymax <- apply(rbind(point.maxima.by.group[2,], ellipse.maxima.by.group[2,]), 2, max)
			}
		} else { # !ellipses
			if (points=='none' & ignore.hidden) {
				# USE MEANS TO CALCULATE EXTREMA
				means.minima.by.group <- rbind('f2'=apply(f2means,2,min,na.rm=TRUE), 'f1'=apply(f1means,2,min,na.rm=TRUE))
				means.maxima.by.group <- rbind('f2'=apply(f2means,2,max,na.rm=TRUE), 'f1'=apply(f1means,2,max,na.rm=TRUE))
				means.ranges.by.group <- means.maxima.by.group - means.minima.by.group
				means.range.shortfall <- matrix(rep(apply(means.ranges.by.group,1,max), each=length(glist)), ncol=length(glist), byrow=TRUE) - means.ranges.by.group
				means.maxima.by.group <- means.maxima.by.group + means.range.shortfall/2
				means.minima.by.group <- means.minima.by.group - means.range.shortfall/2
				xmin <- means.minima.by.group[1,]
				xmax <- means.maxima.by.group[1,]
				ymin <- means.minima.by.group[2,]
				ymax <- means.maxima.by.group[2,]
			} else {
				# USE POINTS TO CALCULATE EXTREMA
				xmin <- point.minima.by.group[1,] # vector by group
				xmax <- point.maxima.by.group[1,]
				ymin <- point.minima.by.group[2,]
				ymax <- point.maxima.by.group[2,]
			}
		}
	} else { # match.axes=='none'
		# USE DIFFERENT EXTREMA FOR EACH SUBSET (DIFFERENT PLOTS MAY VARY IN AXIS SCALES)
		if (ellipses) {
			if (points=='none' & ignore.hidden) {
				# USE ELLIPSES TO CALCULATE EXTREMA
				xmin <- ellipse.minima.by.group[1,]
				xmax <- ellipse.maxima.by.group[1,]
				ymin <- ellipse.minima.by.group[2,]
				ymax <- ellipse.maxima.by.group[2,]
			} else {
				# COMPARE POINTS & ELLIPSES TO CALCULATE EXTREMA
				xmin <- apply(rbind(ellipse.minima.by.group[1,], xmin.table), 2, max)
				xmax <- apply(rbind(ellipse.maxima.by.group[1,], xmax.table), 2, max)
				ymin <- apply(rbind(ellipse.minima.by.group[2,], ymin.table), 2, max)
				ymax <- apply(rbind(ellipse.maxima.by.group[2,], ymax.table), 2, max)
			}
		} else { # !ellipses
			if (points=='none' & ignore.hidden) {
				# USE MEANS TO CALCULATE EXTREMA
				xmin <- apply(f2means,2,min,na.rm=TRUE)
				xmax <- apply(f2means,2,max,na.rm=TRUE)
				ymin <- apply(f1means,2,min,na.rm=TRUE)
				ymax <- apply(f1means,2,max,na.rm=TRUE)
			} else {
				# USE POINTS TO CALCULATE EXTREMA
				xmin <- xmin.table
				xmax <- xmax.table
				ymin <- ymin.table
				ymax <- ymax.table
			}
		}
	}
	# CALCULATE A SENSIBLE TICKMARK STEP SIZE
	xrange <- xmax-xmin
	yrange <- ymax-ymin
	xsteps <- 10^(floor(log(xrange,10)))
	ysteps <- 10^(floor(log(yrange,10)))
	for (i in 1:length(xsteps)) {
		if (xrange[i]/xsteps[i] < 1) {
			xsteps[i] <- 0.1*xsteps[i]
		} else if (xrange[i]/xsteps[i] < 2) {
			xsteps[i] <- 0.2*xsteps[i]
		} else if (xrange[i]/xsteps[i] < 5) {
			xsteps[i] <- 0.5*xsteps[i]
		}
	}
	for (i in 1:length(ysteps)) {
		if (yrange[i]/ysteps[i] < 1) {
			ysteps[i] <- 0.1*ysteps[i]
		} else if (yrange[i]/ysteps[i] < 2) {
			ysteps[i] <- 0.2*ysteps[i]
		} else if (yrange[i]/ysteps[i] < 5) {
			ysteps[i] <- 0.5*ysteps[i]
		}
	}
	# COLORS, ETC
	topmargin <- ifelse(length(titles)==1 & titles[1]=='none', 3, 5)
	vowelcolors <- rep(hcl(0,0,0,means.alpha), length=length(glist)) # default color: (semitransparent) black
	pointcolors <- rep(hcl(0,0,0,points.alpha), length=length(glist)) # default color: (semitransparent) black
	linetypes <- rep(1, length=length(glist)) # default linetype: solid
	symbols <- rep(16, length=length(glist)) # default symbol: filled circle
	if (!uniform.style) {
		if (vary.shapes) {
			symbols <- rep(c(0:2,5,15:18,3,4,6), length=length(glist)) # open{square,circle,triangle,diamond}, filled{square,circle,triangle,diamond}, plus, x, inverted triangle
		}
		if (vary.lines) {
      linetypes <- rep(c('solid', '43', '23258385', '13', 'F3131313', '93', '28282383', '23F3', '232923', '4313', 'F3'), length=length(glist))
		}
		if (length(glist)>1) {
		  if (!grayscale) {
			  vowelcolors <- hcl(h=seq(0,360-(360/length(glist)),length=length(glist)), c=100, l=35, alpha=means.alpha, fixup=TRUE)
			  pointcolors <- hcl(h=seq(0,360-(360/length(glist)),length=length(glist)), c=100, l=35, alpha=points.alpha, fixup=TRUE)
#		  } else {
#			  vowelcolors <- rep(hcl(0,0,0,means.alpha), length=length(glist))
#			  pointcolors <- rep(hcl(0,0,0,points.alpha), length=length(glist))
		  }
		}
	}
	# IF PLOTTING ALL GROUPS ON ONE GRAPH, INITIALIZE OUTPUT DEVICE ONLY ONCE
	if (single.plot) {
		# PDF OUTPUT
		if (output=='pdf') {
			Cairo(file=paste(grouping.factor,'.pdf',sep=''), width=plot.dims[1], height=plot.dims[2], type='pdf', dpi='auto', units=plot.unit)
		# JPG OUTPUT
		} else if (output=='jpg') {
			Cairo(file=paste(grouping.factor,'.jpg',sep=''), width=plot.dims[1], height=plot.dims[2], type='jpeg', dpi=96, units=plot.unit)
#    # SCREEN OUTPUT
#    } else {
#      Cairo(width=plot.dims[1], height=plot.dims[2], type='x11', units=plot.units)
		}
		op <- par(mfrow=c(1,1), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0), family=font.family)
	} else if (output=='screen') {
		# SET UP FOR LATTICE PLOTTING
		num.rows <- num.cols <- ceiling(sqrt(length(glist)))
		if(length(glist) <= num.rows*(num.rows-1)) {
    	num.rows <- num.rows-1
		}
		op <- par(mfrow=c(num.rows,num.cols), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0), family=font.family)
	}

	# PLOT!
	for (i in 1:length(glist)) {
		# GET CURRENT GROUP'S DATA
		curGroup <- glist[i]
		curData <- subset(df, group==curGroup)
		curData$vowel <- factor(curData$vowel) # drop unused levels
		f2m <- tapply(curData$f2, curData$vowel, mean)
		f1m <- tapply(curData$f1, curData$vowel, mean)
		# IF PLOTTING EACH GROUP ON A SEPARATE GRAPH, INITIALIZE OUTPUT DEVICE SEPARATELY FOR EACH GROUP
		if (!single.plot) {
			# PDF OUTPUT
			if (output=='pdf') {
				Cairo(file=paste(curGroup,'.pdf',sep=''), width=plot.dims[1], height=plot.dims[2], type='pdf', dpi='auto', units=plot.unit)
				op <- par(mfrow=c(1,1), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0), family='Charis SIL')
			# JPG OUTPUT
			} else if (output=='jpg') {
				Cairo(file=paste(curGroup,'.jpg',sep=''), width=plot.dims[1], height=plot.dims[2], type='jpeg', dpi=96, units=plot.unit)
				op <- par(mfrow=c(1,1), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0), family='Charis SIL')
#      # SCREEN OUTPUT
#      } else {
#        Cairo(width=plot.dims[1], height=plot.dims[2], type='x11', units=plot.units)
			}
		}
		# CALCULATE PLOT LIMITS & TICKS (MAX BEFORE MIN, TO GET THE AXES TO INCREASE LEFT/DOWN INSTEAD OF RIGHT/UP)
		if (match.axes == 'absolute') { # ALL AXES IDENTICAL
			xs <- max(xsteps)
			ys <- max(ysteps)
			xlims <- c(ceiling(max(xmax)/xs)*xs, floor(min(xmin)/xs)*xs)
			ylims <- c(ceiling(max(ymax)/ys)*ys, floor(min(ymin)/ys)*ys)
			xticks <- seq(xlims[2],xlims[1],xs)
			yticks <- seq(ylims[2],ylims[1],ys)
		} else if (match.axes == 'relative') { # SAME SCALE, MAYBE DIFFERENT LIMITS
			xs <- max(xsteps)
			ys <- max(ysteps)
			xdiff <- max(ceiling(xmax/xs)-floor(xmin/xs)) - ceiling(xmax[i]/xs-floor(xmin[i]/xs))
			ydiff <- max(ceiling(ymax/ys)-floor(ymin/ys)) - ceiling(ymax[i]/ys-floor(ymin[i]/ys))
			xlims <- c(ceiling(ceiling(xdiff/2)+xmax[i]/xs)*xs, floor(floor(xdiff/2)+xmin[i]/xs)*xs)
			ylims <- c(ceiling(ceiling(ydiff/2)+ymax[i]/ys)*ys, floor(floor(ydiff/2)+ymin[i]/ys)*ys)
			xticks <- seq(xlims[2],xlims[1],xs)
			yticks <- seq(ylims[2],ylims[1],ys)
		} else { # match.axes == 'none' # AXES MAYBE DIFFERENT SCALES & LIMITS
			xs <- xsteps[i]
			ys <- ysteps[i]
			xlims <- c(ceiling(xmax[i]/xs)*xs, floor(xmin[i]/xs)*xs)
			ylims <- c(ceiling(ymax[i]/ys)*ys, floor(ymin[i]/ys)*ys)
			xticks <- seq(xlims[2],xlims[1],xs)
			yticks <- seq(ylims[2],ylims[1],ys)
		}
		# TRANSFORM TICKMARKS FROM HERTZ TO NORMALIZED UNIT (IF REQUIRED)
		xtext <- xticks
		ytext <- yticks
		if (norm.method != 'none' & !match.unit) {
			if (norm.method %in% c('z','zscore','ztransform','lobanov')) {
	      xticks <- (xticks-mean(curData$f2hz))/sd(curData$f2hz)
	      yticks <- (yticks-mean(curData$f1hz))/sd(curData$f1hz)
			} else if (norm.method %in% c('logmean','nearey1')) {
	      xticks <- log(xticks)-mean(log(curData$f2hz))
	      yticks <- log(yticks)-mean(log(curData$f1hz))
			} else {
	      xticks <- normalizeVowels(f2=xticks,method=norm.method) # y,x
	      yticks <- normalizeVowels(f1=yticks,method=norm.method) # y,x
			}
			xlims <- c(max(xticks),min(xticks))
			ylims <- c(max(yticks),min(yticks))
		}
		# DRAW AXES & MARGIN TEXT, UNLESS OVERPLOTTING AND WE'RE ALREADY PAST THE FIRST GROUP
		if (!single.plot | i==1) {
			# INITIALIZE EMPTY PLOT
			plot(0, 0, xaxt='n', yaxt='n', xlim=xlims, ylim=ylims, type='n', ann=FALSE, frame.plot=FALSE, asp=aspect.ratio)
			# AXES
			axis(3, at=xticks, labels=xtext, las=0, col=axis.col, col.ticks=axis.col, col.axis=axis.col, cex.axis=0.8, tck=-.005)
			axis(4, at=yticks, labels=ytext, las=2, col=axis.col, col.ticks=axis.col, col.axis=axis.col, cex.axis=0.8, tck=-.005)
			# TITLE
			if (is.null(data)) {
			  title.factor <- as.character(quote(grouping.factor))
			} else {
			  title.factor <- grouping.factor
			}
			if (titles[1]=='auto') {
				# AUTO-GENERATE TITLES
				if(glist[1] == 'noGroupsDefined') {
				  title <- 'Vowels'
				} else if (!single.plot) {
				  title <- paste('Vowels (', curGroup,')',sep='')
				} else {
				  title <- paste('Vowels by', title.factor)
				}
  			mtext(title, side=3, cex=1.2, las=1, line=3.25, font=2)
			} else if (titles[1]!='none') {
				# USE USER-SUPPLIED TITLES
				mtext(titles[i], side=3, cex=1.2, las=1, line=3.25, font=2)
			}
			# AXIS LABELS
			if (axis.titles[1] != 'auto') {
			  hlabel <- axis.titles[1]
			  vlabel <- axis.titles[2]
			} else if (norm.method != 'none' & !match.unit) {
			  hlabel <- paste('F2 (',unit,'-scaled Hz)', sep='')
			  vlabel <- paste('F1 (',unit,'-scaled Hz)', sep='')
			} else {
			  hlabel <- paste('F2 (',unit,')',sep='')
			  vlabel <- paste('F1 (',unit,')',sep='')
			}
			mtext(hlabel, side=3, cex=0.8, las=1, line=1.5, font=2, col=axis.col)
			mtext(vlabel, side=4, cex=0.8, las=3, line=2.5, font=2, col=axis.col)

		} else if (single.plot) {
			# REUSE THE SAME PLOT
			par(new=TRUE)
		} # if (!single.plot | i==1)

		# PLOT VOWEL POINTS
		if (points=='shape') {
			points(curData$f2, curData$f1, type='p', pch=symbols[i], cex=points.cex, col=pointcolors[i])
		} else if (points=='text') {
			text(curData$f2, curData$f1, curData$vowel, col=pointcolors[i], font=1, cex=points.cex)
		}
		# DRAW POLYGON BETWEEN MEANS
		if (polygon) {
	    points(f2m, f1m, type='c', cex=(means.cex+0.25), col=vowelcolors[i], lty=linetypes[i])
		}
		# PLOT VOWEL MEANS
		if (means=='shape') {
			points(f2m, f1m, type='p', pch=symbols[i], cex=means.cex, col=vowelcolors[i])
		} else if (means=='text') {
			text(f2m, f1m, names(f2m), col=vowelcolors[i], font=2, cex=means.cex)
		}
		# PLOT ELLIPSES AROUND VOWEL MEANS
		if (ellipses) {
			# CALCULATE COVARIANCE MATRICES
			covar <- list()
			curVowels <- names(f2m) # unique(curData$vowel)
			for (j in 1:length(curVowels)) {
				curVowelData <- subset(curData, vowel==curVowels[j])
				covar[[j]] <- cov(cbind(curVowelData$f2, curVowelData$f1))
				# PLOT ELLIPSES
				if (!is.na(covar[[j]][1])) {
					ellipse(c(f2m[j], f1m[j]), covar[[j]], type='l', col=vowelcolors[i], lty=linetypes[i], alpha=ellipse.alpha)
				}
			}
		}
    # PLOT LEGEND
		if (legend & (!single.plot | i==length(glist))) {
			if (points=='shape' | means=='shape') {
	      if (ellipses | polygon) { # lines, symbols
		      legend('bottomleft', legend=glist, fill=NA, border=NA, col=vowelcolors, lty=linetypes, pch=symbols, bty='n', inset=0.05, seg.len=3.5) # cex, lwd=1, border='n', text.font=1
	      } else { # symbols only
		      legend('bottomleft', legend=glist, fill=NA, border=NA, col=vowelcolors, pch=symbols, bty='n', inset=0.05) # cex, lwd=1, border='n', text.font=1
	      }
			} else {
			  if (ellipses | polygon) { # lines only
		      legend('bottomleft', legend=glist, fill=NA, border=NA, col=vowelcolors, lty=linetypes, bty='n', inset=0.05, seg.len=3.5) # cex, lwd=1, border='n', text.font=1
			  } else { # boxes only
        	legend('bottomleft', legend=glist, fill=vowelcolors, border=NA, bty='n', inset=0.05) # cex, lwd=1, border='n', text.font=1
			  }
			}
		}
		# CLOSE THE OUTPUT DEVICE FOR EACH INDIVIDUAL GROUP PLOT (UNLESS OUTPUT = SCREEN)
		if (!single.plot) {
			if (output != 'screen') {
				dev.off()
			}
		}
	} # for(i in 1:length(groups))
	# CLOSE THE OUTPUT DEVICE FOR SINGLE OVERPLOTTED GRAPH (UNLESS OUTPUT = SCREEN)
	if (single.plot) {
		if (output != 'screen') {
			dev.off()
		}
	}
  # RESET ORIGINAL GRAPHING PARAMETERS
	if (output == 'screen') {
    par(op)
	}
} #plotVowels <- function(...)

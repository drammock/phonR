# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# phonR version 0.4-1
# Functions for phoneticians and phonologists using R
# Daniel McCloy, drmccloy@uw.edu
# LICENSED UNDER THE GNU GENERAL PUBLIC LICENSE v3.0: http://www.gnu.org/licenses/gpl.html
# DEVELOPMENT OF THIS PACKAGE WAS FUNDED IN PART BY THE NATIONAL INSTITUTES OF HEALTH, GRANT NUMBER R01DC006014 TO PAMELA SOUZA
#
# CHANGELOG:
# v0.4: bugfixes: poly.order now works with arbitrary labels; bug in s-centroid calculation fixed.  Enhancements: added user-override arguments for color, shape and linestyle; added support for diphthong plotting, argument poly.include eliminated (inferred from elements present in poly.order).
# v0.3 bugfixes: font specification on windows now works for direct-to-file output. Enhancements: graphics handling overhauled to use base graphics instead of Cairo(). Several new output formats added. Raster resolution and font size now specifiable. Improved error handling.
# v0.2 bugfixes: points.alpha and means.alpha now work for grayscale plots. Plots with polygons or ellipses but no shapes now get proper legend type (lines, not boxes). Graphical parameters now captured and restored when plotting to onscreen device. Vowels with no variance (e.g., single tokens) no longer crash ellipse function. Vowels not in default poly.order() no longer go unplotted when points='text'. Enhancements: support for custom axis titles (to accommodate pre-normalized values), point and mean sizes, and fonts. Custom line types added (11 total now).
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# USAGE: source("phonR.r")
# --or-- R CMD install phonR_0.3-1.tar.gz (from command line)
# Then library(phonR)
# Then call functions as needed

# NORMALIZATION FUNCTION
normalizeVowels <- function(method, f0=NULL, f1=NULL, f2=NULL, f3=NULL, vowel=NULL, grouping.factor=NULL) {
  m <- tolower(method)
  f <- cbind(f0=as.vector(f0),f1=as.vector(f1),f2=as.vector(f2),f3=as.vector(f3))
  if (is.null(f)) {
    warning('Missing values: at least one of the arguments (f0, f1, f2, or f3) must be supplied.')
  }
  if (!(m %in% c('bark','mel','log','erb','z','zscore','z-score','ztransform','z-transform','lobanov','logmean','nearey1','nearey2','scentroid','s-centroid','s','wattfabricius','watt-fabricius'))) {
    warning('Method must be one of: bark, mel, log, erb, z|zscore|z-score|ztransform|z-transform|lobanov, logmean|nearey1, nearey2, s|scentroid|s-centroid|wattfabricius|watt-fabricius.')
  }
  if (!is.null(grouping.factor) & !(m %in% c('z','zscore','ztransform','lobanov','logmean','nearey1','nearey2','s','scentroid','wattfabricius'))) {
    warning('Normalization method \'',method,'\' is a vowel-intrinsic formula. Argument \'grouping.factor\' is ignored.')
  }
  if (!is.null(vowel) & !(m %in% c('s','scentroid','wattfabricius'))) {
    warning('Normalization method \'',method,'\' is a vowel-intrinsic formula. Argument \'vowel\' is ignored.')
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
  } else if (m %in% c('s','scentroid','wattfabricius','s-centroid','watt-fabricius')) {
    subsets <- by(f, list(vowel,grouping.factor), identity) # 2D list (group x vowel) of lists (f1,f2)
    means.list <- matrix(lapply(subsets, colMeans), ncol=ncol(subsets), dimnames=dimnames(subsets))
    minima <- apply(means.list, 2, function(i) apply(do.call(rbind,i), 2, min)) # TODO: bug here when using diphthongs
    maxima <- apply(means.list, 2, function(i) apply(do.call(rbind,i), 2, max))
    min.id <- apply(means.list, 2, function(i) apply(do.call(rbind,i), 2, which.min))
    max.id <- apply(means.list, 2, function(i) apply(do.call(rbind,i), 2, which.max))
    if (length(unique(min.id['f1',]))>1) {
      warning('The vowel with the lowest mean F1 value (usually /i/) does not match across all speakers/groups. You\'ll have to calculate s-centroid by hand.')
      data.frame(minF1=round(minima['f1',]), vowel=dimnames(means.list)[[1]][min.id['f1',]])
      stop()
    }
    if (length(unique(max.id['f1',]))>1) {
      warning('The vowel with the highest mean F1 value (usually /a/) does not match across all speakers/groups. You\'ll have to calculate s-centroid by hand.')
      data.frame(maxF1=round(maxima['f1',]), vowel=dimnames(means.list)[[1]][max.id['f1',]])
      stop()
    }
    lowvowf2 <- do.call(rbind, means.list[unique(max.id['f1',]),])[,'f2']
    centroids <- rbind(f1=(2*minima['f1',]+maxima['f1',])/3, f2=(minima['f2',]+maxima['f2',]+lowvowf2)/3)
    rnames <- rep(rownames(subsets),times=ncol(subsets))
    cnames <- rep(colnames(subsets),each=nrow(subsets))
    fn <- f/t(centroids[,grouping.factor])
  } else if (grouping.factor[1]=='noGroups') {
    if (m %in% c('lobanov','ztransform','z-transform','zscore','z-score','z')) {
      fn <- scale(f)
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
    subsets <- lapply(by(f, grouping.factor, identity),as.matrix) # list of matrices
    if (m %in% c('lobanov','ztransform','z-transform','zscore','z-score','z')) {
      means.list <- lapply(subsets, colMeans)
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
plotVowels <- function(data=NULL, vowel=NULL, f1=NULL, f2=NULL, f3=NULL, f0=NULL, grouping.factor=NULL, norm.method='none', match.unit=TRUE, match.axes='absolute', points='text', means='text', points.alpha=0.5, means.alpha=1, points.cex=0.6, means.cex=1.2, ignore.hidden=TRUE, ellipses=TRUE, ellipse.alpha=0.3173, polygon=TRUE, poly.order=NULL, single.plot=TRUE, titles='auto', axis.titles='auto', axis.cex=0.8, garnish.col='#666666FF', grayscale=FALSE, colors=NULL, shapes=NULL, lines=NULL, vary.colors=!grayscale, vary.shapes=grayscale, vary.lines=grayscale, legend=single.plot, output='screen', family='', pointsize=12, units='in', width=6.5, height=6.5, res=72, asp=NULL, point.arrows=TRUE, mean.arrows=TRUE, arrowhead.length=0.05, arrowhead.angle=30, point.arrow.width=1, mean.arrow.width=1.5) {
# shapes.by=NULL, colors.by=NULL, lines.by=NULL
# poly.method=c('hull','mean','voronoi')
	# MAKE CASE-INSENSITIVE
	norm.method <- tolower(norm.method)
	match.axes <- tolower(match.axes)
	output <- tolower(output)
	points <- tolower(points)
	means <- tolower(means)
	# SOME DEFAULTS
	if (!is.null(data)) {
		if (is.null(vowel)) vowel <- 'vowel'
		if (is.null(f1)) f1 <- 'f1'
		if (is.null(f2)) f2 <- 'f2'
	} else {
		if (is.null(vowel)) vowel <- vowel
		if (is.null(f1)) f1 <- f1
		if (is.null(f2)) f2 <- f2
	}
	# OUTPUT TYPES
	if (output=='jpeg') output <- 'jpg'
	if (output=='tiff') output <- 'tif'
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
	# CHECK FOR BOGUS ARGUMENTS, ETC
	if (legend %in% c('top','topright','right','bottomright','bottom','bottomleft','left','topleft','center')) {
		legend.pos <- legend
		legend <- TRUE
	} else if (!(legend %in% c(1,0))) {
		warning('Unknown argument value \'', legend, '\': \'legend\' must be logical, or a character vector specifying location (e.g., \'top\', \'bottomright\', \'center\', etc).')
		legend <- single.plot
	} else {
		legend.pos <- 'bottomleft'
	}
	if (single.plot & match.axes != 'absolute') {
		match.axes <- 'absolute'
		warning('When \'single.plot\'=TRUE, \'match.axes\' is coerced to \'absolute\'.')
	}
	if (!(match.axes %in% c('absolute','relative','none'))) {
		warning('Unknown argument value \'', match.axes, '\': \'match.axes\' must be one of \'absolute\', \'relative\', or \'none\'. Using default (\'absolute\').')
		match.axes <- 'absolute'
	}
	if (!(units %in% c('in','cm','mm','px'))) {
	  warning('Unknown argument value \'', units, '\': \'units\' must be one of \'in\', \'cm\', \'mm\', or \'px\'. Using default (\'in\').')
	  units <- 'in'
	}
	if (!(output %in% c('pdf','svg','jpg','tif','png','bmp','screen'))) {
		warning('Unknown argument value \'', output, '\': \'output\' must be one of \'pdf\', \'svg\', \'png\', \'tif\', \'bmp\', \'jpg\', or \'screen\'. Using default (\'screen\').')
		output <- 'screen'
	}
	if (!(points %in% c('shape','text','none'))) {
		warning('Unknown argument value \'', points, '\': \'points\' must be one of \'shape\', \'text\', or \'none\'. Using default (\'text\').')
		points <- 'text'
	}
	if (!(means %in% c('shape','text','none'))) {
		warning('Unknown argument value \'', means, '\': \'means\' must be one of \'shape\', \'text\', or \'none\'. Using default (\'text\').')
		means <- 'text'
	}
	if (ellipses & (ellipse.alpha<0 | ellipse.alpha>1)) {
		warning('Ellipse size is measured as an alpha level, and must be within [0,1]. Using default (0.3173).')
		ellipse.alpha <- 0.3173
	}
	if (!is.null(asp)) {
		if (!is.numeric(asp) | asp <= 0) {
			warning('Aspect ratio (horizontal/vertical) must be a positive number. Using default (\'NULL\', which adapts aspect ratio to maximize plot area usage).')
			asp <- NULL
		}
	}
	if (!match.unit & norm.method %in% c('s','scentroid','s-centroid','wattfabricius','watt-fabricius')) {
		warning('Argument \'match.unit\' coerced to TRUE with norm.method \'s-centroid\': plotting Hz on axes is uninformative for linear transforms.')
		match.unit <- TRUE
	}
	# FONT HANDLING FOR WINDOWS (RELATED BLOCK AT END OF SCRIPT)
	if (.Platform$OS.type=='windows' & family!='' & output %in% c('png','jpg','bmp','tif','screen')) {
		oldFont <- windowsFonts()$sans
		windowsFonts(sans=windowsFont(family))
		family <- 'sans'
		if (output=='screen') {
			warning('FYI, font specification may fail if saving as PDF from onscreen plot window menu. To ensure PDF font fidelity, run plotVowels() with output=\'pdf\'.')
		}
	}
	# DATA PREPROCESSING
	group <- NULL
	# GET THE DATA
	if (!is.null(data)) {
	  # TODO: if f1 is a vector of column names (for diphthong plotting), handle accordingly
		f1 <- data[,match(eval(f1),colnames(data))]
		f2 <- data[,match(eval(f2),colnames(data))]
		vowel <- data[,match(eval(vowel),colnames(data))]
		if (!is.null(f3)) f3 <- data[,match(eval(f3),colnames(data))]
		if (!is.null(f0)) f0 <- data[,match(eval(f0),colnames(data))]
		if (!is.null(grouping.factor)) {
			group <- data[,match(eval(grouping.factor),colnames(data))]
			group <- factor(group)
		} else {
			group <- rep('noGroupsDefined',length(f1))
		}
    diphthong <- FALSE # this is a temporary cheater step
	} else { # is.null(data)
	  if (is.null(dim(f1))) {
	    diphthong <- FALSE
    } else {
      if (!identical(dim(f1),dim(f2))) stop('F1 and F2 dimensions do not match.')
      if (dim(f1)[2] > 2) stop('F1 and F2 must be either vectors or 2-column matrices.')
      colnames(f1) <- c('f1a','f1b')
      colnames(f2) <- c('f2a','f2b')
#      if (!is.null(f3)) colnames(f3) <- c('f3a','f3b')
#      if (!is.null(f0)) colnames(f0) <- c('f0a','f0b')
      diphthong <- TRUE
    } 
	  if (!is.null(grouping.factor)) group <- factor(grouping.factor)
	  else group <- rep('noGroupsDefined',nrow(f1))
	}
	if (diphthong) {
	  df <- data.frame(cbind(f1=f1[,1],f2=f2[,1],f3=f3[,1],f0=f0[,1]))
	  dg <- data.frame(cbind(f1=f1[,2],f2=f2[,2],f3=f3[,2],f0=f0[,2]))
	  df$vowel <- dg$vowel <- vowel
	  df$group <- dg$group <- group
	} else {
	  df <- data.frame(cbind(f1,f2,f3,f0))
	  df$vowel <- vowel
	  df$group <- group
	}
	rm(vowel,group)
	# POLYGON ORDER HANDLING
	if (is.null(poly.order)) {
	# TODO: implement alternative polygon method (chull, voronoi, mean)
#		if (polygon=='mean') {
#			warning('No vowel order specified for polygon drawing, so no polygon will be drawn.')
		if (polygon) {
			warning('No vowel order specified for polygon drawing, so no polygon will be drawn.')
			polygon <- FALSE
		}
	} else { # !is.null(poly.order)
#		if (polygon=='mean') {
		if (polygon) {
			if(length(poly.order) != length(unique(poly.order))) warning('Duplicate entries in \'poly.order\' detected; they will be ignored.')
			poly.order <- unique(as.character(poly.order)) # as.character in case someone passes in a pre-factored list of vowels
			v <- unique(as.character(df$vowel))
			if (length(setdiff(poly.order,v))>0) {
				warning('There are vowels in \'poly.order\' that are not in \'vowel\'; they will be ignored.')
				poly.order <- intersect(poly.order,v)
			}
		} else { # polygon != 'mean'
#			warning('Argument \'poly.order\' ignored unless \'polygon\' is \'mean\'.')
			warning('Argument \'poly.order\' ignored unless \'polygon\' is TRUE.')
		}
	}
	# SORT THE DATA BY VOWEL, SO THAT POLYGON DRAWS IN PROPER ORDER.  SECONDARY ORDERING BY GROUP FOR CONVENIENCE
	if (diphthong) {
	  if (!is.null(poly.order)) {
	    factorLevels <- c(poly.order,setdiff(v,poly.order))
  	  df$vowel <- factor(df$vowel, levels=factorLevels)
  	  factorLevels <- paste(factorLevels,rep('offset',length(factorLevels)),sep='_')
  	  dg$vowel <- factor(paste(dg$vowel,rep('offset',length(dg$vowel)),sep='_'), levels=paste(poly.order,rep('offset',length(poly.order)),sep='_'))
	  }
	  dg <- dg[order(dg$vowel,dg$group),]
	  dg$f1hz <- dg$f1
	  dg$f2hz <- dg$f2
	}
  df <- df[order(df$vowel,df$group),]
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
	if (length(glist)>1 & norm.method %in% c('z','zscore','z-score','ztransform','z-transform','lobanov','s','scentroid','s-centroid','wattfabricius','watt-fabricius') & single.plot & !match.unit) {
		warning('\'match.unit\' coerced to TRUE: cannot draw axes in Hz when normalizing via z-transform or s-centroid with multiple groups.')
		match.unit <- TRUE
	}
	# NORMALIZE THE DATA
	if (norm.method == 'nearey2') {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,f3=df$f3,f0=df$f0,method=norm.method)[,2:3]
		if (diphthong) dg[,1:2] <- normalizeVowels(f1=dg$f1,f2=dg$f2,f3=dg$f3,f0=dg$f0,method=norm.method)[,2:3]
	} else if (norm.method %in% c('z','ztransform','zscore','lobanov')) {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,method=norm.method,grouping.factor=df$group)
		if (diphthong) dg[,1:2] <- normalizeVowels(f1=dg$f1,f2=dg$f2,method=norm.method,grouping.factor=dg$group)
	} else if (norm.method %in% c('s','scentroid','wattfabricius')) {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,method=norm.method,grouping.factor=df$group,vowel=df$vowel)
		if (diphthong) df[,1:2] <- normalizeVowels(f1=dg$f1,f2=dg$f2,method=norm.method,grouping.factor=dg$group,vowel=dg$vowel)
	} else if (norm.method != 'none') {
		df[,1:2] <- normalizeVowels(f1=df$f1,f2=df$f2,method=norm.method)
		if (diphthong) dg[,1:2] <- normalizeVowels(f1=dg$f1,f2=dg$f2,method=norm.method)
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
	  if (diphthong) {
	    x <- c(df$f2,dg$f2)
	    y <- c(df$f1,dg$f1)
	    g <- c(df$group,dg$group)
	    w <- c(df$vowel,dg$vowel)
		  xmin.table <- tapply(x,g,min)
		  xmax.table <- tapply(x,g,max)
		  ymin.table <- tapply(y,g,min)
		  ymax.table <- tapply(y,g,max)
	  } else {
		  x <- df$f2
		  y <- df$f1
		  xmin.table <- tapply(df$f2,df$group,min)
		  xmax.table <- tapply(df$f2,df$group,max)
		  ymin.table <- tapply(df$f1,df$group,min)
		  ymax.table <- tapply(df$f1,df$group,max)
	  }
	} else { # either don't match units or don't normalize, so use Hertz for everything
	  if (diphthong) {
		  x <- c(df$f2hz,dg$f2hz)
		  y <- c(df$f1hz,dg$f1hz)
	    g <- c(df$group,dg$group)
	    w <- c(df$vowel,dg$vowel)
		  xmin.table <- tapply(x,g,min)
		  xmax.table <- tapply(x,g,max)
		  ymin.table <- tapply(y,g,min)
		  ymax.table <- tapply(y,g,max)
	  } else {
		  x <- df$f2hz
		  y <- df$f1hz
		  xmin.table <- tapply(df$f2hz,df$group,min)
		  xmax.table <- tapply(df$f2hz,df$group,max)
		  ymin.table <- tapply(df$f1hz,df$group,min)
		  ymax.table <- tapply(df$f1hz,df$group,max)
	  }
	}
	if (means != 'none') {
	  if (diphthong) {
		  f2means <- tapply(x,list(w,g),mean) #rownames=vowels, colnames=groups
		  f1means <- tapply(y,list(w,g),mean)
	  } else {
		  f2means <- tapply(x,list(df$vowel,df$group),mean) #rownames=vowels, colnames=groups
		  f1means <- tapply(y,list(df$vowel,df$group),mean)
	  }
	}
	if (ellipses) {
	  if (diphthong) subsets <- by(cbind(x,y), list(w,g), identity)
	  else subsets <- by(cbind(x,y), list(df$vowel,df$group), identity)
		centers <- lapply(subsets[!unlist(lapply(subsets, is.null))], colMeans)
		covars <- lapply(subsets[!unlist(lapply(subsets, is.null))], cov)
    covars <- lapply(covars,function(x) replace(x,is.na(x),0))
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
	if (!is.null(shapes)) symbols <- rep(shapes, length=length(glist)) # user-defined
	else if (vary.shapes) symbols <- rep(c(16,1,17,2,15,0,18,5,3,4,6), length=length(glist)) # filled/open{circle,triangle,square,diamond}, plus, x, inverted triangle
	
	if (!is.null(lines)) linetypes <- rep(lines, length=length(glist))
	else if (vary.lines) linetypes <- rep(c('44', 'F4', '4313', 'F3131313', '23F3', '232923', '23258385', '282823B3', '13', '82', 'solid'), length=length(glist))

  if (!is.null(colors)) {
    vowelcolors <- rep(colors, length=length(glist))
    pointcolors <- rep(colors, length=length(glist))
  } else if (vary.colors) {
		if (grayscale) {
			vals <- seq(0,80,length=length(glist)+1)
			vals <- vals[-length(vals)]
			vowelcolors <- hcl(h=0, c=0, l=vals, alpha=means.alpha, fixup=TRUE)
			pointcolors <- hcl(h=0, c=0, l=vals, alpha=points.alpha, fixup=TRUE)
		} else {
			vowelcolors <- hcl(h=seq(0,360-(360/length(glist)),length=length(glist)), c=100, l=35, alpha=means.alpha, fixup=TRUE)
			pointcolors <- hcl(h=seq(0,360-(360/length(glist)),length=length(glist)), c=100, l=35, alpha=points.alpha, fixup=TRUE)
		}
	}
	# IF PLOTTING ALL GROUPS ON ONE GRAPH, INITIALIZE OUTPUT DEVICE ONLY ONCE (BEFORE THE 'FOR' LOOP)
	if (single.plot) {
		if (output=='screen') {
			op <- par(mfrow=c(1,1), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0), family=family, ps=pointsize)
		} else {
			# PDF
			if (output=='pdf') {
				cairo_pdf(filename=paste('vowels_by_',grouping.factor,'.pdf',sep=''), width=width, height=height, pointsize=pointsize, family=family)
			# SVG
			} else if (output=='svg') {
				svg(filename=paste('vowels_by_',grouping.factor,'.svg',sep=''), width=width, height=height, pointsize=pointsize, family=family)
			# JPG
			} else if (output=='jpg') {
				jpeg(filename=paste('vowels_by_',grouping.factor,'.jpg',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize, quality=100)
			# TIF
			} else if (output=='tif') {
				tiff(filename=paste('vowels_by_',grouping.factor,'.tif',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize, compression='lzw')
			# PNG
			} else if (output=='png') {
				png(filename=paste('vowels_by_',grouping.factor,'.png',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize)
			# BMP
			} else if (output=='bmp') {
				bmp(filename=paste('vowels_by_',grouping.factor,'.bmp',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize)
			}
			par(mfrow=c(1,1), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0))
		}
	} else if (output=='screen') { # !single.plot, SO SET UP FOR LATTICE PLOTTING
		num.rows <- num.cols <- ceiling(sqrt(length(glist)))
		if(length(glist) <= num.rows*(num.rows-1)) {
			num.rows <- num.rows-1
		}
	}
  
	# PLOT!
	for (i in 1:length(glist)) {
		# GET CURRENT GROUP'S DATA
		curGroup <- glist[i]
#		curData <- subset(df, group==curGroup)
#		curData$vowel <- factor(curData$vowel) # drop unused levels
#		f2m <- tapply(curData$f2, curData$vowel, mean)
#		f1m <- tapply(curData$f1, curData$vowel, mean)
		if (diphthong) {
		  curDataA <- df[df$group %in% curGroup,]
		  curDataB <- dg[dg$group %in% curGroup,]
		  curData <- data.frame(f1a=curDataA$f1, f1b=curDataB$f1, f2a=curDataA$f2, f2b=curDataB$f2, vowel=curDataA$vowel, group=curDataA$group)
		  curData$vowel <- factor(curData$vowel) # drop unused levels
		  f2m <- data.frame(f2a=tapply(curData$f2a, curData$vowel, mean), f2b=tapply(curData$f2b, curData$vowel, mean))
		  f1m <- data.frame(f1a=tapply(curData$f1a, curData$vowel, mean), f1b=tapply(curData$f1b, curData$vowel, mean))
		} else {
		  curData <- df[df$group %in% curGroup,]
		  curData$vowel <- factor(curData$vowel) # drop unused levels
		  f2m <- tapply(curData$f2, curData$vowel, mean)
		  f1m <- tapply(curData$f1, curData$vowel, mean)
		}
		# IF PLOTTING EACH GROUP ON A SEPARATE GRAPH, INITIALIZE OUTPUT DEVICE ANEW FOR EACH GROUP
		if (!single.plot & output!='screen') {
		  # PDF
		  if (output=='pdf') {
			  cairo_pdf(filename=paste(curGroup,'.pdf',sep=''), width=width, height=height, pointsize=pointsize, family=family)
		  # SVG
		  } else if (output=='svg') {
			  svg(filename=paste(curGroup,'.svg',sep=''), width=width, height=height, pointsize=pointsize, family=family)
		  # JPG
		  } else if (output=='jpg') {
			  jpeg(filename=paste(curGroup,'.jpg',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize, quality=100)
		  # TIF
		  } else if (output=='tif') {
			  tiff(filename=paste(curGroup,'.tif',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize, compression='lzw')
		  # PNG
		  } else if (output=='png') {
			  png(filename=paste(curGroup,'.png',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize)
		  # BMP
		  } else if (output=='bmp') {
			  bmp(filename=paste(curGroup,'.bmp',sep=''), units=units, width=width, height=height, res=res, family=family, pointsize=pointsize)
		  }
		  par(mfrow=c(1,1), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0))
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
			if (norm.method %in% c('z','zscore','z-score','ztransform','z-transform','lobanov')) {
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
		if (!single.plot & output=='screen' & i==1) {
		  # INITIALIZE ONSCREEN LATTICE LAYOUT
		  op <- par(mfrow=c(num.rows,num.cols), mar=c(1,0.5,topmargin,4.5), mgp=c(0,0.3,0), oma=c(0,0,0,0), family=family, ps=pointsize)
	  }
		if (!single.plot | i==1) {
			# INITIALIZE EMPTY PLOT
			plot(0, 0, xlim=xlims, ylim=ylims, type='n', axes=FALSE, ann=FALSE, frame.plot=FALSE, asp=asp)
			# AXES
			axis(3, at=xticks, labels=xtext, las=0, col=garnish.col, col.ticks=garnish.col, col.axis=garnish.col, cex.axis=axis.cex, tck=-.005)
			axis(4, at=yticks, labels=ytext, las=2, col=garnish.col, col.ticks=garnish.col, col.axis=garnish.col, cex.axis=axis.cex, tck=-.005)
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
			mtext(hlabel, side=3, cex=0.8, las=1, line=1.5, font=2, col=garnish.col)
			mtext(vlabel, side=4, cex=0.8, las=3, line=2.5, font=2, col=garnish.col)
		# REUSE THE SAME PLOT
		} else if (single.plot) {
			par(new=TRUE)
		} # if (!single.plot | i==1)

		# PLOT VOWEL POINTS
		if (diphthong) {
		  if (point.arrows) {
		    arrows(curData$f2a, curData$f1a, curData$f2b, curData$f1b, col=pointcolors[i], length=arrowhead.length, angle=arrowhead.angle, lwd=point.arrow.width)
		  }
		  if (points=='shape') {
		    if (point.arrows) { # don't plot where the arrowhead will be
    			points(curData$f2a, curData$f1a, type='p', pch=symbols[i], cex=points.cex, col=pointcolors[i])
		    } else {
  		    segments(curData$f2a, curData$f1a, curData$f2b, curData$f1b, col=pointcolors[i])
    			points(curData$f2a, curData$f1a, type='p', pch=symbols[i], cex=points.cex, col=pointcolors[i])
          points(curData$f2b, curData$f1b, type='p', pch=symbols[i], cex=points.cex, col=pointcolors[i])
		    }
		  } else if (points=='text') {
		    if (point.arrows) { # don't plot where the arrowhead will be
    			text(curData$f2a, curData$f1a, curData$vowel, col=pointcolors[i], font=1, cex=points.cex)
		    } else {
  		    segments(curData$f2a, curData$f1a, curData$f2b, curData$f1b, col=pointcolors[i])
    			text(curData$f2a, curData$f1a, curData$vowel, col=pointcolors[i], font=1, cex=points.cex)
    			text(curData$f2b, curData$f1b, curData$vowel, col=pointcolors[i], font=1, cex=points.cex)
		    }
		  }
		} else if (points=='shape') { # !diphthong
			points(curData$f2, curData$f1, type='p', pch=symbols[i], cex=points.cex, col=pointcolors[i])
		} else if (points=='text') {
			text(curData$f2, curData$f1, curData$vowel, col=pointcolors[i], font=1, cex=points.cex)
		}
		# PLOT ELLIPSES AROUND VOWEL MEANS
		# TODO: add a way to plot only nucleus ellipse, not offglide
		if (ellipses) {
			# CALCULATE COVARIANCE MATRICES
			covar <- list()
			if (diphthong) {
  			curVowels <- rownames(f2m)
			  covar2 <- list()
			  for (j in 1:length(curVowels)) {
			    curVowelData <- curData[curData$vowel %in% curVowels[j],]
			    covar[[j]] <- cov(cbind(curVowelData$f2a, curVowelData$f1a))
			    covar2[[j]] <- cov(cbind(curVowelData$f2b, curVowelData$f1b))
				  if (!is.na(covar[[j]][1])) {
					  ellipse(c(f2m$f2a[j], f1m$f1a[j]), covar[[j]], type='l', col=vowelcolors[i], lty=linetypes[i], alpha=ellipse.alpha)
					  ellipse(c(f2m$f2b[j], f1m$f1b[j]), covar2[[j]], type='l', col=vowelcolors[i], lty=linetypes[i], alpha=ellipse.alpha)
				  }
			  }
			} else { # !diphthong
  			curVowels <- names(f2m) # unique(curData$vowel)
			  for (j in 1:length(curVowels)) {
				  curVowelData <- curData[curData$vowel %in% curVowels[j],] #	curVowelData <- subset(curData, vowel==curVowels[j])
				  covar[[j]] <- cov(cbind(curVowelData$f2, curVowelData$f1))
				  # PLOT ELLIPSES
				  if (!is.na(covar[[j]][1])) {
					  ellipse(c(f2m[j], f1m[j]), covar[[j]], type='l', col=vowelcolors[i], lty=linetypes[i], alpha=ellipse.alpha)
				  }
			  }
			}
		}
		# DRAW POLYGON BETWEEN MEANS
		if (polygon) {
		  if (diphthong) {
  			points(f2m$f2a[poly.order], f1m$f1a[poly.order], type='c', cex=(means.cex+0.25), col=vowelcolors[i], lty=linetypes[i])
  			points(f2m$f2b[poly.order], f1m$f1b[poly.order], type='c', cex=(means.cex+0.25), col=vowelcolors[i], lty=linetypes[i])
		  } else {
  			points(f2m[poly.order], f1m[poly.order], type='c', cex=(means.cex+0.25), col=vowelcolors[i], lty=linetypes[i])
		  }
		}
		# PLOT VOWEL MEANS
		if (diphthong) {
		  if (mean.arrows) {
		    arrows(f2m$f2a, f1m$f1a, f2m$f2b, f1m$f1b, cex=means.cex, col=vowelcolors[i], length=arrowhead.length, angle=arrowhead.angle, lwd=mean.arrow.width)
		  }
		  if (means=='shape') {
		    if (mean.arrows) { # don't plot where the arrowhead will be
			    points(f2m$f2a, f1m$f1a, type='p', pch=symbols[i], cex=means.cex, col=vowelcolors[i])
		    } else {
			    points(f2m$f2a, f1m$f1a, type='p', pch=symbols[i], cex=means.cex, col=vowelcolors[i])
			    points(f2m$f2b, f1m$f1b, type='p', pch=symbols[i], cex=means.cex, col=vowelcolors[i])
		    }
		  } else if (means=='text') {
		    if (mean.arrows) { # don't plot where the arrowhead will be
			    text(f2m$f2a, f1m$f1a, rownames(f2m), col=vowelcolors[i], font=2, cex=means.cex)
		    } else {
			    text(f2m$f2a, f1m$f1a, rownames(f2m), col=vowelcolors[i], font=2, cex=means.cex)
			    text(f2m$f2b, f1m$f1b, rownames(f2m), col=vowelcolors[i], font=2, cex=means.cex)
		    }
		  }
		} else { # !diphthong
		  if (means=='shape') {
			  points(f2m, f1m, type='p', pch=symbols[i], cex=means.cex, col=vowelcolors[i])
		  } else if (means=='text') {
			  text(f2m, f1m, names(f2m), col=vowelcolors[i], font=2, cex=means.cex)
		  }
		}
    # PLOT LEGEND
		if (legend & (!single.plot | i==length(glist))) {
			if (points=='shape' | means=='shape') {
				if (ellipses | polygon) { # lines, symbols
					legend(legend.pos, legend=glist, fill=NA, border=NA, col=vowelcolors, lty=linetypes, lwd=1.5, pch=symbols, bty='n', inset=0.05, seg.len=3.5) # cex, lwd=1, border='n', text.font=1
				} else { # symbols only
					legend(legend.pos, legend=glist, fill=NA, border=NA, col=vowelcolors, pch=symbols, bty='n', inset=0.05) # cex, lwd=1, border='n', text.font=1
				}
			} else {
				if (ellipses | polygon) { # lines only
					legend(legend.pos, legend=glist, fill=NA, border=NA, col=vowelcolors, lty=linetypes, lwd=1.5, bty='n', inset=0.05, seg.len=3.5) # cex, lwd=1, border='n', text.font=1
				} else { # boxes only
					legend(legend.pos, legend=glist, fill=vowelcolors, border=NA, bty='n', inset=0.05) # cex, lwd=1, border='n', text.font=1
				}
			}
		}
		# CLOSE THE OUTPUT DEVICE FOR EACH INDIVIDUAL GROUP PLOT (UNLESS OUTPUT = SCREEN)
		if (!single.plot & output!='screen') dev.off()
	} # for(i in 1:length(groups))
	# CLOSE THE OUTPUT DEVICE FOR SINGLE OVERPLOTTED GRAPH (UNLESS OUTPUT = SCREEN)
	if (single.plot & output!='screen') dev.off()
	# RESET ORIGINAL GRAPHING PARAMETERS
	if (output == 'screen') par(op)
	# FONT HANDLING FOR WINDOWS
	if (.Platform$OS.type=='windows' & family!='' & output %in% c('png','jpg','bmp','tif','screen')) windowsFonts(sans=oldFont)
} #plotVowels <- function(...)

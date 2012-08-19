# set the working directory and read in the data
setwd('/media/DATA/Documents/academics/research/perception/dialect3/data/acoustic_data')
data <- read.delim('dialect3_acousticAnalysis.tab', header=TRUE, sep='\t', fileEncoding = 'UTF-8')

# remove unmeasureable values
data <- subset(data, F1>0 & F2>0)

# region, gender and region+gender subgroups
data$group <- substr(data$talker,1,3)



plotVowels <- function(data, vowel.column, f1.column, f2.column, f3.column=NULL, f0.column=NULL, grouping.factor.column=NULL, norm.method='none', axis.unit=norm.method, points='shape', means='text', ellipse=TRUE, polygon=TRUE, polygon.order=c('i','e','ɪ','ɛ','æ','a','ɑ','ɔ','o','ʊ','u','ʌ'), polygon.draw=length(polygon.order), match.axes='absolute', overplotting=TRUE, output='screen', grayscale=FALSE) {
	# EXPLANATION OF FUNCTION ARGUMENTS
	# data						a data frame containing the values to be plotted
	# vowel.column				the name of the column in which vowel symbols are found
	# f0.column					the name of the column in which f0 values are found
	# f1.column					the name of the column in which F1 values are found
	# f2.column					the name of the column in which F2 values are found
	# f3.column					the name of the column in which F3 values are found
	# grouping.factor.column	the name of the column in which a grouping factor is found.  
	#							The grouping factor will organize data points by color on a single plot (if overplotting=TRUE), 
	#							or each value of the grouping factor on a separate plot (if overplotting=FALSE) 
	# points					How should indiviual vowel tokens be plotted?  "text" plots the character string in vowel.column,
	#							"dots" plots them as filled circles, and "none" omits plotting the vowel tokens
	# means						How should vowel means be plotted?  "text" plots the character string in vowel.column,
	#							"dots" plots the means as filled circles, and "none" omits plotting the vowel means 
	# ellipse					Boolean indicating whether to plot an ellipse around each vowel mean.  Ellipses default to 1 standard deviation ???????????
	# polygon					Boolean indicating whether to plot a line connecting the vowel means into the standard "vowel polygon"
	# overplotting				Boolean indicating whether each value of the grouping factor should be on a separate graph (FALSE) or all on the same graph (TRUE) 
	# output					Possible values are "screen", "pdf", "jpg"
	# match.axes				"absolute" means XXXX  "relative" means XXXX  "none" means XXXX
	# norm.method				Possible values are none, bark, mel, erb, lobanov|ztransform|zscore|z, nearey1|logmean, nearey2

  # MAKE CASE-INSENSITIVE, ETC
  match.axes <- tolower(match.axes)
  output <- tolower(output)
  points <- tolower(points)
  means <- tolower(means)
  axis.unit <- tolower(axis.unit)
  norm.method <- tolower(norm.method)
  polygon.draw <- as.integer(polygon.draw)
  if (output=='jpeg') {output <- 'jpg'}
#  if (norm.method %in% c('lobanov','ztransform','zscore')) {norm.method <- 'z'}
#  if (axis.unit %in% c('lobanov','ztransform','zscore')) {axis.unit <- 'z'}
  if (axis.unit %in% c('none','hertz')) {axis.unit <- 'hz'}
#  if (axis.unit == 'nearey1') {axis.unit <- 'logmean'}

  # CHECK FOR BOGUS ARGUMENTS
#  if (!(axis.unit %in% c('hz','bark','mel','erb','z','log'))) {
#    warning('Unknown argument value \'', axis.unit, '\'. \'axis.unit\' must be one of \'hz\', \'bark\', \'mel\', \'erb\', \'z\', \'log\'.')
  if (axis.unit != norm.method & axis.unit != 'hz') {
    warning('Unknown argument value \'', axis.unit, '\'. \'axis.unit\' must be either \'hz\' or the default value.')
    stop()
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

  # LOAD PACKAGES
  if (ellipse) { require(mixtools) }
  if (output!='screen') {
    require(Cairo)
    CairoFonts(regular='Charis SIL:style=Regular', bold='Charis SIL:style=Bold', italic='Charis SIL:style=Italic', bolditalic='Charis SIL:style=Bold Italic,BoldItalic', symbol='Symbol')
  }


  # # # # # # # # #
  # PREPROCESSING #
  # # # # # # # # #

  # NEAREY2 REQUIRES F0,F1,F2,F3 TO ALL BE SUPPLIED
	if (norm.method=='nearey2') {
	  if (length(unique(c(length(f1),length(f2),length(f3),length(f0)))) > 1) {
	    warning('Unequal vector length: the \'nearey2\' normalization algorithm requires all arguments (f1,f2,f3,f0) to be specified and of equal length.')
	  } else if (is.null(grouping.factor.column)) {
		  df <- data.frame(cbind( 
			  as.character(data[,match(vowel.column, colnames(data))]),   # vowel
			  data[,match(f1.column, colnames(data))], # F1
			  data[,match(f2.column, colnames(data))], # F2 
			  data[,match(f3.column, colnames(data))], # F3 
			  data[,match(f0.column, colnames(data))]  # F0 
		  ))
		  colnames(df) <- c('v','f1','f2','f3','f0')
	  } else {
		  df <- data.frame(cbind( 
			  as.character(data[,match(vowel.column, colnames(data))]),   # vowel
			  data[,match(f1.column, colnames(data))], # F1
			  data[,match(f2.column, colnames(data))], # F2 
			  data[,match(f3.column, colnames(data))], # F3 
			  data[,match(f0.column, colnames(data))],  # F0 
  			as.character(data[,match(grouping.factor.column, colnames(data))])   # grouping factor
		  ))
		  colnames(df) <- c('v','f1','f2','f3','f0','g')
	  }
	df$v <- factor(df$v, levels=polygon.order)
	df$f1 <- as.numeric(as.character(df$f1))
	df$f2 <- as.numeric(as.character(df$f2))
	df$f3 <- as.numeric(as.character(df$f3))
	df$f0 <- as.numeric(as.character(df$f0))
	  
  # ALL OF THE OTHER NORMALIZATION METHODS (OR NO NORMALIZATION) ONLY REQUIRE F1 AND F2 FOR PLOTTING
	} else {
	  if (is.null(grouping.factor.column)) {
		  df <- data.frame(cbind( 
			  as.character(data[,match(vowel.column, colnames(data))]),   # vowel
			  data[,match(f1.column, colnames(data))], # F1
			  data[,match(f2.column, colnames(data))] # F2 
		  ))
		  colnames(df) <- c('v','f1','f2')
	  } else {
		df <- data.frame(cbind( 
			as.character(data[,match(vowel.column, colnames(data))]),   # vowel
			data[,match(f1.column, colnames(data))], # F1
			data[,match(f2.column, colnames(data))], # F2 
			as.character(data[,match(grouping.factor.column, colnames(data))])   # grouping factor
		))
		  colnames(df) <- c('v','f1','f2','g')
	  }
	df$v <- factor(df$v, levels=polygon.order)
	df$f1 <- as.numeric(as.character(df$f1))
	df$f2 <- as.numeric(as.character(df$f2))
  }

  # DO THE NORMALIZATION. IF AXIS TICKS ARE IN A DIFFERENT UNIT, STORE THE HZ VALUES FOR USE IN TICKMARKS
  if (norm.method=='nearey2') {
    df.hz <- df
    df[,2:3] <- normalizeVowels(f1=df$f1,f2=df$f2,f3=df$f3,f0=df$f0,method=norm.method)[,2:3] # we have to pass it f0,f1,f2,f3, but we only want f1,f2 back
  } else if (norm.method!='none') {
    df.hz <- df
    df[,2:3] <- normalizeVowels(f1=df$f1,f2=df$f2,method=norm.method)
  }

	# SORT THE DATA BY VOWEL, SO THAT POLYGON WILL DRAW IN PROPER ORDER
	df <- df[order(df$v),]

	# EXTRACT LISTS OF VOWEL & GROUP VALUES
	vowels <- unique(df$v)
	groups <- unique(df$g)
	if (is.null(groups)) {
		groups <- 'noGroupsDefined'
	}


  # # # # # # # #
  # COLORS, ETC #
  # # # # # # # #

  vowelcolors <- '#000000FF' # first color always black
  linetypes <- 1 # default linetype: solid
  symbols <- 20 # default symbol: small circle
  numcol <- length(groups)-1
	if (grayscale) {
	  if (numcol>0) {
	    linetypes <- c(1:6)
	    symbols <- c(1,20,22,23,24,15,16,17) # circle, filled circle, box, diamond, triangle, filled box, filled diamond, filled triangle
	  }
	} else {
	  if (numcol>0) {
		  vowelcolors <- c(vowelcolors, hcl(seq(0,360-(360/numcol),length=numcol), c=100, l=45, alpha=1))
	  }
	}


	# # # # # # # # # # # # # # #
  # CALCULATE PLOT DIMENSIONS #
	# # # # # # # # # # # # # # #

  if (overplotting & match.axes != 'absolute') {
    match.axes <- 'absolute'
    warning('When \'overplotting\'=TRUE, the \'match.axes\' argument is coerced to \'absolute\'.')
  }

  # MAKE TABLES OF THE DATA RANGES FOR EACH GROUPING FACTOR
  xmin <- tapply(df$f2,df$g,min)
  xmax <- tapply(df$f2,df$g,max)
  ymin <- tapply(df$f1,df$g,min)
  ymax <- tapply(df$f1,df$g,max)
#  extrema <- cbind(xmin,xmax,ymin,ymax)
  
  if (match.axes=='none') {
    # USE DIFFERENT EXTREMA FOR EACH SUBSET (DIFFERENT PLOTS MAY VARY IN AXIS SCALES)
    xrange <- xmax - xmin #these are tables, one value per grouping factor
    yrange <- ymax - ymin

  } else if (match.axes=='relative') {
    # USE EXTREMA CENTERED ON THE CURRENT DATA BUT LARGE ENOUGH IN SPAN TO COVER THE LARGEST SUBSET (ALL AXES GET SAME SCALE)
    xrange <- max(xmax - xmin) #scalars
    yrange <- max(ymax - ymin)

  } else { # match.axes=='absolute'
    # USE THE EXTREMA FROM THE WHOLE DATA FRAME (ALL AXES IDENTICAL)
    xrange <- max(xmax)-min(xmin) #scalars
    yrange <- max(ymax)-min(ymin)
  }

  # CALCULATE THE MOST SENSIBLE TICKMARK SEPARATION SIZE
  xstep <- 10^(floor(log(xrange,10)))
  ystep <- 10^(floor(log(yrange,10)))
  # THE FOR-LOOPS ARE NECESSARY WHEN match.axes=='none' AND APPLY VACUOUSLY IN OTHER CASES
  for (i in 1:length(xstep)) {
    if (xrange[i]/xstep[i] < 1) {
      xstep[i] <- 0.1*xstep[i] 
    } else if (xrange[i]/xstep[i] < 2) {
      xstep[i] <- 0.2*xstep[i]
    } else if (xrange[i]/xstep[i] < 5) {
      xstep[i] <- 0.5*xstep[i]
    }
  }
  for (i in 1:length(ystep)) {
    if (yrange[i]/ystep[i] < 1) {
      ystep[i] <- 0.1*ystep[i] 
    } else if (yrange[i]/ystep[i] < 2) {
      ystep[i] <- 0.2*ystep[i]
    } else if (yrange[i]/ystep[i] < 5) {
      ystep[i] <- 0.5*ystep[i]
    }
  }


  # # # # # # # # # #
  # PREPARE TO PLOT #
  # # # # # # # # # #
	
	# IF PLOTTING ALL GROUPS ON ONE GRAPH, INITIALIZE OUTPUT DEVICE ONLY ONCE
	if (overplotting) {
		# PDF OUTPUT
		if (output=='pdf') {
			Cairo(file=grouping.factor.column, width=7, height=7, type='pdf', dpi='auto', units='in')
			
		# JPEG OUTPUT
		} else if (output=='jpg') {
			Cairo(file=grouping.factor.column, width=7, height=7, type='jpeg', dpi=96, units='in')
			
		# SCREEN OUTPUT
#		} else {
#			Cairo(width=7, height=7, type='x11', units='in')
		}
	}
	

  # # # # #
  # PLOT! #
  # # # # #
	
	for(i in 1:length(groups)) {
	  currentGroup <- groups[i]
		# IF PLOTTING EACH GROUP ON A SEPARATE GRAPH, INITIALIZE OUTPUT DEVICE SEPARATELY FOR EACH GROUP
		if (!overplotting) {
			# PDF OUTPUT
			if (output=='pdf') {
				Cairo(file=paste(grouping.factor.column,i,sep='_'), width=7, height=7, type='pdf', dpi='auto', units='in')

			# JPEG OUTPUT
			} else if (output=='jpg') {
				Cairo(file=paste(grouping.factor.column,i,sep='_'), width=7, height=7, type='jpeg', dpi=96, units='in')
			
			# SCREEN OUTPUT
#			} else {
#				Cairo(width=7, height=7, type='x11', units='in')
			}
		}
		
		# GET CURRENT GROUP'S DATA
		if (currentGroup == 'noGroupsDefined') {
			curData <- df
		} else {
			curData <- subset(df, g==currentGroup)
		}
		
		# CURRENT GROUP'S VOWEL MEANS
		F2means <- tapply(curData$f2, curData$v, mean)
		F1means <- tapply(curData$f1, curData$v, mean)

		# DRAW AXES & MARGIN TEXT, UNLESS OVERPLOTTING AND WE'RE ALREADY PAST THE FIRST GROUP
		if (!overplotting | i==1) {
			par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family='Charis SIL')
			
			# GET DESIRED PLOT LIMITS (MAX BEFORE MIN, TO GET THE AXES TO INCREASE LEFT/DOWN INSTEAD OF RIGHT/UP)
			# xrange, yrange, xstep, ystep already calculated above
			if (match.axes=='absolute') {
			  xlims <- c(ceiling(max(xmax)/xstep)*xstep, floor(min(xmin)/xstep)*xstep)
			  ylims <- c(ceiling(max(ymax)/ystep)*ystep, floor(min(ymin)/ystep)*ystep)

			} else if (match.axes=='relative') {
        xdiff <- max(ceiling(xmax/xstep)-floor(xmin/xstep)) - ceiling(xmax[i]/xstep)-floor(xmin[i]/xstep) # target.num.steps - current.num.steps
			  xlims <- c(ceiling(ceiling(xdiff/2)+xmax[i]/xstep)*xstep, floor(floor(xdiff/2)+xmin[i]/xstep)*xstep)
			  ylims <- c(ceiling(ceiling(ydiff/2)+ymax[i]/ystep)*ystep, floor(floor(ydiff/2)+ymin[i]/ystep)*ystep)
			  			  
			} else {
			  xlims <- c(ceiling(xmax[i]/xstep[i])*xstep[i], floor(xmin[i]/xstep[i])*xstep[i])
			  ylims <- c(ceiling(ymax[i]/ystep[i])*ystep[i], floor(ymin[i]/ystep[i])*ystep[i])	  
			}

			# INITIALIZE EMPTY PLOT
			plot(0, 0, xaxt='n', yaxt='n', xlim=xlims, ylim=ylims, type='n', main='', frame.plot=FALSE)
#			plot(0, 0, xaxt='n', yaxt='n', xlim=c(xmax,xmin), ylim=c(ymax,ymin), type='n', main='', frame.plot=FALSE)

# # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # 

	    # CALCULATE TICK MARKS
#	    if (axis.unit == norm.method) {
        xticks <- seq(xlims[2], xlims[1], xstep[i])
        yticks <- seq(ylims[2], ylims[1], ystep[i])		
#	    } else { #must be hz
#		    if (currentGroup == 'noGroupsDefined') {
#			    curDataHz <- df.hz
#		    } else {
#			    curDataHz <- subset(df.hz, g==currentGroup)
#		    }
#
#		    # transform tick marks from "hz" to "normalize" dimension
#       xticks <- normalizeVowels(f2=xtickshz, method=norm.method)
#       yticks <- normalizeVowels(f1=ytickshz, method=norm.method)
#      }

      # AXES & MARGIN TEXT
      axis(3, at=xticks, las=0, col.axis='black', cex.axis=0.8, tck=-.01)
      axis(4, at=yticks, las=2, col.axis='black', cex.axis=0.8, tck=-.01)
      mtext(paste('Vowels by', grouping.factor.column), side=3, cex=1.2, las=1, line=4.5, font=2)
      mtext(paste('F2 (','Hz',')',sep=''), side=3, cex=1, las=1, line=2.5, font=2)
      mtext(paste('F1 (','Hz',')',sep=''), side=4, cex=1, las=3, line=2.5, font=2)

# # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # 

		# IF OVERPLOTTING, TELL par() TO REUSE THE SAME GRAPH
		} else if (overplotting) {
			par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family='Charis SIL', new=TRUE)
		}
		
		# PLOT VOWEL POINTS
		if (points=='shape') {
			points(curData$f2, curData$f1, type='p', pch=16, cex=0.5, col=vowelcolors[i])
		} else if (points=='text') {
			text(curData$f2, curData$f1, curData$v, col=vowelcolors[i], font=1, cex=1)
		}
		
		# DRAW POLYGON BETWEEN MEANS
		if (polygon) {
			points(F2means[1:polygon.draw], F1means[1:polygon.draw], type='c', cex=1, col=vowelcolors[i])
		}

		# PLOT VOWEL MEANS
		if (means=='shape') {	
			points(F2means, F1means, type='p', pch=16, cex=1, col=vowelcolors[i])
		} else if (means=='text') {	
			text(F2means, F1means, vowels, col=vowelcolors[i], font=2, cex=1.5)
		} 
		
		# PLOT +/- ONE S.D. ELLIPSES AROUND VOWEL MEANS
		if (ellipse) {
			# CALCULATE COVARIANCE MATRICES
			covar <- list()
			for (j in 1:length(vowels)) {
				cVowelData <- subset(curData, v==vowels[j])
				covar[[j]] <- cov(cbind(cVowelData$f2, cVowelData$f1))
				# PLOT ELLIPSES
				ellipse(c(F2means[j], F1means[j]), covar[[j]], type='l', col=vowelcolors[i])
			}
		}
		
		# CLOSE THE OUTPUT DEVICE FOR EACH INDIVIDUAL GROUP PLOT (UNLESS OUTPUT = SCREEN)
		if (!overplotting) {
			if (output != 'screen') {
				dev.off()
			}
		}
	}
	
	# CLOSE THE OUTPUT DEVICE FOR SINGLE OVERPLOTTED GRAPH (UNLESS OUTPUT = SCREEN)
	if (overplotting) {
		if (output != 'screen') {
			dev.off()
		}
	}
}

# plotvowels(data=data, f1.column="F1", f2.column="F2", vowel.column="vowel")

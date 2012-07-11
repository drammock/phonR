#set the working directory and read in the data
setwd('/Users/grant/Documents/NIHSouzaWright/PROJECTS/Dialect3/Dialect3 Results/vowel plotting')
data <- read.delim('FormantAnalysisResultscolumnsplit.txt', header=TRUE, sep='\t', fileEncoding = 'UTF-16')

#remove unmeasureable values
data <- subset(data, F1>0 & F2>0 & F3>0)

#use only the 50% values
data <- subset(data, percent=='50')

#region, gender and region+gender subgroups
data$region <- substr(data$subject,1,2)
data$gender <- substr(data$subject,3,3)
data$group <- substr(data$subject,1,3)




plotVowels <- function(data, vowel.column, f1.column, f2.column, f3.column=NULL, grouping.factor.column=NULL, normalize='hertz', unit=normalize, points='dots', means='text', ellipse=FALSE, polygon=FALSE, polygon.order=c('i','e','ɪ','ɛ','æ','a','ɔ','o','ʊ','u','ʌ'), polygon.draw=length(polygon.order), match.axes='absolute', overplotting=TRUE, output='screen') {
	# EXPLANATION OF FUNCTION ARGUMENTS
	# data						a data frame containing the values to be plotted
	# vowel.column				the name of the column in which vowel symbols are found
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

	# Normalization formulae taken from: Adank, P., Smits, R., & van Hout, R. (2004). A comparison of vowel normalization procedures for language variation research. Journal of the Acoustical Society of America, 116, 3099. doi:10.1121/1.1795335.  Original references from that paper are indicated here:
	# bark <- 26.81*Hz/(1960+Hz)-0.53						# traunmüller 1990; zwicker & terhardt 1980
	# mel  <- 2595*log(1+Hz/700)							# stevens & volkmann 1940
	# erb  <- 21.4*log(1+Hz*0.00437)						# glasberg & moore 1990
	# zscore <- (Hz-grandMeanForTalker)/stanDevForTalker	# Lobanov 1971

	if(ellipse) {
		# LOAD LIBRARY FOR ELLIPSE FUNCTION
		library(mixtools)
	}
	
	# EXTRACT ONLY THE COLUMNS WE NEED
	if(is.null(grouping.factor.column)) {
		df <- data.frame(cbind( 
			data[,match(f1.column, colnames(data))], # F1
			data[,match(f2.column, colnames(data))], # F2 
			as.character(data[,match(vowel.column, colnames(data))])   # vowel
		))
		colnames(df) <- c('f1','f2','v')
	} else {
		df <- data.frame(cbind( 
			data[,match(f1.column, colnames(data))], # F1
			data[,match(f2.column, colnames(data))], # F2 
			as.character(data[,match(vowel.column, colnames(data))]),   # vowel
			as.character(data[,match(grouping.factor.column,colnames(data))])   # grouping factor
		))
		colnames(df) <- c('f1','f2','v','g')
	}
	df$f1 <- as.numeric(as.character(df$f1))
	df$f2 <- as.numeric(as.character(df$f2))
	df$v <- polygon.order
#	df$v <- factor(df$v, levels=c('i', 'e', 'ɪ', 'ɛ', 'æ', 'a', 'ɔ', 'o', 'ʊ', 'u', 'ʌ'))

	# NORMALIZE
	if (normalize=='bark') {
		df$f1n <- 26.81*df$f1/(1960+df$f1)-0.53
		df$f2n <- 26.81*df$f2/(1960+df$f2)-0.53
		df$f3n <- 26.81*df$f3/(1960+df$f3)-0.53
	} else if (normalize=='log') {
		df$f1n <- log(df$f1)
		df$f2n <- log(df$f2)
		df$f3n <- log(df$f3)
	} else if (normalize=='mel') {
		df$f1n <- 2595*log(1+df$f1/700)
		df$f2n <- 2595*log(1+df$f2/700)
		df$f3n <- 2595*log(1+df$f3/700)
	} else if (normalize=='erb') {
		df$f1n <- 21.4*log(0.00437*df$f1+1)
		df$f2n <- 21.4*log(0.00437*df$f2+1)
		df$f3n <- 21.4*log(0.00437*df$f3+1)
	} else if (normalize=='lobanov' | normalize='z') {
		df$f1n <- (df$f1-mean(df$f1))/sd(df$f1)
		df$f2n <- (df$f2-mean(df$f2))/sd(df$f2)
		df$f3n <- (df$f3-mean(df$f3))/sd(df$f3)
	} else if (normalize=='nearey1') {
		df$f1n <- log(df$f1)-mean(log(df$f1))
		df$f2n <- log(df$f2)-mean(log(df$f2))
		df$f3n <- log(df$f3)-mean(log(df$f3))
#	} else if (normalize=='nearey2') {
#		df$f1n <- log(df$f1)-mean(log(df$f0))-mean(log(df$f1))-mean(log(df$f2))-mean(log(df$f3))
#		df$f2n <- log(df$f2)-mean(log(df$f0))-mean(log(df$f1))-mean(log(df$f2))-mean(log(df$f3))
#		df$f3n <- log(df$f3)-mean(log(df$f0))-mean(log(df$f1))-mean(log(df$f2))-mean(log(df$f3))
	} else { # Return untransformed Hz
		df$f1n <- df$f1
		df$f2n <- df$f2
		df$f3n <- df$f3
	}

	# SORT THE DATA BY VOWEL, SO THAT POLYGON WILL DRAW IN PROPER ORDER
	df <- df[order(df$v),]

	# EXTRACT LISTS OF VOWEL & GROUP VALUES
	vowels <- unique(df$v)
	groups <- unique(df$g)
	if(is.null(groups)) {
		groups <- 'noGroups'
	}
	
	# COLORS
	vowelcolors <- '#000000FF' # first color always black
	numcol <- length(groups)-1
	if (numcol>0) {
		vowelcolors <- c(vowelcolors, hcl(seq(0,360-(360/numcol),length=numcol), c=100, l=45, alpha=1))
	}
	
	# SET GRAPH SIZE (BUT NOT BOUNDS) BASED ON NEW "match.axes" ARGUMENT
	
	# CALCULATE GLOBAL MIN AND MAX FOR X AND Y AXES (IN HERTZ)
	xmax <- ceiling(max(df$f2)/100)*100
	xmin <- floor(min(df$f2)/100)*100
	ymax <- ceiling(max(df$f1)/100)*100
	ymin <- floor(min(df$f1)/100)*100
	xticks <- seq(xmin, xmax, 100)
	yticks <- seq(ymin, ymax, 100)

	# CALCULATE TICK MARKS
	# bark log mel erb lobanov|z nearey1 hz
	if(unit==normalize) {
		# figure out what are reasonable whole-number-like values to round to for each of the normalization scales
		# create xticks and yticks accordingly
		
	} else {
		# transform tick marks from "unit" dimension to "normalize" dimension
		# create xticks and yticks accordingly
	}

	# LOAD CAIRO PACKAGE
#	if(output != 'screen') {
		library(Cairo)
		CairoFonts(regular='Charis SIL:style=Regular', bold='Charis SIL:style=Bold', italic='Charis SIL:style=Italic', bolditalic='Charis SIL:style=Bold Italic,BoldItalic', symbol='Symbol' )
#	}
	
	# IF PLOTTING ALL GROUPS ON ONE GRAPH, INITIALIZE OUTPUT DEVICE ONLY ONCE
	if(overplotting) {
		# PDF OUTPUT
		if(output=='pdf') {
			Cairo(file=grouping.factor.column, width=7, height=7, type='pdf', dpi='auto', units='in')
			
		# JPEG OUTPUT
		} else if(output=='jpeg' | output=='jpg') {
			Cairo(file=grouping.factor.column, width=7, height=7, type='jpeg', dpi=96, units='in')
			
		# SCREEN OUTPUT
		} else {
			Cairo(width=7, height=7, type='x11', units='in')
		}
	}
	
	for(i in 1:length(groups)) {
		# IF PLOTTING EACH GROUP ON A SEPARATE GRAPH, INITIALIZE OUTPUT DEVICE SEPARATELY FOR EACH GROUP
		if(!overplotting) {
			# PDF OUTPUT
			if(output=='pdf') {
				Cairo(file=paste(grouping.factor.column,i,sep='_'), width=7, height=7, type='pdf', dpi='auto', units='in')

			# JPEG OUTPUT
			} else if(output=='jpeg' | output=='jpg') {
				Cairo(file=paste(grouping.factor.column,i,sep='_'), width=7, height=7, type='jpeg', dpi=96, units='in')
			
			# SCREEN OUTPUT
			} else {
				Cairo(width=7, height=7, type='x11', units='in')
			}
		}
		
		# GET CURRENT GROUP'S DATA
		if(groups[1] == 'noGroups') {
			curData <- df
		} else {
			curData <- subset(df, g==groups[i])
		}
		
		# CURRENT GROUP'S VOWEL MEANS
		F2means <- tapply(curData$f2, curData$v, mean)
		F1means <- tapply(curData$f1, curData$v, mean)

		# DRAW AXES & MARGIN TEXT, UNLESS OVERPLOTTING AND WE'RE ALREADY PAST THE FIRST GROUP
		if (!overplotting | i==1) {
			par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family='Charis SIL')

			# INITIALIZE EMPTY PLOT
			plot(0, 0, xaxt='n', yaxt='n', xlim=c(xmax,xmin), ylim=c(ymax,ymin), type='n', main='', frame.plot=FALSE)

			# AXES & MARGIN TEXT
			axis(3, at=seq(xmin, xmax, 100), las=0, col.axis='black', cex.axis=0.8, tck=-.01)
			axis(4, at=seq(ymin, ymax, 100), las=2, col.axis='black', cex.axis=0.8, tck=-.01)
			mtext(paste('Vowels by', grouping.factor.column), side=3, cex=1.2, las=1, line=4.5, font=2)
			mtext('F2 (Hz)', side=3, cex=1, las=1, line=2.5, font=2)
			mtext('F1 (Hz)', side=4, cex=1, las=3, line=2.5, font=2)

		# IF OVERPLOTTING, TELL par() TO REUSE THE SAME GRAPH
		} else if (overplotting) {
			par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family='Charis SIL', new=TRUE)
		}
		
		# PLOT VOWEL POINTS
		if(points=='dots') {
			points(curData$f2, curData$f1, type='p', pch=16, cex=0.5, col=vowelcolors[i])
		} else if(points=='text') {
			text(curData$f2, curData$f1, curData$vowels, col=vowelcolors[i], font=2, cex=1)
		}
		
		# DRAW POLYGON BETWEEN MEANS
		if(polygon) {
			points(F2means[1:polygon.draw], F1means[1:polygon.draw], type='c', cex=1, col=vowelcolors[i])
		}

		# PLOT VOWEL MEANS
		if(means=='dots') {	
			points(F2means, F1means, type='p', pch=16, cex=1, col=vowelcolors[i])
		} else if(means=='text') {	
			text(F2means, F1means, vowels, col=vowelcolors[i], font=2, cex=1.5)
		} 
		
		# PLOT +/- ONE S.D. ELLIPSES AROUND VOWEL MEANS
		if(ellipse) {
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
		if(!overplotting) {
			if(output != 'screen') {
				dev.off()
			}
		}
	}
	
	# CLOSE THE OUTPUT DEVICE FOR SINGLE OVERPLOTTED GRAPH (UNLESS OUTPUT = SCREEN)
	if(overplotting) {
		if(output != 'screen') {
			dev.off()
		}
	}
}

# plotvowels(data=data, f1.column="F1", f2.column="F2", vowel.column="vowel")

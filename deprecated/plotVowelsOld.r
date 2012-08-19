plotVowels <- function (f1, f2, f3=0, f0=0, groupingFactor="none", normalize="none", errorBars="none", output="screen", tokens="dots", means="text", polygon=FALSE, centDist=FALSE, force=FALSE) {
	# possible values for normalize: none, log, bark, mel, erb, nearey1, nearey2, z/lobanov
	# possible values of errorBars: none, cross, ellipse
	# possible values of output: screen, jpeg, pdf
	# possible values of tokens/means: dots, text, none (should handle PCH numbers too)
	
	# things to do:
	# polygonal area (means, encompassing)
	# order for polygonal area: iyɪʏeøɛœæaɶɐɑɒʌɔɤoʊɯuɨʉ(ɘɵəɜɞ)
	# mean distance from center
	# repulsive force (token, type)

	
	# STEP 1: CHECK FOR MISSING DATA, UNEQUAL VECTOR LENGTHS, DEAL WITH F3 IF MISSING, ETC.
	
	
	# STEP 2: NORMALIZE
	# Formulae from: Adank, P., Smits, R., & van Hout, R. (2004). A comparison of vowel normalization procedures for language variation research. Journal of the Acoustical Society of America, 116, 3099. doi:10.1121/1.1795335
	# bark <- 26.81*Hz/(1960+Hz)-0.53						# traunmüller 1990; zwicker & terhardt 1980
	# mel  <- 2595*log(1+Hz/700)							# stevens & volkmann 1940
	# erb  <- 21.4*log(1+Hz*0.00437)						# glasberg & moore 1990
	# zscore <- (Hz-grandMeanForTalker)/stanDevForTalker	# Lobanov 1971

	if (normalize=="bark") {
		f1n <- 26.81*f1/(1960+f1)-0.53
		f2n <- 26.81*f2/(1960+f2)-0.53
		f3n <- 26.81*f3/(1960+f3)-0.53
	} else if (normalize=="log") {
		f1n <- log(f1)
		f2n <- log(f2)
		f3n <- log(f3)
	} else if (normalize=="mel") {
		f1n <- 2595*log(1+f1/700)
		f2n <- 2595*log(1+f2/700)
		f3n <- 2595*log(1+f3/700)
	} else if (normalize=="erb") {
		f1n <- 21.4*log(0.00437*f1+1)
		f2n <- 21.4*log(0.00437*f2+1)
		f3n <- 21.4*log(0.00437*f3+1)
	} else if (normalize=="lobanov" | normalize="z") {
		f1n <- (f1-mean(f1))/sd(f1)
		f2n <- (f2-mean(f2))/sd(f2)
		f3n <- (f3-mean(f3))/sd(f3)
	} else if (normalize=="nearey1") {
		f1n <- log(f1)-mean(log(f1))
		f1n <- log(f2)-mean(log(f2))
		f1n <- log(f3)-mean(log(f3))
	} else if (normalize=="nearey2") {
		f1n <- log(f1)-mean(log(f0))-mean(log(f1))-mean(log(f2))-mean(log(f3))
		f2n <- log(f2)-mean(log(f0))-mean(log(f1))-mean(log(f2))-mean(log(f3))
		f3n <- log(f3)-mean(log(f0))-mean(log(f1))-mean(log(f2))-mean(log(f3))
	} else { # Return untransformed Hz
		f1n <- f1
		f2n <- f2
		f3n <- f3
	}

	# STEP 3: PREPARE FOR PLOTTING	
	f1range <- c(max(f1n), min(f1n))
	f2range <- c(max(f2n), min(f2n))
	
	# put vowels in proper order for polygon
	
	
	if (output=="screen") {
		plot()
	} else if (output=="pdf") {
		Cairo(file="XXXXX", type="pdf", units="in", dpi="auto", pointsize=12, width=6, height=6, bg="white")
	} else if (output=="jpeg") {
		Cairo(file="XXXXX", type="jpeg", units="in", dpi="300", pointsize=12, width=6, height=6, bg="white")
	}
	
}

normalizeVowels <- function(f0=NULL, f1=NULL, f2=NULL, f3=NULL, method, grouping.factor=NULL, inverse=FALSE) {
  # R FUNCTION "normalizeVowels"
  # This function normalizes vowel formants using various normalization formulae, taken from: Adank, P., Smits, R., & van Hout, R. (2004). A comparison of vowel normalization procedures for language variation research. Journal of the Acoustical Society of America, 116, 3099. doi:10.1121/1.1795335.  Original references from that paper are indicated here:
  # bark    <- 26.81*Hz/(1960+Hz)-0.53					        # traunmüller 1990; zwicker & terhardt 1980
  # mel     <- 2595*log(1+Hz/700)							          # stevens & volkmann 1940
  # erb     <- 21.4*log(1+Hz*0.00437)		        				# glasberg & moore 1990
  # zscore  <- (Hz-grandMeanForTalker)/stDevForTalker	  # Lobanov 1971
  # nearey1 <- log(Hz) - mean(log(Hz))					        # Nearey 1978
  # nearey2 <- log(Hz) - sum(mean(log(f0)),mean(log(f1)),mean(log(f2)),mean(log(f3)))
  #
  # VERSION 0.3 (2012 07 19)
  #
  # CHANGELOG
  # VERSION 0.3: minor improvement to error handling.
  #
  # VERSION 0.2: implementation of group-based subsetting for z-score and nearey normalizations.
  #
  # AUTHOR: DANIEL MCCLOY (drmccloy@uw.edu)
  # LICENSED UNDER THE GNU GENERAL PUBLIC LICENSE v3.0: http://www.gnu.org/licenses/gpl.html
  # DEVELOPMENT OF THIS SCRIPT WAS FUNDED IN PART BY THE NATIONAL INSTITUTES OF HEALTH, GRANT # 10186254 TO PAMELA SOUZA

  m <- tolower(method)

  f <- cbind(as.vector(f0),as.vector(f1),as.vector(f2),as.vector(f3))
  #length(unique(sapply(list(f0, f1, f2, f3), length))) == 1

  if (is.null(f)) {
    warning('Missing values: at least one of the arguments (f0, f1, f2, or f3) must be supplied.')
  }

  if (!(m %in% c('bark','mel','log','erb','z','zscore','ztransform','lobanov','logmean','nearey1','nearey2'))) {
    warning('Method must be one of: bark, mel, log, erb, z|zscore|ztransform|lobanov, logmean|nearey1, nearey2')
  }

  if (!is.null(grouping.factor) & !(m %in% c('z','zscore','ztransform','lobanov','logmean','nearey1','nearey2'))) {
    warning('Grouping factor ignored for normalization method \"',method,'.\"')
  }

  if (m=='bark') {
    fn <- 26.81*f/(1960+f)-0.53

  } else if (m=='log') {
    fn <- log(f)

  } else if (m=='mel') {
    fn <- 2595*log(1+f/700)

  } else if (m=='erb') {
    fn <- 21.4*log(0.00437*f+1)

  } else if (is.null(grouping.factor)) {
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

normalizeVowels <- function(f0=NULL, f1=NULL, f2=NULL, f3=NULL, method, grouping.factor=NULL, vowel=NULL) {
  # R FUNCTION "normalizeVowels"
  # MOST CURRENT VERSION ALWAYS AT: https://github.com/drammock/phonR
  #
  # This function normalizes vowel formants using various normalization formulae.  Most of the formulae are taken from Adank et al (2004) and Watt & Fabricius (2002).  Full references are below; schematic formulae and original sources are provided as well.
  # Adank, P., Smits, R., & van Hout, R. (2004). A comparison of vowel normalization procedures for language variation research. Journal of the Acoustical Society of America, 116, 3099. doi:10.1121/1.1795335.
  # Watt, D., & Fabricius, A. H. (2002). Evaluation of a technique for improving the mapping of multiple speakers’ vowel spaces in the F1 ~ F2 plane. (D. Nelson, Ed.) Leeds Working Papers in Linguistics and Phonetics, 9, 159–173.
  #
  # bark        <- 26.81*Hz/(1960+Hz)-0.53					        # Traunmüller 1990; Zwicker & Terhardt 1980
  # mel         <- 2595*log(1+Hz/700)							          # Stevens & Volkmann 1940
  # erb         <- 21.4*log(1+Hz*0.00437)		        				# Glasberg & Moore 1990
  # zscore      <- (Hz-grandMeanForTalker)/stDevForTalker	  # Lobanov 1971
  # nearey1     <- log(Hz) - mean(log(Hz))					        # Nearey 1977
  # nearey2     <- log(Hz) - sum(mean(log(f0)),mean(log(f1)),mean(log(f2)),mean(log(f3)))
  # s-centroid  <- Hz_n/centroid_n                          # see Watt & Fabricius 2002 for full explanation
  #
  # VERSION 0.5 (2012 08 11)
  #
  # CHANGELOG
  # VERSION 0.5: fixed log, mel, and erb calculations to use base-10 instead of base-e.
  #
  # VERSION 0.4: added S-centroid normalization method.
  #
  # VERSION 0.3: minor improvement to error handling.
  #
  # VERSION 0.2: implementation of group-based subsetting for z-score and nearey normalizations.
  #
  # AUTHOR: DANIEL MCCLOY (drmccloy@uw.edu)
  # LICENSED UNDER THE GNU GENERAL PUBLIC LICENSE v3.0: http://www.gnu.org/licenses/gpl.html
  # DEVELOPMENT OF THIS SCRIPT WAS FUNDED IN PART BY THE NATIONAL INSTITUTES OF HEALTH, GRANT # R01DC006014 TO PAMELA SOUZA

  m <- tolower(method)

  f <- cbind(as.vector(f0),as.vector(f1),as.vector(f2),as.vector(f3))

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
#    if (is.null(grouping.factor)) { grouping.factor <- 'noGroups' }
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

#  } else if (is.null(grouping.factor)) {
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

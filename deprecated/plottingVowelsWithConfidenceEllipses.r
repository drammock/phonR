# ############################################################################################################# #
# THIS CODE PLOTS VOWELS AS POINTS IN AN F2xF1 SPACE, PLOTS THE MEAN OF EACH VOWEL USING A UNICODE IPA SYMBOL,  #
# AND DRAWS 95% CONFIDENCE ELLIPSES AROUND THE VOWEL MEANS.  INSTRUCTIONS ARE INCLUDED FOR PLOTTING USING       #
# QUARTZ, X11, AND WIN32 (THE ON-SCREEN DISPLAY SYSTEMS BUILT INTO MAC, LINUX, AND WINDOWS, RESPECTIVELY), AS   #
# WELL AS TWO DIFFERENT METHODS OF PLOTTING DIRECTLY TO FILE.                                                   #
# VERSION 0.2 (2011 11 10)                                                                                      #
#                                                                                                               #
# AUTHORS: DANIEL MCCLOY: (drmccloy@uw.edu) & AUGUST MCGRATH                                                    #
# LICENSED UNDER A CREATIVE COMMONS ATTRIBUTION 3.0 LICENSE: http://creativecommons.org/licenses/by/3.0/        #
# ############################################################################################################# #

# GETTING STARTED
# Packages required for this tutorial: "mixtools" (for calculating the variance-covariance matrices and plotting the ellipses)
library(mixtools)
# One of the two direct-to-file methods also requires the Cairo R package and the Cairo graphics engine (http://www.cairographics.org/download/); see below for details.

# As always, you must first set your working directory and read in your data. 
setwd("/Users/dan/Desktop")    # Mac OSX
setwd("/home/dan/Desktop")     # Linux
setwd("C:\\Users\\dan\\Desktop")  # Windows 7 (that's not a mistake, you need to "escape" the backslashes in the path using other backslashes!)

# Here we import data from: Hillenbrand, Getty, Clark & Wheeler (1995). Acoustic characteristics of American English vowels. Journal of the Acoustical Society of America, 97, 3099-3111.
# Original data acquired here: http://homepages.wmich.edu/~hillenbr/voweldata.html
# Nice, cleaned version of the data is here: http://depts.washington.edu/phonlab/resources/HGCWvowels.txt
hgcw <- read.delim("HGCWvowels.txt", header=T, sep="\t")

# At this point it's often a good idea to take a look at the data. The "head" command will show the first six rows:
head(hgcw)

# note that the "vowel" column may appear messed up on some systems because some versions of R can't handle non-ASCII characters in datafiles. Just in case, we have vowel information redundantly encoded in the "arpabet" and "unicode" columns, which we'll use from here on out for subsetting (also good because it's easier to type "er" than "ɝ").

# if there are any missing data, it's often best to remove them at the outset. In Hillenbrand's study, formants that were unmeasurable were marked as "0", so we remove any records with an F1 or F2 of 0.  (the ! mark is negation, so we're retaining only the records where F1 and F2 are non-zero)
hgcw <- subset(hgcw,!hgcw$F1==0)
hgcw <- subset(hgcw,!hgcw$F2==0)

# often you'll want to segregate the data various ways first, to make computations easier. Here, we separate out males, females, and combine boys and girls into one group: "children"
hgcw.m <- subset(hgcw, hgcw$gender=="m")
hgcw.f <- subset(hgcw, hgcw$gender=="f")
hgcw.c <- subset(hgcw, hgcw$gender=="b" | hgcw$gender=="g")

# Now, further separate out by vowel.  I'm choosing to omit diphthongs and rhotics for now, and for purposes of illustration will only look at females.
hgcw.f.iy <- subset(hgcw.f,hgcw.f$arpabet=="iy")
hgcw.f.ih <- subset(hgcw.f,hgcw.f$arpabet=="ih")
hgcw.f.eh <- subset(hgcw.f,hgcw.f$arpabet=="eh")
hgcw.f.ae <- subset(hgcw.f,hgcw.f$arpabet=="ae")
hgcw.f.aa <- subset(hgcw.f,hgcw.f$arpabet=="aa")
hgcw.f.ah <- subset(hgcw.f,hgcw.f$arpabet=="ah")
hgcw.f.ao <- subset(hgcw.f,hgcw.f$arpabet=="ao")
hgcw.f.uh <- subset(hgcw.f,hgcw.f$arpabet=="uh")
hgcw.f.uw <- subset(hgcw.f,hgcw.f$arpabet=="uw")

# Now I'll make a new set that is all the female data, but only the vowels I'm interested in (the monophthongs). This could be done a few different ways, but "rbind" (bind together as rows) works well. We won't use this right away, but later it will make plotting the raw data points easier.
hgcw.f.mono <- rbind(hgcw.f.iy, hgcw.f.ih, hgcw.f.eh, hgcw.f.ae, hgcw.f.aa, hgcw.f.ah, hgcw.f.ao, hgcw.f.uh, hgcw.f.uw)

# in order to plot the ellipses, we will need to calculate variance/covariance matrices for each vowel. Remember to put F2 first, for two reasons: (1) that's the conventional way to plot vowels (F2 on the horizontal axis, F1 on the vertical) so it's good practice to arrange the columns in a way that reminds you of that fact, and (2) we need F2 first so that the ellipses get plotted in the right place and with the right orientation.
hgcw.f.iy.cov <- cov(cbind(hgcw.f.iy$F2, hgcw.f.iy$F1))
hgcw.f.ih.cov <- cov(cbind(hgcw.f.ih$F2, hgcw.f.ih$F1))
hgcw.f.eh.cov <- cov(cbind(hgcw.f.eh$F2, hgcw.f.eh$F1))
hgcw.f.ae.cov <- cov(cbind(hgcw.f.ae$F2, hgcw.f.ae$F1))
hgcw.f.aa.cov <- cov(cbind(hgcw.f.aa$F2, hgcw.f.aa$F1))
hgcw.f.ah.cov <- cov(cbind(hgcw.f.ah$F2, hgcw.f.ah$F1))
hgcw.f.ao.cov <- cov(cbind(hgcw.f.ao$F2, hgcw.f.ao$F1))
hgcw.f.uh.cov <- cov(cbind(hgcw.f.uh$F2, hgcw.f.uh$F1))
hgcw.f.uw.cov <- cov(cbind(hgcw.f.uw$F2, hgcw.f.uw$F1))

# now we calculate the mean for each vowel. The means are joined together as columns using "c" which stands for "combine".
hgcw.means.f.iy <- c(mean(hgcw.f.iy$F2),mean(hgcw.f.iy$F1))
hgcw.means.f.ih <- c(mean(hgcw.f.ih$F2),mean(hgcw.f.ih$F1))
hgcw.means.f.eh <- c(mean(hgcw.f.eh$F2),mean(hgcw.f.eh$F1))
hgcw.means.f.ae <- c(mean(hgcw.f.ae$F2),mean(hgcw.f.ae$F1))
hgcw.means.f.aa <- c(mean(hgcw.f.aa$F2),mean(hgcw.f.aa$F1))
hgcw.means.f.ah <- c(mean(hgcw.f.ah$F2),mean(hgcw.f.ah$F1))
hgcw.means.f.ao <- c(mean(hgcw.f.ao$F2),mean(hgcw.f.ao$F1))
hgcw.means.f.uh <- c(mean(hgcw.f.uh$F2),mean(hgcw.f.uh$F1))
hgcw.means.f.uw <- c(mean(hgcw.f.uw$F2),mean(hgcw.f.uw$F1))

# Now we collect all the means into one dataframe. If you wanted to use lines to connect the vowel means into a polygon, then make sure you put them in the order that you want to draw the line.
hgcw.means.f.mono <- rbind(hgcw.means.f.iy, hgcw.means.f.ih, hgcw.means.f.eh, hgcw.means.f.ae, hgcw.means.f.aa, hgcw.means.f.ah, hgcw.means.f.ao, hgcw.means.f.uh, hgcw.means.f.uw)

# this next vector is the labels that we'll use for the vowel means. If you have trouble getting these to display right (or if you can't even see them displaying correctly in the R script window) then comment-out this line and use the one after it instead, which supplies the unicode codepoints for the IPA symbols instead of the actual characters. Make sure the order matches the order of your means from above.
hgcwVowels <- c("i", "ɪ", "ɛ", "æ", "a", "ʌ", "ɔ", "ʊ", "u")
#hgcwVowels <- c("\u0069", "\u026A", "\u025B", "\u00E6", "\u0061", "\u028C", "\u0254", "\u028A", "\u0075")

# Now we're ready to start plotting.  One way to do this is to plot to a window on the screen, which you can resize at will and then save as PDF using the menu command File > Save As (although I've heard reports that File>SaveAs often doesn't work when graphs contain semi-transparent colors, non-standard fonts, or characters outside the ASCII range). On-screen display can be good for tweaking colors, margins, etc, until you get everything just the way you want it, but if you're doing multiple graphs for a presentation or research paper you may want them all to be exactly the same dimensions, and it is easy to mess that up when working with resizable onscreen windows (especially if you create some graphs now, shut down R, and create the rest later). An alternative is to plot directly to a PDF file (or other file type), which makes it easy to hold things like title size, axis alignment, margins, and colors constant across all graphs. 
# There are built-in functions cairo_pdf(), cairo_ps(), and svg() that allow you to plot directly to file, and these are adequate for many people's needs; type ?cairo_pdf on the R command line for more details.  If these functions don't meet your needs, there is a separate package called "Cairo" (note the capital C) that offers a much more flexible plotting engine, and can write directly to PS, PDF, SVG, JPEG, PNG, and TIFF, as well as draw to an on-screen window.
# The good news is that the bulk of the plotting commands are identical for all three methods (cairo_pdf(), CairoPDF(), and the default onscreen plotting via Quartz/X11/Win32).  Default onscreen plotting is the simplest case, so we'll look at that one first.

# ############################################ #
# PLOTTING ON-SCREEN WITH QUARTZ / X11 / WIN32 #
# ############################################ #
# "par" sets the parameters for the plot: margins, fonts, etc. Use whatever font you like as long as it has coverage of the vowel symbols you want to plot. 

# LINUX
par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family="Charis SIL")

# MAC: use the same command as Linux, but beware that for some reason, the Quartz window is resistant to having its parameters changed this way, so after running this line, go into the menu: R > Preferences > Quartz and change the plotting location from its default to whatever you want, and then change it back. I don't know why this works, but it does.

# WINDOWS: font families can't be directly called through "par()" without first defining a new family.  The "windowsFonts" command defines a new font family (that I'm calling "ipa") that can then be referenced in later lines.  Make sure you actually have the Charis SIL font installed or this won't work (you can get it here: http://scripts.sil.org/CharisSIL_download).  Or alternatively, use some other font that has coverage of the IPA glyph range.
windowsFonts(ipa=windowsFont("Charis SIL")) 
par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family="ipa")


# THE REST OF THE PLOTTING COMMANDS ARE THE SAME FOR LINUX / MAC / WINDOWS
# Now plot the individual vowel data points. If you leave out the xlim, ylim, xaxt, and yaxt arguments, you'll get axes automatically and won't need the "axis" commands, but then the axes will be on the left and bottom, and increasing in the wrong direction. But be careful: when specifying the axes in this way you must take care to set them correctly, lest some of your data plot offscreen. For information on the other arguments: do a google search for "Quick R" and look at the "Axes and Text" and "graphical parameters" pages.
plot(hgcw.f.mono$F2, hgcw.f.mono$F1, xaxt="n", yaxt="n", xlim=c(3200,600), ylim=c(1200,300), type="p", pch=19, cex=0.5, col=rgb(0,0,255,75,maxColorValue=255), bg=rgb(0,0,255,50,maxColorValue=255), main="", frame.plot=FALSE)
axis(3, at=600+c(0:26)*100, las=0, col.axis="black", cex.axis=0.8, tck=-.01)
axis(4, at=300+c(0:9)*100, las=2, col.axis="black", cex.axis=0.8, tck=-.01)
# note: the "at=300+c(0:9)*100" part says: "start at 300 and put in 9 more tickmarks, spaced 100 Hz apart"

# Add margin text: axis labels and a graph title.
mtext("HGCW Females: Vowels, Means, and 95% Confidence Ellipses", side=3, cex=1.2, las=1, line=4.5, font=2)
mtext("F2 (Hz)", side=3, cex=1, las=1, line=2.5, font=2)
mtext("F1 (Hz)", side=4, cex=1, las=3, line=2.5, font=2)

# Now plot the ellipses. The "ellipse" command requires the "mixtools" package, so if you forgot to load that at the outset, do it now.  You can also ajust the size of the ellipse using the "alpha" argument (default is alpha=.05); if you wanted +/- one standard deviation, enter alpha=.3173
ellipse(as.vector(hgcw.means.f.iy), as.matrix(hgcw.f.iy.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ih), as.matrix(hgcw.f.ih.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.eh), as.matrix(hgcw.f.eh.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ae), as.matrix(hgcw.f.ae.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.aa), as.matrix(hgcw.f.aa.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ah), as.matrix(hgcw.f.ah.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ao), as.matrix(hgcw.f.ao.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.uh), as.matrix(hgcw.f.uh.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.uw), as.matrix(hgcw.f.uw.cov), type="l", col="blue")

# Finally, plot the means, but plot them as the unicode symbols instead of points by using the "text" command (we saved this for last so that the labels would not be overlapped by the ellipses). The first two arguments supply the X and Y locations for plotting, and the third argument is the vector of actual text labels. The "cex" (character expansion) argument means "make the symbols bigger by 50%" and "font=2" means "make them bold".
text(hgcw.means.f.mono[,1], hgcw.means.f.mono[,2], hgcwVowels, col="black", font=2, cex=1.5)

# All done.  Save using the File>SaveAs menu command (or the Device>Export menu command if you're using RKWard).

# ######################################## #
# PLOTTING DIRECT-TO-FILE WITH cairo_pdf() #
# ######################################## #
# The cairo_pdf(), cairo_ps(), and svg() functions all work the same way: one line of code to initialize the file, then a bunch of plotting commands, then a line of code to close off the file.  The PDF will be written to whatever your working directory is (i.e., the same folder from which you loaded in the data). If you want you can change the working directory now, before writing out the file.

# LINUX / MAC / WINDOWS
# Start with a cairo_pdf("insertFilenameHere.pdf") command to initialize the file, or use svg() or cairo_ps() if you want those output formats instead.  There are various other arguments besides the filename, but for most purposes the only ones you need to know about are width= and height= (specified in inches; both default to 7) and pointsize= (defaults to 12; whatever you set here is the basis for the "cex" arguments in the lines of code below, so this is a way to make ALL text on your graph bigger instead of adjusting several "cex" arguments separately).
cairo_pdf("hillenbrandFemaleMonophthongsWithEllipses.pdf")

# All these commands are the same as we used above for plotting to an onscreen window.  When using cairo_pdf() on Windows, you no longer need to do the "windowsFont() command nor change the par(family=) argument.
par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family="Charis SIL")
plot(hgcw.f.mono$F2, hgcw.f.mono$F1, xaxt="n", yaxt="n", xlim=c(3200,600), ylim=c(1200,300), type="p", pch=19, cex=0.5, col=rgb(0,0,255,75,maxColorValue=255), bg=rgb(0,0,255,50,maxColorValue=255), main="", frame.plot=FALSE)
axis(3, at=600+c(0:26)*100, las=0, col.axis="black", cex.axis=0.8, tck=-.01)
axis(4, at=300+c(0:9)*100, las=2, col.axis="black", cex.axis=0.8, tck=-.01)
mtext("HGCW Females: Vowels, Means, and 95% Confidence Ellipses", side=3, cex=1.2, las=1, line=4.5, font=2)
mtext("F2 (Hz)", side=3, cex=1, las=1, line=2.5, font=2)
mtext("F1 (Hz)", side=4, cex=1, las=3, line=2.5, font=2)
ellipse(as.vector(hgcw.means.f.iy), as.matrix(hgcw.f.iy.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ih), as.matrix(hgcw.f.ih.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.eh), as.matrix(hgcw.f.eh.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ae), as.matrix(hgcw.f.ae.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.aa), as.matrix(hgcw.f.aa.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ah), as.matrix(hgcw.f.ah.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ao), as.matrix(hgcw.f.ao.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.uh), as.matrix(hgcw.f.uh.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.uw), as.matrix(hgcw.f.uw.cov), type="l", col="blue")
text(hgcw.means.f.mono[,1], hgcw.means.f.mono[,2], hgcwVowels, col="black", font=2, cex=1.5)

# After you're done, call dev.off() to close the file.
dev.off()


# ############################################################## #
# PLOTTING DIRECT-TO-FILE WITH CairoPDF() FROM THE Cairo PACKAGE #
# ############################################################## #
# The Cairo plotting functions work almost the same as the built-in functions above, the main difference is in how fonts are specified and drawn. For this to work, you will need the Cairo R package, but you will also need to install the cairo graphics engine (http://www.cairographics.org/). Installing Cairo is MUCH easier on Linux than on Mac, and is unwieldy at best on Windows. Attempt at your own risk.  Brief installation guidance follows:
# LINUX: You should be able to find the packages through your package manager; on Ubuntu-like systems look for "libcairo2" and "libcairo2-dev"
# MAC: It's fairly easy to install Cairo using MacPorts or fink.  See this page for instructions: http://www.cairographics.org/download/
# WINDOWS: Getting the Cairo graphics engine to work on Windows is really tricky, and the guidance on the Cairo Graphics website doesn't fully work. Your best bet is probably to go to the page http://www.gtk.org/download/ and click on Windows (either 32bit or 64bit depending on your computer).  Then scroll down to "Required third party dependencies" and download the "run-time" files for zlib, cairo, and libpng (it is unclear whether you also need fontconfig, freetype, and expat, so you might as well get them too).  Unzip them and gather up all the DLL files from the "bin" folders that were in each zipped package.  Then copy those DLL files into this folder: C:\\Program Files\R\R-2.14.0\bin\i386  (or on 64bit systems, into C:\\Program Files\R\R-2.14.0\bin\x64). Also take the "fonts.conf" file from the fontconfig zip file, and put that into C:\\Program Files\R\R-2.14.0\etc\i386\fonts\ (or on 64bit systems, C:\\Program Files\R\R-2.14.0\etc\x64\fonts\). That should do the trick. Note that if your R installation is newer than 2.14.0, then substitute the correct version number into the folder paths above.

# Assuming you have the Cairo backend already installed, within R it's just a matter of loading the Cairo package:
library(Cairo)

# ...opening the new file:
# (note that CairoPDF() defaults to 6x6 instead of 7x7 like the cairo_pdf() function does, so for comparable results we must specify dimensions here)
CairoPDF("hillenbrandFemaleMonophthongsWithEllipses_Cairo.pdf", width=7, height=7)

# LINUX: Don't use par(family=) to define fonts, use CairoFonts() instead:
par(mfcol=c(1,1), mar=c(1,1,6.5,4.5))
CairoFonts(
regular="Charis SIL:style=Regular",
bold="Charis SIL:style=Bold",
italic="Charis SIL:style=Italic",
bolditalic="Charis SIL:style=Bold Italic,BoldItalic",
symbol="Symbol"
)

# WINDOWS & MAC: don't use the CairoFonts() lines above, they probably won't work.  Define font using par(family=) as before.
par(mfcol=c(1,1), mar=c(1,1,6.5,4.5), family="Charis SIL")

# ...and continuing with the standard plotting commands.  After you're done, close the file with dev.off().
plot(hgcw.f.mono$F2, hgcw.f.mono$F1, xaxt="n", yaxt="n", xlim=c(3200,600), ylim=c(1200,300), type="p", pch=19, cex=0.5, col=rgb(0,0,255,75,maxColorValue=255), bg=rgb(0,0,255,50,maxColorValue=255), main="", frame.plot=FALSE)
axis(3, at=600+c(0:26)*100, las=0, col.axis="black", cex.axis=0.8, tck=-.01)
axis(4, at=300+c(0:9)*100, las=2, col.axis="black", cex.axis=0.8, tck=-.01)
mtext("HGCW Females: Vowels, Means, and 95% Confidence Ellipses", side=3, cex=1.2, las=1, line=4.5, font=2)
mtext("F2 (Hz)", side=3, cex=1, las=1, line=2.5, font=2)
mtext("F1 (Hz)", side=4, cex=1, las=3, line=2.5, font=2)
ellipse(as.vector(hgcw.means.f.iy), as.matrix(hgcw.f.iy.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ih), as.matrix(hgcw.f.ih.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.eh), as.matrix(hgcw.f.eh.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ae), as.matrix(hgcw.f.ae.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.aa), as.matrix(hgcw.f.aa.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ah), as.matrix(hgcw.f.ah.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.ao), as.matrix(hgcw.f.ao.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.uh), as.matrix(hgcw.f.uh.cov), type="l", col="blue")
ellipse(as.vector(hgcw.means.f.uw), as.matrix(hgcw.f.uw.cov), type="l", col="blue")
text(hgcw.means.f.mono[,1], hgcw.means.f.mono[,2], hgcwVowels, col="black", font=2, cex=1.5)
dev.off()


# source("../_lib_R/basic.R") ; file_asym_stats_plain()
# source("../_lib_R/basic.R") ; file_asym_stats_hits()
# source("../_lib_R/basic.R") ; file_asym_from_stats()

cat("sourcing  ../_lib_R/basic.R", "\n")

library(dplyr)

format_decimal_fixed = function(x, k) formatC(round(x, k), k, format = "f")

runif_sorted = function(sampleSize) {
  # a didactic example from exponential funtions (dexp, pexp, qexp, rexp)
  # on a fast way to generate *sorted*  U[0,1]  random numbers:
  n  = sampleSize ; n1 = n+1
  cE = cumsum(rexp(n1)) 
  rU = cE[seq_len(n)]/cE[n1]  
  return(rU)
  # plot(runif_sorted(1000), ylim=0:1, pch=".")
  # abline(0,1/(1000+1), col=adjustcolor(1, 0.5))
} ; runif_sorted

signifDigits = function(digit) 
{
  # Copyright 2018, Franc Brglez
  # AND contributors to
  # https://stackoverflow.com/questions/27767841/how-do-i-determine-the-number-of-significant-figures-in-data-in-r
  #
  # > xChar <- c("4", "4.0", "4.00", "28.382", "120",
  #              +         "82.3", "100", "100.0", "30.0003")       ; xChar
  # [1] "4"       "4.0"     "4.00"    "28.382"  "120"     "82.3"    "100"     "100.0"   "30.0003"
  # > xNum <- xChar
  # > decimal <- grep(".", xChar, fixed=TRUE)          ; decimal
  # [1] 2 3 4 6 8 9
  # > xNum[decimal] <- gsub(".$", "1", xChar[decimal]) ; xNum
  # [1] "4"       "4.1"     "4.01"    "28.381"  "120"     "82.1"    "100"     "100.1"   "30.0001"
  # > xNum <- as.numeric(xNum)                         ; xNum
  # [1]   4.00000000000000   4.10000000000000   4.01000000000000  28.38100000000000 120.00000000000000
  # [6]  82.09999999999999 100.00000000000000 100.09999999999999  30.00010000000000
  # > n =  30  
  # > # true counts
  #   > xSigDig <- vector(length=length(xNum))
  #   > for (i in n:1) xSigDig[xNum == signif(xNum, i)] <- i
  #   > xSigDig
  #   [1] 1 2 3 5 2 3 1 4 6
  #   >   
  
  xChar         = as.character(digit)
  xNum          = xChar
  decimal       = grep(".", xChar, fixed=TRUE) 
  xNum[decimal] = gsub(".$", "1", xChar[decimal])
  xNum          = as.numeric(xNum)
  n       = 30  # maxiumum number of significant digits
  xSigDig = vector(length=length(xNum))
  for (i in n:1) {xSigDig[xNum == signif(xNum, i)] = i}
  
  return(xSigDig)
  # VIGNETTES
  # > signifDigits(4)
  # [1] 1
  # > signifDigits(4.1)
  # [1] 2
  # > signifDigits(4.01)
  # [1] 3
  # > signifDigits(28.382)
  # [1] 5
  # > signifDigits(120)
  # [1] 2
  # > signifDigits(82.3)
  # [1] 3
  # > signifDigits(100)
  # [1] 1
  # > signifDigits(100.1)
  # [1] 4
  # > signifDigits(30.0001)
  # [1] 6
  
} # signifDigits

printVecAsis <- function(x) {
  # Copyright 2018
  # https://stackoverflow.com/questions/10860020/output-a-vector-in-r-in-the-same-format-used-for-inputting-it-into-r
  
  ifelse(length(x) == 1, x, 
    ifelse(is.character(x), 
    paste0("c(", paste(sapply(x, function(a) paste0("\'",a,"\'")), collapse=", "), ")"),
    paste0("c(", paste(x, collapse=", "), ")"))
  )
  # VIGNETTE
  # > x=1:5 ; x
  # [1] 1 2 3 4 5
  # > printVecAsis(x)
  # [1] "c(1, 2, 3, 4, 5)"
} ;# printVecAsis


file_ext = function(x="fileTest.txt") 
{
  # Copyright 2018   
  # Contributors to
  # https://stackoverflow.com/questions/29627833
  # /extract-extension-of-file-from-working-directory-and-check-condition
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
  # for explanation about '1L' and '1' see 
  # https://stackoverflow.com/questions/7014387/whats-the-difference-between-1l-and-1
} # file_ext

file_root = function(path) 
{
  # Copyright 2018   
  # A code snippet from Brian Ripley
  # http://r.789695.n4.nabble.com
  # /Extracting-File-Basename-without-Extension-td878817.html  
  # NOTE: the function file_path_sans_ext(path), from  fileutils, has issues with DEoptim 
  fileBase = basename(path)
  fileRoot = sub("\\.[^.]*$", "", fileBase)
  return(fileRoot)
} # file_root

run_stamp   = function() 
{
  run_stamp = paste("--------------------------------",
    "\nuserId    =", Sys.info()[["user"]], 
    "\nnodeName  =", Sys.info()[["nodename"]], 
    "\nsysName   =", Sys.info()[["sysname"]], 
    "\ndate      =", date      = format(Sys.time(), "%Y%m%d"),
    "\ntimeStamp =", format(Sys.time(), "%d%H%M%S"),
    "\n--------------------------------"
  )
  return(run_stamp)
}

file_header = function(fileName="fileTest.txt") 
{
  # Copyright 2018, Franc Brglez
  userId      = Sys.info()[["user"]] 
  nodeName    = Sys.info()[["nodename"]] 
  sysName     = Sys.info()[["sysname"]] 
  date        = format(Sys.time(), "%Y%m%d")
  timeStamp   = format(Sys.time(), "%d%H%M%S")
  fileExtension = file_ext(fileName)
  fileRoot      = file_root(fileName)
  fileStamped   = paste(sep="", fileRoot, "_", timeStamp, ".", fileExtension)
  
  fileHeader  = paste( "# fileName  =", fileName,
                     "\n# userId    =", userId,
                     "\n# nodeName  =", nodeName,
                     "\n# sysName   =", sysName,
                     "\n# date      =", date,
                     "\n# timeStamp =", timeStamp,
                     "\n#")
  
  return(list(fileHeader=fileHeader, fileStamped=fileStamped, timeStamp=timeStamp))
} # file_header

table2html = function(filepath) 
{
  # Copyright 2018, George Mathew, george2@ncsu.edu
  
  make_table_cell = function(x) {paste("<td>",x,"</td>", sep="")};
  make_table_header = function(x) {paste("<th>",x,"</th>", sep="")};
  style = "<style> td,th {border: 1px solid black;} </style>";
  file_root      = strsplit(filepath, split = ".", fixed = TRUE)[[1]][1];
  file_extension = strsplit(filepath, split = ".", fixed = TRUE)[[1]][2];
  file_html      = paste(file_root,".", "html", sep="")
  body = "";
  rows = "";
  first_row_seen = FALSE;
  con = file(filepath, "r");
  while ( TRUE ) {
    line = readLines(con, n = 1);
    if ( length(line) == 0 ) {
      html = paste0("<html>", style,"<div>", body, "</div><br/>", "<table>", rows, "</table>" ,"</html>", sep="\n");
      fileConn<-file(paste(file_root, ".html", sep=""));
      writeLines(html, fileConn);
      close(fileConn);
      break;
    }
    first_char = substr(line, 1, 1);
    if (first_char == "#") {
      body = paste(body, line, sep = "<br/>");
    } else {
      if (first_row_seen) {
        row = lapply(strsplit(line, "\t"), make_table_cell);
      } else {
        row = lapply(strsplit(line, "\t"), make_table_header);
        first_row_seen = TRUE;
      }
      row = paste(row[[1]], collapse = " ");
      row = paste("<tr>", row, "</tr>", sep="");
      rows = paste(rows, row, sep="");
    }
  }
  close(con);
  cat(sep="", ".. table2html: created a file named as = ", basename(file_html), "\n");
} # table2html
# table2html('fg_asym_pi_darts_unfinished.txt')

distance_xyz = function(vec1=c(0,0,0), vec2=c(1,1,1)) 
{
  # Copyright 2018, Franc Brglez
  # euclidian distance of two vectors
  d = length(vec1) ; dist = 0.
  for (i in seq_len(d)) {
    dif = vec1[i] - vec2[i]
    dist = dist + dif*dif
  }
  return(sqrt(dist))
  # VIGNETTE
  # > distance.xyz()
  # [1] 1.732051 # = sqrt(1+1+1)
} # distance_xyz

bin2dec = function(x="10111")
{
  dec = strtoi(x, base=2)
  return(dec)
  # VIGNETTE (FB)
  # > bin2dec("11111")
  # [1] 31
  # > bin2dec("10111")
  # [1] 23
  # > bin2dec("10001")
  # [1] 17
} # bin2dec

int2bin<- function(x, ndigits)
{
  # Copyright, 2011, https://stat.ethz.ch/pipermail/r-help/2011-December/297262.html
  # FB to check for speed with int2bin from rnn-package
  base <- array(NA, dim=c(length(x), ndigits))
  for (q in 1:ndigits) {
    base[, ndigits-q+1] <- (x %% 2)
    x <- (x %/% 2)
  }
  bin <- apply(base,1,paste,collapse="")
  return(bin)
  # VIGNETTE (FB)
  # > int2bin(7, 4)
  # [1] "0111"
  # > int2bin(15, 4)
  # [1] "1111"
  # > int2bin(3, 5)
  # [1] "00011"
  # > int2bin(4, 5)
  # [1] "00100"
  # > 
} # int2bin

# bin2dec = function(coordB="10111", xLB=-4, dx=0.25) 
# {
#   coordIdx = strtoi(coordB, base=2)
#   coordD   = xLB + coordIdx*dx
#   #print(cbind(coordB, coordD, coordIdx, dx))
#   return(coordD)
# } # bin2dec

file_asym_from_stats = function(fileName="../_lib_R/data/fg_asym_pi_plain_needles2_6H_stats.txt",
                                 yNames=c("piHat", "OFerror", "runtime")) 
{
  # Copyright 2018, Franc Brglez
  thisFunction = "file_asym_from_stats"
  # read statistics from a well-structured tabbed test file that may or may not have censored data
  
  df = read.table(fileName, header=TRUE, colClasses="character") ;# print(df)
  #print(cbind(df$numThrows, df$mean))
  numRows = dim(df)[1] ; numCols = dim(df)[2]
  cat(".. this file stores a data frame with ", numRows, " rows and ", numCols, " columns.
      \nThe columns are named as \n",
      colnames(df), "\n\n") ;# return()
  
  cols2check = c("sampleSize", "censoredPerc", "solverName", "OFname", "xName", "yNames")
  columnsMissing = NULL
  for (i in seq_len(length(cols2check))) {
    name = cols2check[i] 
    idx  = which(colnames(df) == name) 
    if (length(idx) == 0) {columnsMissing = c(columnsMissing, name)}
  }
  if (length(columnsMissing) > 0) {
    stop(paste("\n.. column name missing from the file", fileName, " is \n", columnsMissing, "\n"))    
  }
  # check for column with the name of 'xName'
  idx  = which(colnames(df) == "xName") 
  xNameString = df$xName[1]
  idx = which(colnames(df) == df$xName[1]) 
  if (length(idx) == 0) {
    stop(paste("\n.. column name mismatch with the string value under column \"xName\" :", 
               "\n   the expected name of the adjacent column is \"", df$xName[1], "\" \n\n", sep=""))    
  }
  
  # dfs = filter(df, df$yNames == "piHat")
  # print(dfs)
  # print(dfs$mean)
  # > source("../_lib_R/basic.R") ; file_asym_from_stats()
  # sourcing  ../_lib_R/basic.R 
  # .. this file stores a data frame with  18  rows and  15  columns.
  # 
  # The columns are named as 
  # sampleSize censoredPerc solverName OFname xName numThrows yNames median mean stDev SE cVar max2mean min max 
  # 
  # sampleSize censoredPerc solverName            OFname     xName numThrows yNames   median     mean        stDev        SE      cVar max2mean
  # 1        100            0   needles2 pi_plain_needles2 numThrows        10  piHat        3 3.164048    0.2678208   0.02678   0.08465    1.355
  # 2        100            0   needles2 pi_plain_needles2 numThrows       100  piHat 3.157895  3.14578    0.0682792  0.006828   0.02171    1.072
  # 3        100            0   needles2 pi_plain_needles2 numThrows      1000  piHat 3.141361 3.142147   0.02009999   0.00201  0.006397    1.019
  # 4        100            0   needles2 pi_plain_needles2 numThrows     10000  piHat  3.14169 3.142187  0.006448923 0.0006449  0.002052    1.006
  # 5        100            0   needles2 pi_plain_needles2 numThrows    100000  piHat 3.141493 3.141608   0.00215543 0.0002155 0.0006861    1.002
  # 6        100            0   needles2 pi_plain_needles2 numThrows   1000000  piHat 3.141641 3.141625 0.0006597609 6.598e-05   0.00021        1
  # min      max
  # 1        3 4.285714
  # 2        3 3.370787
  # 3 3.099174 3.201708
  # 4 3.129564 3.160556
  # 5 3.137124 3.147789
  # 6 3.139507 3.143181
  # [1] "3.164048" "3.14578"  "3.142147" "3.142187" "3.141608" "3.141625"
  # > 
} # file_asym_from_stats
  
file_asym_stats_plain = function(fileName="../_lib_R/data/fg_asym_pi_plain_darts_3.txt",
                           solverName="darts", OFname="pi_plain_darts", xName="numThrows", 
                           yNames=c("piHat", "OFerror", "runtime")) 
{
  # Copyright 2018, Franc Brglez
  thisFunction = "file_asym_stats_plain"
  # read data from  file WITH NO information  about censoring, then create a table of statistics
  df = read.table(fileName, header=TRUE, colClasses="character") ;# print(df)
  # x=df$probeCnt ; print(x) ; x=df[[xName]] ; print(x) ;# is OK but x=df$xName is NOT OK
  # print(df) ; x=df[[xName]] ; y1=df[[yNames[1]]] ; y2=df[[yNames[2]]] ; print(cbind(x,y1,y2)) ;  return() 
  numRows = dim(df)[1]
  numCols = dim(df)[2]
  cat(".. stored file contents in a data frame with ", numRows, " rows and ", numCols, " columns! \n")
  idx  = which(colnames(df) == "isCensored" )
  if (length(idx) != 0) {
    stop(paste(sep="", "\n!!ERROR!! within function = ", thisFunction, 
               "\nThe column name = isCensored SHOULD NOT be included in the file", fileName, "\n"))
  }
  # create the name of fileStats and prepare the header
  fileExtension = file_ext(fileName)
  fileRoot      = file_root(fileName)
  fileStats     = paste(fileRoot, "_stats.", fileExtension, sep="")
  fileStatsPath = paste(getwd(), "/", fileStats, sep="")
  
  header = c(
    file_header(fileStats)$fileHeader,
    paste("# command   = ", thisFunction, "(fileName=", fileName, 
          ", solverName=", solverName, ", ...) \n#", sep="")
  )
  cat(header,"\n")
  write(header, file=fileStatsPath, ncolumns=1, append=FALSE, sep="\t")
  # write column names for all data rows in fileStatsPath 
  colNames = c("sampleSize", "censoredPerc", "solverName", "OFname" ,"xName", xName, "yNames", 
               "median",  "mean",  "stDev",  "SE",  "cVar",  "max2mean", "min",  "max") 
  write(colNames, file=fileStatsPath, ncolumns=length(colNames), append=TRUE, sep="\t") 
  
  # subset data from table by 'xName'
  # nRows = length(df[[xName]])
  xValues = sort(unique(df[[xName]]))    
  for (xIdx in seq_len(length(xValues))) {
    xValue = xValues[xIdx]
    dfs = df[df[[xName]] == xValue ,]     ;# print(xValue) ; print(dfs) ; print(dfs[[xName]])
    numRows = dim(dfs[dfs[[xName]] == xValue ,])[2] - 1 ; # "-1" subtracts the header row!!!
    sampleSize = as.integer(length(dfs[[xName]]))  
    rowF = c(sampleSize, solverName, xName, xValue)
    sampleSize = as.integer(length(dfs[[xName]])) 
    censored = 0
    #cat("xValue=", xValue, " numRows=", numRows, " sampleSize=", sampleSize, " censoredPerc=", censored, "\n")
    rowF = c(sampleSize, censored, solverName, OFname, xName, xValue)
    
    for (j in 1:length(yNames)) {
      yName = yNames[j] ;# print(yName)
      tmp   = as.numeric(dfs[[yName]]) ;# print(tmp)
      mean  = mean(as.numeric(tmp)) ;# print(cbind(j, yName, tmp, mean))
      stDev = sd(as.numeric(tmp))
      #
      SE     = format(stDev/sqrt(sampleSize), digits=4, format="e")
      if (mean > 1e-8) {
        cVar     = format(stDev/mean, digits=4, format="e")
        max2mean = format(max(as.numeric(tmp))/mean, digits=4, format="e")
      } else {
        cVar    = NA
        max2mean = NA
      }
      median = format(median(as.numeric(tmp)), digits=7, format="e") 
      mean   = format(mean, digits=7, format="e")
      stDev  = format(stDev, digits=7, format="e")
      min    = format(min(as.numeric(tmp)), digits=7, format="e")
      max    = format(max(as.numeric(tmp)), digits=7, format="e")
      row    = c(rowF, yName, median, mean, stDev, SE,  cVar, max2mean, min, max)
      #print(row)
      write(row, file=fileStatsPath, ncolumns=length(colNames), append=TRUE, sep="\t")    
    }    
  }
  cat(sep="", ".. created an asymptotic table of statistics named as = ", basename(fileStatsPath), "\n")
  # convert the file into easier-to-read html file:
  table2html(fileStatsPath)
  return()
  
  # VIGNETTE (FB)
  # > df[df[xName] == xVals[2]  & df$isCensored == 0,]$probeCnt
  # [1] "452" "699" "438" "233"
  # > df[df[xName] == xVals[2]  & df$isCensored == 0,][[yNames[1]]]
  # [1] "452" "699" "438" "233"
  # 
  # > foo = NULL
  # > foo = c(a=1)
  # > foo = c(foo, b=2)
  # > foo = c(foo, c=3)
  # > foo
  # a b c 
  # 1 2 3 
  # > foo$a
  # Error in foo$a : $ operator is invalid for atomic vectors
  # > foo[1]
  # a 
  # 1 
  # > as.numeric(foo[1])
  # [1] 1
  # > foo[["a"]]
  # [1] 1
  # > foo[["b"]]
  # [1] 2
  # > foo[["c"]]
  # [1] 3
  # > 
} # file_asym_stats_plain

file_asym_stats_hits  = function(fileName="../_lib_R/data/fg_asym_pi_signif_darts_3.txt", 
                                 solverName="darts", OFname="pi_signif_darts", xName="signifDigits", 
                                 yNames=c("OFerror", "numThrows", "runtime"))
{
  # Copyright 2018, Franc Brglez
  thisFunction = "file_asym_stats_hits"
  # read data from file that may have censored data and compute statistics
  df = read.table(fileName, header=TRUE, colClasses="character") 
  # x=df$signifDigits ; print(x) ; x=df[[xName]] ; print(x) ;# is OK but x=df$xName is NOT OK
  #print(df) ; x=df[[xName]] ; y1=df[[yNames[1]]] ; y2=df[[yNames[2]]] ; print(cbind(x,y1,y2)) ;#  return() 
  numRows = dim(df)[1]
  numCols = dim(df)[2]
  cat(".. stored file contents in a data frame with ", numRows, " rows and ", numCols, " columns! \n")
  idx  = which(colnames(df) == "isCensored" )
  if (length(idx) == 0) {
    stop(paste(sep="", "\n!!ERROR!! within function = ", thisFunction, 
               "\nThe column name = isCensored MUST be included in the file", fileName, "\n"))
  }
  # check if there are rows with censored data
  dfCensored = df[df[["isCensored"]] == TRUE ,]
  numCensoredRows = dim(dfCensored)[1]
  if (numCensoredRows > 0) {
    cat("\n**** WARNING *****",
        "\n     there are ", numCensoredRows, " rows with censored data!!!!!",
        "\n******************\n\n")
  }
  # access and process only the rows where isCensored == FALSE
  # i.e. each of these data rows is considered a "hit"
  dfHits = df[df[["isCensored"]] == FALSE ,] ;# print(dfHits)  
  
  # create the name of fileStats and prepare the header
  fileExtension = file_ext(fileName)
  fileRoot      = file_root(fileName)
  fileStats     = paste(fileRoot, "_stats.", fileExtension, sep="")
  fileStatsPath = paste(getwd(), "/", fileStats, sep="")
  
  header = c(
    file_header(fileStats)$fileHeader,
    paste("# command   = ", thisFunction, "(fileName=", fileName, 
          ", solverName=", solverName, ", ...) \n#", sep="")
  )
  cat(header,"\n")
  write(header, file=fileStatsPath, ncolumns=1, append=FALSE, sep="\t")
  # write column names for all data rows in fileStatsPath 
  colNames = c("sampleSize", "censoredPerc", "solverName", "OFname", "xName", "signifDigits", "yNames", 
               "median",  "mean",  "stDev",  "SE",  "cVar", "max2mean", "min",  "max") ;#  print(colNames)
  write(colNames, file=fileStatsPath, ncolumns=length(colNames), append=TRUE, sep="\t") 
  
  # subset data from dfHits by 'xName'
  nRows   = length(dfHits[[xName]])        ;#  print(nRows)
  xValues = sort(unique(dfHits[[xName]]))  ;#  print(colNames)
  
  for (xIdx in seq_len(length(xValues))) {
    xValue = xValues[xIdx]
    numRows = dim(df[df[[xName]] == xValue ,])[1]  
    dfs = dfHits[dfHits[[xName]] == xValue ,]     ;# print(xValue) ; print(dfs) ; print(dfs[[xName]])
    sampleSize = as.integer(length(dfs[[xName]])) 
    censored = round(100*(numRows - sampleSize)/numRows, 4)
    #cat("xValue=", xValue, " numRows=", numRows, " sampleSize=", sampleSize, " censoredPerc=", censored, "\n")
    rowF = c(sampleSize, censored, solverName, OFname, xName, xValue)
    for (j in 1:length(yNames)) {
      yName = yNames[j] ;# print(yName)
      tmp   = as.numeric(dfs[[yName]]) ;# print(tmp)
      mean  = mean(as.numeric(tmp))
      stDev = sd(as.numeric(tmp))
      #
      SE     = format(stDev/sqrt(sampleSize), digits=4, format="e")
      if (mean > 1e-8) {
        cVar     = format(stDev/mean, digits=4, format="e")
        max2mean = format(max(as.numeric(tmp))/mean, digits=4, format="e")
      } else {
        cVar    = NA
        max2mean = NA
      }
      median = format(median(as.numeric(tmp)), digits=7, format="e") 
      mean   = format(mean, digits=7, format="e")
      stDev  = format(stDev, digits=7, format="e")
      min    = format(min(as.numeric(tmp)), digits=7, format="e")
      max    = format(max(as.numeric(tmp)), digits=7, format="e")
      row    = c(rowF, yName, median, mean, stDev, SE,  cVar, max2mean, min, max)
      #print(row)
      write(row, file=fileStatsPath, ncolumns=length(colNames), append=TRUE, sep="\t")
    }
  }
  cat(sep="", ".. for statistics of this experiment, see table = ", basename(fileStatsPath), "\n")
  # convert the file into easier-to-read html file:
  table2html(fileStatsPath)
  return()

  # VIGNETTES
  # > df[df[xName] == xVals[2]  & df$isCensored == 0,]$probeCnt
  # [1] "452" "699" "438" "233"
  # > df[df[xName] == xVals[2]  & df$isCensored == 0,][[yNames[1]]]
  # [1] "452" "699" "438" "233"
  # 
  # > foo = NULL
  # > foo = c(a=1)
  # > foo = c(foo, b=2)
  # > foo = c(foo, c=3)
  # > foo
  # a b c 
  # 1 2 3 
  # > foo$a
  # Error in foo$a : $ operator is invalid for atomic vectors
  # > foo[1]
  # a 
  # 1 
  # > as.numeric(foo[1])
  # [1] 1
  # > foo[["a"]]
  # [1] 1
  # > foo[["b"]]
  # [1] 2
  # > foo[["c"]]
  # [1] 3
  # > 
} ;# file_asym_stats_hits




golombCover = function(L=6, M=4, seedInit=-1, tolC=0.0001) {
  
  # Copyright, 2018, Franc Brglez
  # https://en.wikipedia.org/wiki/Golomb_ruler
  
  if (seedInit < 0) {seedInit = round(0.5 + 9999*runif(1))} ; set.seed(seedInit)
  
  sizeCliq  = 0.5*M*(M-1)
  targetCov = sizeCliq/L
  
  trials = 0 ; cover = 0 ; labels = NULL 
  while (cover < targetCov - tolC) {
    labels = unique(c(labels, ceiling(L*runif(1))))   
    cover=length(unique(labels))/L  
    if(trials > 10000) {break} 
    trials = trials + 1 
  } 
  #print(prettyNum(rbind(L, M, sizeCliq, targetCov, tolC, seedInit, cover, trials)))
  # we use variable name labels if we try to format number under (rbind(...))
  print(rbind(L, M, sizeCliq, targetCov, tolC, seedInit, cover, trials))
  cat("labels =", sort(labels), "\n")
} # golombCover

golombLabels = function(ruler=c(0,1,4,10,18,23,25)) {
  
  # 7, 25
  # 0 1 4 10 18 23 25
  # 0 1 7 11 20 23 25
  # 0 1 11 16 19 23 25
  # 0 2 3 10 16 21 25
  # 0 2 7 13 21 22 25
  
  M = length(ruler)
  L = max(ruler)
  sizeCliq  = 0.5*M*(M-1)
  cnt = 0
  labels = NULL
  for (i in 1:M) {
    for (j in i:M) {
      if (i != j) {
        cnt = cnt+1
        #print(cbind(cnt,i,j))
        dif = abs(ruler[i] - ruler[j])
        labels = c(labels, dif)
      }
    }
  }
  labels     = sort(labels)
  labelsUniq = unique(labels)
  cat("\n M              =", M,
      "\n L              =", L,
      "\n sizeCliq       =", sizeCliq,
      "\n ruler          =", ruler,
      "\n labels         =", labels,
      "\n labelsSize     =", length(labels),
      "\n labelsUniq     =", labelsUniq,
      "\n labelsUniqSize =", length(labelsUniq),
      "\n cover          =", length(labelsUniq)/L,
      "\n")
} # golombLabels



golombRulers = function(marks=7) 
{
  thisFunction = "golombRulers"
  if (marks == 4) {
    return(c(0,1,4,6))
  } else if (marks == 5) {
    return(c(0,1,4,9,11))
  } else if (marks == 6) {
    return(c(0,1,4,10,12,17))
  } else if (marks == 7) {
    return(c(0,1,4,10,18,23,25))
  } else if (marks == 8) {
    return(c(0,1,4,9,15,22,32,34))
  } else if (marks == 9) {
    return(c(0,1,5,12,25,27,35,41,44))
  } else if (marks == 10) {
    return(c(0,1,6,10,23,26,34,41,53,55))
  } else if (marks == 11) {
    return(c(0,1,4,13,28,33,47,54,64,70,72))
  } else if (marks == 12) {
    return(c(0,2,6,24,29,40,43,55,68,75,76,85))
  } else if (marks == 13) {
    return(c(0,2,5,25,37,43,59,70,85,89,98,99,106))
  } else if (marks == 14) {
    return(c(0,4,6,20,35,52,59,77,78,86,89,99,122,127))
  } else if (marks == 15) {
    return(c(0,4,20,30,57,59,62,76,100,111,123,136,144,145,151))
  } else if (marks == 16) {
    return(c(0,1,4,11,26,32,56,68,76,115,117,134,150,163,168,177))
  } else if (marks == 17) {
    return(c(0,5,7,17,52,56,67,80,81,100,122,138,159,165,168,191,199))
  } else if (marks == 18) {
    return(c(0,2,10,22,53,56,82,83,89,98,130,148,153,167,188,192,205,216))
  } else if (marks == 19) {
    return(c(0,1,6,25,32,72,100,108,120,130,153,169,187,190,204,231,233,242,246))
  } else if (marks == 20) {
    return(c(0,1,8,11,68,77,94,116,121,156,158,179,194,208,212,228,240,253,259,283))
  } else if (marks == 21) {
    return(c(0,2,24,56,77,82,83,95,129,144,179,186,195,255,265,285,293,296,310,329,333))
  } else if (marks == 22) {
    return(c(0,1,9,14,43,70,106,122,124,128,159,179,204,223,253,263,270,291,330,341,353,356))
  } else if (marks == 23) {
    return(c(0,3,7,17,61,66,91,99,114,159,171,199,200,226,235,246,277,316,329,348,350,366,372))
  } else if (marks == 24) {
    return(c(0,9,33,37,38,97,122,129,140,142,152,191,205,208,252,278,286,326,332,353,368,384,403,425))
  } else if (marks == 25) {
    return(c(0,12,29,39,72,91,146,157,160,161,166,191,207,214,258,290,316,354,372,394,396,431,459,467,480))
  } else if (marks == 26) {
    return(c(0,1,33,83,104,110,124,163,185,200,203,249,251,258,314,318,343,356,386,430,440,456,464,475,487,492))
  } else if (marks == 27) {
    return(c(0,3,15,41,66,95,97,106,142,152,220,221,225,242,295,330,338,354,382,388,402,415,486,504,523,546,553))
  } else {
    stop(paste(sep="", "\n!!ERROR!! within function = ", thisFunction, 
               "\nThe value of marks = ", marks, "  is not supported \n"))
  }
} # golombRulers

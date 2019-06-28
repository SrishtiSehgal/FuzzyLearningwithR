##############################
#REFERENCES
##############################

#https://www.tutorialspoint.com/r/r_data_types.htm
#https://www.researchgate.net/profile/Robert_Babuska/publication/281579993_Fuzzy_Clustering_Algorithms_with_Applications_to_Rule_Extraction/links/5602fdb308ae596d2591c1bb/Fuzzy-Clustering-Algorithms-with-Applications-to-Rule-Extraction.pdf
##############################
#PACKAGE INSTALLATION
##############################
if(require(tictoc)){
  install.packages("tictoc",repos = "http://cran.us.r-project.org")
}
library(tictoc) # for time
install.packages("pracma",repos = "http://cran.us.r-project.org")
library(pracma) # for (strcmp)
install.packages("frbs",repos = "http://cran.us.r-project.org")
library(frbs)
install.packages("e1071",repos = "http://cran.us.r-project.org")
library(e1071)

##############################
#ERROR FUNCTIONS
##############################
#MSE
mse_error <- function(x,y) {
  lengthOfX= length(x);
  lengthOfY= length(y);
  mse = -1
  if(lengthOfX != 0 && lengthOfY == lengthOfY){
     mse <- sum( (x-y)*(x-y)) / lengthOfX
  }
 return(mse)
}

#RMSE
rmse_error <- function(x,y) {
  rmse = -1
  mse  = mse_error(x,y)
  if(mse >= 0){
    rmse = sqrt(mse_error(x,y))
  }
 return(rmse)
}

#MAE
mae_error <- function(x,y) {
  lengthOfX= length(x);
  lengthOfY= length(y);
  mae = -1
  if(lengthOfX != 0 && lengthOfY == lengthOfY){
     mae <- sum( abs(x-y)) / lengthOfX
  }
 return(mae)
}

##############################
#INPUT DATA FROM CONFIG FILE
##############################
#args         = commandArgs(trailingOnly=TRUE)
s_ConfigFile = "C:/Users/sehgals/Desktop/fuzzy-rule-based-systems/fuzzy-rule-based-system.cfg" #args[1]
s_nl    = '\n'
s_Comma = ',' 


# SET DEFAULTS

s_RunID                              = '1'
s_InputDirectory                     = './'
s_OutputDirectory                    = './'
s_OutputFileBaseName                 = "frbs"
s_ShuffleTheInputData                = 'no'
s_TrainingTestingProportionSplit     = '0'
s_ComputePredictionsOnTheTrainingSet = "yes"
s_ComputePredictionsOnTheTestingSet  = "no"
s_CreateLogFile                      = 'no'
s_MethodType                         = "WM"
s_NbrOfLabels                        = "3"
s_TypeOfMembrFunct                   = "GAUSSIAN"
s_TypeTnorm                          = "MIN"
s_TypeSnorm                          = "MAX"
s_TypeDefuz                          = "WAM"
s_TypeOfImplicationFunction          = "ZADEH"
s_Name                               = "Sim-0"

if( file.access(s_ConfigFile) == -1) {
	stop(sprintf("Specified file ( %s ) does not exist", s_ConfigFile))
} else {
	lines <- readLines(s_ConfigFile) # file to string
}

#remove spaces
lines <- gsub(" ", "", lines);

# string tokenizer, creating a list composed of pairs of strings
variableValuePairs <- strsplit(lines, "=")

# Get the indices of those lines that are not comments.
indices <- which( substring(variableValuePairs,1,1) != '#' );

# Retain only those list elements belonging to non-commented lines.
variableValuePairs <- variableValuePairs[indices];

# ON THE FLY, DEFINE A FUNC THAT TAKES PAIRS, 
# CREATE A VARIABLE OUT OF THE FIRST ELEMENT OF THE PAIR 
# AND ASSIGN (assign) TO IT A VALUE GIVEN BY THE SECOND ELEMENT OF THE PAIR.
# THEN APPLY THE FUNC TO EACH MEMBER OF THE LIST OF PAIRS (lapply).
s <-lapply(variableValuePairs, function(x) assign(x[1], x[2], envir = globalenv()))

##############################
#ERROR CHECKING
##############################

if( isFALSE(exists("s_ProblemType")) || isTRUE(identical(s_ProblemType, NULL))){
  cat('ERROR: (s_ProblemType) is not defined. It must be in {classification, regression}.\nRun Aborted.\n', sep="", collapse="")
  quit()
}

s_FullPathInputData  = paste(s_InputDirectory ,s_InputDataFile, sep="", collapse="")
if( isFALSE(file.exists(s_FullPathInputData))){
  cat('ERROR: Input File (', s_FullPathInputData, ') does not exist.\nRun Aborted.\n', sep="", collapse="")
  quit()
}

# Record a log file (if requested).
if( exists("s_CreateLogFile")      == TRUE  && isTRUE(identical(s_CreateLogFile, "yes"))){
   s_OutputLogFile = paste(s_OutputDirectory, s_OutputFileBaseName,".log", sep="", collapse="")
   cat('Log File = (', s_OutputLogFile, ').\n\n', sep = "", collapse="")
   sink(s_OutputLogFile)
   cat('-------------------------------------\n'  , sep="", collapse="")
   cat('   FUZZY RULE BASED SYSTEM   	   \n'  , sep="", collapse="")
   cat('-------------------------------------\n\n', sep="", collapse="")
}

# Output Files
s_ErrorsOutputFileTra         = paste(s_OutputDirectory ,s_OutputFileBaseName, "-errors-tra-"     , s_RunID, ".csv", sep="", collapse="")
s_PredAndObsCsvOutputFileTra = paste(s_OutputDirectory ,s_OutputFileBaseName, "-pred-and-obs-tra-", s_RunID, ".csv", sep="", collapse="")
s_ErrorsOutputFileTst         = paste(s_OutputDirectory ,s_OutputFileBaseName, "-errors-tst-"     , s_RunID, ".csv", sep="", collapse="")
s_PredAndObsCsvOutputFileTst = paste(s_OutputDirectory ,s_OutputFileBaseName, "-pred-and-obs-tst-", s_RunID, ".csv", sep="", collapse="")

##############################
#LOAD DATA
##############################

# first row contains variable names, comma is separator
# assign the variable id to row names
# note the / instead of \ on mswindows systems

#mydata <- read.table("c:/mydata.csv", header=TRUE, sep=",", row.names="id")

cat('LOADING DATA MATRIX\n', sep="", collapse="")
dataMatrix      = read.csv(s_FullPathInputData, header=T)
cat('LOADING DATA MATRIX: DONE\n', sep="", collapse="")
nbrOfRows       = nrow(dataMatrix)
nbrOfObjects    = nbrOfRows

##############################
#SHUFFLE THE DATA
##############################
if(strcmp(s_ShuffleTheInputData,"yes")==TRUE){
  dataMatrixShuffled <- dataMatrix[sample(nbrOfObjects), ] 
}else{
  dataMatrixShuffled = dataMatrix
}

##############################
#DATA SPLIT
##############################
d_TrainingTestingProportionSplit = as.numeric(s_TrainingTestingProportionSplit)

nbrOfObjectsTra = nbrOfObjects
if(d_TrainingTestingProportionSplit != 0){
  nbrOfObjectsTra = as.integer(d_TrainingTestingProportionSplit * nbrOfObjects) 
}

nbrOfColumns    =  ncol(dataMatrix)
targetVariable  = nbrOfColumns
nbrOfPredictors = nbrOfColumns - 1
writeLines('\n')
writeLines(paste('Nbr of Rows: ', nbrOfRows    , sep="", collapse=""))
writeLines(paste('Nbr of Cols: ', nbrOfColumns , sep="", collapse=""))
#writeLines(paste('Nbr of Rows: ', nbrOfRows   , sep="", collapse=""), fs_OutputFile)
#writeLines(paste('Nbr of Cols: ', nbrOfColumns, sep="", collapse=""), fs_OutputFile)
vl1_AllObjects         <- (1:nbrOfObjects)
vl1_ObjectsInTheTra    <- (1:nbrOfObjectsTra)
vl1_ObjectsInTheTst    <- ( (nbrOfObjectsTra+1):nbrOfObjects )
vl1_AllVariables       <- (1:nbrOfColumns)
vl1_PredictorVariables <- (1:nbrOfPredictors)

if(identical(s_ComputePredictionsOnTheTrainingSet, NULL) || identical(s_ComputePredictionsOnTheTrainingSet, "")){
  s_ComputePredictionsOnTheTrainingSet = "no"
}  
if(identical(s_ComputePredictionsOnTheTestingSet, NULL) || identical(s_ComputePredictionsOnTheTestingSet, "")){
  s_ComputePredictionsOnTheTestingSet = "yes"
}  

rangeDataInput <- apply(dataMatrix [, -ncol(dataMatrix)], 2, range)
rangeDataInput <- apply(dataMatrix , 2, range)

##############################
#TRAINING SET
##############################
tra                 <- dataMatrixShuffled[  1:nbrOfObjectsTra, 1:nbrOfColumns]
observedTra         <- matrix        (tra[  , targetVariable ], ncol = 1)
lengthOfObservedTra = length(observedTra)

##############################
#TESTING SET
##############################
if(d_TrainingTestingProportionSplit != 0){
tst                 <- dataMatrixShuffled[vl1_ObjectsInTheTst, vl1_AllVariables]
tstNoTarget         <- tst[ , vl1_PredictorVariables]
observedTst         <- matrix(tst[ , targetVariable        ], ncol = 1)
lengthOfObservedTst = length(observedTst)
}else{
 # load the testing set
 s_FullPathDataTst   = paste(s_InputDirectory ,s_InputDataFileTst, sep="", collapse="")
 dataMatrixTst       = read.csv(s_FullPathDataTst, header=T)
 nbrOfRowsTst        = nrow(dataMatrixTst)
 nbrOfObjectsTst     = nbrOfRowsTst
 vl1_ObjectsInTheTst = (1:nbrOfObjectsTst)
 tstNoTarget         <-        dataMatrixTst[ , vl1_PredictorVariables]
 observedTst         <- matrix(dataMatrixTst[ , targetVariable        ], ncol = 1)
 lengthOfObservedTst = length(observedTst)
}

writeLines(paste('Nbr of objects in the Training set: ', lengthOfObservedTra, '\n', sep="", collapse=""))
writeLines(paste('Nbr of objects in the Testing  set: ', lengthOfObservedTst, '\n', sep="", collapse=""))

##############################
#METHOD DESCRIPTION
##############################
#Method name, Description           , FRBS model, Grouping, Tasks
#FH.GBML  , Ishibuchi's method based on hybridization of "GFS.GCCL" and the Pittsburgh approach, FRBCS , Genetic fuzzy systems, Classification
#FRBCS.CHI , FRBCS based on Chi's technique, FRBCS , Space partition, Classification
#FRBCS.W  , FRBCS based on Ishibuchi's technique using weight factor, FRBCS ,Space partition, Classification
#GFS.GCCL , Ishibuchi's method based on genetic cooperative competitive learning, FRBCS , Genetic fuzzy systems, Classification
#SLAVE    , Structural learning algorithm on vague environment, FRBCS ,Genetic fuzzy systems, Classification
#ANFIS    , Adaptive-network-based fuzzy inference system, TSK, Fuzzy neural networks, Regression
#DENFIS   , Dynamic evolving neural fuzzy inference system, CLUSTERING , Clustering , Regression
#FS.HGD   , FRBS using heuristics and gradient descent method, TSK ,Gradient descent , Regression
#GFS.FR.MOGUL ,Genetic fuzzy for fuzzy rule learning based on the MOGUL methodology, APPROXIMATE ,Genetic fuzzy systems, Regression
#GFS.THRIFT , Genetic fuzzy system based on Thrift's method , MAMDANI ,Genetic fuzzy systems, Regression
#HYFIS    , Hybrid neural fuzzy inference system, MAMDANI ,Fuzzy neural networks, Regression
#SBC      , Subtractive clustering , CLUSTERING , Clustering , Regression
#WM       , Wang and Mendel's technique, MAMDANI , Space partition, Regression
#FIR.DM   , Fuzzy inference rules by descent method , TSK ,Gradient descent ,Regression
#

if(0){# Test data from the package (frbs).
   ##################################
   ## I. Regression Problem
   ###################################
   ## In this example, we just show how to predict using Wang and Mendel's technique but
   ## users can do it in the same way for other methods.
   data.train <- matrix(c(5.2, -8.1, 4.8, 8.8, -16.1, 4.1, 10.6, -7.8, 5.5, 10.4, -29.0,
   5.0, 1.8, -19.2, 3.4, 12.7, -18.9, 3.4, 15.6, -10.6, 4.9, 1.9,
   -25.0, 3.7, 2.2, -3.1, 3.9, 4.8, -7.8, 4.5, 7.9, -13.9, 4.8,
   5.2, -4.5, 4.9, 0.9, -11.6, 3.0, 11.8, -2.1, 4.6, 7.9, -2.0,
   4.8, 11.5, -9.0, 5.5, 10.6, -11.2, 4.5, 11.1, -6.1, 4.7, 12.8,
   -1.0, 6.6, 11.3, -3.6, 5.1, 1.0, -8.2, 3.9, 14.5, -0.5, 5.7,
   11.9, -2.0, 5.1, 8.1, -1.6, 5.2, 15.5, -0.7, 4.9, 12.4, -0.8,
   5.2, 11.1, -16.8, 5.1, 5.1, -5.1, 4.6, 4.8, -9.5, 3.9, 13.2,
   -0.7, 6.0, 9.9, -3.3, 4.9, 12.5, -13.6, 4.1, 8.9, -10.0,
   4.9, 10.8, -13.5, 5.1), ncol = 3, byrow = TRUE)
   data.fit <- matrix(c(10.5, -0.9, 5.2, 5.8, -2.8, 5.6, 8.5, -0.2, 5.3, 13.8, -11.9,
   3.7, 9.8, -1.2, 4.8, 11.0, -14.3, 4.4, 4.2, -17.0, 5.1, 6.9,
   -3.3, 5.1, 13.2, -1.9, 4.6), ncol = 3, byrow = TRUE)
   newdata <- matrix(c(10.5, -0.9, 5.8, -2.8, 8.5, -0.2, 13.8, -11.9, 9.8, -1.2, 11.0,
   -14.3, 4.2, -17.0, 6.9, -3.3, 13.2, -1.9), ncol = 2, byrow = TRUE)
   range.data<-matrix(c(0.9, 15.6, -29, -0.2, 3, 6.6), ncol=3, byrow = FALSE)

   #############################################################
   ## I.1 Example: Implementation of Wang & Mendel
   #############################################################
   method.type <- "WM"
   ## collect control parameters into a list
   ## num.labels = 3 means we define 3 as the number of linguistic terms
   control.WM <- list(num.labels = 3, type.mf = "GAUSSIAN", type.tnorm = "MIN", type.snorm = "MAX", type.defuz = "WAM", type.implication.func = "ZADEH", name = "Sim-0")
   ## generate the model and save it as object.WM
   startTime <- Sys.time()
      #object.WM <- frbs.learn(data.train, range.data, method.type, control.WM) # ORIGINAL
      object.WM <- frbs.learn(data.train, range.data=NULL, method.type, control.WM)
   endTime <- Sys.time()
   computingTime = as.numeric(difftime(endTime, startTime, units="secs"))
   cat('PREDICTIONS (tra): DONE (', sprintf("%.3f", computingTime ), ') secs\n', sep="", collapse="")
   ## the prediction process
   ## The following code can be used for all methods
   startTime <- Sys.time()
      res <- predict(object.WM, newdata)
   endTime <- Sys.time()
   computingTime = as.numeric(difftime(endTime, startTime, units="secs"))
   cat('PREDICTIONS (tst): DONE', sprintf("%.3f", computingTime ), ') secs\n', sep="", collapse="")
   
   tra = data.train
   tst = newdata
}

cat('METHOD TYPE   = ', sprintf("%s", s_MethodType ),'\n', sep="", collapse="")
nbrOfLabels        = as.integer(s_NbrOfLabels)
cat('nbrOfLabels   = ', sprintf("%d", nbrOfLabels),'\n', sep="", collapse="")

cat('\ncontrolList = ', sep="", collapse="")
controlList = list(num.labels = nbrOfLabels, type.mf = s_TypeOfMembrFunct, type.tnorm = s_TypeTnorm
                  , type.snorm = s_TypeSnorm
                  , type.defuz = s_TypeDefuz, type.implication.func = s_TypeOfImplicationFunction
                  , name=s_Name
                  )
cat(str(controlList), '\n', sep="", collapse="")

##############################
#LEARNING
##############################
cat('LEARNING\n', sep="", collapse="")
startTime <- Sys.time()
  frbsObject <- frbs.learn(tra, range.data=rangeDataInput, method.type = s_MethodType, control = controlList)
endTime <- Sys.time()
computingTime = as.numeric(difftime(endTime, startTime, units="secs"))
cat('LEARNING: DONE (', sprintf("%.3f", computingTime ), ') secs\n', sep="", collapse="")

##############################
#PREDICTION ON TRAINING SET
##############################
if(identical(s_ComputePredictionsOnTheTrainingSet,"yes")){
cat('\nPREDICTION on the training set\n', sep="", collapse="")
   startTime <- Sys.time()
   predTra <- predict(frbsObject, tra[vl1_ObjectsInTheTra, vl1_PredictorVariables ])
   endTime   <- Sys.time()
   computingTime = as.numeric(difftime(endTime, startTime, units="secs"))
   cat('PREDICTIONS (tra): DONE (', sprintf("%.3f", computingTime ), ') secs\n', sep="", collapse="")
   lengthOfPredTra     = length(predTra)

   df_PredTra = cbind(predTra,observedTra)
   colnames(df_PredTra) <- c( "predictedTra", "observedTra" )
   cat('SAVING (',sprintf("%s", s_PredAndObsCsvOutputFileTra), ')\n', sep="", collapse="")
   write.csv(df_PredTra,        s_PredAndObsCsvOutputFileTra, ,row.names=FALSE)
   cat('SAVING (',sprintf("%s", s_PredAndObsCsvOutputFileTra), '): DONE\n', sep="", collapse="")
   
   mseTra  = mse_error (predTra, observedTra)
   rmseTra = rmse_error(predTra, observedTra)
   maeTra  = mae_error (predTra, observedTra)
   corrTra = cor       (predTra, observedTra[vl1_ObjectsInTheTra, ]);
   cat('rmseTra = ', sprintf("%f", rmseTra), '\n', sep="", collapse="")
   cat('maeTra  = ', sprintf("%f", maeTra ), '\n', sep="", collapse="")
   cat('R_Tra   = ', sprintf("%f", corrTra), '\n', sep="", collapse="")

##############################
#SAVE ERRORS TO FILE
##############################
  fsTra = file(s_ErrorsOutputFileTra)
    s = 'mseTra,rmseTra,maeTra,corrTra'
    s = paste(s,'\n',sprintf("%f", mseTra ), s_Comma, sprintf("%f", rmseTra ), s_Comma, sprintf("%f", mseTra ), s_Comma, sprintf("%f",  corrTra), sep = "", collapse="");
    writeLines(s, con=fsTra, sep='\n')
  close(fsTra)
}
#
#cat('s_ComputePredictionsOnTheTestingSet = ', sprintf("|%s|", s_ComputePredictionsOnTheTestingSet), '\n', sep="", collapse="")
# (strcmp) in R returns a boolean, as oppposed to C, where it returns an integer.
if(strcmp(s_ComputePredictionsOnTheTestingSet,"yes")==TRUE || identical(s_ComputePredictionsOnTheTestingSet,NULL) ){

##############################
#PREDICTION ON TESTING SET
##############################
  cat('\nPREDICTION on the testing set : ', sprintf("%s", s_ComputePredictionsOnTheTestingSet), '\n', sep="", collapse="")
  startTime <- Sys.time()
    predTst <- predict(frbsObject, tstNoTarget)
  endTime   <- Sys.time()
  computingTime = as.numeric(difftime(endTime, startTime, units="secs"))
  df_Pred = cbind(predTst,observedTst)
  colnames(df_Pred) <- c( "predictedTst", "observedTst" )
  
  cat('SAVING (',sprintf("%s", s_PredAndObsCsvOutputFileTst), ')\n', sep="", collapse="")
  write.csv(df_Pred          , s_PredAndObsCsvOutputFileTst, ,row.names=FALSE)
  cat('SAVING (',sprintf("%s", s_PredAndObsCsvOutputFileTst), '): DONE\n', sep="", collapse="")
  cat('PREDICTIONS (tst): DONE (', sprintf("%.3f", computingTime ), ') secs\n', sep="", collapse="")
  lengthOfPredTst     = nrow(predTst)
  if(length(predTst ) != lengthOfObservedTst ){
    s = cat(sprintf("%d", lengthOfPredTst), " != ",  sprintf("%d", lengthOfObservedTst ), sep="", collapse="")
    cat('ERROR): length(predTst) != length(observedTst)  (', s, ')\n', sep="", collapse="")
  }
  mseTst  = mse_error (predTst, observedTst)
  rmseTst = rmse_error(predTst, observedTst)
  maeTst  = mae_error (predTst, observedTst)
  corrTst = cor       (predTst, observedTst);
  cat('rmseTst = ', sprintf("%f", rmseTst ), '\n', sep="", collapse="")
  cat('maeTst  = ', sprintf("%f", maeTst  ), '\n', sep="", collapse="")
  cat('R_Tst   = ', sprintf("%f", corrTst ), '\n', sep="", collapse="")

##############################
#SAVE ERRORS TO FILE
##############################
  fsTst = file(s_ErrorsOutputFileTst)
    s = 'mseTst,rmseTst,maeTst,corrTst'
    s = paste(s,'\n',sprintf("%f", mseTst ), s_Comma, sprintf("%f", rmseTst ), s_Comma, sprintf("%f", mseTst ), s_Comma, sprintf("%f",  corrTst), sep = "", collapse="");
    writeLines(s, con=fsTst, sep='\n')
  close(fsTst)
  
}else{
  cat('PREDICTION on the tst set: NOT DONE\n', sep="", collapse="")
}

cat('\nDONE\n--------\n', sep="", collapse="")
sink()
cat('\nDONE\n--------\n', sep="", collapse="")
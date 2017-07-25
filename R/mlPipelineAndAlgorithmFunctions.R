#' Get the one hot encoding data
#'
#' @param inputForMLPipeline takes dataframe
#' @return MLOutputNN return dataframe by performing numerical encoding and one hot encoding on input data

mlPipeline <- function(inputForMLPipeline){
  oneHotEncoadingColumns = c("Size","Category..L6.","Collection","Fab.Content","Fabrictype","Neck","Yarn","Sleeve","Gender","Tops.Bottom","BRAND")
  reqColumns = colnames(inputForMLPipeline)[!colnames(inputForMLPipeline) %in% c("Sku","Year","Date")]
  #Iterate over the columns and apply onehotencoding and removed columns having levels of factor == 1
  UniqueLevelColumns =c()
  for(colname in reqColumns){
    if(nlevels(as.factor(inputForMLPipeline[[colname]]))==1){
      UniqueLevelColumns=append(UniqueLevelColumns,colname)
    }
  }
  oneHotEncoadingColumns = setdiff(oneHotEncoadingColumns,UniqueLevelColumns)
  ohc_data = dummy.data.frame(inputForMLPipeline[oneHotEncoadingColumns],sep="")
  inputForMLPipeline=cbind(inputForMLPipeline,ohc_data)
  drops=union(oneHotEncoadingColumns,UniqueLevelColumns)
  inputForMLPipeline = inputForMLPipeline <- inputForMLPipeline[ , !(names(inputForMLPipeline) %in% drops)]
  print(dim(inputForMLPipeline))
  names(inputForMLPipeline)=make.names(names(inputForMLPipeline), unique=TRUE)
  return(inputForMLPipeline)
}

#' Normalize the one hot encoding data to generate input for NN
#'
#' @param MlOutputData is a dataframe
#' @return the dataframe after did the normalisation
#' @export

NormalizeData<-function(MlOutputData){
  charactercolumns = colnames(InputForNormalisation)[!sapply(InputForNormalisation, class) %in% c('numeric','integer')]
  for(column in charactercolumns){
    MlOutputData[[column]] = as.numeric(as.character(MlOutputData[[column]]))
  }
  mins=as.numeric(apply(MlOutputData,2,min))
  print(length(mins))
  maxs=as.numeric(apply(MlOutputData,2,max))
  print(length(maxs))
  MlOutputData=as.data.frame(scale(MlOutputData,center = mins, scale = maxs - mins))
  return(MlOutputData)
}

#' Seperate the traing and test(prediction) data from raw input data
#'
#' @param Normalizedata accepts dataframe
#' @param Selected_Data return the dataframe for the given time period range
#' @export

getTrainingTestDataFromRawInput <- function(Normalizedata,Starting_Date,Ending_Date){
  Selected_Data<- subset(Normalizedata,as.Date(Normalizedata$Date) >= as.Date(Starting_Date) & as.Date(Normalizedata$Date) <= as.Date(Ending_Date))
}

#' Used to get the columns which need to be predict during algorithm runs
#'
#' @param data is a dataframe
#' @param columns unnecessary columns vector
#' @return  ReqColumns a vector having columns which need to be predicted
#' @export

complimentary_colum_selector<-function(data,columns){
  AllColumns<-colnames(data)
  ReqColumns=AllColumns[!AllColumns %in% columns]
  return(ReqColumns)
}

#' NN algorithm function
#'
#' @param Training_set is a data frame to train the network
#' @param Prediction_set is also a data frame to test the NN
#' @param hidden_layers is a vector to specify the number of nurons in the hidden layers
#' @param step_max maximum steps for training the neural network
#' @param pipeline_output is used to denormalize the data
#' @return ref a dataframe with actual and predicted values for the sales
#' @export

nnAlgorithm<-function(Training_set,Prediction_set,hidden_layers,step_max,pipeline_output){
  set.seed(10)
  train_data_col<-colnames(Training_set)
  formul <- as.formula(paste("Sales_Qty ~", paste(train_data_col[!train_data_col %in% c("Sku","Year","Date","Sales_Qty")], collapse = " + ")))
  print("Calling the neuralnet function ..,")
  nn <- neuralnet(formul,data=Training_set,hidden=hidden_layers,stepmax = step_max,linear.output=T)
  print("ran neuralnet function..,")
  #Choose the columns need to be predict
  reference_col_test<-c("Sales_Qty","Sku","Year","Date")
  prediction_columns=complimentary_colum_selector(Prediction_set,reference_col_test)
  #calculate the values for sales
  pr.nn <- compute(nn,Prediction_set[prediction_columns])
  #Denormalize the redicted values
  predicted<<- pr.nn$net.result*(max(pipeline_output$Sales_Qty)-min(pipeline_output$Sales_Qty))+min(pipeline_output$Sales_Qty)
  actualsales<<- Prediction_set$Sales_Qty*(max(pipeline_output$Sales_Qty)-min(pipeline_output$Sales_Qty))+min(pipeline_output$Sales_Qty)
  periods<<-Prediction_set$Period*(max(pipeline_output$Period)-min(pipeline_output$Period))+min(pipeline_output$Period)
  soh<<-Prediction_set$Soh_Qty*(max(pipeline_output$Soh_Qty)-min(pipeline_output$Soh_Qty))+min(pipeline_output$Soh_Qty)
  print("Done with denormalization of values")
  #ERROR calculation
  error=mse(predicted,actualsales)
  ref<<-cbind(Prediction_set$Sku,as.character(Prediction_set$Year),as.character(Prediction_set$Date),periods,soh,predicted,actualsales)
  colnames(ref)<<-c("Sku","Year","Date","Period","SOH","Predicted_sales","Actual_sales")
  print("done with creating the final result of NN algorithm")
  return(ref)
}

#' RF algorithm function
#'
#' @param Training_set data frame to train the network
#' @param Prediction_set also a dataframe to test the model
#' @param Ntree,Mtry are numerical values, to fit the RF model
#' @return ref resultant dataframe of RF algorithm
#' @export

rfAlgorithm = function(Training_set,Prediction_set,Ntree,Mtry){
  #Data Shuffling
  Training_set<- Training_set[sample(nrow(Training_set)),]
  Prediction_set <- Prediction_set[sample(nrow(Prediction_set)),]
  print("Shuffled the traing and prediction data ..,")
  #Training  Algorithm
  training_set_columns <- colnames(Training_set)
  formula<- as.formula(paste("Sales_Qty ~", paste(training_set_columns[!training_set_columns %in% c("Sales_Qty","Sku","Year","Date")], collapse = " + ")))
  print("Calling RF model")
  fitting <- randomForest(formula, data = Training_set,ntree=Ntree,mtry=Mtry)
  #returns columns other than specified in arguments
  columns<-c("Sales_Qty","Sku","Year","Date")
  prediction_columns=complimentary_colum_selector(Prediction_set,columns)
  predicted= predict(fitting,Prediction_set[prediction_columns])
  ref=cbind(Prediction_set["Sku"],Prediction_set["Year"],Prediction_set["Date"],Prediction_set["Period"],Prediction_set["Soh_Qty"],predicted,Prediction_set["Sales_Qty"])
  Prediction_set_error<-mse(predicted,Prediction_set$Sales_Qty)
  return(ref)
}

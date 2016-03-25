run_analysis<-function(){
    ##Read allthe text filesinto memory for tidying
  ##read them seperately
  
  xtrain<-read.table("X_train.txt")
  ytrain<-read.table("y_train.txt")
  subtrain<-read.table("subject_train.txt")
  
  
  xtest<-read.table("X_test.txt")
  ytest<-read.table("y_test.txt")
  subtest<-read.table("subject_test.txt")
  
  activity<-read.table("activity_labels.txt",stringsAsFactors = FALSE)
  feat<-read.table("features.txt")
  
  ##give all the variables names to be able to subset the part you need  
  
  names(xtrain)<-feat[,2]
  names(xtest)<-feat[,2]
  
  names(subtrain)<-"volunteers"
  names(subtest)<-"volunteers"
  
  ##select only the variables with mean and standard deviation 
  mean_std1<-grep("[m][e][a][n]|[s][t][d]", names(xtrain))
  xtrain<-xtrain[,mean_std1]
  
  mean_std2<-grep("[m][e][a][n]|[s][t][d]", names(xtest))
  xtest<-xtest[,mean_std2]
  
  ##rename y_train values to the appropriate activity
  
  for(i in 1:nrow(ytrain)){
    if(ytrain[i,]==1){ytrain[i,]=activity[1,2]}
    else if(ytrain[i,]==2){ytrain[i,]=activity[2,2]}
    else if(ytrain[i,]==3){ytrain[i,]=activity[3,2]}
    else if(ytrain[i,]==4){ytrain[i,]=activity[4,2]}
    else if(ytrain[i,]==5){ytrain[i,]=activity[5,2]}
    else if(ytrain[i,]==6){ytrain[i,]=activity[6,2]}
    
  }##end of y_train for
  names(ytrain)<-"activity"
  ##rename y_test values to the appropraite activity
  
  for(i in 1:nrow(ytest)){
    if(ytest[i,]==1){ytest[i,]=activity[1,2]}
    else if(ytest[i,]==2){ytest[i,]=activity[2,2]}
    else if(ytest[i,]==3){ytest[i,]=activity[3,2]}
    else if(ytest[i,]==4){ytest[i,]=activity[4,2]}
    else if(ytest[i,]==5){ytest[i,]=activity[5,2]}
    else if(ytest[i,]==6){ytest[i,]=activity[6,2]}
    
  }##end of y_test for
  names(ytest)<-"activity"
  ##Bind the training and test set variables together 
  ##to get the complete table required
  
  final_train<-cbind(subtrain,ytrain,xtrain)
  final_test<-cbind(subtest,ytest,xtest)
  
  tidydata<-rbind(final_train,final_test)
  tidydata<-tidydata[order(tidydata[,1]),]
  return(tidydata)
  }##end function
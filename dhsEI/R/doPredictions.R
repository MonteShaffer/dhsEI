#' Write my dataframe format to required output formate
#'
#' @param dframe my data frame, see data("stage1") for sample
#' @param dpath path to write to file
#' @param sname stem of file name, not including "CSV"
#' @param trainme if true, we truncate to only the 100 training elements
#'
#' @return string, name of filename stored
writePredictionToCSV = function(dframe,dpath="",sname="myName",trainme=T)
{
    
    
    data("train1"); # has 100 unique values

if(dpath==""){ dpath = paste(getwd(),"",sep="/"); }  # has trailing slash
filename = paste(dpath,sname,".csv",sep="");   
    # write back to required format
    cat("Id,Probability", file=filename,append=F,fill=T,sep="");
   

    ilen = dim(dframe)[1];
    for(i in 1:ilen)
        {
        ro = dframe[i,];
        # is id in train1
        if(trainme == T && !is.element(ro$id,train1))
            {
            next;
            }
        myStem = paste(ro$id,"_","Zone",sep="");
        for(j in 1:17)
            {
            # 00360f79fd6e02781457eda48f85da90_Zone1,0
            myStr = paste(myStem,j,",",sep="");
                myC = paste("Prob","zone",j,sep="_");
            myStr = paste(myStr,ro[[myC]],sep="");
            
            cat(myStr, file=filename,append=T,fill=T,sep="");
            }
        }
    filename;
}


#' Prediction based on some fixed probability
#'
#' @param prob fixed probability [0-1]
#'
#' @return nothing, stores file in dhsEI/dhsEI/example/trials/submit1
#' @export
#'
#' @examples 
#' predictUniformProbability(0.5);
#' predictUniformProbability(0.15);
#' predictUniformProbability(0.05);
#' predictUniformProbability(0.10);
predictUniformProbability = function(prob=0.5)
{
    
data("stage1");
data("train1");
    
    mdata = stage1;
    mdata[,2:18] = prob;
    
    # truncate to train1 length (train1 and stage1 are independent)
    mdata[1:100,1] = train1;
    mdata = mdata[1:100,];
    
     
   # setwd("P:/_.github._/dhsEI/dhsEI/example/trials/submit1");
   # setwd("/data/R_data/_DHS_");
    
    
    writePredictionToCSV(mdata,"",paste("uniform_probability_",prob,sep=""));
}

#' Prediction based on mean of training set
#'
#' @return nothing, stores file in dhsEI/dhsEI/example/trials/submit1
#' @export
#'
#' @examples 
#' predictMeanProbability();
predictMeanProbability = function()
{
    
data("stage1");
data("train1");
    mdata = stage1;
    means = as.numeric( plyr::colwise(mean)(mdata[,2:18]) );
    for(i in 2:18)
        {
        mdata[,i] = means[i-1];
        }
    
    # truncate to train1 length (train1 and stage1 are independent)
        mdata[1:100,1] = train1;
    mdata = mdata[1:100,];
    
    
    # setwd("P:/_.github._/dhsEI/dhsEI/example/trials/submit1");
    # setwd("/data/R_data/_DHS_");
    
    writePredictionToCSV(mdata,"","mean_probability");
}




#' Prediction based on mean of training set (with zone-9 offset)
#'
#' @param prob probability for zone-9
#'
#' @return nothing, stores file in dhsEI/dhsEI/example/trials/submit1
#' @export
#'
#' @examples 
#' predictMeanProbabilityPsych9(0.038);
#' predictMeanProbabilityPsych9(0.05);
predictMeanProbabilityPsych9 = function(prob=0.038)
{
    
    data("stage1");
    data("train1");
    mdata = stage1;
    means = as.numeric( plyr::colwise(mean)(mdata[,2:18]) );
    for(i in 2:18)
    {
        mdata[,i] = means[i-1];
    }
    
    # truncate to train1 length (train1 and stage1 are independent)
    mdata[1:100,1] = train1;
    mdata = mdata[1:100,];
    
    mdata[,10] = prob;
    
    
    # setwd("P:/_.github._/dhsEI/dhsEI/example/trials/submit1");
    # setwd("/data/R_data/_DHS_");
    
    writePredictionToCSV(mdata,"",paste("mean_probability_9",prob,sep=""));
    
    
    
    
}









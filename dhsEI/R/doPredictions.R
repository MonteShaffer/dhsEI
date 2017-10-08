# doPredictions

#' Write my dataframe format to required output formate
#'
#' @param dframe my data frame, see data("stage1") for sample
#' @param dpath path to write to file
#' @param sname stem of file name, not including "CSV"
#'
#' @return nothing, file is stored

writePredictionToCSV = function(dframe,dpath="",sname="myName")
{

if(dpath==""){ dpath = paste(getwd(),"",sep="/"); }  # has trailing slash
filename = paste(dpath,sname,".csv",sep="");   
    # write back to required format
    cat("Id,Probability", file=filename,append=F,fill=T,sep="");
   

    ilen = dim(dframe)[1];
    for(i in 1:ilen)
        {
        ro = dframe[i,];
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
    
    mdata = stage1;
    mdata[,2:18] = prob;
     
    setwd("P:/_.github._/dhsEI/dhsEI/example/trials/submit1");
    
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
    mdata = stage1;
    means = as.numeric( plyr::colwise(mean)(mdata[,2:18]) );
    for(i in 2:18)
        {
        mdata[,i] = means[i-1];
    }
    
    setwd("P:/_.github._/dhsEI/dhsEI/example/trials/submit1")
    
    writePredictionToCSV(mdata,"","mean_probability");
}




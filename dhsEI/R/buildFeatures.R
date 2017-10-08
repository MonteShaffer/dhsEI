
#' Build Animations
#'
#' @param rtype file type "aps" or "a3daps" (low / hi resolution)
#'
#' @return nothing
#' @export
#'
#' @examples
#' buildAnimations("aps");
#' buildAnimations("a3daps");
buildAnimations = function(rtype="aps")
{
    
    dFolder = "/data/R_data/_DHS_/profiles";
    folders = list.dirs(path=dFolder, recursive=F, full.names=F);
    flen = length(folders);
    for(i in 1:flen)
    {
        f = folders[i];
        status = ("###      CCCC  of  TTTT                  ###");
        status = gsub("CCCC",i,status);
        status = gsub("TTTT",flen,status);
        print("#####################################");
        print(f);
        print(status);
        flush.console();
        
        print("#####################################");
        
        buildAnimation(f,rtype); 
    }
    
}

# find.package("dhsEI");

# 00360f79fd6e02781457eda48f85da90
# 0043db5e8c819bffc15261b1f1ac5e42
# rkey="00360f79fd6e02781457eda48f85da90"; rtype="aps";

#' Build and Cache Animation
#'
#' @param rkey unique scan (md5)
#' @param rtype file type "aps" or "a3daps" (low / hi resolution)
#'
#' @return nothing
#' @export
#'
#' @examples 
#' buildAnimation("00360f79fd6e02781457eda48f85da90","aps");
#' buildAnimation("00360f79fd6e02781457eda48f85da90","a3daps");
#' 
#' buildAnimation("0043db5e8c819bffc15261b1f1ac5e42","aps");
#' buildAnimation("0043db5e8c819bffc15261b1f1ac5e42","a3daps");
buildAnimation = function(rkey,rtype="aps")
{
    
    dFolder = paste("/data/R_data/_DHS_/profiles",rkey,"",sep="/");
    rFolder = paste(dFolder,"animation",sep="");
    rFolder = paste(rFolder,rtype,"",sep="/");
        if(!dir.exists(rFolder)) { dir.create(rFolder,recursive=T); }
    
    rHTML = paste(rFolder,rkey,".html",sep="");
    
    cwd = getwd(); # current working directory
    
    
    hinfo = dinfo = NULL;
    
    if(!file.exists(rHTML))
        {
            
        if(rtype=="aps")
            {
            load(paste(dFolder,"raw.Rda",sep=""));
                dinfo = data.aps;
            load(paste(dFolder,"header.Rda",sep=""));
                hinfo = header.aps;
            } else if(rtype=="a3daps")
                {
                load(paste(dFolder,"rawA3D.Rda",sep=""));
                    dinfo = data.a3daps;
                load(paste(dFolder,"headerA3D.Rda",sep=""));
                    hinfo = header.a3daps;
                }
        
        
        dims = dim(dinfo);
            nx = dims[1];
            ny = dims[2];
            nr = dims[3]; # number of rotations
        
        setwd(rFolder);
        
        animation::saveHTML(
                for(i in 1:nr)
                {
                    image(dinfo[,,i])
                },
                
                img.name = paste( rkey ,"_",sep=""), 
                htmlfile = paste( rkey ,".html",sep=""),
                #navigator = T,
                
                ani.width=nx,
                ani.height=ny,
                #title = paste("Zones: ",paste(s_$zones, sep=" - ", collapse="")),
                #verbose = T,
                autobrowse = F #,
                #description = myD
            );
        
        
        # return working directory
        setwd(cwd);
        } else { print("cached"); }
    
      
    
}





# data("stage1");





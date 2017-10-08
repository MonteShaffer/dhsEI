
#' Reads header of `aps` file [also a3daps]
#'
#' @param fname string, filename
#'
#' @return list of details
#' @export
#'
read_header <- function(fname){
    
    h = list()
    fid = file(fname, "rb")
    
    h$filename <- readChar(fid,20,useBytes = T)
    h$parent_filename <- readChar(fid,20,useBytes = T)
    h$comments1 <- readChar(fid,80,useBytes = T)
    h$comments2 <- readChar(fid,80,useBytes = T)
    h$energy_type <- readBin(fid,integer(),1,size=2)
    h$config_type <- readBin(fid,integer(),1,size=2)
    h$file_type <- readBin(fid,integer(),1,size=2)
    h$trans_type <- readBin(fid,integer(),1,size=2)
    h$scan_type <- readBin(fid,integer(),1,size=2)
    h$data_type <- readBin(fid,integer(),1,size=2)
    h$date_modified <- readChar(fid,16,useBytes = T)
    h$frequency <- readBin(fid,double(),1,size=4)
    h$mat_velocity <- readBin(fid,double(),1,size=4)
    h$num_pts <- readBin(fid,integer(),1,size=4)
    h$num_polarization_channels <- readBin(fid,integer(),1,size=2)
    h$spare00 <- readBin(fid,integer(),1,size=2)
    h$adc_min_voltage <- readBin(fid,double(),1,size=4)
    h$adc_max_voltage <- readBin(fid,double(),1,size=4)
    h$band_width <- readBin(fid,double(),1,size=4)
    h$spare01 <- readBin(fid,integer(),5,size=2)
    h$polarization_type <- readBin(fid,integer(),4,size=2)
    h$record_header_size <- readBin(fid,integer(),1,size=2)
    h$word_type <- readBin(fid,integer(),1,size=2)
    h$word_precision <- readBin(fid,integer(),1,size=2)
    h$min_data_value <- readBin(fid,double(),1,size=4)
    h$max_data_value <- readBin(fid,double(),1,size=4)
    h$avg_data_value <- readBin(fid,double(),1,size=4)
    h$data_scale_factor <- readBin(fid,double(),1,size=4)
    h$data_units <- readBin(fid,integer(),1,size=2)
    h$surf_removal <- readBin(fid,integer(),1,size=2)
    h$edge_weighting <- readBin(fid,integer(),1,size=2)
    h$x_units <- readBin(fid,integer(),1,size=2)
    h$y_units <- readBin(fid,integer(),1,size=2)
    h$z_units <- readBin(fid,integer(),1,size=2)
    h$t_units <- readBin(fid,integer(),1,size=2)
    h$spare02 <- readBin(fid,integer(),1,size=2)
    h$x_return_speed <- readBin(fid,double(),1,size=4)
    h$y_return_speed <- readBin(fid,double(),1,size=4)
    h$z_return_speed <- readBin(fid,double(),1,size=4)
    h$scan_orientation <- readBin(fid,integer(),1,size=2)
    h$scan_direction <- readBin(fid,integer(),1,size=2)
    h$data_storage_order <- readBin(fid,integer(),1,size=2)
    h$scanner_type <- readBin(fid,integer(),1,size=2)
    h$x_inc <- readBin(fid,double(),1,size=4)
    h$y_inc <- readBin(fid,double(),1,size=4)
    h$z_inc <- readBin(fid,double(),1,size=4)
    h$t_inc <- readBin(fid,double(),1,size=4)
    h$num_x_pts <- readBin(fid,integer(),1,size=4)
    h$num_y_pts <- readBin(fid,integer(),1,size=4)
    h$num_z_pts <- readBin(fid,integer(),1,size=4)
    h$num_t_pts <- readBin(fid,integer(),1,size=4)
    h$x_speed <- readBin(fid,double(),1,size=4)
    h$y_speed <- readBin(fid,double(),1,size=4)
    h$z_speed <- readBin(fid,double(),1,size=4)
    h$x_acc <- readBin(fid,double(),1,size=4)
    h$y_acc <- readBin(fid,double(),1,size=4)
    h$z_acc <- readBin(fid,double(),1,size=4)
    h$x_motor_res <- readBin(fid,double(),1,size=4)
    h$y_motor_res <- readBin(fid,double(),1,size=4)
    h$z_motor_res <- readBin(fid,double(),1,size=4)
    h$x_encoder_res <- readBin(fid,double(),1,size=4)
    h$y_encoder_res <- readBin(fid,double(),1,size=4)
    h$z_encoder_res <- readBin(fid,double(),1,size=4)
    h$date_processed <- readChar(fid,8,useBytes = T)
    h$time_processed <- readChar(fid,8,useBytes = T)
    h$depth_recon <- readBin(fid,double(),1,size=4)
    h$x_max_travel <- readBin(fid,double(),1,size=4)
    h$y_max_travel <- readBin(fid,double(),1,size=4)
    h$elevation_offset_angle <- readBin(fid,double(),1,size=4)
    h$roll_offset_angle <- readBin(fid,double(),1,size=4)
    h$z_max_travel <- readBin(fid,double(),1,size=4)
    h$azimuth_offset_angle <- readBin(fid,double(),1,size=4)
    h$adc_type <- readBin(fid,integer(),1,size=2)
    h$spare06 <- readBin(fid,integer(),1,size=2)
    h$scanner_radius <- readBin(fid,double(),1,size=4)
    h$x_offset <- readBin(fid,double(),1,size=4)
    h$y_offset <- readBin(fid,double(),1,size=4)
    h$z_offset <- readBin(fid,double(),1,size=4)
    h$t_delay <- readBin(fid,double(),1,size=4)
    h$range_gate_start <- readBin(fid,double(),1,size=4)
    h$range_gate_end <- readBin(fid,double(),1,size=4)
    h$ahis_software_version <- readBin(fid,double(),1,size=4)
    h$spare_end <- readBin(fid,double(),10,size=4)
    close(fid)
    return(h)
    
}



#' Reads header of `aps` file [also a3daps]
#'
#' @param fname string, filename
#'
#' @return array data [unless `ahi` then real/imag]
#' @export
#'

read_data <- function(fname) {
    
    extension <- file_ext(fname)
    h = read_header(fname)
    nx = h$num_x_pts
    ny = h$num_y_pts
    nt = h$num_t_pts
    fid <- file(fname,"rb")
    dummy <- readBin(fid,"raw",512)
    if (extension %in% c('aps', 'a3daps')){
        if (h$word_type==7) {
            data <- readBin(fid,double(),nx*ny*nt,size=4)
        } else if (h$word_type==4){
            data <- readBin(fid,integer(),nx*ny*nt,size=2)
        }
        close(fid)
        data <- data * h$data_scale_factor
        data<-array(data,dim = c(nx,ny,nt))
    } else if (extension == 'a3d') {
        if (h$word_type==7) {
            data <- readBin(fid,double(),nx*ny*nt,size=4)
        } else if (h$word_type==4){
            data <- readBin(fid,integer(),nx*ny*nt,size=2)
        }
        close(fid)
        data <- data * h$data_scale_factor
        data<-array(data,dim = c(nx,nt,ny))  
    } else if (extension == 'ahi'){
        data <- readBin(fid,double(),2*nx*ny*nt,size=4)
        close(fid)
        data<-array(data,dim = c(2,ny,nx,nt))  
        real = data[1,,,]
        imag = data[2,,,]
    }
    if (extension != '.ahi'){
        return(data)
    } else {
        data <- list()
        data$real <- real
        data$imag <- imag
        return(data)
    }  
}



#' Copy `aps` to profiles directory, separate 'data' from 'header' as separate objects
#'
#' data.aps as raw.Rda
#' header.aps as header.Rda
#'
#' @param aps_dir 
#'
#' @return nothing
#' @export
#'
#' @examples buildRawProfileAPS("/data/R_data/_DHS_/aps/sample/");
#' 
#' buildRawProfileAPS("/data/R_data/_DHS_/aps/stage1/");
#' 
buildRawProfileAPS = function(aps_dir)
{
    
    # aps_dir = "/data/R_data/_DHS_/aps/sample/";
    files = list.files(path=aps_dir,pattern="\\.aps$");
    flen = length(files);
    for(i in 1:flen)
    {
        f = files[i];
            status = ("###      CCCC  of  TTTT                  ###");
            status = gsub("CCCC",i,status);
            status = gsub("TTTT",flen,status);
            print("#####################################");
            print(f);
            print(status);
            flush.console();
            
            print("#####################################");
        myFile = paste(aps_dir,f,sep="");
        myExt = tools::file_ext(myFile);
        myStem = gsub(paste(".",myExt,sep=''),"",f);
        
        dFolder = paste("/data/R_data/_DHS_/profiles",myStem,"",sep="/");
        if(!dir.exists(dFolder)) { dir.create(dFolder,recursive=T); }
        
        dFile = paste(dFolder,f,sep='');
        if(!file.exists(dFile))
        {
            file.copy(myFile,dFile);
        }
        
        
        dData = paste(dFolder,"raw.Rda",sep='');
        if(!file.exists(dData))
            {
            data.aps = read_data(myFile);
            save(data.aps,file=dData);
            }
        
        hData = paste(dFolder,"header.Rda",sep='');
        if(!file.exists(hData))
            {
            header.aps = read_header(myFile);
            save(header.aps,file=hData);
            }
        
    }
}





#' Copy `a3daps` to profiles directory, separate 'data' from 'header' as separate objects
#'
#' data.a3daps as raw.Rda
#' header.a3daps as header.Rda
#'
#' @param a3daps_dir 
#'
#' @return nothing
#' @export
#'
#' @examples buildRawProfileA3DAPS("/data/R_data/_DHS_/a3daps/sample/");
#' 
#' buildRawProfileA3DAPS("/data/R_data/_DHS_/a3daps/stage1/");
#' 
buildRawProfileA3DAPS = function(a3daps_dir)
{
    
    # a3daps_dir = "/data/R_data/_DHS_/a3daps/sample/";
    files = list.files(path=a3daps_dir,pattern="\\.a3daps$");
    flen = length(files);
    for(i in 1:flen)
    {
        f = files[i];
        status = ("###      CCCC  of  TTTT                  ###");
        status = gsub("CCCC",i,status);
        status = gsub("TTTT",flen,status);
        print("#####################################");
        print(f);
        print(status);
        flush.console();
        
        print("#####################################");
        myFile = paste(a3daps_dir,f,sep="");
        myExt = file_ext(myFile);
        myStem = gsub(paste(".",myExt,sep=''),"",f);
        
        dFolder = paste("/data/R_data/_DHS_/profiles",myStem,"",sep="/");
        if(!dir.exists(dFolder)) { dir.create(dFolder,recursive=T); }
        
        dFile = paste(dFolder,f,sep='');
        if(!file.exists(dFile))
        {
            file.copy(myFile,dFile);
        }
        
        
        dData = paste(dFolder,"rawA3D.Rda",sep='');
        if(!file.exists(dData))
        {
            data.a3daps = read_data(myFile);
            save(data.a3daps,file=dData);
        }
        
        hData = paste(dFolder,"headerA3D.Rda",sep='');
        if(!file.exists(hData))
        {
            header.a3daps = read_header(myFile);
            save(header.a3daps,file=hData);
        }
        
    }
}




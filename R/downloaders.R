#' @title Download HAND, Slope, and Cathmask rasters by HUC6
#' @description This function downloads staged HAND data to a local directory.
#' @param HUC6 The HUC6 unit(s) to download
#' @param outdir The folder path where data should be downloaded and extracted
#' @param type The type of data to download. Options include 'catchmask'. 'hand', or 'slp'. 
#' If 'all' is used (DEFAULT) all three type are downloaded.
#' @export
#' @examples
#' \dontrun{
#'   download_hand("010100", outdir, "all")
#' }

download_hand <- function(HUC6, outdir, type = 'all'){
  
  files = c('catchmask', 'slp', 'hand')
  
  if(!type %in% c(files, 'all')){
    stop(paste("Type must be one or more of: ", paste(files, collapse = ", "), "or all"))
  }
  
  if(type == 'all'){ type = files}
  
  base.url = 'https://web.corral.tacc.utexas.edu/nfiedata/HAND/'
  
  g = expand.grid(HUC6, type)
  
  urls = paste0(base.url, g$Var1, "/", g$Var1, g$Var2, '.tif')
  
  message("Beginning downlaod of ", length(urls), " files...")
  for(i in 1:length(urls)){
    downloader(outdir, urls[i])
  }
}

#' @title Function to download data from URL to out directory using httr.
#' @description General downloader
#' @param dir path to output directory
#' @param url the location of the online resource
#' @importFrom httr GET write_disk progress

downloader <- function(dir, url){
  
  if (!dir.exists(dir)) { dir.create(dir, recursive = T) }
  
  file <-  file.path(dir, basename(url))
  
  if (!file.exists(file)) {
    
    message("\tDownloading ", basename(url))
    
    resp <-  httr::GET(url,
                       httr::write_disk(file, overwrite = TRUE),
                       httr::progress())
    
    if (resp$status_code != 200) {
      warning(basename(url), " Download unsuccessfull :(")
    }
    
  } else {
    message("\t", basename(url), " already exists ...")
  }
}

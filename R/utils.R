lookupN = data.frame(order = c(1:10),
  inN = c(.083, .076, .067, .054, .046, .039, .038, .030, .023, .017),
  outN = c(.181, .168, .150, .127, .108, .089, .087, .066, .053, .0383),
  stringsAsFactors = FALSE
)

#' @title Global Variables
#' @importFrom utils globalVariables 
#' @keywords internal
#' @export

utils::globalVariables(
  c('catchID', 'cellarea', 'hand_raster', 'in_bank', 'slope_raster')
)

#' @title Build a Top Width Raster
#' @description From a defined top width and NHD segement, create a raster defining in and out of bank regions. 
#' In bank = 1, out of bank = 0.
#' @param r Input raster with grid to output to
#' @param nhd A nhd sf polyline defining a river segment
#' @param TW a top width to buffer the nhd segment to
#' @return a binary raster of in and out of bank areas
#' @export
#' @importFrom fasterize fasterize
#' @importFrom sf st_transform st_buffer st_union st_as_sf

buildTW_raster = function(r, nhd, TW){
  
  line = nhd %>% st_transform(5070)
  
  st_buffer(line, TW) %>%
    st_transform(r@crs) %>% 
    st_union() %>%
    st_as_sf() %>%
    fasterize::fasterize(r, background = 0)
}

#' @title Fill Data.frame
#' @param d 
#' @param cm a catchmask raster
#' @param pos in bank (1) or out of bank (0)
#' @return data.frame
#' @keywords internal
#' @export
#' @importFrom dplyr arrange mutate

fill.df = function(d, cm, pos = 1){
  tmp = d[d$in_bank == pos,]
  not.in = cm[!(cm %in% tmp$catchID)]
  t = cbind(not.in, matrix(as.integer(0), ncol = NCOL(tmp)- 1, nrow = length(not.in)))
  fin = rbind(as.matrix(tmp), as.matrix(t)) %>% data.frame() %>% arrange(catchID) %>% mutate(in_bank = pos)
  fin
}

#' @title Determine flooded cells
#' @param x 
#' @param sub 
#' @keywords internal
#' @return vector
#' @export

flood = function(x, sub) {
  tmp.row = x - sub$hand_raster
  tmp.row[tmp.row < 0] = 0
  tmp.row * sub$cellarea
}

#' @title Pipe Re-export
#' @description re-export magrittr pipe operator
#' @importFrom magrittr %>%
#' @name %>%
#' @keywords internal
#' @export

NULL

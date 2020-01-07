#' @title Build Synthetic Rating Curve
#' @param stages a vector of stages over which to predict a flow value
#' @param catchmask a catchment mask raster
#' @param hand  a HAND raster
#' @param slope a slope raster
#' @param inbank an in-bank / out-bank raster
#' @param table a river-reach file
#' @param inN an in bank roughness value
#' @param outN an out of bank roughness value
#' @param control if TRUE in-bank N must be less then out of bank N
#' @return a SRC data.frame
#' @importFrom raster getValues
#' @importFrom dplyr bind_cols filter mutate group_by summarise_all select
#' @importFrom stats setNames
#' @export

src_generate = function(stages = NULL,
                    catchmask, hand, slope, inbank, 
                    table, inN = .05, outN = .05, control = T){
  
  stage = if(is.null(stages)){ seq(0,17.9832, 0.3048)} else {stages}
  
  sub = data.frame(
    catchmask    = getValues(catchmask),
    hand_raster  = getValues(hand),
    slope_raster = getValues(slope),
    in_bank      = getValues(inbank),
    cellarea     = (31.91881 ^ 2) / 10.76391  # square meters to square feet!!
  ) %>% 
    filter(!is.na(hand_raster),
           !is.na(catchmask),
           catchmask %in% table$CatchID) %>%
    mutate(slope_add = cellarea * sqrt(1 + slope_raster ^ 2))
  
  if(nrow(sub) != 0){
    
    m   = lapply(stage, function(x) sub$hand_raster < x) %>% 
      bind_cols() 
    
    tmp = lapply(1:ncol(m), function(x) {sub$slope_add}) %>% bind_cols()
    
    slope_count = data.frame(tmp * m) %>%
      mutate(catchID = sub$catchmask, in_bank = sub$in_bank) %>%
      group_by(catchID, in_bank) %>%
      summarise_all(~sum(., na.rm = TRUE))
    
    rm(tmp)
    
    cm = sort(unique(sub$catchmask))
    
    Ch_BedArea = fill.df(slope_count, cm, 1) %>% dplyr::select(-in_bank, -catchID) 
    OB_BedArea = fill.df(slope_count, cm, 0) %>% dplyr::select(-in_bank, -catchID)
    BedArea    = Ch_BedArea + OB_BedArea
    
    CellCount =  mutate(m, catchID = sub$catchmask) %>% 
      group_by(catchID) %>%
      summarise_all(~ sum(., na.rm = TRUE)) %>% 
      dplyr::select(-catchID)
    
    SurfaceArea = CellCount * sub$cellarea
    
    Volume = lapply(c(stage), flood, sub = sub) %>% 
      bind_cols() %>%
      mutate(catchID = sub$catchmask) %>%
      group_by(catchID) %>%
      summarise_all(~ sum(., na.rm = TRUE)) %>%
      dplyr::select(-catchID)
    
    rm(sub)
    rm(m)
    
    length_array = table$Length_m
    
    out = list()
    
    g = expand.grid(inN, outN) %>% setNames(c('inN', 'outN'))
    if(control){ g = g[g$inN <= g$outN,] }
    g = g[!duplicated(g),]
    
    for(n in 1:nrow(g)){
      
      inN = g$inN[n]
      outN = g$outN[n]
      
      TopWidth           = SurfaceArea/length_array
      Ch_WettedPerimeter = Ch_BedArea/length_array
      OB_WettedPerimeter = OB_BedArea/length_array
      WettedPerimeter    = Ch_WettedPerimeter + OB_WettedPerimeter
      n_adjust           = ((Ch_WettedPerimeter * (inN^1.5) + (OB_WettedPerimeter *(outN^1.5)))^0.667)/(WettedPerimeter^0.667)
      WetArea            = Volume/length_array
      HydraulicRadius    = WetArea / WettedPerimeter
      Discharge          = (WetArea* HydraulicRadius^(2/3) * table$Slope^(1/2)) / n_adjust
      
      vec_t = function(x){ as.vector(t(x))}
      
      out[[n]] = data.frame(
        'ID'                = rep(table$CatchID, each = length(stage)),
        'Length_m'          = rep(table$Length_m, each = length(stage)),
        'Slope'             = rep(table$Slope, each = length(stage)),
        'OB_Roughness'      = rep(outN, each = length(stage)),
        'Ch_Roughness'      = rep(inN, each = length(stage)),
        'Stage_m'           = rep(stage, times = NROW(table)),
        'FloodCells'        = vec_t(CellCount),
        'SurfaceArea_m2'    = vec_t(SurfaceArea),
        'Ch_BedArea_m2'     = vec_t(Ch_BedArea),
        'OB_BedArea_m2'     = vec_t(OB_BedArea),
        'BedArea_m2'        = vec_t(BedArea),
        'n_adjusted'        = vec_t(n_adjust),
        'Volume_m3'         = vec_t(Volume),
        'TopWidth_m'        = vec_t(TopWidth),
        'Ch_to_TotWP'       = vec_t(Ch_WettedPerimeter/ WettedPerimeter),
        'WettedPerimeter_m' = vec_t(WettedPerimeter),
        'WetArea_m2'        = vec_t(WetArea),
        'HydraulicRadius_m' = vec_t(HydraulicRadius),
        'Discharge_m3s-1'   = vec_t(Discharge),
        stringsAsFactors    = TRUE
      )
      
    }
    
  } else {
    out = NULL
  }
  return(out)
}

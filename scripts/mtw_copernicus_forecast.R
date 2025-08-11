suppressPackageStartupMessages({
  library(stars)
  library(oisster)
  library(andreas)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

list_mtw = function(p = 90,
                     path = andreas::copernicus_path("mtw","chfc")){
  ff = list.files(file.path(path, sprintf("q%0.2i", p[1])), 
                  full.names = TRUE,
                  pattern = "^.*\\.tif$") 
  doys = gsub(".tif", "", basename(ff), fixed = TRUE)
  names(ff) <- doys
  ff
}


generate_mtw = function(prob = c(10, 90),
                         event_window = 5){
  
  cpath = andreas::copernicus_path("chfc/GLOBAL_ANALYSISFORECAST_PHY_001_024")
  pfiles_lo = list_mtw(p = prob[1])
  pfiles_hi = list_mtw(p = prob[2])
  cDB = andreas::read_database(cpath) |>
    dplyr::filter(variable == "thetao") |>
    dplyr::arrange(date) |>
    dplyr::filter(date >= Sys.Date() - event_window) |>
    dplyr::mutate(doy = format(.data$date, "%j"))
  
  ss = cDB |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        #print(row)
        
        p_hi = stars::read_stars(pfiles_hi[row$doy]) |>
          
          rlang::set_names("p_hi")
        p_lo = stars::read_stars(pfiles_lo[row$doy]) |>
          rlang::set_names("p_lo")
        
        sst = compose_filename(row, cpath) |>
          stars::read_stars() |>
          rlang::set_names("sst")
        stars::st_dimensions(sst) <- stars::st_dimensions(p_hi)
        
        mwe = p_hi * 0
        ix = sst[[1]] > p_hi[[1]]
        mwe[[1]][ix] <- 1
        ix = sst[[1]] < p_lo[[1]]
        mwe[[1]][ix] <- -1
        names(mwe) <- "mwe"
        c(sst, p_lo, p_hi, mwe, along = NA_integer_, tolerance = 1e-6)
      }) 
  do.call(c, append(ss, list(along = list(time = cDB$date))))
}

count_mwe = function(x = generate_mtw()){
  stars::st_apply(x['mwe'], 1:2, cumsum)
}

s = generate_mtw()
n = count_mwe(s)



plot_mtw = function(x){
  ggplot() + 
    geom_stars(data = s['mwe']) +
    scale_fill_gradient2(high = scales::muted("red"), 
                         low = scales::muted("blue")) + 
    coord_equal() +
    facet_wrap(~time) +
    theme_void() +
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))
}

plot_count = function(x = count_mwe()){
  ggplot() + 
    geom_stars(data = s['mwe']) +
    #scale_fill_gradient2(high = scales::muted("red"), 
    #                     low = scales::muted("blue")) + 
    coord_equal() +
    facet_wrap(~time) +
    theme_void() +
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))
}



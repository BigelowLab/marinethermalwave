suppressPackageStartupMessages({
  library(stars)
  library(oisster)
  library(andreas)
  library(dplyr)
  library(ggplot2)
  library(scales)
})


#' Generate a listing of available files
#' 
#' @param prob numeric percentile (0-100)
#' @param path the path to the percentile data directories
#' @return a named vector of full filenames
list_mtw = function(prob = 90,
                     path = andreas::copernicus_path("mtw","chfc")){
  ff = list.files(file.path(path, sprintf("q%0.2i", prob[1])), 
                  full.names = TRUE,
                  pattern = "^.*\\.tif$") 
  doys = gsub(".tif", "", basename(ff), fixed = TRUE)
  names(ff) <- doys
  ff
}


#' Generate the MTW even data for Copernicus `thetao` surface data
#' 
#' @param prob number, one or more probablilities (percentiles)
#' @param dates Date, two element start and stop
#' @return stars object with sst (thetao), phi, plo and mwe (marine weather event)
#'    heating events are positive, cooling events are negative
generate_mtw = function(prob = c(10, 90),
                        dates = c(Sys.Date() - 5, Sys.Date() + 10)){
  
  dates = seq(from = dates[1], to = dates[2], by = "day")
  cpath = andreas::copernicus_path("chfc/GLOBAL_ANALYSISFORECAST_PHY_001_024")
  pfiles_lo = list_mtw(prob = prob[1])
  pfiles_hi = list_mtw(prob = prob[2])
  cDB = andreas::read_database(cpath) |>
    dplyr::filter(variable == "thetao") |>
    dplyr::arrange(date) |>
    dplyr::filter(date %in% dates) |>
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


#' Given marine thermal wave data, compute duration of heating/cooling waves
#' 
#' @param x stars object that includes 'mwe' 
#' @param event_window num, the number of days needed to define an event
#' @param gap_width num, when gaps of this or fewer days occur between events, 
#'   try to merge the events
#' @param mask logical, if TRUE, then mask the result so only events of 
#'   event_window days are returned.  Shorter events are set to 0.
#' @return stars object with records of thermal waves durations (hence "mtwd").
encode_mtwd = function(x = generate_mtw(), 
                       event_window = 5, 
                       gap_width = 2,
                       mask = TRUE){
  if ('mwe' %in% names(x)) x = x['mwe']
  m = x[[1]]
  R = apply(m, 1:2, 
            function(v = c(0,1,1,1,1,1,0,0,1,1,1,1,1,1,-1)){
              r = rle(v)
              # right here we need to deal with gaps <= gap_width
              # if a gap < gap_width and before/after are the same sign
              # and greater than event_window than it is 
              ix = r$lengths <= gap_width
              if (any(ix)){
                ix = which(ix)
                for (i in ix){
                  if (i == 1){
                    # skip - do nothing
                  } else if (i == length(r$lengths)) {
                    # skip - do nothing
                  } else {
                    # merge the runs... by adjusting v
                    if (sign(r$values[i-1]) == sign(r$values[i+1]) &&
                        r$lengths[i-1] >= event_window[1] && 
                        r$lengths[i+1] >= event_window[1] ){
                        # gap is less than gap_width, 
                        # signs before and after are the same
                        # each is at least event_window long
                        starts = cumsum(c(1, r$lengths))
                        start = starts[i-1]
                        len = sum(r$lengths[(i-1):(i+1)])
                        index = seq(from = start, length = len)
                        v[index] = r$values[i-1]
                    } # signs are the same before and after - otherwise skip
                  }
                }
                r = rle(v)
              } # any ix
              n = rep(r$lengths, times = r$lengths) * sign(v)
              n
            }) |>
    aperm(c(2,3,1))
  if (mask) {
    ix = R > (-1*event_window) & R < event_window
    R[ix] <- 0
  }
  x[[1]] <- R
  names(x) <- "mtwd"
  x
}


if (FALSE){
  mtw = generate_mtw()
  mtwd = encode_mtwd(mtw)
}


#' Plot a set of 'mtwd' rasters
#' 
#' @param x stars 'mtwd' raster ala `encode_mtwd`
#' @return ggplot2 object 
plot_mtwd = function(x = encode_mtwd()){
  ggplot() + 
    geom_stars(data = x[1]) +
    scale_fill_gradient2(high = scales::muted("red"), 
                         low = scales::muted("blue")) + 
    coord_equal() +
    facet_wrap(~time) +
    theme_void() +
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))
}


plot_mtw = function(x = generate_mtw()){
  ggplot() + 
    geom_stars(data = x['mwe']) +
    scale_fill_gradient2(high = scales::muted("red"), 
                         low = scales::muted("blue"),
                         guide = "none") + 
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

count_mwe = function(x = generate_mtw()){
  m = x['mwe'][[1]]
  mhi = (m > 0) * 1
  mhics = apply(mhi, 1:2, cumsum) |>
    aperm(c(2,3,1))
  mlo = (m < 0) * 1
  mlocs = apply(mlo, 1:2, cumsum) |>
    aperm(c(2,3,1))
  
  m = m * 0
  ihi = which(mhics > 0)
  m[ihi] = mhics[ihi]
  ilo = which(mlocs > 0)
  m[ilo] = 0 - mlocs[ilo]
  
  r = x['mwe'] |>
    rlang::set_names("mtw")
  r[[1]] = m
  r
}

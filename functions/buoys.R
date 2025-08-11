read_buoys = function(form = c("table", "sf")[2]){
  
  #' Read in a table of buoy names and locations
  #' 
  #' @param form chr, one of 'table' or 'sf'
  #' @return either a table or sf table
  
  x = structure(list(name = c("wms", "cms", "pb", "ems", "jb", "nec", "nslb", "gb", "bb"), 
                     longname = c("Western Maine Shelf", "Central Maine Shelf", 
                                  "Penobscot Bay", "Eastern Maine Shelf", "Jordan Basin", 
                                  "Northeast Channel", "Nantucket Shoals LB",
                                  "Georges Bank", "Banqureau Banks"), 
                     id = c("B01", "E01", "F01", "I01", "M01", "N01", "NSLB", "GB", "BB"), 
                     lon = c(-70.4277, -69.3578, -68.99689, -68.11359, -67.88029,
                             -65.9267, -69.254, -66.546, -57.100),
                     lat = c(43.18065, 43.7148, 44.05495, 44.10163, 43.49041, 
                             42.3233, 40.500, 41.088, 44.240)), 
                row.names = c(NA, -9L), 
                class = c("tbl_df", "tbl", "data.frame"))
  if (tolower(form[1]) == "sf") x = sf::st_as_sf(x,
                                                 coords = c("lon", "lat"),
                                                 crs = 4326)
  x
}
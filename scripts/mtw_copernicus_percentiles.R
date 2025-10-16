# usage: mtw_copernicus_percentiles.R [--] [--help] [--prob PROB] [--n N]
# [--varname VARNAME] [--depth DEPTH] [--region REGION] [--email EMAIL]
# 
# Computes N-year daily of p percentiles for each day of year
# 
# flags:
#   -h, --help     show this help message and exit
# 
# optional arguments:
# -p, --prob     probability 0-1 [default: 0.1,0.9]
# -n, --n        number of recent years to compute percentiles [default: 30]
# -v, --varname  variable name(s) such as 'thetao' or 'thetao+so' [default: thetao+so]
# -d, --depth    depth(s) such as 'sur' or 'sur+bot' [default: sur+bot]
# -r, --region   one regional code ala 'chfc' - just one please [default: chfc]
# -e, --email    email to send at end, 'none' to skip [default: none]

suppressPackageStartupMessages({
  library(andreas)
  library(stars)
  library(dplyr)
  library(argparser)
  library(charlier)
})

# mthw/chfc/thetao/q10
# <root>/region/variable/depth/qNN/001.tif
main = function(probs = c(0.1, 0.9), 
                N = 30, 
                region = 'chfc',
                varnames = c("temp", "sal"),
                depths = c("sur", "bot"),
                lut = variable_lut()){
  charlier::start_logger(file.path(regionpath, "log.txt"))
  charlier::info("varname: %s, depths: %s N: %i, probs: %s", 
                 paste(varnames, collapse = ", "), 
                 paste(depths, collapse = ", "),
                 N, 
                 paste(probs, collapse = ", "))
  r = andreas::read_database(datapath, multiple = TRUE) |>
    dplyr::filter(period == "day",
                  name %in% varnames,
                  depth %in% depths,
                  treatment == "raw") |>
    dplyr::filter(date <= as.Date("2023-12-31")) |>
    dplyr::arrange(date) |>
    dplyr::mutate(doy = format(date, "%j")) |>
    dplyr::group_by(name, depth, doy) |>
    dplyr::slice_tail(n = N[1]) |>    # magically it takes the earliest N per doy
                                      # day 366 will be less than N but let's ignore
                                      # that issue for now
    dplyr::group_map(
      function(grp, key){
        charlier::info("varname: %s depth: %s doy: %s", 
                       key$name, key$depth, key$doy)
        # mthw/chfc/thetao/q10
        # <root>/region/variable/depth/qNN/001.tif
        #varname = dplyr::filter(lut, variable == key$variable) |>
        #  dplyr::pull(name)
        ofile = file.path(regionpath,
                          key$name, 
                          key$depth,
                          sprintf("q%0.2i", probs * 100),
                          sprintf("%s.tif", key$doy))
        odir = dirname(ofile)
        if (!all(dir.exists(odir))) ok = sapply(odir, dir.create, recursive = TRUE, showWarnings = FALSE)
        r = andreas::read_andreas(grp, datapath) |>
          stars::st_apply(1:2, quantile, probs = probs, na.rm = TRUE)
        for (i in seq_along(probs)){
          dplyr::slice(r, "quantile", i) |>
            stars::write_stars(ofile[i])
        }
        dates = range(grp$date)
        dplyr::tibble(
          varname = key$name[1],
          depth = key$depth[1],
          doy = key$doy[1],
          first = dates[1],
          last = dates[2],
          n = nrow(grp),
          probs = Args$prob)
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    readr::write_csv(file.path(andreas::copernicus_path("mthw"), "metadata.csv"))
  return(0)
}

parse_argument = function(x, type = c("character", "numeric")[1], sep = '[,+]'){
  strsplit(x, sep)[[1]] |>
    as(type)
}


Args = arg_parser("Computes N-year daily of p percentiles for each day of year",
                  name = "mtw_copernicus_percentiles.R",
                  hide.opts = TRUE) |>
  argparser::add_argument("--prob",
                          help = "probability 0-1",
                          default = "0.1,0.9") |>
  argparser::add_argument("--n",
                          help = "number of recent years to compute percentiles",
                          default = 30) |>
  argparser::add_argument("--varname",
                          help = "variable name(s) such as 'temp' or 'temp+sal'",
                          default = "temp+sal") |>
  argparser::add_argument("--depth",
                          help = "depth(s) such as 'sur' or 'sur+bot'",
                          default = "sur+bot") |>
  argparser::add_argument("--region",
                          help = "one regional code ala 'chfc' - just one please",
                          default = "chfc") |>
  argparser::add_argument("--email",
                          help = "email to send at end, 'none' to skip",
                          default = "none") |>
  argparser::parse_args()


region = Args$region[1]
regionpath = andreas::copernicus_path("mthw", region)
datapath = andreas::copernicus_path(region)
probs = parse_argument(Args$prob, type = "numeric")
N = Args$n
varnames =  parse_argument(Args$varname)
depths =  parse_argument(Args$depth)

if (!interactive()){
  ok = main(probs = probs, N = N, ds, depth = depths, region = region)
  if (grepl("@", Args$email, fixed = TRUE)){
    charlier::sendmail(to = Args$email,
                       subject = "mtw_copernicus_percentile is done",
                       message = "You are welcome!")
  }
  quit(save = "no", status = ok)
}

# usage: mtw_coperncius_percentiles.R [--] [--help] [--prob PROB] [--n N] [--email EMAIL]
# 
# Computes N-year daily of p percentiles for each day of year
# 
# flags:
#   -h, --help   show this help message and exit
# 
# optional arguments:
#   -p, --prob   probability 0-1 [default: 0.9]
#   -n, --n      number of recent years to compute percentiles [default: 30]
#   -e, --email  email to send at end, 'none' to skip [default: none]

suppressPackageStartupMessages({
  library(andreas)
  library(stars)
  library(dplyr)
  library(argparser)
  library(charlier)
})

main = function(prob = c(0.1, 0.9), N = 30, varname = "thetao"){
  charlier::start_logger(file.path(varpath, "log.txt"))
  charlier::info("varname: %s, N: %i, probs: %s", 
                 varname, N, paste(prob, collapse = ", "))
  r = andreas::read_database(datapath, multiple = TRUE) |>
    dplyr::filter(period == "day",
                  variable == varname[1],
                  treatment == "raw") |>
    dplyr::filter(date <= as.Date("2023-12-31")) |>
    dplyr::arrange(date) |>
    dplyr::mutate(doy = format(date, "%j")) |>
    dplyr::group_by(doy) |>
    dplyr::slice_tail(n = N[1]) |>
    dplyr::group_map(
      function(grp, key){
        charlier::info("varname: %s doy: %s", varname, key$doy[1])
        ofile = file.path(opath, sprintf("%s.tif", key$doy[1]))
        r = andreas::read_andreas(grp, datapath) |>
          stars::st_apply(1:2, quantile, probs = prob, na.rm = TRUE)
        for (i in seq_along(prob)){
          dplyr::slice(r, "quantile", i) |>
            stars::write_stars(ofile[i])
        }
        dates = range(grp$date)
        dplyr::tibble(
          varname = varname, 
          doy = key$doy[1],
          first = dates[1],
          last = dates[2],
          n = nrow(grp),
          probs = Args$prob)
      }) |>
    dplyr::bind_rows() |>
    readr::write_csv(file.path(outpath, "metadata.csv"))
  return(0)
}

parse_probs = function(x = "0.1,0.9"){
  strsplit(x, ",", fixed = TRUE)[[1]] |> 
    as.numeric()
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
                          help = "variable name",
                          default = "thetao") |>
  argparser::add_argument("--email",
                          help = "email to send at end, 'none' to skip",
                          default = "none") |>
  argparser::parse_args()


datapath = andreas::copernicus_path("chfc")
prob = parse_probs(Args$prob)
N = Args$n
varname = Args$varname
varpath = andreas::copernicus_path("mtw", "chfc", varname)
opath = andreas::copernicus_path(varpath, sprintf("q%0.2i", prob * 100))
if (!all(dir.exists(opath))) ok = sapply(opath, dir.create, recursive = TRUE, showWarnings = FALSE)

if (!interactive()){
  ok = main(prob = prob, N = N, varname = varname)
  if (grepl("@", Args$email, fixed = TRUE)){
    charlier::sendmail(to = Args$email,
                       subject = "mtw_copernicus_percentile is done",
                       message = "You are welcome!")
  }
  quit(save = "no", status = ok)
}

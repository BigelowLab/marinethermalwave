path = "/mnt/ecocast/corecode/R/marinethermalwave"
setwd(path)
knitr::knit(input = file.path("README.Rmd"),
                output = file.path("README.md"))

cmd = "git add *"
ok = system(cmd)

cmd = sprintf("git commit -a -m 'autocommit %s'", 
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
ok = system(cmd)

cmd = "git push origin main"
ok = system(cmd)
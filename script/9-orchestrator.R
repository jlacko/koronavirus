# orchestrátor: stahne data, nakreslí obrázky, nahraje soubor pro blog & that's it - do žádné složitější logiky se nepouští

# ať je v logu na co koukat... :)
print(paste("korunavirus nastartován", Sys.time()))

# cesta k packagím při spuštění z příkazové řádky (i.e. cron job)
.libPaths("/usr/lib/R/site-library")

setwd('/home/jindra/koronavirus/')

capture.output({
  
  source('./script/1-digest-data.R') # stahne surová data a uloží do adresáře /data
  source('./script/2-draw-chart.R')    # z dat v /data spočte obrázek o ČR a trendu
  source('./script/3-srovnani-zemi.R') # z dat v /data spočte obrázek o sronvání se světem
  
}, file = '/dev/null')


if (file.exists('sync.sh')) system('./sync.sh') # uloží obrázky na internet 

print(paste("korunavirus doběhl", Sys.time()))

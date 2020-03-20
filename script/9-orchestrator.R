# pouze pro zjednodušení orchestrace; vnitřní logika žádná
  
  source('./script/1-digest-data.R') # stahne surová data a uloží do adresáře /data
  source('./script/2-draw-chart.R')    # z dat v /data spočte obrázek o ČR a trendu
  source('./script/3-srovnani-zemi.R') # z dat v /data spočte obrázek o sronvání se světem


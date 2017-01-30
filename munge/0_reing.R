library(tidyverse)

# load('cache/dat.ocupados.sector.RData')
head(dat.ocupados.sector)
names(dat.ocupados.sector)[2] <- "year.reing"

# load('cache/dat.reing.conj.RData')
head(dat.reing.conj)

# load('cache/dat.ocupados.RData')
head(dat.ocupados)
names(dat.ocupados)[names(dat.ocupados) == 'año.reing'] <- "year.reing"


# • Sector económico
tab <- dat.reing.conj %>% 
  # filter(CVE_ENT_FEDERATIVA == 9) %>% 
  filter(CVE_MODALIDAD == 10) %>%
  group_by(mes.reing, 
           año.reing, sector.rec) %>% 
  summarise(cuenta = n(),
            unicos = length(unique(CVE_NSS)) ) %>% 
  ungroup %>% 
  mutate(fechames = as.Date(paste('01', mes.reing, 
                                  año.reing, sep = "-"),
                            "%d-%m-%Y" )) %>% 
  filter(fechames < "2016-05-01", 
         !is.na(sector.rec))
# rm('dat.reing.conj')



# ocupación por sector
tt <- tab %>% 
  dplyr::select(-mes.reing, -año.reing) %>% 
  mutate(sector.ocup = as.character(fct_recode(sector.rec, 
                                  otros = 'serv. sociales',
                                  indman = 'ind. transformación',
                                  servicios = 'serv. empresas, personas y hogar', 
                                  comercio = 'comercio', 
                                  constr = 'ind. construccion', 
                                  otros = 'transporte y comunicaciones',
                                  otros = 'ind. extractiva',
                                  otros = 'ind. eléctrica ext. y suministro agua',
                                  agropecuario = 'agr gan silv pesca y caza'))) %>%
  left_join(
    dat.ocupados.sector %>% 
      mutate(fechames = as.Date( paste("01", mes.reing, year.reing, sep = "-"),
                                 format = "%d-%m-%Y") ) ,
    by = c("fechames", "sector.ocup")
  ) %>% 
  mutate(tasa.ocup.sector = tasa.ocup.sector/100) %>% 
  # indice global de actividad económica
  left_join(
    read_csv("data/igae_mensual.csv") %>% 
      mutate(fechames = as.Date( paste("01", month, year, sep = "-"),
                                 format = "%d-%m-%Y")) %>% 
      dplyr::select(-year, -month), 
    by =  "fechames"
  ) %>% 
  # indice global de actividad económica
  left_join(
    read_csv("data/tipodecambio.csv") %>% 
      dplyr::rename(fechames = fecha),
    by = c("fechames")
  ) %>% 
  filter(!is.na(tasa.ocup.sector)) %>% 
  # tt <- tab.cuenta.sector %>% 
  left_join(
    dat.ocupados %>% 
      mutate(fechames = as.Date( paste("01", mes.reing, year.reing, sep = "-"),
                                 format = "%d-%m-%Y")) %>% 
      dplyr::select(-mes.reing, -year.reing), 
    by =  "fechames"
  ) 

# NACIONAL
tab.cuenta.sector <- tt
apply(is.na(tab.cuenta.sector), 2, sum)
dim(tab.cuenta.sector)

tab.cuenta.sector$sector.rec %>% levels()
rec.sectores <- paste("'",
                      sort(tab.cuenta.sector$sector.rec %>% levels()), 
                      "' = '",
                      c("agricultura", "comercio", "construcción", 
                        "eléctrica", "extractiva", "transformación",
                        "servicios", "serv. sociales", "transporte"),
                      "'", sep = "", collapse = "; ")

table(tab.cuenta.sector$sector.rec, car::recode(
  tab.cuenta.sector$sector.rec, 
  rec.sectores
), useNA = "always")

tab.cuenta.sector$sector.rec <-  car::recode(
  tab.cuenta.sector$sector.rec, 
  rec.sectores
)

cache('tab.cuenta.sector')

# MODALIDAD 10
tab.cuenta.sector10 <- tt
apply(is.na(tab.cuenta.sector10), 2, sum)
dim(tab.cuenta.sector)
cache('tab.cuenta.sector10')

# CDMX
tab.cuenta.sector.cdmx <- tab.cuenta.sector.cdmx %>% 
  left_join(
    read_csv(file = "data/temp_cdmx.csv")
  )

cache('tab.cuenta.sector.cdmx')

library(ProjectTemplate)
load.project()

load('cache/tab.cuenta.sector.RData')


# Modelo completo

table(datos$sector.rec, as.numeric(datos$sector.rec))

datos <- tab.cuenta.sector %>% 
  filter(fechames < "2016-04-01", 
         fechames >= "2014-01-01") %>% 
  mutate(sector.num = as.numeric(sector.rec),
         unicos = unicos) %>% 
  # filter(sector.num %in% c(2,3,4,5,9)) %>%
  # mutate(sector.num = as.numeric(factor(as.character(sector.rec))) ) %>%
  arrange(fechames, sector.rec) 

dplyr::select(datos, sector.rec, sector.num) %>% unique

datos %>% print(n = 30, width = Inf)
datos %>% tail()

n <- nrow(datos)
nsec <- n_distinct(datos$sector.num)
data <- list("n" = n,
             "nsec" = nsec,
             # "y" =  c(datos$unicos[-(n-(nsec-1)):-n], rep(NA, nsec) ),
             "y" = c(round(datos$unicos/100)[-(n-(nsec-1)):-n], rep(NA, nsec) ),
             # "y" = datos$unicos,
             "sector" = datos$sector.num, 
             "tasa.ocup" = datos$tasa.ocup.sector, 
             "igae" = datos$igae/100)

# Inciales y Parámetros
init <- function(){ list(
  alpha = 0,
  beta.sector = rep(0, n_distinct(datos$sector.num) )
  )}#rep(1, max(datos$sector.num)) ) }
params <-  c('alpha', 'beta.igae', 'beta.sector.adj', 'yf')

poi.jags <- jags(data,
                 init,
                 params,
                 model.file="doc/model_poisson.jags",
                 n.iter = 100000,
                 n.chains = 1, 
                 n.thin = 1,
                 n.burnin = 10000)
poi.jags$BUGSoutput$DIC
plot(poi.jags)

# saveRDS(poi.jags, "cache/modelos/poi_jags.rds")

tab.sum <- poi.jags$BUGSoutput$summary
tab.sum %>% head(11)
tab.preds <- tab.sum[grep("yf", rownames(tab.sum)), ] %>% 
  data.frame(check.names = F)  %>% 
  cbind(
    datos %>% 
      dplyr::select(fechames, 
                    observado = unicos, 
                    sector =sector.num) %>% 
      mutate(observado= round(observado/100))
  )
cor(tab.preds$mean, tab.preds$observado)^2

# serie de tiempo obs vs. pred
ggplot(tab.preds, aes(x = fechames, y = mean)) + 
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = .4) + 
  geom_line(color = 'red', alpha = .4)  +
  geom_line(aes(y = observado), color = 'gray50')  +
  facet_wrap(~sector, scales = 'free')

# scatterplot de obs vs. pred
ggplot(tab.preds, aes(x = observado, y = mean)) + 
  geom_abline(color = 'red')+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = .2) + 
  geom_point(color = 'gray50')  + 
  ylab('ajustado') + 
  xlab('observado') + 
  facet_wrap(~sector, scales = 'free')
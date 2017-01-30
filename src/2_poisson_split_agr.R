library(ProjectTemplate)
load.project()

# load('cache/tab.cuenta.sector.RData')
# load('cache/tab.cuenta.sector.cdmx.RData')


# • Modelo sectores pequeños

tab.cuenta.sector <- tab.cuenta.sector %>% 
  mutate(sector.rec = fct_reorder(sector.rec, unicos))

tab.cuenta.sector %>% 
  mutate(sector.num = as.numeric(sector.rec)) %>% 
  dplyr::select(sector.rec, sector.num) %>% 
  unique %>% 
  arrange(sector.num)

datos <- tab.cuenta.sector %>% 
  filter(fechames < "2016-04-01", 
         fechames >= "2008-01-01") %>% 
  mutate(sector.num = as.numeric(sector.rec),
         unicos = unicos) %>% 
  filter(sector.num %in% c(3)) %>%
  mutate(sector.num = as.numeric(factor(as.character(sector.rec))) ) %>%
  arrange(fechames, sector.rec) 

table(datos$sector.rec, as.numeric(datos$sector.rec))
(tab.sector <- dplyr::select(datos, sector.rec, sector.num) %>% unique)

datos %>% print(n = 30, width = Inf)
datos %>% tail()

n <- nrow(datos)
nsec <- n_distinct(datos$sector.num)
data <- list("n" = n,
             "nsec" = nsec,
             "y" =  c(datos$unicos[-(n-(nsec-1)):-n], rep(NA, nsec) ),
             # "y" = c(round(datos$unicos/100)[-(n-(nsec-1)):-n], rep(NA, nsec) ),
             # "y" = datos$unicos,
             "sector" = datos$sector.num, 
             "tasa.ocup" = datos$tasa.ocup.sector, 
             "tipo.cambio" = datos$tipo_cambio, 
             "til" = datos$til/100, 
             # "temp" = datos$temp, 
             "igae" = datos$igae/100)

# Inciales y Parámetros
init <- function(){ list(
  alpha = 0,
  beta.sector = rep(0, n_distinct(datos$sector.num) )
)}#rep(1, max(datos$sector.num)) ) }
params <-  c('alpha', 
             'beta.til', 'beta.tcambio', 
             'beta.t',
             'yf')

poiagr.jags <- jags(data,
                    init,
                    params,
                    model.file="doc/model_poisson_agr.jags",
                    n.iter = 100000,
                    n.chains = 1, 
                    n.thin = 1,
                    n.burnin = 10000)
poiagr.jags$BUGSoutput$DIC
plot(poiagr.jags)

# poiagr.jags <- read_rds("cache/modelos/poiagr.jags.rds")

tab.sum <- poiagr.jags$BUGSoutput$summary
tab.sum %>% head(11)
tab.preds <- tab.sum[grep("yf", rownames(tab.sum)), ] %>% 
  data.frame(check.names = F)  %>% 
  cbind(
    datos %>% 
      dplyr::select(fechames, 
                    observado = unicos, 
                    sector =sector.rec) #%>% 
      # mutate(observado= round(observado/100))
  )
tab.coef <- tab.preds %>% filter(fechames < "2014-03-01")
cor(tab.coef$mean, tab.coef$observado)^2

# serie de tiempo obs vs. pred
gg.preds <- ggplot(tab.preds %>% 
                     filter(fechames < "2016-03-01"),
                   aes(x = fechames, y = mean)) + 
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = .3) + 
  geom_line(color = 'red')  +
  geom_line(aes(y = observado), color = 'gray20')  +
  facet_wrap(~sector, scales = 'free') + 
  ylab("Nuevos Empleos") + 
  xlab("Mes y Año")
ggsave(filename = "graphs/4_agr_preds.png", plot = gg.preds, 
       width = 9, height = 5)

# scatterplot de obs vs. pred
ggplot(tab.preds, aes(x = observado, y = mean)) + 
  geom_abline(color = 'red')+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = .2) + 
  geom_point(color = 'gray50')  + 
  ylab('ajustado') + 
  xlab('observado') + 
  facet_wrap(~sector, scales = 'free')

# saveRDS(poiagr.jags, "cache/modelos/poiagr.jags.rds")


# • alpha
tab.coefs <- tab.sum[grep("alpha|til|tcambio", rownames(tab.sum)), ] %>% 
  data.frame(check.names = F) %>% 
  mutate(coeficiente = c("intercept", "tipo de cambio", "tasa informalidad")) %>% 
  dplyr::select(coeficiente, mean:`97.5%`)

prob.alpha <- 1 - (poiagr.jags$BUGSoutput$sims.list$alpha %>% 
                      apply(., 2, prob))
prob.tcambio <- 1 - (poiagr.jags$BUGSoutput$sims.list$beta.tcambio %>% 
                      apply(., 2, prob))
prob.til <- 1 - (poiagr.jags$BUGSoutput$sims.list$beta.til %>% 
                      apply(., 2, prob))

tab.coefs %>% 
  mutate(probabilidad = c(prob.alpha, prob.tcambio, prob.til)) %>% 
  xtable() %>% 
  print(include.rownames = F)

gg <- ggplot(tab.coefs, aes(x = mean, y = coeficiente)) + 
  geom_errorbarh(aes(xmin = `25%`, xmax = `75%`), 
                 height = .2, alpha = .8 )  +
  geom_errorbarh(aes(xmin = `2.5%`, xmax = `97.5%`), 
                 height = .2, alpha = .5 ) +
  geom_point(color = 'red', size = 3) + 
  xlab( expression(beta) ) + 
  ylab(NULL) + 
  ggtitle('Coeficientes')
ggsave(filename = "graphs/4_agr_coefs.png", plot = gg, 
       width = 8.5, height = 3.5)

# • Tasa ocupacion
tasa.ocup <- tab.sum[grep("beta.t\\[", rownames(tab.sum)), ] %>% 
  data.frame(check.names = F)  %>% 
  cbind(
    datos %>% 
      dplyr::select(fechames, 
                    sector =sector.rec)
  )

gg <- ggplot(tasa.ocup, aes( x = fechames, y = mean, 
                             group = sector))+ 
  geom_smooth(method = 'lm', se = F, size = .3, color = 'gray40') + 
  geom_line(aes(color = sector)) + 
  ylab(expression(beta)) + 
  xlab("Fecha y Mes") + 
  ggtitle("Tasa de Ocupación por Sector")
ggsave(gg, filename = "graphs/4_agr_tasaocup.png", 
       width = 8.5, height = 5)


# Convergencia

gg.alpha <- ParamSimPlot(poiagr.jags$BUGSoutput$sims.list$alpha , "alpha")
gg.betatil <- ParamSimPlot(poiagr.jags$BUGSoutput$sims.list$beta.til , "TIL")
gg.betatc <- ParamSimPlot(poiagr.jags$BUGSoutput$sims.list$beta.tcambio , "Tipo Cambio")

png("graphs/6_agr_coefs_pelos.png")
gridExtra::grid.arrange(gg.alpha[[1]], 
                        gg.betatil[[1]],
                        gg.betatc[[1]], 
                        gg.alpha[[3]],
                        gg.betatil[[3]],
                        gg.betatc[[3]],
                        nrow = 2)  
dev.off()


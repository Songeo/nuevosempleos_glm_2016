library(ProjectTemplate)
library(forcats)
library(xtable)
library(car)
reload.project()

load('cache/tab.cuenta.sector.RData')


gg.nac <- tab.cuenta.sector %>% 
  group_by(fechames) %>% 
  summarise(total = sum(unicos)) %>% 
  ggplot(aes(x = fechames, y = total/100)) + 
  geom_line(color = 'gray40', size = 1) +
  # geom_smooth(se = F) + 
  ylab('Conteo nacional (x100)') + 
  xlab('Fecha')
ggsave(plot = gg.nac, filename = "graphs/1_nac_conteo.png",
       width = 8, height = 3)

tab.cuenta.sector <- tab.cuenta.sector %>% 
  mutate(sector.rec = fct_reorder(sector.rec, unicos))

gg.sec <- ggplot(tab.cuenta.sector, aes(x = fechames, 
                                        y = unicos/100)) + 
  geom_line(aes(color = sector.rec, 
                group = sector.rec), size = 1) +
  guides(color = guide_legend(title = "Sector económico")) +
  ylab('Conteo por sector (x100)') + 
  xlab('Fecha') + 
  scale_color_manual( values = cbPalette)

ggsave(plot = gg.sec, filename = "graphs/1_sector_conteo.png", 
       width = 8, height = 3)  


ggbox.sec <- ggplot(tab.cuenta.sector, aes(x = fct_reorder(sector.rec, unicos), 
                              y = unicos/100)) + 
  geom_hline(yintercept = mean(tab.cuenta.sector$unicos)/100,
             linetype = 2) +
  geom_label( x = 1, 
              y = mean(tab.cuenta.sector$unicos)/100 + 10, 
              hjust = 0, 
              label = "Total")+
  geom_boxplot(aes(fill = sector.rec), alpha = .3) +
  xlab(NULL) + 
  ylab('Conteo por sector (x100)') + 
  theme(legend.position = 'none') + 
  scale_fill_manual( values = cbPalette) +
  coord_flip()

ggsave(plot = ggbox.sec, filename = "graphs/1_sector_boxplot.png", 
       width = 8, height = 6)  

 # Tabla resumen
tab.cuenta.sector %>% 
  group_by(sector.rec) %>% 
  summarise(mediana = median(unicos),
            promedio = mean(unicos))  %>% 
  ungroup %>% 
  mutate(sector.rec = fct_reorder(sector.rec, promedio)) %>% 
  arrange(desc(sector.rec)) %>% 
  xtable(rownames = F, digits = 2)




c("Nacional", 
  median(tab.cuenta.sector$unicos),  
  mean(tab.cuenta.sector$unicos))
  
    

# Prueba
mod.poi <- bayesglm( formula = unicos ~   
                       ns(mes.reing,3) + año.reing + 
                       tasa.ocup.sector +
                      sector.rec,
                     data = tab.cuenta.sector, family = 'poisson')

# Explicativas

# load('cache/dat.ocupados.sector.RData')
head(dat.ocupados.sector)
names(dat.ocupados.sector)[2] <- "year.reing"

gg <- dat.ocupados.sector %>% 
  mutate(sector =fct_recode(sector.ocup, 
                            construccion = "constr",
                            manufacturera = 'indman')) %>% 
  mutate(fechames = as.Date( paste("01", mes.reing,
                                   year.reing, sep = "-"),
                             format = "%d-%m-%Y") ) %>% 
  ggplot(data = ., aes(x = fechames, y = tasa.ocup.sector, 
             color = fct_reorder(sector, tasa.ocup.sector) )) + 
  geom_line()+ 
  scale_color_manual(values = cbbPalette) + 
  guides(color = guide_legend( title = "Sector")) +
  ylab("Tasa de Ocupación Sector") + 
  xlab("Fecha y Mes")
ggsave(plot = gg, filename = "graphs/1_tasaocup.png")

# load('cache/dat.ocupados.RData')
head(dat.ocupados)
names(dat.ocupados)[names(dat.ocupados) == 'año.reing'] <- "year.reing"
gg <- dat.ocupados %>% 
  mutate(fechames = as.Date( paste("01", mes.reing,
                                   year.reing, sep = "-"),
                             format = "%d-%m-%Y") ) %>% 
  ggplot(data = ., aes(x = fechames, y = til )) + 
  geom_line()+ 
  ylim(c(50, 70))+
  ylab("Tasa de Informalidad Laboral") + 
  xlab("Fecha y Mes")
ggsave(plot = gg, filename = "graphs/1_til.png", width = 8.5, height = 5)
  
# Tipo de cambio
dat.tipocambio <- read_csv("data/tipodecambio.csv")
head(dat.tipocambio)
gg <- dat.tipocambio %>% 
  ggplot(data = ., aes(x = fecha, y = tipo_cambio )) + 
  geom_line()+ 
  ylim(c(10, 20))+
  ylab("Tipo de Cambio Peso/Dolar") + 
  xlab("Fecha y Mes")
ggsave(plot = gg, filename = "graphs/1_tipocambio.png", width = 8.5, height = 5)

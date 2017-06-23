# taller de visualización de datos censales con R
#Foromundo UNIGIS 2017
#Juan Fernando Correa Caicedo


# instalar librerias ----
install.packages("tidyverse")

library(rgdal)
library(sp)
library(tidyverse)
library(broom)
# paletas de color
#library(RColorBrewer)
library(viridis)


#listar shapefiles  
list.files(path = "shp/MGN", pattern="\\.shp$")

#cargar shapefiles -----
su_valle<-readOGR(dsn = "shp/MGN", layer = "MGN_SectorUrbano")
# inspeccion shapefiles cargados ----
class(su_valle)
su_valle
summary(su_valle)
names(su_valle)
proj4string(su_valle)
su_valle@proj4string
plot(su_valle)
head(su_valle@data,3)

su_cali<-su_valle[su_valle$SECR_SETR_ == "001" & su_valle$CPOB_CPOB_=="000",]
plot(su_cali)
summary(su_cali)



#eliminar campos sin interes
su_cali@data<-select(su_cali@data,SETU_CCDGO,SETU_CCNCT,SETU_NAREA)
summary(su_cali)
# su_cali.f<-fortify(su_cali,region ="SETU_CCDGO" )
su_cali.t<-tidy(su_cali,region ="SETU_CCDGO" )
head(su_cali.t)

#incorporar la info perdida
su_cali.t %>% left_join( su_cali@data,by=c("id" = "SETU_CCDGO")) ->su
head(su)


# pintemos los mapas ----
ggplot(data = su,mapping = aes(x=long,y=lat,group=group))+geom_polygon(mapping = aes(fill=SETU_NAREA))+coord_fixed()

p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
summary(p)
p+geom_polygon(aes(fill=SETU_NAREA))+
  coord_fixed()+
  scale_fill_viridis()


h<-ggplot(su_cali@data,aes(x=SETU_NAREA))
h+geom_histogram(mapping = aes(fill=cut_interval(SETU_NAREA,n = 10)))+scale_fill_viridis(discrete = T)

p+geom_polygon(aes(fill=cut_interval(SETU_NAREA,n = 30)))+
  coord_fixed()+
  scale_fill_viridis(discrete = T)


p+geom_polygon(aes(fill=cut_interval(SETU_NAREA,n = 30)))+
  coord_fixed()+
  scale_fill_viridis(discrete = T)+
  theme_void()

#Datos proyectado -----
#CRS Magana Sirgas
crs.mc<-CRS("+proj=tmerc +lat_0=3.441883333 +lon_0=-76.5205625 +k=1 +x_0=1061900.18 +y_0=872364.63 +a=6379137 +b=6357748.961329674 +units=m +no_defs" )
class(crs.mc)

su_cali.mc<-spTransform(su_cali,crs.mc)
summary(su_cali.mc)
#objeto tidy
su_cali.mc.t<-tidy(su_cali.mc,region ="SETU_CCDGO" )
head(su_cali.mc.t)

#incorporar la info perdida
su_cali.mc.t %>% left_join( su_cali.mc@data,by=c("id" = "SETU_CCDGO")) ->su
head(su)

#pintar mapa MC
p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
summary(p)
p+geom_polygon(aes(fill=SETU_NAREA))+
  coord_fixed()+
  scale_fill_viridis()

#calcular capa con centroides y etiquetas de cada SU
label_su<-su_cali.mc$SETU_CCDGO
centroids.df<-as.data.frame(coordinates(su_cali.mc))
names(centroids.df) <- c("long", "lat") 
summary(centroids.df)
su.label<-data.frame(label_su,centroids.df)
summary(su.label)
#graficar Su con etiquetas.
ggplot(su,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="lightgrey",color="white")+coord_equal()+
  theme_void()+
  geom_text(data = su.label,mapping =  aes(x=long,y=lat,label=label_su, group=NULL),size=1.5)+
  labs(title="Sectores Urbanos del Censo del 2005 ",
       subtitle="Los sectores seleccionados están parcial o \ntotalmente contenidos en el perímetro urbano 2015",
       caption="Fuente: Cartografia Censo 2005 - DANE")




# Carga datos CP2005 ----
#usar RSTUDIO
cp.personas <- read_csv("./data/CP2005 - t_persona_edad.csv",col_types = cols(su_id = col_character()))
cp.viviendas <- read_csv("./data/CP2005 - t_tipo_vivienda.csv", col_types = cols(su_id = col_character()))
cp.personas
summary(cp.personas)

cp.viviendas %>% rename(otro_tipo=otro_tipo_de_vivienda) ->cp.viviendas
summary(cp.viviendas)
plot(cp.personas %>% select(personas,edad_promedio))

#unir los datos ----

cp.personas %>% full_join(cp.viviendas, by="su_id") ->cp
head(cp)
nrow(cp)
#buscar duplicados
cp %>% 
  group_by(su_id) %>% 
  filter(n()>1) 

cp$su_id %>% nchar()
su_cali.mc$SETU_CCNCT %>%
  as.character()%>% nchar()

# Para el caso de descarga de información con agregaciones sector urbano, 
# sección urbana o manzana censal, es importante 
# suprimir los digitos 7 y 8 del codigo censal antes de cruzar con la información de las
# capas correspondiente de la cartografía censal del MGN 

cp$su_id %>% substr(1,6) -> c1
cp$su_id %>% substr(9,20) -> c2
c2
SETU_CCNCT<-paste0(c1,c2) 
nchar(SETU_CCNCT)
class(SETU_CCNCT)

#unir con cp
 data.frame(SETU_CCNCT,cp) %>% select(-su_id) ->cp.df
cp.df %>%head()
cp.df %>% nrow()
#buscar repetidos
cp.df %>% 
  group_by(SETU_CCNCT) %>% 
  filter(n()>1)  





# hay 33 repetidos. ¿que hacer? 
#descicion: sumar repetidos
cp.df %>% 
  group_by(SETU_CCNCT) %>% 
  summarise_all(sum) ->cp.df.s
nrow(cp.df.s)

su_cali.mc@data %>% nrow()

left_join(su_cali.mc@data ,cp.df.s, by="SETU_CCNCT") -> su.cp
su.cp %>% nrow()
#calcular densidad de poblacion y porcentaje de viviendas por tipo

su.cp %>%
  mutate(densidad_pob=personas/SETU_NAREA)

su.cp %>%
  mutate(densidad_pob=personas/SETU_NAREA,
         p_casas=casa/total_viviendas,
         p_apto=apartamento/total_viviendas,
         p_casaindigena=casa_indigena/total_viviendas,
         p_cuarto=tipo_cuarto/total_viviendas,
         p_otroviv=otro_tipo/total_viviendas) ->su.cp.calc

summary(su.cp.calc)

su_cali.mc.cp<-su_cali.mc
su_cali.mc.cp@data<-su.cp.calc
summary(su_cali.mc.cp)


#dibujar el mapa de densisdad ----


#incorporar la info perdida
su_cali.mc.t %>% left_join( su_cali.mc.cp@data,by=c("id" = "SETU_CCDGO")) ->su
head(su)

#pintar mapa Personas
p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
p+geom_polygon(aes(fill=personas))+
  coord_fixed()+
  scale_fill_viridis()

#pintar mapa densidad población
p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
p+geom_polygon(aes(fill=densidad_pob))+
  coord_fixed()+
  scale_fill_viridis("Densidad Población")

#pintar mapa viviendas
p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
p+geom_polygon(aes(fill=total_viviendas))+
  coord_fixed()+
  scale_fill_viridis("Viviendas")


#histograma

h<-ggplot(su_cali.mc.cp@data,aes(x=densidad_pob))
h+geom_histogram(mapping = aes(fill=cut_interval(densidad_pob,n = 10)),bins = 10)+
  scale_fill_viridis(discrete = T)

h+geom_histogram(mapping = aes(fill=cut_width(densidad_pob,0.01)),bins = 10)+
  scale_fill_viridis(discrete = T)
#relacion entre varibles
names(su_cali.mc.cp@data)
seleccionadas<-c("SETU_CCDGO","densidad_pob","p_casas","p_apto","p_casaindigena","p_cuarto","p_otroviv")
su_cali.mc.cp@data %>% select(one_of(seleccionadas)) ->seldata
su_cali.sel<-su_cali.mc
su_cali.sel@data<-seldata
summary(su_cali.sel)

# scatter matrix ----


plot(seldata[,2:ncol(seldata)])

#con GGally
install.packages("GGally")
library(GGally)

ggpairs(seldata[,2:ncol(seldata)])


lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

ggpairs(
  seldata[,2:ncol(seldata)], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 3))
)


#facets ----

#wide data, long data
names(seldata)
gather(data = seldata,key = varcp,value = valor,densidad_pob:p_otroviv) -> seldata.long


#incorporar la info seleccionada
su_cali.mc.t %>% left_join( seldata.long,by=c("id" = "SETU_CCDGO")) ->su
head(su)

#facet varibles seleccionadas
p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
p+geom_polygon(aes(fill=valor))+
  coord_fixed()+
  scale_fill_viridis()+
  facet_wrap(~varcp)


p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
p+geom_polygon(aes(fill=valor))+
  coord_fixed()+
  scale_fill_viridis()+
  facet_wrap(~varcp)
#sin densidad de poblacion
gather(data = seldata,key = varcp,value = valor,p_casas:p_otroviv) -> seldata.long2
su_cali.mc.t %>% left_join( seldata.long2,by=c("id" = "SETU_CCDGO")) ->su

p<-ggplot(data = su,mapping = aes(x=long,y=lat,group=group))
p+geom_polygon(aes(fill=valor))+
  coord_fixed()+
  scale_fill_viridis()+
  theme_void()+
  facet_wrap(~varcp)

#salvar una graficas
ggsave(path = "./output",filename = "mapviviendas.png")





rm(list=ls())
## open the library
library(tidyverse); library(writexl); library(ggplot2); library(cowplot)

# set up the work directory 
#data.dir<-'/Volumes/public/homes/aj01baqo/data from NutNet/SPEI/'
graph.dir<-"H:/potential fundings/graphs/"
data.dir<-'H:/data from NutNet/SPEI/'
setwd(graph.dir)

colour.crosswalk1 <- c("Dry"="#E69F00", "Normal"="light grey",   "Wet"="#0072B2") # "#000000"

##### Read the data for geolocation OF NutNet sites
all.s <- read.csv(paste0(data.dir, 'comb-by-plot-clim-soil-2022-11-15.csv'), header = T)
colnames(all.s)
## select variables needed 
site.inf<-all.s%>%select(site_code, habitat, continent, country, latitude, longitude, experiment_type, first_nutrient_year)%>%distinct()%>%
 arrange(continent, country)%>%as.data.frame()
table(site.inf$continent)
site.europe<-site.inf%>%filter(continent=="Europe")
table(site.europe$first_nutrient_year)
length(unique(site.inf$country))

## get the data of precipitation and potential evaporation
prec <- read.csv(paste0(data.dir, 'CRU-monthly-pre-1901-2021.csv'), header = T)%>%dplyr::rename(prec = value)
pet<-read.csv(paste0(data.dir, 'CRU-monthly-pet-1901-2021.csv'), header = T, sep=",")%>%dplyr::rename(pet = value)
  
### note that potential evaporation was calculated as average per day. 
month.df <- data.frame(months = c("Jan","Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       month1 = seq(1, 12, 1),
                       days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

# calculate the sum of precipitation and potential evaporation per year
colnames(prec)
colnames(pet)

# water balance and SPEI
dd0 <- prec %>% merge(y = pet, by=c("site_code", "years", "months", "date")) %>%
  merge(y = month.df, by=c("months"))  %>%
  mutate(pet1 = pet * days) %>%
  arrange(site_code, years, month1)%>%group_by(site_code, years)%>%
  summarise(sum.prec=sum(prec), sum.pet=sum(pet1))%>%mutate(water.balance=sum.prec-sum.pet)%>%
  group_by(site_code) %>%mutate(spei_12 = scale(water.balance))%>%
  mutate(spei_12=as.numeric(spei_12), events=case_when(spei_12>=1.28 ~ "Wet", spei_12<= -1.28 ~ "Dry", TRUE~"Normal"))

# focus in the last 20 years
dd <- dd0%>%filter(years>2000)%>%
  merge(site.inf%>%select(continent, country, site_code, latitude, longitude), by=c("site_code"))%>%arrange(country)%>%
  mutate(longitude1=round(longitude, 2), latitude1=round(latitude, 2))%>%mutate(site_code_1=paste0(site_code, " (", longitude1,", ", latitude1, ")"))

# too many sites, need to show them in three pdfs 
group.sites<-site.inf%>%arrange(continent, country)%>%mutate(grp=rep(1:3, c(50, 50, 43)))
dd1<-dd%>%merge(group.sites%>%select(site_code, grp), by=c("site_code"))%>%arrange(grp)

for (i in 1:3)  {
 # i<-1
(pp.spei.all.sites<-dd1 %>%filter(grp == i)%>%
    ggplot(aes(years, spei_12, color=events))+geom_point(size=2, alpha=0.7)+theme_cowplot(font_size = 9)+panel_border()+
    facet_wrap(~site_code_1, scale="free_x", ncol=5)+
    scale_x_continuous(expand=c(0,0), limits = c(2000, 2023), breaks = seq(2000,2023,6))+
    scale_colour_manual(values = colour.crosswalk1) +
    geom_hline(yintercept = 0, linetype="dotted")+
     guides(color = guide_legend(nrow = 1, byrow = TRUE))+theme(legend.position = "none")+
    labs(x=NULL, y="SPEI per year", color="Climate patterns"))
ggsave(pp.spei.all.sites, file=paste0("SPEI_12 for NutNet sites_", i , ".pdf"), width = 210, height = 290, units="mm", dpi=600)
}

# show a graph for 10 sites in Europe 
dd.eu<-dd%>%filter(site_code%in% site.europe$site_code)%>%
   filter(site_code %in% c("frue.ch",   "badlau.de", "kilp.fi",      "cereep.fr",    "burren.ie", "ahth.is",       "veluwe.nl",    "sval.no" ,    "comp.pt" ,     "hero.uk" ))
unique(dd.per.month$site_code_1)  
dd.eu$site_code_1<-factor(dd.eu$site_code_1, levels=c("frue.ch (8.54, 47.11)" ,     "badlau.de (11.88, 51.39)" ,   "kilp.fi (20.87, 69.06)" ,     "cereep.fr (2.66, 48.28)" ,
                                                                    "burren.ie (-8.99, 53.07)",  "ahth.is (-19.67, 65.13)",   "veluwe.nl (5.81, 52.03)" ,
                                                                    "sval.no (16.45, 78.69)" ,  "comp.pt (-8.79, 38.83)" ,     "hero.uk (-0.64, 51.41)" ))
length(unique(dd.eu$country))
unique(dd.eu$site_code_1)

(pp.spei<-dd.eu%>% 
    ggplot(aes(years, spei_12, color=events))+geom_point(size=1.5, alpha=0.5)+theme_cowplot(font_size = 9)+panel_border()+
    facet_wrap(~site_code_1, scale="free_x", ncol=5)+
    scale_x_continuous(expand=c(0,0), limits = c(2000, 2023), breaks = seq(2000,2023,6))+
    geom_hline(yintercept = 0, linetype="dotted")+
    scale_colour_manual(values = colour.crosswalk1) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE))+theme(legend.position = "none")+
    labs(x=NULL, y="SPEI per year", color="Climate patterns"))
ggsave(pp.spei, file="H:/potential fundings/graphs/SPEI_12 in 10 european sites.png", width = 210, height = 110, units="mm", dpi=600)

#################################################################################################
############################## climate extremes within seasons ##################################
#################################################################################################

dd.per.month0 <- prec %>% merge(y = pet, by=c("site_code", "years", "months", "date")) %>%
  merge(y = month.df, by=c("months"))  %>%
  mutate(pet1 = pet * days) %>%
  arrange(site_code, years, month1)%>%group_by(site_code, years, month1, months)%>%
  summarise(sum.prec=sum(prec), sum.pet=sum(pet1))%>%mutate(ratio_prec_pet=sum.prec/sum.pet, water.balance=sum.prec-sum.pet)%>%
  group_by(site_code, month1) %>%mutate(spei.per.month = scale(water.balance))%>%
  mutate(spei.per.month=as.numeric(spei.per.month), events=case_when(spei.per.month>=1.28 ~ "Wet", spei.per.month<= -1.28 ~ "Dry", TRUE~"Normal"))%>%
  filter(month1 >3 & month1 <11)%>%merge(site.inf%>%select(continent, country, site_code, latitude, longitude)%>%distinct(), by=c("site_code"))%>%arrange(country)%>%
  mutate(longitude1=round(longitude, 2), latitude1=round(latitude, 2))%>%mutate(site_code_1=paste0(site_code, " (", longitude1,", ", latitude1, ")"))
unique(dd.per.month0$months)
dd.per.month0$months<-factor(dd.per.month0$months, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))

#  show sites in three pdfs 
dd.per.month00<-dd.per.month0%>%merge(group.sites%>%select(site_code, grp), by=c("site_code"))%>%arrange(grp)%>%filter(years%in%c(2000, 2020))

for (i in 1:3)  {
  # i<-1
  dd.per.month.temp<-dd.per.month00%>% filter(grp == i)%>% mutate(month2=as.character(month1))%>%mutate(grp=paste0(site_code, years))
  
  (pp.spei.per.month<-dd.per.month.temp%>%
     ggplot(aes(months, spei.per.month, color=events, shape=factor(years), group=grp))+geom_point(size=1.5, alpha=0.5)+theme_cowplot(font_size = 9)+panel_border()+
     facet_wrap(~site_code, ncol=5)+
     #geom_hline(yintercept = 0, linetype="dashed")+
     geom_line(color="light grey", linewidth=0.1)+
     scale_shape_manual(values = c(15, 17)) +
     scale_colour_manual(values = colour.crosswalk1) + theme(legend.position = "top")+
     guides(shape = guide_legend(nrow = 1, byrow = TRUE))+
     labs(x=NULL, y="SPEI per month", color="Climate events", shape="Years"))
  
   ggsave(pp.spei.per.month, file=paste0("SPEI within months for NutNet sites_", i , ".pdf"), width = 210, height = 290, units="mm", dpi=600)
}

dd.per.month<-dd.per.month0%>% filter(site_code%in% site.europe$site_code)%>%
  filter(years%in%c(2000, 2020))%>%arrange(country, site_code, years, month1)%>%
  filter(site_code %in% c("frue.ch",   "badlau.de", "kilp.fi",      "cereep.fr",    "burren.ie", "ahth.is",       "veluwe.nl",    "sval.no" ,    "comp.pt" ,     "hero.uk" ))

unique(dd.per.month$site_code_1)  
dd.per.month$site_code_1<-factor(dd.per.month$site_code_1, levels=c("frue.ch (8.54, 47.11)" ,     "badlau.de (11.88, 51.39)" ,   "kilp.fi (20.87, 69.06)" ,     "cereep.fr (2.66, 48.28)" ,
                                                       "burren.ie (-8.99, 53.07)",  "ahth.is (-19.67, 65.13)",   "veluwe.nl (5.81, 52.03)" ,
                                                       "sval.no (16.45, 78.69)" ,  "comp.pt (-8.79, 38.83)" ,     "hero.uk (-0.64, 51.41)" ))
 
(pp.spei.per.month<-dd.per.month%>%    # filter(years==2021)%>%mutate(month2=as.character(month1))%>%
    mutate(grp=paste0(site_code, years))%>%
    ggplot(aes(months, spei.per.month, color=events, shape=factor(years), group=grp))+geom_point(size=1.5, alpha=0.5)+theme_cowplot(font_size = 9)+panel_border()+
    facet_wrap(~site_code_1, nrow=2)+
    #geom_hline(yintercept = 0, linetype="dashed")+
    geom_line(color="light grey", linewidth=0.1)+
    scale_shape_manual(values = c(15, 17)) +
    scale_colour_manual(values = colour.crosswalk1) + theme(legend.position = "top")+
    guides(shape = guide_legend(nrow = 1, byrow = TRUE))+
    labs(x=NULL, y="SPEI per month", color="Climate events", shape="Years"))
 #  ggsave(pp.spei.per.month, file="SPEI per month in 10 european sites.png", width = 210, height = 73, units="mm", dpi=600)
 
(ppp<-plot_grid(pp.spei.per.month, pp.spei, nrow=2))

ggsave(ppp, file="SPEI per year and month in 10 european sites.png", width = 210, height = 120, units="mm", dpi=600)
 
# the end 
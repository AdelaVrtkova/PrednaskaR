#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////         Analýza biomedicínských dat - bílá nebo černá magie?      /////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# Mgr. Adéla Vrtková
# Lékařská fakulta, Ostravská univerzita
# Katedra aplikované matematiky, Fakulta elektrotechniky a informatiky, VŠB - Technická univerzita Ostrava

# Tento materiál byl vytvořen pro zvanou přednášku v rámci projektu CeBMI.

# Zvláštní poděkování míří ke kolegům z Katedry aplikované matematiky, Fakulty elektrotechniky a informatiky, 
# VŠB - Technické univerzity Ostrava - Ing. Martině Litschmannové, Ph.D. a Ing. Michalovi Bérešovi 
# za jejich nápady a rady.

# Příprava prostředí (bude-li nutné, nejprve package nainstalujte)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

#/////////////////////////////////////////////////
# Popis datového souboru #####
# 
# Kachexie označuje stav silné celkové sešlosti jedince spojené s úbytkem váhy a výraznou slabostí. 
# Většinou je následkem vážných nádorových onemocnění nebo těžkých infekcí. 
# V tomto kontextu vědci často zkoumají složení krve v závislosti na onemocnění pacienta a na výskytu kachexie.
# 
# V datovém souboru najdete údaje o onkologických pacientech s rakovinou plic s kachexií a bez projevů kachexie. 
# Zároveň jsou v datech údaje i o třetí skupině – a to kontrolní skupině zdravých pacientů, 
# tj. bez rakoviny i bez kachexie. U všech pacientů byla měřena hladina hormonu rezistinu (ng/ml).
# 
# U onkologických pacientů byla hladina rezistinu změřena při vstupní prohlídce a poté 3 měsíce 
# po chemoterapii. U pacientů z kontrolní skupiny byl rezistin změřen pouze při vstupní prohlídce.
# 
# Skupina onkologických pacientů s kachexií je v datech kódována jako „RK“. 
# Skupina onkologických pacientů bez kachexie je kódována „RBK“ a kontrolní skupina jako „KONT“. 
# U onkologických pacientů je navíc uvedeno stádium rakoviny (na škále 0, I, II, III, IV).
#/////////////////////////////////////////////////

# Import dat, která jsou ve std. datovém formátu (bude-li potřeba - přidejte celou cestu k danému souboru)
data <- read_excel("data_kachexie.xlsx")

# V RStudiu je k dispozici celý náhled na data (kliknutí na data v Environment)
head(data)
tail(data)

#/////////////////////////////////////////////////
# Seznamte se, ggplot2 #####

# Nejprve definujeme "estetiku" (aesthetics - aes):
  # Důležitá část, kde specifikujeme proměnnou na ose x a/nebo na ose y.
  # Lze ale i určit parametr, který ovlivní velikost (size) nebo barvu (color) vykreslených objektů (např. bodů).
  # Dalšími parametry v estetice jsou - fill, linetype, label, shape a další...
 
# Následuje určení "geometrie" (geometries - geom_???):
  # Tato část definuje, jak se mají data znázornit.
  # Např. jako body (geom_point), čáry (geom_line), krabicové grafy (geom_boxplot), sloupcové grafy (geom_bar),...
  # Je třeba uvážit typ dat a na základě toho, jakou chceme informaci předat, zvolit geometrii.
  # Různé "geom" lze i kombinovat, má-li to smysl.
 
# V dalších vrstvách lze nastavovat:
  # rozdělení na tzv. "facets" (mřížka grafů),
  # zakomponování "statistiky" - vrstva "statistics" - např. přidání trendu, vykreslení průměru jako bodu apod.,
  # vrstvu "coordinates" měřítka os, změnit je na logaritmické, apod.,
  # změnit vzhled pomocí definovaných grafických témat - vrstva "theme" - případně si nastavit své vlastní.
#/////////////////////////////////////////////////

# Příprava "tidy" dat ze standardního datového formátu
# Úkol: 
# Vytvořte si úsudek o vývoji hladiny rezistinu od vstupní prohlídky k prohlídce po 3 měsících pro pacienty s rakovinou.
data_tidy = 
  data %>% 
  filter(Skupina %in% c("RK", "RBK"))  %>% 
  pivot_longer(cols = c("Rezistin_vstup", "Rezistin_3M"), 
               names_to = "Prohlidka", 
               values_to = "Rezistin")

head(data_tidy)
tail(data_tidy)

# Překódování nové proměnné Prohlidka dle potřeby
data_tidy$Prohlidka = factor(data_tidy$Prohlidka, 
                             levels = c("Rezistin_vstup", "Rezistin_3M"),
                             labels = c("Vstup","3 měsíce"))


#/////////////////////////////////////////////////
# Síla krabicových grafů #####

# Základní vícenásobný krabicový graf srovnávající hladinu rezistinu při vstupní prohlídce a prohlídce po 3 měsících
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin)) +
  geom_boxplot()

# Prozkoumejte výstup se změnou v "estetice"
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin, 
           color = Skupina)) +
  geom_boxplot()

# Prozkoumejte výstup se změnou v "estetice"
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin, 
           fill = Skupina)) +
  geom_boxplot()

# Prozkoumejte výstup se změnou v "estetice"
ggplot(data_tidy, 
       aes(x = interaction(Prohlidka, Skupina), 
           y = Rezistin)) +
  geom_boxplot()

# Prozkoumejte výstup se zapojením vrsty "facet"
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin)) +
  geom_boxplot()+
  facet_wrap("Skupina")

# Na výše uvedeném grafu budeme stavět dále, přidáme vykreslení průměru
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin)) +
  geom_boxplot()+
  facet_wrap("Skupina")+ 
  stat_summary(geom = "point", 
               fun = "mean", 
               size = 4, 
               shape = 3)

# Nastavení grafického tématu
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin)) +
  geom_boxplot()+
  facet_wrap("Skupina")+ 
  stat_summary(geom = "point", 
               fun = "mean", 
               size = 4, 
               shape = 3)+
  theme_bw()+
  theme(axis.text = element_text(color = "black", 
                                 size = 14),
        axis.title = element_text(color = "black", 
                                  size = 14))

# Změna desetinných teček za desetinné čárky ve výstupu
options(OutDec= ",") 

# Úprava názvů os
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin)) +
  geom_boxplot()+
  facet_wrap("Skupina")+ 
  stat_summary(geom = "point", 
               fun = "mean", 
               size = 4, 
               shape = 3)+
  theme_bw()+
  theme(axis.text = element_text(color = "black", 
                                 size = 14),
        axis.title = element_text(color = "black", 
                                  size = 14))+
  labs(x = " ", y = "Hladina rezistinu (ng/ml)")


# Párový vícenásobný krabicový graf - ideální způsob vykreslení tzv. párových dat
ggplot(data_tidy, 
       aes(x = Prohlidka, 
           y = Rezistin)) +
  geom_boxplot()+
  geom_line(aes(group = ID), 
            size = 0.1)+
  geom_point(size = 0.8, 
             color = "grey")+
  facet_wrap("Skupina")+ 
  stat_summary(geom = "point", 
               fun = "mean", 
               size = 4, 
               shape = 3)+
  theme_bw()+
  theme(axis.text = element_text(color = "black", 
                                 size = 14),
        axis.title = element_text(color = "black", 
                                  size = 14))+
  labs(x = " ", y = "Hladina rezistinu (ng/ml)")

#/////////////////////////////////////////////////
# Hrátky se sloupcovým grafem #####

# Úkol: 
# Využijte sloupcový graf a posuďte strukturu pacientů s rakovinou z hlediska stádia rakoviny, 
# vyzkoušejte aplikovat některé z „triků“.

# Příprava datového souboru - vyfiltrování záznámů pacientů s rakovinou
data_bar = data  %>% 
  filter(Skupina %in% c("RK", "RBK"))

# Příprava základního sloupcového grafu
ggplot(data_bar, 
       aes(x = Stadium))+
  geom_bar()

# Kouzlo - useknutí osy y
ggplot(data_bar, 
       aes(x = Stadium))+
  geom_bar()+
  coord_cartesian(ylim = c(12,25))

# Kouzlo - logaritmické měřítko
ggplot(data_bar, 
       aes(x = Stadium))+
  geom_bar()+
  scale_y_log10()

# A teď vážně... Jak by takový graf mohl vypadat pro publikaci?
# Sloupcový graf je lépe "ovladatelný", pokud jej připravíme z tabulky četností.

tab = 
  data_bar %>%
        group_by(Stadium) %>%
        summarise(Abs_cet = n()) %>%
        mutate(Rel_cet = round(100*(Abs_cet/sum(Abs_cet)),1))
tab

# Ošetření zaokrouhlovací chyby
tab$Rel_cet[5] = 100 - sum(tab$Rel_cet[1:4])
tab

# Konstrukce finálního sloupcového grafu
ggplot(tab, 
       aes(x = Stadium, 
           y = Abs_cet))+
  geom_bar(stat = "identity", 
           fill = "gray60")+
  geom_text(aes(label = paste0(Abs_cet, " (", Rel_cet, " %)")), 
            vjust = -0.5,
            size = 4)+
  scale_y_continuous(limits = c(0,30),
                     breaks = seq(0,30,5))+
  labs(x = "Stádium rakoviny", y = "Počet pacientů")+
  theme_bw()+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"))



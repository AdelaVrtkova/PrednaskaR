#///////////////////////////////////////////////////////////////////////////////////////////
#/////////                         Analýza dat v R                             /////////////
#/////////      O závislostech, o vizualizaci (a možná i o jednom hadovi)      /////////////
#///////////////////////////////////////////////////////////////////////////////////////////

# Nezobrazuje-li se skript správně -> File - Reopen with Encoding -> UTF-8

# Mgr. Adéla Vrtková (adela.vrtkova@vsb.cz)
# Lékařská fakulta, Ostravská univerzita
# Katedra aplikované matematiky, Fakulta elektrotechniky a informatiky, VŠB - Technická univerzita Ostrava

# Tento materiál byl vytvořen pro zvanou přednášku v rámci projektu CeBMI.

#///////////////////////////////////////////////////////////////////////////////////////////
### 0. Příprava prostředí ####

# Bude-li nutné, nejprve package nainstalujte pomocí install.packages(...)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

# Import dat ze souboru data.RData (bude-li potřeba - přidejte celou cestu k danému souboru)
load("data.RData")

# Nastavení hezčího grafického tématu pro všechny vytvářené grafy
theme_set(theme_bw())

#///////////////////////////////////////////////////////////////////////////////////////////
### 1. Analyzujte závislost množství tělesného tuku a obvodu pasu ####

# Základní jednoduchý bodový graf, který jste viděli v prezentaci
ggplot(data,                    # název datové sady
       aes(x = Mnozstvi_tuku,   # proměnná na ose x
           y = Obvod_pasu))+    # proměnná na ose y
  geom_point()+                 # geometrie - jak data vykreslit - jako body
  labs(x = "Množství tuku (%)", # vrstva s popisy os
       y = "Obvod pasu (cm)")

# Jste takto spokojeni? Napadá Vás vylepšení/rozšíření?
# Očkáváte třeba vliv pohlaví na danou závislost? 
# Odpověď si rozmyslete a poté se podívejte na další graf.

ggplot(data,                    # název datové sady
       aes(x = Mnozstvi_tuku,   # proměnná na ose x
           y = Obvod_pasu,      # proměnná na ose y
           color = Pohlavi))+   # proměnná ovlivňující barvu
  geom_point()+                 # geometrie - jak data vykreslit - jako body
  labs(x = "Množství tuku (%)", # vrstva s popisy os
       y = "Obvod pasu (cm)")

# Co jste zjistili? Analyzovali byste dále závislost s ohledem na pohlaví nebo bez ohledu na pohlaví?
# Jaký korelační koeficient byste použili v tomto případě?

# Vzhledem ke zjevnému vlivu pohlaví bychom měli spíše přistoupit k analýze závislosti právě s ohledem na pohlaví.
# Základní dva korelační koeficienty jsou Pearsonův (hodnotí LINEÁRNÍ závislost) a Spearmanův (hodnotí jakoukoliv MONOTÓNNÍ závislost).
# V tomto případě se patrně může jednat o lineární závislost v jednotlivých podskupinách.
# Graf lze tedy doplnit o Pearsonův korelační koeficient.

ggplot(data,                    # název datové sady
       aes(x = Mnozstvi_tuku,   # proměnná na ose x
           y = Obvod_pasu,      # proměnná na ose y
           color = Pohlavi))+   # proměnná ovlivňující barvu
  geom_point()+                 # geometrie - jak data vykreslit - jako body
  labs(x = "Množství tuku (%)", # vrstva s popisy os
       y = "Obvod pasu (cm)")+
  stat_cor(method = "pearson",  # doplnění Pearsonova korel. koeficientu
           r.digits = 3,        # zaokrouhlení korel. koeficientu
           p.accuracy = 0.001)  # zaokrouhlení p-hodnoty

# Graf bychom dále mohli upravit pomocí dalších vrstev do podoby pro publikaci.
# Prozkoumejte kód a pochopte funkce jednotlivých vrstev.

ggplot(data,                    # název datové sady
       aes(x = Mnozstvi_tuku,   # proměnná na ose x
           y = Obvod_pasu,      # proměnná na ose y
           color = Pohlavi))+   # proměnná ovlivňující barvu
  geom_point()+                 # geometrie - jak data vykreslit - jako body
  geom_smooth(method = "lm")+   # geometrie - přidání lineárního trendu
  labs(x = "Množství tuku (%)", # vrstva s popisy os
       y = "Obvod pasu (cm)")+
  stat_cor(method = "pearson",    # doplnění Pearsonova korel. koeficientu
           r.digits = 3,          # zaokrouhlení korel. koeficientu
           p.accuracy = 0.001)+   # zaokrouhlení p-hodnoty
  scale_color_manual(breaks = c("muž", "žena"),              # varianty proměnné tak, jak jsou uvedeny v datech
                     values = c("dodgerblue", "indianred"))+ # přiřazení barev
  theme(legend.position = "none")    # odstranění legendy (lze také: bottom, left, right, top)

# Úkol A ##############################
# Využijte předchozího kódu a analyzujte závislost BMI a množství tělesného tuku.
# Návrh řešení naleznete na konci skriptu.



#---

#///////////////////////////////////////////////////////////////////////////////////////////
### 2. Analyzujte závislost množství tělesného tuku a kvality spánku pacienta ####

# Jednoduchý vícenásobný krabicový graf
ggplot(data,                     # název datové sady
       aes(x = Kvalita_spanku,   # proměnná na ose x
           y = Mnozstvi_tuku))+  # proměnná na ose y
  geom_boxplot()+                # geometrie - jak data vykreslit - jako krabicové grafy
  labs(x = "Kvalita spánku",     # vrstva s popisy os
       y = "Množství tuku (%)")

# Úkol B ##############################
# Využijte předchozího kódu a analyzujte závislost obvodu pasu a kvality spánku pacienta.
# Výstupy zkuste vytvořit s ohledem na pohlaví pacienta (tzn. zakomponujte faktor pohlaví do vizualizace).
# Návrh řešení naleznete na konci skriptu.



#---

# Na tomto místě si uděláme drobné okénko do knihovny dplyr, pomocí které dokážeme velice jednoduše
# vytvořit tabulku s číselnými charakteristikami pro kvantitativní proměnnou a jednoduše
# zakomponovat i třídění dle kategoriálních proměnných

data %>%
  group_by(Kvalita_spanku) %>%     # třídění dle kvality spánku, při odstranění vypočte charakteristiky pro celou numerickou proměnnou
                                   # lze přidat další kategoriální proměnnou (např. pohlaví)
  summarise(Min = min(Mnozstvi_tuku, na.rm = T),
            Q1 = quantile(Mnozstvi_tuku, 0.25, na.rm = T),
            Med = quantile(Mnozstvi_tuku, 0.5, na.rm = T),
            Prumer = mean(Mnozstvi_tuku, na.rm = T),
            Q3 = quantile(Mnozstvi_tuku, 0.75, na.rm = T),
            Max = max(Mnozstvi_tuku, na.rm = T),
            Sm_odch = sd(Mnozstvi_tuku, na.rm = T))

#///////////////////////////////////////////////////////////////////////////////////////////
### 3. Analyzujte závislost kvality spánku pacienta a pohlaví pacienta  ####

# Základem analyzované závislosti je kontingenční tabulka
table(data$Pohlavi, data$Kvalita_spanku)

# Abychom mohli vytvořit 100% skládaný sloupcový graf, 
# potřebujeme si kont. tabulku připravit do požadované struktury, doplnit
# vhodné relativní četnosti, provést zaokrouhlení atd.

# Na tomto místě se nám vyplatí využít knihovnu dplyr. Prozkoumejte kód a výstup.
# Příprava kont. tabulky s tzv. řádkovými rel. četnostmi
tab_rad = 
  data %>% 
  group_by(Pohlavi, Kvalita_spanku) %>% 
  summarise(abs_cet = n()) %>% 
  mutate(rel_cet = round(100*abs_cet/sum(abs_cet),1))  # funkce mutate() vytvoří nový sloupec nebo změní stávající
tab_rad

# Příprava kont. tabulky s tzv. sloupcovými rel. četnostmi
tab_sl = 
  data %>% 
  group_by(Kvalita_spanku, Pohlavi) %>% 
  summarise(abs_cet = n()) %>% 
  mutate(rel_cet = round(100*abs_cet/sum(abs_cet),1))  # funkce mutate() vytvoří nový sloupec nebo změní stávající
tab_sl

# POZOR!!! Výše uvedený kód neošetřuje zaokrouhlovací chybu!!! Automatizace by byla na tomto místě složitá, 
# výběr hodnoty, kterou upravíme tak, aby byl odpovídající součet 100 % je značně individuální a na místě 
# by byla proto "ruční" úprava.

# Máme-li "ručně" ošetřenou zaokrouhlovací chybu, můžeme si ještě vytvořit
# sloupec, kde spojíme absolutní a relativní četnosti.
tab_rad =
  tab_rad %>% 
  mutate(popis = paste0(abs_cet," (", rel_cet, " %)")) # funkce mutate() vytvoří nový sloupec nebo změní stávající
tab_rad

tab_sl =
  tab_sl %>% 
  mutate(popis = paste0(abs_cet," (", rel_cet, " %)")) # funkce mutate() vytvoří nový sloupec nebo změní stávající
tab_sl

# Základní nastavení pro vykreslení 100% skládaného sloupcového grafu.
# VŠIMNĚTE SI NASTAVENÍ ESTETIKY aes(...) dle volby řádkových/sloupcových rel. četností!!!
tab_rad # tabulka s řádkovými rel. četnostmi
ggplot(tab_rad,                              # název tabulky
       aes(x = Pohlavi,                      # proměnná na ose x
           y = rel_cet,                      # proměnná na ose y
           fill = Kvalita_spanku))+          # proměnná dle které se "dělí" sloupce
  geom_bar(stat = "identity",                # vycházíme z tabulky četností, proto stat = "identity"
           position = position_stack())      # skládaný sloupcový graf (angl. stacked), proto position_stack

tab_sl # tabulka s řádkovými rel. četnostmi
ggplot(tab_sl,                               # název tabulky
       aes(x = Kvalita_spanku,               # proměnná na ose x
           y = rel_cet,                      # proměnná na ose y
           fill = Pohlavi))+                 # proměnná dle které se "dělí" sloupce
  geom_bar(stat = "identity",                # vycházíme z tabulky četností, proto stat = "identity"
           position = position_stack())      # skládaný sloupcový graf (angl. stacked), proto position_stack

# Grafy je ještě nutné dále upravit do podoby pro publikaci.
ggplot(tab_rad,                              # název tabulky
       aes(x = Pohlavi,                      # proměnná na ose x
           y = rel_cet,                      # proměnná na ose y
           fill = Kvalita_spanku))+          # proměnná dle které se "dělí" sloupce
  geom_bar(stat = "identity",                # vycházíme z tabulky četností, proto stat = "identity"
           position = position_stack(),      # skládaný sloupcový graf (angl. stacked), proto position_stack
           alpha = 0.6)+                     # průhlednost výplně sloupců
  geom_text(aes(label = popis),              # přidání popisků do sloupců
            position = position_stack(vjust = 0.5),  # skládaný sloupcový graf (angl. stacked), proto position_stack
            size = 4)+                               # velikost popisků
  labs(x = " ", 
       y = "Kumulativní relativní četnost (%)",
       fill = "Kvalita spánku")+
  scale_fill_manual(breaks = c("dobrá", "špatná"),             # varianty proměnné tak, jak jsou uvedeny v datech
                    values = c("forestgreen", "firebrick1"))+  # přiřazení barev
  theme(legend.position = "top")                               # umístění legendy
  
# Úkol C #####
# Připravte 100% skládaný sloupcový graf prezentující strukturu pohlaví pacientů dle kvality spánku.
# Vyjděte z tabulky tab_sl a pohrajte si i s grafikou. Návrh grafu naleznete na konci skriptu.
tab_sl # tabulka se sloupcovými rel. četnostmi





#---

#///////////////////////////////////////////////////////////////////////////////////////////
###* Řešení ####

#* Úkol A #####
# Využijte předchozího kódu a analyzujte závislosti BMI a množství tělesného tuku.
ggplot(data,                      # název datové sady
       aes(x = BMI,               # proměnná na ose x
           y = Mnozstvi_tuku,     # proměnná na ose y
           color = Pohlavi))+     # proměnná ovlivňující barvu
  geom_point()+                   # geometrie - jak data vykreslit - jako body
  labs(x = bquote(BMI~(kg/m^2)),  # vrstva s popisy os - pomocí bquote(...) lze vkládat (nejen) horní/dolní indexy
       y = "Množství tělesného tuku (%)")+
  stat_cor(method = "spearman",    # doplnění Spearmanova korel. koeficientu
           r.digits = 3,           # zaokrouhlení korel. koeficientu
           p.accuracy = 0.001)+    # zaokrouhlení p-hodnoty
  scale_color_manual(breaks = c("muž", "žena"),              # varianty proměnné tak, jak jsou uvedeny v datech
                     values = c("dodgerblue", "indianred"))+ # přiřazení barev
  theme(legend.position = "none")    # odstranění legendy (lze také: bottom, left, right, top)

# POZOR! Zde se patrně nejedná o lineární závislost! 
# Použití Pearsonova korelačního koeficientu by mohlo být zavádějící!
# Nezapomínejme, že máme také Spearmanův korelační koeficient!
# Proložení křivkou trendu (kvadratického, logaritmického, jiného) by vyžadovalo další analýzu a zkoumání toho,
# jaký trend je vlastně vhodný. Pak bychom museli zabřednout do teorie regresních modelů...

# Legendu záměrně vynecháváme, jelikož tuto informaci lze zahrnout do titulku grafu, který stejně v práci musí být.
# Např.: Obr. 1: Závislost BMI a množství tělesného tuku dle pohlaví: modrá - muži, červená - ženy (v grafu jsou uvedeny 
# Spearmanovy korelační koeficienty s p-hodnotami testu významnosti) 
#---

#* Úkol B #####
# Využijte předchozího kódu a analyzujte závislost obvodu pasu a kvality spánku pacienta.
# Výstupy zkuste vytvořit s ohledem na pohlaví pacienta (tzn. zakomponujte faktor pohlaví do vizualizace).

# Zohlednit pohlaví můžeme několika způsoby - např. nastavením parametru přímo v estetice
ggplot(data,                     # název datové sady
       aes(x = Kvalita_spanku,   # proměnná na ose x
           y = Obvod_pasu,       # proměnná na ose y
           fill = Pohlavi))+     # proměnná ovlivňující barvu výplně
  geom_boxplot()+                # geometrie - jak data vykreslit - jako krabicové grafy
  labs(x = "Kvalita spánku",     # vrstva s popisy os
       y = "Obvod pasu (cm)",
       fill = "")+
  scale_fill_manual(breaks = c("muž", "žena"),              # varianty proměnné tak, jak jsou uvedeny v datech
                    values = c("dodgerblue", "indianred"))  # přiřazení barev

ggplot(data,                            # název datové sady
       aes(x = Pohlavi,                 # proměnná na ose x
           y = Obvod_pasu,              # proměnná na ose y
           fill = Kvalita_spanku))+     # proměnná ovlivňující barvu výplně
  geom_boxplot(alpha = 0.6)+            # geometrie - jak data vykreslit - jako krabicové grafy (parametr alpha nastaví průhlednost výplně)
  labs(x = " ",                         # vrstva s popisy os
       y = "Obvod pasu (cm)",           
       fill = "Kvalita spánku")+
  scale_fill_manual(breaks = c("dobrá", "špatná"),              # varianty proměnné tak, jak jsou uvedeny v datech
                    values = c("forestgreen", "firebrick1"))    # přiřazení barev

# Nebo pomocí tzv. facetů
ggplot(data,                     # název datové sady
       aes(x = Kvalita_spanku,   # proměnná na ose x
           y = Obvod_pasu))+     # proměnná na ose y
  geom_boxplot()+                # geometrie - jak data vykreslit - jako krabicové grafy
  labs(x = "Kvalita spánku",     # vrstva s popisy os
       y = "Obvod pasu (cm)")+
  facet_wrap("Pohlavi")          # vrstva facets - vytvoření matice grafů dle variant požadované proměnné

# Je zcela na analytikovi, který graf se mu líbí více a který je dle něj nejvhodnější pro prezentaci
# analyzované závislosti.
#---

#* Úkol C #####
# Připravte 100% skládaný sloupcový graf prezentující strukturu pohlaví pacientů dle kvality spánku.
# Vyjděte z tabulky tab_sl a pohrajte si i s grafikou. Návrh grafu naleznete na konci skriptu.
tab_sl # tabulka se sloupcovými rel. četnostmi

ggplot(tab_sl, 
       aes(x = Kvalita_spanku, 
           y = rel_cet, 
           fill = Pohlavi))+
  geom_bar(stat = "identity",                # Vycházíme z tabulky četností, proto stat = "identity"
           position = position_stack(),      # Skládaný sloupcový graf (angl. stacked), proto position_stack
           alpha = 0.6)+                     # Průhlednost výplně sloupců
  geom_text(aes(label = popis),              # Přidání popisků do sloupců
            position = position_stack(vjust = 0.5),  # Skládaný sloupcový graf (angl. stacked), proto position_stack
            size = 4)+
  labs(title = " ",
       x = "Kvalita spánku", 
       y = "Kumulativní relativní četnost (%)",
       fill = " ")+
  theme(legend.position = "top")+ # umístění legendy
  scale_fill_manual(breaks = c("muž", "žena"),
                    values = c("dodgerblue2", "indianred"))


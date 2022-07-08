---
title: "rÃ©cup eau"
author: "V.Alexandre"
date: "07/07/2022"
output: html_document
---

```{r}
library(tidyverse)
library(sf)
library(esri2sf)
library(rvest)
library(RSelenium)
library(netstat)
library(tesseract)

tesseract_download("fra")
fra <- tesseract("fra")
```

# On liste les url des stations

```{r}
stations78<-c("https://baignades.sante.gouv.fr/baignades/consultSite.do?dptddass=078&plv=no&idCarte=fra&annee=2022&site=078001159&xmin=188874.5200501122&ymin=6217846.556605792&xmax=215627.4799498878&ymax=6236191.443394208",
              "https://baignades.sante.gouv.fr/baignades/consultSite.do?dptddass=078&plv=no&idCarte=fra&annee=2022&site=078001158&xmin=177958.7600250561&ymin=6281503.278302896&xmax=191335.2399749439&ymax=6290675.721697104",
              "https://baignades.sante.gouv.fr/baignades/consultSite.do?dptddass=078&plv=no&idCarte=fra&annee=2022&site=078001161&xmin=212158.2600250561&ymin=6268277.278302896&xmax=225534.7399749439&ymax=6277449.721697104")
```

# on fait le esritosf
```{r}

MS5<-esri2sf("https://sig.social.gouv.fr/arcgis/rest/services/baignades/fra_vue_baignade/MapServer/5")
MS5%>%ggplot()+
  geom_sf()

```
youpi !

On va gÃ©nÃ©rer les url pour aller taper dans les pages

## creation des urls Ã  scraper
```{r}
Baignades22<-MS5%>%filter(annee==2022)%>%
  filter(dptddass%in%c("075","077","078","091","092","093","094","095","060"))%>%
  mutate(url=paste0("https://baignades.sante.gouv.fr/baignades/consultSite.do?isite=",dptddass,isite,"&dptddass=",dptddass,"&annee=2022&site=",dptddass,isite,""))

Baignades18<-MS5%>%filter(annee==2018)%>%
  filter(dptddass%in%c("075","077","078","091","092","093","094","095","060"))%>%
  mutate(url=paste0("https://baignades.sante.gouv.fr/baignades/consultSite.do?isite=",dptddass,isite,"&dptddass=",dptddass,"&annee=2022&site=",dptddass,isite,""))
```

```{r}
yatillesdetailsduprelevement<-function(tableau){str_detect(string = tableau[1,1],pattern = "DÃ©tails des prÃ©lÃ¨vements de l'annÃ©e")}

scrapMoiLaBaignade<-function(url_a_scrap){
  data_scraped<-tibble(url=url_a_scrap)
  pagehtml_scraped<-read_html(data_scraped$url)
  textes<-pagehtml_scraped%>%html_nodes(".titre_depeche")%>%html_text()%>%str_trim(.,side = "both")
  return(list(titre=textes%>%map(1)%>%map(1),
              class=textes%>%map(1)%>%map(2)))
}

```


#Application de la fonction
```{r}
AllBaignades<-Baignades22$url%>%map(scrapMoiLaBaignade)
AllBaignades[[1]]
JusteNoms<-AllBaignades%>%map("titre")%>%map(1)
JusteClass<-AllBaignades%>%map("titre")%>%map(2)

Principal<-tibble(url=Baignades22$url,
       lsite=Baignades22$lsite,
       noms=unlist(JusteNoms),
       classement22=unlist(JusteClass),
       geom=as.character(Baignades22$geoms))

googlesheets4::write_sheet(Principal, "https://docs.google.com/spreadsheets/d/1OlGkk0gLnKDlqF-D5zMxjhN9MTfqpZfeFix9SwQOPE4/edit#gid=0", 
                           sheet="Classement 22")


ToutesAnnees<-MS5%>%mutate(geoms=as.character(geoms))

googlesheets4::write_sheet(ToutesAnnees%>%filter(dptddass%in%c("075","077","078","091","092","093","094","095","060")), "https://docs.google.com/spreadsheets/d/1OlGkk0gLnKDlqF-D5zMxjhN9MTfqpZfeFix9SwQOPE4/edit#gid=0", 
                           sheet="Tout IDFO")
```


On va tenter le Rselenium

on va dans l'invite de cmd 
on cd vers documents/rselenium on tape "java -jar selenium" tabulation pour terminer le nom du fichier puis

```{r}
# Start Selenium Session
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "firefox"
)
remDr$open()


```

On va charger la page
```{r}
remDr$navigate(paste0(Baignades22$url[3],"&plv=all#d"))
Sys.sleep(2) 
positions<-remDr$getWindowPosition()
positions<-c(positions$x,positions$y)

remDr$screenshot(file = 'pageurl2.png')

```

On sort les coordonnÃ©es en x,y 
```{r}

results <- tesseract::ocr_data("pageurl2.png", engine = fra)
results_df<-results%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
  mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))

```


```{r}
sourcepourextract<-tesseract::ocr_data("txt_pour_extract.png", engine = fra)
sourcepourextract_df<-sourcepourextract%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
  mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))

sourcepourextract_df$word<-c(paste0("date",1:5), paste0("sf",1:5), paste0("ect",1:5), paste0("ct",1:5),
                             paste0("hm",1:5), paste0("ph",1:5), paste0("stam",1:5), paste0("cac",1:5),
                             paste0("tsp",1:5), paste0("ost",1:5), "date6","sf6","ect6","ct6","hm6","ph6","stam6",
                             "cac6","tsp6","ost6",paste0("res",1:10))
sourcepourextract_df<-sourcepourextract_df%>%select(word,xmin,ymin,xmax,ymax)%>%
  rename("element"=1,"xmin_ref"=2,"ymin_ref"=3,"xmax_ref"=4,"ymax_ref"=5)
colnames(sourcepourextract_df)
#saveRDS(sourcepourextract_df,"sourcepourextract_df.rda")

sourcepourextract_1_tb<-tesseract::ocr_data("txt_pour_extract_1seule_table.png", engine = fra)
sourcepourextract_1_tb_df<-sourcepourextract_1_tb%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
  mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))

sourcepourextract_1_tb_df$word<-c(paste0("date",1:5), paste0("sf",1:5), paste0("ect",1:5), paste0("ct",1:5),
                             paste0("hm",1:5), paste0("ph",1:5), paste0("stam",1:5), paste0("cac",1:5),
                             paste0("tsp",1:5), paste0("ost",1:5),paste0("res",1:10))
sourcepourextract_1_tb_df<-sourcepourextract_1_tb_df%>%select(word,xmin,ymin,xmax,ymax)%>%
  rename("element"=1,"xmin_ref"=2,"ymin_ref"=3,"xmax_ref"=4,"ymax_ref"=5)
colnames(sourcepourextract_1_tb_df)
#saveRDS(sourcepourextract_1_tb_df,"sourcepourextract_1_tb_df.rda")


sourcepourextract_1_tbV2<-tesseract::ocr_data("txt_pour_extract_1seule_table_2.png", engine = fra)
sourcepourextract_1_tbV2_df<-sourcepourextract_1_tbV2%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
  mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))

sourcepourextract_1_tbV2_df$word<-c(paste0("date",1:5), paste0("sf",1:5), paste0("ect",1:5), paste0("ct",1:5),
                             paste0("hm",1:5), paste0("ph",1:5), paste0("stam",1:5), paste0("cac",1:5),
                             paste0("tsp",1:5), paste0("ost",1:5),paste0("res",1:10))
sourcepourextract_1_tbV2_df<-sourcepourextract_1_tbV2_df%>%select(word,xmin,ymin,xmax,ymax)%>%
  rename("element"=1,"xmin_ref"=2,"ymin_ref"=3,"xmax_ref"=4,"ymax_ref"=5)
colnames(sourcepourextract_1_tbV2_df)
#saveRDS(sourcepourextract_1_tbV2_df,"sourcepourextract_1_tbV2_df.rda")

```

On compare les rÃ©sultats avec ce que Ã§a devrait Ãªtre
```{r}
sourcepourextract_df<-readRDS("sourcepourextract_df.rda")

results_df<-results%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
  mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))

merged_df<-merge(results_df,sourcepourextract_df,all = TRUE)%>%
  mutate(xmoy=round((xmin+xmax)/2),ymoy=round((ymin+ymax)/2),
         xref=round((xmin_ref+xmax_ref)/2),yref=round((ymin_ref+ymax_ref)/2))%>%
  mutate(DistX=abs(xref-xmoy),DistY=abs(yref-ymoy),SommeDist=DistX+DistY)%>%
  group_by(element)%>%
  slice_min(order_by =SommeDist, n=1,with_ties = FALSE )
  

Resultattab<-merged_df%>%filter(SommeDist<=25)%>%select(element,word,confidence)%>%
  mutate(NumPos=readr::parse_number(element),
         ElementSansChiffre=gsub(NumPos,"",element))

Resultattab2<-Resultattab%>%
  pivot_wider(id_cols =  "ElementSansChiffre",values_from="word",names_from = "NumPos")



```


### On crÃ©e la moulinette
```{r}
i<-1
sourcepourextract_df<-readRDS("sourcepourextract_df.rda")
sourcepourextract_df2<-readRDS("sourcepourextract_1_tbV2_df.rda")
listedes2tables<-c("001956")
listedes1table<-Baignades22%>%filter(!(isite %in% listedes2tables))%>%ungroup()%>%st_drop_geometry()%>%
  select(isite)%>%unlist()
#pour 1 ligne

for (i in listedes1table){
  annee<-"2022"
  lsite<-Baignades22$lsite[Baignades22$isite==i]
  isite<-i
  url<-Baignades22$url[Baignades22$isite==i]
  remDr$navigate(paste0(url,"&plv=all#d"))
  Sys.sleep(2) 
  remDr$screenshot(file = paste0(isite,'.png'))
  results <- tesseract::ocr_data(paste0(isite,'.png'), engine = fra)
  if (nrow(results)==5) {
    print(paste0("non pour ",isite))
  }else{
    results_df<-results%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
    mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))
  
  merged_df<-merge(results_df,sourcepourextract_df2,all = TRUE)%>%
    mutate(xmoy=round((xmin+xmax)/2),ymoy=round((ymin+ymax)/2),
           xref=round((xmin_ref+xmax_ref)/2),yref=round((ymin_ref+ymax_ref)/2))%>%
    mutate(DistX=abs(xref-xmoy),DistY=abs(yref-ymoy),SommeDist=DistX+DistY)%>%
    group_by(element)%>%
    slice_min(order_by =SommeDist, n=1,with_ties = FALSE )
  
  Resultattab<-merged_df%>%filter(SommeDist<=25)%>%select(element,word,confidence)%>%
    mutate(NumPos=readr::parse_number(element),
           ElementSansChiffre=gsub(NumPos,"",element))%>%mutate(urlscrappee=url)
  
  saveRDS(Resultattab,paste0(annee,"_",isite,".rda"))
  usethis::ui_done(isite)
  }
}


#pour 2 lignes


for (i in listedes2tables){
  annee<-"2022"
  lsite<-Baignades22$lsite[Baignades22$isite==i]
  isite<-i
  url<-Baignades22$url[Baignades22$isite==i]
  remDr$navigate(paste0(url,"&plv=all#d"))
  Sys.sleep(2) 
  remDr$screenshot(file = paste0(isite,'.png'))
  results <- tesseract::ocr_data(paste0(isite,'.png'), engine = fra)
  if (nrow(results)==5) {
    print(paste0("erreur pour ",isite))
  }else{
    results_df<-results%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
    mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))
  
  merged_df<-merge(results_df,sourcepourextract_df,all = TRUE)%>%
    mutate(xmoy=round((xmin+xmax)/2),ymoy=round((ymin+ymax)/2),
           xref=round((xmin_ref+xmax_ref)/2),yref=round((ymin_ref+ymax_ref)/2))%>%
    mutate(DistX=abs(xref-xmoy),DistY=abs(yref-ymoy),SommeDist=DistX+DistY)%>%
    group_by(element)%>%
    slice_min(order_by =SommeDist, n=1,with_ties = FALSE )
  
  Resultattab<-merged_df%>%filter(SommeDist<=25)%>%select(element,word,confidence)%>%
    mutate(NumPos=readr::parse_number(element),
           ElementSansChiffre=gsub(NumPos,"",element))%>%mutate(urlscrappee=url)
  
  saveRDS(Resultattab,paste0(annee,"_",isite,".rda"))
  usethis::ui_done(isite)
  }
}




```


On joint ensuite les Ã©lÃ©ments d'une annÃ©e dans un seul df

```{r}
Ensemble2022<-list.files(".",pattern = "2022_")%>%map_dfr(.f = function(x){
  return(readRDS(x))
})

Ensemble2022_G<-Ensemble2022%>%
   pivot_wider(id_cols =  c("urlscrappee","ElementSansChiffre"),values_from="word",names_from = NumPos)


```

#On essaie de le faire tourner pour 2018

```{r}


#listedes2tables<-c("001956")
listedes1table<-Baignades18%>%filter(!(isite %in% listedes2tables))%>%ungroup()%>%st_drop_geometry()%>%
  select(isite)%>%unlist()
listedes1table<-Baignades18$isite
#pour 1 ligne

for (i in listedes1table){
  annee<-"2018"
  lsite<-Baignades18$lsite[Baignades18$isite==i]
  isite<-i
  url<-gsub("2022","2018",Baignades18$url[Baignades18$isite==i])
  remDr$navigate(paste0(url,"&plv=all#d"))
  Sys.sleep(2) 
  remDr$screenshot(file = paste0(annee,"_",isite,'.png'))
  results <- tesseract::ocr_data(paste0(annee,"_",isite,'.png'), engine = fra)
  if (nrow(results)==5) {
    print(paste0("non pour ",isite))
  }else{
    results_df<-results%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
    mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))
  
  merged_df<-merge(results_df,sourcepourextract_df2,all = TRUE)%>%
    mutate(xmoy=round((xmin+xmax)/2),ymoy=round((ymin+ymax)/2),
           xref=round((xmin_ref+xmax_ref)/2),yref=round((ymin_ref+ymax_ref)/2))%>%
    mutate(DistX=abs(xref-xmoy),DistY=abs(yref-ymoy),SommeDist=DistX+DistY)%>%
    group_by(element)%>%
    slice_min(order_by =SommeDist, n=1,with_ties = FALSE )
  
  Resultattab<-merged_df%>%filter(SommeDist<=25)%>%select(element,word,confidence)%>%
    mutate(NumPos=readr::parse_number(element),
           ElementSansChiffre=gsub(NumPos,"",element))%>%mutate(urlscrappee=url)
  
  saveRDS(Resultattab,paste0(annee,"_",isite,".rda"))
  usethis::ui_done(isite)
  }
}


#pour 2 lignes


for (i in listedes2tables){
  annee<-"2018"
  lsite<-Baignades18$lsite[Baignades18$isite==i]
  isite<-i
  url<-Baignades18$url[Baignades22$isite==i]
  remDr$navigate(paste0(url,"&plv=all#d"))
  Sys.sleep(2) 
  remDr$screenshot(file = paste0(isite,'.png'))
  results <- tesseract::ocr_data(paste0(isite,'.png'), engine = fra)
  if (nrow(results)==5) {
    print(paste0("erreur pour ",isite))
  }else{
    results_df<-results%>%separate(col=bbox,sep=",",into = c("xmin","ymin","xmax","ymax"))%>%
    mutate(xmin=as.numeric(xmin),ymin=as.numeric(ymin),xmax=as.numeric(xmax),ymax=as.numeric(ymax))
  
  merged_df<-merge(results_df,sourcepourextract_df,all = TRUE)%>%
    mutate(xmoy=round((xmin+xmax)/2),ymoy=round((ymin+ymax)/2),
           xref=round((xmin_ref+xmax_ref)/2),yref=round((ymin_ref+ymax_ref)/2))%>%
    mutate(DistX=abs(xref-xmoy),DistY=abs(yref-ymoy),SommeDist=DistX+DistY)%>%
    group_by(element)%>%
    slice_min(order_by =SommeDist, n=1,with_ties = FALSE )
  
  Resultattab<-merged_df%>%filter(SommeDist<=25)%>%select(element,word,confidence)%>%
    mutate(NumPos=readr::parse_number(element),
           ElementSansChiffre=gsub(NumPos,"",element))%>%mutate(urlscrappee=url)
  
  saveRDS(Resultattab,paste0(annee,"_",isite,".rda"))
  usethis::ui_done(isite)
  }
}

```
# récup données via selenium

```{r}
#Grande Boucle
ToutesAnneesIDFO<-MS5%>%
  filter(dptddass%in%c("075","077","078","091","092","093","094","095","060"))%>%
  mutate(url=paste0("https://baignades.sante.gouv.fr/baignades/consultSite.do?isite=",dptddass,isite,"&dptddass=",dptddass,"&annee=",annee,"&site=",dptddass,isite,"&plv=all#d"))%>%st_drop_geometry()

i<-70
#pb au 66-67
for (i in 1:nrow(ToutesAnneesIDFO)){
  lsite<-ToutesAnneesIDFO$lsite[i]
  isite<-ToutesAnneesIDFO$isite[i]
  url<-ToutesAnneesIDFO$url[i]
  annee<-ToutesAnneesIDFO$annee[i]
  remDr$navigate(url)
  Sys.sleep(2) 
  
  Tables<-remDr$findElement(using = "xpath",value = '/html/body/center/table')
  ALLTABLES<-Tables$getPageSource()[[1]]%>%read_html()%>%html_table()
  Details<-ALLTABLES%>%
    map(.f = function(x){str_detect(string = colnames(x)[1],pattern = "Paramètres obligatoires")})
  tt<-ALLTABLES[unlist(Details)]
  if (length(tt)==1) {
    ResultatsDetailles<-tt[[1]]
  ResultatsDetailles<-ResultatsDetailles[,nchar(colnames(ResultatsDetailles))>3]
    ResultatsDetailles<-ResultatsDetailles[,1:ncol(ResultatsDetailles)]
    ResultatsDetailles_L<-ResultatsDetailles%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("Tableur_",annee,"_",isite,".rda"))
  } else if(length(tt)==2){
    ResultatsDetailles_1<-tt[[1]]
    ResultatsDetailles_1<-ResultatsDetailles_1[,1:ncol(ResultatsDetailles_1)]
    ResultatsDetailles_2<-tt[[2]]
    ResultatsDetailles_2<-ResultatsDetailles_2[,nchar(colnames(ResultatsDetailles_2))>3]
    ResultatsDetailles_2<-ResultatsDetailles_2[,1:ncol(ResultatsDetailles_2)]
    
    ResultatsDetailles_L_1<-ResultatsDetailles_1%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    ResultatsDetailles_L_2<-ResultatsDetailles_2%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    
    ResultatsDetailles_L<-rbind(ResultatsDetailles_L_1,ResultatsDetailles_L_2)%>%
      filter(date!="Valeur limite bon/moyen")%>%
      filter(date!="Valeur limite moyen/mauvais")%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("Tableur_",annee,"_",isite,".rda"))
  }  else if(length(tt)==3){
    ResultatsDetailles_1<-tt[[1]]
    ResultatsDetailles_1<-ResultatsDetailles_1[,1:ncol(ResultatsDetailles_1)]
    ResultatsDetailles_2<-tt[[2]]
    ResultatsDetailles_2<-ResultatsDetailles_2[,1:ncol(ResultatsDetailles_2)]
    ResultatsDetailles_3<-tt[[3]]
    ResultatsDetailles_3<-ResultatsDetailles_3[,nchar(colnames(ResultatsDetailles_3))>3]
    ResultatsDetailles_3<-ResultatsDetailles_3[,1:ncol(ResultatsDetailles_3)]
    
    ResultatsDetailles_L_1<-ResultatsDetailles_1%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    ResultatsDetailles_L_2<-ResultatsDetailles_2%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
     ResultatsDetailles_L_3<-ResultatsDetailles_3%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    
    ResultatsDetailles_L<-rbind(ResultatsDetailles_L_1,ResultatsDetailles_L_2,ResultatsDetailles_L_3)%>%
      filter(date!="Valeur limite bon/moyen")%>%
      filter(date!="Valeur limite moyen/mauvais")%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("Tableur_",annee,"_",isite,".rda"))
  } else if(length(tt)==3){
    ResultatsDetailles_1<-tt[[1]]
    ResultatsDetailles_1<-ResultatsDetailles_1[,1:ncol(ResultatsDetailles_1)]
    ResultatsDetailles_2<-tt[[2]]
    ResultatsDetailles_2<-ResultatsDetailles_2[,1:ncol(ResultatsDetailles_2)]
    ResultatsDetailles_3<-tt[[3]]
    ResultatsDetailles_3<-ResultatsDetailles_3[,1:ncol(ResultatsDetailles_3)]
    ResultatsDetailles_4<-tt[[4]]
    ResultatsDetailles_4<-ResultatsDetailles_4[,nchar(colnames(ResultatsDetailles_4))>3]
    ResultatsDetailles_4<-ResultatsDetailles_4[,1:ncol(ResultatsDetailles_4)]
    
    ResultatsDetailles_L_1<-ResultatsDetailles_1%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    ResultatsDetailles_L_2<-ResultatsDetailles_2%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
     ResultatsDetailles_L_3<-ResultatsDetailles_3%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
     ResultatsDetailles_L_4<-ResultatsDetailles_4%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    
    ResultatsDetailles_L<-rbind(ResultatsDetailles_L_1,ResultatsDetailles_L_2,ResultatsDetailles_L_3,ResultatsDetailles_L_4)%>%
      filter(date!="Valeur limite bon/moyen")%>%
      filter(date!="Valeur limite moyen/mauvais")%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("Tableur_",annee,"_",isite,".rda"))
  }  else{
    ResultatsDetailles_L<-tibble(parametres_obligatoires="",date="",valeur="",fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("Tableur_",annee,"_",isite,".rda"))
  }
  usethis::ui_done(paste0(i,"_",ToutesAnneesIDFO$lsite[i]," :  ",ToutesAnneesIDFO$annee[i],"       -      "))
  
  ##################
  #On veut avoir les images liées à l'historique
  # en bas
  TestTD<-Tables$getPageSource()[[1]]%>%read_html()%>%html_nodes(".titre_depeche")%>%html_text()
  TestTD<-str_trim(TestTD[2],side="both")
  
  #On sort ensuite les classements globaux et leurs dates
  
  
  Classements_tmp<-ALLTABLES%>%
    map(.f = function(x){str_detect(string = x[1,1],pattern = "Mauvais|Bon|Moyen")})
  tt2<-ALLTABLES[unlist(Classements_tmp)]
  if (length(tt2)==1) {
    ResultatsClassements<-tt2[[1]]
    ResultatsClassements<-ResultatsClassements[1,]
    ResultatsClassements<-ResultatsClassements[,nchar(ResultatsClassements[1,])>=10]
    ResultatsClassements_G<-as_tibble(t(ResultatsClassements))
    ResultatsClassements_G<-ResultatsClassements_G%>%
      mutate(date=substr(V1,1,10),
             resultat=str_trim(substr(V1,11,nchar(V1)),side="both"))%>%
      select(date,resultat)%>%
      mutate(fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
  
    saveRDS(ResultatsClassements_G,paste0("Classement_",annee,"_",isite,".rda"))
  } else if(length(tt2)==2){
     ResultatsClassements<-tt2[[1]]
    ResultatsClassements<-ResultatsClassements[1,]
    ResultatsClassements<-ResultatsClassements[,nchar(ResultatsClassements[1,])>=10]
    ResultatsClassements_G<-as_tibble(t(ResultatsClassements))
    ResultatsClassements_G<-ResultatsClassements_G%>%
      mutate(date=substr(V1,1,10),
             resultat=str_trim(substr(V1,11,nchar(V1)),side="both"))%>%
      select(date,resultat)%>%
      mutate(fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
  
    ResultatsClassements_2<-tt2[[2]]
    ResultatsClassements_2<-ResultatsClassements_2[1,]
    ResultatsClassements_2<-ResultatsClassements_2[,nchar(ResultatsClassements_2[1,])>=10]
    ResultatsClassements_2_G<-as_tibble(t(ResultatsClassements_2))
    ResultatsClassements_2_G<-ResultatsClassements_2_G%>%
      mutate(date=substr(V1,1,10),
             resultat=str_trim(substr(V1,11,nchar(V1)),side="both"))%>%
      select(date,resultat)%>%
      mutate(fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
  
    
    saveRDS(rbind(ResultatsClassements_G,ResultatsClassements_2_G),paste0("Classement_",annee,"_",isite,".rda"))
  } else{
    ResultatsClassements_G<-tibble(date="",resultat="",fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
    saveRDS(ResultatsClassements_G,paste0("Classement_",annee,"_",isite,".rda"))
  }
  
}
  

```

On a récupéré ce qui nous intéressait, maintenant, on va l'ouvrir et le mettre dans un tableur

# On regroupe

```{r}
getwd()
ListesDesClassements<-list.files(".",pattern = "Classement_")
OuvreLesClassements<-function(x){readRDS(x)}
EnsembleClassementsEaux<-ListesDesClassements%>%
  map_dfr(OuvreLesClassements)

ListesDesTableurs<-list.files(".",pattern = "Tableur_")
OuvreLesTableurs<-function(x){readRDS(x)}
EnsembleTableursEaux<-ListesDesTableurs%>%
  map_dfr(OuvreLesTableurs)

```


```{r}
EnsembleClassementsEaux<-EnsembleClassementsEaux%>%separate(col=fichier,into=c("annee","isite"))
EnsembleTableursEaux<-EnsembleTableursEaux%>%separate(col=fichier,into=c("annee","isite"))

EnsembleClassementsEaux<-EnsembleClassementsEaux%>%left_join(ToutesAnneesIDFO%>%select(isite,lsite,annee,dptddass,lcom,class_nat)%>%
                                                               mutate(annee=as.character(annee)))
EnsembleTableursEaux<-EnsembleTableursEaux%>%left_join(ToutesAnneesIDFO%>%select(isite,lsite,annee,dptddass,lcom,class_nat)%>%
                                                               mutate(annee=as.character(annee)))

#Nettoyage
EnsembleTableursEaux<-EnsembleTableursEaux%>%filter(date!="")%>%filter(date!="Valeur limite bon/moyen")%>%filter(date!="Valeur limite moyen/mauvais")

BilanAnnuelIsite<-ListesDesClassements%>%
  map_dfr(OuvreLesClassements)%>%
  separate(col=fichier,into=c("annee","isite"))%>%
  left_join(ToutesAnneesIDFO%>%select(isite,lsite,annee,dptddass,lcom,class_nat)%>%
                                                               mutate(annee=as.character(annee)))%>%
  select(isite,lsite,annee,dptddass,lcom,classement_annee_global)%>%distinct(.keep_all = TRUE)%>%
  mutate(classement_annee_global=gsub("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"," ",classement_annee_global))

EnsembleClassementsEaux<-EnsembleClassementsEaux%>%select(-classement_annee_global,-class_nat)%>%distinct(.keep_all = TRUE)

PourAjoutGeocode<-cbind(MS5%>%select(dptddass,isite)%>%mutate(code=paste0(dptddass,isite))%>%
  filter(code%in%paste0(ToutesAnneesIDFO$dptddass,ToutesAnneesIDFO$isite))%>%st_drop_geometry(),
  MS5%>%select(dptddass,isite)%>%mutate(code=paste0(dptddass,isite))%>%
  filter(code%in%paste0(ToutesAnneesIDFO$dptddass,ToutesAnneesIDFO$isite))%>%st_coordinates()%>%as_tibble())


write.csv(EnsembleClassementsEaux%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE),"Eaux_IDFO_Classements.csv",row.names = FALSE)
write.csv(EnsembleTableursEaux%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE),"Eaux_IDFO_Tableurs.csv",row.names = FALSE)
write.csv(BilanAnnuelIsite%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE),"Eaux_IDFO_BilansAnnuels.csv",row.names = FALSE)
PourAjoutGeocode%>%select(dptddass,isite)%>%distinct(.keep_all=T)%>%
  group_by(dptddass)%>%
  count()


PourCarte<-EnsembleTableursEaux%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE)%>%
  filter(annee==2022)%>%
  arrange(dptddass,isite,date)%>%
  group_by(dptddass,isite)%>%
  filter(date==max(date))%>%
  mutate(titre=lsite,
         description=paste0("<b>",parametres_obligatoires," : </b>",valeur))
Suite<-EnsembleTableursEaux%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE)%>%
  #filter(annee==2022)%>%
  filter(isite%in%c("000001","001948","002668","001954"))%>%
  select(isite,lsite,X,Y)%>%distinct(.keep_all=T)

PourCarte2<-PourCarte%>%
  group_by(dptddass,isite,titre,X,Y,date)%>%
  summarise(description=paste0("Prélèvement en date du ",date,"<br>",paste0(description,collapse = "<br>")))%>%
  left_join(EnsembleClassementsEaux%>%select(isite,date,resultat))

PourCarte2<-PourCarte2%>%distinct(.keep_all=TRUE)
PourCarte2<-PourCarte2%>%select(-date)
write.csv(PourCarte2,"carte_dwalp.csv",row.names = F)

unique(EnsembleTableursEaux$isite)  PourCarte2$isite
```

###
# Pour mettre en open data


```{r}

ToutesAnnees<-MS5%>%
  mutate(url=paste0("https://baignades.sante.gouv.fr/baignades/consultSite.do?isite=",dptddass,isite,"&dptddass=",dptddass,"&annee=",annee,"&site=",dptddass,isite,"&plv=all#d"))%>%st_drop_geometry()

for (i in 1:nrow(ToutesAnnees)){
  lsite<-ToutesAnnees$lsite[i]
  isite<-ToutesAnnees$isite[i]
  url<-ToutesAnnees$url[i]
  annee<-ToutesAnnees$annee[i]
  remDr$navigate(url)
  Sys.sleep(2) 
  
  Tables<-remDr$findElement(using = "xpath",value = '/html/body/center/table')
  ALLTABLES<-Tables$getPageSource()[[1]]%>%read_html()%>%html_table()
  Details<-ALLTABLES%>%
    map(.f = function(x){str_detect(string = colnames(x)[1],pattern = "Paramètres obligatoires")})
  tt<-ALLTABLES[unlist(Details)]
  if (length(tt)==1) {
    ResultatsDetailles<-tt[[1]]
  ResultatsDetailles<-ResultatsDetailles[,nchar(colnames(ResultatsDetailles))>3]
    ResultatsDetailles<-ResultatsDetailles[,1:ncol(ResultatsDetailles)]
    ResultatsDetailles_L<-ResultatsDetailles%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("FRANCE_Tableur_",annee,"_",isite,".rda"))
  } else if(length(tt)==2){
    ResultatsDetailles_1<-tt[[1]]
    ResultatsDetailles_1<-ResultatsDetailles_1[,1:ncol(ResultatsDetailles_1)]
    ResultatsDetailles_2<-tt[[2]]
    ResultatsDetailles_2<-ResultatsDetailles_2[,nchar(colnames(ResultatsDetailles_2))>3]
    ResultatsDetailles_2<-ResultatsDetailles_2[,1:ncol(ResultatsDetailles_2)]
    
    ResultatsDetailles_L_1<-ResultatsDetailles_1%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    ResultatsDetailles_L_2<-ResultatsDetailles_2%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    
    ResultatsDetailles_L<-rbind(ResultatsDetailles_L_1,ResultatsDetailles_L_2)%>%
      filter(date!="Valeur limite bon/moyen")%>%
      filter(date!="Valeur limite moyen/mauvais")%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("FRANCE_Tableur_",annee,"_",isite,".rda"))
  }  else if(length(tt)==3){
    ResultatsDetailles_1<-tt[[1]]
    ResultatsDetailles_1<-ResultatsDetailles_1[,1:ncol(ResultatsDetailles_1)]
    ResultatsDetailles_2<-tt[[2]]
    ResultatsDetailles_2<-ResultatsDetailles_2[,1:ncol(ResultatsDetailles_2)]
    ResultatsDetailles_3<-tt[[3]]
    ResultatsDetailles_3<-ResultatsDetailles_3[,nchar(colnames(ResultatsDetailles_3))>3]
    ResultatsDetailles_3<-ResultatsDetailles_3[,1:ncol(ResultatsDetailles_3)]
    
    ResultatsDetailles_L_1<-ResultatsDetailles_1%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    ResultatsDetailles_L_2<-ResultatsDetailles_2%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
     ResultatsDetailles_L_3<-ResultatsDetailles_3%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    
    ResultatsDetailles_L<-rbind(ResultatsDetailles_L_1,ResultatsDetailles_L_2,ResultatsDetailles_L_3)%>%
      filter(date!="Valeur limite bon/moyen")%>%
      filter(date!="Valeur limite moyen/mauvais")%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("FRANCE_Tableur_",annee,"_",isite,".rda"))
  } else if(length(tt)==3){
    ResultatsDetailles_1<-tt[[1]]
    ResultatsDetailles_1<-ResultatsDetailles_1[,1:ncol(ResultatsDetailles_1)]
    ResultatsDetailles_2<-tt[[2]]
    ResultatsDetailles_2<-ResultatsDetailles_2[,1:ncol(ResultatsDetailles_2)]
    ResultatsDetailles_3<-tt[[3]]
    ResultatsDetailles_3<-ResultatsDetailles_3[,1:ncol(ResultatsDetailles_3)]
    ResultatsDetailles_4<-tt[[4]]
    ResultatsDetailles_4<-ResultatsDetailles_4[,nchar(colnames(ResultatsDetailles_4))>3]
    ResultatsDetailles_4<-ResultatsDetailles_4[,1:ncol(ResultatsDetailles_4)]
    
    ResultatsDetailles_L_1<-ResultatsDetailles_1%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    ResultatsDetailles_L_2<-ResultatsDetailles_2%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
     ResultatsDetailles_L_3<-ResultatsDetailles_3%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
     ResultatsDetailles_L_4<-ResultatsDetailles_4%>%
      gather(-`Paramètres obligatoires`,key="Date",value = "Valeur")%>%
      filter(Date!=Valeur)%>%
      janitor::clean_names(.)
    
    ResultatsDetailles_L<-rbind(ResultatsDetailles_L_1,ResultatsDetailles_L_2,ResultatsDetailles_L_3,ResultatsDetailles_L_4)%>%
      filter(date!="Valeur limite bon/moyen")%>%
      filter(date!="Valeur limite moyen/mauvais")%>%
      mutate(fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("FRANCE_Tableur_",annee,"_",isite,".rda"))
  }  else{
    ResultatsDetailles_L<-tibble(parametres_obligatoires="",date="",valeur="",fichier=paste0(annee,"_",isite))
    saveRDS(ResultatsDetailles_L,paste0("FRANCE_Tableur_",annee,"_",isite,".rda"))
  }
  usethis::ui_done(paste0(i,"_",ToutesAnnees$lsite[i]," :  ",ToutesAnnees$annee[i],"       -      "))
  
  ##################
  #On veut avoir les images liées à l'historique
  # en bas
  TestTD<-Tables$getPageSource()[[1]]%>%read_html()%>%html_nodes(".titre_depeche")%>%html_text()
  TestTD<-str_trim(TestTD[2],side="both")
  
  #On sort ensuite les classements globaux et leurs dates
  
  
  Classements_tmp<-ALLTABLES%>%
    map(.f = function(x){str_detect(string = x[1,1],pattern = "Mauvais|Bon|Moyen")})
  tt2<-ALLTABLES[unlist(Classements_tmp)]
  if (length(tt2)==1) {
    ResultatsClassements<-tt2[[1]]
    ResultatsClassements<-ResultatsClassements[1,]
    ResultatsClassements<-ResultatsClassements[,nchar(ResultatsClassements[1,])>=10]
    ResultatsClassements_G<-as_tibble(t(ResultatsClassements))
    ResultatsClassements_G<-ResultatsClassements_G%>%
      mutate(date=substr(V1,1,10),
             resultat=str_trim(substr(V1,11,nchar(V1)),side="both"))%>%
      select(date,resultat)%>%
      mutate(fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
  
    saveRDS(ResultatsClassements_G,paste0("France_Classement_",annee,"_",isite,".rda"))
  } else if(length(tt2)==2){
     ResultatsClassements<-tt2[[1]]
    ResultatsClassements<-ResultatsClassements[1,]
    ResultatsClassements<-ResultatsClassements[,nchar(ResultatsClassements[1,])>=10]
    ResultatsClassements_G<-as_tibble(t(ResultatsClassements))
    ResultatsClassements_G<-ResultatsClassements_G%>%
      mutate(date=substr(V1,1,10),
             resultat=str_trim(substr(V1,11,nchar(V1)),side="both"))%>%
      select(date,resultat)%>%
      mutate(fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
  
    ResultatsClassements_2<-tt2[[2]]
    ResultatsClassements_2<-ResultatsClassements_2[1,]
    ResultatsClassements_2<-ResultatsClassements_2[,nchar(ResultatsClassements_2[1,])>=10]
    ResultatsClassements_2_G<-as_tibble(t(ResultatsClassements_2))
    ResultatsClassements_2_G<-ResultatsClassements_2_G%>%
      mutate(date=substr(V1,1,10),
             resultat=str_trim(substr(V1,11,nchar(V1)),side="both"))%>%
      select(date,resultat)%>%
      mutate(fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
  
    
    saveRDS(rbind(ResultatsClassements_G,ResultatsClassements_2_G),paste0("Classement_",annee,"_",isite,".rda"))
  } else{
    ResultatsClassements_G<-tibble(date="",resultat="",fichier=paste0(annee,"_",isite),
             classement_annee_global=TestTD)
    saveRDS(ResultatsClassements_G,paste0("France_Classement_",annee,"_",isite,".rda"))
  }
}
  
ListesDesClassements<-list.files(".",pattern = "France_Classement_")
OuvreLesClassements<-function(x){readRDS(x)}
EnsembleClassementsEauxFrance<-ListesDesClassements%>%
  map_dfr(OuvreLesClassements)

ListesDesTableurs<-list.files(".",pattern = "France_Tableur_")
OuvreLesTableurs<-function(x){readRDS(x)}
EnsembleTableursEauxFrance<-ListesDesTableurs%>%
  map_dfr(OuvreLesTableurs)


EnsembleClassementsEauxFrance<-EnsembleClassementsEauxFrance%>%separate(col=fichier,into=c("annee","isite"))
EnsembleTableursEauxFrance<-EnsembleTableursEauxFrance%>%separate(col=fichier,into=c("annee","isite"))

EnsembleClassementsEauxFrance<-EnsembleClassementsEauxFrance%>%left_join(ToutesAnneesIDFO%>%select(isite,lsite,annee,dptddass,lcom,class_nat)%>%
                                                               mutate(annee=as.character(annee)))
EnsembleTableursEauxFrance<-EnsembleTableursEauxFrance%>%left_join(ToutesAnneesIDFO%>%select(isite,lsite,annee,dptddass,lcom,class_nat)%>%
                                                               mutate(annee=as.character(annee)))

#Nettoyage
EnsembleTableursEauxFrance<-EnsembleTableursEauxFrance%>%filter(date!="")%>%filter(date!="Valeur limite bon/moyen")%>%filter(date!="Valeur limite moyen/mauvais")

BilanAnnuelIsite<-ListesDesClassements%>%
  map_dfr(OuvreLesClassements)%>%
  separate(col=fichier,into=c("annee","isite"))%>%
  left_join(ToutesAnneesIDFO%>%select(isite,lsite,annee,dptddass,lcom,class_nat)%>%
                                                               mutate(annee=as.character(annee)))%>%
  select(isite,lsite,annee,dptddass,lcom,classement_annee_global)%>%distinct(.keep_all = TRUE)%>%
  mutate(classement_annee_global=gsub("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"," ",classement_annee_global))

EnsembleClassementsEauxFrance<-EnsembleClassementsEauxFrance%>%select(-classement_annee_global,-class_nat)%>%distinct(.keep_all = TRUE)

PourAjoutGeocode<-cbind(MS5%>%select(dptddass,isite)%>%mutate(code=paste0(dptddass,isite))%>%
  filter(code%in%paste0(ToutesAnnees$dptddass,ToutesAnnees$isite))%>%st_drop_geometry(),
  MS5%>%select(dptddass,isite)%>%mutate(code=paste0(dptddass,isite))%>%
  filter(code%in%paste0(ToutesAnnees$dptddass,ToutesAnnees$isite))%>%st_coordinates()%>%as_tibble())


write.csv(EnsembleClassementsEauxFrance%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE),"Eaux_Classements.csv",row.names = FALSE)
write.csv(EnsembleTableursEauxFrance%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE),"Eaux_Tableurs.csv",row.names = FALSE)
write.csv(BilanAnnuelIsite%>%left_join(PourAjoutGeocode)%>%distinct(.keep_all = TRUE),"Eaux_BilansAnnuels.csv",row.names = FALSE)
```




```{r}
remDr$close()
```


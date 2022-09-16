library(readr)
library(tidyverse)
library(ggrepel)


person_2020 <- read_csv("Z:/ZW_R_DICT/Animated_plot/traffic_data/FARS2020NationalCSV/person.csv")

View(person_2020)

person_2020%>%
  select(STATENAME,ST_CASE,VEH_NO,VPICMAKENAME,VPICMODELNAME,AGE,SEX,INJ_SEVNAME)%>%
  #group_by(INJ_SEVNAME)%>%count()
  filter(INJ_SEVNAME %in% c('Suspected Serious Injury (A)','Fatal Injury (K)'))%>%
  distinct(ST_CASE,VEH_NO,.keep_all=TRUE)%>%
  select(STATENAME,ST_CASE,VEH_NO,VPICMAKENAME,VPICMODELNAME)%>%
  filter(VEH_NO>0)%>%
  add_count(VPICMAKENAME)%>%
  #only plot brands with total incidences > 500 
  filter(n>500)%>%
  mutate(make_mod=paste0(VPICMAKENAME,'_',VPICMODELNAME))%>%
  add_count(make_mod)%>%
  #only plot models with >100 incidences and >5% of the brand's total incidences
  mutate(Model=if_else(nn>0.05*n & nn>100,VPICMODELNAME,'Other'))%>%
  select(VPICMAKENAME,n,Model)%>%
  group_by(VPICMAKENAME,Model)%>%
  count()%>%
  arrange(VPICMAKENAME,desc(n))%>%
  group_by(VPICMAKENAME)%>%
  mutate(Model_id=as.factor(row_number()))%>%
  ggplot(aes(x=reorder(VPICMAKENAME,n,sum),y=n,fill=Model_id))+
  geom_bar(stat = 'identity',position = 'stack',show.legend = FALSE)+
  ylab('Number of Fatal or Serious Injury Incidences in 2020')+
  xlab('')+
  geom_text(aes(label=if_else(Model %in% c('Dakota','Wrangler','RAV4'),'',
                              if_else(Model=='Electra Glide Ultra Classic',
                                      'Electra Glide-\nUltra Classic',Model))),
                  size = 3, position = position_stack(vjust = 0.5))+
  geom_text_repel(aes(label=if_else(Model %in% c('Dakota','Wrangler','RAV4'),Model,'')),
            size = 3, position = position_stack(vjust = 0.5),
            point.padding = 1,force = 1,direction = 'y')+
  scale_fill_brewer(palette='Set3')+
  coord_flip()+
  theme_bw()


################
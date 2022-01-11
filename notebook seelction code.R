#Lendo bases de dados descritas acima:
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(knitr)
library(gsheet)

#reading data for folume of sales and time estimates
df_notebooks = gsheet2tbl('https://docs.google.com/spreadsheets/d/1aB1nHR6KjOuhOQVfblfaYEyRGSTvCevVG3bL0o7iU84/edit?usp=sharing',sheetid = 1) %>%
  as_tibble()

names(df_notebooks) = df_notebooks[3,] %>% unlist() %>% str_remove_all("(/)|(\\()|(\\))|(\\?)") %>% 
  str_replace_all(" ","_")

df_notebooks2 = df_notebooks %>%
  filter(row_number()>3) %>% 
  select(Notebook,Processor,Processor_Score    
         ,Processor_Cores,RAM_Memory_GB,Memory_SpeedMhz    
         ,Expandible_RAM,GPU_Type,GPU_Card           
         ,GPU_Score,Storage_capacity_GB,Storage_type       
         ,Expandible_Storage,Screen_size_In,Resolution_px      
         ,Price_BRL,Obs) %>%
  mutate(Price_BRL = str_remove_all(Price_BRL,"(R\\$ )|(\\,)") %>% as.numeric()
         ,Processor_Score = as.numeric(Processor_Score)
         ,Memory_SpeedMhz = as.numeric(Memory_SpeedMhz)
         ,RAM_Memory_GB = as.numeric(RAM_Memory_GB)
         ,Memory_SpeedMhz = as.numeric(Memory_SpeedMhz)
         ,GPU_Score = as.numeric(GPU_Score)
         ,Processor_Score = as.numeric(Processor_Score)
         ,Screen_size_In = as.numeric(Screen_size_In)
         ,Storage_capacity_GB = as.numeric(Storage_capacity_GB)) %>% 
  mutate_if(function(x) {is.character(x) && (!(x %in% c("Notebook","Processor")))},as.factor) %>%
  filter(!is.na(Notebook))

#First we need to cast boolean and flags into numerical:

matrix_notebooks = df_notebooks2 %>% 
  select(Processor_Score,RAM_Memory_GB,Memory_SpeedMhz
         ,GPU_Type,GPU_Score,Resolution_px,Processor_Cores,Storage_capacity_GB
         ,Storage_type,Screen_size_In,Price_BRL) %>% 
  #normalizing variables:
  model.matrix(~Processor_Score
             # +Processor_Cores
             +RAM_Memory_GB
             +Memory_SpeedMhz
             +GPU_Type
             +GPU_Score
             +Storage_capacity_GB
             +Storage_type
             +Screen_size_In
             +Price_BRL-1
             ,data=.) %>% unclass() %>% as.matrix() %>% 
  apply(X = .,MARGIN=2,FUN = function(x) (x-mean(x))/sd(x)) %>% 
  .[,-which(colnames(.)=="GPU_TypeDedicada")]

skimr::skim(df_notebooks2 %>% select(Processor_Score
                                     ,Processor_Cores
                                     ,Screen_size_In
                                     ,RAM_Memory_GB
                                     ,Memory_SpeedMhz
                                     ,GPU_Type
                                     ,GPU_Score
                                     ,Storage_capacity_GB
                                     ,Storage_type
                                     ,Resolution_px
                                     ,Price_BRL))

princomp_analysis = princomp(matrix_notebooks)
cumsum(princomp_analysis$sdev/sum(princomp_analysis$sdev))
princomp_analysis$loadings

plott = princomp_analysis$loadings %>% unclass() %>% 
  as_tibble() %>% mutate_all(.funs = function(x) round(x,3)) %>% 
  mutate(variable = row.names(princomp_analysis$loadings %>% unclass()) %>% as.factor()) %>% 
  pivot_longer(names(.)[!str_detect(names(.),"variable")]) %>% 
  mutate(name_num = str_extract(name,"[0-9]{1,2}") %>%  
           as.numeric() %>% sprintf(fmt="%02d")) %>% 
  mutate(name=str_remove_all(name,"[0-9]{1,}$") %>% paste(.,name_num,sep="")) %>% 
  select(-name_num) %>% 
  ggplot(aes(x=1,y=value,fill=variable))+
  geom_col(position = "dodge")+
  facet_grid(. ~ name)

plotly::ggplotly(plott)

kmeans_notebook = kmeans(matrix_notebooks,4)  

#now we use princomp to plot into a 3 dimensional space using the 3 most important components and clusters found with kmeans:
cluster_build = princomp_analysis$scores[,1:3] %>% as_tibble() %>% 
  mutate(clusters = kmeans_notebook$cluster) %>% 
  bind_cols(df_notebooks2 %>% select(Notebook))

plotly::plot_ly(data = cluster_build,
        x = ~Comp.1, y = ~Comp.2, z = ~Comp.3,
        text = ~ Notebook,
        type = 'scatter3d', mode ='markers',
        color = as.factor(kmeans_notebook$cluster))


#we can check what's inside each cluster by variable:
library(plotly)

df_notebooks3 = df_notebooks2 %>% 
  mutate(clusters = kmeans_notebook$cluster %>% as.factor()) %>% 
  select(Processor_Score,RAM_Memory_GB,GPU_Score,Price_BRL,Storage_capacity_GB,clusters) %>% 
  pivot_longer(c("Processor_Score","RAM_Memory_GB","GPU_Score","Price_BRL","Storage_capacity_GB")) %>% 
  group_by(clusters,name) %>% 
  mutate(  min=min(value)
          ,q3 = quantile(value,0.75)
          ,median = median(value)
          ,q1 = quantile(value,0.25)
          ,max=max(value),.groups="drop") %>% 
  ungroup()

plott = df_notebooks3 %>% 
  ggplot(aes(x=clusters,y=value,fill=clusters,groups=clusters))+
  geom_violin(aes(text=paste("max:",max
                             ,"\nq3",q3
                             ,"\nmedian",median
                             ,"\nq1",q1
                             ,"\nmin:",min)))+
  facet_grid(name ~ .,scale="free_y")

ggplotly(plott,tooltip = c("text"))

library('ggplot2')
library('dplyr')
library('RColorBrewer')
library('wordcloud')
library('ggthemes')
library('ggrepel')
library('grid')
library('gridExtra')
library('knitr')

df<-read.csv("/home/jaogjao/Documentos/Faculdade/2017-2/Mineração de Repositórios/dataset/dataset.csv",stringsAsFactors = FALSE)
df <- df[- grep("None", df$stars),]
df$stars<-sapply(df$stars, function(x) as.numeric(as.character(gsub('','',x))))

df1<-read.csv("/home/jaogjao/Documentos/Faculdade/2017-2/Mineração de Repositórios/dataset/dataset2.csv",stringsAsFactors = FALSE)
df1 $Number.of.Stars<-sapply(df1$Number.of.Stars, function(x) as.numeric(as.character(gsub('k','',x))))




df %>% 
  select(language, stars) %>% 
  group_by(language) %>% 
  summarize(tot = sum(stars)) %>% 
  ggplot(aes(x=reorder(language,tot),y=reorder(tot,tot),fill=tot)) + 
  geom_histogram(stat='identity') + coord_flip() + 
  geom_text(aes(label=tot), position=position_identity(), vjust=0)+
  theme_igray() +
  labs(y = "Estrelas", x = "Linguagem")+
  #scale_y_discrete(name="Estrelas", labels = comma)+
  scale_fill_gradientn(name='Stars',colours=rev(brewer.pal(10,'Spectral'))) +
  guides(fill =  FALSE)+
  ggtitle("Most popular Language used in projects per Number of Stars") +
  theme(legend.text=element_text(angle=45,size=10))
  
  df %>% filter(language=="") %>% summarise(count=n())

top100 <-df %>%
  top_n(stars,n=150)
library(scales)
df2 <- df1
top100 %>% 
  select(language, stars) %>% 
  group_by(language) %>% 
  summarize(tot = sum(stars)) %>% 
  ggplot(aes(x=reorder(language,tot),y=reorder(tot,tot),fill=tot)) + 
  geom_histogram(stat='identity') + coord_flip() + 
  labs(y = "Estrelas", x = "Linguagem")+
  geom_text(aes(label=tot), position=position_identity(), vjust=0)+
  theme_igray() +
  scale_y_discrete(name="Estrelas", labels = comma)+
  scale_fill_gradientn(name='Stars',colours=rev(brewer.pal(10,'Spectral'))) +
  guides(fill = FALSE)+
  ggtitle("Most popular Language used in Top 100 projects per Number of Stars") +
  theme(axis.title =element_text())


cont = 0

top100$Tags = "zzzzNone"

for(j in 1:nrow(df1)){
  for (i in 1:nrow(top100)) {
    if((df1$Url[j]) %in% (paste("https://github.com/",top100$repository[i], sep = ""))){
      top100$Tags[i]<-df1$Tags[j]
      print(df1$Url[j])    
      cont = cont+ 1
      }
    if(cont == 100){break}
  }
}
print(cont)




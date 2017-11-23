library('scales')
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

top100 <-df %>%
  top_n(stars,n=580)

cont = 0

top100$Tags = "zzzzNone"

for(j in 1:nrow(df1)){
  for (i in 1:nrow(top100)) {
    if((df1$Url[j]) %in% (paste("https://github.com/",top100$repository[i], sep = ""))){
      top100$Tags[i]<-df1$Tags[j]
      #print(df1$Url[j])    
      cont = cont+ 1
    }
    if(cont == 200){break}
  }
}

top100 <- top100[- grep("zzzzNone", top100$Tags),]

top100 %>% 
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
  
  top100 %>% filter(language=="") %>% summarise(count=n())

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
  ggtitle("Most popular Language used in Top 140 projects per Number of Stars") +
  theme(axis.title =element_text())


top100%>% filter(Tags=="")%>%summarise(count=n())
allTags<-c()
freqTags<-c()
starsTags<-c()
langTags<-c()
cont<- 0
contProj <- 0
for(i in 1:nrow(top100)){
  curTags<-top100$Tags[i]
  if(curTags!=""){
    contProj <- contProj+1
    curLength <- length(strsplit(curTags,',')[[1]])
    freqTags[contProj]<-curLength
    starsTags[contProj]<-top100$stars[i]
    langTags[contProj]<-top100$language[i]
    for(j in 1:curLength){
      allTags[j+cont]<-strsplit(curTags,',')[[1]][j]
    }
    cont<-length(allTags)
  }
}
display.brewer.all()

RES<-data.frame(table(allTags))
set.seed(1234)
op <- par(mfrow = c(1,1), bg="gray50")
wordcloud(RES$allTags,RES$Freq, min.freq = 1, colors = brewer.pal(10,'YlOrRd'),scale = c(5, .5), random.order = FALSE)




df%>% filter(Tags=="")%>%summarise(count=n())
allTags<-c()
freqTags<-c()
starsTags<-c()
langTags<-c()
cont<- 0
contProj <- 0
for(i in 1:nrow(df)){
  curTags<-df$Tags[i]
  if(curTags!=""){
    contProj <- contProj+1
    curLength <- length(strsplit(curTags,',')[[1]])
    freqTags[contProj]<-curLength
    starsTags[contProj]<-df$stars[i]
    langTags[contProj]<-df$language[i]
    for(j in 1:curLength){
      allTags[j+cont]<-strsplit(curTags,',')[[1]][j]
    }
    cont<-length(allTags)
  }
}
display.brewer.all()
RES<-data.frame(table(allTags))
set.seed(1234)
op <- par(mfrow = c(1,1), bg="gray90")
wordcloud(RES$allTags,RES$Freq, min.freq = 1, colors = brewer.pal(10,'Spectral'),scale = c(5, .5), random.order = FALSE)

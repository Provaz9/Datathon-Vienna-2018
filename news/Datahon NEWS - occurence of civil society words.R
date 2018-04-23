###########################################################################
#Script for measure Occurence of civil society words in the news
#(and Occurence of civil society topics)
#BY Provaznik Jan


# Connect to data bases ----
rm(list=ls(all=TRUE))
library(RPostgreSQL)

getwd()


#DATABASE CONECTION_____________________________________
setwd("D:/Provaz-dokumenty/Datathon")
con_pg <- dbConnect(
  drv = dbDriver("PostgreSQL"),
  host = "datathon.data-lab.io",
  port = 5432,
  user = "read_only_user",
  dbname ="postgres",
  password ="datathon2018"
)


dbGetInfo(con_pg)

#PREPARING DATA FROM DATABASE----------------------------------------------------------------------------------------------
query.sgl="SELECT table_schema,table_name FROM information_schema.tables ORDER BY table_schema,table_name;"

dbGetQuery(con_pg,query.sgl)

query.sgl="SELECT table_name FROM information_schema.tables where table_schema='public';"
tables=dbGetQuery(con_pg,query.sgl)
tables

#________________________________________________________________________________________________________________
###CREATING DATASETS
Datanames="a" #Creating the object
for (i in 1:7) { 
query.sgl=paste("SELECT * FROM public.",tables[i,],";",sep="")
o=dbGetQuery(con_pg,query.sgl)
o=o[o$pubdate>="2018-01-01 00:00:00",] #data from 2018
Encoding(o[,"title"])="UTF-8"
Encoding(o[,"description"])="UTF-8"
Encoding(o[,"document"])="UTF-8"
o[,"title"]=tolower(o[,"title"])
o[,"description"]=tolower(o[,"description"])
o[,"document"]=tolower(o[,"document"])
assign( tables[i,],o)
Datanames[i]=tables[i,]
}


remove(o,query.sgl)
topics <- read.csv2("Civil Society Topics v2.csv")#csv with civil society words
topics[,"English"]=tolower(topics[,"English"])
topics[,"German"]=tolower(topics[,"German"])
topics[,"Czech"]=tolower(topics[,"Czech"])


Datanames
DatanamesA=Datanames[c(1,2,4,7)]
DatanamesA=DatanamesA[-1] #Standard REMOVED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#we want data from 2018 and we use for that standard_rss
DatanamesCZ=Datanames[c(3,5,6)]


#________________________________________________________________________________________________________________
### COUNTS OF OCCURENCE IN TITLES AND DESCRIPTION OF NEWS
##count of TITLES with word from list (not count of word occurence)
topics.counts=topics
topics.counts[,"Tit_total"]=0#Creating empty column
topics.counts[,"Tit_totalCZ"]=0#Creating empty column
#CZECH
k=length(topics.counts)#n of columms in table which is for insert
for (j in 1:length(DatanamesCZ)) {
  topics.counts[,paste("Tit_",DatanamesCZ[j],sep="")]=0#Creating empty column
  topics.counts[,paste("TitR_",DatanamesCZ[j],sep="")]=0#Creating empty column
  news=get(DatanamesCZ[j])#calling for Dataset
  n=nrow(news)#n of news published
      for (i in 1:nrow(topics)) { 
      x=length(grep(topics[i,"Czech"],news[,"title"]))#finding news with word from topics and than counting it
      topics.counts[i,k+(j*2-1)]=x#saving number of occurence
      topics.counts[i,k+j*2]=round(x/n*100,10)#relative number of occurence to n of news by given publisher
      topics.counts[i,"Tit_total"]=topics.counts[i,"Tit_total"]+x#number of total occurences
      topics.counts[i,"Tit_totalCZ"]=topics.counts[i,"Tit_totalCZ"]+x#number of total occurences in titles of given country
      }
}

#AUSTRIA
k=length(topics.counts)#n of columms in table which is for insert
topics.counts[,"Tit_totalA"]=0#Creating empty column
for (j in 1:length(DatanamesA)) { 
  topics.counts[,paste("Tit_",DatanamesA[j],sep="")]=0#Creating empty column
  topics.counts[,paste("TitR_",DatanamesA[j],sep="")]=0#Creating empty column  
  news=get(DatanamesA[j])#calling for Dataset
  n=nrow(news)#n of news published
      for (i in 1:nrow(topics)) { 
      x=length(grep(topics[i,"German"],news[,"title"])) #finding new with word from topics and than counting it
      topics.counts[i,k+(j*2-1)]=x#saving number of occurence
      topics.counts[i,k+j*2]=round(x/n*100,10)#relative number of occurence to n of news by given publisher
      topics.counts[i,"Tit_total"]=topics.counts[i,"Tit_total"]+x
      topics.counts[i,"Tit_totalA"]=topics.counts[i,"Tit_totalA"]+x#number of total occurences in articles of given country 
       }
}


##count of DESCRIPTIONS with word from list (not count of word occurence)
topics.counts[,"Desc_total"]=0#Creating empty column
topics.counts[,"Desc_totalCZ"]=0#Creating empty column
#CZECH
k=length(topics.counts)#n of columms in table which is for insert
for (j in 1:length(DatanamesCZ)) {
  topics.counts[,paste("Desc_",DatanamesCZ[j],sep="")]=0#Creating empty column
  topics.counts[,paste("DescR_",DatanamesCZ[j],sep="")]=0#Creating empty column
  news=get(DatanamesCZ[j])#calling for Dataset
  n=nrow(news)#n of news published
  for (i in 1:nrow(topics)) { 
    x=length(grep(topics[i,"Czech"],news[,"description"]))#finding new with word from topics and than counting it
    topics.counts[i,k+(j*2-1)]=x#saving number of occurence
    topics.counts[i,k+j*2]=round(x/n*100,10)#relative number of occurence to n of news by given publisher
    topics.counts[i,"Desc_total"]=topics.counts[i,"Desc_total"]+x
    topics.counts[i,"Desc_totalCZ"]=topics.counts[i,"Desc_totalCZ"]+x#number of total occurences in articles of given country
     }
}

#AUSTRIA
topics.counts[,"Desc_totalA"]=0#Creating empty column
k=length(topics.counts)#n of columms in table which is for insert
for (j in 1:length(DatanamesA)) { 
  topics.counts[,paste("Desc_",DatanamesA[j],sep="")]=0#Creating empty column
  topics.counts[,paste("DescR_",DatanamesA[j],sep="")]=0#Creating empty column  
  news=get(DatanamesA[j])#calling for Dataset
  n=nrow(news)#n of news published
  for (i in 1:nrow(topics)) { 
    x=length(grep(topics[i,"German"],news[,"description"])) #finding new with word from topics and than counting it
    topics.counts[i,k+(j*2-1)]=x#saving number of occurence
    topics.counts[i,k+j*2]=round(x/n*100,10)#relative number of occurence to n of news by given publisher
    topics.counts[i,"Desc_total"]=topics.counts[i,"Desc_total"]+x
    topics.counts[i,"Desc_totalA"]=topics.counts[i,"Desc_totalA"]+x#number of total occurences in articles of given country
    }
}


##Number of articles in given country (for calculation of relative numbers)
nA=0#number of articles of given country
for (j in 1:length(DatanamesA)) { 
  nA=nrow(get(DatanamesA[j]))
  nA=nA+nA
}

nCZ=0
for (j in 1:length(DatanamesCZ)) { 
  nCZ=nrow(get(DatanamesCZ[j]))
  nCZ=nCZ+nCZ
}

topics.countsG=topics.counts
remove(i,j,k,n,x,news)
topics.counts[,"TitR_totalCZ"]=round(topics.counts[,"Tit_totalCZ"]/nCZ*100,10)
topics.counts[,"TitR_totalAT"]=round(topics.counts[,"Tit_totalA"]/nA*100,10)
topics.counts[,"DescR_totalCZ"]=round(topics.counts[,"Desc_totalCZ"]/nCZ*100,10)
topics.counts[,"DescR_totalAT"]=round(topics.counts[,"Desc_totalA"]/nA*100,10)




### SEARCHING FOR TOP 10 MOST USED WORDS FROM GIVEN EXCEL SHEET
orderCol=c("Tit_total","Desc_total","TitR_totalCZ","TitR_totalAT","DescR_totalCZ","DescR_totalAT")
#Some columns arent use in next part at the end

#Creating datasets of Top 10 for graph
OrColName="a"
for (i in 1:length(orderCol)) {
  a=topics.counts[order(topics.counts[,orderCol[i]],decreasing = TRUE),] 
  b=head(a,10)
  assign(paste0("G.", orderCol[i],sep=""),b)
}
remove(a)

#Combination of relative numbers
G.TitR_total=head(topics.counts[order(topics.counts[,"TitR_totalCZ"]+topics.counts[,"TitR_totalAT"],decreasing = TRUE),],10)
G.DescR_total=head(topics.counts[order(topics.counts[,"DescR_totalCZ"]+topics.counts[,"DescR_totalAT"],decreasing = TRUE),],10)

library(ggplot2)
library(reshape2)

##SORTED BY ABSOLUTE TOTAL NUMBERS - TITLE
G.TitR_total2=G.Tit_total
# G.Tit_total=G.Tit_total[,c("English","Tit_totalCZ","Tit_totalA")]
# 
# G.Tit_total<-melt(G.Tit_total,id.vars="English")
# 
# ggplot(data=G.Tit_total,aes(x=English,y=value,fill=factor(variable)))+
#   geom_bar(stat="identity",position="dodge")+
#   theme_bw()+
#   ggtitle("Top 10 civil society words in news titles by country at the beggining of 2018")+
#   ylab("Number of occurences")+
#   xlab("Word")+
#   theme(axis.text.x=element_text(angle=90,hjust=1))
# 
# 
###SORTED BY RELATIVE TOTAL NUMBERS - TITLE - SORT BY RELATIVE
# G.TitR_total=G.TitR_total[,c("English","TitR_totalCZ","TitR_totalA")]
# 
# G.TitR_total<-melt(G.TitR_total,id.vars="English")
# 
# ggplot(data=G.TitR_total,aes(x=English,y=value,fill=factor(variable)))+
#   geom_bar(stat="identity",position="dodge")+
#   theme_bw()+
#   ggtitle("Top 10 most frequently used civil society words in news titles by country at the beggining of 2018")+
#   ylab("Rel. num. of occur. on every published articles in given country (%)")+
#   xlab("Word")+
#   theme(axis.text.x=element_text(angle=90,hjust=1))
# 


##RELATIVE TOTAL NUMBERS - TITLE- SORTED BY TOTAL
G.TitR_total2=G.TitR_total2[,c("English","TitR_totalCZ","TitR_totalAT")]

G.TitR_total2<-melt(G.TitR_total2,id.vars="English")

g11=ggplot(data=G.TitR_total2,aes(x=English,y=value,fill=factor(variable)))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  ggtitle("Top 10 civil society words used in news titles \n (in CZ/AT at the beginning of 2018)")+
  ylab("Rel. num. of occur. on every published articles in given country (%)")+
  xlab("Word")+
  scale_fill_manual(values=c("darkblue", "red"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
#Example of interpretation: 2,5% of Austria news titles contain word Children.


#RELATIVE TOTAL NUMBERS - DESCRIPTION-SORTED BY TOTAL
G.DescR_total2=G.Desc_total[,c("English","DescR_totalCZ","DescR_totalAT")]

G.DescR_total2<-melt(G.DescR_total2,id.vars="English")

g12=ggplot(data=G.DescR_total2,aes(x=English,y=value,fill=factor(variable)))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  ggtitle("Top 10 civil society words used in description of articles \n (in CZ/AT at the beginning of 2018)")+
  ylab("Rel. num. of occur. on every published articles in given country (%)")+
  xlab("Word")+
  scale_fill_manual(values=c("darkblue", "red"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
#2,5% of Austria news titles contain word Children.

# 
# 
# #CZECH TOP 10
# #RELATIVE TOTAL NUMBERS - TITLE-SORTED BY RELATIVE
# G.TitR_totalCZ=G.TitR_totalCZ[,c("English","TitR_totalCZ","TitR_totalA")]
# 
# G.TitR_totalCZ<-melt(G.TitR_totalCZ,id.vars="English")
# 
# g21=ggplot(data=G.TitR_totalCZ,aes(x=English,y=value,fill=factor(variable)))+
#   geom_bar(stat="identity",position="dodge")+
#   theme_bw()+
#   ggtitle("Top 10 civil society words used in news titles in CZ at the beginning of 2018")+
#   ylab("Rel. num. of occur. on every published articles in given country (%)")+
#   xlab("Word")+
#   theme(axis.text.x=element_text(angle=90,hjust=1))
# 
# 
# 
# #RELATIVE TOTAL NUMBERS - Desc-SORTED BY RELATIVE
# G.DescR_totalCZ=G.DescR_totalCZ[,c("English","DescR_totalCZ","DescR_totalA")]
# 
# G.DescR_totalCZ<-melt(G.DescR_totalCZ,id.vars="English")
# 
# g22=ggplot(data=G.DescR_totalCZ,aes(x=English,y=value,fill=factor(variable)))+
#   geom_bar(stat="identity",position="dodge")+
#   theme_bw()+
#   ggtitle("Top 10 civil society words used at the beginning of articles in CZ at the beginning of 2018")+
#   ylab("Rel. num. of occur. on every published articles in given country (%)")+
#   xlab("Word")+
#   theme(axis.text.x=element_text(angle=90,hjust=1))
# 
# 
# 
# #AUSTRIA TOP 10
# #RELATIVE TOTAL NUMBERS - TITLE-SORT BY RELATIVE
# G.TitR_totalA=G.TitR_totalA[,c("English","TitR_totalCZ","TitR_totalA")]
# 
# G.TitR_totalA<-melt(G.TitR_totalA,id.vars="English")
# 
# g31=ggplot(data=G.TitR_totalA,aes(x=English,y=value,fill=factor(variable)))+
#   geom_bar(stat="identity",position="dodge")+
#   theme_bw()+
#   ggtitle("Top 10 civil society words used in news titles in A at the beginning of 2018")+
#   ylab("Rel. num. of occur. on every published articles in given country (%)")+
#   xlab("Word")+
#   theme(axis.text.x=element_text(angle=90,hjust=1))
# 
# 
# 
# #RELATIVE TOTAL NUMBERS - Desc-SORTED BY RELATIVE
# G.DescR_totalA=G.DescR_totalA[,c("English","DescR_totalCZ","DescR_totalA")]
# 
# G.DescR_totalA<-melt(G.DescR_totalA,id.vars="English")
# 
# g32=ggplot(data=G.DescR_totalA,aes(x=English,y=value,fill=factor(variable)))+
#   geom_bar(stat="identity",position="dodge")+
#   theme_bw()+
#   ggtitle("Top 10 civil society words used at the beginning of articles in A at the beginning of 2018")+
#   ylab("Rel. num. of occur. on every published articles in given country (%)")+
#   xlab("Word")+
#   theme(axis.text.x=element_text(angle=90,hjust=1))

library(gridExtra)
grid.arrange(g11,g12,nrow=1,ncol=2)
#grid.arrange(g21,g22,nrow=1,ncol=2)
#grid.arrange(g31,g32,nrow=1,ncol=2)


#grid.arrange(g11,g12,g21,g22,g31,g32,nrow=3,ncol=2)

#_____________________________________________________________________________________________________
###OCCURENCE OF CIVIL SOCIETY TOPICS (MEASURED BY WORD GROUPS CREATED BY STRATEGY TEAM)
#There is inaccuracy - In the case of article with two different civil society words it will be returned count 2 instead of 1

#discontecting because of useability of sqldf package
dbDisconnect(con_pg)
detach(RPostgreSQL)
library(sqldf)
topics_countsG=topics.countsG
topics.countsG=sqldf(
  "
  select Category, sum(Tit_total) as Tit_total, sum(Tit_totalCZ) as Tit_totalCZ, sum(Tit_totalA) as Tit_totalA
  ,sum(Desc_total) as Desc_total, sum(Desc_totalCZ) as Desc_totalCZ, sum(Desc_totalA) as Desc_totalA
  FROM
  topics_countsG
group by 1
    ")

#Relative numbers
topics.countsG[,"TitR_totalCZ"]=round(topics.countsG[,"Tit_totalCZ"]/nCZ*100,10)
topics.countsG[,"TitR_totalAT"]=round(topics.countsG[,"Tit_totalA"]/nA*100,10)
topics.countsG[,"DescR_totalCZ"]=round(topics.countsG[,"Desc_totalCZ"]/nCZ*100,10)
topics.countsG[,"DescR_totalAT"]=round(topics.countsG[,"Desc_totalA"]/nA*100,10)

#Excluding of most represented category (because of visualisation)
topics.countsG=topics.countsG[topics.countsG$Category!="W/O CATEGORY",]

#RELATIVE TOTAL GROUP NUMBERS - TITLE-SORTED BY TOTAL
G.TitR_totalG=topics.countsG[,c("Category","TitR_totalCZ","TitR_totalAT")]

G.TitR_totalG<-melt(G.TitR_totalG,id.vars="Category")

gg1=ggplot(data=G.TitR_totalG,aes(x=Category,y=value,fill=factor(variable)))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  ggtitle("Grouped civil society words used in news titles \n (in CZ/AT at the beginning of 2018)")+
  ylab("Rel. num. of occur. on every published articles in given country (%)")+
  xlab("Word category")+
  scale_fill_manual(values=c("darkblue", "red"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
#2,5% of Austria news titles contain word Children.
gg1

#RELATIVE TOTAL NUMBERS - Description-SORTED BY TOTAL
G.DescR_totalG=topics.countsG[,c("Category","DescR_totalCZ","DescR_totalAT")]

G.DescR_totalG<-melt(G.DescR_totalG,id.vars="Category")

gg2=ggplot(data=G.DescR_totalG,aes(x=Category,y=value,fill=factor(variable)))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  ggtitle("Grouped civil society topics occured in description of articles \n (in CZ/AT at the beginning of 2018)")+
  ylab("Rel. num. of occur. on every published articles in given country (%)")+
  xlab("Word category")+
  scale_fill_manual(values=c("darkblue", "red"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
#2,5% of Austria news titles contain word Children.
grid.arrange(gg1,gg2,nrow=1,ncol=2)

#__________________________________________________________________________________________________
###FINAL GRAPHS IN PRESENTATION
g12
gg2

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(explor)
library(NbClust)
library(cluster)
library(knitr)
library(corrplot)
library(reshape2)
library(kableExtra)

##(1)data cleaning
#load the data
dt <- read.csv('Socialism.csv', 
               stringsAsFactors = FALSE) 

#prepare the data
new_dt <- dt %>% 
  select(-c("X", "ccode", "scode", "exp.military.wdi")) %>%    #remove repeated index 
  select(-ends_with(c(".indiv", ".corp", ".oth", ".workers",   #remove minor types of tax
                    ".emplr", ".self", ".wealth", ".estate", 
                    ".ftrans", ".nonrecur", ".xfer", ".use",
                    "barriers.expl", "barriers.other",         #remove minor types of regulation barrier
                    ".excl", ".edu", ".mem", ".quot", ".price", 
                    ".entry", ".conduct", ".prices",           #remove minor types of prof regulation
                    ".bizform", ".adv", ".coop"))) %>% 
  select_if(~sum(is.na(.)) <= 1/3*46) %>%                      #delete columns with over one third NAs
  mutate_all(funs(replace_na(., 0))) %>%                       #transforms left NAs to 0
  filter(country != 0) %>%                                     #remove rows with unknown countries
  relocate(c("country", "polity2"))

##(2)descriptive statistics
#top countries with highest level of democracy
dem_rank <- new_dt %>% 
  group_by(country) %>% 
  arrange(desc(polity2)) %>% 
  select(c("country", "polity2")) %>% kable()

#summary statistics of other variables
sum_dt <- data.frame(Min = apply(new_dt[4:85], 2, min),          # minimum
                     Q1 = apply(new_dt[4:85], 2, quantile, 1/4), # First quartile
                     Med = apply(new_dt[4:85], 2, median),       # median
                     Mean = apply(new_dt[4:85], 2, mean),        # mean
                     Q3 = apply(new_dt[4:85], 2, quantile, 3/4), # Third quartile
                     Max = apply(new_dt[4:85], 2, max))          # Maximum
sum_dt <- round(sum_dt, 2) %>% kable()
head(sum_dt)

#correlation matrix
re <- cor(new_dt[4:85])
round(re, 2)
corrplot(re, type = "upper")

##(3)PCA
row.names(new_dt) <- new_dt$country   #set country as the individual label
res <- PCA(new_dt[,-1],               #remove country
           quanti.sup = 1,            #select polity2
           scale.unit = T)
 
#variance associated with the component
fviz_screeplot(res, ncp = 10)

#variable graph color by contrib values
fviz_pca_var(res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Fig.2 Variables - PCA",
             repel = TRUE) #avoid text overlapping

#contributions of variables to PC1
fviz_contrib(res, choice = "var", axes = 1, top = 10, 
             title = "Fig.3 Contribution of variables to PC1")

#contributions of variables to PC2
fviz_contrib(res, choice = "var", axes = 2, top = 10,
             title = "Fig.4 Contribution of variables to PC2")

#individual graph
fviz_pca_ind(res, habillage = 1, repel = TRUE,
             title = "Fig.5 Individuals - PCA") 
             
##(4)PAM clustering
dt_scale <- scale(new_dt[2:85])
set.seed(123)
pam.res <- pam(dt_scale, 2, metric = "euclidean")

#optimal number of clusters-silhouette
fviz_nbclust(dt_scale, cluster::pam, method = "silhouette")

#optimal number of clusters-gap
fviz_nbclust(dt_scale, pam, method = c("gap")) 

#plot cluster
fviz_cluster(pam.res, repel = TRUE, title = "Fig.7 Cluster plot") 

#PAM-quality
fviz_silhouette(pam.res, palette = "jco",
                ggtheme = theme_classic())
                
#result analysis
new_dt.c <- cbind(new_dt, pam.res$clustering)
colnames(new_dt.c)[86] <- c("pam.group")
new_dt.m <- melt(new_dt.c[,c("pam.group", "polity2","reg.ret.overall", 
                             "exp.consumption", "reg.statecontrol", "exp.total",
                             "tax.socsec.tot")], id.var = "pam.group")
new_dt.m$pam.group <- as.character(new_dt.m$pam.group)
new_dt.m$pam.group[new_dt.m$pam.group == "1"] <- "democratic group" 
new_dt.m$pam.group[new_dt.m$pam.group == "2"] <- "autocratic group"

ggplot(data = new_dt.m, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill = pam.group),outlier.size = 1) +
  facet_wrap( ~ variable, scales="free", ncol = 2) +
  guides(fill=guide_legend(title="Groups")) +
  labs(title = "Fig.9 Result Analysis - PAM")


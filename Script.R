# loading libraries 
library(text2map)
library(text2vec)
library(gutenbergr)
library(tidyverse)
library(textclean)
library(stringi)
library(openxlsx)
library(dplyr)
library(wordVectors)
library(quanteda)
library(factoextra)
library(FactoMineR)
library(writexl)
library("reshape2") 
library(radiant)

#set working directory 
setwd("/Users/tatiana/Desktop/Публикации/2024/Research_results")

#loading file with data 
dataforanalysis1 = read.xlsx("/Users/tatiana/Desktop/Публикации/2024/Research_results/dataforanalysis1.xlsx")
str(dataforanalysis1)

# creating Fig. 1 
totalwords = summary(as.factor(dataforanalysis$WORD_TYPE))
summary(totalwords)
sd(totalwords)

agg <- count(dataforanalysis, WORD_TYPE)
head(agg)
agg_ord <- mutate(agg, WORD_TYPE = reorder(WORD_TYPE, -n, sum))
h= ggplot(agg_ord) + geom_bar(aes(x = WORD_TYPE, y = n), stat = "identity", fill="blue")
h + coord_flip() + theme(axis.text.y = element_text(size = 8)) + xlab("Стимул") + ylab("Число рядов")
ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/wordtype1.png",   width=8, height=8, dpi=600)

# creating Fig. 2
totalauthors = summary(as.factor(dataforanalysis$AU))
summary(totalauthors)
sd(totalauthors)

agg1 <- count(dataforanalysis, AUTHOR)
agg_ord1 <- mutate(agg1, AUTHOR = reorder(AUTHOR, -n, sum))
h= ggplot(agg_ord1) + geom_bar(aes(x = AUTHOR, y = n), stat = "identity", fill="blue")
h + coord_flip() + theme(axis.text.y = element_text(size = 8)) + xlab("ID автора") + ylab("Число рядов")
ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/author.png",   width=8, height=8, dpi=600)

# obtaining SD values from language model
## loading word2vec model using different methods
model =  read.vectors("/Users/tatiana/Documents/Модели_векторы/НКРЯ+Russian Wikipedia Dump of November 2021/model.bin")
WE <- read.wordvectors("/Users/tatiana/Documents/Модели_векторы/НКРЯ+Russian Wikipedia Dump of November 2021/model.bin", type = "bin", normalize = F) 
str(WE)

## make document-feature matrix from association raws
assoc_corpus <- corpus(dataforanalysis, text_field="Association.raw", docid_field = "ID") 
tokenmy = tokens(assoc_corpus)
tokenmy
dtm_assoc = dfm(tokenmy, tolower = F)
dtm_assoc

## extracting Petrenko features 
# build the semantic direction 1 (and so on till 18):
additions  <- c("приятный_ADJ")
substracts <- c("неприятный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE)
doc1_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)
head(doc1_closeness)

#merge all data frames together
petrenko_list %>% reduce(full_join, by="doc_id")
petrenko_list = as.data.frame(petrenko_list)
write.xlsx(petrenko_list, "petrenko_raw.xlsx") # this returns raw values (not normalized)

#normalize SD values and merge with metadata 
## normalize
petrenko_feat = read.xlsx("/Users/tatiana/Desktop/Публикации/2024/Research_results/petrenko_raw.xlsx")  
View(petrenko_feat)
petrenko_feat=petrenko_feat[, -1]
petrenko_min = normalize(petrenko_feat, method = "range", range = c(0, 1))
View(petrenko_min)
write.xlsx(petrenko_min, "petrenko_norm.xlsx") # save normalized  SD values 

## merge with metadata
rawwithpetr= cbind(dataforanalysis, petrenko_feat)
rawwithpetr$AUTHOR = as.factor(rawwithpetr$AUTHOR)
rawwithpetr$WORD_TYPE = as.factor(rawwithpetr$WORD_TYPE)
View(rawwithpetr)
normwithpetr= cbind(dataforanalysis, petrenko_min)
normwithpetr$AUTHOR = as.factor(normwithpetr$AUTHOR)
normwithpetr$WORD_TYPE = as.factor(normwithpetr$WORD_TYPE)
View(normwithpetr)

# Calculate coefficient of variation for  all numeric columns by WORD_TYPE
library(dplyr)
normwithpetrMETA = normwithpetr[, -c(3:22)] 
str(normwithpetrMETA)
group_summary_df <- normwithpetrMETA %>%
  group_by(WORD_TYPE) %>%
  summarise(across(where(is.numeric), list(cv = ~sd(.)/mean(.) * 100)))
summary(group_summary_df)

## visualise
data_long <- melt(group_summary_df)                                      # Reshaping data frame
head(data_long)  
gg = ggplot(data_long, aes(x = variable, y = value)) +            # Applying ggplot function
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.25) + labs(x = "Название шкалы", y = "Коэффициент вариации") 
gg + coord_flip() + theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))
ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/wordtypeCV.png",   width=8, height=8, dpi=600)
 
# Calculate coefficient of variation for  all numeric columns by AUTHOR
group_summary_df1 <- normwithpetrMETA %>%
  group_by(AUTHOR) %>%
  summarise(across(where(is.numeric), list(cv = ~sd(.)/mean(.) * 100)))
View(group_summary_df1)
summary(group_summary_df1) 

## visualise
data_long1 <- melt(group_summary_df1)                                      # Reshaping data frame
head(data_long1)  
gg = ggplot(data_long1, aes(x = variable, y = value)) +            # Applying ggplot function
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.25) + labs(x = "Название шкалы", y = "Коэффициент вариации") 
gg + coord_flip() + theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))
ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/AuthorCV.png",   width=8, height=8, dpi=600)

# comparing values of CV
t.test(group_summary_df1$Опасность_cv, group_summary_df$Опасность_cv)
t.test(group_summary_df1$Единость_cv, group_summary_df$Единость_cv)

# creating word profiles
word_plot <- pivot_longer(normwithpetr,
                                cols = 23:40,
                                names_to = "feats",
                                values_to = "counts")
word <- word_plot %>%
  filter(WORD_TYPE == "мир2") 
g.norm <- ggplot(data = word, aes(x = feats, y = counts)) +
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.25)   
g.norm + coord_flip() +  labs(title="мир2") + theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
                                                       axis.title.x = element_blank(),
                                                       axis.title.y = element_blank())

ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/ПРОФИЛИ СЛОВ/мир2.png",   width=10, height=8, dpi=600)


## correlation matrix
# for psychological traits and states 
psycho = cor(normwithpetr[, c(3:20)])
psycho
testRes = cor.mtest(normwithpetr[, c(3:20)], conf.level = 0.95)
head(testRes)

dd=corrplot(psycho, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'lower',
           sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, tl.srt = 45, 
           insig = 'label_sig', pch.col = 'grey20', order = 'AOE')
ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/corr_psycho.png",   width=10, height=8, dpi=600) 

# for SD scores
head(res)
d=corrplot(res, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'lower',
           sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, tl.srt = 45, 
           insig = 'label_sig', pch.col = 'grey20', order = 'AOE')
ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/Рисунки_рабочее/corr2.png",   width=10, height=8, dpi=600)


# PCA and factor analysis
## KMO test 
# preliminary
summary(pre_factor(normwithpetr, vars = c(21:38)))
# final
dd = summary(full_factor(normwithpetr, vars = c(21:38), nr_fact = 7))
View(dd)
# PCA
normwithpetr = normwithpetr[, -1]
normwithpetr = normwithpetr[, -21]
View(normwithpetr)
res.pca = PCA(normwithpetr, scale.unit=F, ncp=7, quali.sup=c(1:2), quanti.sup=c(3:20), graph=F)
get_eig(res.pca)
fviz_eig(res.pca, choice = c("variance", "eigenvalue"), 
         geom = c("bar", "line"), barfill = "steelblue",
         barcolor = "steelblue", linecolor = "black", 
         ncp = 10, addlabels = F, xlab="Компоненты", ylab = "Процент объясненной вариации")

var <- get_pca_var(res.pca)
head(var)
str(normwithpetr)
fviz_pca_var(res.pca, col.var="contrib", labelsize = 3, 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7))
?fviz_pca_var

ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/Рисунки/признаки.png",   width=9, height=9, dpi=300)


# describe dimensions
dimdesc(res.pca, axes=c(1,2))
res.pca$quanti.sup
res = dimdesc(res.pca, axes=1:3, proba=0.05)
куres$Dim.1$quali
res$Dim.3$category
res
# Hierarchical Clustering on Principal Components
res.hcpc = HCPC(res.pca)
res.hcpc <- HCPC(res.pca, nb.clust=4, graph = F)
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category
plot(res.hcpc, axes=c(1,2), choice="3D.map", rect=TRUE, 
     draw.tree=TRUE, ind.names=FALSE, t.level="all", title="Дендрограмма",
     new.plot=FALSE, max.plot=15, tree.barplot=FALSE)

plot(res.hcpc, choice="3D.map", angle=75, title="Дендрограмма")

ggsave("/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/Рисунки/кластеры3.png",   width=9, height=9, dpi=300)

# cluster description 
names(res.hcpc$data.clust)[names(res.hcpc$data.clust) == 'clust'] <- 'Cluster'
rescluster=res.hcpc$data.clust
View(rescluster)
write.xlsx(rescluster, "clustermember1.xlsx", rowNames=T)

# cluster description 1
## autoEDA 
autoEDA_results <- autoEDA(rescluster, 
                           y = "Cluster", returnPlotList = TRUE,
                           outcomeType = "automatic", removeConstant = TRUE, 
                           removeZeroSpread = TRUE, removeMajorityMissing = TRUE, 
                           imputeMissing = TRUE, clipOutliers = FALSE, 
                           minLevelPercentage = 0.025, predictivePower = TRUE, 
                           outlierMethod = "tukey", lowPercentile = 0.01, 
                           upPercentile = 0.99, plotCategorical = "groupedBar", 
                           plotContinuous = "histogram", bins = 30, 
                           rotateLabels = TRUE, color = "#26A69A", 
                           verbose = FALSE) 
head(res.hcpc$data.clust$clust)
View(rescluster)


#kruskal.test
#Agreeableness
kruskal.test_osn1 <- kruskal.test(Agreeableness ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Agreeableness, rescluster$clust,
                     p.adjust.method = "BH")

#Extraversion
kruskal.test_osn1 <- kruskal.test(Extraversion ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Extraversion, rescluster$clust,
                     p.adjust.method = "BH")

#Contiousness
kruskal.test_osn1 <- kruskal.test(Contiousness ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Contiousness, rescluster$clust,
                     p.adjust.method = "BH")

#Neurotism
kruskal.test_osn1 <- kruskal.test(Neurotism ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Neurotism, rescluster$clust,
                     p.adjust.method = "BH")

#Openness
kruskal.test_osn1 <- kruskal.test(Openness ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Openness, rescluster$clust,
                     p.adjust.method = "BH")

#Interes 
kruskal.test_osn1 <- kruskal.test(Interes  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Interes, rescluster$clust,
                     p.adjust.method = "BH")
 
# Radost 
kruskal.test_osn1 <- kruskal.test(Radost  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Radost, rescluster$clust,
                     p.adjust.method = "BH")

# Udivlenie 
kruskal.test_osn1 <- kruskal.test(Udivlenie  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Udivlenie, rescluster$clust,
                     p.adjust.method = "BH")


# Gore 
kruskal.test_osn1 <- kruskal.test(Gore  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Gore, rescluster$clust,
                     p.adjust.method = "BH")


# Gnev 
kruskal.test_osn1 <- kruskal.test(Gnev  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Gnev, rescluster$clust,
                     p.adjust.method = "BH")

# Otvrascenie 
kruskal.test_osn1 <- kruskal.test(Otvrascenie  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Otvrascenie, rescluster$clust,
                     p.adjust.method = "BH")

# Prezrenie 
kruskal.test_osn1 <- kruskal.test(Prezrenie  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Prezrenie, rescluster$clust,
                     p.adjust.method = "BH")

# Strah 
kruskal.test_osn1 <- kruskal.test(Strah  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Strah, rescluster$clust,
                     p.adjust.method = "BH")

# Styd 
kruskal.test_osn1 <- kruskal.test(Styd  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Styd, rescluster$clust,
                     p.adjust.method = "BH")

# Vina 
kruskal.test_osn1 <- kruskal.test(Vina  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$Vina, rescluster$clust,
                     p.adjust.method = "BH")

# PEM 
kruskal.test_osn1 <- kruskal.test(PEM  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$PEM, rescluster$clust,
                     p.adjust.method = "BH")
rescluster %>%
  group_by(clust) %>%
  dplyr::summarize(median = median(PEM, na.rm=TRUE))

# NEM 
kruskal.test_osn1 <- kruskal.test(NEM  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$NEM, rescluster$clust,
                     p.adjust.method = "BH")

# TDEM 
kruskal.test_osn1 <- kruskal.test(TDEM  ~ clust, data = rescluster)
kruskal.test_osn1
pairwise.wilcox.test(rescluster$TDEM, rescluster$clust,
                     p.adjust.method = "BH") 
 
chisq1 <- chisq.test(rescluster$AUTHOR, rescluster$clust, simulate.p.value = TRUE)
chisq1
chisq2 <- chisq.test(rescluster$WORD_TYPE, rescluster$clust, simulate.p.value = TRUE)
chisq2
corrplot(chisq2$residuals, is.cor = FALSE)
contrib <- 100*chisq2$residuals^2/chisq2$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# inspecting clusters
table(rescluster$clust, rescluster$AUTHOR)
table(rescluster$clust, rescluster$WORD_TYPE)
table3 = prop.table(table(rescluster$clust, rescluster$WORD_TYPE), 2)
head(table3)
table4 = prop.table(table(rescluster$clust, rescluster$AUTHOR), 2)
write.xlsx(table3, "/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/word.cluster.xlsx")
write.xlsx(table4, "/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/author.cluster.xlsx")

# inspecting relations in clusters
library(GoodmanKruskal)
GKtau(rescluster$clust, rescluster$WORD_TYPE)
GKtau(rescluster$clust, rescluster$AUTHOR)
GKtau(rescluster$WORD_TYPE, rescluster$AUTHOR)
GKmat <- GKtauDataframe(rescluster)
plot(GKmat, diagSize = 0.8)

library('vcd')
dd= cramer_v(rescluster$clust, rescluster$WORD_TYPE, correct = TRUE)
dd

# mean values in clusters
## linguistic characteristics 

rescluster1 = t(ddply(rescluster, 'clust', summarize, mean_Приятность = mean(Приятность), mean_Полезность = mean(Полезность), mean_Веселость = mean(Веселость), mean_Живость = mean(Живость), mean_Быстрота = mean(Быстрота), mean_Яркость = mean(Яркость), mean_Пассивность = mean(Пассивность), mean_Упорядоченность = mean(Упорядоченность), mean_Устойчивость = mean(Устойчивость), mean_Кратковременность = mean(Кратковременность), mean_Единость = mean(Единость), mean_Конкретность = mean(Конкретность), mean_Сложность = mean(Сложность), mean_Крупность  = mean(Крупность), mean_Сила = mean(Сила), mean_Сила = mean(Сила), mean_Редкость = mean(Редкость),  mean_Единичность = mean(Единичность), mean_Опасность = mean(Опасность))) 
View(rescluster1) 
rescluster1= as.data.frame(rescluster1)
dat <- as.data.frame(sapply(rescluster1, as.numeric))
dat = round_df(dat, 3)
View(dat)
write.xlsx(dat, "/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/mean.cluster1.xlsx", rowNames=T)

# psycho 
rescluster1 = t(ddply(rescluster, 'clust', summarize, median_Agreeableness = median(Agreeableness), mean_Extraversion = median(Extraversion), median_Contiousness  = median(Contiousness), mean_Openness  = median(Openness), mean_Interes = median(Interes), mean_Radost = median(Radost), mean_Prezrenie = median(Prezrenie), mean_Styd = median(Styd), mean_PEM = median(PEM))) 
View(rescluster1)

rescluster2 = t(ddply(rescluster, 'clust', summarize, mean_Agreeableness = mean(Agreeableness), mean_Extraversion = mean(Extraversion), median_Contiousness  = mean(Contiousness), mean_Openness  = mean(Openness), mean_Interes = mean(Interes), mean_Radost = mean(Radost), mean_Prezrenie = mean(Prezrenie), mean_Styd = mean(Styd), mean_PEM = mean(PEM))) 
View(rescluster2)
rescluster2= as.data.frame(rescluster2)
dat2 <- as.data.frame(sapply(rescluster2, as.numeric))
dat2 = round_df(dat2, 3)
View(dat2)
write.xlsx(dat2, "/Users/tatiana/Desktop/Публикации/2024/Research_results/Данные для размещения/mean.cluster2.xlsx", rowNames=T)

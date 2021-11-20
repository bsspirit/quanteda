######################
# 量化文本分析
# @author: Zhangdan
# @email: bsspirit@gmail.com
######################

rm(list=ls())
library(quanteda)
library(readtext)
library(ggplot2)
library(magrittr)
library(stringr)
library(plyr)
library(reshape2)
setwd("C:/work/R/text/quanteda")

# 加载数据集
docs<-readtext("./job/*.txt",
            docvarsfrom = "filenames",
            encoding = "UTF-8") # 公司，待遇，地点，职位，时间

###############
# 建立语料库
###############
doc.corpus<-corpus(docs)
doc.corpus<-doc.corpus[order(doc.corpus$docvar1),]
summary(doc.corpus)

# 增加文档变量
docvars(doc.corpus, "公司") <- c("青萌数海","当当网","易华录","BOSS直聘","京东集团","小米","Flash Express","Shopee","珀菲克特","翼鸥教育","微博","京东集团","BOSS直聘","美团","便利蜂")
docvars(doc.corpus, "职位")<- rep("数据分析师",15)
docvars(doc.corpus, "工资")<- c("25-50K","20-35K","15-25K","14-15K","20-40K","20-40K","11-22K","25-50K","30-45K","25-50K","20-30K","20-35k","25-50K","20-27K","30-60K")
docvars(doc.corpus, "薪数")<- c("14","14","","","14","14","16","15","14","","14","15","16","","")
salary<-str_remove(doc.corpus$工资,"K") %>% str_split("-") %>% ldply
docvars(doc.corpus,"工资min")<-salary$V1
docvars(doc.corpus,"工资max")<-salary$V2
  
# 查看基本信息
doc.corpus
ndoc(doc.corpus)
docnames(doc.corpus)
as.character(doc.corpus)[2]

doc.smy<-summary(doc.corpus);doc.smy

# 看基本变量
ggplot(data = doc.smy, aes(x = 公司, y = Tokens, group = 1)) + geom_line() +  geom_point() 
ggplot(data = doc.smy, aes(x = 工资, y = Tokens, group = 1)) + geom_line() +  geom_point() 

df<-reshape(doc.smy[,c("公司","工资min","工资max","薪数")],varying = list(2:3), idvar = "id",v.names = "工资", direction = "long")
row.names(df)<-NULL
df$time[which(df$time==1)]<-'min'
df$time[which(df$time==2)]<-'max'
df$薪数[which(df$薪数=="")]<-12
df$薪数<-as.integer(df$薪数)
df$薪数<-1+(df$薪数-12)*0.1
df
ggplot(data = df, aes(x = 公司, y = 工资, colour = time)) + 
  geom_line(aes(group=time)) +  geom_point(aes(size=薪数)) 

# 数据处理
corpus_sample(doc.corpus,3) # sample
doc.smy[which.max(doc.smy$Tokens), ]

# 合并2个语料库
corp12<-doc.corpus[1]+doc.corpus[2]
summary(corp12)

# 取子集，过滤条件
summary(corpus_subset(doc.corpus, 工资min >= 20))
summary(corpus_subset(doc.corpus, 薪数 >= 14 & 工资min >= 20 & 工资max>=40))

###############
# 分词
###############
doc.tokens<-tokens(doc.corpus)
as.character(doc.tokens[1])

doc.tokens.sentence <- tokens(doc.corpus, what = "sentence")
doc.tokens.sentence[1]

doc.tokens.character <- tokens(doc.corpus, what = "character")
as.character(doc.tokens.character[1])

# 特征清洗和停用词表
as.character(doc.tokens[1])
t1<-tokens(doc.tokens, remove_punct = TRUE, remove_numbers = TRUE)
as.character(t1[1])
t2 <- tokens_select(t1, stopwords('english'),selection='remove')
as.character(t2[1])
t3 <- tokens_select(t2, stopwords("zh", source = "misc"),selection='remove')
as.character(t3[1])

# 停用词表
stopwords("english")
stopwords(language = "en", source = "smart")
stopwords(language = "zh",source="misc")

myword<-c("公司","非常","做","一条","员工","学习","在一起","提供","主要","一家","介绍","进行",
          "能够","要求","利用","理解","熟悉","涉及","计划","大幅","制定","帮助","懂得","都是",
          "实现","定位","基础","至少","一个","驱动","能力","包括","都是","具有","致力","成立",
          "深入","并与","处理","具备","方向","方面","多个","广泛","应用","多年","清晰","相关",
          "外部","内部","突出","丰富","拥有","取得","建立","助力","建立","形成","判断","日常",
          "使用","解决","覆盖","整合","开展","充分利用","工作","科技","制作","共同","了解","各类",
          "实践","推动","旗下","善于","掌握","推进","支持","协调","配合","考虑","成为","研究",
          "优先","良好","严谨","指导","展开","拓展","正式","网上","综合","横跨","安排","协助",
          "把握","紧紧","认真","完成","一定","打造","方法","常用","资格","事项","负责","任职",
          "参与","描述","职位","执行","职责","梳理","岗位","责任","跟进","协作","岗位","统筹",
          "搭建","编写","主流","高效","第一","效果","建议","一流","经历","内容","比较","程度",
          "灵活运用","发展","手段","便捷","大概是","重度","绝对","必须","|","平台","或有","直接",
          "容易","方式","总体","率先","范围","引入","直接","智能","+","经常","保持","部门","建设",
          "复合","情况","改善","提升","一种","多种","认识","输出","响应","相应","尤其是","形态",
          "目标","达到","追求","不断","精神","专研","心中","多方面","发现","人心","坚持","始终",
          "感动","第二","享受","都能","每个人","确定","深度","基本","富有","善于","使命","努力",
          "美好","胸怀","持续","提高","洞察","体验","朋友","钻研","擅于","生态","带来","热爱",
          "最快","快速","完善","年底","创建","来自于","位于","中心","知名","最快","头部","冠军",
          "三项","强势","拿下","强烈","年度","斩获","高达","一线","＋","超过","深耕","前瞻","杰出",
          "优秀","高度","擅长","灵活","有意","优势","智慧","新增","沉淀","依靠","领先","季度",
          "核心","迅速","目前","潜在","看到","一句","一起","想到","推出","联系","完整","发掘",
          "听到","随地","之间","一张","写成","事情","多方","第四次","接近","入选","操作","拿到",
          "提出","广大","中的","刻画","力求","初心","之初","常规","更有","特色","开设","全面",
          "小小的","第三","更多","更高","承担","水平","承受","整个","涉足","开通","桥梁")
t4 <- tokens_select(t3,myword,selection='remove')
as.character(t4[1])

###############
# 文字搜索
###############
kwic(t4, pattern = "算法", valuetype = "regex")
kwic(t4, pattern = "精通", valuetype = "regex")
kwic(t4, pattern = "管理", valuetype = "regex")
kwic(t4, pattern = "学历", valuetype = "regex")
kwic(t4, pattern = "R|python|sql", valuetype = "regex")
kwic(t4, pattern = "flink|hadoop", valuetype = "regex")



###############
# 矩阵化
###############
doc.dfm1<-dfm(doc.tokens)
doc.dfm1

nfeat(doc.dfm1)
featnames(doc.dfm1)

doc.dfm<-dfm(t4)
doc.dfm

nfeat(doc.dfm)
featnames(doc.dfm)

topfeatures(doc.dfm, 200)

# 按文档变量分组
dfm_group(doc.dfm,groups = 工资)
dfm_group(doc.dfm,groups = 公司)

# 按字典分组
dict <- dictionary(
  list(算法 = c("算法","模型","统计"),
         业务 = c("业务","行业","需求"),
         大数据 = c("hadoop","Spark","Flink"),
         编程 = c("R","Python","Java","sql")))

tokens_lookup(t4,dictionary = dict) %>%
  dfm()%>%
  dfm_group(groups = 工资)

tokens_lookup(t4,dictionary = dict) %>%
  dfm()%>%
  dfm_group(groups = 公司)


# 词云
library("quanteda.textplots")
set.seed(1)
textplot_wordcloud(doc.dfm, min_count = 3, random_order=TRUE,
                   rotation = 0.25, min_size = 1, max_size = 4,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


###############
# 特征共现矩阵
###############
fcmat<-fcm(t4, context = "window", window = 5);fcmat
#fcmat<-fcm(t4, context = "window", count = "weighted", window = 5)
#fcmat<-fcm(t4, context = "document")
feat <- names(topfeatures(fcmat, 50));feat
fcmat<-fcm_select(fcmat, pattern = feat)
textplot_network(fcmat,min_freq = 0.5,vertex_labelsize = 1.5 * log(rowSums(fcmat)+1))



# 文本相似
library("quanteda.textstats")
simi <- textstat_simil(doc.dfm,margin = "documents", method = "cosine")
simi

library(pheatmap)
pheatmap(as.matrix(simi))
pheatmap(as.matrix(simi),kmeans_k = 3)
summary(doc.corpus)

# 相似
as.character(t4[2])
as.character(t4[6])
as.character(t4[7])
as.character(t4[12])

# 不相似
as.character(t4[13])
as.character(t4[15])
as.character(t4[1])

# 词相似
tstat_sim <- textstat_simil(doc.dfm, 
                            doc.dfm[, c("数据", "业务", "统计","python","学历")],
                            method = "cosine", margin = "features")
lapply(as.list(tstat_sim), head, 10)



# 聚类分析
dfm_tmp <- dfm_trim(doc.dfm, min_termfreq = 3, min_docfreq = 3)
#分层聚类 -  在归一化dfm上计算距离
dist_tmp <- textstat_dist(dfm_weight(dfm_tmp, scheme = "prop"))
# 聚类分析文本距离
cluster_tmp <- hclust(as.dist(dist_tmp))
# 按文档名标注
cluster_tmp$labels <- docnames(dfm_tmp)
# 绘制树状图
plot(cluster_tmp, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")


# 关联分析
textstat_collocations(t4, size = 2, tolower = TRUE) %>% head(20)
textstat_collocations(t4, size = 2, min_count = 10, tolower = TRUE)%>% head(20)
textstat_collocations(t4, size = 5, tolower = TRUE)%>% head(20)


# 文档位置：wordfish无监督文档缩放分析
library("quanteda.textmodels")
tmod_wf <- textmodel_wordfish(doc.dfm, dir = c(2, 1))
textplot_scale1d(tmod_wf, groups = docvars(doc.dfm, "工资"))
textplot_scale1d(tmod_wf, groups = docvars(doc.dfm, "公司"))

textplot_scale1d(tmod_wf, margin = "features", 
                 highlighted = c("数据", "工资", "学历","python", "r", "模型", 
                                 "建模","业务", "sql","跨境","电商","项目","用户"))


# 主题模型
library(stm)
set.seed(100)
model_stm <- stm(dfm_tmp, K = 10)
plot(model_stm)    

library(seededlda)
model_lda <- textmodel_lda(dfm_tmp, k = 10)
terms(model_lda, 10)
dfm_tmp$topic <- topics(model_lda)
table(dfm_tmp$topic) %>% plot

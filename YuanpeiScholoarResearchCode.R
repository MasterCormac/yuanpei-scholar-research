library(haven)
library(skimr)

data_1<-read_sav("/Users/zz/Downloads/元培_R代码/元培问卷数据_93.sav")
data_1$gender
data_1$grade_f<-as.factor(data_1$grade)
data_1$grade_f<-ifelse(data_1$grade_f=="1","2",data_1$grade_f)
col<-colnames(data_1)
col<-as.data.frame(col)
table(data_1$gender)
#Cronbach's alpha coefficient#####
#Internal consistency: “Cronbach’s alpha” coefficient and “split-half reliability”
#     Reliability refers to the consistency or stability of results obtained from a testing tool (scale).

#install.packages("psych")
library("psych") #alpha(data,check.keys=TRUE) 
library(ltm)

# load library ltm
#install.packages("ltm")
cronbach.alpha(data_1[,col[c(grep("TM_", col[,1])),][1:20]])
cronbach.alpha(data_1[,col[c(grep("FFM_", col[,1])),][1:20]])
cronbach.alpha(data_1[,col[c(grep("AS_", col[,1])),][1:5]])
cronbach.alpha(data_1[,col[c(grep("age|gender|grade|school_type|use_of_intelligent_electronic_product", col[,1])),]])
cronbach.alpha(data_1[,c(col[c(grep("AS_", col[,1])),][1:5],col[c(grep("FFM_", col[,1])),][1:20],col[c(grep("TM_", col[,1])),][1:20],
                             col[c(grep("age|gender|grade|school_type|use_of_intelligent_electronic_product",
                                   col[,1])),])])


#Data processing#####
data<-as.data.frame(data_1)
m<-skim(data)
head(data,6)
col<-colnames(data)
col<-data.frame(col)
#Time management abilities
data$TM_GandP_n<-data$TM_goals_and_priorities
data$TM_M_n<-data$TM_mechanics
data$TM_PforO_n<-data$TM_preference_for_organization
data$TM_PCofT_n<-data$TM_perceived_control_of_time


data$FFM_E_n<-data$FFM_Extroversion
data$FFM_A_n<-data$FFM_Agreeableness
data$FFM_C_n<-data$FFM_Consciousness
data$FFM_N_n<-data$FFM_Neuroticism
data$FFM_O_n<-data$FFM_Openness

data$AS_n<-data$AS_total
table(data$age)

data$age<-as.factor(data$age)
data$age_n<-ifelse(data$age =="1","13",
                 ifelse(data$age =="2","14",
                        ifelse(data$age =="3","15",
                               ifelse(data$age =="4","16",
                                      ifelse(data$age =="5","17",
                                             ifelse(data$age =="6","18",data$age ))))))
data$gender_f<-as.factor(data$gender)
data$grade_f<-as.factor(data$grade)
data$grade_f<-ifelse(data$grade_f=="1","2",data$grade_f)
data$school_type_f<-as.factor(data$school_type)
data$phone_f<-as.factor(data$use_of_intelligent_electronic_product)
data$id<-as.factor(data$index)




col<-colnames(data)
col<-data.frame(col)

a<-col[c(grep("_n$|_f$",col[,1])),]
data<-subset(data,select=c("id",a))
data_1<-data



###Descriptive Stats####
col<-colnames(data)
col<-as.data.frame(col)
o<-col[c(grep("_o$",col[,1])),]
num<-col[c(grep("_n$",col[,1])),]
f<-col[c(grep("_f$",col[,1])),]
data[,c(num)]<-lapply(data[,c(num)],as.numeric)
data[,c(f)]<-lapply(data[,c(f)],as.character)
data[,c(f)]<-lapply(data[,c(f)],as.factor)
table(data$age_n)
library(CBCgrps)
table<-multigrps(data[,-1],norm.rd = 1,cat.rd = 1,sk.rd =1,sim=TRUE,
                        gvar ="gender_f",,
                        p.rd = 3,ShowStatistic = T,minfactorlevels=3)

setwd("~/Desktop/z/0222 results/")
write.csv(table,"table1.csv")



### Cor. coefficients (皮尔逊Pearson - r) ####
data$score<-apply(data[,col[c(grep("TM_",col[,1])),]],1,sum)
library(Hmisc)#加载包
i="FFM_C_n"
for(i in c(col[c(grep("FFM_",col[,1])),],"AS_n")){
a<- cor.test(data[,"score"], data[,c(i)], method=c("pearson"))
print(paste(i,"  P value :",round(a$p.value,3),"  cor :",round(a$estimate,3) ))
}
data$g<-as.character(data$grade_f)
data$g<-as.numeric(data$g)
a<- cor.test(data[,"g"], data[,"age_n"], method=c("pearson"))
print(paste(i,"  P value :",round(a$p.value,3),"  cor :",round(a$estimate,3) ))


aa<-subset(data,select=c(col[c(grep("FFM_|TM_|AS_", col[,1])),],"score"))
aa<-lapply(aa[1:length(aa)], as.numeric)
aa<-as.data.frame(aa)
colnames(aa)

# calculate the correlation matrix
library(ggcorrplot)
corr<- round(cor(aa[1:length(aa)]),3)
kappa (corr,exact= TRUE )#[1]1.298785e+18    #>1000, so there is severe multicollinearity
p.mat <- round(cor_pmat(aa),3)
#
labels_0<-c("TM_GandP_n"="目标和优先级能力",
            "TM_M_n"="执行力能力",
            "TM_PforO_n"="组织倾向能力",
            "TM_PCofT_n" ="时间控制感知能力",
            "FFM_E_n"="外向性",
            "FFM_A_n"="宜人性",
            "FFM_C_n"="尽责性",
            "FFM_N_n"="神经质",
            "FFM_O_n"="开放性",
            "score"="时间管理能力总分",
            "AS_n"="压力")

fig_<-
  ggcorrplot(corr, method = c("square"), type = c("full"), 
             ggtheme = ggplot2::theme_void,
             title = " ", 
             show.legend = T, legend.title = "", show.diag = T, 
             colors = c("#839EDB", "white", "#FF8D8D"), outline.color ="grey", 
             hc.order = T, hc.method = "single", 
             lab = T, lab_col = "black",  lab_size =2, 
             p.mat = p.mat, 
             sig.level = 0.05, 
             insig = c("pch"), pch =1, 
             pch.col = "grey", pch.cex = 10, 
             tl.cex = 12,tl.col = "black", tl.srt = 45, digits = 3)+
  theme(text=element_text(family="STKaiti"),
        axis.text.x=element_text(angle=70, hjust =1,colour="black",size=8), #设置x轴刻度标签的字体显示倾斜角度（）angle，并向下调整1(hjust = 1)，字体簇为Times大小为20
        axis.text.y=element_text(size=8,hjust =1) #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
  )+
  scale_y_discrete(labels = labels_0 )+
  scale_x_discrete(labels = labels_0 )

mypath_1<-"/Users/cecdsc/Desktop/z/0222 results/"
save_f<-function(mypath_1,plot_nub,figname){
  mypath <- file.path(mypath_1, 
                      paste(paste("plot",plot_nub, figname,sep = "_"), 
                            "tiff", sep = ".")) 
  tiff(filename =mypath, width =1920,height=2000, units = "px", res=400)
  print(eval(as.name(figname)))
  dev.off() 
}
save_f(mypath_1,"1","fig_")
getwd()


#Performing multiple regression analysis 
#with Big Five personality traits as IVs and overall time management ability as DV. 
#     This helps determine the independent impact of each personality trait on time management 
#     while controlling for other factors.

form1<- as.formula(
  paste0(
    "score ~",
    paste(col[c(grep("FFM_",col[,1])),],collapse = "+")
  )
)

mlr1 <- lm(form1,data)
summary(mlr1,round=3)

form2<- as.formula(
  paste0(
    "score ~",
    paste(col[c(grep("FFM_|AS_",col[,1])),],collapse = "+")
  ))
mlr2 <- lm(form2,data)
summary(mlr2,round=3)

form3<- as.formula(
  paste0(
    "score ~",
    paste(col[c(grep("FFM_|gender|grade|school_type|phone",col[,1])),],collapse = "+")
  ))
mlr3 <- lm(form3,data)
summary(mlr3,round=3)

#Mediation effect analysis#######
#install.packages("mediation")
library(mediation)

#Performing multiple regression analysis 
#with Big Five personality traits as IVs and overall time management ability as DV. 
#     This helps determine the independent impact of each personality trait on time management 
#     while controlling for other factors.
form1<- as.formula(
  paste0(
    "AS_0 ~",
    paste(col[c(grep("FFM_|gender|grade|school_type|phone",col[,1])),],collapse = "+")
  )
)

mlr1 <- lm(form1,data)
summary(mlr1,round=3)

form2<- as.formula(
  paste0(
    "score ~",
    paste(col[c(grep("FFM_|AS_gender|grade|school_type|phone",col[,1])),],collapse = "+")
  ))
mlr2 <- lm(form2,data)
summary(mlr2,round=3)


set.seed(123) #replicability of result
result = mediate(form1,form2,treat="CX3CL1",mediator = "Meta52",boot = T)#默认1000次抽样
#note the Chinese variable names... should have been changed earlier
summary(result)



#Mediation analysis - 1#####
library(bruceR)
X.names<- c("FFM_O_n") # name of IVs
M.names<- c("TM_PCofT_n") # name of MVs
covs.names<-c("grade_f")
Y.name <- "AS_n"

result1<- list()
k = 1
for (X.name in X.names){
  for (M.name in M.names) {
    result1[[X.name]] <- list()
    result1[[X.name]][[k]] <- PROCESS(data, y = Y.name, x = X.name,
                                 meds = M.name,
                                 covs = covs.names,
                                 ci="boot", nsim=1000, seed =1)
    k = k +1
  }
}



#Academic stress
library(Hmisc)
for(i in c(col[c(grep("TM_",col[,1])),],"score")){
  a<-cor.test(data[,"AS_n"], data[,c(i)], method=c("pearson"))
  print(paste(i,"  P value :",round(a$p.value,3),"  cor :",round(a$estimate,3) ))
}

#pathway: openness -> perceived time control -> stress








#Mediation analysis - 2#####
library(bruceR)
X.names<- c("FFM_N_n") # name of IVs
M.names<- c("TM_PCofT_n") # name of MVs
covs.names<-c()
Y.name <- "AS_n"
grades<-c(2,3,4)
result1<- list()
k = 1
for (grade in grades){
for (X.name in X.names){
  for (M.name in M.names) {
    result1[[X.name]] <- list()
    data_123<-subset(data,data$grade_f==grade)
    result1[[X.name]][[k]] <- PROCESS(data_123[,], y = Y.name, x = X.name,
                                      meds = M.name,
                                      covs = covs.names,
                                      ci="boot", nsim=1000, seed =1)
    k = k +1
  }
}}



#Academic stress
library(Hmisc)#加载包
for(i in c(col[c(grep("TM_",col[,1])),],"score")){
  a<-cor.test(data[,"AS_n"], data[,c(i)], method=c("pearson"))
  print(paste(i,"  P value :",round(a$p.value,3),"  cor :",round(a$estimate,3) ))
}

#pathway: neuroticism -> perceived time control -> stress










#Principal component analysis#####
data_1<-read_sav("~/Desktop/元培问卷数据_母版.sav")
col<-colnames(data_1)
col<-as.data.frame(col)

library(ggcorrplot)
aa<-subset(data_1,select=c(col[c(grep("TM_", col[,1])),]))

aa<-lapply(aa[1:length(aa)], as.numeric)
aa<-as.data.frame(aa)

# calculate the correlation matrix
corr<- round(cor(aa[1:length(aa)]),1)
kappa (corr,exact= TRUE )#[1]1.298785e+18    #>1000, so there is severe multicollinearity
# calculate the pvalue matrix for correlation
p.mat <- round(cor_pmat(aa),3)

#Visualizing correlation matrix [https://www.jianshu.com/p/97f3420ac0f5]
#plotting，default method = "square"
fig_cor_image<-
  ggcorrplot(corr, method = c("square"), type = c("full"), 
             ggtheme = ggplot2::theme_void,
             title = " ", 
             show.legend = T, legend.title = "", show.diag = T, 
             colors = c("#839EDB", "white", "#FF8D8D"), outline.color = "white", 
             hc.order = T, hc.method = "single", 
             lab = T, lab_col = "black",  lab_size =2, 
             p.mat = p.mat, 
             sig.level = 0.05, insig = c("blank"), pch = 4, 
             pch.col = "white", pch.cex = 8, tl.cex = 12,
             tl.col = "black", tl.srt = 45, digits = 2)+
  theme(text=element_text(family="STKaiti"),
        axis.text.x=element_text(angle=90, hjust =1,colour="black",size=8), #设置x轴刻度标签的字体显示倾斜角度（）angle，并向下调整1(hjust = 1)，字体簇为Times大小为20
        axis.text.y=element_text(size=8,hjust =1) #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
  )
scale_y_discrete(labels = labels_0 )+
  scale_x_discrete(labels = labels_0 )

fig_cor_image

#PCA
set.seed(10086)
pca = prcomp(aa, scale = T)
#visualization
#install.packages("factoextra")
library("FactoMineR")
library("factoextra")

summary(pca)
fig_pca_eig_image<-fviz_eig(pca, addlabels = TRUE)
fig_pca_eig_image
#save_f(mypath_1,"2","fig_pca_eig_image")
# Graph of the variables
fig_pca_var<-fviz_pca_var(pca, col.var = "black")
fig_pca_var

data_1$gender

#result of PCA
xx<-pca$x[,1:4]
image_pca<-as.data.frame(xx)
a<-c("PC1","PC2","PC3","PC4")
names(image_pca)<-a



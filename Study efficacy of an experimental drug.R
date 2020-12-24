#Part 1. Treatment evaluation
#Loading R Package
library(dplyr)

FIGSIZE = c(1000,1000)
#1.1. Merge all tables into a unify table 
df = rbind(df_A, df_B, df_C, df_D)

#1.2. Select only the patients in the treatment group
df = filter(df,TxGroup=="Treatment")

#1.3. Select the useful columns
df = df[c("VisitDay","PANSS_Total")]
head(df,5)

#1.4. Print out the Scatterplot
visitDay = df$VisitDay
PANSSTotal = df$PANSS_Total
png(paste("Desktop/Stanford_Summer_Session/STATS202/Final", sep = ""), width = FIGSIZE[1],height = FIGSIZE[2])
plot(x = visitDay, y = PANSSTotal, main = "PANSS Total vs Day for Treatment Group", xlab = "Day of Visit", ylab = "PANSS Total", col = "dodgerblue2")

#1.5. Obtain a Linear Fit 
linear_fit = lm(PANSSTotal~visitDay)
summary(linear_fit)
abline(linear_fit, col="red")

#1.6. Print the Gradient of Regression Line
coef(linear_fit)

#1.7. Select only the patients in the control group
df_Control = rbind(df_A,df_B,df_C,df_D)
df_Control = filter(df_Control, TxGroup =="Control")

#1.8. Select VisitDay & PANSSTotal
df_Control = df_Control[c("VisitDay","PANSS_Total")]
head(df_Control, 5)
visitDay = df_Control$VisitDay
PANSSTotal = df_Control$PANSS_Total
plot(x=visitDay,y=PANSSTotal, main = "PANSS Total vs Day For Control Group",xlab="Day of Visit",ylab = "PANSS Total", col = "dodgerblue2")

#1.9. Obtain a Linear Fit
linear_fit_Control = lm(PANSSTotal~visitDay)
summary(linear_fit_Control)
coef(linear_fit_Control)

#1.10. Checking random stuffs
df_random = rbind(df_A,df_B,df_C,df_D)
df_random = filter(df_random, TxGroup =="Treatment")
visitDay_random = df_random$VisitDay

#Part 2. Patient Segmentation
#Loading R Packages
library(logging)
library(tidyverse) #data manipulation
library(factoextra) #clustering algorithms & visual
library(cluster) #clustering algorithms
library(NbClust) #allow to check the optimal K
library("scatterplot3d")
library(car)
library(rgl)
library(FactoMineR)
library(corrplot)
library(dplyr)


#2.1. Segment by 30 PANSS symptoms
#2.1.1 Pre-processing data to select necessary variables
df_segmentation = rbind(df_A,df_B,df_C,df_D)
df_segmentation = filter(df_segmentation, VisitDay == 0)
df_segmentation = subset(df_segmentation, select = -c(Study, Country, SiteID, AssessmentID, TxGroup, LeadStatus, VisitDay, PANSS_Total))
df_segmentation = df_segmentation[!duplicated(df_segmentation$PatientID),]
df_segmentation$PatientID
rownames(df_segmentation) <- df_segmentation$PatientID
df_segmentation$PatientID <- NULL
df_segmentation = scale(df_segmentation)
head(df_segmentation)

#2.1.2. Find optimum k with Elbow Method
#Test silhouette, elbow, and gap_stat methods to define the optimal cluster
#Test silhouette method
A <- fviz_nbclust(df_segmentation, kmeans, method = "silhouette")
A
#Test Elbow method
B <- fviz_nbclust(df_segmentation, kmeans, method = "wss")
B
#Test Gap Statistic
#set nboot = 50 to ensure the function speedy
set.seed(100)
C <- fviz_nbclust(df_segmentation, kmeans, nstart=25, method = "gap_stat", nboot = 50)
#After reviewing, optimal K is 2
km.elbow = fviz_nbclust(df_segmentation, kmeans, method = "wss") + geom_vline(xintercept = 2, linetype = 5)+labs(subtitle = "Elbow method")
opt.km_result = kmeans(df_segmentation, 2, nstart=25)
min_wss = as.numeric(unlist(opt.km_result[5]))
for (i in i:20){
  curr.km_result = kmeans(df_segmentation, 2, nstart=25)
  curr_wss = as.numeric(unlist(curr.km_result[5]))
  if(curr_wss<min_wss){
    opt.km_result = curr.km_result
    min_wss = curr_wss
  }
}
fviz_cluster(opt.km_result,data=df_segmentation)

#2.2. Categorize 3 PANSS Categories
#2.2.1 Pre-processing the dataset
df_cate = bind_rows(df_A,df_B,df_C,df_D,df_E)
df_cate = filter(df_cate, VisitDay == 0)

#2.2.2. Obtain columns based on sum of symptoms in each category
#Applying rowSums function to calculate the total score of each category
P_Total <- rowSums(df_cate[,9:15],na.rm = TRUE)
N_Total <- rowSums(df_cate[,16:22],na.rm = TRUE)
G_Total <- rowSums(df_cate[,23:38],na.rm = TRUE)
#Create the table to organize the obtain result
symptom_df_cate <- data.frame(P_Total,N_Total,G_Total)

#Filter the Patient ID from the original dataset and match it with the generated table above
df_cate <- subset(df_cate,select = c(PatientID))
df_cate <- cbind(df_cate,symptom_df_cate)

#2.2.3. Check and illuminate duplicated data

A <- df_cate[!duplicated(df_cate$PatientID),]
rownames(A) <- A$PatientID
A$PatientID <- NULL

#2.2.4. Generate 3D scatterplot
scatterplot3d(x=A$P_Total,y=A$N_Total,z=A$G_Total, xlab = "P Total", ylab = "N Total", zlab = "G Total", color = "deepskyblue4")

#2.2.5. Check and realize that the optimal K is 5

#Conduct elbow test
km.elbow = fviz_nbclust(A, kmeans , method = "wss")

#Conduct silhouette test
silhouette <- fviz_nbclust(A, kmeans, method = "silhouette")

#Conduct Gap Stats test
set.seed(100)
gap_stats <- fviz_nbclust(A, kmeans, nstart=25, method = "gap_stat", nboot = 50)

#2.2.6. Categorize the dataset
opt.km_result <- kmeans(A,5,nstart = 25)

min_wss <- as.numeric(unlist(opt.km_result[5]))

for(i in i:20){
  curr.km_result <- kmeans(A,5,nstart = 25)
  curr_wss <- as.numeric(unlist(opt.km_result[5]))
  if(curr_wss < min_wss){
    opt.km_result = curr.km_result
    min_wss = curr_wss
  }
}

#2.2.7. Create a new colum - cluster
A$cluster <- factor(opt.km_result$cluster)

#2.2.8. Visualize the dataset by using scatterplot3d; x-axis: P Total, y-axis: N Total, z-axis: G Total, and colour-rule: cluster
scatterplot3d(x=A$P_Total,y=A$N_Total,z=A$G_Total,xlab = "P Total",ylab = "N Total",zlab = "G Total", color = rainbow(5)[A$cluster])

#2.3. Segmentation by PCA Analysis and K-means

#2.3.1. Pre-processing data to select crucial variables
df_pca = rbind(df_A,df_B,df_C,df_D)
df_pca = filter(df_pca,VisitDay==0)
df_pca = subset(df_pca, select = -c(Study,Country,SiteID, RaterID, AssessmentID, TxGroup,LeadStatus, VisitDay,PANSS_Total))
df_pca = df_pca[!duplicated(df_pca$PatientID),]
rownames(df_pca) <- df_pca$PatientID
df_pca$PatientID <- NULL
df_pca <- data.frame(scale(df_pca))

#2.3.2. Create the correlation matrix
corr <- cor(df_pca)
corrplot(corr,type = "upper",order = "hclust",t1.col = "black",t1.srt=45)

#2.3.3. Perform PCA - Principle Components Analysis
pc <- princomp(df_pca)
plot(pc,col="dodgerblue3")

#2.3.4. Select the .# of pc reaching 60% of the total variance
pc <- prcomp(df_pca)

var_list <- summary(pc)$importance[2,]
pc_length <- 0
cur_var_prop <- 0
for (i in 1:length(var_list)){
  cur_var_prop = cur_var_prop + var_list[i]
  if(cur_var_prop>=.6){
    pc_length = i
    break
  }
}

#2.3.5. Select the 1st i pc to represent more than 60% variance 
sel_comp = data.frame(pc$x[,1:i])
plot(sel_comp)

#2.3.6. Identify the optimal K
#Conduct elbow test - optimal K is 5
D = fviz_nbclust(sel_comp, kmeans , method = "wss")+geom_vline(xintercept = 5,linetype=2)
#Conduct silhouette test - optimal K is 2
E <- fviz_nbclust(sel_comp, kmeans, method = "silhouette")
#Conduct Gap Stats test - optimal K is 1
set.seed(100)
G <- fviz_nbclust(sel_comp, kmeans, nstart=25, method = "gap_stat", nboot = 50)

#2.3.7. Apply K = 5
opt.km_result = kmeans(sel_comp,5,nstart = 25)
plot(sel_comp,col=opt.km_result$cluster)

#2.3.8. Split the dataset into separate dataframe corresponding to 5 clusters

#2.3.9. Create the table to store the data
df_pca_cluster <- data.frame(opt.km_result$cluster)

#2.3.10. Filter the data to categorize the dataset
df_pca_C1 <- filter(df_pca_cluster,opt.km_result.cluster==1)
df_pca_C2 <- filter(df_pca_cluster,opt.km_result.cluster==2)
df_pca_C3 <- filter(df_pca_cluster,opt.km_result.cluster==3)
df_pca_C4 <- filter(df_pca_cluster,opt.km_result.cluster==4)
df_pca_C5 <- filter(df_pca_cluster,opt.km_result.cluster==5)

#2.3.11. Filter into main dataframe to obtain the symptom scores
df_pca_C1_1 <- subset(df_pca,rownames(df_pca)%in% rownames(df_pca_C1))
df_pca_C2_2 <- subset(df_pca,rownames(df_pca)%in% rownames(df_pca_C2))
df_pca_C3_3 <- subset(df_pca,rownames(df_pca)%in% rownames(df_pca_C3))
df_pca_C4_4 <- subset(df_pca,rownames(df_pca)%in% rownames(df_pca_C4))
df_pca_C5_5 <- subset(df_pca,rownames(df_pca)%in% rownames(df_pca_C5))

#2.3.12. Obtain the means of all symptom scores in each cluster
res.means_df_pca <- rbind(sapply(df_pca_C1_1,mean),sapply(df_pca_C2_2,mean),sapply(df_pca_C3_3,mean),sapply(df_pca_C4_4,mean),sapply(df_pca_C5_5,mean))
rownames(res.means_df_pca) <- c("C1 Mean","C2 Mean","C3 Mean","C4 Mean","C5 Mean")
print(res.means_df_pca)

#Part 3. Binary Classification
#Loading R Packages
library(logging)
library(tree)
library(gbm)
library(glmnet)
library(randomForest)
library(class)
library(data.table)
library(mltools)
library(MLmetrics)

#3.1. Pre-processing the dataset
df_class <- rbind(df_A,df_B,df_C,df_D)

#3.2. Classify 1: "Flagged or CS", and 2: "Passed"
df_class$LeadStatus[df_class$LeadStatus == "Assign to CS"] = 1
df_class$LeadStatus[df_class$LeadStatus == "Flagged"] = 1
df_class$LeadStatus[df_class$LeadStatus == "Passed"] = 0

#3.3. Filter the dataset
df_class <- subset(df_class, select=-c(Study,PatientID,Country,SiteID,RaterID,AssessmentID))
df_class$TxGroup = as.factor(df_class$TxGroup)

#3.4. Pre-processing df_E dataset
df_class_E = subset(df_E,select=-c(Study,PatientID,Country,SiteID,RaterID,AssessmentID))
df_class_E$TxGroup <- as.factor(df_E$TxGroup)

#3.5. Assign -1 as LeadStatus of df_class_E
df_class_E$LeadStatus <- replicate(nrow(df_E),-1)

#3.6. Split 80% the original data for training
smp_size <- .8*nrow(df_class)

#3.7. Assign data into train_index randomly
train_index <- sample(nrow(df_class),smp_size)
data.train <- df_class[train_index,]
data.test <- df_class[-train_index,]

#3.8. Using Boosting to fit the dataset

#3.9. Ccross-validation 10
boost_fit <- gbm(LeadStatus~.,data.train,cv.folds = 10,bag.fraction = 0.75,distribution = "bernoulli",n.trees = 1000,shrinkage = 0.05)

#3.10. Predict on response
boost_prob <- predict(boost_fit,data.test,n.trees = 1000,type ="response")

#3.11. If boost_prob >0.5, assign 1, else assign 0
boost_pred <- ifelse(boost_prob >0.5,1,0)

#3.12. Create false type
table(data.test$LeadStatus,boost_pred)

#3.13. Accuracy rate 77.9%
boost_acc <- sum(data.test$LeadStatus==boost_pred)/nrow(data.test)

#3.14. Using Logistic regression to fit the dataset
log_fit <- glm(as.factor(LeadStatus)~.,data.train,family = binomial())
log_prob <- predict(log_fit,data.test,type = "response")

#3.14. if log_prob >0.5, assign 1, else assign 0
log_pred <- ifelse(log_prob>.5,1,0)

#3.15. Review false type
table(data.test$LeadStatus,log_pred)

#3.16. Accuracy rate 76.46%
log_acc <- sum(data.test$LeadStatus==log_pred)/nrow(data.test)

#3.17. Plots the test accuracy againts mtry for Random Forest
num_pred <- ncol(data.train)-1
test_acc <- 1:num_pred

for (i in 1:num_pred){print(paste("iteration:", i, "/",num_pred))
  rf_fit <- randomForest(as.factor(LeadStatus)~.,data.train,mtry=i,ntree=1000)
  rf_prob <- predict(rf_fit,data.test,ntree=1000,type="prob")
  rf_prob <- rf_prob[,2]
  rf_pred <- ifelse(rf_prob>.5,1,0)
  rf_acc <- sum(data.test$LeadStatus==rf_pred)/nrow(data.test)
  print(paste("rf accuracy:",rf_acc))
  test_acc[i] <- rf_acc
}
  

plot(1:num_pred,test_acc,type = "b",xlab = "mtry",ylab = "Test Accuracy")

#3.18. Fit using Random Forest
rf_fit <- randomForest(as.factor(LeadStatus)~.,data.train,ntree=1000,max.depth=5,min.node.size=10,splitrule=Gini,replace=TRUE,sampsize=nrow(data.train),importance=TRUE,mtry=7)
rf_prob <- predict(rf_fit,data.test,ntree=1000,type = "prob")
rf_pred <- ifelse(rf_prob>.5,1,0)
rf_acc <- sum(data.test$LeadStatus==rf_pred)/nrow(data.test)
paste("Random Forest accuracy:",rf_acc)

#3.19. Boosting final fit on the df_E
fin_boost_fit <- gbm(LeadStatus~.,df_class,cv.folds = 10,bag.fraction = 0.75,distribution = "bernoulli",n.trees = 1000,shrinkage = .05) 
fin_boost_prob <- predict(fin_boost_fit,df_E,n.trees=1000,type="response")
write.csv(fin_boost_prob,file = paste("boost_result.csv",sep = ""),row.names = TRUE)

#3.20. Random Forest Final Fit on Study E
fin_rf_fit <- randomForest(as.factor(LeadStatus) ~., data =df_class, ntree =1000 , max.depth =5,min.node.size =10,splitrule =Gini,replace =TRUE ,sampsize = nrow(data.train),importance = TRUE)
fin_rf_prob <- predict(fin_rf_fit,df_class_E,n.tree=1000,type="prob")
write.csv(fin_rf_prob,file=paste("rf_result.csv",sep = ""),row.names = TRUE)

#Data Science Assignment 2 
pulsar <- read.csv("C:/Users/Dewan Arun Singh/Downloads/DataSets/predicting-a-pulsar-star/pulsar_stars.csv")
library(knitr)
library(dplyr)
library(ggplot2)
library(e1071)
library(tree)
library(reshape2)

error.vector = 0 # Is later used to store the errors for the models in testing and classification
#########################################################################################################
#                                 Data Exploration & Cleaning                                           #
#########################################################################################################

#Giving more readable column names
colnames(pulsar)[1:8] <- c("MIP", "STD_IP", "KIP", 
                           "SKW_IP", "M_DM", "STD_DM", "K_DM", "SKW_DM")
#check structure of the dataset
pulsar$target_class[pulsar$target_class == 0] = "Not Pulsar"
pulsar$target_class[pulsar$target_class == 1] = "Pulsar"
pulsar$target_class = as.factor(pulsar$target_class)
str(pulsar)

#check for NA
any(is.na(pulsar))



#Splitting the data set into 3 portions: Training, Validation and Test dataset.
#  
set.seed(19884240)
tr = sample(dim(pulsar)[1],(0.6 * (dim(pulsar)[1])))
train = pulsar[tr, ]
vld = setdiff(1:nrow(pulsar), tr)
tst = sample(vld, size = (length(vld)/2))
vld1 = setdiff(vld, tst)
validation = pulsar[vld1, ]
test = pulsar[tst,]

#Checking if the probability of finding a true value is consistent through original,
# training, validation and test datasets.
length(which(pulsar$target_class=="Pulsar"))/length(pulsar$target_class) #Original
length(which(train$target_class=="Pulsar"))/length(train$target_class) #Training 
length(which(validation$target_class=="Pulsar"))/length(validation$target_class) #Validation
length(which(test$target_class=="Pulsar"))/length(test$target_class) # Testing

#Changing the target_class to character factor variable
#target = as.character(pulsar$target_class)
#target[target=="0"] = "Not Pulsar"
#target[target=="1"] = "Pulsar"

########################################################################################
######################    Training set exploratory plotting    #########################
########################################################################################
g <- ggplot(train)
#box plot for MIP

g +aes(target_class, MIP) + geom_boxplot(aes(col = target_class))

#box plot for STD_MIP
g +aes(target_class, STD_IP) + geom_boxplot(aes(col = target_class))

#box plot for KIP
g +aes(target_class, KIP) + geom_boxplot(aes(col = target_class))

#box plot for SKW_IP

g +aes(target_class, SKW_IP) + geom_boxplot(aes(col = target_class))

#box plot for M_DM
g +aes(target_class, M_DM) + geom_boxplot(aes(col = target_class))

#box plot for STD_DM
g +aes(target_class, STD_DM) + geom_boxplot(aes(col = target_class))

#box plot for K_DM
g +aes(target_class, K_DM) + geom_boxplot(aes(col = target_class))

#box plot for SKW_DM
g +aes(target_class, SKW_DM) + geom_boxplot(aes(col = target_class))


#####################################   Heat Map    #####################################################

cormat = round(cor(train[,-dim(train)[2]]), 2)
head(cormat)
melted_cormat = melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1,y=Var2, fill = value)) + geom_tile()

#Function to get the upper half of the heat map
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

upper_cormat <- get_upper_tri(cormat = cormat)

#Melt the co-relation matrix
melted_cormat_upper = melt(upper_cormat, na.rm = TRUE)

#Heat Map
ggplot(data = melted_cormat_upper, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white",
            midpoint = 0, limit = c(-1,1), space = "lab", name = "Pearson\nCoorelation") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, color = "black", size = 10, vjust = 0.5, hjust = 0.5)) + 
  coord_fixed() + labs(x = "All Variables in Dataset", y = "All Variables in Dataset", 
                         title = "HEAT MAP") 

#Hierarchical Clustering of the co-relation matrix
reorder_cormat <- function(cormat){
  #Use coorelation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  print(hc)
   cormat <- cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat = cormat)
upper_cormat <- get_upper_tri(cormat = cormat)
melted_cormat_upper = melt(upper_cormat, na.rm = TRUE)

ggheatmap <- ggplot(data = melted_cormat_upper, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                                    midpoint = 0, limit = c(-1,1), space = "lab", name = "Pearson\nCoorelation") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, color = "black", size = 10, vjust = 0.5, hjust = 0.5)) + 
  coord_fixed() + labs(x = "All Variables in Dataset", y = "All Variables in Dataset", 
                       title = "HEAT MAP")

print(ggheatmap)

#Final heat map

ggheatmap <- ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

print(ggheatmap)

################################# Co-relation Matrix with target_class #################################

#melted.train = melt(train, id.vars = "target_class")
#head(melted.train)
#ggplot(data = melted.train, aes(x = target_class, y = variable, fill = value)) + 
#  geom_tile(aes(color = factor(target_class))) + 
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#            midpoint = 90, limit = c(-20,200), space = "lab", name = "Median value\nof Variable") +
#  theme_minimal() +
#  theme(axis.title.x = element_blank(),
#        axis.title.y = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank(),
#       axis.ticks = element_blank())


df = GroupedBarPlots_df(train)
df1 = GroupedBarPlots_df_sd(train)
df2 = GroupedBarPlots_df_median(train)

ggplot(data = df, aes(x = Var3, y = mean, fill = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Target Class", title = "Comparison of Mean of All Variables Grouped By Target Class")+
  theme_minimal()
  
ggplot(data = df1, aes(x = Var3, y = std, fill = Var1)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Target Class", title = "Comparison of Standard Deviation of All Variables Grouped By Target Class") +
theme_minimal()

ggplot(data = df, aes(x = Var1, y = mean, group = Var3)) + 
  geom_line(aes(color = Var3), size = 1.5)+
  geom_point() +
  labs(x = "All Variables in the Dataset", y = "Mean",
       title = "Comparison of Mean of All Variables Grouped By Target Class")+
  scale_color_discrete(name = "Classification") +
  theme_minimal() 
  

ggplot(data = df1, aes(x = Var1, y = std, group = Var3)) + 
  geom_line(aes(color = Var3), size = 1.5)+
  geom_point() +
  labs(title = "Comparison of Standard Deviation of All Variables Grouped By Target Class", 
       x = "All variables", y = "Standard Deviation")+
  scale_color_discrete(name = "Classification") +
  theme_minimal()

ggplot(data = df2, aes(x = Var1, y = median, group = Var3)) + 
  geom_line(aes(color = Var3), size = 1.5)+
  geom_point() +
  labs(title = "Comparison of Median of All Variables Grouped By Target Class", 
       x = "All variables", y = "Median")+
  scale_color_discrete(name = "Classification") +
  theme_minimal()
########################################    MODELLING   ###############################################
#Logistic Regression
model.logistic = glm(target_class~., family = "binomial", data = train)
model.logistic1 = glm(target_class~MIP + STD_IP + 
                      KIP + SKW_IP + M_DM + STD_DM , family = "binomial", data = train)
summary(model.logistic1)
anova(model.logistic1)

model.logistic2 = glm(target_class~MIP +
                        KIP +
                        SKW_IP + 
                        M_DM + STD_DM, family = "binomial", data = train)
summary(model.logistic2)
anova(model.logistic2)

glm.train.prob = predict(model.logistic2, type = "response")
glm.pred.train = rep("Not Pulsar", length(glm.train.prob))
glm.pred.train[glm.train.prob > 0.5] = "Pulsar"
error.vector[1] = misclass.rate(table(glm.pred.train, train$target_class))

glm.prob <- predict(model.logistic2, validation, type = "response")
glm.pred <- rep("Not Pulsar", length(glm.prob))
glm.pred[glm.prob > 0.5] = "Pulsar"
error.vector[2] = misclass.rate(table(glm.pred, validation$target_class))

ggplot(data = model.logistic2, aes(MIP,fitted.values(model.logistic2))) + 
  geom_point(aes(col = target_class), size = 2, alpha = 1/2)
  


#Classification Tree
tree_model = tree(target_class~., data = train)
plot(tree_model)
summary(tree_model)
text(tree_model, pretty = 1)
cv_tree_model = cv.tree(tree_model, FUN = prune.misclass)
prune_tree_model = prune.misclass(tree_model, best = 2)
summary(prune_tree_model)

#Training Misclassification rate
tree_pred_train = predict(tree_model, type="class")
error.vector[3] = misclass.rate(table(tree_pred_train, train$target_class))

#validation Classification Decision Tree
tree_pred = predict(tree_model, validation, type = "class")
error.vector[4] = misclass.rate(table(tree_pred, validation$target_class))

#SVM
str(pulsar)
svm_model = svm(target_class~., data = train, kernel = "linear", cost = 10, scale = TRUE)
plot(svm_model, pulsar, KIP~SKW_IP)
svm.tune = tune(svm, target_class~., data = train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))

svm_predict_train = predict(svm_model,train, type = "class")
error.vector[5] = misclass.rate(table(svm_predict_train, train$target_class))
svm_predict = predict(svm_model,test, type = "class")
error.vector[6] = misclass.rate(table(svm_predict,test$target_class))

########################################    PCA    ######################################################

train.pca = prcomp(train[,-length(train)], scale. = TRUE)
summary(train.pca)
str(train.pca)

plot(train.pca, type = "l")
biplot(train.pca, scale = 0)

train2 <- cbind(train, train.pca$x[,1:2])
head(train2)

ggplot(data = train2, mapping = aes(PC1, PC2, col = target_class, fill = target_class)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") + theme_minimal()

cor(train[,-length(train)], train.pca$x[,1:2])  

##################################  K Means CLustering    ##############################################
set.seed(19884240)

km.res = kmeans(train[,-length(train)], 2, nstart = 50)
summary(km.res)
print(km.res)

#Visualization
library(factoextra)
fviz_cluster(km.res, data = train[,-9])

#############################    Random Forest Classifier     ###################################
library(randomForest)
forestModel = randomForest(target_class~., data = train, proximity = TRUE)
summary(forestModel)

#Plotting error rate
err.data = data.frame(Trees = rep(1:nrow(forestModel$err.rate), times = 3),
                      Type = rep(c("OOB","Not Pulsar", "Pulsar"), each = nrow(forestModel$err.rate)),
                      Error = c(forestModel$err.rate[,"OOB"],
                                forestModel$err.rate[,"Not Pulsar"],
                                forestModel$err.rate[,"Pulsar"]))
ggplot(data = err.data, mapping = aes(x = Trees, y = Error))+
  geom_line(aes(color = Type))

#Training misclassification Rate
forestModel_pred_train = predict(forestModel, type = "class")
error.vector[7] = misclass.rate(table(forestModel_pred_train, train$target_class))
#Validation Misclassification Rate
forestModel_pred = predict(forestModel, validation, type = "class")
error.vector[8] = misclass.rate(table(forestModel_pred, validation$target_class))
############################    Error Comparison    #############################################
errordf = data.frame(error = (error.vector *100),
                     accuracy = (100 - (error.vector*100)),
                     model = rep(c("Logistic", "Tree", "SVC", "Forest"), each = 2),
                     Type = rep(c("Training","Validation"), times = 4))
str(errordf)
ggplot(data = errordf, mapping = aes(x = model,y = error, group = Type)) +
  geom_line(aes(col = Type)) +
  geom_point() +
  labs(title = "Model versus Error Percentage", y = "Error(in %age)" ) +
  theme_minimal()

ggplot(data = errordf, mapping = aes(x = model,y = accuracy, group = Type)) +
  geom_line(aes(col = Type)) +
  geom_point() +
  labs(title = "Model versus Accuracy Percentage", y = "Accuracy(in %age)" ) +
  theme_minimal()

###########################   Final Test Run    ################################################
forestModel_pred_test = predict(forestModel, test, type = "class")
table(forestModel_pred_test, test$target_class)
misclass.rate(table(forestModel_pred_test, test$target_class))

x = predict(model.logistic2, test, type = "response")
X = rep("Not Pulsar", length(x))
X[x>0.5] = "Pulsar"
table(X, test$target_class)
misclass.rate(table(X, test$target_class))





################################################################################################
Y = replicate(1000,{
  sample.trgt = sample(1:nrow(validation), replace = TRUE)
  y = predict(forestModel, validation[sample.trgt,], type = "class")
  misclass.rate(table(y, validation[sample.trgt,]$target_class))
})

y = replicate(1000,{
  sample.trgt = sample(1:nrow(test), replace = TRUE)
  y = predict(forestModel, test[sample.trgt,], type = "class")
  misclass.rate(table(y, test[sample.trgt,]$target_class))
})
  
z = replicate(1000,{
  sample.trgt = sample(1:nrow(test), replace = TRUE)
  x = predict(model.logistic2, test[sample.trgt,], type = "response")
  X = rep("Not Pulsar", length(x))
  X[x>0.5] = "Pulsar"
  misclass.rate(table(X, test$target_class))
})



# Self Study

pred = predict(model.logistic2, train, type = "response")
samp = sample(which(pred < 0.25),400)
df = data.frame(MIP = train[samp,]$MIP, 
                pred = pred[samp])

for(i in 0:3){
  samp2 = sample(which((pred > (0.25 * i)) & (pred < (0.25) * i + 1)), 400)
  df1 = data.frame(MIP = train[samp2,]$MIP, 
                   pred = pred[samp2])
  
  df = rbind(df, df1)
}

df$MIP = seq(from = 160/dim(df)[1] , to = 160, by = 160/dim(df)[1])
df$pred = sort(df$pred, decreasing = TRUE)

ggplot(data = model.logistic2, aes(MIP,fitted.values(model.logistic2))) + 
  geom_point(aes(col = target_class), size = 2, alpha = 1/2) +
  geom_line(data = df, mapping = aes(MIP,pred), 
            size = 0.8, stat = "identity", position = "identity")



###########################linear model####################################

linearModel = lm(MIP~STD_IP+KIP+SKW_IP , data = train)
summary(linearModel)

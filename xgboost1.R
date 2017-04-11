library(mice)
library(caTools)
library(xgboost)


train = read.csv("Train.csv")
test = read.csv("Test.csv")

train_new = train[, !(names(train) %in% c("Item_Outlet_Sales"))]
all = rbind(train_new, test)

all$Item_Identifier = NULL
all$Item_Fat_Content[all$Item_Fat_Content=='reg'] = "Regular"
all$Item_Fat_Content[all$Item_Fat_Content=='LF' | all$Item_Fat_Content=='low fat'] = "Low Fat"
all$Item_Fat_Content = droplevels(all$Item_Fat_Content)
all$Num_years = 2013 - all$Outlet_Establishment_Year
all$Outlet_Establishment_Year = NULL
all$Is_Low_Fat = 0
all$Is_Low_Fat[all$Item_Fat_Content== 'Low Fat']=1
all$Item_Fat_Content=NULL
all$Outlet_Size[all$Outlet_Size==""] = "Medium"
all$Outlet_Size = droplevels(all$Outlet_Size)
all = cbind(all,with(all, data.frame(model.matrix(~Outlet_Size-1, all))))
all$Outlet_Size=NULL
all = cbind(all,with(all, data.frame(model.matrix(~Outlet_Location_Type-1, all))))
all = cbind(all,with(all, data.frame(model.matrix(~Outlet_Type-1, all))))
all$Outlet_Identifier = NULL
all$Outlet_Size = NULL
all$Outlet_Location_Type = NULL
all$Outlet_Type = NULL
all = cbind(all,with(all, data.frame(model.matrix(~Item_Type-1, all))))
all$Item_Type = NULL



imp <- mice(all, m =5, method ="pmm")
all = complete(imp)


train_new = all[0:nrow(train_new),]
test_new = all[(nrow(train_new) + 1):nrow(all),]
train_new = cbind(train_new, train$Item_Outlet_Sales)
train_new$Item_Outlet_Sales = train_new$`train$Item_Outlet_Sales`
train_new$`train$Item_Outlet_Sales` = NULL


split = sample.split(train_new$Item_Outlet_Sales, SplitRatio = 0.8)
train_final = subset(train_new, split==TRUE)
validation_final = subset(train_new, split==FALSE)

set.seed(1000)
param <- list(
objective = 'reg:linear',  # --linear regression / output: value
eta = 0.1,
gamma = 1,
eval_metric = 'rmse' ,
min_child_weight = 4,
max_depth = 20,
subsample = 0.85,
colsample_bytree = 0.5,
max_delta_step = 20
)
round = 1500
xgModel1 <- xgb.train(param, xgb.DMatrix(as.matrix(train_new[,-1]), label=train_new$Item_Outlet_Sales), round)
XG_Test = xgb.DMatrix(as.matrix(test_new))
pred1 <- predict(xgModel1, XG_Test)
pred1[pred1<0]=0


test1 = test
test1$Item_Outlet_Sales = pred1
write.csv(test1[c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")], "sub1_xgb_all.csv", row.names = FALSE)


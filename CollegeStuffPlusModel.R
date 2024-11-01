library(tidyverse)
library(Boruta)
library(randomForest)
library(xgboost)
library(Matrix)
library(caret)

# Pull Data
#data <- read_csv("Data/2024 Full Season Data - Sheet1 copy.csv")

pred_data = read.csv("Data/2024 Full Season Data - Sheet1 copy.csv")

data_all = list.files("Data/ImportData", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv)
dataF = lapply(data_all, function(df) {
  df %>% mutate(across(AwayTeamForeignID, as.character)) %>% 
    mutate(across(HomeTeamForeignID, as.character))
})
data = bind_rows(dataF)

# Data Cleaning ----

# gives list of column names and numbers to find n
colnames(data) %>% data.frame()

# remove unneeded columns, combine balls and strikes to make a count column
dataA = data %>%
  .[-c(3,7,11,14,28,60:75,80:167)] %>% 
  mutate(
    Count = paste(Balls,Strikes, sep = "-"),.after = Strikes,.keep = "unused") 

# Combine common pitch call and hit types into single descriptors
dataA$Call = NA
dataA$HitType = NA
dataA = dataA %>% relocate(Call,.after = PitchCall)
dataA = dataA %>% relocate(HitType,.after = Call)
dataA$Call[grepl("FoulBall",dataA$PitchCall)] = "strike"
dataA$Call[grepl("FoulBallNotFieldable",dataA$PitchCall)] = "strike"
dataA$Call[grepl("FoulBallFieldable",dataA$PitchCall)] = "strike"
dataA$Call[grepl("StrikeCalled",dataA$PitchCall)] = "strike"
dataA$Call[grepl("StrikeSwinging",dataA$PitchCall)] = "strike"
dataA$Call[grepl("BallCalled",dataA$PitchCall)] = "ball"
dataA$Call[grepl("BallinDirt",dataA$PitchCall)] = "ball"
dataA$Call[grepl("InPlay",dataA$PitchCall)] = "bip"
dataA$HitType[grepl("Popup",dataA$TaggedHitType)] = "FB"
dataA$HitType[grepl("FlyBall",dataA$TaggedHitType)] = "FB"
dataA$HitType[grepl("LineDrive",dataA$TaggedHitType)] = "LD"
dataA$HitType[grepl("GroundBall",dataA$TaggedHitType)] = "GB"
dataA$TaggedPitchType[grepl("Undefined",dataA$TaggedPitchType)] = NA

# combine tagged and auto pitches to form one column
dataB = dataA %>% 
  mutate(
    PitchType = NA,
    PitchComb = coalesce(TaggedPitchType, AutoPitchType),
    .after = Count
  )

# list of unique pitches in combined column
unique(dataB$PitchComb) %>% data.frame()

# Combine common pitch types into single descriptors 
# using closest pitch category (ie sweeper = slider)
dataB$PitchType[grepl('Fastball', dataB$PitchComb)] = 'FB'
dataB$PitchType[grepl('OneSeamFastBall', dataB$PitchComb)] = 'FB'
dataB$PitchType[grepl('FourSeamFastBall', dataB$PitchComb)] = 'FB'
dataB$PitchType[grepl('Sinker', dataB$PitchComb)] = 'FB'
dataB$PitchType[grepl('Cutter', dataB$PitchComb)] = 'FB'
dataB$PitchType[grepl('Four-Seam', dataB$PitchComb)] = 'FB'
dataB$PitchType[grepl('TwoSeamFastBall', dataB$PitchComb)] = 'FB'
dataB$PitchType[grepl('ChangeUp', dataB$PitchComb)] = 'CH'
dataB$PitchType[grepl('Changeup', dataB$PitchComb)] = 'CH'
dataB$PitchType[grepl('Slider', dataB$PitchComb)] = 'SL'
dataB$PitchType[grepl('Curveball', dataB$PitchComb)] = 'CB'
dataB$PitchType[grepl('Splitter', dataB$PitchComb)] = 'SP'
dataB$PitchType[grepl('Knuckleball', dataB$PitchComb)] = 'KC'

unique(dataB$PitchType) %>% data.frame()


# Stuff Plus ----

# wOBA for ball vs strike in all possible counts
count_values = data.frame(
  Count = c('3-0','3-1','2-0','3-2','1-0','2-1','0-0','1-1','2-2','0-1','1-2','0-2'),
  strike_rv = c(-0.117,-0.066,-0.062,-0.294,-0.035,-0.069,-0.037,-0.054,-0.209,-0.051,-0.171,-0.150),
  ball_rv = c(0.051,0.168,0.143,0.234,0.088,0.064,0.032,0.048,0.085,0.024,0.038,0.021)
)

# wOBA for GB, LD, FB in different counts
outcome_values = data.frame(
  Count = c('3-0','3-1','2-0','3-2','1-0','2-1','0-0','1-1','2-2','0-1','1-2','0-2'),
  GB_rv = c(-0.314,-0.197,-0.171,-0.131,-0.109,-0.107,-0.074,-0.061,-0.046,-0.038,-0.008,0.013),
  LD_rv = c(0.045,0.162,0.188,0.228,0.250,0.252,0.285,0.298,0.313,0.321,0.351,0.372),
  FB_rv = c(-0.212,-0.095,-0.069,-0.029,-0.007,-0.005,0.028,0.041,0.056,0.064,0.094,0.115)
)

# join count values to original data frame and apply run values
df = dataB %>% 
  left_join(count_values, by = "Count") %>% 
  mutate(RV_Call = ifelse(!(Call %in% c("ball", "strike")),NA,
                     ifelse(Call == "ball", ball_rv, strike_rv)), 
         .after = Count) %>% 
  relocate(ball_rv, .after = Count) %>% 
  relocate(strike_rv, .after = ball_rv) 

# join outcome values to original data frame and apply run values
df = df %>% 
  left_join(outcome_values, by = "Count") %>% 
  mutate(RV_Play = ifelse(HitType == "GB", GB_rv,
                          ifelse(HitType == "LD", LD_rv,
                                 ifelse(HitType == "FB", FB_rv, NA))), 
         .after = RV_Call) %>% 
  relocate(GB_rv, .after = strike_rv) %>% 
  relocate(LD_rv, .after = GB_rv) %>% 
  relocate(FB_rv, .after = LD_rv)

# add run values for count and outcome to one column
df = df %>% 
  mutate(RV = coalesce(RV_Call, RV_Play), .after = RV_Play)

# take FB for each pitcher and create columns for velo, vmov, and hmov
p_avgs = df %>%
  filter(PitchType == "FB") %>% 
  group_by(Pitcher) %>% 
  summarise(avg_velo = mean(RelSpeed, na.rm = T),
            avg_vmov = mean(InducedVertBreak, na.rm = T),
            avg_hmov = mean(HorzBreak, na.rm = T))

# join fb average data frame to original data frame
dfA = df %>% 
  left_join(p_avgs, by = "Pitcher") %>% 
  relocate(avg_velo, .after = RelSpeed) %>% 
  relocate(avg_vmov, .after = InducedVertBreak) %>% 
  relocate(avg_hmov, .after = HorzBreak)

# calculate velo diff, vdiff, and hdiff based on fb average data for each
# offspeed pitch
dfB = dfA %>% 
  mutate(
    velo_diff = ifelse(
    PitchType %in% c("SL","CH", "CB", "SP", "KC"),
    RelSpeed - avg_velo, NA),
    vdiff = ifelse(
    PitchType %in% c("SL","CH", "CB", "SP", "KC"),
    InducedVertBreak - avg_vmov, NA),
    hdiff = ifelse(
    PitchType %in% c("SL","CH", "CB", "SP", "KC"),
    HorzBreak - avg_hmov, NA)
  ) %>% 
  relocate(velo_diff, .after = avg_velo) %>% 
  relocate(vdiff, .after = avg_vmov) %>% 
  relocate(hdiff, .after = avg_hmov)

fbSub = filter(dfB, PitchType == "FB",
               !is.na(RV),
               !is.na(RelSpeed),
               !is.na(SpinRate),
               !is.na(InducedVertBreak),
               !is.na(Extension)
               )

rr_FB = filter(fbSub, PitcherThrows == "Right" & BatterSide == "Right")

indicies = sample(1:nrow(rr_FB), nrow(rr_FB)*.05)

rrFB_sample = rr_FB[indicies,]

Boruta_FB = Boruta(RV ~ RelSpeed + SpinRate + Extension + VertApprAngle + 
                     HorzApprAngle + RelHeight + RelSide + pfxx + pfxz, 
                   data = rrFB_sample)

print(Boruta_FB)

plot(Boruta_FB)

fb_fs = attStats(Boruta_FB) %>% data.frame() %>% select(meanImp, decision)
fb_fs = fb_fs[order(fb_fs$meanImp, decreasing = T),]
print(fb_fs)


# Modeling ----

fb_data_random = fbSub[sample(1:nrow(fbSub), nrow(fbSub)*.1 , replace = F),]

fb_train = fb_data_random[1:round(nrow(fb_data_random)*.7),]
  
fb_test = fb_data_random[1:round(nrow(fb_data_random)*.3),]

model = randomForest(RV ~ RelSpeed + SpinRate + Extension + VertApprAngle +
                       HorzApprAngle + RelHeight + RelSide + pfxx + pfxz,
                     data = fb_train)



daf = fbSub %>% 
  mutate(prediction = predict(model, newdata = fbSub), .after = RV)

sqrt(mean((daf$RV - daf$prediction)^2))

train_pred = predict(model, newdata = fb_train)

test_pred = predict(model, newdata = fb_test)

train_rmse = sqrt(mean((fb_train$RV - train_pred)^2))

test_rmse = sqrt(mean((fb_test$RV - test_pred)^2))

leaderboard = 
daf %>% 
  group_by(Pitcher) %>% 
  summarise(Pitches = n(),
            PQ = -100*sum(prediction)/Pitches)


pred_data_fb = filter(pred_data, TaggedPitchType == "Fastball" )

pred_data_fb = pred_data_fb %>% 
  mutate(prediction = predict(model, newdata = pred_data_fb), .after = Strikes)

pred_data_fb %>% 
  group_by(Pitcher) %>% 
  summarise(Pitches = n(),
            PQ = -100*sum(prediction)/Pitches)


# Off Speed ----
os_subsdecreasing = os_subset = filter(dfB, TaggedPitchType %in% c("Slider", "ChangeUp", "Curveball")) %>% 
  drop_na(InducedVertBreak, SpinRate, RV)

Boruta_OS = Boruta(RV ~ RelSpeed + SpinRate + Extension + InducedVertBreak + 
                     HorzBreak + RelHeight + 
                     RelSide + PlateLocHeight + PlateLocSide + velo_diff +
                     vdiff + hdiff
                     , data = os_subset)

print(Boruta_OS)

plot(Boruta_OS)

attStats(Boruta_OS)

# XG Boost ----

a = select(fb_data_random, RV, RelSpeed,
           VertApprAngle, HorzApprAngle, SpinRate, RelHeight, RelSide, Extension, 
           pfxx, pfxz)


X = as.matrix(a[, !names(a) %in% 'RV'])
y = a$RV

set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

tune_grid <- expand.grid(
  nrounds = 100,
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.7, 0.8, 1.0)
)

# Train model with cross-validation
set.seed(42)
xgb_tune <- train(
  X_train, y_train,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tune_grid
)

# Best parameters found by caret
best_params <- xgb_tune$bestTune
print(best_params)

# Parameters
params <- list(
  objective = "reg:squarederror",  # Use "binary:logistic" for classification
  eval_metric = "rmse",
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  subsample = best_params$subsample
)

# Train the model
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_params$nrounds,
  watchlist = list(val = dtest),
  early_stopping_rounds = 10
)

# Predict on the test data
y_pred <- predict(model, newdata = dtest)

# Calculate RMSE
rmse <- sqrt(mean((y_test - y_pred)^2))
print(paste("RMSE:", rmse))

importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = model)
xgb.plot.importance(importance_matrix)


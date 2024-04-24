# install library
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("e1071")
install.packages("caret")
install.packages("ISLR")

# import necessary library
library(readr)
library(dplyr)
library(ggplot2)
library(e1071)
library(caret)
library (ISLR)

# importing dataset
setwd("C:/Users/eliza/RStudio")
df <- read.csv("student_prediction.csv")
df

# preprocessing dataset
df$GENDER <- factor(df$GENDER,levels = c(1,2), labels = c("Female","Male"))
df$HS_TYPE <- factor(df$HS_TYPE,levels = c(1,2,3), labels = c("Private","State", "other"))
df$AGE <- factor(df$AGE,levels = c(1,2,3), labels = c("18-21","22-25","above 26"))
df$SCHOLARSHIP <- factor(df$SCHOLARSHIP,levels = c(1,2,3,4,5), labels = c("None","25%","50%","75%","Full"))
df$WORK <- factor(df$WORK,levels = c(1,2), labels = c("Yes","No"))
df$ACTIVITY <- factor(df$ACTIVITY,levels = c(1,2), labels = c("Yes","No"))
df$PARTNER <- factor(df$PARTNER,levels = c(1,2), labels = c("Yes","No"))
df$SALARY <- factor(df$SALARY,levels = c(1,2,3,4,5), labels = c("USD 135-200","USD 201-270","USD 271-340","USD 341-410","above USD 410"))
df$TRANSPORT <- factor(df$TRANSPORT,levels = c(1,2,3,4), labels = c("Bus","Private Car/Taxi","Bicycle","other"))
df$LIVING <- factor(df$LIVING,levels = c(1,2,3,4), labels = c("Rental","Dormitory","with Family","other"))
df$MOTHER_EDU <- factor(df$MOTHER_EDU,levels = c(1,2,3,4,5,6), labels = c("Primary School","Secondary School","High School","University","MSc.","Ph.D."))
df$FATHER_EDU <- factor(df$FATHER_EDU,levels = c(1,2,3,4,5,6), labels = c("Primary School","Secondary School","High School","University","MSc.","Ph.D."))
df$X._SIBLINGS <- factor(df$X._SIBLINGS,levels = c(1,2,3,4,5),labels = c("1","2","3","4","5 or above"))
df$KIDS <- factor(df$KIDS,levels = c(1,2,3), labels = c("Married","Divorced","Died - one or both of them"))
df$MOTHER_JOB <- factor(df$MOTHER_JOB,levels = c(1,2,3,4,5,6), labels = c("Retired","Housewife","Government Officer","Private Sector Employee","Self-employment","other"))
df$FATHER_JOB <- factor(df$FATHER_JOB,levels = c(1,2,3,4,5), labels = c("Retired","Government Officer","Private Sector Employee","Self-employment","other"))
df$STUDY_HRS <- factor(df$STUDY_HRS,levels = c(1,2,3,4,5), labels = c("None","<5","6-10","11-20",">20"))
df$READ_FREQ <- factor(df$READ_FREQ,levels = c(1,2,3), labels = c("None","Sometimes","Often"))
df$READ_FREQ_SCI <- factor(df$READ_FREQ_SCI,levels = c(1,2,3), labels = c("None","Sometimes","Often"))
df$ATTEND_DEPT <- factor(df$ATTEND_DEPT,levels = c(1,2), labels = c("Yes","No"))
df$IMPACT <- factor(df$IMPACT,levels = c(1,2,3), labels = c("Positive","Negative","Neutral"))
df$ATTEND <- factor(df$ATTEND,levels = c(1,2,3), labels = c("Always","Sometimes","Never"))
df$PREP_STUDY <- factor(df$PREP_STUDY,levels = c(1,2,3), labels = c("Alone","with Friends","not Applicable"))
df$PREP_EXAM <- factor(df$PREP_EXAM,levels = c(1,2,3), labels = c("Closest Date to the Exam","Regularly During the Semester","Never"))
df$NOTES <- factor(df$NOTES,levels = c(1,2,3), labels = c("Never","Sometimes","Always"))
df$LISTENS <- factor(df$LISTENS,levels = c(1,2,3), labels = c("Never","Sometimes","Always"))
df$LIKES_DISCUSS <- factor(df$LIKES_DISCUSS,levels = c(1,2,3), labels = c("Never","Sometimes","Always"))
df$CLASSROOM <- factor(df$CLASSROOM,levels = c(1,2,3), labels = c("not Useful","Useful","not Applicable"))
df$CUML_GPA <- factor(df$CUML_GPA,levels = c(1,2,3,4,5), labels = c("2.00","2.00 2.49","2.50 2.99","3.00 3.49","3.49"))
df$EXP_GPA <- factor(df$EXP_GPA,levels = c(1,2,3,4,5), labels = c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","above 3.49"))
df$GRADE <- factor(df$GRADE,levels = c(0,1,2,3,4,5,6,7), labels =c("Fail","DD", "DC","CC","CB", "BB","BA", "AA"), ordered = TRUE)

# checking if there is any missing value
cat("Missing Values : \n")
print(anyNA(df))

view(df)

# data exploration
numRows <- nrow(df)
cat("Number of rows: ",numRows,"\n")
numCols<-ncol(df)
cat("Number of columns : ",numCols,"\n")
summary(df)
#---------------------------------------------------------------------------------------------------------------------------

library(polycor)
polycor_matrix <- polychor(df$CUML_GPA, df$PREP_EXAM)
print(polycor_matrix)


#---------------------------------------------------------------------------------------------------------------------------
chitable <- table(df$CUML_GPA, df$PREP_EXAM)
chi_square_test <- chisq.test(chitable)
print(chi_square_test)

#---------------------------------------------------------------------------------------------------------------------------
# UNIVARIATE = BAR CHART

countPrepExam <- df %>%
  count(PREP_EXAM)
countPrepExam

ggplot(data = countPrepExam, 
       aes(x = reorder(PREP_EXAM, n), y = n, fill = PREP_EXAM)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Student Exam Preparation Bar Chart",
       fill = "Exam Preparation Frequency",
       x = "Exam Preparation",
       y = "Total") +
  scale_fill_manual(values = c("#DCAAE4","#FDC2E4", "#A8DACD")) +
  theme_bw() +
  theme(legend.margin = margin(t = 30, r = 30, b = 0, l = 30),
        plot.title = element_text(size = 16, 
                                  hjust = 0.5, 
                                  vjust = 0.5, 
                                  margin = margin(10, 10, 10, 10)))

#---------------------------------------------------------------------------------------------------------------------------
# UNIVARIATE = PIE CHART

data_sum <- df %>%
  group_by(PREP_EXAM) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
data_sum
ggplot(data = data_sum,
       aes(x = "",y = percent, fill = PREP_EXAM)) +
  geom_bar(stat = "identity", width = 1, color = NA) + # set color to NA to remove piechart outline
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percent),"%")),
            position = position_stack(vjust = 0.5),
            size = 6) +
  labs(title = "Student Exam Preparation Pie Chart",
       fill = "Exam Preparation Frequency") +
  scale_fill_manual(values = c("#DCAAE4","#FDC2E4", "#A8DACD")) +
  theme_void() + #biar ga keluar 0/100 diluar lingkaran 
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, 
                                  hjust = 0.5, 
                                  vjust = 0.5, 
                                  margin = margin(10, 10, 10, 10)),
        legend.title = element_text(size = 12))


#---------------------------------------------------------------------------------------------------------------------------
# BIVARIATE = BAR CHART

label_data <- df %>%
  group_by(PREP_EXAM, CUML_GPA) %>%
  summarise(count = n())

ggplot(data = label_data, aes(x = PREP_EXAM, y = count, fill = PREP_EXAM)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = label_data, aes(label = count),  
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("#CEE4B3", "#FBF5AE","#F2AE7F"))  +
  labs(title = "Student Cumulative GPA Based on Exam Preparation",
       x = "Exam Preparation",
       y = "Total",
       fill = "Preparation Frequency",
       subtitle = "CUMULATIVE GPA")+
  facet_grid(.~CUML_GPA)+
  theme_bw() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(size = 16, 
                                  hjust = 0.5, 
                                  vjust = 0.5, 
                                  margin = margin(10, 10, 0, 10)),
        plot.subtitle = element_text(size = 12, 
                                     hjust = 0.5, 
                                     vjust = 0.5, 
                                     margin = margin(10, 10, 10, 10)))


?scale_fill_brewer

#---------------------------------------------------------------------------------------------------------------------------
# BIVARIATE = SEGMENTED BAR CHART

df_summary <- df %>%
  group_by(CUML_GPA, PREP_EXAM) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Plotting the data with counts and percentages
ggplot(df, 
       aes(x = factor(CUML_GPA, 
                      levels = 1:5, 
                      labels = c("<2.00", "2.00-2.49", "2.50-2.99", 
                                 "3.00-3.49", "above 3.49")),
           fill = factor(PREP_EXAM, 
                         levels = c(1,2,3),
                         labels = c("Closest to the exam",
                                    "Regularly during the semester", "Never")))) +
  geom_bar(position = "fill") +
  geom_text(data = df_summary,
            aes(label = paste0(count, 
                               "\n(", round(percent), "%)"),
                y = percent / 100 / 2), # Adjust y position for label placement
            position = position_fill(vjust = 0.5),
            color = "black",
            size = 4) +
  scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#CEE4B3", "#FBF5AE","#F2AE7F" )) +
  labs(title = "Student Cumulative GPA Based on Exam Preparation",
       x = "Exam Preparation",
       y = "Total",
       fill = "Preparation Frequency") +
  theme_bw() 

df_summary <- df %>%
  group_by(CUML_GPA, PREP_EXAM) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)


#---------------------------------------------------------------------------------------------------------------------------
# MULTIVARIATE = BAR CHART IN FACET_GRID

setwd("C:/Users/eliza/RStudio")
df <- read.csv("student_prediction.csv")


df$PREP_EXAM <- factor(df$PREP_EXAM, levels = 1:3, labels = c("Closest", "Regularly", "Never"))
df$CUML_GPA <- factor(df$CUML_GPA, levels = 1:5, labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49"))
df$LISTENS <- factor(df$LISTENS, levels = 1:3, labels = c("Never","Sometimes","Always") )

# Summarize data for plotting
data <- df %>%
  group_by(PREP_EXAM, CUML_GPA, LISTENS) %>%
  summarise(count = n())
print(data)
View(data)
# Plotting with ggplot
ggplot(data = data, aes(x = PREP_EXAM, y = count, fill = LISTENS)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 2.5) +
  scale_fill_manual(values = c("yellow","orange","red")) + 
  facet_wrap(. ~ CUML_GPA) +
  labs(
    title = "Impact of Listening in Class and Exam Preparation on Student's CGPA",
    x = "Exam Preparation",
    y = "Count",
    fill = "Listening in class"
  ) +
  theme_grey() +
  
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5)))

#---------------------------------------------------------------------------------------------------------------------------
# NAIVE BAYES CLASSIFIER TRAINING AND TEST

ntrain <- floor(nrow(df)*.7) #training data set 70%, testing 30%
ntrain
set.seed(123)   # set seed to ensure you always have same random numbers generated
train_ind <- sample(seq_len(nrow(df)),size = ntrain)  
# Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train <- df[train_ind,] #creates the training dataset with row numbers stored in train_ind
test <- df[-train_ind,]
#train <- df[1:ntrain,] # 1 sampe ntrain
# misah data, mau ngambil baris data mulai dari 1 sampai data terakhir di ntrain
#test <- df[(ntrain+1):nrow(df),] # dari posisi lanjutin train +1, sampai row terakhir df
dim(train)
dim(test)
#train<-subset(train, select = -c(CUML_GPA))
#head(train)
y_test <- test$CUML_GPA #jadi kunci jawaban untuk pencocokan ketika model sudah jadi
# di test masih ada gpa
#test<-subset(test, select = -c(CUML_GPA)) #gpa dibuang, karena gpa sudah ditaruh di y-test
head(test)
st_model = naiveBayes(as.factor(CUML_GPA) ~., data=train) # buat model 
# cumulative gpa jadi target variable, terhadap ~. sisa kolom yg masih ada
# data = train -> data training dimasukkan ke model
st_model #jadi modelnya
modelPred <- predict(st_model, test) # dari model yg berupa rumus, di beri input data testing
#model punya output
cMatrix <- table(modelPred, y_test)
#diccookan apakah output model cocok dengan kenyataan (y_test)
# yang dicocokkan adalah CUML_GPA
cMatrix
plot(cMatrix)
confusionMatrix(cMatrix)

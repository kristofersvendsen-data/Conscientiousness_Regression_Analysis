##### Library #####

library(psych) #contains bfi data set
library(skimr)
library(tidyverse)
library(Hmisc)
library(MBESS)
library(lme4)
library(interactions)
library(RColorBrewer)
library(gtsummary)
library(lm.beta)
library(car)

##### Load Data Set #####

bfi.raw = bfi #Load dataset
bfi.raw <- select(bfi.raw, C1:C5, gender:age) #create subset of conscientiousness questions with gender, education and age

##### Inspect Data #####

glimpse(bfi.raw)
head(bfi.raw)
colSums(is.na(bfi.raw))


##### Cleaning #####

###### Remove NAs in Education ######
bfi.raw <- filter(bfi.raw, !is.na(education)) #removes 223 entries
colSums(is.na(bfi.raw)) #verify no na in education


###### Verify and Correct Data Types #####
sapply(bfi.raw, class)

bfi.raw$gender<- as.factor(bfi.raw$gender)
is.factor(bfi.raw$gender)

bfi.raw$education <- as.factor(bfi.raw$education)
is.factor(bfi.raw$education)


###### Label Gender and Education Level #####
bfi.raw$gender <- factor(bfi.raw$gender,
                         levels = c(1,2),
                         labels = c("male", "female"))
summary(bfi.raw$gender)

bfi.raw$education <- factor(bfi.raw$education,
                            levels = c(1,2,3,4,5),
                            labels = c("some HS", "HS", "some college", "college", "graduate degree"))
summary(bfi.raw$education)


###### Verify Age Range #####
summary(bfi.raw$age)
hist(bfi.raw$age,
     breaks = 80)
head(sort(bfi.raw$age), 10) #print the lowest 10 ages
head(sort(bfi.raw$age, decreasing = TRUE), 10) #print the highest 10 ages

bfi.raw <- filter(bfi.raw, age>11) #removes 2 entries
summary(bfi.raw)


###### Inspect Survey Question NAs #####
colSums(is.na(bfi.raw))
na_rows <- bfi.raw %>% #87 total responses with NAs
  filter(if_any(everything(), is.na))

na_rows$na_count <- rowSums(is.na(na_rows)) #count number of NAs per participant
table(na_rows$na_count) #summarize NAs per participant

table(na_rows$education)
table(na_rows$age)
table(na_rows$gender)

#remove survey NAs
bfi.raw <- drop_na(bfi.raw) #removes 87 entries

###### Store Clean Data Set #####
bfi.clean <- bfi.raw



##### Analysis #####

###### Calculate conscientiousness score #####

consc.key = list(consc = c("C2", "C2", "C3", "-C4", "-C5")) #reverse scored items are marked with negative
consc.scores = scoreItems(consc.key, select(bfi.clean, C1:C5), totals = T, min = 1, max = 6)
bfi.clean$consc <- consc.scores$scores

hist(bfi.clean$consc)


###### Realiability Measures #####

consc <- select(bfi.clean, C1:C5) #take subset of conscientiousness questions
consc$C4 <- (7 - consc$C4) #flip scoring of reverse scored items: (max_score + 1) - score
consc$C5 <- (7 - consc$C5)

#reliability and bootstrapped confidence intervals, long to process, best for publication
consc.alpha <- ci.reliability(consc, type = 'alpha', interval.type = "bca")
consc.alpha

consc.omegah <- ci.reliability(consc, type = 'hierarchical', interval.type = "bca") #hierarchical omega
consc.omegah

#reliability estimates with quick confidence intervals
consc.alpha.quick <- ci.reliability(consc, type ='alpha', interval.type = 'feldt')
consc.alpha.quick

consc.omegah.quick <- ci.reliability(consc, type = 'hierarchical', interval.type = 'icc')
consc.omegah.quick


###### Regression #####

#create a model using age, gender, and education to predict conscientiousness

model0 <- lm(data = bfi.clean, consc ~ 1) #null model
summary(model0)

model1 <- lm(data = bfi.clean, consc ~ age + gender + education)
summary(model1)

anova(model0, model1)

plot(model1) #plot residuals to verify assumptions
#Plot 1 = linearity assumption, good is red line is horizontal/flat
#Plot 2 = normality assumption, good is follows diagonal line
#Plot 3 = homoscedasticity, good is red line is horizontal/flat
#Plot 4 = outliers. 


vif(model1) #check variance inflation factor to ensure no interaction between predictor terms


##### Visualization #####

# Create dataframe for vis with proper punctuation

bfi.vis <- select(bfi.clean, gender:age, consc) #create subset for visualizations

bfi.vis <-
  rename(bfi.vis, 
         Gender = gender,
         Education = education,
         Age = age,
         Conscientiousness = consc
  )


bfi.vis$Education <- factor(bfi.vis$Education,
                            levels = c("some HS", "HS", "some college", "college", "graduate degree"),
                            labels = c("Some High School", "High School", "Some College", "College", "Graduate Degree"))

bfi.vis$Gender <- factor(bfi.vis$Gender,
                         levels = c("male", "female"),
                         labels = c("Male", "Female"))

# Create consistent color pallette

#Use a color palette, best if you have variables with more levels
#mycolors <- brewer.pal(2, "Set1")
#names(mycolors) <- levels(bfi.clean$gender)
#gendercolors <- scale_colour_manual(name = "gender", values = mycolors) #store for scatter plots
#gendercolorsbar <- scale_fill_manual(name = "gender", values = mycolors) #store for bar charts

#Assign a specific color to each gender
gendercolors <- scale_colour_manual(values = c("Male" = "#008080", "Female" = "#cc5500")) #store for scatter plots
gendercolorsbar <- scale_fill_manual(values = c("Male" = "#008080", "Female" = "#cc5500")) #store for bar charts


###### Population Pyramid ######

bins.age <- seq(from = 0, to = 100, by = 5) #define bin size

gender.age <- select(bfi.vis, Gender, Age) #select Gender and Age

gender.age$age_group <- cut(gender.age$Age, bins.age, include.lowest = T, right = F) #organize in bins

pop.pyramid <- gender.age %>% 
  group_by(Gender) %>% 
  count(age_group) %>% 
  arrange(Gender) #arrange Male first for consistent legend format

plot1 <- ggplot(
  pop.pyramid,
  aes(
    x = age_group,
    fill = Gender,
    y = ifelse(               #separate Males and Females
      test = Gender =="Male",
      yes = -n, #puts Males on left
      no = n
    )
  )
) +
  geom_bar(stat = "identity")+
  gendercolorsbar

plot.pop.pyramid <- plot1 +
  scale_y_continuous(
    labels = abs,       #plot absolute values to remove negative used above
    limits = max(pop.pyramid$n) *c(-1,1) #create symetrical limits
  ) +
  theme_minimal()+
  coord_flip()+
  labs(
    x = "Age (years)",
    y = "Number of Participants",
    fill = "Gender",
    title = "Age and Gender Distribution"
    
  )
plot.pop.pyramid 



###### Education by Gender Plot #####

edu.gender <- select(bfi.vis, Gender, Education) #create subset
edu.gender.count <- edu.gender %>%  
  group_by(Gender) %>%  #group by Gender
  count(Education)     #count participants with each level of education




plot.edu.gender <- ggplot(edu.gender.count, aes(fill = Gender, y= n, x=Education))+
  geom_bar(position = "stack", stat = "identity")+
  labs(y= "Number of Participants", x="Highest Education Level")+
  ggtitle("Highest Education Level Achieved")+
  theme_minimal()+
  gendercolorsbar


###### Conscientiousness by Gender Plot #####

plot.consc.gender <- ggplot(bfi.vis, aes(fill = Gender, x= Conscientiousness ))+
  geom_histogram(binwidth = 2, color = "black")+
  labs(y = "Number of Participants")+
  theme_minimal()+
  ggtitle("Conscientiousness Score by Gender")+
  gendercolorsbar
plot.consc.gender


##### Summary info for write-up ######

summary(bfi.vis) #easy descriptors of key variables
tapply(bfi.vis$Conscientiousness, bfi.vis$Gender, summary) #conscientiousness by gender
tapply(bfi.vis$Age, bfi.vis$Gender, summary) #age by gender
tapply(bfi.vis$Education, bfi.vis$Gender, summary) #education by gender


#reliability measures
consc.alpha #alpha with bootstrapped 95%CI for best report
consc.omegah #hierarchical omega with boostrapped 95%CI for best report

consc.alpha.quick #alpha with Feldt 95%CI for quick report
consc.omegah.quick #hierarchical omega with icc 95%CI for quick report

plot(model1) #assumptions check reporting
summary(model1) #model report


####Regression Table
modelvis <- lm(data = bfi.vis, Conscientiousness ~ Age + Gender + Education) #rerun model with vis data


reg.table <- tbl_regression(modelvis) %>% # create regression table
  bold_p() %>% 
  add_glance_table(include = c("r.squared", "adj.r.squared", "nobs", "statistic" , "p.value"))
reg.table

as_gt(reg.table) %>% # save .png of regression table
  gt::gtsave(filename = "consc_reg_table.png")



#### Figures

plot.pop.pyramid #population pyramid showing gender and age distribution of sample
ggsave("Population_Pyramid.png", plot = plot.pop.pyramid, width = 15, height = 12, units = "cm")

plot.edu.gender #bar chart showing education level of sample by gender
ggsave("Education_Level.png", plot = plot.edu.gender, width = 17, height = 12, units = "cm")

plot.consc.gender #histogram showing conscientiousness score by gender
ggsave("Conscientiousness_Histogram.png", plot = plot.consc.gender, width = 15, height = 12, units = "cm")
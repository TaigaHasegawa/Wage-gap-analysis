ods html close; 
options nodate nonumber leftmargin=1in rightmargin=1in;;
title;
ods escapechar="~";
ods rtf file='/folders/myfolders/TaigaHasegawaFinalProject.rtf' 
	nogtitle startpage=no;
ods noproctitle;
data attrition;
	infile "/folders/myfolders/WA_Fn-UseC_-HR-Employee-Attrition-3.csv" delimiter=",";
	input Age  Attrition $ BusinessTravel $ DailyRate Department $ DistanceFromHome	Education EducationField $ EmployeeCount EmployeeNumber	EnvironmentSatisfaction	Gender $ HourlyRate JobInvolvement JobLevel JobRole	$ JobSatisfaction MaritalStatus	$ MonthlyIncome	MonthlyRate	NumCompaniesWorked	Over18 $ OverTime $ PercentSalaryHike PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel	TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance	YearsAtCompany	YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager;
	if _n_ =1 then delete;
	/*eliminate the useless variables*/
	drop Over18 OverTime EmployeeNumber EmployeeCount;
run;
ods text="My question is to find what cause wage gap and what kind of impact each variable has on wage.";
ods text="I used 'MonthlyIncome' as response variable because this variable represented the monthly income.";
ods text="Before starting to analyze, I omitted the Over18, Overtime, EmployeeNumber and EmployeeCount from the data because they had only one value and there wes no variation in them.";
ods text="I made two models to analyze wage gap. First is the ANOVA model and second is the Linear Regression. ANOVA used only categorical variables as predictors but linear regression model used all variables including categorical, descrete and continuous variables. ";
/*Check the normality of monthly income*/
ods text="First I checked the features of monthlyincome. The Goodness of Fit and Test For Normality shows that MonthlyIncome doesn't follow the normal distribution. I might need to transfom the monthlyincome later.";
proc univariate data=attrition normal;
	var monthlyincome;
	histogram monthlyincome/normal;
	ods select Histogram GoodnessOfFit TestsForNormality;
run;

ods text="ANOVA";
ods text="At the beginning I used all the categorical variables in the model and selected variables by ss1 and ss3. Diagnostics showes that there are several points above the line in the Cook's distance but they are not so deviate from other points and we can ignore. QQplot is straight and seems to follow normal distribution. There appears to be a little bit decreasing variation in the residuals as the predicted value increases but it is still okay and so we don't have to use transformation.";
/*ANOVA using all categorical variables*/
proc glm data=attrition plots=diagnostics;
	class BusinessTravel JobLevel Department Attrition EducationField Gender JobRole MaritalStatus Education EnvironmentSatisfaction JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance;
	model MonthlyIncome=BusinessTravel JobLevel Attrition Department EducationField Gender JobRole MaritalStatus Education EnvironmentSatisfaction JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance/ ss1 ss3;
	ods select OverallANOVA ModelANOVA DiagnosticsPanel;
run;
ods text="Based on the ss1 and ss3, Joblevel and JobRole are significant. Education also seemes to be significant. I used variables that seemed to be significant in the next model.";
/*ANOVA using variables that are siginificant in the previous anova model*/
proc glm data=attrition plots=diagnostics;
	class JobLevel Education JobRole Attrition ;
	model MonthlyIncome=Joblevel Education JobRole/ ss1 ss3;
	lsmeans Joblevel Education JobRole/pdiff=all  cl;
	ods select OverallANOVA ModelANOVA LSMeans LSMeanDiffCL DiagnosticsPanel;
run;
ods text="In the final model, Joblevel and JobRole are significant and we can also add Education as significant variables.";
ods text="Next I'll see the least squares means. We can know that as job level goes up, the monthly income is also increasing. 95% Confidence Limits does not include 0 in Least Squares Means for Effect JobLevel.";
ods text="As to education, all 95% Confidence Limits includes 0 but we can clearly know that Education level 1 and 4 gets the higher salary than education level 2, 3, and 5. Level 1 means bellow college, level 2 means college level, level 3 means bachelor level, level 4 means master level, and level 5 means doctor level.";
ods text="Regarding JobRole, we can know that the monthly income is decreasing in the following order, Manager, Human Resorces, Research, Sales Executive, Healthcare Representative, Manufacturing Director, Laboratory Technician, Sales Representative.";

/*making the dummy variable*/
ods text="Linear Regression";
proc glmmod data=attrition outdesign=attrition_dummy outparm=attritionParm noprint;
   class BusinessTravel attrition JobLevel Department EducationField Gender JobRole MaritalStatus Education EnvironmentSatisfaction JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance;
   model MonthlyIncome=Age--YearsWithCurrManager;
run;
/*Linear Regression*/
ods text="First I used the linear regression to find the significant variables among all categorical, descrete and continous variables but because proc reg didn't support categorical variables, I had to make the dummy variables for categorical variables by using proc glmmod.";
proc reg data=attrition_dummy;
	model MonthlyIncome=Col1--Col54 Col56--Col77/ vif selection=stepwise sle=.05 sls=.05;
	ods select ParameterEstimates FitStatistics DiagnosticsPanel;
run;
ods text= "Stepwise selection was used to choose the significant predictors. 'Job level', 'Job Role', 'num companies worked', and 'total working years' were chosen as significant variable and R square is 0.9315. It's pretty nice. ";
ods text="Diagnostics were all good. There was no extreme point, QQplot was relatively straight, and predicted value vs monthly income scatter plot was relatively flat.";

/*split into training and test dataset*/
ods text="Finally, I tried to predict the monthly income. To make a test dataset, I splited the data into training data and test data. Training data used the 80% of the whole data and test data used the 20% of the whole data.";
data attrition_sub;
	set attrition;
	n=ranuni(8);
run;
proc sort data=attrition_sub;
	by n;
run;
data training testing;
	set attrition_sub nobs=nobs;
	if _n_<=.8*nobs then output training;
	else output testing;
run;
ods text="Then I made the prediction model by using proc glmselect. I used stepwise selection and set the rejection level at α. Faraway notes that α could be 0.15 to 0.20 if the goal of themodel fitting is prediction. I set the α at 0.15 in this case.";
/*making the prediction model*/
/*looking for smallest average squared errors of test dataset*/
  
proc glmselect data=training testdata=testing
                  seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot);
      partition fraction(validate=0.5);
      class Education JobLevel EnvironmentSatisfaction Attrition JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance BusinessTravel Department EducationField Gender JobRole MaritalStatus;
   		model MonthlyIncome=Age--MaritalStatus MonthlyRate--YearsWithCurrManager
              / selection=stepwise(choose = AIC 
                                   select = sl) 
                sle=.20 sls=.20
                hierarchy=single stb stats = press ;
                ods select FitStatistics;
run; 
proc glmselect data=training testdata=testing
                  seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot);
      partition fraction(validate=0.5);
      class Education JobLevel EnvironmentSatisfaction Attrition JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance BusinessTravel Department EducationField Gender JobRole MaritalStatus;
   		model MonthlyIncome=Age--MaritalStatus MonthlyRate--YearsWithCurrManager
              / selection=stepwise(choose = AIC 
                                   select = sl) 
                sle=.15 sls=.15
                hierarchy=single stb stats = press ;
                ods select FitStatistics;
run; 
proc glmselect data=training testdata=testing
                  seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot);
      partition fraction(validate=0.5);
      class Education JobLevel EnvironmentSatisfaction Attrition JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance BusinessTravel Department EducationField Gender JobRole MaritalStatus;
   		model MonthlyIncome=Age--MaritalStatus MonthlyRate--YearsWithCurrManager
              / selection=stepwise(choose = AIC 
                                   select = sl) 
                sle=.10 sls=.10
                hierarchy=single stb stats = press ;
                ods select FitStatistics;
run; 
proc glmselect data=training testdata=testing
                  seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot);
      partition fraction(validate=0.5);
      class Education JobLevel EnvironmentSatisfaction Attrition JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance BusinessTravel Department EducationField Gender JobRole MaritalStatus;
   		model MonthlyIncome=Age--MaritalStatus MonthlyRate--YearsWithCurrManager
              / selection=stepwise(choose = AIC 
                                   select = sl) 
                sle=.05 sls=.05
                hierarchy=single stb stats = press ;
       ods select FitStatistics ;
run; 
proc glmselect data=training testdata=testing
                  seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot);
      partition fraction(validate=0.5);
      class Education JobLevel EnvironmentSatisfaction Attrition JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance BusinessTravel Department EducationField Gender JobRole MaritalStatus;
   		model MonthlyIncome=Age--MaritalStatus MonthlyRate--YearsWithCurrManager
              / selection=stepwise(choose = AIC 
                                   select = sl) 
                sle=.05 sls=.05
                hierarchy=single stb stats = press ;
       ods select FitStatistics ParameterEstimates criterionPanel;
run;
proc glmselect data=training testdata=testing
                  seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot);
      partition fraction(validate=0.5);
      class Education JobLevel EnvironmentSatisfaction Attrition JobInvolvement JobSatisfaction PerformanceRating RelationshipSatisfaction StockOptionLevel WorkLifeBalance BusinessTravel Department EducationField Gender JobRole MaritalStatus;
   		model MonthlyIncome=Age--MaritalStatus MonthlyRate--YearsWithCurrManager
              / selection=stepwise(choose = SBC 
                                   select = sl) 
                sle=.05 sls=.05
                hierarchy=single stb stats = press ;
      ods select FitStatistics criterionPanel;
run; 
ods text="The result showed that Attrition, JobLevel, JobRole, NumCompaniesWorked and TotalWorkingYears were included in the prediction model. Attrition was added to the previous model newly. AIC, PRESS, AICC, Adjusted R square all prefered this model. Average squared errors of test dataset is 1709651 and that of training dataset is 1532224. We can say that this prediction model is good because average squared errors of test dataset is relatively small even in test dataset.";
ods text="Conclusion";
ods text="Joblevel is the most significant predictor in the model. Even Joblevel itself has the 0.925 R square. JobRole is the second signficant predictor. This means that this company put more emphasis on high job level and specific job role than on NumCompaniesWorked and TotalWorkingYears.";
ods text="We can also say that job attrition and monthly income is correlated with each other. Those who quit the job got the less salary than those who decided to continue work.";
proc glm data=attrition;
	class joblevel;
	model monthlyincome=joblevel;
	ods select FitStatistics;
run;
ods rtf close;

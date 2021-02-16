# Craiglist-Linear-Regression
Linear Regression Model for Predicting Used Car Prices on Charlotte Craigslist.

The complete project with explanation and results can be seen in the "FINAL PROJECT - V3.docx" file.

This was my final project for Elements of Statistics II at UNC-Charlotte. The purpose of the project was to determine what quantitative and qualitative variables would be statistically useful for the calling price of a used vehicle on Craigstlist in Charlotte.

I started with a simple search, determining what test variables are available on Craigslist. I then determined whether some results may return inaccurate data (e.g., vehicles listed for $0 to lure in customers) and adjusted the search filters based on the results.

I then built a web crawler using BeautifulSoup in Python to (attempt) to extract the first 500 results. The program did not perform exactly as intended, returning between 300-400 vehicles in a CSV file before crashing. This was more than enough data for the scope of this project.

I then wrote a program in R Script for variable selection (variable instantiation, full linear model development, stepwise regression, all-possible-regressions selection procedure, and model fitting), model selection (partial f-tests), assumption checks (lack of fit, homoscedasticity, normality), weeding outliers (residuals, standardized residuals, studentized residuals, Cooks Distance, dfbetas, and dffits) and the Global F-Test to see if final model is statistically useful.

The final result is a Linear Regression Model that is able to predict the calling price of a used vehicle on Charlotte Craigstlist with 91% sample variation explained by the model and within $2,440 standard deviation.


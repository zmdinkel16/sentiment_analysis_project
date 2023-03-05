# Sentiment Analysis of Financial Companies
 
## Author
Zach Dinkel

## Introduction
I was given a datatable with over 670,000 rows and 18 columns containing information about the complaints that individual financial institutions (E.g. Equifax, Bank of America, Wells Fargo & Co.) receieved in 2013. Below I will explain my approach in analyzing this data.  

## Data Dictionary
The columns used in my analysis were the following:

1. company: the company that received the complaint
2. date.received: the date that the company received this complaint
3. product: the general product offered by the company that the complaint pertains to (E.g. Credit Report, Mortgage, Debt Collection)
4. sub.product: the specific product the customer complained about (E.g. Checking Account, Conventional Fixed Mortgage, Vehicle Loan)
5. issue: the specific issue that the customer complained about (E.g. Managing the loan/lease, Using debit/ATM card)
6. consumer.complaint.narrative: customer explanation/narrative of what and how the issue occurred


## Data Cleaning


## Data Analysis

### Graph 1 - Complaints Recieved by Month
<img src="/images/Rplot.png" alt="Girl in a jacket" width="75%" height="75%">

* All of the comlpaint data in this dataset was received between 1/1/2013 and 12/31/2013
* The most complaints came in the months March, August, and October
* There were the least amount of complaints during the winter season
* Although this graph is interesting to look at, it does not give use much information past what is stated above
<br /><br /><br />
1. Created an interactive shiny application to display information about the actualy complaints received by the companies in the dataset
### Graphs 2 thru 7 - Shiny App Manipulation
<img src="/images/Shinyapp1.png" alt="Girl in a jacket" width="100%" height="100%">
<img src="/images/Shinyapp2.png" alt="Girl in a jacket" width="100%" height="100%">
<br />

#### Drop-down Selection
* Gives the user the ability to select how many companies they shall want displayed in each visualization (E.g. this person chose 20 companies to be displayed) 
#### Graph 2 - DataTable next to drop-down selection: 
* Shows that credit reporting was the most faulty product for Equifax, Experian, and Transunion while other mortgages were the most faulty product for Bank of America
#### Graph 3 - Wordcloud:     
* Shows the most common words used in the consumer complaints that were sent to the top 20 companies
#### Graph 4 - Total Negative Sentiment Score:     
* Equifax, Experian, and Bank of America had the most negative complaints followed by those listed in the bar chart
#### Graph 5 - Negative Sentiment Score per Word in Complaint:     
* Some companies listed as the worst in Graph 4 are much larger than others that aren't displayed. Therefore, I calculated the average sentiment per word in each complaint by company and displayed the worst in this graph. We can see that Medical Debt Recovery Inc., Metropolitan Home Mortgage Inc., and Gross Polowy LLC have the most negative scores. This means that the complaints received by these companies were the most negative complaints received by any other companies. 
#### Graph 6 - Most Common Complaints by Company
* Shows that inccorrect information on credit report was the most common complaint received and that Transunion, Experian, and Equifax were the source of these complaints. Loan modification, collection, foreclosure, and dealing with lender or servicer were the second and third most common complaints. 
#### Graph 7 - Total Positive Sentiment Score:
* This shows the companies with the least negative complaints received. Freedom Mortgage and Great Lakes were the clear frontrunners. 
<br /><br /><br />

## Opportunity for Improvement
* If I were to redo or had more time to work on this project, I would do the following:
    * I would give the user the choice in the shinyapp to further split the data by state. 
    * I would split the by date to show which complaints occured at which times of year.
    * I would have showed what percentage of complaints were submitted by older folks and what the complaints were.

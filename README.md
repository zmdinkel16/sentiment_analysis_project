# Sentiment Analysis of Financial Companies
 
## Author
Zach Dinkel

## Introduction
I was given a datatable with over 670,000 rows and 18 columns containing information about the complaints that individual financial institutions (E.g. Equifax, Bank of America, Wells Fargo & Co.) receieved in 2013. Below I will explain my approach in analyzing this data.  
<br />

## Data Dictionary
The columns used in my analysis were the following:

1. company: the company that received the complaint
2. date.received: the date that the company received this complaint
3. product: the general product offered by the company that the complaint pertains to (E.g. Credit Report, Mortgage, Debt Collection)
4. sub.product: the specific product the customer complained about (E.g. Checking Account, Conventional Fixed Mortgage, Vehicle Loan)
5. issue: the specific issue that the customer complained about (E.g. Managing the loan/lease, Using debit/ATM card)
6. consumer.complaint.narrative: customer explanation/narrative of what and how the issue occurred
<br />

## Data Cleaning
<br />
1. Column Names:

* Made all column names lowercase
* Renamed 4 column names so they were more clear
```
names(df) <- tolower(names(df))

df <- df %>%
  rename(date.sent = date.sent.to.company,
         complaint.response = company.response.to.consumer,
         timely.response = timely.response., 
         consumer.disputed = consumer.disputed.)
```
<br />
2. Substituting Unique Characters Scattered Throughout Dataframe:

* Substituted all 'X' and '/' in df with nothing so that they would all be deleted. This was due to redacted information,such as dates, that appeared as XX/XX/XXXX in the data. 
* Substituted all words that had " n't" with "n't" so that they did not appear as 2 words.
```
df$consumer.complaint.narrative <- gsub("X", "", df$consumer.complaint.narrative)
df$consumer.complaint.narrative <- gsub("/", "", df$consumer.complaint.narrative)
df$consumer.complaint.narrative <- gsub(" n't", "n't", df$consumer.complaint.narrative)
```
<br />
3. Making dates appear as the date character type:

* Dates initially appeared in the df as character types
```
df$date.received <- as.Date(df$date.received, format = "%m/%d/%Y")
df$date.sent <- as.Date(df$date.sent, format = "%m/%d/%Y")
```
<br />
4. Replacing all empty values in the sub.product column with NA:

* Replacing all blank values in the sub.product column with NA values will make it easier later on to manipulate the cells that are blank
```
df$sub.product[df$sub.product == ""] <- NA
```
<br /> 
<br /> 
<br /> 

## Data Analysis
### Pivot Tables Created for Analysis
<br /> 
1. complaints_by_month: 

* Created to display the amount of complaints received by month.
```
complaints_by_month <- df %>%
  select(date.received, product) %>%
  mutate(month = month(date.received)) %>%
  group_by(month) %>%
  summarise(count = n())
```
<br /> 
2. company_complaints_tidy: 

* Created to format complaints received in a tidy format so that I could assign sentiment values to each word in my analysis.
```
company_complaints_tidy <- df %>%
  select(company, consumer.complaint.narrative, state) %>%
  unnest_tokens(word, consumer.complaint.narrative)
```
<br /> 
3. afinn: 

* Created to display the total sentiment value for each company in the data.
```
afinn <- company_complaints_tidy %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(company) %>%
  summarise(sentiment = sum(value)) %>% 
  arrange(sentiment)
```
<br /> 
4. sentiment_per_word:

* Some companies had many more complaints than other simply because they have larger customer bases. So, I created this pivot table to display the average sentiment value per word for each company to combat this bias in the data.
```
sentiment_per_word <- company_complaints_tidy %>%
  group_by(company) %>%
  count(word, name = 'word_count') %>%
  group_by(company) %>%
  summarise(word_count = sum(word_count)) %>%
  left_join(afinn, by = 'company') %>%
  mutate(weighted_sentiment = sentiment/word_count) %>%
  select(company, weighted_sentiment)
```
<br /> 
5. most_common_issue

* I created this pivot table to show the most common product that was causing customers to complain about in each company
```
most_common_issue <- df %>%
  select(company, sub.product, product) %>%
  mutate(sub.product = coalesce(sub.product, product))%>%
  group_by(company, sub.product) %>%
  summarise(count = n()) %>%
  summarise(product = sub.product[which.max(count)], count = max(count))
```
<br /> 
6. common_complaints

* I created this to display what was the most common issue received in each company
```
common_complaints <- df %>%
  select(company, issue) %>%
  group_by(company, issue) %>%
  summarise(count = n()) %>%
  summarise(issue = issue[which.max(count)], count = max(count))
```
<br /> 
<br /> 
<br /> 

## Visualizations Created With Pivots
### Graph 1 - Complaints Recieved by Month
<img src="/images/Rplot.png" alt="Girl in a jacket" width="75%" height="75%">

* All of the comlpaint data in this dataset was received between 1/1/2013 and 12/31/2013
* The most complaints came in the months March, August, and October
* There were the least amount of complaints during the winter season
* Although this graph is interesting to look at, it does not give use much information past what is stated above
<br />

### Graphs 2 thru 7 - Shiny App Manipulation
1. Created an interactive shiny application to display information about the actualy complaints received by the companies in the dataset
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

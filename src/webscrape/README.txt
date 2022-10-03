# Web Spiders

When you write web spiders to collect data from the web there are two things to consider:

- Make sure you read the terms of service for web sites whose data you want to use. I have found that calling or emailing web site owners explaining how I want to use the data on their site usually works to get permission.
- Make sure you don't access a site too quickly. It is polite to wait a second or two between fetching pages and other assets from a web site.

## Running some examples

try:

        (ql:quickload "webscrape")
        (webscrape:fetch-page "https://markwatson.com")
        (webscrape:fetch-page "http://knowledgebooks.com")
        
note: https is not currently working on ABCL or Clozure CCL



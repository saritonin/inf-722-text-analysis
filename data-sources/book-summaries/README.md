# CMU Book Summary Dataset
This file, containing plot summaries of books sourced from Wikipedia matched with 
book metadata sourced from Google's now-defunct [Freebase graph database](https://developers.google.com/freebase).  
The former Freebase data has since been imported to Wikidata, 
where it may [still be found](https://www.wikidata.org/wiki/Wikidata:WikiProject_Freebase).

## File used for this analysis
Sourced from: http://www.cs.cmu.edu/~dbamman/data/booksummaries.tar.gz via http://www.cs.cmu.edu/~dbamman/booksummaries.html

License: [Creative Commons Attribution-ShareAlike](http://creativecommons.org/licenses/by-sa/3.0/us/legalcode)

File: booksummaries.txt as modified on June 2, 2013 at 7:18 PM

Downloaded on: January 30, 2023

## Potential alternative dataset collection method
1. Get "book" pages by looking for direct [transclusion of the Infobox book template](https://en.wikipedia.org/wiki/Special:WhatLinksHere/Template%3AInfobox%20book?hidelinks=1&hideimages=1).
2. Retrieve content of page's "Synopsis" or "Plot Summary" section, if one exists.

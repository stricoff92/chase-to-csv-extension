

### Local Storage Keys

 - `onPage`  boolean flag that indicates whether a tab is on the accounts page
 - `running` boolean flag that indicates whether data is being scraped
 - `data`    associations of chase <-> Accounting accounts
                [
                    [CHASE_ID, ACC_ID],
                    ...
                ]

### Messaging Event Keys
 - `onPage`         event that indicates `onPage` local storage value just flipped to true
 - `offPage`        event that indicates `onPage` local storage value just flipped to false
 - `scrapeStarted`  event that indicates `running` local storage value just flipped to true
 - `scrapeStopped`  event that indicates `running` local storage value just flipped to false
 - `progressBar`    event that indicates the progress bar should get set to a value/max
 - `debugMessage`   event that indicates a new debug message as be created


### Row Filters:

 - rowFilters: request.rowFilters
```
[
    {
        and: [CONTAINS_VALUE]
        or: [CONTAINS_VALUE_0, CONTAINS_VALUE_n]
    }
]
```



### Local Storage Keys

 - `onPage`  boolean flag that indicates whether a tab is on the accounts page
 - `running` boolean flag that indicates whether data is being scraped
 - `data`    associations of chase <-> FEZ accounts
                [
                    [CHASE_ID, FEZ_ID],
                    ...
                ]

### Messaging Event Keys
 - `onPage`         event that indicates `onPage` local storage value just flipped to true
 - `offPage`        event that indicates `onPage` local storage value just flipped to false
 - `scrapeStarted`  event that indicates `running` local storage value just flipped to true
 - `scrapeStopped`  event that indicates `running` local storage value just flipped to false

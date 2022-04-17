
const log = (msg) => {
    console.log("HCE: " + JSON.stringify(msg));
}
log("content_scripts/scraper.js running");


async function getRunning() {
    return new Promise((resolve) => {
        chrome.storage.local.get(['running'], (result) => {
            resolve(result.running);
        });
    });
}

async function getLookupTable() {
    return new Promise((resolve) => {
        chrome.storage.local.get(['data'], (result) => {
            /*  Mapping of BANK-ID => ACC-ID
            */
            resolve(new Map(result.data));
        });
    });
}

window.addEventListener("load", main, false);
function main () {
    const checkTableTimer = setInterval(checkForTable, 300);
    var timedOut;
    const timedOutCallbackTimer = setTimeout(()=>{
        log("<EVENT timedOut>");
        timedOut = true;
    }, 12000)

    let onPage = false;

    function checkForTable () {
        if (document.querySelector("#accountsTableAG1Table0")) {
            clearInterval(checkTableTimer);
            clearTimeout(timedOutCallbackTimer);
            processEventFoundTable();
            onPage = true;
        } else if (timedOut) {
            clearInterval(checkTableTimer);
        } else {
            log("waiting for table...");
        }
    }

    chrome.runtime.onMessage.addListener(async (request, sender, sendResponse)=> {
         if (request.event === "scrapeStarted") {
            console.log("received event to start scraping");
            console.log(request);
            if(onPage) {
                sendResponse(true);
                let linksClicked = [];
                const lookup = await getLookupTable();
                console.log({lookup});
                setTimeout(()=>{
                    scrapeData(
                        request.startDate, request.endDate, lookup, linksClicked,
                    );
                }, 500);
            }
        }
    });

}

function processEventFoundTable() {
    log("<EVENT foundTable>");
    chrome.storage.local.get(['onPage'], (result) => {
        if(!result.onPage) {
            log("setting onPage = true");
            chrome.storage.local.set({onPage: true});
            chrome.runtime.sendMessage({event: "onPage"});
        }
    })
}

function confirmIfFalsy(value, message) {
    if(!value) {
        if(!confirm(message)) {
            throw new Error("User Exit")
        }
    }
}

function clickSeeAllAccountsLinkIfItsThere() {
    const element = document.querySelector("#seeAllAccountsAG1Table")
    if(!element) {
        log("could not find link to view all accounts")
        return;
    }
    log("clicking see all accounts link")
    element.click();
}

function canViewMoreAccounts() {
    return !!document.querySelector("#seeAllAccountsAG1Table");
}

function getChaseCurrentAccountNumber() {
    const res = /accountId=\d+/.exec(location.hash);
    if(res) {
        return res[0].split("=")[1]
    }
}

async function scrapeData(startDate, endDate, lookup, linksClicked) {
    log("scrapeData running, checking storage for running flag")
    const running = await getRunning();
    console.log({running})
    if(!running) {
        log("running flag=false, bye")
        return;
    }

    // Wait for table to load
    const tableContainer = document.getElementById("accountsTableAG1Table0");
    if(!tableContainer) {
        log("could not find table container, wating")
        setTimeout(()=>{
            scrapeData(startDate, endDate, lookup, linksClicked)
        }, 200);
        return
    }

    // Wait until table is fully expanded.
    clickSeeAllAccountsLinkIfItsThere();
    if(canViewMoreAccounts()){
        log("waiting for full account list")
        setTimeout(()=>{
            scrapeData(startDate, endDate, lookup, linksClicked);
        }, 100);
        return;
    }

    // Loop through account rows (behind shadow root)
    const table = tableContainer.shadowRoot.querySelector('table');
    const tableRows = table.querySelectorAll(".data-table-for-accounts__row");
    log("searching " + tableRows.length + " rows for unclicked links")
    for(let i=0; i< tableRows.length; i++) {
        const tr = tableRows[i];
        const rowHeader = tr.querySelector(".data-table-for-accounts__row-header");
        const rowHeaderText = rowHeader.innerText
        if(linksClicked.indexOf(rowHeaderText) != -1) {
            log("skipping row " + rowHeaderText)
            continue;
        }

        // Navigate to the account page
        log("checking row " + rowHeaderText);
        linksClicked.push(rowHeaderText);
        tr.querySelector("a").click();

        // Check if extension has bank account number saved.
        setTimeout(()=>{
            const chaseId = getChaseCurrentAccountNumber();
            log("on transaction page for account number " + chaseId);
            if(!chaseId) {
                log("Could not find account number, skipping.")
                document.querySelector("#requestAccounts").click();
                setTimeout(() => {
                    scrapeData(startDate, endDate, lookup, linksClicked);
                });
                return;
            }
            if(!lookup.has(chaseId)) {
                log("no ACCOUNTING ID found for this account");
                document.querySelector("#requestAccounts").click();
                setTimeout(() => {
                    scrapeData(startDate, endDate, lookup, linksClicked);
                });
                return;
            }
            const accId = lookup.get(chaseId);
            log("row has associated ACC account " + accId);
            setTimeout(()=> {
                scrapeTransactionData(0, startDate, endDate, lookup, linksClicked, accId, chaseId);
            });
        });
        return;
    }
    chrome.storage.local.set({running: false}, ()=> {
        chrome.runtime.sendMessage({event: "scrapeStopped"})
    });
}


function scrapeTransactionData(attemptNumber, startDate, endDate, lookup, linksClicked, AccountingId, ChaseAccountId) {

    // Check for any takeovers
    const continueWithActivity = document.getElementById("continueWithActivity");
    if(continueWithActivity) {
        log("clicking continue with activity button")
        continueWithActivity.click();
    }

    // Wait for table to load, or timeout.
    const table = document.getElementById('activityTableslideInActivity');
    if (!table) {
        attemptNumber++;
        if(attemptNumber < 100) {
            setTimeout(()=>{
                scrapeTransactionData(attemptNumber, startDate, endDate, lookup, linksClicked, AccountingId, ChaseAccountId)
            }, 120);
        }
        return;
    }

    // Waiting for "see all activity" rows to load
    const loaderElem = document.querySelector(".loader-section");
    if(loaderElem) {
        attemptNumber++
        setTimeout(()=>{
            scrapeTransactionData(
                attemptNumber, startDate, endDate, lookup, linksClicked, AccountingId, ChaseAccountId)
        }, 120);
        return;
    }

    const startDateObj = parseISODateString(startDate);
    const endDateObj = parseISODateString(endDate);

    // Keep clicking "See more activity" until
    //  - oldest transaction date < startDate
    //  - OR the see more activity button no longer appears.
    const rows = table.querySelectorAll("tr");
    if(rows.length < 2) {
        return
    }
    let oldestDate;
    for(let i=1; i<rows.length; i++) {
        let rowDateStr = rows[i].querySelector("td.date").innerText;
        if(rowDateStr && isChaseDateString(rowDateStr)) {
            oldestDate = parseChaseDateString(rowDateStr);
        }
    }
    if(oldestDate > startDate) {
        const seeMoreBtn = document.getElementById("seeMore");
        if (seeMoreBtn) {
            seeMoreBtn.click();
            setTimeout(()=>{
                scrapeTransactionData(0, startDate, endDate, lookup, linksClicked, AccountingId, ChaseAccountId);
            }, 50);
            return;
        }
    }

    // Loop through and collect data
    let prevDateStr;
    for(let i=1; i<rows.length; i++) {
        const row = rows[i];
        const dateTd = row.querySelector("td.date");
        let dateStr = dateTd.innerText;
        log("found date string " + dateStr)
        if(dateStr && isChaseDateString(dateStr)) {
            prevDateStr = dateStr;
        } else if(prevDateStr) {
            dateStr = prevDateStr;
        } else {
            log("skipping row due to bad date");
            continue;
        }
        log("parsing string " + dateStr)
        const rowDateObj = parseChaseDateString(dateStr);
        if(rowDateObj > endDateObj) {
            continue;
        }
        if(rowDateObj < startDateObj) {
            break;
        }

        const descriptionText = abbreviateDescription(row.querySelector("td.description").innerText);
        const amountText = row.querySelector("td.amount").innerText;
        const amountFloat = parseFloat(amountText.replace("$", ""));

        payload = {
            event: "rowData",
            amountFloat,
            descriptionText,
            AccountingId,
            ChaseAccountId,
        }
        chrome.runtime.sendMessage(payload, ()=>{})
    }

    // Go back to accounts list
    document.querySelector("#requestAccounts").click();
    setTimeout(()=>{
        scrapeData(startDate, endDate, lookup, linksClicked);
    })

}

function isChaseDateString(dateString) {
    // matches date like 'Apr 5, 2022'
    return /^[A-Z]{1}[a-z]{2}\s\d{1,2}\,\s\d{4}$/.test(dateString);
}

function parseChaseDateString(dateString) {
    const months = {
        Jan: 1,
        Feb: 2,
        Mar: 3,
        Apr: 4,
        May: 5,
        Jun: 6,
        Jul: 7,
        Aug: 8,
        Sep: 9,
        Oct: 10,
        Nov: 11,
        Dec: 12,
    }
    const [m, d, y] = dateString.replace(",", "").split(" ");
    const monthInt = months[m]
    if(!monthInt) {
        throw new Error("could not parse, unknown month, " + dateString);
    }
    const date = parseInt(d)
    if(!date || date > 31 || date < 1) {
        throw new Error("could not parse, unknown day of month, " + dateString);
    }
    const year = parseInt(y);
    if(!year) {
        throw new Error("could not parse, unknown year, " + dateString);
    }
    return new Date(year, monthInt, date);
}

function parseISODateString(dateString) {
    return new Date(...dateString.split("-"))
}

function abbreviateDescription(descr) {
    return descr;
}
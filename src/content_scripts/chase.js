
const log = (msg) => {
    const data = JSON.stringify(msg);
    console.log("HCE: " + data);
    chrome.runtime.sendMessage({event: "debugMessage", data});
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

const elementSelectors = new Map([
    [
        'tableContainer',
        '#accountsTableAG1Table0',
    ], [
        'viewMoreAccountsLinkContainer',
        '#seeAllAccountsAG1Table',
    ], [
        'viewAllAccountsLink',
        '#requestAccounts',
    ], [
        "accountsTableRow",
        ".data-table-for-accounts__row",
    ], [
        "accountsTableRowHeader",
        ".data-table-for-accounts__row-header",
    ],[
        "continueWithActivityBtn",
        "#continueWithActivity",
    ], [
        "transactionTable",
        "#activityTableslideInActivity",
    ], [
        "transactionTableLoader",
        ".loader-section",
    ], [
        "seeMoreTransactions",
        "#seeMore",
    ],[
        "transactionRowDate",
        "td.date",
    ], [
        "transactionRowDescription",
        "td.description",
    ], [
        "transactionRowAmount",
        "td.amount",
    ],
])

const getElementSelector = (name) => {
    if (elementSelectors.has(name)) {
        return elementSelectors.get(name);
    }
    throw new Error("Unknown element selector")
}

window.addEventListener("load", main, false);
function main () {
    const checkTableTimer = setInterval(checkForTable, 300);
    var timedOut;
    const timedOutCallbackTimer = setTimeout(()=>{
        log("timed out");
        timedOut = true;
    }, 12000)

    let onPage = false;

    function checkForTable () {
        if (document.querySelector(getElementSelector("tableContainer"))) {
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
                const isRunning = await getRunning();
                if(isRunning) {
                    return;
                }
                const lookup = await getLookupTable();
                if(lookup.size == 0) {
                    return alert("Cannot scrape, the lookup table is empty.");
                }
                sendResponse(true);
                chrome.storage.local.set({running: true}, ()=>{
                    setTimeout(()=>{
                        scrapeData(
                            {
                                startDate: request.startDate,
                                endDate: request.endDate,
                                maxAccounts: request.maxAccounts,
                                rowFilters: request.rowFilters,
                                lookup,
                                linksClicked: [],
                                results: [],
                                notices: [],
                            }
                        );
                    }, 250);
                });
            }
        } else if (request.event === "healthCheckStarted") {
            console.log("received event to start health check");
            console.log(request);
            if(onPage) {
                const isRunning = await getRunning();
                if(isRunning) {
                    return;
                }
                chrome.storage.local.set({running: true}, runHealthCheck);
            }
        }
        // // https://stackoverflow.com/questions/48107746/chrome-extension-message-not-sending-response-undefined
        return true;
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
    });
}

function confirmIfFalsy(value, message) {
    if(!value) {
        if(!confirm(message)) {
            throw new Error("User Exit");
        }
    }
}

function clickSeeAllAccountsLinkIfItsThere() {
    const element = document.querySelector(getElementSelector("viewMoreAccountsLinkContainer")
);
    if(!element) {
        log("could not find link to view all accounts");
        return;
    }
    log("clicking see all accounts link");
    element.click();
}

function canViewMoreAccounts() {
    return !!document.querySelector(getElementSelector("viewMoreAccountsLinkContainer")
);
}

function getChaseCurrentAccountNumber() {
    const res = /accountId=\d+/.exec(location.hash);
    if(res) {
        return res[0].split("=")[1];
    }
}

async function waitForElement(selector) {
    return new Promise((resolve) => {
        const inner = () => {
            const elem = document.querySelector(selector);
            if (elem) {
                resolve(elem);
            } else {
                setTimeout(inner, 100);
            }
        }
        inner();
    })
}

async function scrapeData(scrapeKwargs) {
    log("scrapeData running, checking storage for running flag")
    const running = await getRunning();
    console.log({ running });
    if(!running) {
        log("running flag=false, bye");
        return;
    }

    // Wait for table to load
    const tableContainer = await waitForElement(getElementSelector("tableContainer"))

    // Wait until table is fully expanded.
    clickSeeAllAccountsLinkIfItsThere();
    if(canViewMoreAccounts()){
        log("waiting for full account list")
        setTimeout(()=>{
            scrapeData(scrapeKwargs);
        }, 100);
        return;
    }

    // Loop through account rows (behind shadow root)
    const table = tableContainer.shadowRoot.querySelector('table');
    const tableRows = table.querySelectorAll(getElementSelector("accountsTableRow"));
    log("searching " + tableRows.length + " rows for unclicked links")
    for(let i=0; i< tableRows.length && i < scrapeKwargs.maxAccounts; i++) {
        const tr = tableRows[i];
        const rowHeader = tr.querySelector(getElementSelector("accountsTableRowHeader"));
        const rowHeaderText = rowHeader.innerText
        if(scrapeKwargs.linksClicked.indexOf(rowHeaderText) != -1) {
            continue;
        }

        // update progress bar
        chrome.runtime.sendMessage({ event: "progressBar", data: {
            value: i + 1,
            max: Math.min(tableRows.length, scrapeKwargs.maxAccounts),
        }});

        // Navigate to the account page
        log("checking row " + rowHeaderText);
        scrapeKwargs.linksClicked.push(rowHeaderText);
        tr.querySelector("a").click();

        // Check if extension has bank account number saved.
        setTimeout(()=>{
            const chaseId = getChaseCurrentAccountNumber();
            log("on transaction page for account number " + chaseId);
            if(!chaseId) {
                log("Could not find bank account id in URL, skipping.")
                scrapeKwargs.notices.push(
                    "WARNING: could not get chase account ID from URL for link: " + rowHeaderText
                );
                document.querySelector(getElementSelector("viewAllAccountsLink")).click();
                setTimeout(() => {
                    scrapeData(scrapeKwargs);
                });
                return;
            }
            if(!scrapeKwargs.lookup.has(chaseId)) {
                log("no ACCOUNTING ID found for this account");
                scrapeKwargs.notices.push("Skipping CHASE account " + chaseId + " no ACCOUNTING ID found")
                document.querySelector(getElementSelector("viewAllAccountsLink")).click();
                setTimeout(() => {
                    scrapeData(scrapeKwargs);
                });
                return;
            }
            const accountingId = scrapeKwargs.lookup.get(chaseId);
            log("row has associated ACC account " + accountingId);
            scrapeKwargs.notices.push("Scraping CHASE account " + chaseId + " matching id: " + accountingId);
            setTimeout(()=> {
                scrapeTransactionData({...scrapeKwargs, accountingId, chaseId});
            });
        }, 200);
        return;
    }
    chrome.storage.local.set({running: false}, ()=> {
        chrome.runtime.sendMessage({event: "scrapeStopped"})
    });

    downloadCSVOutput(scrapeKwargs.results, scrapeKwargs.notices);
}

function getFileNameTimestamp() {
    return (new Date()).toLocaleString().replace(/[\s\:\/\,]/g, "");
}
function downloadCSVOutput(rows, notices) {
    if(!rows.length) {
        return;
    }
    const ts = getFileNameTimestamp()
    const csvLink = document.createElement("a");
    csvLink.download = `results-${ ts }.csv`;
    const csv = rows.map((v) => {return v.join(',')}).join('\n');
    csvLink.href = encodeURI("data:text/csv," + csv);
    csvLink.click();

    const logLink = document.createElement("a");
    logLink.download = `results-${ ts }.txt`;
    const logLines = notices.join('\n');
    logLink.href = "data:text/plain;charset=utf-8," + encodeURIComponent(logLines);
    logLink.click();

    alert(
        "CSV Results are downloading to your downloads folder"
    );
}

function parseChaseAmountStringToCents(amountString) {
    if(!/^\-?\$\d/.test(amountString)) {
        throw new Error("could not parse amount text"); // starts with -$DIGIT
    }
    if(!/\.\d{2}$/.test(amountString)) {
        throw new Error("could not parse amount text"); // ends with .DIGITDIGIT
    }
    return Math.round(parseFloat(amountString.replace(/[^\-0-9]/g, "")));
}
async function scrapeTransactionData(scrapeKwargs) {

    const accountLinkHeader = scrapeKwargs.linksClicked[scrapeKwargs.linksClicked.length - 1];
    const lastName = accountLinkHeader.split(" ")[0];

    // Check for any takeovers
    const continueWithActivity = document.querySelector(
        getElementSelector("continueWithActivityBtn")
    );
    if(continueWithActivity) {
        log("clicking continue with activity button");
        scrapeKwargs.notices.push("DEBUG: pressing 'continue with activity' button");
        continueWithActivity.click();
        setTimeout(()=>{
            scrapeTransactionData(scrapeKwargs);
        });
        return;
    }

    // Wait for table to load
    log("waiting for transaction table to load");
    const table = await waitForElement(getElementSelector("transactionTable"));

    // Waiting for "see all activity" rows to load
    const loaderElem = document.querySelector(
        getElementSelector("transactionTableLoader")
    );
    if(loaderElem) {
        log("DEBUG: found loader element, waiting..");
        setTimeout(()=>{
            scrapeTransactionData(scrapeKwargs);
        }, 120);
        return;
    }

    const startDateObj = parseISODateString(scrapeKwargs.startDate);
    const endDateObj = parseISODateString(scrapeKwargs.endDate);

    // Keep clicking "See more activity" until
    //  - oldest transaction date < startDate
    //  - OR the see more activity button no longer appears.
    const rows = table.querySelectorAll("tr");
    if(rows.length < 2) {
        return
    }
    let oldestDate;
    for(let i=1; i<rows.length; i++) {
        let rowDateStr = rows[i].querySelector(
            getElementSelector("transactionRowDate")
        ).innerText;
        if(rowDateStr && isChaseDateString(rowDateStr)) {
            oldestDate = parseChaseDateString(rowDateStr);
        }
    }
    if(oldestDate >= startDateObj) {
        const seeMoreBtn = document.querySelector(
            getElementSelector("seeMoreTransactions")
        );
        if (seeMoreBtn) {
            seeMoreBtn.click();
            setTimeout(()=>{
                scrapeTransactionData(scrapeKwargs);
            }, 50);
            return;
        }
    }

    // Loop through and collect data
    let prevDateStr;
    for(let i=1; i<rows.length; i++) {
        const row = rows[i];
        const dateTd = row.querySelector(getElementSelector("transactionRowDate"));
        let dateStr = dateTd.innerText;
        log("found date string " + dateStr)
        if(dateStr && isChaseDateString(dateStr)) {
            prevDateStr = dateStr;
        } else if(prevDateStr) {
            dateStr = prevDateStr;
        } else {
            log("skipping row due to bad date");
            scrapeKwargs.notices.push("INFO: skipping row due to bad date, on page " + accountLinkHeader);
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

        const descriptionText = row.querySelector(
            getElementSelector("transactionRowDescription")
        ).innerText;
        const amountText = row.querySelector(
            getElementSelector("transactionRowAmount")
        ).innerText;
        const amountCents = parseChaseAmountStringToCents(amountText);

        let csvRow;
        try {
            csvRow = processRow({
                amountCents,
                descriptionText,
                lastName,
                rowDateObj,
                accountingId: scrapeKwargs.accountingId,
                chaseId: scrapeKwargs.chaseId,
            }, scrapeKwargs.rowFilters);
        }
        catch (err) {
            scrapeKwargs.notices.push(err.message);
        }
        if(csvRow) {
            log("recording CSV row")
            scrapeKwargs.results.push(csvRow);
        }
    }

    delete scrapeKwargs.accountingId;
    delete scrapeKwargs.chaseId;

    // Go back to accounts list
    document.querySelector(getElementSelector("viewAllAccountsLink")).click();
    setTimeout(()=>{
        scrapeData(scrapeKwargs);
    });

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
    return new Date(year, monthInt - 1, date);
}

function parseISODateString(dateString) {
    if(!/^\d{4}\-[0-1][0-9]\-[0-3][0-9]$/.test(dateString)) {
        throw new Error("Could not parse ISO date string: " + dateString);
    }
    const parts = dateString.split("-");
    parts[1] = parseInt(parts[1]) - 1;
    return new Date(...parts);
}

function abbreviateDescription(row) {
    let memoParts = [];

    // Last name
    memoParts.push(row.lastName);

    // Date
    const dateStr = row.rowDateObj.toISOString().slice(0, 10);
    memoParts.push(
        `${dateStr.split("-")[1]}/${dateStr.split("-")[2]}`
    );

    if(/^check\#\d+$/.test(row.descriptionText.toLowerCase().replace(/\s/g, ''))) {
        // Check number.
        let checkNumPart = row.descriptionText.replace("#", "").replace(/\s/g, '');
        memoParts.push(checkNumPart);
    } else {
        // Misc transaction description.
        let cleanedDescr = (
            row.descriptionText
                .toLowerCase()
                .split("")
                .filter(c=>/[a-z0-9]/.test(c))
                .join("")
                .slice(0, 10)
        );
        memoParts.push(cleanedDescr);
    }

    return memoParts.join(" ");
}

function processRow(row, rowFilters) {
    console.log({processingRow: row});

    // check if we should skip
    const cleanedDesc = row.descriptionText.toLowerCase().replace(/\s/g, "");
    let skip = false;
    for(let i=0; i<rowFilters.length; i++) {
        const filt = rowFilters[i];
        if(filt.TYPE === "exclude") {
            skip = filt.AND.filter(val => cleanedDesc.indexOf(val) != -1).length == filt.AND.length;
            if(!skip) {
                continue;
            }
            skip = filt.OR.filter(val => cleanedDesc.indexOf(val) != -1).length > 0 || filt.OR.length == 0;
            if(skip) {
                log("skipping row " + row.descriptionText);
                throw new Error("excl filter: SKIPPING " + row.descriptionText);
            }
        }
        else if (filt.TYPE === "include") {
            skip = filt.AND.filter(val => cleanedDesc.indexOf(val) != -1).length != filt.AND.length;
            if(skip) {
                log("skipping row " + row.descriptionText);
                throw new Error("incl filter: SKIPPING " + row.descriptionText);
            }
            if(filt.OR.length == 0){
                continue;
            }
            skip = filt.OR.filter(val => cleanedDesc.indexOf(val) != -1).length == 0;
            if(skip) {
                log("skipping row " + row.descriptionText);
                throw new Error("incl filter: SKIPPING " + row.descriptionText);
            }
        }
        else {
            throw new Error("unknown type")
        }
    }

    /*
        accountId,
        memo,
        debitAmount,
        creditAbout,
    */
    const dr = row.amountCents > 0 ? (row.amountCents / 100).toFixed(2) : 0;
    const cr = row.amountCents < 0 ? (row.amountCents / -100).toFixed(2) : 0;
    return [
        row.accountingId,
        abbreviateDescription(row),
        dr,
        cr,
    ];
}

/*
 *
 *
 * HEALTH
 * CHECK
 *
 *
 */
const WAIT_FOR_ACCOUNT_TABLE_TOKEN = "WAIT_FOR_ACCOUNT_TABLE_TOKEN";
async function selectAccountTable() {
    return new Promise((resolve) => {
        const inner = () => {
            let table;
            try {
                const tableContainer = document.querySelector(getElementSelector("tableContainer"));
                table = tableContainer.shadowRoot.querySelector('table');
            } catch (err) {
                return setTimeout(inner, 25);
            }
            if (!table) {
                return setTimeout(inner, 25);
            }
            resolve(table);
        }
        setTimeout(inner);
    });
}
function clickAccountsButton() {
    document.querySelector(getElementSelector("viewAllAccountsLink")).click();
}
const TESTS = [
    {
        name:"Account table is findable and has findable rows",
        cb: async function() {
            const tableContainer = document.querySelector(getElementSelector("tableContainer"));
            if(!tableContainer) {
                return "could not find container accountsTableAG1Table0"
            }
            const table = tableContainer.shadowRoot.querySelector('table');
            if(!table) {
                return "could not find nested table"
            }
            const tableRows = table.querySelectorAll(getElementSelector("accountsTableRow"));
            if (tableRows.length < 5) {
                return"accounts table as too few rows"
            }
        },
    },
    {
        name:"Account table column has findable link to transaction table",
        cb: async function() {
            const table = await selectAccountTable();
            const tableRows = table.querySelectorAll("tr");
            const row = tableRows[4];
            const headerCol = row.querySelector(getElementSelector("accountsTableRowHeader"));
            if(!headerCol) {
                return "could not find row heading";
            }
            const anchor = headerCol.querySelector("a")
            if(!anchor) {
                return "could not find clickable link"
            }
        },
    },
    {
        name:"Can click on row header link and navigate account details",
        cb: async function() {
            const tableHash = location.hash;
            const table = await selectAccountTable();
            const tableRows = table.querySelectorAll("tr");
            const anchor = tableRows[4].querySelector("th").querySelector("a");
            anchor.click();
            return new Promise((resolve) => {
                setTimeout(()=>{
                    const detailsHash = location.hash;
                    if(tableHash == detailsHash) {
                        resolve("URL did not update after clicking link")
                    } else {
                        resolve();
                    }
                }, 25);
            })
        },
    },
    {
        name:"Can get account ID from URL",
        cb: async function() {
            return new Promise((resolve) => {
                setTimeout(()=>{
                    const accountId = getChaseCurrentAccountNumber();
                    if(accountId && /^\d{6,}$/.test(accountId)) {
                        log("found account id " + accountId);
                        resolve();
                    } else {
                        resolve("could not get account id from URL");
                    }
                });
            })
        },
    },
    {
        name: "Can select transaction table",
        cb: async function() {
            const errMsg = await new Promise((resolve) => {
                let attemptNumber = 0
                const inner = async () => {
                    const table = await waitForElement(getElementSelector("transactionTable"));
                    if(!table) {
                        attemptNumber++;
                        if(attemptNumber > 100) {
                            return resolve("could not find transaction table")
                        }
                        setTimeout(inner, 100);
                    } else {
                        clickAccountsButton();
                        setTimeout(resolve);
                        return;
                    }
                }
                setTimeout(inner);
            });
            if(errMsg) {
                return errMsg;
            }
        },
    },
    WAIT_FOR_ACCOUNT_TABLE_TOKEN,
    {
        name: "Can find loading message when transactions are loading",
        cb: async function() {
            const acctable = await selectAccountTable();
            const tableRows = acctable.querySelectorAll("tr");
            const anchor = tableRows[4].querySelector("th").querySelector("a");
            anchor.click();
            let errMsg = await new Promise((resolve) => {
                const inner = async () => {
                    const table = await waitForElement(getElementSelector("transactionTable"));
                    if(!table) {
                        attemptNumber++;
                        if(attemptNumber > 100) {
                            return resolve("could not find transaction table");
                        }
                        setTimeout(inner, 100);
                        return;
                    }
                    const loaderElem = document.querySelector(
                        getElementSelector("transactionTableLoader")
                    );
                    if(!loaderElem) {
                        attemptNumber++;
                        if(attemptNumber > 100) {
                            return resolve("could not find loading message")
                        }
                        setTimeout(inner);
                        return;
                    } else {
                        resolve();
                    }
                };
                setTimeout(inner);
            });
            if(errMsg) {
                return errMsg;
            }
        },
    },
    {
        name: "Can select 5+ transaction table rows",
        cb: async function() {
            let errMsg = await new Promise((resolve) => {
                let attemptNumber = 0
                const inner = async () => {
                    const table = await waitForElement(getElementSelector("transactionTable"));
                    if(!table) {
                        attemptNumber++;
                        if(attemptNumber > 100) {
                            return resolve("could not find transaction table")
                        }
                        setTimeout(inner, 100);
                        return;
                    }
                    const rows = table.querySelectorAll("tr");
                    if(rows.length < 5) {
                        attemptNumber++;
                        if(attemptNumber > 100) {
                            return resolve("could not find transaction table rows")
                        }
                        setTimeout(inner, 100);
                        return;
                    }
                    clickAccountsButton();
                    setTimeout(resolve);
                    return;
                }
                setTimeout(inner);
            });
            if(errMsg) {
                return errMsg;
            }
        },
    },
    WAIT_FOR_ACCOUNT_TABLE_TOKEN,
    {
        name: "Can select 'see more transactions' link to load more transactions.",
        cb: async function() {
            const acctable = await selectAccountTable();
            const tableRows = acctable.querySelectorAll("tr");
            const anchor = tableRows[1].querySelector("th").querySelector("a");
            anchor.click();
            let errMsg = await new Promise((resolve) => {
                let attemptNumber = 0
                const inner = async () => {
                    const table = await waitForElement(getElementSelector("transactionTable"));
                    if(!table) {
                        attemptNumber++;
                        if(attemptNumber > 100) {
                            return resolve("could not find transaction table")
                        }
                        setTimeout(inner, 100);
                        return;
                    }
                    const loaderElem = document.querySelector(
                        getElementSelector("transactionTableLoader")
                    );
                    if(loaderElem) {
                        log("found loader element, waiting..");
                        setTimeout(inner, 50);
                        return;
                    }
                    const seeMoreBtn = document.querySelector(
                        getElementSelector("seeMoreTransactions")
                    );
                    if(!seeMoreBtn) {
                        resolve("could not find 'see more transactions' button.")
                    } else {
                        resolve();
                    }
                };
                setTimeout(inner);
            });
            if(errMsg) {
                return errMsg;
            }
        },
    },
    {
        name: "Date column is selectable and has expected format",
        cb: async function () {
            const table = await waitForElement(getElementSelector("transactionTable"));
            const rows = table.querySelectorAll("tr");
            for(let i=1; i< rows.length; i++) {
                const row = rows[i];
                const dateTd = row.querySelector(getElementSelector("transactionRowDate"));
                if(dateTd) {
                    const text = dateTd.innerText;
                    if(!text) {
                        continue;
                    }
                    if(isChaseDateString(text) && parseChaseDateString(text)) {
                        log("chase date string format matches")
                        return;
                    }
                }
            }
            return "could not find date column"
        },
    },
    {
        name: "Description column is selectable and has some text",
        cb: async function () {
            const table = await waitForElement(getElementSelector("transactionTable"));
            const rows = table.querySelectorAll("tr");
            for(let i=1; i< rows.length; i++) {
                const row = rows[i];
                const descTd = row.querySelector(getElementSelector("transactionRowDescription"));
                if(!descTd) {
                    continue;
                }
                if(!descTd.innerText) {
                    continue;
                }
                return;
            }
            return "could not find description column or text"
        }
    },
    {
        name: "can find and parse transaction amount",
        cb: async function () {
            const table = await waitForElement(getElementSelector("transactionTable"));
            const rows = table.querySelectorAll("tr");
            for(let i=1; i< rows.length; i++) {
                const row = rows[i];
                const amountTd = row.querySelector(getElementSelector("transactionRowAmount"));
                if(!amountTd) {
                    continue;
                }
                if(!amountTd.innerText) {
                    continue;
                }
                const amountCents = parseChaseAmountStringToCents(amountTd.innerText);
                if(amountCents > 1 || amountCents < -1) {
                    return;
                }
            }
            return "could not find amount column or text";
        },
    },
    {
        name: "row confirmation test",
        cb: async function() {
            const table = await waitForElement(getElementSelector("transactionTable"));
            const rows = table.querySelectorAll("tr");
            for(let i=1; i< rows.length; i++) {
                const row = rows[i];
                const amountTd = row.querySelector(getElementSelector("transactionRowAmount"));
                if(!amountTd) {
                    continue;
                }
                if(!amountTd.innerText) {
                    continue;
                }
                const amountCents = parseChaseAmountStringToCents(amountTd.innerText);
                const descTd = row.querySelector(getElementSelector("transactionRowDescription"));
                if(!descTd) {
                    continue;
                }
                if(!descTd.innerText) {
                    continue;
                }
                const descriptionText = descTd.innerText;
                const dateTd = row.querySelector(getElementSelector("transactionRowDate"));
                if(!dateTd) {
                    continue;
                }
                const dateText = dateTd.innerText;
                if(!dateText) {
                    continue;
                }
                if(!isChaseDateString(dateText) ) {
                    continue;
                }
                row.style.backgroundColor = "rgb(255, 255, 0, 0.2)";
                try {
                    row.scrollIntoView({ block: "center" });
                } catch (err) {
                    console.warn("could not call scrollIntoView");
                }
                const dateObj = parseChaseDateString(dateText);
                const ISODate = dateObj.toISOString().slice(0, 10)
                const confirmText = [
                    "Please confirm the collected data matches the highlighted row:",
                    ` - Date (YYYY-MM-DD): ${ISODate}`,
                    ` - Amount (in cents): ${amountCents.toLocaleString()}`,
                    ` - Description text: ${descriptionText}`,
                    'Click OK to confirm this data is correct.',
                ]
                // Enter new async block so row highlight shows up on the DOM
                // before user is prompted with an alert.
                return await new Promise((resolve) => {
                    setTimeout(() => {
                        if(!confirm(confirmText.join("\n"))) {
                            resolve("could not confirm correct transaction data");
                        } else {
                            resolve();
                        }
                    }, 100);
                });
            }
            return "could not confirm correct transaction data";
        },
    }
]
async function _runHealthCheck() {
    const alertOut = [];
    let anyFailed = false;
    for(let i in TESTS) {
        const isRunning = await getRunning();
        if (!isRunning) {
            return;
        }

        chrome.runtime.sendMessage({ event: "progressBar", data: {
            value: i,
            max: TESTS.length,
        }});


        if(TESTS[i] === WAIT_FOR_ACCOUNT_TABLE_TOKEN) {
            log("waiting for table")
            await selectAccountTable();
            log("found table")
            continue;
        }

        log("running test index " + i);

        let result;
        let passed;
        try {
            result = await TESTS[i].cb();
            passed = !result
        } catch(err) {
            alertOut.push(`ERROR: ${TESTS[i].name}\n${err.message}`);
            log(`ERROR: ${TESTS[i].name}`)
            anyFailed = true;
        }
        if(result) {
            alertOut.push(`FAIL: ${TESTS[i].name}`);
            alertOut.push(`  ${result}`);
            log(`FAIL: ${TESTS[i].name}`)
            anyFailed = true;
            break;
        } else if (passed) {
            alertOut.push(`OK: ${TESTS[i].name}`);
            log(`PASS: ${TESTS[i].name}`)
        }
    }

    log("tests complete");
    if(anyFailed) {
        alertOut.unshift("❌ FAIL * * * * *");
    } else {
        alertOut.unshift("✅ OK - all tests pass");
    }
    clickAccountsButton();
    setTimeout(() => {
        alert(alertOut.join("\n"));
    }, 500);
}

async function runHealthCheck() {
    try {
        await _runHealthCheck();
    }
    catch(err) {
        alert("Error:\n" + err.message)
        throw err
    } finally {
        chrome.storage.local.set({running: false}, ()=> {
            chrome.runtime.sendMessage({event: "healthCheckStopped"})
        });
    }
}
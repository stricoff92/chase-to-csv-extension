
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
                                skipped: [],
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
    const element = document.querySelector("#seeAllAccountsAG1Table");
    if(!element) {
        log("could not find link to view all accounts");
        return;
    }
    log("clicking see all accounts link");
    element.click();
}

function canViewMoreAccounts() {
    return !!document.querySelector("#seeAllAccountsAG1Table");
}

function getChaseCurrentAccountNumber() {
    const res = /accountId=\d+/.exec(location.hash);
    if(res) {
        return res[0].split("=")[1];
    }
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
    const tableContainer = document.getElementById("accountsTableAG1Table0");
    if(!tableContainer) {
        log("could not find table container, waiting")
        setTimeout(()=>{
            scrapeData(scrapeKwargs)
        }, 200);
        return
    }

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
    const tableRows = table.querySelectorAll(".data-table-for-accounts__row");
    log("searching " + tableRows.length + " rows for unclicked links")
    for(let i=0; i< tableRows.length && i < scrapeKwargs.maxAccounts; i++) {
        const tr = tableRows[i];
        const rowHeader = tr.querySelector(".data-table-for-accounts__row-header");
        const rowHeaderText = rowHeader.innerText
        if(scrapeKwargs.linksClicked.indexOf(rowHeaderText) != -1) {
            continue;
        }

        // update progress bar
        chrome.runtime.sendMessage({ event: "progressBar", data: {
            value: i + 1,
            max: tableRows.length,
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
                document.querySelector("#requestAccounts").click();
                setTimeout(() => {
                    scrapeData(scrapeKwargs);
                });
                return;
            }
            if(!scrapeKwargs.lookup.has(chaseId)) {
                log("no ACCOUNTING ID found for this account");
                document.querySelector("#requestAccounts").click();
                setTimeout(() => {
                    scrapeData(scrapeKwargs);
                });
                return;
            }
            const accountingId = scrapeKwargs.lookup.get(chaseId);
            log("row has associated ACC account " + accountingId);
            setTimeout(()=> {
                scrapeTransactionData({...scrapeKwargs, accountingId, chaseId});
            });
        }, 200);
        return;
    }
    chrome.storage.local.set({running: false}, ()=> {
        chrome.runtime.sendMessage({event: "scrapeStopped"})
    });

    downloadCSVOutput(scrapeKwargs.results, scrapeKwargs.skipped)
}

function getFileNameTimestamp() {
    return (new Date()).toLocaleString().replace(/[\s\:\/\,]/g, "");
}
function downloadCSVOutput(rows, skipped) {
    if(!rows.length) {
        return;
    }
    const tempLink = document.createElement("a");
    tempLink.download = `results-${ getFileNameTimestamp() }.csv`;
    const csv = rows.map((v) => {return v.join(',')}).join('\n');
    tempLink.href = encodeURI("data:text/csv," + csv);
    tempLink.click();
    alert(
        "CSV Results are downloading to your downloads folder. Skipped:\n"
        + skipped.join("\n")
    );
}

async function scrapeTransactionData(scrapeKwargs) {

    const accountLinkHeader = scrapeKwargs.linksClicked[scrapeKwargs.linksClicked.length - 1];
    const lastName = accountLinkHeader.split(" ")[0];

    // Check for any takeovers
    const continueWithActivity = document.getElementById("continueWithActivity");
    if(continueWithActivity) {
        log("clicking continue with activity button")
        continueWithActivity.click();
    }

    // Wait for table to load, or timeout.
    const table = document.getElementById('activityTableslideInActivity');
    if (!table) {
        setTimeout(()=>{
            scrapeTransactionData(scrapeKwargs)
        }, 120);
        return;
    }

    // Waiting for "see all activity" rows to load
    const loaderElem = document.querySelector(".loader-section");
    if(loaderElem) {
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
        let rowDateStr = rows[i].querySelector("td.date").innerText;
        if(rowDateStr && isChaseDateString(rowDateStr)) {
            oldestDate = parseChaseDateString(rowDateStr);
        }
    }
    if(oldestDate >= startDateObj) {
        const seeMoreBtn = document.getElementById("seeMore");
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

        const descriptionText = row.querySelector("td.description").innerText;
        const amountText = row.querySelector("td.amount").innerText;
        const amountCents = Math.round(parseFloat(amountText.replace("$", "")) * 100);

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
            scrapeKwargs.skipped.push(err.message);
        }
        if(csvRow) {
            log("recording CSV row")
            scrapeKwargs.results.push(csvRow);
        }
    }

    delete scrapeKwargs.accountingId;
    delete scrapeKwargs.chaseId;

    // Go back to accounts list
    document.querySelector("#requestAccounts").click();
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
    return new Date(year, monthInt, date);
}

function parseISODateString(dateString) {
    return new Date(...dateString.split("-"))
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
        skip = filt.AND.filter(val => cleanedDesc.indexOf(val) != -1).length == filt.AND.length;
        if(!skip) {
            continue;
        }
        skip = filt.OR.filter(val => cleanedDesc.indexOf(val) != -1).length > 0;
        if(skip) {
            log("skipping transfer row " + row.descriptionText);
            throw new Error("row filter: " + row.descriptionText);
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
const WAIT_FOR_TABLE_TOKEN = "WAIT_FOR_TABLE_TOKEN";
function selectTable() {
    const tableContainer = document.querySelector("#accountsTableAG1Table0");
    const table = tableContainer.shadowRoot.querySelector('table');
    if (!table) {
        throw new Error("Could not find table");
    }
    return table;
}
function clickAccountsButton() {
    document.querySelector("#requestAccounts").click();
}
const TESTS = [
    {
        name:"Account table is findable and has findable rows",
        cb: async function() {
            const tableContainer = document.querySelector("#accountsTableAG1Table0");
            if(!tableContainer) {
                return "could not find container accountsTableAG1Table0"
            }
            const table = tableContainer.shadowRoot.querySelector('table');
            if(!table) {
                return "could not find nested table"
            }
            const tableRows = table.querySelectorAll(".data-table-for-accounts__row");
            if (tableRows.length < 5) {
                return"accounts table as too few rows"
            }
        }
    },
    {
        name:"Account table column has findable link to transaction table",
        cb: async function() {
            const table = selectTable();
            const tableRows = table.querySelectorAll("tr");
            const row = tableRows[4];
            const headerCol = row.querySelector(".data-table-for-accounts__row-header");
            if(!headerCol) {
                return "could not find row heading";
            }
            const anchor = headerCol.querySelector("a")
            if(!anchor) {
                return "could not find clickable link"
            }
        }
    },
    {
        name:"Can click on row header link and navigate back to account table",
        cb: async function() {
            const tableHash = location.hash;
            const table = selectTable();
            const tableRows = table.querySelectorAll("tr");
            const anchor = tableRows[4].querySelector("th").querySelector("a");
            anchor.click();
            return new Promise((resolve) => {
                setTimeout(()=>{
                    const detailsHash = location.hash;
                    if(tableHash == detailsHash) {
                        clickAccountsButton();
                        resolve("URL did not update after clicking link")
                    } else {
                        clickAccountsButton();
                        resolve();
                    }
                }, 25);
            })
        }
    },
    WAIT_FOR_TABLE_TOKEN,
    {
        name:"",
        cb: async function() {
        }
    },
]
async function _runHealthCheck(offset) {
    const _offset = offset || 0;
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


        if(TESTS[i] === WAIT_FOR_TABLE_TOKEN) {
            log("waiting for table")
            await new Promise((resolve, reject) => {
                const inner = (attempts) => {
                    if(attempts > 1000) {
                        return reject();
                    }
                    let table;
                    try {
                        table = selectTable();
                        resolve();
                    } catch (err) {
                        log("waiting for table...")
                        setTimeout(()=>{
                            inner(attempts + 1);
                        }, 25);
                    }
                }
                inner(0);
            });
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
            alertOut.push(`ERROR: ${TESTS[i].name}`);
            log(`ERROR: ${TESTS[i].name}`)
            anyFailed = true;
        }
        if(result) {
            alertOut.push(`FAIL: ${TESTS[i].name}`);
            alertOut.push(`  ${result}`);
            log(`FAIL: ${TESTS[i].name}`)
            anyFailed = true;
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
    setTimeout(() => {
        alert(alertOut.join("\n"));
    }, 500);
}

async function runHealthCheck() {
    try {
        await _runHealthCheck();
    }
    catch(err) {
        throw err
    } finally {
        chrome.storage.local.set({running: false}, ()=> {
            chrome.runtime.sendMessage({event: "healthCheckStopped"})
        });
    }
}
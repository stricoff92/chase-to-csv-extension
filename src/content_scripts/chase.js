
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

function isoDateToHumanDate(isoDate) {
    if(!/^\d{4}\-[0-1][0-9]\-[0-3][0-9]$/.test(isoDate)) {
        throw new Error("Could not parse ISO date string: " + isoDate);
    }
    const dropLeadingZero = numStr => numStr.replace(/^0/, '');
    const parts = isoDate.split("-");
    months = {
        "01": "Jan", "02": "Feb", "03": "Mar", "04": "Apr", "05": "May",
        "06": "Jun", "07": "Jul", "08": "Aug", "09": "Sep", "10": "Oct",
        "11": "Nov", "12": "Dec",
    };
    return `${months[parts[1]]} ${dropLeadingZero(parts[2])}, ${parts[0]}`;
}

function updateLastRunData(scrapeKwargs) {
    const payload = {
        lastRunCompleted: isoDateToHumanDate((new Date()).toISOString().slice(0, 10)),
        lastRunFrom: isoDateToHumanDate(scrapeKwargs.startDate),
        lastRunTo: isoDateToHumanDate(scrapeKwargs.endDate),
    };
    chrome.storage.local.set(payload);
}

const elementSelectors = new Map([
    [
        'tableContainer', // wraps a shadowroot which contains the table
        // '#accountsTableAG1Table0',
        // '#account-table-DDA',
        "mds-data-table-for-accounts",
        // "mds-tile#ACTIVITY_Id",
    ], [
        'viewMoreAccountsLinkContainer',
        '#seeAllAccountsAG1Table',
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
        "#ACTIVITY-dataTableId-mds-diy-data-table",
    ], [
        "transactionTableLoader",
        ".loader-section",
    ], [
        "seeMoreTransactions",
        "#see-more-button_id",
    ],[
        "transactionRowDate",
        "th",
    ], [
        "transactionRowDescription",
        "td:nth-of-type(1)",
    ], [
        "transactionRowAmount",
        "td:nth-of-type(3)",
    ], [
        "transactionRowBalance",
        "td:nth-of-type(4)",
    ], [
        "fullAccountNumberLinkContainer",
        "#primary-additional-item-account-and-routing-number",
    ], [
        "accountNumber",
        ".account-number",
    ]
]);

const getElementSelector = (name) => {
    if (elementSelectors.has(name)) {
        return elementSelectors.get(name);
    }
    throw new Error("Unknown element selector");
}

window.addEventListener("load", main, false);
function main () {
    const checkTableTimer = setInterval(checkForTable, 300);
    var timedOut;
    const timedOutCallbackTimer = setTimeout(()=>{
        log("timed out");
        timedOut = true;
    }, 20000);

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

    chrome.runtime.onMessage.addListener(async (request, sender, sendResponse) => {
         if (request.event === "scrapeStarted") {
            log("received event to start scraping");
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
                                csvRows: request.csvRows,
                                prefixColumns: request.prefixColumns,
                                maxAccounts: request.maxAccounts,
                                rowFilters: request.rowFilters,
                                accFilters: request.accFilters,
                                plugAccountId: request.plugAccountId,
                                lookup,
                                linksClicked: [],
                                results: [],
                                notices: [`INFO Scrape Started, KWARGS ${JSON.stringify(request)}`],
                            }
                        );
                    }, 250);
                });
            }
        }
        else if (request.event === "healthCheckStarted") {
            log("received event to start health check");
            console.log(request);
            if(onPage) {
                const isRunning = await getRunning();
                if(isRunning) {
                    return;
                }
                chrome.storage.local.set({running: true}, runHealthCheck);
            }
        }
        else if (request.event === "BalanceScrapeStarted") {
            log("received event to start balance scrape");
            console.log(request);
            if(onPage) {
                const isRunning = await getRunning();
                if(isRunning) {
                    return;
                }
                const lookup = await getLookupTable();
                // sendResponse(true);
                chrome.storage.local.set({running: true}, ()=>{
                    createBalanceCSVLoopAccounts({
                        endDate: request.endDate,
                        lookup,
                        linksClicked: [],
                        resultRows: [[
                            "name",
                            "chase_id",
                            "accounting_id",
                            "balance_transaction_date",
                            `balance_on_${request.endDate}`,
                        ]],
                        notices: [`INFO Balance Scrape Started, KWARGS ${JSON.stringify(request)}`],
                    });
                });
            }
        }
        // // https://stackoverflow.com/questions/48107746/chrome-extension-message-not-sending-response-undefined
        return true;
    });
}

function validateTransactionTableHasExpectedColumnHeaders(thead) {
    // chase removed sensible class names from their td elements,
    // so lets look at the column headers in <thead> to make sure they match
    // expected values.
    const tableHeadElements = thead.querySelectorAll("th");
    const expectedHeaderValues = [
        "Date",
        "Description",
        "Type",
        "Amount",
        "Balance",
    ];
    for(let i=0; i<expectedHeaderValues.length; i++) {
        if(tableHeadElements[i].innerText != expectedHeaderValues[i]) {
            throw new Error(
                `Expected column header ${expectedHeaderValues[i]} at index ${i}, got ${tableHeadElements[i].innerText}`
            );
        }
    }
}

function validateRowsStartWithColumnHeadersRow(rows) {
    if(rows[0].classList[0].indexOf("header-row") == -1) {
        throw new Error("Could not find header row in transaction table rows list");
    }
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
    const element = document.querySelector(
        getElementSelector("viewMoreAccountsLinkContainer")
    );
    if(!element) {
        log("could not find link to view all accounts");
        return;
    }
    log("clicking see all accounts link");
    element.click();
}

function scrollToBottom() {
    window.scrollTo(0, document.body.scrollHeight);
}

function canViewMoreAccounts() {
    return !!document.querySelector(getElementSelector("viewMoreAccountsLinkContainer"));
}

function getChaseCurrentAccountNumber() {
    const res = /accountId=\d+/.exec(location.hash);
    if(res) {
        return res[0].split("=")[1];
    }
}

function findAndCloseModal() {
    (document.querySelector("mds-dialog-modal")
        .shadowRoot.querySelector("mds-sticky-footer")
        .shadowRoot.querySelector("mds-button")
        .click()
    );
}

async function openAccountDetailsModal() {
    await new Promise(resolve => {
        const innerWait = () => {
            const selector = getElementSelector("fullAccountNumberLinkContainer");
            try {
                // document.querySelectorAll(selector)[0].childNodes[0].shadowRoot.querySelector("a").click();
                document.querySelector(selector).click();
                resolve();
            } catch(err) {
                console.warn("could not find 'see full account number, waiting'");
                setTimeout(innerWait, 20);
            }
        };
        innerWait();
    });
}

async function getChaseAccountNumberFromModal(closeModal) {
    return await new Promise(resolve => {
        const inner = () => {
            log("looking for account number...");
            let accountNumberContainer;
            try {
                accountNumberContainer = (
                    document
                        .querySelector(getElementSelector("accountNumber"))
                        .querySelector("mds-description-list")
                        .shadowRoot.querySelector(".description-list")
                );
            } catch(err) {
                log("error when querying for account number, will try again");
                return setTimeout(inner, 200);
            }
            const containerParts = accountNumberContainer.innerText.split("\n");
            log(`found inner text ${accountNumberContainer.innerText}, parts ${containerParts}`);
            if(
                accountNumberContainer
                && containerParts.length == 4
                && containerParts[0] == "Account number"
                && /^\d+$/.test(containerParts[1])
            ) {
                const result = accountNumberContainer.innerText.split("\n")[1];
                log(`found result ${result}`);
                if(closeModal) {
                    findAndCloseModal();
                }
                resolve(result);
            } else {
                log("could not find account number, trying again...");
                return setTimeout(inner, 200);
            }
        }
        inner();
    });
}

async function waitForElement(selector) {
    return new Promise((resolve) => {
        const inner = () => {
            log("waiting for element: " + selector);
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

function getAccountTableFromContainer(tableContainer) {
    return tableContainer.shadowRoot.querySelector('table');
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
    if(canViewMoreAccounts()){
        log("waiting for full account list");
        clickSeeAllAccountsLinkIfItsThere();
        setTimeout(()=>{
            scrapeData(scrapeKwargs);
        }, 25);
        return;
    }

    // Loop through account rows (behind shadow root)
    const table = getAccountTableFromContainer(tableContainer);
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

        // Check if we should skip this account
        let skip = false
        const sanitizedRowHeaderText = rowHeaderText.toLowerCase().replace(/\s/g, "");
        for(let i in scrapeKwargs.accFilters) {
            const filt = scrapeKwargs.accFilters[i];
            if (filt.INCLUDE.length > 0) {
                skip = filt.INCLUDE.filter(key => sanitizedRowHeaderText.indexOf(key) != -1).length == 0;
                if(skip) {
                    scrapeKwargs.notices.push(`INFO skipping account ${rowHeaderText}, no INCLUDE match`);
                    break;
                }
            }
            if (filt.EXCLUDE.length > 0) {
                skip = filt.EXCLUDE.filter(key => sanitizedRowHeaderText.indexOf(key) != -1).length > 0;
                if(skip) {
                    scrapeKwargs.notices.push(`INFO skipping account ${rowHeaderText}, has EXCLUDE match`);
                    break;
                }
            }
        }
        if(skip) {
            log("skipping account row " + rowHeaderText)
            continue;
        }

        // Navigate to the account page
        log("clicking account link " + rowHeaderText);
        scrapeKwargs.linksClicked.push(rowHeaderText);
        setTimeout(() => {
            tr.querySelector("a").click();
        }, 150);

        // Check if extension has bank account number saved.
        setTimeout(async ()=>{
            await openAccountDetailsModal();
            const chaseId = await getChaseAccountNumberFromModal(true);
            log("on transaction page for account number " + chaseId);
            if(!chaseId) {
                alert(
                    "ERROR: could not get chase account ID from webpage: " + rowHeaderText
                );
                return;
            }
            if(!scrapeKwargs.lookup.has(chaseId)) {
                log("no ACCOUNTING ID found for this account");
                scrapeKwargs.notices.push(
                    `WARNING skipping CHASE account ${rowHeaderText} (${chaseId}) no ACCOUNTING ID found`
                );
                clickAccountsButton();
                setTimeout(() => {
                    scrapeData(scrapeKwargs);
                });
                return;
            }
            const accountingId = scrapeKwargs.lookup.get(chaseId);
            log("row has associated ACC account " + accountingId);
            scrapeKwargs.notices.push(
                `INFO Scraping CHASE account ${rowHeaderText} (${chaseId}) matching id: ${accountingId}`
            );
            setTimeout(()=> {
                scrapeTransactionData({...scrapeKwargs, accountingId, chaseId}, rowHeaderText);
            });
        }, 300);
        return;
    }
    chrome.storage.local.set({running: false}, ()=> {
        chrome.runtime.sendMessage({event: "scrapeStopped"})
    });

    downloadCSVOutput(
        scrapeKwargs.results,
        scrapeKwargs.notices,
    );
    updateLastRunData(scrapeKwargs);
}

function getFileNameTimestamp() {
    return (new Date()).toLocaleString().replace(/[\s\:\/\,]/g, "");
}
function downloadCSVOutput(rows, notices) {
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

function cleanAmountString(amountString) {
    const cleanedChars = []
    for(let i=0; i<amountString.length; i++) {
        const originalChar = amountString.charAt(i);
        const originalCharCode = originalChar.charCodeAt(0);
        if(originalCharCode == 10) { // new line. number is repeated in markup due to accessibility?
            break;
        }
        else if(originalCharCode == 8722) { // unicode minus sign, replace with ascii minus
            cleanedChars.push("-");
        }
        else {
            cleanedChars.push(originalChar);
        }
    }
    return cleanedChars.join("");
}

function parseChaseAmountStringToCents(amountString) {
    amountString = cleanAmountString(amountString);
    if(!/^\-?\$\d/.test(amountString)) {
        throw new Error(`could not parse amount text "${amountString}"`); // starts with -$DIGIT
    }
    if(!/\.\d{2}$/.test(amountString)) {
        throw new Error(`could not parse amount text "${amountString}"`); // ends with .DIGITDIGIT
    }
    return Math.round(parseFloat(amountString.replace(/[^\-0-9]/g, "")));
}
function parseChaseAmountStringToDollarsFloat(amountString) {
    return parseChaseAmountStringToCents(amountString) / 100;
}


async function scrapeTransactionData(scrapeKwargs, rowHeaderText) {
    // Check for any takeovers
    const continueWithActivity = document.querySelector(
        getElementSelector("continueWithActivityBtn")
    );
    if(continueWithActivity) {
        log("clicking continue with activity button");
        scrapeKwargs.notices.push("WARNING: clicking 'continue with activity' button");
        continueWithActivity.click();
        setTimeout(()=>{
            scrapeTransactionData(scrapeKwargs, rowHeaderText);
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
        log("found transactions loading element, waiting..");
        scrollToBottom();
        setTimeout(()=>{
            scrapeTransactionData(scrapeKwargs, rowHeaderText);
        }, 50);
        return;
    }

    const thead = table.querySelector("thead");
    validateTransactionTableHasExpectedColumnHeaders(thead);

    const startDateObj = parseISODateString(scrapeKwargs.startDate);
    const endDateObj = parseISODateString(scrapeKwargs.endDate);

    // Keep clicking "See more activity" until
    //  - oldest transaction date < startDate
    //  - OR the see more activity button no longer appears.
    const rows = table.querySelectorAll("tr");
    if(rows.length < 2) {
        scrapeKwargs.notices.push(
            `WARNING skipping sub account ${rowHeaderText}, no transaction rows found.`
        );
        clickAccountsButton();
        setTimeout(()=>{
            scrapeData(scrapeKwargs);
        });
        return;
    } else {
        try {
            validateRowsStartWithColumnHeadersRow(rows);
        } catch (err) {
            alert("ERROR: could not find transaction table heading row.")
            return;
        }
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
    if(oldestDate && oldestDate >= startDateObj) {
        const seeMoreBtn = document.querySelector(
            getElementSelector("seeMoreTransactions")
        );
        if (seeMoreBtn) {
            seeMoreBtn.click();
            setTimeout(()=>{
                scrapeTransactionData(scrapeKwargs, rowHeaderText);
            }, 50);
            return;
        }
    }
    else if(!oldestDate) {
        scrapeKwargs.notices.push(
            "WARNING could not find any transaction dates, skipping"
        );
        // Go back to accounts list
        clickAccountsButton();
        delete scrapeKwargs.accountingId;
        delete scrapeKwargs.chaseId;
        setTimeout(()=>{
            scrapeData(scrapeKwargs);
        });
        return;
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
            scrapeKwargs.notices.push(
                "WARNING skipping row due to unreadable date, " + rowHeaderText + ", data: " + dateStr);
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

        // Positive number results in CREDIT adj to plug
        // account (Expense account with debit balance).
        // Negative number results in DEBIT adj to plig
        // account (Expense account with debit balance).
        const amountCents = parseChaseAmountStringToCents(amountText);
        const lastName = rowHeaderText.split(" ")[0];
        let csvRow;
        let csvPlugRow;
        const data = {
            amountCents,
            descriptionText,
            lastName,
            rowDateObj,
            accountingId: scrapeKwargs.accountingId,
            chaseId: scrapeKwargs.chaseId,
            prefixColumns: scrapeKwargs.prefixColumns,
        };
        try {
            csvRow = processRow(data, scrapeKwargs.rowFilters, scrapeKwargs.csvRows);
            csvPlugRow = processPlugRow(
                scrapeKwargs.plugAccountId,
                amountCents * -1,
                lastName,
                scrapeKwargs.csvRows,
                data,
            );
        }
        catch (err) {
            scrapeKwargs.notices.push(err.message);
            console.error(err.message);
        }
        if(csvRow && csvPlugRow) {
            log("recording CSV row");
            scrapeKwargs.results.push(csvRow, csvPlugRow);
        }
    }

    delete scrapeKwargs.accountingId;
    delete scrapeKwargs.chaseId;

    // Go back to accounts list
    clickAccountsButton();
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

function is_USCIS_Check(memo) {
    const lcm = memo.toLowerCase();
    return Boolean(
        lcm.indexOf("name:uscis") != -1
        && [...lcm.matchAll(/id\:\d{4,}/g)].length > 0
    );
}

function is_IND_ID_check(memo) {
    const lcm = memo.toLowerCase();
    return Boolean(
        lcm.indexOf("ind id") != -1
        && [...lcm.matchAll(/ind\sid\:\d{4,}/g)].length > 0
    );
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
    }
    else if(is_IND_ID_check(row.descriptionText)) {
        const idMatches = [
            ...row.descriptionText
                .toLowerCase()
                .matchAll(/ind\sid\:\d{4,}/g)
        ];
        const matchCt = idMatches.length;
        if(matchCt == 0) {
            throw new Error("Could not extract IND id check number")
        }
        memoParts.push(idMatches[matchCt - 1][0]);
    }
    else if (is_USCIS_Check(row.descriptionText)){
        const idMatches = [
            ...row.descriptionText
                .toLowerCase()
                .matchAll(/id\:\d{4,}/g)
        ];
        const matchCt = idMatches.length;
        if(matchCt == 0) {
            throw new Error("Could not extract USCIS id")
        }
        memoParts.push(`USCIS ${ idMatches[matchCt - 1][0] }`);
    }
    else {
        // Misc transaction description.
        let cleanedDescr = (
            row.descriptionText
                .toLowerCase()
                .split("")
                .filter(c=>/[a-z0-9]/.test(c))
                .join("")
                .slice(0, 15)
        );
        memoParts.push(cleanedDescr);
    }

    return memoParts.join(" ");
}

function processPlugRow(plugAccountId, plugDebitBalanceCents, lastName, csvRowNames, row) {
    const csvRow = [];
    if(row.prefixColumns && row.prefixColumns.length > 0) {
        csvRow.push(...row.prefixColumns);
    }
    const dr = ((plugDebitBalanceCents > 0 ? plugDebitBalanceCents : 0) / 100).toFixed(2);
    const cr = ((plugDebitBalanceCents < 0 ? plugDebitBalanceCents * -1 : 0) / 100).toFixed(2);
    const memo = abbreviateDescription(row);
    for(let i in csvRowNames) {
        const colName = csvRowNames[i];
        if(colName == "account") {
            csvRow.push(plugAccountId)
        } else if (colName == "memo") {
            csvRow.push(memo);
        } else if (colName == "dr") {
            csvRow.push(dr);
        } else if (colName == "cr") {
            csvRow.push(cr);
        } else {
            throw new Error("unknown CSV column: " + colName)
        }
    }
    return csvRow;
}

function processRow(row, rowFilters, csvRowNames) {
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
                throw new Error("DEBUG filter: SKIPPING " + row.descriptionText);
            }
        }
        else if (filt.TYPE === "include") {
            skip = filt.AND.filter(val => cleanedDesc.indexOf(val) != -1).length != filt.AND.length;
            if(skip) {
                log("skipping row " + row.descriptionText);
                throw new Error("DEBUG incl filter: SKIPPING " + row.descriptionText);
            }
            if(filt.OR.length == 0){
                continue;
            }
            skip = filt.OR.filter(val => cleanedDesc.indexOf(val) != -1).length == 0;
            if(skip) {
                log("skipping row " + row.descriptionText);
                throw new Error("DEBUG incl filter: SKIPPING " + row.descriptionText);
            }
        }
        else {
            throw new Error("WARNING unknown type")
        }
    }

    const dr = row.amountCents > 0 ? (row.amountCents / 100).toFixed(2) : 0;
    const cr = row.amountCents < 0 ? (row.amountCents / -100).toFixed(2) : 0;

    const csvRow = []
    if(row.prefixColumns && row.prefixColumns.length > 0) {
        csvRow.push(...row.prefixColumns);
    }

    for(let i in csvRowNames) {
        const colName = csvRowNames[i];
        if(colName == "account") {
            csvRow.push(row.accountingId)
        } else if (colName == "memo") {
            csvRow.push(abbreviateDescription(row))
        } else if (colName == "dr") {
            csvRow.push(dr);
        } else if (colName == "cr") {
            csvRow.push(cr);
        } else {
            throw new Error("unknown CSV column: " + colName)
        }
    }
    return csvRow;
}

/*
 *
 *
 * BALANCE
 * REPORT
 *
 *
 */
async function createBalanceCSVLoopAccounts(scrapeKwargs) {
    log("createBalanceCSVLoopAccounts() called");
    // console.log({scrapeKwargs});

    const tableContainer = await waitForElement(getElementSelector("tableContainer"))
    const table = getAccountTableFromContainer(tableContainer);
    const tableRows = table.querySelectorAll(getElementSelector("accountsTableRow"));
    log("searching " + tableRows.length + " rows for unclicked links");

    for(let i=0; i< tableRows.length; i++) {
        /* This forloop iterates until finds an element to process.
         *   After processing 1x element the forloop BREAKs.
        */
        const tr = tableRows[i];
        const rowHeader = tr.querySelector(getElementSelector("accountsTableRowHeader"));
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
        log("clicking account link " + rowHeaderText);
        scrapeKwargs.linksClicked.push(rowHeaderText);
        setTimeout(() => {
            tr.querySelector("a").click();
        }, 150);

        // Grab information from subaccount page
        setTimeout(async ()=>{
            await openAccountDetailsModal();
            const chaseId = await getChaseAccountNumberFromModal(true);
            log("on transaction page for account number " + chaseId);
            if(!chaseId) {
                alert(
                    "ERROR: could not get chase account ID from webpage: " + rowHeaderText
                );
                return;
            }
            let accountingId = "N/A";
            if(scrapeKwargs.lookup.has(chaseId)) {
                accountingId = scrapeKwargs.lookup.get(chaseId);
            } else {
                log("no ACCOUNTING ID found for this account");
                scrapeKwargs.notices.push(
                    `WARNING CHASE account ${rowHeaderText} (${chaseId}) no ACCOUNTING ID found`
                );
            }

            log("row has associated ACC account " + accountingId);
            scrapeKwargs.notices.push(
                `INFO Scraping CHASE account ${rowHeaderText} (${chaseId}) matching id: ${accountingId}`
            );
            setTimeout(()=> {
                scrapeBalanceData({...scrapeKwargs, accountingId, chaseId}, rowHeaderText);
            });
        }, 300);
        return;
    }

    // All links are clicked
    chrome.storage.local.set({running: false}, ()=> {
        chrome.runtime.sendMessage({event: "BalanceScrapeStopped"})
    });
    outputBalanceReportCSV(
        scrapeKwargs.resultRows, scrapeKwargs.notices);
}

async function scrapeBalanceData(scrapeKwargs, rowHeaderText) {
    // Check for any takeovers
    const continueWithActivity = document.querySelector(
        getElementSelector("continueWithActivityBtn")
    );
    if(continueWithActivity) {
        log("clicking continue with activity button");
        scrapeKwargs.notices.push("WARNING: clicking 'continue with activity' button");
        continueWithActivity.click();
        setTimeout(()=>{
            scrapeBalanceData(scrapeKwargs, rowHeaderText);
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
        log("found transactions loading element, waiting..");
        scrollToBottom();
        setTimeout(()=>{
            scrapeBalanceData(scrapeKwargs, rowHeaderText);
        }, 50);
        return;
    }

    const endDateObj = parseISODateString(scrapeKwargs.endDate);
    // Keep clicking "See more activity" until
    //  - oldest transaction date  > endDate
    //  - OR the see more activity button no longer appears.
    const rows = table.querySelectorAll("tr");
    if(rows.length < 2) {
        scrapeKwargs.notices.push(
            `WARNING skipping ${rowHeaderText}, no transaction rows found.`
        );
        clickAccountsButton();
        setTimeout(()=>{
            createBalanceCSVLoopAccounts(scrapeKwargs);
        });
        return;
    }
    validateRowsStartWithColumnHeadersRow(rows);
    let oldestDate;
    for(let i=1; i<rows.length; i++) {
        let rowDateStr = rows[i].querySelector(
            getElementSelector("transactionRowDate")
        ).innerText;
        if(rowDateStr && isChaseDateString(rowDateStr)) {
            oldestDate = parseChaseDateString(rowDateStr);
        }
    }
    if(oldestDate && oldestDate > endDateObj) {
        // We need to go back further //
        const seeMoreBtn = document.querySelector(
            getElementSelector("seeMoreTransactions")
        );
        if (seeMoreBtn) {
            log("clicking see more transactions button...");
            seeMoreBtn.click();
            setTimeout(()=>{
                scrapeBalanceData(scrapeKwargs, rowHeaderText);
            }, 50);
            return;
        }
    }
    else if(!oldestDate) {
        scrapeKwargs.notices.push(
            "WARNING could not find any transaction dates, skipping"
        );
        // Go back to accounts list
        clickAccountsButton();
        delete scrapeKwargs.accountingId;
        delete scrapeKwargs.chaseId;
        setTimeout(()=>{
            createBalanceCSVLoopAccounts(scrapeKwargs);
        });
        return;
    }
    // We've gone back far enough //

    // Loop through rows until a "balance row" is found
    let prevDateStr, isLastRow, row, balanceRowFound = false, balanceRowDateStr;
    for(let i=1; i<rows.length; i++) {
        isLastRow = i == (rows.length - 1);
        row = rows[i];
        const dateTd = row.querySelector(getElementSelector("transactionRowDate"));
        let dateStr = dateTd.innerText;
        log("found date string " + dateStr)
        if(dateStr && isChaseDateString(dateStr)) {
            prevDateStr = dateStr;
        } else if(prevDateStr) {
            dateStr = prevDateStr;
        } else {
            log("skipping row due to bad date");
            scrapeKwargs.notices.push(
                "WARNING skipping row due to unreadable date, " + rowHeaderText + ", date: " + dateStr);
            continue;
        }
        log("parsing string " + dateStr)
        const rowDateObj = parseChaseDateString(dateStr);
        if(!isLastRow && rowDateObj > endDateObj) {
            continue;
        }
        if(isLastRow || (rowDateObj <= endDateObj)) {
            balanceRowFound = true;
            balanceRowDateStr = dateStr.replace(/,/g, '');
            break;
        }
    }
    if(balanceRowFound) {
        const amountText = row.querySelector(
            getElementSelector("transactionRowBalance")
        ).innerText;
        const amount = parseChaseAmountStringToDollarsFloat(amountText);
        const csvRow = [
            rowHeaderText,
            scrapeKwargs.chaseId,
            scrapeKwargs.accountingId,
            balanceRowDateStr,
            amount,
        ];
        scrapeKwargs.resultRows.push(csvRow);
    }

    delete scrapeKwargs.accountingId;
    delete scrapeKwargs.chaseId;

    // Go back to accounts list
    clickAccountsButton();
    setTimeout(()=>{
        createBalanceCSVLoopAccounts(scrapeKwargs);
    });
}

function outputBalanceReportCSV(rows, notices) {
    log(`writing ${rows.length} rows to csv file`);
    const ts = getFileNameTimestamp()
    const csvLink = document.createElement("a");
    csvLink.download = `balances-${ ts }.csv`;
    const csv = rows.map((v) => {return v.join(',')}).join('\n');
    csvLink.href = encodeURI("data:text/csv," + csv);
    csvLink.click();

    log(`writing ${notices.length} lines to text file`);
    const logLink = document.createElement("a");
    logLink.download = `balances-${ ts }.txt`;
    const logLines = notices.join('\n');
    logLink.href = "data:text/plain;charset=utf-8," + encodeURIComponent(logLines);
    logLink.click();
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
                table = getAccountTableFromContainer(tableContainer);
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
    log("clickAccountsButton()");
    (document.querySelector("mds-breadcrumb")
        .shadowRoot.querySelector("mds-link")
        .shadowRoot.querySelector("a")
        .click());
    log("clickAccountsButton() done");
}
const TESTS = [
    {
        name:"Account table is findable and has findable rows",
        cb: async function() {
            const tableContainer = document.querySelector(getElementSelector("tableContainer"));
            if(!tableContainer) {
                return "could not find container accountsTableAG1Table0"
            }
            const table = getAccountTableFromContainer(tableContainer);
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
        name:"Account table column has anchor link",
        cb: async function() {
            const table = await selectAccountTable();
            const tableRows = table.querySelectorAll(getElementSelector("accountsTableRow"));
            const row = tableRows[4];
            const headerCol = row.querySelector(getElementSelector("accountsTableRowHeader"));
            if(!headerCol) {
                return "could not find row heading";
            }
            const anchor = headerCol.querySelector("a")
            if(!anchor) {
                return "could not find clickable link";
            }
            if(!/^.*\s\(\.{3}\d{4}\)$/.test(anchor.innerText)) {
                return "found unexpected sub account link text";
            }
        },
    },
    {
        name:"Can click on row header link and navigate account details",
        cb: async function() {
            const tableHash = location.hash;
            const table = await selectAccountTable();
            const tableRows = table.querySelectorAll(getElementSelector("accountsTableRow"));
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
        name:"Can find show account id link",
        cb: async function() {
            await waitForElement(getElementSelector("fullAccountNumberLinkContainer"));
            const button = document.querySelector(getElementSelector("fullAccountNumberLinkContainer"));
            if (!button) {
                return "Could not find 'get account number' button";
            }
            return;
        },
    },
    {
        name:"Can get account id from modal",
        cb: async function() {
            log("opening modal");
            await openAccountDetailsModal();
            log("getting chase account ID");
            const chaseId = await getChaseAccountNumberFromModal(false);
            if(!chaseId) {
                return "could not get chase account ID from modal";
            }
            if(!confirm(`Is this the correct account number? ${chaseId}\n\nClick OK if the account number is correct.`)) {
                return "could not get chase account ID from modal";
            }
            findAndCloseModal();
            return;
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
                let attemptNumber = 0;
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
                    // Loading element doesnt always appear, if transaction table is selectable
                    // consider this test passed.
                    const loaderElem = document.querySelector(
                        getElementSelector("transactionTableLoader")
                    ) || document.querySelector(getElementSelector("transactionTable"));
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
        name: "Can find expected transaction table column headers",
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
                    const thead = table.querySelector("thead");
                    validateTransactionTableHasExpectedColumnHeaders(thead);
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
    {
        name: "Can select transaction table rows",
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
                    try {
                        validateRowsStartWithColumnHeadersRow(rows);
                    } catch (err) {
                        return resolve("expected first row to have \"header-row\" class name");
                    }
                    if(rows.length < 5) {
                        attemptNumber++;
                        if(attemptNumber > 100) {
                            return resolve("could not find transaction table rows")
                        }
                        setTimeout(inner, 100);
                        return;
                    }
                    else if (rows[1].querySelectorAll("td").length < 4) {
                        return resolve("could not find table TD cells");
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
            const tableRows = acctable.querySelectorAll(getElementSelector("accountsTableRow"));
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
                if(!/\-?\$[\d|\,]+\.\d{2}$/.test(amountTd.innerText)) {
                    return "unexpected currency format";
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
            log("WAIT_FOR_ACCOUNT_TABLE_TOKEN")
            await selectAccountTable();
            log("found table")
            continue;
        }

        log("running test index " + i + ` ${TESTS[i].name}`);

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
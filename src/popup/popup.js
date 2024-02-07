
const log = (msg) => {
    console.log("HCE: " + msg);
}

async function validateExtensionVersion() {
    const url = "https://raw.githubusercontent.com/stricoff92/chase-to-csv-extension/master/manifest.json"
    const resp = await fetch(url, {timeout: 3500});
    const repoManifest = await resp.json();
    var localManifest = chrome.runtime.getManifest();
    return parseFloat(repoManifest.version) <= parseFloat(localManifest.version);
}

function validateChromeVersion() {
    const version = /Chrome\/([0-9.]+)/.exec(navigator.userAgent)[1];
    const majorVersion = parseInt(version.split(".")[0]);
    return majorVersion >= 97;
}

function setUpdateStatusNotice(msg) {
    const updateStatus = document.querySelector(
        "#extension-update-status-container"
    );
    updateStatus.classList.remove("hidden");
    updateStatus.innerText = msg;
}

function setPopupOnPage() {
    const notFound = document.querySelector("#page-not-found-container");
    notFound.classList.add("hidden");
    const found = document.querySelector("#page-found-container");
    found.classList.remove("hidden");
    document.querySelector("#reset-btn-container").classList.remove("hidden");
    document.querySelector("#scrape-running-container").classList.add("hidden");
    const bodyElem = document.querySelector("body");
    bodyElem.style.border = "2px solid white";
}

function setPopupRunning() {
    const notFound = document.querySelector("#page-not-found-container");
    notFound.classList.add("hidden");
    const found = document.querySelector("#page-found-container");
    found.classList.add("hidden");
    document.querySelector("#reset-btn-container").classList.add("hidden");
    document.querySelector("#scrape-running-container").classList.remove("hidden");
    const bodyElem = document.querySelector("body");
    bodyElem.style.border = "2px solid red";
}

function setPopupOffPage() {
    const notFound = document.querySelector("#page-not-found-container");
    notFound.classList.remove("hidden");
    const found = document.querySelector("#page-found-container");
    found.classList.add("hidden");
    document.querySelector("#reset-btn-container").classList.add("hidden");
    document.querySelector("#scrape-running-container").classList.add("hidden");
    const bodyElem = document.querySelector("body");
    bodyElem.style.border = "none";
}

function updateProgress(value, max) {
    const progress = document.getElementById("progress-bar");
    progress.value = value;
    progress.max = max;
}

function updateDebugMessage(msg) {
    document.getElementById("debug-message").innerText = msg;
}

function getDefaultStartEndDate(isoDate) {
    const [yearInt, monthInt, _dayInt] = isoDate.split("-").map(v=> parseInt(v));
    const prevMonthInt = monthInt == 1 ? 12 : monthInt - 1;
    const prevYearInt = prevMonthInt != 12 ? yearInt : yearInt - 1;
    let prevDayStart = 1;
    let prevDayEnd;
    if([1, 3, 5, 7, 8, 10, 12].indexOf(prevMonthInt) != -1) {
        prevDayEnd = 31;
    } else if(prevMonthInt != 2) {
        prevDayEnd = 30
    } else {
        const leapYears = [2024, 2028, 2032, 2036, 2040, 2044, 2048, 2052, 2056, 2060];
        if(yearInt > leapYears[leapYears.length - 1]) {
            console.warn("Could not calculate if leapyear or not. not autofilling date inputs.")
            return {};
        }
        prevDayEnd = leapYears.indexOf(prevYearInt) != -1 ? 29 : 28
    }

    const padNumber = n => n > 9 ? n : "0" + n;
    return {
        start: `${prevYearInt}-${padNumber(prevMonthInt)}-${padNumber(prevDayStart)}`,
        end: `${prevYearInt}-${padNumber(prevMonthInt)}-${padNumber(prevDayEnd)}`,
    }
}

const REQUIRED_CSV_COLUMNS = ['account', 'memo', 'dr', 'cr'];

document.addEventListener("DOMContentLoaded", async () => {

    let versionOk;
    try {
        versionOk = await validateExtensionVersion();
    } catch(err) {
        setUpdateStatusNotice(
            "ERROR: Could not verify extension version."
        );
    }
    if(versionOk === false) {
        setUpdateStatusNotice(
            "A new extension version is available. Please update."
        );
    }
    try {
        versionOk = validateChromeVersion();
    } catch(err) {
        setUpdateStatusNotice(
            "ERROR: Could not verify chrome version."
        );
    }
    if(versionOk === false) {
        setUpdateStatusNotice(
            "A new Chrome version is available. Please update."
        );
    }

    await new Promise(resolve => {
        chrome.tabs.query({url: ["https://*.chase.com/*"]}, (tabs) => {
            if(tabs.length == 0) {
                setPopupOffPage();
                chrome.storage.local.set({onPage:false, running:false}, ()=>{
                    resolve();
                });
            } else {
                resolve();
            }
        })
    });

    document.querySelector("#reset-on-page-btn").addEventListener("click", ()=>{
        chrome.storage.local.set({onPage:false, running:false});
        setPopupOffPage();
    });

    document.querySelector("#view-accounts-anchor").addEventListener("click", () => {
        chrome.tabs.create({ url: 'src/page/account-table.html' })
    });

    document.querySelector("#compare-csvs-anchor").addEventListener("click", () => {
        chrome.tabs.create({ url: 'src/page/compare-csvs.html' })
    });

    document.querySelector("#show-advanced-options-anchor").addEventListener("click", ()=>{
        document.querySelector("#show-advanced-options-container").style.display = "none";
        document.querySelector("#advanced-options-container").style.display = "block";
    });

    chrome.runtime.onMessage.addListener((request, sender, sendResponse)=> {
        if(request.event === "onPage") {
            setPopupOnPage();
        } else if (request.event === "offPage") {
            setPopupOffPage();
        } else if (request.event === "scrapeStarted") {
            setPopupRunning();
        } else if (request.event === "scrapeStopped") {
            setPopupOnPage();
        } else if (request.event == "healthCheckStopped") {
            setPopupOnPage();
        } else if (request.event === "progressBar") {
            updateProgress(request.data.value, request.data.max);
        } else if (request.event === "debugMessage") {
            updateDebugMessage(request.data);
        }

        sendResponse(true);
    });

    document.querySelector("#start-balance-scrape-end-date-input").value = (
        (new Date()).toISOString().slice(0, 10)
    )

    const onLoadKeys = [
        'onPage',
        'running',
        'previousTransactionFilter',
        'previousAccountFilter',
        'csvColumns',
        'prefixColumns',
        'plugAccountId',
        'lastRunCompleted',
        'lastRunFrom',
        'lastRunTo',
    ];
    chrome.storage.local.get(onLoadKeys, (result) => {
        if(result.previousTransactionFilter) {
            document.getElementById("new-scrape-row-desc-filter").value = (
                result.previousTransactionFilter
            );
        }
        if(result.previousAccountFilter) {
            document.getElementById("new-scrape-acc-desc-filter").value = (
                result.previousAccountFilter
            );
        }
        if(result.csvColumns) {
            document.getElementById("new-scrape-csv-columns").value = (
                result.csvColumns
            );
        } else {
            document.getElementById("new-scrape-csv-columns").value = (
                JSON.stringify(REQUIRED_CSV_COLUMNS)
            );
        }
        if(result.prefixColumns) {
            document.getElementById("new-scrape-column-prefixes").value = (
                result.prefixColumns
            );
        }
        if(result.plugAccountId) {
            document.getElementById("new-scrape-plug-account-input").value = (
                result.plugAccountId
            );
        }
        if(result.onPage && !result.running) {
            setPopupOnPage();
        } else if (result.onPage && result.running) {
            setPopupRunning();
        } else {
            setPopupOffPage();
        }
        if(result.lastRunCompleted) {
            document.getElementById("last-completed-data").innerText = (
                result.lastRunCompleted
            );
        }
        if(result.lastRunFrom) {
            document.getElementById("last-from-data").innerText = (
                result.lastRunFrom
            );
        }
        if(result.lastRunTo) {
            document.getElementById("last-to-data").innerText = (
                result.lastRunTo
            );
        }
    });

    const detaultDates = getDefaultStartEndDate(
        (new Date()).toISOString().slice(0, 10)
    );
    if(detaultDates.start && detaultDates.end) {
        document.getElementById("new-scrape-start-date-input").value = (
            detaultDates.start
        );
        document.getElementById("new-scrape-end-date-input").value = (
            detaultDates.end
        );
    }


    document.getElementById("start-balance-scrape-btn").addEventListener("click", () => {
        const tabQueryParams = {
            active: true,
            currentWindow: true,
            url: [
                "https://*.chase.com/*",
            ],
        };
        chrome.tabs.query(tabQueryParams, (tabs) => {
            if(tabs.length == 0) {
                alert("Not on chase website. Could not generate balance report.")
                return;
            }
            const endDate = document.getElementById("start-balance-scrape-end-date-input").value;
            const payload = {
                event: "BalanceScrapeStarted",
                endDate,
            }
            chrome.tabs.sendMessage(
                tabs[0].id,
                payload,
                ()=>{
                    setPopupRunning();
                },
            )
        });
    })
    document.getElementById("start-health-check-btn").addEventListener("click", () => {
        const tabQueryParams = {
            active: true,
            currentWindow: true,
            url: [
                "https://*.chase.com/*",
            ],
        };
        chrome.tabs.query(tabQueryParams, (tabs) => {
            if(tabs.length == 0) {
                alert("Not on chase website. Could not start health check.")
                return;
            }
            chrome.tabs.sendMessage(
                tabs[0].id,
                {event: "healthCheckStarted"},
                ()=>{
                    setPopupRunning();
                },
            )
        });
    });
    document.getElementById("start-scrape-btn").addEventListener("click", () => {
        const errorArea = document.getElementById("start-scrape-error-area");
        errorArea.classList.add("hidden")
        errorArea.innerHTML = "";
        const startDate = document.getElementById("new-scrape-start-date-input").value;
        const endDate = document.getElementById("new-scrape-end-date-input").value;
        const maxAccounts = parseInt(
            document.getElementById("start-scrape-max-rows-input").value
        );
        const plugAccountId = document.getElementById("new-scrape-plug-account-input").value || "";
        chrome.storage.local.set({ plugAccountId });

        let rowFilters;
        try{
            rowFilters = JSON.parse(
                document.getElementById("new-scrape-row-desc-filter").value
                || "[]"
            )
        } catch(err) {}
        if(rowFilters) {
            chrome.storage.local.set({
                previousTransactionFilter: JSON.stringify(rowFilters)
            });
        }

        let accFilters;
        try{
            accFilters = JSON.parse(
                document.getElementById("new-scrape-acc-desc-filter").value
                || "[]"
            )
        } catch(err) {}
        if(accFilters) {
            chrome.storage.local.set({
                previousAccountFilter: JSON.stringify(accFilters)
            });
        }

        let csvRows;
        try {
            csvRows = JSON.parse(
                document.getElementById("new-scrape-csv-columns").value
                || "[]"
            )
        } catch (err) {}
        if(csvRows) {
            chrome.storage.local.set({
                csvColumns: JSON.stringify(csvRows)
            });
        }

        let prefixColumns;
        try {
            prefixColumns = JSON.parse(
                document.getElementById("new-scrape-column-prefixes").value
                || "[]"
            )
        } catch (err) {}
        if(prefixColumns) {
            chrome.storage.local.set({
                prefixColumns: JSON.stringify(prefixColumns)
            });
        }

        let errors = []
        if(!startDate) {
            errors.push("Start date is required.");
        }
        if(!endDate) {
            errors.push("End date is required.");
        }
        if(!maxAccounts || maxAccounts < 1) {
            errors.push("Max accounts must be positive number.");
        }
        if(!rowFilters || !Array.isArray(rowFilters)) {
            errors.push("invalid transaction row filter format")
        } else {
            for(let i in rowFilters) {
                if(typeof rowFilters[i].AND === "undefined" || typeof rowFilters[i].OR === "undefined") {
                    errors.push(`Row filter index ${i} missing AND/OR data.`)
                }
                if(!Array.isArray(rowFilters[i].AND) || !Array.isArray(rowFilters[i].OR)) {
                    errors.push(`Row filter index ${i} has invalid AND/OR data.`);
                }
                if(typeof rowFilters[i].TYPE === "undefined") {
                    errors.push(`Row filter index ${i} missing TYPE data.`)
                } else if (rowFilters[i].TYPE != "include" && rowFilters[i].TYPE != "exclude") {
                    errors.push(`Row filter index ${i} TYPE data invalid. must use include or exclude.`)
                }
            }
        }
        if(!accFilters || !Array.isArray(accFilters)) {
            errors.push("invalid account row filter format")
        } else {
            for(let i in accFilters) {
                if(typeof accFilters[i].INCLUDE === "undefined" || typeof accFilters[i].EXCLUDE === "undefined") {
                    errors.push(`Account filter index ${i} missing INCLUDE/EXCLUDE data.`)
                }
                if(!Array.isArray(accFilters[i].INCLUDE) || !Array.isArray(accFilters[i].EXCLUDE)) {
                    errors.push(`Account filter index ${i} invalid INCLUDE/EXCLUDE format.`)
                }
            }
        }

        if(!csvRows) {
            errors.push("invalid CSV Columns format");
        }
        else {
            for(let i in REQUIRED_CSV_COLUMNS) {
                if (csvRows.indexOf(REQUIRED_CSV_COLUMNS[i]) == -1) {
                    errors.push("missing CSV column name " + REQUIRED_CSV_COLUMNS[i]);
                }
            }
        }

        if (errors.length) {
            errorArea.innerHTML = errors.join("<br>");
            errorArea.classList.remove("hidden");
            return;
        }

        const startDateParts = startDate.split("-");
        const endDateParts = endDate.split("-");
        // are these dates right?
        const startDateObj = new Date(startDateParts[0], startDateParts[1] - 1, startDateParts[2]);
        const endDateObj = new Date(endDateParts[0], endDateParts[1] - 1, endDateParts[2]);
        if(startDateObj >= endDateObj) {
            errorArea.innerHTML = "Start date must be before end date.";
            errorArea.classList.remove("hidden");
            return;
        }

        const tabQueryParams = {
            active: true,
            currentWindow: true,
            url: [
                "https://*.chase.com/*",
            ],
        };
        chrome.tabs.query(tabQueryParams, (tabs) => {
            if(tabs.length == 0) {
                errorArea.innerHTML = "Current tab is not chase website. Please navigate to website.";
                errorArea.classList.remove("hidden");
                return;
            }
            const payload = {
                event: "scrapeStarted",
                startDate,
                endDate,
                maxAccounts,
                rowFilters,
                accFilters,
                csvRows,
                prefixColumns,
                plugAccountId,
            }
            chrome.tabs.sendMessage(
                tabs[0].id,
                payload,
                setPopupRunning,
            )
        });
    });

    document.getElementById("cancel-crawl-btn").addEventListener("click", () => {
        chrome.storage.local.set({running: false}, ()=> {
            setPopupOnPage();
        })
    });
});

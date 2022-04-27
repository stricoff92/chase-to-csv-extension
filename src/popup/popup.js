
const log = (msg) => {
    console.log("HCE: " + msg);
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

document.addEventListener("DOMContentLoaded", () => {
    document.querySelector("#reset-on-page-btn").addEventListener("click", ()=>{
        chrome.storage.local.set({onPage:false});
        setPopupOffPage();
    });

    document.querySelector("#view-accounts-anchor").addEventListener("click", () => {
        chrome.tabs.create({ url: 'src/page/account-table.html' })
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

    chrome.storage.local.get(['onPage', 'running', 'previousFilter'], (result) => {
        if(result.previousFilter) {
            document.getElementById("new-scrape-row-desc-filter").value = (
                result.previousFilter
            );
        }
        if(result.onPage && !result.running) {
            setPopupOnPage();
        } else if (result.onPage && result.running) {
            setPopupRunning();
        } else {
            setPopupOffPage();
        }
    });

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
        let rowFilters;
        try{
            rowFilters = JSON.parse(
                document.getElementById("new-scrape-row-desc-filter").value
                || "[]"
            )
        } catch(err) {

        }
        chrome.storage.local.set({
            previousFilter: JSON.stringify(rowFilters)
        });

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
            errors.push("invalid row filter format")
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

        if (errors.length) {
            errorArea.innerHTML = errors.join("<br>");
            errorArea.classList.remove("hidden");
            return;
        }

        const startDateParts = startDate.split("-");
        const endDateParts = endDate.split("-");
        const startDateObj = new Date(startDateParts[0], startDateParts[1], startDateParts[2]);
        const endDateObj = new Date(endDateParts[0], endDateParts[1], endDateParts[2]);
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
                return;
            }
            chrome.tabs.sendMessage(
                tabs[0].id,
                {event: "scrapeStarted", startDate, endDate, maxAccounts, rowFilters},
                ()=>{
                    setPopupRunning();
                },
            )
        });
    });

    document.getElementById("cancel-crawl-btn").addEventListener("click", () => {
        chrome.storage.local.set({running: false}, ()=> {
            setPopupOnPage();
        })
    });
});

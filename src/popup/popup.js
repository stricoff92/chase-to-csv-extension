
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
            setPopupOnPage()
        } else if (request.event === "offPage") {
            setPopupOffPage();
        } else if (request.event === "scrapeStarted") {
            setPopupRunning();
        } else if (request.event === "scrapeStopped") {
            setPopupOnPage();
        }
    });

    chrome.storage.local.get(['onPage', 'running'], (result) => {
        if(result.onPage && !result.running) {
            setPopupOnPage();

        } else if (result.onPage && result.running) {
            setPopupRunning();

        } else {
            setPopupOffPage();
        }
    });

    document.getElementById("start-scrape-btn").addEventListener("click", () => {
        const errorArea = document.getElementById("start-scrape-error-area");
        errorArea.classList.add("hidden")
        errorArea.innerHTML = "";
        const startDate = document.getElementById("new-scrape-start-date-input").value;
        const endDate = document.getElementById("new-scrape-end-date-input").value;
        errors = []
        if(!startDate) {
            errors.push("Start date is required.");
        }
        if(!endDate) {
            errors.push("End date is required.");
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

        chrome.tabs.query({active: true, currentWindow:true}, (tabs) => {
            chrome.tabs.sendMessage(
                tabs[0].id,
                {event: "scrapeStarted", startDate, endDate},
                (resp)=>{
                    if(resp) {
                        chrome.storage.local.set({running: true});
                        setPopupRunning();
                    }
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

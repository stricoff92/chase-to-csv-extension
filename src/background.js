

chrome.runtime.onInstalled.addListener(() => {
    console.log("Hello! bank-to-csv extension has been installed 👋");
    chrome.storage.local.set({onPage: false, running: false});
});

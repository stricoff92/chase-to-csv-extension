

chrome.runtime.onInstalled.addListener(() => {
    console.log("Hello! Havens-Chrome-Extention has been installed 👋");
    chrome.storage.local.set({onPage: false, running: false, data:[]});
});

chrome.runtime.onMessage.addListener((request, sender, sendResponse)=> {
    if(request.event === "rowData") {
        console.log({rowData:request});
        sendResponse(true);
    }
});
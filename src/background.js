

chrome.runtime.onInstalled.addListener(() => {
    console.log("Hello! Havens-Chrome-Extention has been installed 👋");
    chrome.storage.local.set({onPage: false, running: false});
});

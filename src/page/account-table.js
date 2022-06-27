


function drawTable(parent) {
    table = document.createElement("table");

    // Assemble heading
    header = document.createElement("tr");
    heading1 = document.createElement("th");
    heading1.innerText = "BANK-ID";
    heading2 = document.createElement("th");
    heading2.innerText = "ACC-ID";
    heading3 = document.createElement("th");
    header.append(heading1);
    header.append(heading2);
    header.append(heading3);
    table.append(header);

    chrome.storage.local.get(['data'], (results) => {
        const data = results.data || [];
        for(let i=0; i<data.length; i++) {
            const tr = document.createElement("tr");
            const td1 = document.createElement("td");
            const td2 = document.createElement("td");
            td1.innerText = data[i][0];
            td2.innerText = data[i][1];
            tr.append(td1);
            tr.append(td2);

            const td3 = document.createElement("td");
            const btn = document.createElement("button");
            btn.innerText = "delete"
            btn.addEventListener("click", ()=>{
                deleteRow(data[i][0]);
            })
            td3.append(btn);
            tr.append(td3);
            table.append(tr);
        }
        parent.append(table);
    });
}

function deleteRow(bankId) {
    if(!confirm("Delete bank ID '" + bankId + "' ?")) {
        return;
    }
    chrome.storage.local.get(['data'], (results) => {
        let data = results.data;
        data = data.filter(row=> row[0] !== bankId);
        chrome.storage.local.set({data}, ()=> {
            location.reload();
        });
    });
}

const isValidId = (id) => /^[a-zA-Z0-9]+$/.test(id);

function validateNewIds(newBankId, newAccId) {
    errors = []
    if(!newBankId) {
        errors.push("Bank ID is required.");
    }
    if(!newAccId) {
        errors.push("Accounting ID is required.");
    }
    if(!isValidId(newBankId)) {
        errors.push("Bank ID is invalid. Must use characters A-Z and 0-9");
    }
    if(!isValidId(newAccId)) {
        errors.push("Accounting ID is invalid. Must use characters A-Z and 0-9");
    }
    return errors;
}

function resetErrors() {
    errors = document.getElementById("error-container");
    errors.innerHTML = "";
    errors.classList.add("hidden");
}
function drawErrors(errors) {
    container = document.getElementById("error-container");
    container.classList.remove("hidden");
    errors.forEach((e)=> {
        const row = document.createElement("p");
        row.innerText = e;
        container.append(row);
    });
}

document.addEventListener("DOMContentLoaded", () => {

    // Draw table.
    const parent = document.getElementById("table-container");
    if(!parent) {
        throw new Error("Could not find parent element");
    }
    drawTable(parent);

    // Updadate data usage
    chrome.storage.local.getBytesInUse((usage) =>{
        progress = document.getElementById("storage-usage-progress");
        progress.value = usage;
        progress.max = chrome.storage.local.QUOTA_BYTES;
        percent = document.getElementById("percentage-usage");
        percent.innerText = ((usage / chrome.storage.local.QUOTA_BYTES) * 100).toFixed(4);
    });

    // Add new row handler.
    document.getElementById("add-row-btn").addEventListener("click", () => {
        resetErrors();
        const newBankId = document.getElementById("new-row-bank-input").value.trim();
        const newAccId = document.getElementById("new-row-acc-input").value.trim();
        const errors = validateNewIds(newBankId, newAccId);
        if(errors.length) {
            return drawErrors(errors);
        }
        chrome.storage.local.get(['data'], (results) => {
            const errors = [];
            const data = results.data || [];
            for(let i=0; i<data.length; i++) {
                const row = data[i];
                const bankId = row[0];
                const accId = row[1];
                if(bankId === newBankId) {
                    errors.push(`Bank ID in use: ${newBankId}`);
                }
                if(accId === newAccId) {
                    errors.push(`Account ID in use: ${newAccId}`);
                }
            }
            if(errors.length) {
                return drawErrors(errors);
            }

            data.push([newBankId, newAccId]);
            chrome.storage.local.set({data}, () => {
                location.reload();
            });
        });
    });

    document.getElementById("export-to-csv-anchor").addEventListener("click", () => {
        chrome.storage.local.get(['data'], (results) => {
            const data = results.data;
            data.unshift(['BANK ID', 'ACCOUNTING ID']);
            const tempLink = document.createElement("a");
            tempLink.download = "accounts.csv";
            const csv = data.map((v) => {return v.join(',')}).join('\n');
            tempLink.href = encodeURI("data:text/csv," + csv);
            tempLink.click();
        })
    });

    // CSV Import
    document.getElementById("import-csv-btn").addEventListener("click", ()=> {
        const fileInput = document.getElementById("import-file-picker-input");
        const skipFirstRow = document.getElementById("skip-first-row-input").checked;
        if(!fileInput.value) {
            return alert("Please select a CSV file to import.");
        }
        const file = fileInput.files[0];
        if(!file) {
            return alert("Hmmm, could not import this file.");
        }
        chrome.storage.local.get(['data'], (results) => {
            const existingRows = results.data || [];
            const existingBankIds = existingRows.map(r=>r[0])
            const existingAccIds = existingRows.map(r=>r[1])

            const reader = new FileReader();
            reader.readAsText(file, "UTF-8");
            reader.onload = (evt) => {
                const result = evt.target.result;
                const newRows = [];
                const errors = [];
                result.split("\n").forEach((row, ix)=>{
                    if(ix == 0 && skipFirstRow) {
                        return;
                    }
                    const vals = row.split(",")
                    const newBankId = vals[0].trim();
                    const newAccId = vals[1].trim();
                    if(existingBankIds.indexOf(newBankId) != -1) {
                        errors.push("Bank id already in use: " + newBankId)
                    }
                    if(existingAccIds.indexOf(newAccId) != -1) {
                        errors.push("Accounting id already in use: " + newAccId)
                    }
                    if(!isValidId(newBankId)) {
                        errors.push("Invalid Bank ID. Must use characters A-Z 0-9 " + newBankId)
                    }
                    if(!isValidId(newAccId)) {
                        errors.push("Invalid Accounting ID. Must use characters A-Z 0-9 " + newBankId)
                    }
                    newRows.push([newBankId, newAccId])
                });
                if(errors.length) {
                    return alert("An Error Occured\n\n" + errors.join("\n"))
                }

                const finalRows = existingRows.concat(newRows)
                chrome.storage.local.set({data: finalRows}, ()=>{
                    location.reload();
                });
            }
        });
    })

    document.getElementById("delete-lookup-anchor").addEventListener("click", ()=> {
        const msg = "You are about to delete this table. Make sure you have downloaded an up to date copy before continuing.\n\nContinue?";
        if(!confirm(msg)) {
            return;
        }
        chrome.storage.local.set({data: []}, ()=>{
            location.reload();
        });
    })
});
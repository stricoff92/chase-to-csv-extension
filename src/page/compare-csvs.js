

async function csvToArray(fileHandle, columns, prefixColCount) {
    return new Promise(resolve => {
        const reader = new FileReader();
        reader.readAsText(fileHandle, "UTF-8");
        reader.onload = (event) => {
            const csvText = event.target.result;
            const rows = [];
            csvText.split("\n").forEach(rowText => {
                const rowObj = {};
                const rowArr = rowText.split(",");

                columns.forEach((colName, colNameIx) => {
                    rowObj[colName] = rowArr[colNameIx + prefixColCount];
                });
                rowObj.cr = parseFloat(rowObj.cr);
                rowObj.dr = parseFloat(rowObj.dr);
                rows.push(rowObj);
            });
            resolve(rows);
        };
    });
}

async function getBankToAccountingMapping() {
    return new Promise((resolve) => {
        chrome.storage.local.get(['data'], (result) => {
            /* Mapping of ACC-ID => BANK-ID
            */
            resolve(
                new Map(result.data.map(
                    row=>([row[1], row[0]])
                ))
            );
        });
    });
}


document.addEventListener("DOMContentLoaded", () => {

    // Preload form
    const onLoadKeys = [
        'csvColumns',
        'prefixColumns',
        'plugAccountId',
    ];
    chrome.storage.local.get(onLoadKeys, (result) => {
        if(result.csvColumns) {
            document.querySelector("#csv-columns-input").value = result.csvColumns;
        }
        if(result.plugAccountId) {
            document.querySelector("#plug-account-id-input").value = result.plugAccountId;
        }
        if(result.prefixColumns) {
            const pcc = JSON.parse(result.prefixColumns).length;
            document.querySelector("#prefix-column-count-input").value = pcc;
        }
    });


    document.querySelector("#compare-csvs-btn").addEventListener("click", async ()=>{
        // Clear errors
        const errorArea = document.querySelector("#form-error-area");
        errorArea.innerHTML = "";
        errorArea.style.display = "none";

        // Get form data and validate
        const errors = [];
        const csvColumnsJSON = document.querySelector("#csv-columns-input").value;
        let csvColumns;
        try {
            csvColumns = JSON.parse(csvColumnsJSON);
        } catch(err) {
            errors.push("invalid CSV Column format");
        }
        if(csvColumns) {
            const expectedColumns = ["account","memo","dr","cr"];
            expectedColumns.forEach(expectedCol => {
                if(csvColumns.indexOf(expectedCol) == -1) {
                    errors.push("expected CSV column '" + expectedCol + "'");
                }
            })
        }

        const plugAccountId = document.querySelector("#plug-account-id-input").value;
        if(!plugAccountId) {
            errors.push("plug account id is required");
        }

        const outflowThreshold = parseInt(document.querySelector("#outflow-threshold-input").value);
        if(!outflowThreshold || outflowThreshold < 0) {
            errors.push("invalid outflow threshold");
        }

        const prefixColCount = parseInt(document.querySelector("#prefix-column-count-input").value || 0);
        if(prefixColCount < 0) {
            errors.push("invalid number of prefix columns");
        }

        const fileInputBase = document.getElementById("base-csv-file-input");
        const fileInputCurr = document.getElementById("current-csv-file-input");
        if(!fileInputBase.value || !fileInputCurr.value) {
            errors.push("CSV file selections are required.")
        }
        const fileBase = fileInputBase.files[0];
        const fileCurr = fileInputCurr.files[0];
        if(!fileBase || !fileCurr) {
            errors.push("hmmm, could not import CSV files")
        }

        // Show errors if any
        if(errors.length) {
            errorArea.innerHTML = errors.join("<br>");
            errorArea.style.display = "block";
            return;
        }

        document.getElementById("data-entry-form").style.display = "none";

        const baseCSVdata = await csvToArray(fileBase, csvColumns, prefixColCount);
        const currCSVdata = await csvToArray(fileCurr, csvColumns, prefixColCount);
        const summaries = getAccountSummaries(
            currCSVdata,
            baseCSVdata,
            plugAccountId,
        );

        const accToBankMap =  await getBankToAccountingMapping();
        const tablesData = getTablesData(summaries, accToBankMap, outflowThreshold);
        const tablesContainer = document.getElementById("tables-container");
        tablesContainer.style.display = "block";

        // Draw Tables
        for(let i=0; i<tablesData.length; i++) {
            let tableData = tablesData[i];
            console.log({tableData})

            let tableContainer = document.createElement("div");
            tableContainer.style.margin = "1rem";

            let tableTitle = document.createElement("a");
            tableTitle.href = "#";
            tableTitle.innerText = `${tableData.title} (${tableData.rows.length})`;
            tableTitle.style.fontWeight = "bold";
            tableTitle.style.fontSize = "1.2rem";

            let dataTable = document.createElement("table");

            dataTable.classList.add("sortable-theme-light");
            dataTable.setAttribute("data-sortable", "");

            dataTable.style.display = "none";
            tableTitle.addEventListener("click", () => {
                dataTable.style.display = dataTable.style.display == "none" ? "block" : "none";
            });

            let thead = document.createElement("thead");
            let titleRow = document.createElement("tr");
            tableData.columns.forEach(heading => {
                let th = document.createElement("th");
                if(heading.toLowerCase().indexOf("curr. ") == 0) {
                    th.style.backgroundColor = "#004d1d";
                } else if (heading.toLowerCase().indexOf("base ") == 0) {
                    th.style.backgroundColor = "#06004d";
                } else {
                    th.style.backgroundColor = "#3b3b3b";
                }
                th.innerText = heading;
                titleRow.append(th);
            });
            thead.append(titleRow)
            dataTable.append(thead);

            function spanToDataValue(span) {
                const text = span.innerText;
                if(/^\$\s*-?\d/.test(text)) {
                    return text.replace(/(\$|\s|\,)/g, "");
                }
                else if (/\%$/.test(text)) {
                    return text.replace(/(\%|\s|\,)/g, "");
                } else {
                    return text;
                }
            }
            let tbody = document.createElement("tbody");
            tableData.rows.forEach(rowSpans => {
                let tr = document.createElement("tr");
                rowSpans.forEach(span => {
                    let td = document.createElement("td");
                    td.append(span);
                    td.setAttribute("data-value", spanToDataValue(span))
                    tr.append(td);
                });
                tbody.append(tr)
            });
            dataTable.append(tbody);

            tableContainer.append(tableTitle);
            tableContainer.append(dataTable);
            tablesContainer.append(tableContainer);
        }
        // All tables must be drawn before calling init()
        Sortable.init();

        // Add final click handlers
        document.getElementById("show-all-anchor").addEventListener('click', () => {
            document.querySelectorAll("table").forEach(t => {
                t.style.display = "block";
            });
        });
        document.getElementById("hide-all-anchor").addEventListener('click', () => {
            document.querySelectorAll("table").forEach(t => {
                t.style.display = "none";
            });
        });

    });

});

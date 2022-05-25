

async function csvToArray(fileHandle, columns) {
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
                    rowObj[colName] = rowArr[colNameIx];
                });
                rowObj.cr = parseFloat(rowObj.cr);
                rowObj.dr = parseFloat(rowObj.dr);
                rows.push(rowObj);
            });
            resolve(rows);
        };
    });
}


document.addEventListener("DOMContentLoaded", () => {

    // Preload form
    const onLoadKeys = [
        'csvColumns',
        'plugAccountId',
    ];
    chrome.storage.local.get(onLoadKeys, (result) => {
        if(result.csvColumns) {
            document.querySelector("#csv-columns-input").value = result.csvColumns;
        }
        if(result.plugAccountId) {
            document.querySelector("#plug-account-id-input").value = result.plugAccountId;
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

        const baseCSVdata = await csvToArray(fileBase, csvColumns);
        const currCSVdata = await csvToArray(fileCurr, csvColumns);

        const summaries = getAccountSummaries(
            currCSVdata,
            baseCSVdata,
            plugAccountId,
        );

        console.log(summaries);
        const tablesData = getTablesData(summaries);

    });

});

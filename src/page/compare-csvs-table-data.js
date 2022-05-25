
function createTextSpan(value, bold) {
    const s = document.createElement("span");
    if(bold) {
        s.style.fontWeight = "bold";
    }
    s.innerText = value;
    return s;
}

function createCurrencySpan(value) {
    const s = document.createElement("span");
    if(value < 0) {
        s.style.color = "#ff0000";
    }
    s.innerText = "$ " + value.toFixed(2);
    return s;
}

function createPercentageSpan(value) {
    const s = document.createElement("span");
    if(value < 0) {
        s.style.color = "#ff0000";
    }
    s.innerText = Math.round(value * 100).toFixed(0) + "%";
    return s;
}


function getTablesData(accountSummaries, accToBankMap, outflowThreshold) {
    /* Returned data structure
        [
            {
                title: STRING,
                columns: ARRAY<STRING>
                rows: ARRAY<ARRAY<span>>
            },
            ...
        ]
    */
    const tables = [];
    let table;
    let count;

    // Cliffs
    table = {};
    table.title = 'Activity Cliffs';
    table.columns = [
        "Account",
        "Base Transaction Ct.",
        "Curr. Transaction Ct.",
        "Base Money Movement",
        "Curr. Money Movement",
    ];
    const cliffs = accountSummaries.filter(s => s.isCliff);
    cliffs.sort((a, b) => b.currentCount - a.currentCount);
    table.rows = cliffs.map(summary => ([
        createTextSpan(
            `${summary.account} (${accToBankMap.get(summary.account)})`,
            true,
        ),
        createTextSpan(summary.baseCount),
        createTextSpan(summary.currentCount),
        createCurrencySpan(summary.baseMovementAmount),
        createCurrencySpan(summary.currentMovementAmount),
    ]));
    tables.push(table);


    // Top outflow amounts
    table = {};
    table.title = "Top Outflows Over Threshold";
    table.columns = [
        "Account",
        "Curr. Outflow Amt.",
        "Curr. Money Movement Amt.",
        "Base Outflow Amt.",
        "Base Money Movement Amt.",
    ];
    const topOutflows = accountSummaries.filter(
        s => s.currentOutflowAmount >= outflowThreshold
    );
    topOutflows.sort((a, b) => b.currentOutflowAmount - a.currentOutflowAmount)
    table.rows = topOutflows.map(summary => ([
        createTextSpan(
            `${summary.account} (${accToBankMap.get(summary.account)})`,
            true,
        ),
        createCurrencySpan(summary.currentOutflowAmount),
        createCurrencySpan(summary.currentMovementAmount),
        createCurrencySpan(summary.baseOutflowAmount),
        createCurrencySpan(summary.baseMovementAmount),
    ]));
    tables.push(table);


    // Top % Changes in Money Movement
    count = 5;
    table = {};
    table.title = "Top % Change in Money Movement";
    table.columns = [
        "Account",
        "Curr. Money Movement Amt.",
        "Base Money Movement Amt.",
        "% Change",
    ];
    const MMChangePerc = accountSummaries.filter(s => !s.isCliff);
    MMChangePerc.sort(
        (a, b) => b.movementPercentChange - a.movementPercentChange
    );
    const topMMPercChanges = [];
    for(let i=0; i<count; i++) {
        if(MMChangePerc[i]) {
            topMMPercChanges.push(MMChangePerc[i]);
        }
    }
    for(let i=0; i<count; i++) {
        let j = MMChangePerc.length - (i + 1);
        if(MMChangePerc[j]) {
            topMMPercChanges.push(MMChangePerc[j]);
        }
    }
    table.rows = topMMPercChanges.map(summary => ([
        createTextSpan(
            `${summary.account} (${accToBankMap.get(summary.account)})`,
            true,
        ),
        createCurrencySpan(summary.currentMovementAmount),
        createCurrencySpan(summary.baseMovementAmount),
        createPercentageSpan(summary.movementPercentChange),
    ]));
    tables.push(table);


    // Top % Changes in Transaction Count
    count = 5;
    table = {};
    table.title = "Top % Change in Transaction Count";
    table.columns = [
        "Account",
        "Curr. Transaction Ct.",
        "Base Transaction Ct.",
        "% Change",
    ];
    const TCChangePerc = accountSummaries.filter(s => !s.isCliff);
    TCChangePerc.sort(
        (a, b) => b.transactionCountPercentChange - a.transactionCountPercentChange
    );
    const topTCPercChanges = [];
    for(let i=0; i<count; i++) {
        if(TCChangePerc[i]) {
            topTCPercChanges.push(TCChangePerc[i]);
        }
    }
    for(let i=0; i<count; i++) {
        let j = TCChangePerc.length - (i + 1);
        if(TCChangePerc[j]) {
            topTCPercChanges.push(TCChangePerc[j]);
        }
    }
    table.rows = topTCPercChanges.map(summary => ([
        createTextSpan(
            `${summary.account} (${accToBankMap.get(summary.account)})`,
            true,
        ),
        createTextSpan(summary.currentCount),
        createTextSpan(summary.baseCount),
        createPercentageSpan(summary.transactionCountPercentChange),
    ]));
    tables.push(table);


    return tables;
}


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


function getTablesData(accountSummaries) {
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

    // Cliffs
    table = {};
    table.title = 'Activity Cliffs';
    table.columns = [
        "Account",
        "Base Transaction Ct.", "Curr. Transaction Ct.",
        "Base Money Movement", "Curr. Money Movement",
    ];
    const cliffs = accountSummaries.filter(s => s.isCliff);
    cliffs.sort((a, b) => a.currentCount - b.currentCount);
    table.rows = cliffs.map(summary => ([
        createTextSpan(summary.account, true),
        createTextSpan(summary.baseCount),
        createTextSpan(summary.currentCount),
        createCurrencySpan(summary.baseMovementAmount),
        createCurrencySpan(summary.currentMovementAmount),
    ]));
    tables.push(table);



    return tables;


}

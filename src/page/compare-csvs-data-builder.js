

function calculatePercentDelta(base, current) {
    if (base == 0 && current != 0) {
        return current > 0 ? 1 : -1;
    }
    if(base == 0 && current == 0) {
        return 0;
    }
    return (current - base) / base;
}


function getSummary(rows, account) {
    /* Get summary details for a single account
    */
    const data = {
        account,

        baseCount: 0,
        currentCount: 0,
        baseOutflowCount: 0,
        currentOutflowCount: 0,
        baseInflowCount: 0,
        currentInflowCount: 0,

        baseOutflowAmount: 0,
        currentOutflowAmount: 0,
        baseInflowAmount: 0,
        currentInflowAmount: 0,

        baseMovementAmount: 0,
        currentMovementAmount: 0,

        isCliff: false,
        isCliffToZero: false,
        isCliffFromZero: false,
        movementPercentChange: 0,
        inflowPercentChange: 0,
        outflowPercentChange: 0,
        transactionCountPercentChange: 0,

    };

    for(let i=0; i<rows.length; i++) {
        let row = rows[i];
        let isBase = row.isBase;
        let isOutflow = row.cr > 0;
        if(isBase) {
            data.baseCount++;
            if(isOutflow) {
                data.baseOutflowCount++;
                data.baseOutflowAmount += row.cr;
                data.baseMovementAmount -= row.cr;
            } else {
                data.baseInflowCount++;
                data.baseInflowAmount += row.dr;
                data.baseMovementAmount += row.dr;
            }
        } else {
            data.currentCount++;
            if(isOutflow) {
                data.currentOutflowCount++;
                data.currentOutflowAmount += row.cr;
                data.currentMovementAmount -= row.cr;
            } else {
                data.currentInflowCount++;
                data.currentInflowAmount += row.dr;
                data.currentMovementAmount += row.dr;
            }
        }
    }

    if(data.currentCount && !data.baseCount) {
        data.isCliff = true;
        data.isCliffFromZero = true;
    }
    if(!data.currentCount && data.baseCount) {
        data.isCliff = true;
        data.isCliffToZero = true;
    }

    if(!data.isCliff) {
        data.movementPercentChange = calculatePercentDelta(
            data.baseMovementAmount,
            data.currentMovementAmount,
        );
        data.inflowPercentChange = calculatePercentDelta(
            data.baseInflowAmount,
            data.currentInflowAmount,
        );
        data.outflowPercentChange = calculatePercentDelta(
            data.baseOutflowAmount,
            data.currentOutflowAmount,
        );
        data.transactionCountPercentChange = calculatePercentDelta(
            data.baseCount,
            data.currentCount,
        )
    }

    return data;
}

function getAccountSummaries(currentData, baseData, plugAccountId) {
    /* Returned data structure:
        [
            accountSummary,
            ...
        ]
    */

    baseData = baseData.map(r=>({...r, isBase: true}));
    currentData = currentData.map(r=>({...r, isBase: false}));
    const data = currentData.concat(baseData);
    const out = [];

    for(let i=0; i<data.length; i++) {
        const accountId = data[i].account;

        if(accountId == plugAccountId) {
            continue;
        }


        if(typeof out.find(r => r.account == accountId) !== 'undefined') {
            continue;
        }

        const rows = data.filter(r => r.account == accountId);
        out.push(
            getSummary(rows, accountId)
        );
    }

    return out;
}

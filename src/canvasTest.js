
function penDown(idString, x, y) {
    var canvas = document.getElementById(idString);
    var ctx = canvas.getContext("2d");
    ctx.beginPath();
    ctx.moveTo(x, y);
}


function drawTo(idString, x, y) {
    var canvas = document.getElementById(idString);
    var ctx = canvas.getContext("2d");
    ctx.lineTo(x, y);
    ctx.stroke();
}


function downEventHandeler(idString, event) {
    if (event.pointerType == "pen") {
        penDown(idString, event.offsetX, event.offsetY);
    }
}


function moveEventHandeler(idString, event) {
    if (event.pointerType == "pen") {
        var events = event.getCoalescedEvents();
        drawTo(idString, event.offsetX, event.offsetY);
    }
}

function addListeners(idString) {
    document.addEventListener("DOMContentLoaded", function () { console.log("DOMLoaded")
        var can = document.getElementById(idString);
        can.addEventListener("pointerdown", downEventHandeler);
        can.addEventListener("pointermove", moveEventHandeler);
        can.addEventListener("pointerup", moveEventHandeler);
    });
}


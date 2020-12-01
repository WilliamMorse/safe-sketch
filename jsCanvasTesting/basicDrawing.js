window.addEventListener('load', () =>{
    const canvas = document.getElementById("canvasOne");
    const ctx = canvas.getContext("2d"); 
    const dpr = window.devicePixelRatio * 2 ; // screen pixels / css pixels 

    // Resize code 
    function resize() {
        canvas.height = (window.innerHeight - 20) * dpr;
        canvas.width = (window.innerWidth - 20) * dpr;
        console.log(dpr);
    }

    resize();

    window.addEventListener('resize', () =>{resize()});
    window.addEventListener('onContextMenu', () =>{});


    function penDown(ev){
        if (ev.pointerType == "pen") { 
            ctx.beginPath();
            ctx.moveTo(ev.offsetX * dpr, ev.offsetY * dpr);
            ctx.lineWidth = 2*dpr;
            ctx.lineCap = "round";
            ctx.lineTo(ev.offsetX * dpr, ev.offsetY * dpr);
            ctx.stroke();
        } 
    }

    function penDraw(ev){
        if (ev.pointerType == "pen") { 
            ctx.lineWidth = 2*dpr;
            ctx.lineCap = "round";
            ctx.lineTo(ev.offsetX * dpr, ev.offsetY * dpr);
            ctx.stroke();
        }
    }

    function penUp(ev){
        if (ev.pointerType == "pen") { 
            ctx.lineWidth = 2*dpr;
            ctx.lineCap = "round";
        }
    }
    function onMove(ev){
        for (let coalescedEv of ev.getCoalescedEvents())
            penDraw(coalescedEv);
            console.log(ev.getCoalescedEvents().length);
    }

    // event listeners
    canvas.addEventListener("pointerdown", penDown);
    canvas.addEventListener("pointermove", onMove);
    canvas.addEventListener("pointerup", onMove);

    

});



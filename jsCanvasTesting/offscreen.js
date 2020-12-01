






const offscreen = document.querySelector('canvas').transferControlToOffscreen();
const worker = new Worker('myworkerurl.js');
worker.postMessage({ canvas: offscreen }, [offscreen]);

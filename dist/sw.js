importScripts('https://storage.googleapis.com/workbox-cdn/releases/5.1.2/workbox-sw.js');

console.log(workbox.routing)
workbox.routing.registerRoute(
    ({request}) => request.destination === 'script',
    new workbox.strategies.StaleWhileRevalidate()
);

workbox.routing.registerRoute(
    ({request}) => request.destination === 'style',
    new workbox.strategies.StaleWhileRevalidate()
);
workbox.routing.registerRoute(
    "/",
    new workbox.strategies.StaleWhileRevalidate()
);
workbox.routing.registerRoute(
    "/index.html",
    new workbox.strategies.StaleWhileRevalidate()
);




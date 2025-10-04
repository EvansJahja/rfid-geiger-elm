import { Capacitor } from '@capacitor/core';
import { CapacitorBluetoothSerial } from 'capacitor-bluetooth-serial';
import { Camera } from '@capacitor/camera';
import { Elm } from './Main.elm';
import { DB, open } from './db.ts';

const platform = Capacitor.getPlatform();
const app = Elm.Main.init({ node: document.getElementById('elm-app'), flags: { platform } });

// Module-level variables to maintain serial port state

app.ports.debugPort.subscribe(function(message) {
    switch(message) {
        default:
            console.log("Unknown debug message from Elm:", message);
    }
});

app.ports.requestDeviceList.subscribe(function() {
    console.log("Requesting Bluetooth device list...");
    CapacitorBluetoothSerial.checkAndRequestBluetoothPermission().then(() => {
      CapacitorBluetoothSerial.listDevices().then(({devices}) => {
          console.log("Available Bluetooth devices:", JSON.stringify(devices));
          app.ports.deviceList.send(devices.map(d => ({ name: d.name, address: d.address })));
      })
    });
});

app.ports.registerListener.subscribe(async function() {

    CapacitorBluetoothSerial.watchData(({data}) => {
        const uint8Array = new Uint8Array(data);
        const numberArray = Array.from(uint8Array);
        app.ports.serialData.send(numberArray);
    }).then(callbackId => {
        console.log("Watch data listener registered with callback ID:", callbackId);
    });


});

app.ports.deviceConnect.subscribe(async function(address) {
    try {
        app.ports.serialStatus.send(["serial_connecting"]);
        await CapacitorBluetoothSerial.connect(address);
        console.log("Connected to device");
        app.ports.serialStatus.send(["serial_connected"]);
    } catch (error) {
        console.error("Connection error:", error);
        app.ports.serialStatus.send(["serial_error", error.message || "Unknown error"]);
    }
});
        



app.ports.serialSend.subscribe(async function(data) {
    try {
        await CapacitorBluetoothSerial.sendData({ data });
    } catch (error) {
        console.error("Error sending data:", error);
        app.ports.serialStatus.send(["serial_error", "Send failed: " + error.message]);
    }
});

app.ports.takePicture.subscribe(async function() {
    const image = await Camera.getPhoto({
        allowEditing: false,
        source: 'CAMERA',
        resultType: 'dataUrl'
    });
    app.ports.pictureResult.send(image.dataUrl);
});

app.ports.indexedDbCmd.subscribe(async function([cmd, args]) {
    console.log("Received IndexedDB command from Elm:", cmd, args);
    switch (cmd) {
        case "open":
            if (db) {
                throw new Error("IndexedDB is already opened");
            }
            db = await open();
            console.log("Opened IndexedDB:", db);
            break;
        case "findItem":
            if (!db) {
                throw new Error("IndexedDB is not opened");
            }
            const item = await db.findItem(args);
            console.log("Found item:", item);
            app.ports.indexedDbSub.send(["findItemResult", item]);
            break;
        // Handle other commands as needed
    }
});

/**
 * @type {DB | null}
 */
let db = null;


// // indexdb stuffs
// console.log("Setting up IndexedDB...");
// /**
//  * @type {IDBRequest}
//  */
// const request = window.indexedDB.open("MyDatabase", 1);

// request.onsuccess = function(event) {
//     /**
//      * @type {IDBDatabase}
//      */
//     const db  = event.target.result;
//     console.log("IndexedDB opened successfully:", db);

//     const tx = db.transaction("item", "readwrite");
//     const objItem = tx.objectStore("item");

//     objItem.add({ title: "Sample Item 1", description: "This is a sample item." });
//     objItem.add({ title: "Sample Item 2", description: "This is a sample item." });

//     const itemCount = objItem.count();
//     console.log(`Counting items in 'item' store: ${itemCount}`);
// }

// request.onerror = function(event) {
//     console.error("IndexedDB error:", event.target.error);
// }
// request.onupgradeneeded = function(event) {
//     console.log("Called onupgradeneeded", event);
//     const db = event.target.result;
//     const oldVersion = event.oldVersion;
//     if (oldVersion < 1) {
//         console.log("Upgrading IndexedDB to version 1");
//         const itemObjStore = db.createObjectStore("item", { keyPath: "title"});
//         console.log("Object store 'item' created.");

//         const categoryObjStore = db.createObjectStore("category", { keyPath: "tag" });
//         categoryObjStore.createIndex("itemTitle", "itemTitle", { unique: false });
//         console.log("Object store 'category' created.");
//     }



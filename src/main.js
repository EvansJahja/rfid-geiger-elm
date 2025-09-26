import { Capacitor } from '@capacitor/core';
import { CapacitorBluetoothSerial } from 'capacitor-bluetooth-serial';
import { Elm } from './Main.elm';

const app = Elm.Main.init({ node: document.getElementById('elm-app') });

// Module-level variables to maintain serial port state

app.ports.debugPort.subscribe(function(message) {
    switch(message) {
        // case "GetDeviceList":
        //     app.ports.deviceList.send([{"name": "Device A", "address": "00:11:22:33:44:55"}, {"name": "Device B", "address": "66:77:88:99:AA:BB"}]);
        //     break;
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

app.ports.requestPort.subscribe(async function() {
    CapacitorBluetoothSerial.checkAndRequestBluetoothPermission().then(() => {
      CapacitorBluetoothSerial.listDevices().then(({devices}) => {
          console.log("Available Bluetooth devices:", JSON.stringify(devices));
          app.ports.deviceList.send(devices.map(d => d.name));
      })
    });
});

app.ports.registerListener.subscribe(async function() {

    CapacitorBluetoothSerial.watchData(({data}) => {
        const uint8Array = new Uint8Array(data);
        const numberArray = Array.from(uint8Array);
        console.log("Received data from device:", numberArray);
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
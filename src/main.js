import { Capacitor } from '@capacitor/core';
import { CapacitorBluetoothSerial } from 'capacitor-bluetooth-serial';
import { Camera } from '@capacitor/camera';
import { Elm } from './Main.elm';

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

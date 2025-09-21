// A reference to your Elm app, typically created when the app is initialized.
const app = Elm.Main.init({ node: document.getElementById('elm-app') });

// Module-level variables to maintain serial port state
let serialPort = null;
let serialWriter = null;


app.ports.requestPort.subscribe(async function() {
  
  try {
    app.ports.serialStatus.send(["serial_waiting_for_user"]);
    serialPort = await navigator.serial.requestPort();
    app.ports.serialStatus.send(["serial_connecting"]);
    
    await serialPort.open({ baudRate: 9600 });
    app.ports.serialStatus.send(["serial_connected"]);

    // Get the writer and store it for later use
    serialWriter = serialPort.writable.getWriter();

    // Read raw bytes instead of decoded text
    const reader = serialPort.readable.getReader();

    while (true) {
      const { value, done } = await reader.read();
      if (done) {
        console.log('[readLoop] DONE', done);
        reader.releaseLock();
        break;
      }
      
      // Convert Uint8Array to regular array of integers for Elm
      const byteArray = Array.from(value);
      console.log("📥 Received bytes:", byteArray);
      
      // Send the raw bytes to Elm
      app.ports.serialData.send(byteArray);
    }
  } catch (error) {
    console.error("🔴 Serial connection error: ", error);
    app.ports.serialStatus.send(["serial_error", error.message || "Unknown error"]);
  }
});

app.ports.serialSend.subscribe(async function(byteList) {
    console.log("📤 Sending bytes:", byteList);
    
    if (!serialWriter) {
        console.error("🔴 No serial writer available. Connect to port first!");
        app.ports.serialStatus.send(["serial_error", "Port not connected"]);
        return;
    }
    
    try {
        // Convert to Uint8Array for serial port
        const uint8Array = new Uint8Array(byteList);
        console.log("Sending to serial:", uint8Array); 
        
        // Send to serial port (don't release the writer - we want to reuse it)
        await serialWriter.write(uint8Array);
        console.log("✅ Bytes sent successfully");

        // // For dummy testing, echo back what we sent (convert back to byte array)
        // const echoBytes = Array.from(uint8Array);
        // app.ports.serialData.send(echoBytes);
    } catch (error) {
        console.error("🔴 Error sending data:", error);
        app.ports.serialStatus.send(["serial_error", "Send failed: " + error.message]);
    }
});
// A reference to your Elm app, typically created when the app is initialized.
const app = Elm.Main.init({ node: document.getElementById('elm-app') });


app.ports.requestPort.subscribe(async function() {
  
  try {
    app.ports.serialStatus.send(["serial_waiting_for_user"]);
    const port = await navigator.serial.requestPort();
    app.ports.serialStatus.send(["serial_connecting"]);
    
    await port.open({ baudRate: 9600 });
    app.ports.serialStatus.send(["serial_connected"]);

    const decoder = new TextDecoderStream();
    const inputDone = port.readable.pipeTo(decoder.writable);
    const inputStream = decoder.readable;

    const reader = inputStream.getReader();

    while (true) {
      const { value, done } = await reader.read();
      if (done) {
        console.log('[readLoop] DONE', done);
        reader.releaseLock();
        break;
      }
      // Send the received data back to Elm
      app.ports.serialData.send(value);
    }
  } catch (error) {
    console.error("🔴 Serial connection error: ", error);
    app.ports.serialStatus.send(["serial_error", error.message || "Unknown error"]);
  }
});
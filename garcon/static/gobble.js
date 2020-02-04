console.log("GOO");

var boggle = new WebSocket ("ws://localhost:8000");

boggle.onopen = () => {
    console.log("i'm open");
    boggle.send("jake");
}

boggle.onmessage = (msg) => {
    console.log(msg.data);
}

boggle.onclose = () => {
    alert("close");
}

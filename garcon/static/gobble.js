console.log("MUSH!");

var boggle = new WebSocket ("ws://localhost:8000");
var name;

get_name = () => {
    name = prompt('name?');
    boggle.send(name);
}

boggle.onopen = () => {
    get_name();
}

boggle.onmessage = (msg) => {
    if (name === null) { get_name(); }
    msg = JSON.parse(msg.data);
    console.log(msg);
    return msg;
}

boggle.onclose = () => {
    alert("close");
}

console.log("BOGGLE BITCH!");

var boggle = new WebSocket ("ws://localhost:8000");
var board;
var words;
var name;

board_row = (row) => {
    mktd = (letter) => {
        return "<td>" + letter + "</td>";
    }
    return `<tr>${row.split("").map(mktd).join("")}</tr>`
}

update_board = () => {
    var r1 = board.slice(0,4);
    var r2 = board.slice(4,8);
    var r3 = board.slice(8,12);
    var r4 = board.slice(12,16);
    var rs = [r1,r2,r3,r4];
    var html = `<table>${rs.map(board_row).join("")}</table>`;
    document.getElementById('gobble').innerHTML = html;
}

get_name = (msg="") => {
    name = prompt('name?' + msg);
    boggle.send(name);
}

boggle.onopen = () => {
    get_name();
}

boggle.onmessage = (msg) => {
    var res = JSON.parse(msg.data);
    if (res === "name-is-taken") {
        get_name(msg=" (previous one is taken)");
    } else if (!(res['board'] == null)) {
        board = res['board'];
        update_board();
    } else {
        console.log(res);
    }
}

boggle.onclose = () => {
    alert("close");
}

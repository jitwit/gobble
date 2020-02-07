var boggle = new WebSocket ("ws://localhost:8000");
var board;
var words;
var name;

$(() => {
    
    add_word = (word) => { boggle.send('gobble ' + word); }
    del_word = (word) => { boggle.send('dobble ' + word); }
    query_words = () => { boggle.send('words'); }
    query_players = () => { boggle.send('who-else'); }
    
    boggle.onopen = (e) => {
        set_name();
    }
        
    boggle.onmessage = (msg) => {
        var res = JSON.parse(msg.data);
        if (res === "name-is-taken") {
            set_name(msg=" (previous one is taken)");
        } else if (!(res['board'] == null)) {
            board = res['board'];
            update_board();
            query_words();
        } else if (!(res['words'] == null)) {
            words = res['words'];
            update_words();
        } else if (!(res['scores'] == null)) {
            var o = res['scores'];
            var h = "";
            for (var s in o) {
                h += `<li>${s} got ${o[s]}</li>`;
            }
            console.log(o);
            $("#scores").html(`<h3>scores</h3><ul>${h}</ul>`);
        } else {
            console.log(res);
        }
    }

    board_row = (row) => {
        mktd = (letter) => { return "<td>" + letter + "</td>"; }
        return `<tr>${row.split("").map(mktd).join("")}</tr>`
    }
    
    update_board = () => {
        var rows = [0,1,2,3].map((n) => board_row(board.slice(4*n,4*(n+1))));
        $("#gobble").html(`<table>${rows.join("")}</table>`);
    }
    
    update_words = () => {
        var items = words.map((w) => `<li>${w}</li>`).join("");
        $("#submissions").html(`${items}`);
    }
    
    set_name = (msg="") => {
        name = prompt('name?' + msg);
        boggle.send(name);
    }
    
    $("#mush").submit((e) => {
        e.preventDefault();
        add_word($("#scratch").val());
        $("#scratch").val("");
        query_words();
    });

    $("#submissions").on("click","li",(e) => {
        del_word($(e.target).text());
        query_words();
    });
    
    boggle.onclose = () => { alert("lost connection. refresh?"); }

})

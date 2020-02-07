var boggle = new WebSocket ("ws://192.168.2.18:8000");
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
            $("#submissions").html(words);
        } else if (!(res['scores'] == null)) {
            $("#scores").html(res['scores']);
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
        $("#scratch").focus();
        del_word($(e.target).text());
        query_words();
    });
    
    boggle.onclose = () => { alert("lost connection. refresh?"); }

})

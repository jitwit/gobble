var boggle = new WebSocket ("ws://localhost:8000");

$(() => {
    var words_node = document.getElementById("submissions");
    var gobble_node = document.getElementById("gobble");
    var write_node = $("#scratch");

    var board;
    var words;
    var name;
    
    add_word = (word) => { boggle.send('gobble ' + word); }
    del_word = (word) => { boggle.send('dobble ' + word); }
    query_words = () => { boggle.send('words'); }
    query_players = () => { boggle.send('who-else'); }
    update_words = () => {
        words_node.innerHTML = words;
    }
    
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
        } else {
            console.log(res);
        }
    }

    board_row = (row) => {
        mktd = (letter) => { return "<td>" + letter + "</td>"; }
        return `<tr>${row.split("").map(mktd).join("")}</tr>`
    }
    
    update_board = () => {
        var rs = [0,1,2,3].map((n) => board.slice(4*n,4*(n+1)));
        var html = `<table>${rs.map(board_row).join("")}</table>`;
        gobble_node.innerHTML = html;
    }
    
    
    set_name = (msg="") => {
        name = prompt('name?' + msg);
        boggle.send(name);
    }
    
    $("#mush").submit((e) => {
        e.preventDefault();
        add_word($("#scratch").val());
        query_words();
    });
    
//    write_node.onsubmit = (e) => {  console.log(e);    }
    boggle.onclose = () => { alert("close"); }

})

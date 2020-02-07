var boggle = new WebSocket ("ws://192.168.2.18:8000");

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
            $("#gobble").html(res['board']);
            query_words();
        } else if (!(res['words'] == null)) {
            words = res['words'];
            $("#submissions").html(words);
        } else if (!(res['peeps'] == null)) {
            $("#scores").html(res['peeps']);
        } else {
            console.log(res);
        }
    }

    set_name = (msg="") => {
        name = prompt('name?' + msg);
        boggle.send(name);
        $("#scratch").focus();
    };
    
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
    
    boggle.onclose = () => {
        alert("lost connection. refresh?");
    };

});

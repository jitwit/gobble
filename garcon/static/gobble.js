var expires;

$(() => {
    var boggle = new WebSocket ("ws://192.168.2.13:8000");
    add_word = (word) => { boggle.send('gobble ' + word); };
    del_word = (word) => { boggle.send('dobble ' + word); };
    query_words = () => { boggle.send('words'); };
    boggle.onopen = (e) => { set_name(); };
    boggle.onmessage = (msg) => {
        var res = JSON.parse(msg.data);
        if (res === "name-is-taken") { set_name(msg=" (previous one is taken)"); }
        else if (!(res['board'] == null)) { $("#gobble").html(res['board']); query_words(); }
        else if (!(res['words'] == null)) { $("#submissions").html(res['words']); }
        else if (!(res['peeps'] == null)) { $("#scores").html(res['peeps']); }
        else if (!(res['time'] == null)) { expires = new Date (res['time']); timer(); }
        else { console.log(res); }
    };
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
    timer = () => {
        $("#timer").html(expires);
    }
    boggle.onclose = () => { alert("lost connection."); };
});

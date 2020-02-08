$(() => {
    var boggle = new WebSocket ("ws://192.168.2.13:8000");
    add_word = async (word) => { boggle.send('gobble ' + word); };
    del_word = async (word) => { boggle.send('dobble ' + word); };
    query_words = () => { boggle.send('words'); };
    boggle.onopen = (e) => { set_name(); };
    boggle.onmessage = async (msg) => {
        var res = JSON.parse(msg.data);
        if (res === "name-is-taken") { set_name(msg=" (previous one is taken)"); }
        else if (!(res['board'] == null)) { $("#gobble").html(res['board']); query_words(); }
        else if (!(res['words'] == null)) { $("#submissions").html(res['words']); }
        else if (!(res['peeps'] == null)) { $("#scores").html(res['peeps']); }
        else if (!(res['time'] == null)) { timer(new Date (res['time'])); }
        else { console.log(res); }
    };
    set_name = async (msg="") => {
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
    timer = async (expires) => {
        var end = expires.getTime();
        var update = async () => {
            var now = (new Date ()).getTime();
            var left = Math.floor((end-now)/1000+1);
            if (left > 0) {
                if (left >= 30) {
                    $("#timer").html("remaining " + (left-30));
                } else {
                    $("#timer").html("next " + left);
                }
                setTimeout(update,1000);
            }
        };
        update();
    }
    boggle.onclose = () => { alert("lost connection."); };
});

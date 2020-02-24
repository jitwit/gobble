$(() => {
    var boggle = new WebSocket ("ws://" + window.location.host);
    var dt = 1000;

    add_word = async (word) => { boggle.send('gobble ' + word); };
    del_word = async (word) => { boggle.send('dobble ' + word); };
    query_words = async () => { boggle.send('words'); };
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

    timer = async (expires,round=90,pause=30) => {
        var end = expires.getTime();
        var show = async () => {
            var now = (new Date ()).getTime();
            var left = Math.floor((end-now+999)/1000);
            if (left > 0) {
                if (left > pause) { $("#timer").html("remaining " + (left-pause)); }
                else { $("#timer").html("next " + left); }
                setTimeout(show,dt);
            }
        }
        show();
    }

    boggle.onopen = (e) => { set_name(); };
    boggle.onmessage = async (msg) => {
        var res = JSON.parse(msg.data);
        if (res === "name-is-taken") { set_name(msg=" (previous one is taken)"); }
        else if (!(res['board'] == null)) { $("#gobble").html(res['board']); query_words(); }
        else if (!(res['words'] == null)) { $("#submissions").html(res['words']); }
        else if (!(res['peeps'] == null)) { $("#people").html(res['peeps']); }
        else if (!(res['time'] == null)) { timer(new Date (res['time']),res['round'],res['pause']); }
        else if (!(res['word_list'] == null)) { $("#word-list").html(res['word_list']); }
        else { console.log(res); }
    };
    boggle.onclose = () => { alert("lost connection."); };
});

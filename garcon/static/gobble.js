var boggle;

$(() => {
    if (window.history && window.history.pushState) {
        window.history.pushState('forward', null, './#forward');
        $(window).on('popstate', () => { alert('sure about that?'); });
    }

    boggle = new WebSocket ("ws://" + window.location.host);
    var dt = 1000;

    add_word = (word) => { boggle.send('gobble ' + word); };
    del_word = (word) => { boggle.send('dobble ' + word); };
    chirp = (tweet) => { boggle.send('chirp ' + tweet); };
    query_words = () => { boggle.send('words'); };
    set_name = (msg="") => {
        var name = prompt('name?' + msg);
        boggle.send(name);
        $("#scratch").focus();
    };

    $("#mush").submit((e) => {
        e.preventDefault();
        add_word($("#scratch").val());
        $("#scratch").val("");
        query_words();
    });
    
    $("#tweet").submit((e) => {
        e.preventDefault();
        chirp($("#scribble").val());
        $("#scribble").val("");
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
    
    boggle.onmessage = (msg) => {
        var res = JSON.parse(msg.data);
        if (res === "name-is-taken") { set_name(msg=" (previous one is taken)"); }
        if (!(res['board'] == null)) { $("#gobble").html(res['board']); $("#scratch").focus(); query_words(); }
        if (!(res['words'] == null)) { $("#submissions").html(res['words']); }
        if (!(res['peeps'] == null)) { $("#people").html(res['peeps']); } // defns, actually
        if (!(res['chirp'] == null)) { $("#tweets").html(res['chirp']); }
        if (!(res['pinou'] == null)) { $("#pinou").html(res['pinou']); }
        if (!(res['time'] == null)) { timer(new Date (res['time']),res['round'],res['pause']); }
        if (!(res['solution'] == null)) { $("#solution").html(res['solution']); }
        if (!(res['scores'] == null)) { $("#scores").html(res['scores']); }
        // idk: { console.log(res); }
    };
    boggle.onerror = (e) => {
        console.log(e);
    };
        
    boggle.onclose = () => { alert("lost connection."); };
});

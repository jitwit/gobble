var boggle;

$(() => {
    if (window.history && window.history.pushState) {
        window.history.pushState('forward', null, './#gob');
        window.history.pushState('forward', null, './#gobble');
        $(window).on('popstate', () => { alert('sure about that?'); });
    }

    boggle = new WebSocket ("ws://" + window.location.host);
    var dt = 1000; var attempts = 0; var attempt_limit = 5;
    var whoami = localStorage.getItem("whoami");

    add_word = (word) => { boggle.send('gobble ' + word); };
    del_word = (word) => { boggle.send('dobble ' + word); };
    like_word = (word) => { boggle.send('wobble ' + word); };
    chirp = (tweet) => { boggle.send('chirp ' + tweet); };
    query_words = () => { boggle.send('words'); };

    set_name = (msg="") => {
	whoami = prompt('name ?' + msg, whoami || "");
	localStorage.setItem("whoami", whoami);
	boggle.send(whoami);
	$("#scratch").focus();
    };
    
    clear_new_round = () => {
	$("#scratch").val(""); $("#scratch").focus(); query_words();
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

    $("#scores").on("click","div",(e) => {
        like_word($(e.target).text());
    });

    $("#disconnected").on("click","p",(e) => {
	console.log("hiho");
//	location.reload();
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
        var m = JSON.parse(msg.data);
        if (m === "name-is-taken") { set_name(msg=" (previous one is taken)"); }
        if (m === "name-is-empty") { set_name(msg=" (name must be something)"); }
        if (m === "name-too-long") { set_name(msg=" (name too long, should at most 12 letters)"); }
        if (!(m['board'] == null)) { $("#gobble").html(m['board']); clear_new_round (); }
        if (!(m['words'] == null)) { $("#submissions").html(m['words']); }
        if (!(m['peeps'] == null)) { $("#people").html(m['peeps']); } // defns, actually
        if (!(m['chirp'] == null)) { $("#tweets").html(m['chirp']); }
        if (!(m['pinou'] == null)) { $("#pinou").html(m['pinou']); }
        if (!(m['time'] == null)) { timer(new Date (m['time']),m['round'],m['pause']); }
        if (!(m['solution'] == null)) { $("#solution").html(m['solution']); }
        if (!(m['scores'] == null)) { $("#scores").html(m['scores']); }
        // idk: { console.log(res); }
    };
    boggle.onerror = (e) => {
	document.body.innerHTML = `<div id="disconnected">${e}</div>`;
    };

    boggle.onclose = () => {
	document.body.innerHTML = `<div id="disconnected">lost connection. try refreshing...</div>`;
    }; 
});

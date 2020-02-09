var vs_src = `#version 300 es
in vec2 pos;
out vec2 xy;
void main(void) {
  gl_Position = vec4(pos,0,1);
  xy = pos;
}
`;

var frag_src = `#version 300 es
precision mediump float;
in vec2 xy;
uniform float utime;
out vec4 clr;

void main (void) {
  float c;
  c = 2.0-(xy.y-2.0*(utime-1.0));
  c = max(0.0,min(1.0,c));
  c = pow(c,2.0);
  clr = vec4(c,c,c,1);
}
`;

var vertices = new Float32Array(
    [-1,-1,1,-1,1,1,-1,1]
);

$(() => {
    var boggle = new WebSocket ("ws://192.168.2.13:8000");
    var hourglass = document.querySelector("#hourglass");
    var gl = hourglass.getContext("webgl2");
    var dt = 50;
    var expires;
    var round;
    var pause;

    var vs = gl.createShader(gl.VERTEX_SHADER);
    var fs = gl.createShader(gl.FRAGMENT_SHADER);
    var program = gl.createProgram();
    
    gl.shaderSource(vs,vs_src);
    gl.compileShader(vs);
    gl.shaderSource(fs,frag_src);
    gl.compileShader(fs);

    gl.attachShader(program,vs);
    gl.attachShader(program,fs);
    gl.linkProgram(program);
    gl.useProgram(program);
    gl.deleteProgram(program);

    var pos = gl.getAttribLocation(program,"pos");
    var utime = gl.getUniformLocation(program, "utime");
    var vao = gl.createVertexArray();
    var buffer = gl.createBuffer();
    
    gl.bindBuffer(gl.ARRAY_BUFFER,buffer);
    gl.bufferData(gl.ARRAY_BUFFER,vertices,gl.STATIC_DRAW);
    gl.vertexAttribPointer(pos,2,gl.FLOAT,false,0,0);
    gl.enableVertexAttribArray(pos);
    gl.drawArrays(gl.TRIANGLE_FAN,0,4);

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
        console.log(round,pause);
        var end = expires.getTime();
        var show = async () => {
            var now = (new Date ()).getTime();
            var left = Math.floor(1+((end-now)/1000));
            if (left > 0) {
                if (left > pause) {
                    $("#timer").html("remaining " + (left-pause));
                } else {
                    $("#timer").html("next " + left);
                }
                setTimeout(show,1000);
            }
        }
        var render = async () => {
            var now = (new Date ()).getTime();
            var left = end-now;
            var time = (left/1000-pause)/round;// to seconds
            time = Math.max(time,-(round/pause)*Math.min(time,0));
            gl.uniform1f(utime, time);
            gl.drawArrays(gl.TRIANGLE_FAN,0,4);
            if (left > 0) { setTimeout(render,dt); }
        };
        show();
        render();
    }    
    
    boggle.onopen = (e) => { set_name(); };
    boggle.onmessage = async (msg) => {
        var res = JSON.parse(msg.data);
        if (res === "name-is-taken") { set_name(msg=" (previous one is taken)"); }
        else if (!(res['board'] == null)) { $("#gobble").html(res['board']); query_words(); }
        else if (!(res['words'] == null)) { $("#submissions").html(res['words']); }
        else if (!(res['peeps'] == null)) { $("#scores").html(res['peeps']); }
        else if (!(res['time'] == null)) { timer(new Date (res['time']),res['round'],res['pause']); }
        else { console.log(res); }
    };
    boggle.onclose = () => { alert("lost connection."); };
});

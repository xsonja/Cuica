var main = Elm.fullscreen(Elm.Main, {
	randomSeed : {code:" ", seed:100.0}
});

main.ports.sendKey.subscribe(keysound);

function keysound(cstr) {
    var c = cstr.toLowerCase();
    main.ports.randomSeed.send({code:c, seed:Date.now()});
    
	if (c == 'a') {
		playsound('Sounds/a_growshrink.mp3');
	} 
    else if (c==' '){
        playsound("Sounds/space_castanet.mp3");
    }
    else if (c=='b'){
        playsound("Sounds/b_bass.mp3");
    }
    else if (c=='c'){
        playsound("Sounds/c_guitar.mp3");
    }
    else if (c == 'd') {
        playsound('Sounds/d_pop.mp3');
    } 
    else if (c=='e'){
        playsound("Sounds/e_arpeggio.mp3");
    }
    else if (c=='f') {
        playsound("Sounds/f_snap.mp3");
    }
    else if (c == 'g') {
        playsound('Sounds/g_poss.mp3');
    } 
    else if (c == 'h') {
        playsound('Sounds/h_bounce.mp3');
    } 
    else if (c=='i'){
        playsound("Sounds/i_pitpat.mp3");
    }
    else if (c == 'j') {
        playsound('Sounds/j_bars_shimmer.mp3');
    }
    else if (c=='k'){
        playsound("Sounds/k_cuica.mp3");
    }
    else if (c=='l'){
        playsound("Sounds/l_shake.mp3");
    }
    else if (c == 'm') {
        playsound("Sounds/m_kickcym.mp3");
    }
    else if (c=='n'){
        playsound("Sounds/n_upshrink.mp3");
    }
    else if (c=='o'){
        playsound("SD0010.mp3");
    }
    else if (c=='p'){
        playsound("Sounds/p_jagged.mp3");
    }
    else if (c=='q'){
        playsound("Sounds/q_sent.mp3");
    }
    else if (c=='r'){
        playsound("Sounds/r_piano.mp3");
    }
    else if (c == 's') {
		playsound('Sounds/s_chord.mp3');
	} 
    else if (c == 't') {
        playsound('Sounds/t_bongo.mp3');
    }       
    else if (c=='u'){
        playsound("Sounds/u_clap.mp3");
    }
    else if (c=='v'){
        playsound("Sounds/v_flute.mp3");
    }
    else if (c=='w'){
        playsound("Sounds/w_click.mp3");
    }
    else if (c=='x'){
        playsound("Sounds/x_bell.mp3");
    }
    else if (c == 'y') {
        playsound("BD0000.mp3");
    }
    else if (c=='z'){
        playsound("Sounds/z_sine.mp3");
    } 
}

function playsound(string) {
	myAudio = new Audio(string); 
	myAudio.play();
}


//
//  Copyright (c) 2008 Kiyoka Nishiyama, All rights reserved.
//  See COPYING for terms and conditions of using this software
//
var timeToFocus   = 1000; // ms
var slideTime     =  800; // ms
var dropOutTime   = 1500; // ms

// for In-place edit
function otEditStart( bodyId ) {
    if( 'none' == $(bodyId + ':editarea').getStyle( 'display' )) {
	new Effect.SlideDown( $(bodyId + ':editarea'), {duration:slideTime / 1000.0} );
	setTimeout( function () {
	    $(bodyId + ':edittext').focus();
	    $(bodyId + ':edittext').select();
	}, timeToFocus );
    }
    else {
	new Effect.SlideUp( $(bodyId + ':editarea'), {duration:slideTime / 1000.0} );
    }
    return false;
}

function otEditEnd( bodyId ) {
    //new Effect.Shake( $(bodyId + ':editarea'), {duration:dropOutTime / 1000.0} );
    new Effect.DropOut( $(bodyId + ':editarea'), {duration:dropOutTime / 1000.0} );
}

// for tooltips.js
function setup_tooltips( ) {
    $$("body .help").each( function(input) {
	new Tooltip(input, {backgroundColor: "#FC9", borderColor: "#C96",
			    textColor: "#000", textShadowColor: "#FFF",
			    maxWidth: 400,
			    appearDuration: .15,
			    hideDuration: .15,
			    opacity: .85
			   });
    });
}


var timeToFocus = 2000; // ms
var timeToAlert = 4000; // ms

function editstart( bodyId ) {
    if( 'none' == $(bodyId + ':editarea').getStyle( 'display' )) {
	$(bodyId + ':editarea').appear();
	setTimeout( function () {
	    $(bodyId + ':edittext').focus();
	    $(bodyId + ':edittext').select();
	}, timeToFocus );
	setTimeout( function () {
	    alert( 'This edit feature is demonstration. you cannot update this wiki page, sorry...' );
	}, timeToAlert );
    }
    return false;
}

function editend( bodyId ) {
    $(bodyId + ':editarea').fade();
}

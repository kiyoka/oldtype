<script type="text/javascript" charset="utf-8">
$$("ul .help").each( function(link) {
    new Tooltip(link, {mouseFollow: false});
});
$$("p .help").each( function(input) {
    new Tooltip(input, {backgroundColor: "#333", borderColor: "#333", 
                        textColor: "#FFF", textShadowColor: "#000"});
});
$$("form input.help").each( function(input) {
    new Tooltip(input, {backgroundColor: "#FC9", borderColor: "#C96", 
                        textColor: "#000", textShadowColor: "#FFF"});
});
</script>

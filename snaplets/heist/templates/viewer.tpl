<!DOCTYPE html>
<html>
    <head>
        <script
        src="//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js">
        </script>
        <style>
            #nexts li div:hover {
                cursor : pointer;
            }
            #nexts li {
                margin-top : 5px;
            }
        </style>
    </head>
    <body>
        <div id=id style="display:none">
            <deformedId/>
        </div>
        <div id=appRoot style="display:none">
            <appRoot/>
        </div>
        <p>
            Your amazing explorer is amazing, exploring
        </p>
        <p id=currentParagraph>
        </p>
        <ol id=nexts>
        </ol>
        <script>
            var id = $("#id").text().trim();
            var appRoot = $("#appRoot").text().trim();
            var endPoint = appRoot + "/deform/" + id;
            var history = "";
            var clicker = function () {
                    $("#nexts").fadeOut(600);
                    $("#currentParagraph").text($(this).text());
                    clickedKey = $(this).attr("id");
                    history = history + ":" + clickedKey;
                    $.getJSON(endPoint, {history: history}, 
                        function(data, status) {
                        var items = [];
                        $.each(data["similars"], function(value) {
                                var item = data["similars"][value]
                                var k = item["id"];
                                var t = item["content"];
                                items.push("<li><div id=" + k + ">" + t + "</div></li>");
                            });
                        $("#nexts").html(items.join(''));
                        $.each($("#nexts li div"), function() {
                            $(this).click(clicker); 
                        });
                        $("#nexts").fadeIn(200);
                    });
                };
            $.getJSON(endPoint, function(data, status) {
                $("#currentParagraph").text(data["document"]);
                var items = [];
                $.each(data["similars"], function(value) {
                        var item = data["similars"][value]
                        var k = item["id"];
                        var t = item["content"];
                        items.push("<li><div id=" + k + ">" + t + "</div></li>");
                    });
                $("#nexts").html(items.join(''));
                $.each($("#nexts li div"), function() {
                    $(this).click(clicker);
                });
            });
        </script>
    </body>
</html>

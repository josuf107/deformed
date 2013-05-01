<!DOCTYPE html>
<html>
    <head>
        <script
        src="//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js">
        </script>
    </head>
    <body>
        <div id=id style="display:none">
            <deformedId/>
        </div>
        <div id=appRoot style="display:none">
            <appRoot/>
        </div>
        <h1>Hello!</h1>
        <p>Enter some text:</p>
        <form id=textForm method=post action="${appRoot}/deform/${deformedId}">
            <textarea id=text name=text value="" rows=40 cols=120></textarea>
            <input type=hidden id=redir name="redir"
            value="${appRoot}/deform/${deformedId}/explorer"/>
            <input type=submit value=explore>
        </form>
    </body>
</html>

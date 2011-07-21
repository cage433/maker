$(document).ready(function () {
    $("form").each(function (index, form) { new Form($(form)).bind() })
    $('#json2HTML').click(function () { $('.scala').hide(); $('.cs').hide(); $('.json').show() })
    $('#json2JSON').click(function () { $('.scala').hide(); $('.cs').hide(); $('.json').show() })
    $('#scala').click(function () { $('.json').hide(); $('.cs').hide(); $('.scala').show() })
    $('#cs').click(function () { $('.json').hide(); $('.scala').hide(); $('.cs').show() })
});

function Form(form) {
    var action = buildAction()

    form.attr("action", action).prepend(action).append("<img style='display: none;' src='ajax-loader.gif'/><code/><p/>")

    var formId  = "#" + form.attr("id")
    var inputs  = $(formId + " > input:text")
    var code    = $(formId + " > code")
    var errors  = $(formId + " > p")
    var spinner = $(formId + " > img")

    function buildAction() {
        var location = document.location.href
        var action = form.attr("action").length == 0 ? "" : form.attr("action") + "/"

        return (location.substring(0, location.lastIndexOf(".")) || location) + "/" + action
    }

    function getJson() {
        spinner.show()

        $.getJSON(url())
            .complete(function () { spinner.hide() })
            .success(success)
            .error(error)

        return false
    }

    function url() {
        return form.attr("action") + inputs.map(function (index, element) { return element.value }).get().join("/")
    }

    function success(json) {
        console.log("success")
        errors.hide(); inputs.removeClass("bad"); code.hide().empty().append(render(json)).fadeIn("fast")
    }

    function error(detail) {
        var responseText = $($.parseXML(detail.responseText)).find("div")
        responseText.find("p").css("display", "block").css("white-space", "pre")
        code.hide(); inputs.addClass("bad"); errors.hide().empty().append(responseText).fadeIn("fast")
    }

    function render(json) {
        return $('#json2HTML').is(':checked') ? HTML.display(json) : PRETTY.display(json)
    }

    return {
        bind: function () {
            form.submit(getJson) 
        }
    }
}
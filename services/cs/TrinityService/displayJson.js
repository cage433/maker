$(document).ready(function () {
    $("form").each(function (index, form) { new Form($(form)).bind() })
});

function Form(form) {
    var action = buildAction()

    form.attr("action", action).prepend(action).append("<img style='display: none;' src='ajax-loader.gif'/><code/>")

    var formId  = "#" + form.attr("id")
    var inputs  = $(formId + " > input:text")
    var code    = $(formId + " > code");
    var spinner = $(formId + " > img")

    function buildAction() {
        var location = document.location.href

        return (location.substring(0, location.lastIndexOf(".")) || location) + "/" + form.attr("action") + "/"
    }

    function getJson() {
        spinner.show()

        $.getJSON(url())
            .complete(function ()     { spinner.hide()                           })
            .success (function (json) { inputs.removeClass("bad"); display(json) })
            .error   (function ()     { inputs.addClass("bad")                   })

        return false
    }

    function url() {
        return form.attr("action") + inputs.map(function (index, element) { return element.value }).get().join("/")
    }

    function display(json) {
        code.hide().empty().append(render(json)).fadeIn("fast");
    }

    function render(json) {
        return $('#json2HTML').is(':checked') ? HTML.display(json) : PRETTY.display(json)
    }

    return {
        bind: function () { form.submit(getJson) }
    }
}
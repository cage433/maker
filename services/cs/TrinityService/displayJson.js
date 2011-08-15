$(document).ready(function () {
    VIS.data.trunc = false

    $('input[name=renderingStyle]').click(function () { renderer.show($(this).attr("id")) })
    $('#html').attr('checked', 'checked'); renderer.show("json")
    $('#service').each(function (index, service) {
        $.getJSON("/Doc/Meta" + $(service).attr('href'))
            .success(function (serviceMetaData) {
                $('#scalaTemplate').tmpl(serviceMetaData).appendTo('.scala')
                $('#csTemplate').tmpl(serviceMetaData).appendTo('.cs')
                $('#formsTemplate').tmpl(serviceMetaData).appendTo('.json')
                $("form").each(function (index, form) { new Form($(form)).bind() })
            })
    })
});

var renderer = {
    show: function(class) { $(".rendering").hide(); $("." + class).show() },
    pathParam: function(param) { return param.Binding == "Path" ? '@PathParam("' + param.Name + '") ' : '' },
    separator: function(sep, index, length) { return (index < length - 1) ? sep : ""},
    scalaParameter: function(index, param, length) {
        return this.pathParam(param) + param.Name + ": " + param.ParameterType.scalaType() + this.separator(", ", index, length)
    },
    csMethod: function(uri, verb) { return (verb == 'GET') ? "[WebGet UriTemplate='" + uri + "']" : "[WebInvoke UriTemplate = '" + uri + "' Method = '" + verb + "']"; },
    csParameter: function(index, param, length) { return param.ParameterType.packageless() + ' ' + param.Name + renderer.separator(", ", index, length) },
    parameterlessUri: function(uri, numParameters) {
        var result = uri.indexOf('{') < 0 ? uri : uri.substring(0, uri.indexOf('{'))

        return numParameters == 0 ? result.stripSuffix('/') : result
    }
}

$.extend(String.prototype, {
    stripSuffix:   function(suffix)    { return this.endsWith(suffix) ? this.substring(0, this.length - suffix.length) : this.toString() },
    stripPrefix:   function(prefix)    { return this.indexOf(prefix) == 0 ? this.substring(prefix.length) : this.toString()              },
    endsWith:      function(searchFor) { return this.lastIndexOf(searchFor) == (this.length - searchFor.length)                          },
    package:       function()          { return this.lastIndexOf(".") == -1 ? "" : this.substring(0, this.lastIndexOf("."))              },
    packageClause: function()          { return this.lastIndexOf(".") == -1 ? "" : "package " + this.package()                           },
    namespaceStart:function()          { return this.lastIndexOf(".") == -1 ? "" : "namespace " + this.package() + "\n{\n"               },
    namespaceEnd:  function()          { return this.lastIndexOf(".") == -1 ? "" : "}\n"                                                 },
    decapitalize:  function()          { return this.substring(0, 1).toLowerCase() + this.substring(1)                                   },
    isCapitalized: function()          { return this.length > 0 && this.charAt(0).toUpperCase() == this.charAt(0)                        },
    scalaType:     function()          { return this.packageless().replace("<", "[").replace(">", "]").replace("Int32", "Int")           },

    packageless:   function()          { 
        return this.indexOf("<") == -1 ? this.substring(this.lastIndexOf(".") + 1) : 
            $(this.split("<")).map(function(i, element) {return element.packageless()}).toArray().join("<")
    },
    packages: function() {
        if (this.indexOf("<") == -1) {
            return [this.package()] 
        } else {
            return [this.substring(0, this.indexOf("<")).package(), this.substring(this.indexOf("<") + 1).package()]
        }
    }
})

Array.prototype.pushIfMissing = function(elements) {
    var self = this;
    $(elements).each(function (i, element) { if (self.indexOf(element) == -1) self.push(element) })
}

function imports(serviceType, methods) { return importsImpl(serviceType, methods, "import", "._") }
function usings(serviceType, methods)  { return importsImpl(serviceType, methods, "using", ";") }

function importsImpl(serviceType, methods, prefix, suffix) {
    var types = []

    $(methods).each(function(mi, method) {
        types.pushIfMissing(method.ReturnType.packages())
        $(method.Parameters).each(function(pi, parameter) {
            types.pushIfMissing(parameter.ParameterType.packages())
        })
    })

    return $(types.sort()).not($(serviceType.package()))
        .filter(function(i, e) {return !e.isCapitalized() && e.length > 0})
        .map(function (i, type) { return prefix + " " + type + suffix}).toArray().join("\n") + "\n\n"
}

function Form(form) {
    var action = document.location.href + "/" + form.attr("action")

    form.attr("action", action).prepend(action).append("<img style='display: none;' src='/Doc/Files/ajax-loader.gif'/><code/><p/>")

    var formId  = "#" + form.attr("id")
    var inputs  = $(formId + " > input:text").autoGrowInput().trigger('keyup')
    var spinner = $(formId + " > img")
    var code    = $(formId + " > code")
    var errors  = $(formId + " > p")

    function getJson() {
        spinner.show()
        var url = form.attr("action") + inputs.map(function (index, element) { return element.value }).get().join("/")
        $.getJSON(url).complete(function () { spinner.hide() }).success(showJson).error(showError)

        return false
    }

    function showJson(json) {
        errors.hide(); inputs.removeClass("bad");
        $(formId + " > code > code > input[type=checkbox][name=keep]").not(':checked').parent().remove()
        var newCode = $('<code style="display: none;"><input type="checkbox" name="keep" value="" />Keep<br/></code>')
        var rendering = $('#html').is(':checked') ? HTML.display(json) : PRETTY.display(json)
        code.prepend(newCode); newCode.append(rendering).fadeIn("fast")
    }

    function showError(detail) {
        var responseText = $($.parseXML(detail.responseText)).find("div")
        responseText.find("p").css("display", "block").css("white-space", "pre")
        code.hide(); inputs.addClass("bad"); errors.hide().empty().append(responseText).fadeIn("fast")
    }

    return {
        bind: function () { form.submit(getJson) }
    }
}
$(document).ready(function () {
    VIS.data.trunc = false

    $('input[name=renderingStyle]').click(function () { renderer.show($(this).attr("id")) })
    $('#html').attr('checked', 'checked'); renderer.show("json")
    $('#service').each(function (index, service) {
        $.getJSON("/RPC/Doc/Meta" + $(service).attr('href').prefixWith("/"))
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
        return numParameters == 0 ? uri.takeUntil("{").stripSuffix('/') : uri.takeUntil("{")
    },
    example: function(example, parameters) { return (example != "") ? example : this.queryStructure(parameters) },
    queryStructure: function(parameters) {
        var pathParams = $(parameters).collect(function(i, param) { return (param.Binding == "Path") ? [param.Name] : [] }).toArray().join("/")
        var queryParams = $(parameters).collect(function(i, param) { return (param.Binding == "Query") ? [param.Name + "=" + param.Name] : [] }).toArray().join("&")

        return (queryParams.length == 0) ? pathParams : pathParams + "?" + queryParams
    }
}

jQuery.fn.extend({
    collect: function(f) { return this.map(function(i, e) { return f(i, e) }).filter(function(i, es) { return es.length > 0 }) }
})

$.extend(String.prototype, {
    takeUntil:     function(char)      { return this.contains(char) ? this.substring(0, this.indexOf(char)): this.toString()             },
    dropUntil:     function(char)      { return this.contains(char) ? this.substring(this.indexOf(char)) : ""                            },
    dropUntilIncl: function(char)      { return this.contains(char) ? this.substring(this.indexOf(char) + 1) : ""                        },
    prefixWith:    function(prefix)    { return this.startsWith(prefix) ? this.toString() : prefix + this.toString()                     },
    strip:         function(char)      { return this.split(char).join("")                                                                },
    stripSuffix:   function(suffix)    { return this.endsWith(suffix) ? this.substring(0, this.length - suffix.length) : this.toString() },
    stripPrefix:   function(prefix)    { return this.startsWith(prefix) ? this.substring(prefix.length) : this.toString()                },
    startsWith:    function(searchFor) { return this.indexOf(searchFor) == 0                                                             },
    endsWith:      function(searchFor) { return this.lastIndexOf(searchFor) == (this.length - searchFor.length)                          },
    package:       function()          { return this.contains(".") ? this.substring(0, this.lastIndexOf(".")) : ""                       },
    packageClause: function()          { return this.contains(".") ? "package " + this.package() : ""                                    },
    namespaceStart:function()          { return this.contains(".") ? "namespace " + this.package() + "\n{\n" : ""                        },
    namespaceEnd:  function()          { return this.contains(".") ? "}\n" : ""                                                          },
    decapitalize:  function()          { return this.substring(0, 1).toLowerCase() + this.substring(1)                                   },
    isCapitalized: function()          { return this.length > 0 && this.charAt(0).toUpperCase() == this.charAt(0)                        },
    scalaType:     function()          { return this.packageless().replace("<", "[").replace(">", "]").replace("Int32", "Int")           },
    contains:      function(char)      { return this.indexOf(char) >= 0                                                                  },

    packageless:   function()          {
        return this.contains("<") ? $(this.split("<")).map(function(i, e) { return e.packageless() }).toArray().join("<")
            : this.substring(this.lastIndexOf(".") + 1)
    },
    packages: function() {
        return this.contains("<") ? [this.takeUntil("<").package(), this.dropUntilIncl("<").package()] : [this.package()]
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

    return $(types.sort()).not($(serviceType.package())).collect(function(i, type) {
        return (!type.isCapitalized() && type.length > 0) ? [prefix + " " + type + suffix] : []
    }).toArray().join("\n") + "\n\n"
}

function Form(form) {
    var service = $("body").attr("href")
    var root = document.location.pathname.substring(0, document.location.pathname.indexOf(service))
    var action = document.location.href.replace($("body").attr("href"), "").stripSuffix("/") + "/" + form.attr("action")

    form.attr("action", action).prepend(action).append("<img style='display: none;' src='" + root + "/Doc/Files/ajax-loader.gif'/><code/><p/>")

    var formId  = "#" + form.attr("id")
    var inputs  = $(formId + " > input:text").autoGrowInput().trigger('keyup')
    var spinner = $(formId + " > img")
    var code    = $(formId + " > code")
    var errors  = $(formId + " > p")

    function getJson() {
        spinner.show()
        var url = form.attr("action") + inputs.map(function (index, element) { return element.value }).get().join("/")
//        console.log(url)
        $.getJSON(url).complete(function () { spinner.hide() }).success(showJson).error(showError)

        return false
    }

    function showJson(json) {
        errors.hide().empty(); inputs.removeClass("bad");
        $(formId + " > code > code > input[type=checkbox][name=keep]").not(':checked').parent().remove()
        var newCode = $('<code style="display: none;"><input type="checkbox" name="keep" value="" />Keep<br/></code>')
        var rendering = $('#html').is(':checked') ? HTML.display(json) : PRETTY.display(json)
        code.prepend(newCode); newCode.append(rendering).fadeIn("fast")
    }

    function showError(detail) {
//        console.log("showError.detail: ", detail)
        var responseText = errorText(detail)
//        console.log("showError.responseText: ", responseText)
        $(formId + " > code > code > input[type=checkbox][name=keep]").not(':checked').parent().remove()
        inputs.addClass("bad"); errors.hide().empty().append(responseText).fadeIn("fast")
        errors.append(detail.responseText)
//        console.log("showError.done")
    }

    function errorText(detail) {
        try {
            var responseText = $($.parseXML(detail.responseText)).find("div")
            responseText.find("p").css("display", "block").css("white-space", "pre")
            return responseText
        } catch (err) {
            return detail.responseText
        }
    }

    return {
        bind: function () { form.submit(getJson) }
    }
}
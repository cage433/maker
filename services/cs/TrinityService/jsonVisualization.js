/*global COOKIE: true, HTML: true, PRETTY: true, STRUCTURE: true, VIS: true,
         $, Event, Dom, Lang, window, document, YAHOO
*/

/** @fileoverview
  *
  * 2010-05-15: JSLint compliance. HTML5 elements (and layout changes). New
  *             "Validation" function. HTML output now supports collapsing
  *             regions, like a tree-view would.
  *
  * 2009-05-05: Support for new ways to mangle the input:
  *               Remove Line Breaks
  *               Decode URI
  *               Trim non-JSON
  *
  * 2009-03-13: Support for pretty-printing JSON. Better cookie handling.
  *
  * 2009-03-08: "clear" now clears the JSON location field as well.
  *
  * 2009-03-06: Improved default sort of array-type data structrues to be
  *               numeric instead of alphabetical.
  *             User's preferences will be saved in a cookie and restored
  *               next time they visit they page.
  *
  * 2009-02-24: Avoid creation of duplicate ID values when rendering an array.
  *             Enhanced "click-for-path" in array, object, and structure to
  *               handle clicks in the cell but not in the data itself.
  *
  * 2009-02-20: Improved support for detecting data structures.
  *             Provided ability to click in the output to generate a
  *             path for reaching that value.
  *
  * 2009-02-14: Added support for detecting data-table like structures.
  *             Pass current version of JSLint.
  */

/*! Non-minified version: default.js */

// HTML5 elements act "funny" in IE unless you create a few beforehand.
/*@cc_on
document.createElement("output");
document.createElement("footer");
document.createElement("time");
@*/

if (!Array.prototype.indexOf) {
	/** Returns the first index at which a given element can be found in the
	  * array, or -1 if it is not present. This algorithm is exactly the one
	  * used in Firefox and SpiderMonkey.
	  *
	  * @param  {Object}    elt    Element to locate in the array.
	  * @param  {Integer}  [from]  The index at which to begin the search.
	  *
	  *                            Defaults to 0, i.e. the whole array will be
	  *                            searched.
	  *
	  *                            If the index is greater than or equal to
	  *                            the length of the array, -1 is returned,
	  *                            i.e. the array will not be searched.
	  *
	  *                            If negative, it is taken as the offset
	  *                            from the end of the array.
	  *
	  *                            Note that even when the index is negative,
	  *                            the array is still searched from front to
	  *                            back. If the calculated index is less than
	  *                            0, the whole array will be searched.
	  * @return {Number} */
	Array.prototype.indexOf = function (elt) {
		var len  = this.length,
			from = Number(arguments[1]) || 0;

		from = (from < 0) ? Math.ceil(from) : Math.floor(from);

		if (from < 0) {
			from += len;
		}

		for (; from < len; from++) {
			if (this.hasOwnProperty(from) && this[from] === elt) {
				return from;
			}
		}
		return -1;
	};
}


if (!Array.prototype.lastIndexOf) {
	/** Returns the last index at which a given element can be found in the
	  * array, or -1 if it is not present. The array is searched backwards,
	  * starting at fromIndex. This algorithm is exactly the one used in
	  * Firefox and SpiderMonkey.
	  *
	  * @param  {Object}    elt    Element to locate in the array.
	  * @param  {Integer}  [from]  The index at which to start searching
	  *                            backwards.
	  *
	  *                            Defaults to the array's length, i.e. the
	  *                            whole array will be searched.
	  *
	  *                            If the index is greater than or equal to
	  *                            the length of the array, the whole array
	  *                            will be searched. If negative, it is taken
	  *                            as the offset from the end of the array.
	  *                            Note that even when the index is negative,
	  *                            the array is still searched from back to
	  *                            front. If the calculated index is less than
	  *                            0, -1 is returned, i.e. the array will not
	  *                            be searched.
	  * @return {Number} */
	Array.prototype.lastIndexOf = function (elt) {
		var len  = this.length,
			from = Number(arguments[1]);

		if (isNaN(from)) {
			from = len - 1;
		} else {
			from = (from < 0) ? Math.ceil(from) : Math.floor(from);
			if (from < 0) {
				from += len;
			} else if (from >= len) {
				from = len - 1;
			}
		}

		for (; from > -1; from--) {
			if (this.hasOwnProperty(from) && this[from] === elt) {
				return from;
			}
		}
		return -1;
	};
}


if (!Array.prototype.some) {
	/** Tests whether some element in the array passes the test implemented
	  * by the provided function. This algorithm is exactly the one used in
	  * Firefox and SpiderMonkey.
	  *
	  * @param  {Function}  fun     Function to test for each element.
	  * @param  {Object}   [thisp]  Object to use as this when executing fun.
	  * @return {Boolean} */
	Array.prototype.some = function (fun) {
		var i, thisp, len = this.length;
		if (typeof fun !== "function") {
			throw new TypeError();
		}

		thisp = arguments[1];
		for (i = 0; i < len; i++) {
			if (this.hasOwnProperty(i) && fun.call(thisp, this[i], i, this)) {
				return true;
			}
		}

		return false;
	};
}



if (!Array.prototype.every) {
	/** Tests whether all elements in the array pass the test implemented by
	  * the provided function. This algorithm is exactly the one used in
	  * Firefox and SpiderMonkey.
	  *
	  * @param  {Function}  fun     Function to test for each element.
	  * @param  {Object}   [thisp]  Object to use as this when executing fun.
	  * @return {Boolean} */
	Array.prototype.every = function (fun) {
		var i, thisp, len = this.length;
		if (typeof fun !== "function") {
			throw new TypeError();
		}

		thisp = arguments[1];
		for (i = 0; i < len; i++) {
			if (this.hasOwnProperty(i) && !fun.call(thisp, this[i], i, this)) {
				return false;
			}
		}

		return true;
	};
}



if (!Array.prototype.forEach) {
	/** Executes a provided function once per array element.
	  * This algorithm is exactly the one used in Firefox and SpiderMonkey.
	  * @param  {Function}  fun     Function to test for each element.
	  * @param  {Object}   [thisp]  Object to use as this when executing fun.
	  */
	Array.prototype.forEach = function (fun) {
		var i, thisp, len = this.length;
		if (typeof fun !== "function") {
			throw new TypeError();
		}

		thisp = arguments[1];
		for (i = 0; i < len; i++) {
			if (this.hasOwnProperty(i)) {
				fun.call(thisp, this[i], i, this);
			}
		}
	};
}



if (!Array.prototype.filter) {
	/** Creates a new array with all elements that pass the test implemented
	  * by the provided function. This algorithm is exactly the one used in
	  * Firefox and SpiderMonkey.
	  *
	  * @param  {Function}  fun     Function to test for each element.
	  * @param  {Object}   [thisp]  Object to use as this when executing fun.
	  * @return {Array} */
	Array.prototype.filter = function (fun) {
		var res, thisp, i, val, len = this.length;
		if (typeof fun !== "function") {
			throw new TypeError();
		}

		res = [];
		thisp = arguments[1];
		for (i = 0; i < len; i++) {
			if (this.hasOwnProperty(i)) {
				val = this[i]; // in case fun mutates this
				if (fun.call(thisp, val, i, this)) {
					res.push(val);
				}
			}
		}

		return res;
	};
}



if (!Array.prototype.map) {
	/** Creates a new array with the results of calling a provided function on
	  * every element in this array. This algorithm is exactly the one used in
	  * Firefox and SpiderMonkey.
	  *
	  * @param  {Function} fun
	  * @param  {Object}   [thisp]
	  * @return {Array} */
	Array.prototype.map = function (fun) {
		var res, thisp, i, len = this.length;
		if (typeof fun !== "function") {
			throw new TypeError();
		}

		res = [];
		res.length = len;
		thisp = arguments[1];
		for (i = 0; i < len; i++) {
			if (this.hasOwnProperty(i)) {
				res[i] = fun.call(thisp, this[i], i, this);
			}
		}

		return res;
	};
}



if (!Array.prototype.reduce) {
	/** Apply a function simultaneously against two values of the array
	  * (from left-to-right) as to reduce it to a single value.
	  * This algorithm is exactly the one used in Firefox and SpiderMonkey.
	  *
	  * @param  {Function}  callback      Function to execute on each value in
	  *                                   the array.
	  *
	  * @param  {Object}   [initialValue] Object to use as the first argument
	  *                                   to the first call of the callback.
	  * @return {Object} */
	Array.prototype.reduce = function (fun) {
		var rv,
			len = this.length,
			i   = 0;

		if (typeof fun !== "function") {
			throw new TypeError();
		}

		// no value to return if no initial value and an empty array
		if (len === 0 && arguments.length === 1) {
			throw new TypeError();
		}

		if (arguments.length >= 2) {
			rv = arguments[1];
		} else {
			for (; i < len; i += 1) {
				if (this.hasOwnProperty(i)) {
					rv = this[i++];
					break;
				}
			}

			// if array contains no values, no initial value to return
			if (i === len && !this.hasOwnProperty(len - 1)) {
				throw new TypeError();
			}
		}

		for (; i < len; i++) {
			if (this.hasOwnProperty(i)) {
				rv = fun.call(null, rv, this[i], i, this);
			}
		}

		return rv;
	};
}


if (!Array.prototype.reduceRight) {
	/** Apply a function simultaneously against two values of the array
	  * (from right-to-left) as to reduce it to a single value. This algorithm
	  * is exactly the one used in Firefox and SpiderMonkey.
	  *
	  * @param  {Function}  callback       Function to execute on each value
	  *                                    in the array.
	  *
	  * @param  {Object}   [initialValue]  Object to use as the first argument
	  *                                    to the first call of the callback.
	  * @return {Object} */
	Array.prototype.reduceRight = function (fun) {
		var rv,
			len = this.length,
			i   = len - 1;

		if (typeof fun !== "function") {
			throw new TypeError();
		}

		// no value to return if no initial value, empty array
		if (len === 0 && arguments.length === 1) {
			throw new TypeError();
		}

		if (arguments.length >= 2) {
			rv = arguments[1];
		} else {
			for (i = len - 1; i >= 0; i -= 1) {
				if (this.hasOwnProperty(i)) {
					rv = this[i--];
					break;
				}
			}

			// if array contains no values, no initial value to return
			if (i === -1 && !this.hasOwnProperty(0)) {
				throw new TypeError();
			}
		}

		for (; i >= 0; i--) {
			if (this.hasOwnProperty(i)) {
				rv = fun.call(null, rv, this[i], i, this);
			}
		}

		return rv;
	};
}





/** @namespace */

VIS = {

	/** Dom id values that we find interesting
	  * @property
	  * @type {object}
	  */
	data: {
		input:       'jsonInput',
		output:      'jsonOutput',
		validateOut: 'jsonValidateOutput',
		size:        'jsonSize',
		inStrict:    'jsonStrict',
		inEval:      'jsonEval',
		outHTML:     'json2HTML',
		outJSON:     'json2JSON',
		preserve:    'jsonSpace',
		dates:       'jsonDate',
		dataTables:  'jsonData',
		trunc:       'jsonTrunc',
		location:    'jsonLocation',
		options:     'jsonOptionSet',
		outputSet:   'jsonOutputSet'
	},

	/** The series of paths through the rendered json object that we discover.
	  * When the user clicks on a value, the nearest ID attribute of the click
	  * target is matched up with an index for this array.  The value at that
	  * index is the path that must be used to reach the target value.
	  *
	  * @property
	  * @type {array}
	  */
	paths: [],

	/** Resolve all ID values.
	  * This is called automatically upon page load.
	  * This also turns off firefox's spellcheck on the input box.
	  * Also does settings according to the user's last render cookie.
	  */
	init: function () {
		var x, cookie;

		for (x in this.data) {
			if (this.data.hasOwnProperty(x)) {
				this.data[x] = $(this.data[x]);
			}
		}

		if (this.data.input.spellcheck === true) {
			this.data.input.spellcheck = false;
		}

		[
			{ el: this.data.output,    fn: this.doClick                  },
			{ el: this.data.outputSet, fn: this.update_option_visibility },
			{ el: 'cmdRender',         fn: this.render                   },
			{ el: 'cmdValidate',       fn: this.validate                 },
			{ el: 'cmdClear',          fn: this.clear                    },
			{ el: 'cmdEncode',         fn: this.doEncode                 },
			{ el: 'cmdCollapse',       fn: this.collapseAll              },
			{ el: 'cmdExpand',         fn: this.expandAll                },
			{ el: 'cmdRemoveCRLF',     fn: this.removeCRLF               },
			{ el: 'cmdDecodeURI',      fn: this.decodeURI                },
			{ el: 'cmdTrim2JSON',      fn: this.trimToJSON               },
			{ el: 'cmdHelp',           fn: this.help                     },
			{ el: 'cmdBeer',           fn: this.beer                     }
		].forEach(function (o) {
			Event.addListener(o.el, 'click', o.fn, this, true);
		}, this);

		Event.addListener(
			this.data.validateOut,
			'mouseover',
			this.overValidation,
			this,
			true
		);

		Event.addListener(
			this.data.validateOut,
			'mouseout',
			this.overValidation,
			this,
			true
		);

		cookie = COOKIE.get('json');
		cookie = (cookie) ? cookie : '101111';
		cookie = cookie.split('');
		this.data.inStrict.checked   = (cookie[0] === '1');
		this.data.inEval.checked     = (cookie[0] === '0');
		this.data.preserve.checked   = (cookie[1] === '1');
		this.data.dates.checked      = (cookie[2] === '1');
		this.data.dataTables.checked = (cookie[3] === '1');
		this.data.trunc.checked      = (cookie[4] === '1');
		this.data.outHTML.checked    = (cookie[5] === '1');
		this.data.outJSON.checked    = (cookie[5] === '0');

		this.update_option_visibility();
	},


	update_option_visibility: function () {
		var style = this.data.outHTML.checked ? 'HTML' : 'PRETTY';

		this.data.options.className = style;

		if (style === 'PRETTY') {
			document.getElementById('htmlCommands')
				.style.visibility = 'hidden';
		} else {
			document.getElementById('htmlCommands')
				.style.visibility = '';
		}

	},


	help: function (e) {
		Event.stopEvent(e);
		var target = Event.getTarget(e);

		window.open(
			target.href,
			target.target,
			'width=550,resizable=yes,scrollbars=yes'
		);

		return false;
	},


	beer: function (e) {
		Event.stopEvent(e);
		var target = Event.getTarget(e);

		window.open(
			target.href,
			target.target,
			'width=550,height=300,resizable=yes,scrollbars=yes'
		);

		return false;
	},


	lastHover: null,
	/** When the mouse is over a list item in the validation area, highlight
	  * the corresponding error, and vice versa.
	  */
	overValidation: function (e) {

		var target = Event.getTarget(e),
			name = target ? target.nodeName.toLowerCase() : '',
			related;

		if (name !== 'span') {
			return;
		}

		related =
			document.getElementById(
				(target.className === 'ERR') ?
					'HIGHLIGHT_' + target.id.substring(4) :
					'ERR_' + target.id.substring(10)
			);

		if (this.lastHover) {
			Dom.removeClass(this.lastHover.target,  'hover');
			Dom.removeClass(this.lastHover.related, 'hover');
			this.lastHover = null;
		}

		if (e.type === 'mouseover') {
			Dom.addClass(target, 'hover');
			Dom.addClass(related, 'hover');
			this.lastHover = {
				target:  target,
				related: related
			};
		}

	},


	/** Handle a click event in the output area.
	  * This determines the path required to reach a given element.
	  *
	  * @param {Event} e
	  */
	doClick: function (e) {
		var target = Event.getTarget(e),
			path;

		function findID(el) {
			return (el.id !== '');
		}

		if (
			this.data.outHTML.checked &&
				target.nodeName.toLowerCase() === 'caption'
		) {
			HTML.toggleArea(target);
		}

		if (target.id === '') {
			target = Dom.getAncestorBy(target, findID);
		}

		path = target.id;
		if (path === this.data.output.id) {
			path = '';
		}
		path = this.findPath(path);

		this.data.location.innerHTML = path;
	},


	/** This regular expression is used to help resolve the ID attribute of a
	  * clicked-on item into a corresponding index for the "paths" array.
	  * @property
	  * @type {RegExp}
	  */
	pathRE: /^px?(\d+)$/,

	/** Given the string value of an ID attribute, this returns the
	  * corresponding path value, as it exists in the "paths" array.
	  * @param  {string} path
	  * @return {string}
	  */
	findPath: function (path) {
		if (this.pathRE.test(path) === false) {
			return '';
		}
		path = Number(this.pathRE.exec(path)[1]);
		return (path > this.paths.length) ? '' : this.paths[path];
	},


	/** Static regular expression, used to detect dates.
	  * @property
	  * @type {RegExp}
	  */
	dateRE: /^(\d{4})-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z$/,


	/** Clear the display and prepare for new input. */
	clear: function () {
		this.reset();
		this.data.input.value = '';
		this.data.input.focus();
	},

	/** Parse the textarea's value as JSON.
	  * @return {object}
	  */
	parse: function (json) {

		/*jslint evil: true */
		var result;

		try {
			result = this.data.inStrict.checked ?
				Lang.JSON.parse(json) :
				eval("(" + json + ")");
		} catch (ex) {
			result = ex;
		}

		/*jslint evil: false */
		return result;
	},
	

	/** Parse the textarea's value as JSON.
	  * Re-encode as strict JSON.
	  */
	doEncode: function () {

		var json = this.data.input.value;

		this.reset();
		if (Lang.trim(json).length === 0) {
			return;
		}

		json = this.parse(json);

		if (json instanceof Error) {
			json = this.validate();
		}

		if (json instanceof Error) {
			return;
		}

		json = Lang.JSON.stringify(json);
		this.data.output.innerHTML =
			'<pre>' + this.html(json) + '</pre>';
	},

	/** Parse the textarea's value as JSON.
	  * Render that json as HTML.
	  * Also show the size of the JSON in characters.
	  * If this is IE, fixes the width of tables to accomodate the captions.
	  * Also set a cookie indicating the user's preferences.
	  */
	render: function () {

		var json = this.data.input.value;
		if (Lang.trim(json).length === 0) {
			this.reset();
			return;
		}

		json = this.parse(json);

		if (json instanceof Error) {
			json = this.validate();
		}

		this.visualize(json);
	},


	/** Reset the display */
	reset: function () {
		this.data.location.innerHTML = '';
		this.data.output.className = '';
		this.data.output.innerHTML = '';
		this.data.validateOut.innerHTML = '';
		this.data.size.innerHTML = '';
		this.lastHover = null;
	},


	visualize: function (token) {
		var size = this.data.input.value.length,
			style = this.data.outHTML.checked ? HTML : PRETTY,

			cookie = [
				this.data.inStrict.checked   ? '1' : '0',
				this.data.preserve.checked   ? '1' : '0',
				this.data.dates.checked      ? '1' : '0',
				this.data.dataTables.checked ? '1' : '0',
				this.data.trunc.checked      ? '1' : '0',

				this.data.outHTML.checked     ? '1' :
					this.data.outJSON.checked ? '0' : '2'

			].join('');

		this.paths = ['root'];

		this.data.output.className =
			this.data.outHTML.checked ? 'HTML' : 'PRETTY';

		this.data.output.innerHTML = style.display(token, 0);

		size = this.formatSize(size) + ' characters';
		size = this.html(size);
		this.data.size.innerHTML = size;

		if (COOKIE.get('json') !== cookie) {
			COOKIE.set('json', cookie);
		}

		/*@cc_on
		if (this.data.outHTML.checked) {
			this.fixTableWidths();
		}
		@*/

	},


	/** Validate the provided JSON according to "strict" mode, as defined at
	  * http://www.json.org/
	  *
	  * Since we need to know WHY something is invalid, we must tokenize the
	  * json manually.
	  *
	  * This tokenizer is incomplete.  It will not handle json containing
	  * unicode values greater than \uFFFF.  Those seem to be quite obscure
	  * anyhow.  Perhaps I'll fix it later.
	  *
	  * @return {Any}    Since this tokenizer is more forgiving than either
	  *                  native JSON or window.eval, some methods (render and
	  *                  re-encode) will attempt to tokenize with this if the
	  *                  default tokenization fails.  Therefore, this method
	  *                  will attempt to return a partially tokenized result,
	  *                  even if it failed somewhere along the way.
	  *
	  */
	validate: function () {

		var output, stopped, token, tokenize, unicodeCombiners, unicodeLetters,
			json = this.data.input.value,
			cursor = 0,
			eof = (json.length === 0),
			digitStart = /^[\-0-9]$/,
			warnings = [],


			// These regex's were produced algorithmically, from Standard
			// ECMA-262, 3rd edition.  They could have been one big expression,
			// but that would be ugly.

			whiteSpace = [
				/^[\t\v\r\n\f\x20\xA0]$/,
				/^[\u1680\u180E\u2000-\u200A\u2028\u2029\u202F\u205F\u3000]$/
			],

			unicodeControl = [
				/^[\u0000-\u001f\uffda-\uffdc]$/
			],

			unicodeConnectors = [
				/^[\u005F\u203F\u2040\u2054\uFE33\uFE34\uFE4D-\uFE4F\uFF3F]$/
			],

			unicodeDigits = [
				/^[\u0030-\u0039\u0660-\u0669\u06f0-\u06f9\u07c0-\u07c9]$/,
				/^[\u0966-\u096f\u09e6-\u09ef\u0a66-\u0a6f\u0ae6-\u0aef]$/,
				/^[\u0b66-\u0b6f\u0be6-\u0bef\u0c66-\u0c6f\u0ce6-\u0cef]$/,
				/^[\u0d66-\u0d6f\u0e50-\u0e59\u0ed0-\u0ed9\u0f20-\u0f29]$/,
				/^[\u1040-\u1049\u1090-\u1099\u17e0-\u17e9\u1810-\u1819]$/,
				/^[\u1946-\u194f\u19d0-\u19d9\u1b50-\u1b59\u1bb0-\u1bb9]$/,
				/^[\u1c40-\u1c49\u1c50-\u1c59\ua620-\ua629\ua8d0-\ua8d9]$/,
				/^[\ua900-\ua909\uaa50-\uaa59\uffda-\uffdc]$/
			];

		unicodeCombiners = [
			/^[\u0300-\u036f\u0483-\u0487\u0591-\u05bd\u05c1-\u05c2\u05bf]$/,
			/^[\u05c4-\u05c5\u0610-\u061a\u064b-\u065e\u06d6-\u06dc\u05c7]$/,
			/^[\u06df-\u06e4\u06e7-\u06e8\u06ea-\u06ed\u0730-\u074a\u0670]$/,
			/^[\u07a6-\u07b0\u07eb-\u07f3\u0901-\u0903\u093e-\u094d\u0711]$/,
			/^[\u0951-\u0954\u0962-\u0963\u0981-\u0983\u09be-\u09c4\u093c]$/,
			/^[\u09c7-\u09c8\u09cb-\u09cd\u09e2-\u09e3\u0a01-\u0a03\u09bc]$/,
			/^[\u0a3e-\u0a42\u0a47-\u0a48\u0a4b-\u0a4d\u0a70-\u0a71\u09d7]$/,
			/^[\u0a81-\u0a83\u0abe-\u0ac5\u0ac7-\u0ac9\u0acb-\u0acd\u0a3c]$/,
			/^[\u0ae2-\u0ae3\u0b01-\u0b03\u0b3e-\u0b44\u0b47-\u0b48\u0a51]$/,
			/^[\u0b4b-\u0b4d\u0b56-\u0b57\u0b62-\u0b63\u0bbe-\u0bc2\u0a75]$/,
			/^[\u0bc6-\u0bc8\u0bca-\u0bcd\u0c01-\u0c03\u0c3e-\u0c44\u0abc]$/,
			/^[\u0c46-\u0c48\u0c4a-\u0c4d\u0c55-\u0c56\u0c62-\u0c63\u0b3c]$/,
			/^[\u0c82-\u0c83\u0cbe-\u0cc4\u0cc6-\u0cc8\u0cca-\u0ccd\u0b82]$/,
			/^[\u0cd5-\u0cd6\u0ce2-\u0ce3\u0d02-\u0d03\u0d3e-\u0d44\u0bd7]$/,
			/^[\u0d46-\u0d48\u0d4a-\u0d4d\u0d62-\u0d63\u0d82-\u0d83\u0cbc]$/,
			/^[\u0dcf-\u0dd4\u0dd8-\u0ddf\u0df2-\u0df3\u0e34-\u0e3a\u0d57]$/,
			/^[\u0e47-\u0e4e\u0eb4-\u0eb9\u0ebb-\u0ebc\u0ec8-\u0ecd\u0dca]$/,
			/^[\u0f18-\u0f19\u0f3e-\u0f3f\u0f71-\u0f84\u0f86-\u0f87\u0dd6]$/,
			/^[\u0f90-\u0f97\u0f99-\u0fbc\u102b-\u103e\u1056-\u1059\u0e31]$/,
			/^[\u105e-\u1060\u1062-\u1064\u1067-\u106d\u1071-\u1074\u0eb1]$/,
			/^[\u1082-\u108d\u1712-\u1714\u1732-\u1734\u1752-\u1753\u0f35]$/,
			/^[\u1772-\u1773\u17b6-\u17d3\u180b-\u180d\u1920-\u192b\u0f37]$/,
			/^[\u1930-\u193b\u19b0-\u19c0\u19c8-\u19c9\u1a17-\u1a1b\u0f39]$/,
			/^[\u1b00-\u1b04\u1b34-\u1b44\u1b6b-\u1b73\u1b80-\u1b82\u0fc6]$/,
			/^[\u1ba1-\u1baa\u1c24-\u1c37\u1dc0-\u1de6\u1dfe-\u1dff\u108f]$/,
			/^[\u20d0-\u20dc\u20e5-\u20f0\u2de0-\u2dff\u302a-\u302f\u135f]$/,
			/^[\u3099-\u309a\ua67c-\ua67d\ua823-\ua827\ua880-\ua881\u17dd]$/,
			/^[\ua8b4-\ua8c4\ua926-\ua92d\ua947-\ua953\uaa29-\uaa36\u18a9]$/,
			/^[\uaa4c-\uaa4d\ufe00-\ufe0f\uffda-\uffdc\u20e1\ua66f\ua802]$/,
			/^[\ua806\ua80b\uaa43\ufb1e]$/
		];

		unicodeLetters = [
			/^[\u0041-\u005a\u0061-\u007a\u00c0-\u00d6\u00d8-\u00f6\u00aa]/,
			/^[\u00f8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u0370-\u0374\u00b5]/,
			/^[\u0376-\u0377\u037a-\u037d\u0388-\u038a\u038e-\u03a1\u00ba]/,
			/^[\u03a3-\u03f5\u03f7-\u0481\u048a-\u0523\u0531-\u0556\u02ec]/,
			/^[\u0561-\u0587\u05d0-\u05ea\u05f0-\u05f2\u0621-\u064a\u02ee]/,
			/^[\u066e-\u066f\u0671-\u06d3\u06e5-\u06e6\u06ee-\u06ef\u0386]/,
			/^[\u06fa-\u06fc\u0712-\u072f\u074d-\u07a5\u07ca-\u07ea\u038c]/,
			/^[\u07f4-\u07f5\u0904-\u0939\u0958-\u0961\u0971-\u0972\u0559]/,
			/^[\u097b-\u097f\u0985-\u098c\u098f-\u0990\u0993-\u09a8\u06d5]/,
			/^[\u09aa-\u09b0\u09b6-\u09b9\u09dc-\u09dd\u09df-\u09e1\u06ff]/,
			/^[\u09f0-\u09f1\u0a05-\u0a0a\u0a0f-\u0a10\u0a13-\u0a28\u0710]/,
			/^[\u0a2a-\u0a30\u0a32-\u0a33\u0a35-\u0a36\u0a38-\u0a39\u07b1]/,
			/^[\u0a59-\u0a5c\u0a72-\u0a74\u0a85-\u0a8d\u0a8f-\u0a91\u07fa]/,
			/^[\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2-\u0ab3\u0ab5-\u0ab9\u093d]/,
			/^[\u0ae0-\u0ae1\u0b05-\u0b0c\u0b0f-\u0b10\u0b13-\u0b28\u0950]/,
			/^[\u0b2a-\u0b30\u0b32-\u0b33\u0b35-\u0b39\u0b5c-\u0b5d\u09b2]/,
			/^[\u0b5f-\u0b61\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u09bd]/,
			/^[\u0b99-\u0b9a\u0b9e-\u0b9f\u0ba3-\u0ba4\u0ba8-\u0baa\u09ce]/,
			/^[\u0bae-\u0bb9\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0a5e]/,
			/^[\u0c2a-\u0c33\u0c35-\u0c39\u0c58-\u0c59\u0c60-\u0c61\u0abd]/,
			/^[\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0ad0]/,
			/^[\u0cb5-\u0cb9\u0ce0-\u0ce1\u0d05-\u0d0c\u0d0e-\u0d10\u0b3d]/,
			/^[\u0d12-\u0d28\u0d2a-\u0d39\u0d60-\u0d61\u0d7a-\u0d7f\u0b71]/,
			/^[\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dc0-\u0dc6\u0b83]/,
			/^[\u0e01-\u0e30\u0e32-\u0e33\u0e40-\u0e46\u0e81-\u0e82\u0b9c]/,
			/^[\u0e87-\u0e88\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0bd0]/,
			/^[\u0eaa-\u0eab\u0ead-\u0eb0\u0eb2-\u0eb3\u0ec0-\u0ec4\u0c3d]/,
			/^[\u0edc-\u0edd\u0f40-\u0f47\u0f49-\u0f6c\u0f88-\u0f8b\u0cbd]/,
			/^[\u1000-\u102a\u1050-\u1055\u105a-\u105d\u1065-\u1066\u0cde]/,
			/^[\u106e-\u1070\u1075-\u1081\u10a0-\u10c5\u10d0-\u10fa\u0d3d]/,
			/^[\u1100-\u1159\u115f-\u11a2\u11a8-\u11f9\u1200-\u1248\u0dbd]/,
			/^[\u124a-\u124d\u1250-\u1256\u125a-\u125d\u1260-\u1288\u0e84]/,
			/^[\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u0e8a]/,
			/^[\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u0e8d]/,
			/^[\u1318-\u135a\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u0ea5]/,
			/^[\u166f-\u1676\u1681-\u169a\u16a0-\u16ea\u1700-\u170c\u0ea7]/,
			/^[\u170e-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176c\u0ebd]/,
			/^[\u176e-\u1770\u1780-\u17b3\u1820-\u1877\u1880-\u18a8\u0ec6]/,
			/^[\u1900-\u191c\u1950-\u196d\u1970-\u1974\u1980-\u19a9\u0f00]/,
			/^[\u19c1-\u19c7\u1a00-\u1a16\u1b05-\u1b33\u1b45-\u1b4b\u103f]/,
			/^[\u1b83-\u1ba0\u1bae-\u1baf\u1c00-\u1c23\u1c4d-\u1c4f\u1061]/,
			/^[\u1c5a-\u1c7d\u1d00-\u1dbf\u1e00-\u1f15\u1f18-\u1f1d\u108e]/,
			/^[\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f5f-\u1f7d\u10fc]/,
			/^[\u1f80-\u1fb4\u1fb6-\u1fbc\u1fc2-\u1fc4\u1fc6-\u1fcc\u1258]/,
			/^[\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u12c0]/,
			/^[\u1ff6-\u1ffc\u2090-\u2094\u210a-\u2113\u2119-\u211d\u17d7]/,
			/^[\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u17dc]/,
			/^[\u2183-\u2184\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2c6f\u18aa]/,
			/^[\u2c71-\u2c7d\u2c80-\u2ce4\u2d00-\u2d25\u2d30-\u2d65\u1f59]/,
			/^[\u2d80-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u1f5b]/,
			/^[\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u1f5d]/,
			/^[\u2dd8-\u2dde\u3005-\u3006\u3031-\u3035\u303b-\u303c\u1fbe]/,
			/^[\u3041-\u3096\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u2071]/,
			/^[\u3105-\u312d\u3131-\u318e\u31a0-\u31b7\u31f0-\u31ff\u207f]/,
			/^[\ua000-\ua48c\ua500-\ua60c\ua610-\ua61f\ua62a-\ua62b\u2102]/,
			/^[\ua640-\ua65f\ua662-\ua66e\ua67f-\ua697\ua717-\ua71f\u2107]/,
			/^[\ua722-\ua788\ua78b-\ua78c\ua7fb-\ua801\ua803-\ua805\u2115]/,
			/^[\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\u2124]/,
			/^[\ua90a-\ua925\ua930-\ua946\uaa00-\uaa28\uaa40-\uaa42\u2126]/,
			/^[\uaa44-\uaa4b\uf900-\ufa2d\ufa30-\ufa6a\ufa70-\ufad9\u2128]/,
			/^[\ufb00-\ufb06\ufb13-\ufb17\ufb1f-\ufb28\ufb2a-\ufb36\u214e]/,
			/^[\ufb38-\ufb3c\ufb40-\ufb41\ufb43-\ufb44\ufb46-\ufbb1\u2d6f]/,
			/^[\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\u2e2f]/,
			/^[\ufe70-\ufe74\ufe76-\ufefc\uff21-\uff3a\uff41-\uff5a\u3400]/,
			/^[\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\u4db5]/,
			/^[\uffda-\uffdc\u4e00\u9fc3\uac00\ud7a3\ufb1d\ufb3e]/
		];

		function warn(msg, lengthOfError) {

			var position = cursor,
				lastLF = json.lastIndexOf('\n', position),
				line = 1,
				character = position - (lastLF + 1),
				warning = {
					message: msg
				};

			while (lastLF !== -1) {
				line += 1;
				lastLF = json.lastIndexOf('\n', lastLF - 1);
			}

			if (lengthOfError === undefined) {
				warning.position = position;
				warning.line = line;
				warning.character = character;
			} else {
				warning.endPosition = position;
				warning.endLine = line;
				warning.endCharacter = character;

				position = position - lengthOfError + 1;
				lastLF = json.lastIndexOf('\n', position);
				line = 1;
				character = position - (lastLF + 1);

				while (lastLF !== -1) {
					line += 1;
					lastLF = json.lastIndexOf('\n', lastLF - 1);
				}

				warning.startPosition = position;
				warning.startLine = line;
				warning.startCharacter = character;

			}

			warnings.push(warning);
		}

		function characterInRange(character, range) {

			var x;

			if (character.length !== 1) {
				return false;
			}

			for (x = 0; x < range.length; x += 1) {
				if (range[x].test(character) === true) {
					return true;
				}
			}

			return false;
		}

		function rollback(n) {
			if (n === undefined) {
				n = 1;
			}
			cursor -= n;
			eof = (cursor > json.length);
		}

		function consume(n) {
			if (n === undefined) {
				n = 1;
			}
			var output = json.substring(cursor, cursor + n);
			cursor += n;
			eof = (cursor > json.length);
			return output;
		}

		function peek(n) {
			if (n === undefined) {
				n = 1;
			}
			return json.substring(cursor, cursor + n);
		}

		function consumeWhitespace() {
			var space = consume();
			while (characterInRange(space, whiteSpace) && !eof) {
				space = consume();
			}
			if (eof) {
				return false;
			}
			rollback();
			return true;
		}

		function consumeDigits() {
			var output = '';

			do {
				switch (peek()) {
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					output += consume();
					break;
				default:
					return output;
				}
			} while (!eof);

			return output;
		}

		function tokenize_unicodeEscape() {

			var expected = /^\\u[0-9a-fA-F]{4}$/;

			if (expected.test(peek(6)) === false) {
				return false;
			}

			return consume(6);
		}

		function tokenize_identifier() {

			var output = '';

			function tokenStart() {

				if (token === '\\') {

					rollback();
					token = tokenize_unicodeEscape();
					if (token === false) {
						token = consume(); // attempt to move on.
						warn('Expected unicode escape sequence.');
						return false;
					}

					return true;
				}

				return (token === '$' || token === '_' ||
					characterInRange(token, unicodeLetters));
			}


			// IdentifierStart may be a unicode letter, dollar sign,
			// underscore, or a backslash followed by a unicode escape.
			token = consume();
			if (tokenStart() === false) {

				throw new Error(
					'Identifier must start with a unicode letter, dollar ' +
						'sign, underscore, or unicode escape sequence.'
				);

			}

			output += token;

			// All remaining tokens have more relaxed rules:
			// All tokenStart characters are legal, plus:
			// unicode combining mark, unicode digit, or unicode connector
			// punctuation.
			do {

				token = consume();
				if (
					tokenStart() ||
						characterInRange(token, unicodeCombiners) ||
						characterInRange(token, unicodeDigits) ||
						characterInRange(token, unicodeConnectors)
				) {

					output += token;

				} else {

					rollback();

					if (VIS.reserved.indexOf(output) !== -1) {
						warn(
							"Reserved words may not be identifiers.",
							output.length
						);
					}

					return output;

				}

			} while (!eof);

			// in case of eof...
			if (VIS.reserved.indexOf(output)) {
				warn("Reserved words may not be identifiers.", output.length);
			}

			return output;
		}

		function tokenize_string(delimit) {

			if (delimit === undefined) {
				delimit = '"';
			}

			var output = '', complete = false;

			// Consume initial delimiter.
			consume();

			do {
				token = consume();

				if (eof) {
					throw new Error("Unexpected end of input.");
				}

				if (token === delimit) {
					complete = true;

				} else if (token === '\\') {
					token = consume();

					if (eof) {
						throw new Error("Unexpected end of input.");

					} else if (token === delimit) {
						output += delimit;

					// JSON.org indicates that \" is a valid escape
					// character, but \' is not. I presume this is because
					// strings must be delimited by " and not '.  Since that
					// is treated as a warning here, it seems logical that the
					// reverse warning should also apply: if ' is a delimiter,
					// then \" should not be a valid escape.
					} else if (
						(token === "'" && delimit === '"') ||
							(token === '"' && delimit === "'")
					) {
						warn("Unnecessary escapement.", 2);
						output += delimit;

					} else {
						switch (token) {
						case "\\":
							output += '\\';
							break;
						case "/":
							output += '/';
							break;
						case "b":
							output += '\b';
							break;
						case "f":
							output += '\f';
							break;
						case "n":
							output += '\n';
							break;
						case "r":
							output += '\r';
							break;
						case "t":
							output += '\t';
							break;
						case "u":
							rollback(2);
							token = tokenize_unicodeEscape();
							if (token === false) {
								output += consume(2);
								warn("Invalid unicode escape sequence.", 2);
							} else {
								output += token;
							}
							break;
						default:
							warn("Invalid escape sequence.", 2);
							output += "\\" + token;
						}
					}

				} else {

					if (characterInRange(token, unicodeControl)) {
						warn(
							"Character is not legal in this position. Use " +
								"an escape sequence to represent this " +
								"character."
						);
					}

					output += token;
				}

			} while (!complete);

			return output;
		}

		function tokenize_object() {

			var property, propLen, obj = { }, complete = false;

			// consume brace
			consume();

			// Consume until non-whitespace is found.
			if (consumeWhitespace() === false) {
				throw new Error("Unexpected end of input.");
			}

			token = peek();

			// If this is a closing brace, we are done.
			if (token === '}') {
				consume();
				return obj;
			}

			do {

				// Properties must have double quotes.
				if (token !== '"') {

					property =
						(token === "'") ?
						tokenize_string("'") :
						tokenize_identifier();

					// If we just got a string, increase the length of the
					// warning by two characters to account for the quotes.
					propLen =
						property.length +
						((token === '"' || token === "'") ? 2 : 0);

					warn(
						"Object properties must be enclosed in double quotes.",
						propLen
					);

				} else {
					property = tokenize_string();
					propLen = property.length + 2;
				}

				if (eof) {
					throw new Error("Unexpected end of input.");
				}

				if (obj.hasOwnProperty(property)) {
					warn("Duplicate property name: " + property, propLen);
				}

				// Now look for some white space, then a colon.
				if (consumeWhitespace() === false) {
					throw new Error("Unexpected end of input.");
				}

				token = consume();
				if (token !== ':') {
					throw new Error(
						"Expecting a colon to separate properties " +
							"from values."
					);
				}

				// Consume more whitespace.
				if (consumeWhitespace() === false) {
					throw new Error("Unexpected end of input.");
				}

				token = peek();

				if (token === ',' || token === '}') {
					throw new Error("All properties must have values.");
				}

				// Now consume this value.
				obj[property] = tokenize();

				if (eof) {
					throw new Error("Unexpected end of input.");
				}

				// Consume more whitespace, then a possible comma.
				if (consumeWhitespace() === false) {
					throw new Error("Unexpected end of input.");
				}

				token = consume();

				// Closing brace?
				if (token === '}') {
					complete = true;

				} else if (token !== ',') {
					throw new Error(
						"Object properties must be separated by commas."
					);

				// More whitespace...
				} else if (consumeWhitespace() === false) {
					throw new Error("Unexpected end of input.");

				}

				// Set the cursor to the next property, so we can loop back.
				token = peek();

			} while (!complete);

			return obj;
		}

		function tokenize_array() {

			var output = [], complete = false;

			// Consume brace.
			consume();

			if (consumeWhitespace() === false) {
				throw new Error("Unexpected end of input.");
			}

			token = peek();

			// Closing brace ends the array.
			if (token === ']') {
				consume();
				return output;
			}

			do {

				// Issue warnings for empty array values.
				while (token === ',') {

					consume();
					warn("Empty array values are not allowed.");

					if (consumeWhitespace() === false) {
						throw new Error("Unexpected end of input.");
					}
					token = peek();

					// Closing brace ends the array.
					if (token === ']') {
						consume();
						return output;
					}

				}

				output[output.length] = tokenize();

				if (eof) {
					throw new Error("Unexpected end of input.");
				}

				// Whitespace...
				if (consumeWhitespace() === false) {
					throw new Error("Unexpected end of input.");
				}

				token = consume();

				// Closing brace?
				if (token === ']') {
					complete = true;

				// Otherwise, must have a comma.
				} else if (token !== ',') {
					throw new Error(
						"Array values must be separated by commas."
					);

				// Whitespace...
				} else if (consumeWhitespace() === false) {
					throw new Error("Unexpected end of input.");

				}

				token = peek();

			} while (!complete);

			return output;
		}

		function tokenize_number() {

			token = peek();

			var one2nine = /^[1-9]$/,
				output = '';

			// Optional negativity.
			if (token === '-') {
				output += consume();
				token = peek();
				if (eof) {
					throw new Error("Unexpected end of input.");
				}
			}

			// If the first digit is 1 - 9, we consume as many more digits
			// as possible.
			if (one2nine.test(token)) {

				output += consumeDigits();
				if (eof) {
					return Number(output);
				}
				token = peek();

			// If the first digit was a zero, then we must follow with either
			// a period or an exponent marker.
			} else if (token === '0') {

				output += consume();
				if (eof) {
					return Number(output);
				}
				token = peek();

				// Invalid leading zero is a warning, but we attempt to
				// continue.
				if (one2nine.test(token)) {
					warn("Invalid leading zero.");
					output += consumeDigits();

					if (eof) {
						return Number(output);
					}
					token = peek();
				}

			} else {
				throw new Error("Expecting a number.");
			}

			if (token === '.') {
				output += consume();
				token = consumeDigits();
				if (token.length === 0) {
					warn("Invalid decimal point.");
				}
				output += token;
				token = peek();
			}

			if (token === 'e' || token === 'E') {
				output += consume();
				token = peek();

				if (token === '+' || token === '-') {
					output += consume();
				}

				token = consumeDigits();
				if (token.length === 0) {
					throw new Error("Invalid exponent.");
				}
				output += token;
			}

			return Number(output);
		}

		tokenize = function () {

			var x;

			token = peek();

			// curly brace; starts an object
			if (token === "{") {
				return tokenize_object();
			}

			// square brace; starts an array
			if (token === '[') {
				return tokenize_array();
			}

			if (token === '"') {
				return tokenize_string();
			}

			if (token === "'") {
				x = tokenize_string("'");
				warn(
					"Strings must be delimited by double quotes.",
					x.length + 2
				);
				return x;
			}

			if (token === '-' && peek(9) === '-Infinity') {
				consume(9);
				warn('"-Infinity" is not a legal JSON literal.', 9);
				return -Infinity;
			}

			if (digitStart.test(token)) {
				return tokenize_number();
			}

			if (token === 't' && peek(4) === 'true') {
				consume(4);
				return true;
			}

			if (token === 'f' && peek(5) === 'false') {
				consume(5);
				return false;
			}

			if (token === 'n' && peek(4) === 'null') {
				consume(4);
				return null;
			}

			if (token === 'u' && peek(9) === 'undefined') {
				consume(9);
				warn('"undefined" is not a legal JSON literal.', 9);
				return undefined;
			}

			if (token === 'N' && peek(3) === 'NaN') {
				consume(3);
				warn('"NaN" is not a legal JSON literal.', 3);
				return NaN;
			}

			if (token === 'I' && peek(8) === 'Infinity') {
				consume(8);
				warn('"Infinity" is not a legal JSON literal.', 8);
				return Infinity;
			}

			throw new Error("Unexpected content.");
		};

		this.reset();
		if (eof) {
			return undefined;
		}

		try {
			stopped = false;
			token = tokenize();
		} catch (ex) {
			stopped = true;
			warn(ex.message);
			warn("Stopping: unable to continue.");
			token = ex;
		}

		if (stopped === false && cursor !== json.length) {
			output = consume(json.length - cursor);
			warn("Unexpected content.", output.length);
			rollback(output.length);
		}

		warnings = warnings.sort(function (a, b) {
			a = a.hasOwnProperty('startPosition') ?
				a.startPosition : a.position;

			b = b.hasOwnProperty('startPosition') ?
				b.startPosition : b.position;

			return a - b;
		});

		if (warnings.length === 0) {

			this.data.validateOut.innerHTML = '<h3>Valid JSON</h3>';
			return token;

		}

		output =
			'<h3 class="ERR">Invalid JSON</h3><ol>' +

			warnings.map(function (warning, idx) {

				if (warning.message === 'Stopping: unable to continue') {
					return '<li class="ERR">' + warning.message + '</li>';
				}

				var template =
					warning.hasOwnProperty('startPosition') ?
					(warning.startLine !== warning.endLine) ?

					'<li>From line <span class="NUMBER">{startLine}</span>,' +
					' character <span class="NUMBER">{startCharacter}</span>' +
					' to line <span class="NUMBER">{endLine}</span>, ' +
					' character <span class="NUMBER">{endCharacter}</span>:' +
					' <span class="ERR" id="ERR_{idx}">{message}</span></li>' :

					'<li>On line <span class="NUMBER">{startLine}</span>, ' +
					'characters <span class="NUMBER">{startCharacter}</span>' +
					'-<span class="NUMBER">{endCharacter}</span>: ' +
					'<span class="ERR" id="ERR_{idx}">{message}</span></li>' :

					'<li>On line <span class="NUMBER">{line}</span>, ' +
					'character <span class="NUMBER">{character}</span>: ' +
					'<span class="ERR" id="ERR_{idx}">{message}</span></li>';

				return Lang.substitute(template, {
					idx:            idx.toString(),
					startLine:      VIS.formatSize(warning.startLine),
					startCharacter: VIS.formatSize(warning.startCharacter),
					endLine:        VIS.formatSize(warning.endLine),
					endCharacter:   VIS.formatSize(warning.endCharacter),
					message:        VIS.html(warning.message),
					line:           VIS.formatSize(warning.line),
					character:      VIS.formatSize(warning.character)
				});

			// Note the extra space before the </pre>; this is for errors
			// at the end of the input.
			}).join('') + '</ol><hr /><pre>' + VIS.html(json) + ' </pre><hr />';

		this.data.validateOut.innerHTML = output;

		// For each error, create a span that encloses the affected
		// characters, and associate it with the error message.
		// First step is to locate the <pre>.
		output =
			this.data.validateOut.getElementsByTagName('hr')[0]
			.nextSibling;

		warnings.forEach(function (warning, idx) {

			var start, end, region, prev, cur, next,
				complete = output.firstChild;

			if (warning.hasOwnProperty('startPosition')) {

				start = warning.startPosition - 1;
				end   = warning.endPosition;

			} else {

				start = warning.position - 1;
				end   = warning.position;

			}

			if (YAHOO.env.ua.ie) {
				// IE takes two characters for line feeds, which we must
				// correct now.
				start -= (warning.startLine - 1);
				end   -= (warning.startLine - 1);
			}

			prev = output.appendChild(document.createTextNode(
				complete.nodeValue.substring(0, start)
			));

			cur = output.appendChild(document.createElement('span'));

			next = output.appendChild(document.createTextNode(
				complete.nodeValue.substring(end)
			));

			cur.appendChild(document.createTextNode(
				complete.nodeValue.substring(start, end)
			));

			cur.className = 'ERR';

			output.removeChild(complete);

			// ta-da!  Now we know where these characters are located!
			region = Dom.getRegion(cur);

			output.appendChild(complete);
			output.removeChild(prev);
			output.removeChild(cur);
			output.removeChild(next);

			cur.removeChild(cur.firstChild);
			cur.className = 'HIGHLIGHT';
			cur.id = 'HIGHLIGHT_' + idx.toString();
			this.data.validateOut.appendChild(cur);
			cur.style.width = region.width + 'px';
			cur.style.height = region.height + 'px';
			Dom.setXY(cur, region);
			Dom.setStyle(cur, 'opacity', '0.5'); // should prolly be in CSS

			warning.highlight = cur;
		}, this);

		return token;
	},


	/** Collapse all sections (HTML view only) **/
	collapseAll: function () {
		this.expandOrCollapse('-');
	},

	/** Expand all sections (HTML view only) **/
	expandAll: function () {
		this.expandOrCollapse('+');
	},

	/** Expand or collapse all HTML sections.
	  * @param {string} toggleState    State to flip.
	  */
	expandOrCollapse: function (toggleState) {
		var x, state,
			captions = this.data.output.getElementsByTagName('caption'),
			toFlip = [];

		for (x = 0; x < captions.length; x += 1) {
			state = captions[x].firstChild.nodeValue.substring(1, 2);
			if (state === toggleState) {
				toFlip[toFlip.length] = captions[x];
			}
		}

		toFlip.forEach(HTML.toggleArea);
	},


	/** Fixes up line break problems, caused by copy-pasting from firebug **/
	removeCRLF: function () {
		this.data.input.value =
			this.data.input.value
			.replace(/^\s\s*/, '')
			.replace(/\s\s*$/, '')
			.replace(/\n/g, '');
	},


	/** Decode URI Component from input **/
	decodeURI: function () {
		var input = this.data.input;
		input.value = decodeURIComponent(input.value);
	},


	/** Trims the input value of all sorts of stuff, looking for the first
	  * set of either {} or [] that can be found.
	  *
	  * If more than one outermost set is found, prompt the user about what
	  * to do.
	  */
	trimToJSON: function () {
		var start, c, x,
			input    = this.data.input,
			value    = input.value,
			inString = null,
			opener   = null,
			closer   = null,
			depth    = 0,
			pairs    = [];

		value = value.replace(/^\s\s*/, '').replace(/\s\s*$/, '');

		// Cycle through each character in the string.
		// Either { or [ may start a JSON segment.
		// Once in a JSON segment, either ' or " may start a string.
		// Once in a string, a string must be terminated by a matching ' or "
		// In a string, the matching ' or " may be escaped by \
		// The JSON segment is ended by a matching ] or }
		// Multiple JSON segments may exist.
		for (x = 0; x < value.length; x++) {
			c = value.charCodeAt(x);
			switch (c) {
			case 34: // "
			case 39: // '
				// Ignore if we are not in a JSON segment.
				if (opener === null) {
					break;
				}
				// If we are in a string, check to see if this closes it.
				if (inString) {
					if (
						c === inString &&
							value.charCodeAt(x - 1) !== 92 // 92 === \ (escape)
					) {
						inString = null;
					}
				// Otherwise, we start a string.
				} else {
					inString = c;
				}
				break;

			case 123: // {
			case 91:  // [
				// Ignore if we are inside a string.
				if (inString) {
					break;
				}
				// If we aren't in one already, then this marks the
				// beginning of a JSON segment.
				if (opener === null) {
					opener = c;
					closer = opener + 2;
					depth++;
					start = x;
				// If we are already in a segment, then and this opens
				// another segment of the same type, we have to increase
				// the depth... So the closing brace for the inner segment
				// does not close the outer segment.
				} else if (c === opener) {
					depth++;
				}
				break;

			case 125: // }
			case 93:  // ]
				// Ignore if we are inside a string.
				if (inString) {
					break;
				}

				// If this is a closing brace that matches our opening brace,
				// decrease the depth by one.
				if (c === closer) {
					depth--;
					// If we reach zero-depth, we finished the JSON segment.
					// Stash it into the array and move on.
					if (depth === 0) {
						pairs.push(value.substring(start, x + 1));
						opener = null;
					}
				}

				break;
			}
		}

		switch (pairs.length) {
		case 0:
			window.alert('No braces found.');
			return;

		case 1:
			this.data.input.value = pairs[0];
			return;

		default:
			if (
				window.confirm(
					"Multiple potential objects found.\n" +
						"Wrap all objects in an array?"
				)
			) {
				this.data.input.value = "[" + pairs.join(",") + "]";
			}
			return;
		}
	},


	/** Format a number to be human-readable, with commas and whatnot.
	  *
	  * @param  {number} n
	  * @return {string}
	  */
	formatSize: function (n) {
		if (isNaN(n) || n.length === 0) {
			return '';
		}

		var s = n.toString(),
			i = s.length - 3;

		while (i >= 1) {
			s = s.substring(0, i) + ',' + s.substring(i, s.length);
			i -= 3;
		}
		return s;
	},


	/** Prepare a string for insertion as HTML.
	  * @param  {string} s
	  * @return {string}
	  */
	html: function (s) {
		return s
			.toString()
			.replace(/&/g, '&amp;')
			.replace(/</g, '&lt;')
			.replace(/>/g, '&gt;');
	},


	/** This regular expresion helps identify whether a string could be a
	  * legal javascript variable name.  Javascript variables must start with
	  * a letter, underscore, or $ symbol. Subsequent characters may also
	  * include numbers.
	  * @property
	  * @type {RegExp}
	  */
	variableRE: /^[a-z_$][\w$]*$/i,

	/** This is a list of reserved words in javascript. They cannot be valid
	  * variable names, so when we render a path, these words cannot be used
	  * with dot-notation.
	  * @property
	  * @type {array}
	  */
	reserved: [
		'abstract', 'boolean', 'break', 'byte', 'case', 'catch', 'char',
		'class', 'const', 'continue', 'debugger', 'default', 'delete', 'do',
		'double', 'else', 'enum', 'export', 'extends', 'false', 'final',
		'finally', 'float', 'for', 'function', 'goto', 'if', 'implements',
		'import', 'in', 'instanceof', 'int', 'interface', 'long', 'native',
		'new', 'null', 'package', 'private', 'protected', 'public', 'return',
		'short', 'static', 'super', 'switch', 'synchronized', 'this', 'throw',
		'throws', 'transient', 'true', 'try', 'typeof', 'var', 'void',
		'volatile', 'while', 'with'
	],

	/** Returns a string as it would need to be encoded if it were going to
	  * be used to access the property of an object in javascript.
	  * This will add either a dot for dot-notation, or use square brackets
	  * if necessary.
	  * @param  {string} s
	  / @return {string}
	  */
	variable: function (s) {
		if (
			this.variableRE.test(s) === true &&
				this.reserved.indexOf(s) === -1
		) {
			// No need to HTML encode this.
			// Characters that are not legal HTML are also not legal in
			// javascript variables, and would have been detected by the
			// regular expression.
			return '.' + s;
		}

		return '[<span class="STRING">' +
			this.html(Lang.JSON.stringify(s)) + '</span>]';
	},


	/** When calculating the width of a table, IE8- does not take the width
	  * of the caption into account.  This results in visual oddities, where
	  * the caption will spill outside of its container.  We fix this in IE
	  * by locating each table, calculating the correct width, and setting it
	  * manually.
	  */
	fixTableWidths: function () {

		var x, container, tableWidth, captionWidth,
			captions = this.data.output.getElementsByTagName('caption');

		for (x = 0; x < captions.length; x += 1) {

			tableWidth = captions[x].parentNode.offsetWidth;
			captionWidth = captions[x].scrollWidth + 4;
			container = captions[x].parentNode.parentNode;

			if (container.nodeName.toLowerCase() === 'td') {
				container.style.width =
					Math.max(tableWidth, captionWidth).toString() + 'px';
			}
		}

	}


};


PRETTY = {

	indent: 0,
	newLine: function () {
		var x, output = '<br />';
		for (x = 0; x < this.indent; x++) {
			output += '\u00A0\u00A0\u00A0\u00A0';
		}
		return output;
	},

	display: function (obj, path) {

		return Lang.isArray(obj) ?
			this.array(obj, path) :

			Lang.isBoolean(obj) ?
			this.bool(obj, path) :

			Lang.isFunction(obj) ?
			this.func(obj, path) :

			Lang.isNull(obj) ?
			'<span id="p' + path + '" title="null" class="NULL">null</span>' :

			Lang.isNumber(obj) ?
			'<span id="p' + path + '" title="Number" class="NUMBER">' +
			obj.toString() + '</span>' :

			Lang.isString(obj) ?
			this.string(obj, path) :

			Lang.isUndefined(obj) ?
			'<span id="p' + path +
			'" title="undefined" class="UNDEF">undefined</span>' :

			(obj instanceof Error) ?
			this.err(obj, path) :

			(obj instanceof Date) ?
			this.date(obj, path) :

			(obj instanceof RegExp) ?
			HTML.regExp(obj, path) :

			Lang.isObject(obj) ?
			this.obj(obj, path) :

			isNaN(obj) ?
			'<span id="p' + path + '" title="NaN" class="ERR">NaN</span>' :

			(obj === Infinity) ?
			'<span id="p' + path +
			'" title="Infinity" class="ERR">Infinity</span>' :
			
			'<span id="p' + path + '" class="IDK">[Unknown Data Type]</span>';
	},

	array: function (a, path) {
		var x, output = [];

		this.indent++;

		for (x = 0; x < a.length; x++) {
			VIS.paths.push(
				VIS.paths[path] + '[<span class="NUMBER">' + x + '</span>]'
			);

			output[x] = this.display(a[x], VIS.paths.length - 1);
		}

		output = this.newLine() + output.join(',' + this.newLine());

		this.indent--;

		return (a.length) ?
			'<span id="p' + path + '" class="ARRAY">[' +
			output + this.newLine() + ']</span>' :

			'<span id="p' + path + '" class="ARRAY">[\u00A0]</span>';
	},

	bool: function (b, path) {
		return (b) ?
			'<span id="p' + path +
			'" title="Boolean" class="BOOL">true</span>' :

			'<span id="p' + path +
			'" title="Boolean" class="BOOL">false</span>';
	},

	func: function (f, path) {
		var i, s = f.toString();
		if (VIS.data.trunc.checked) {
			i = s.indexOf('{') + 50;
			if (i < s.length) {
				s = VIS.html(s.substring(0, i)) + '\u2026\n}';
				s = s.replace(/\n/g, this.newLine());
				return '<code id="p' + path +
					'" title="Function (truncated)" class="FUNC">' +
					s + '</code>';
			}
		}
		s = VIS.html(s).replace(/\n/g, this.newLine());

		return '<code id="p' + path + '" title="Function" class="FUNC">' +
			s + '</code>';
	},

	string: function (s, path) {

		if (VIS.data.dates.checked && VIS.dateRE.test(s)) {
			return this.date(s, path);
		}

		s = Lang.JSON.stringify(s);

		if (VIS.data.trunc.checked && s.length > 68) {
			s = s.substring(1, s.length - 1);
			s = s.substring(0, 67) + '\u2026';
			s = '"' + s + '"';
		}

		return '<span id="p' + path + '" title="String" class="STRING">' +
			VIS.html(s) + '</span>';
	},

	err: function (e, path) {
		if (e.message === 'parseJSON') {
			return '<span id="p' + path +
				'" title="Error" class="ERR">Invalid JSON</span>';
		}

		VIS.paths.push(VIS.paths[path] + '.message');
		return '<span id="p' + path +
			'" title="Error" class="ERR">new Error(' +
			this.string(e.message, VIS.paths.length - 1) + ')</span>';
	},

	date: function (d, path) {
		return '<span id="p' + path + '" title="Date" class="DATE">' +
			Lang.JSON.stringify(d) + '</span>';
	},

	obj: function (o, path) {
		var x, body = [];

		this.indent++;

		for (x in o) {
			if (o.hasOwnProperty(x)) {
				VIS.paths.push(VIS.paths[path] + VIS.variable(x));

				body.push(
					'<span id="px' + (VIS.paths.length - 1) + '">' +
						Lang.JSON.stringify(x) + ': ' +
						this.display(o[x], VIS.paths.length - 1) + '</span>'
				);
			}
		}

		if (body.length) {
			body = this.newLine() + body.join(',' + this.newLine());
		}

		this.indent--;

		return (body.length) ?
			'<span id="p' + path + '" class="OBJ">{' +
			body + this.newLine() + '}</span>' :

			'<span id="p' + path + '" class="OBJ">{\u00A0}</span>';
	}


};

HTML = {

	toggleArea: function (captionNode) {

		var x,
			table = captionNode.parentNode,
			state = captionNode.firstChild.nodeValue.substring(1, 2);

		if (state === '-') {

			if (table.tHead) {
				table.tHead.style.display = 'none';
			}

			for (x = 0; x < table.tBodies.length; x += 1) {
				table.tBodies[x].style.display = 'none';
			}

			captionNode.firstChild.nodeValue =
				'[+]' + captionNode.firstChild.nodeValue.substring(3);

		} else {

			if (table.tHead) {
				table.tHead.style.display = '';
			}

			for (x = 0; x < table.tBodies.length; x += 1) {
				table.tBodies[x].style.display = '';
			}

			captionNode.firstChild.nodeValue =
				'[-]' + captionNode.firstChild.nodeValue.substring(3);

		}

	},

	/** Render a javascript object of unknown type as HTML
	  * @param  {object} obj
	  * @param  {number} path
	  * @return {string}
	  */
	display: function (obj, path) {

		return Lang.isArray(obj) ?
			VIS.data.dataTables.checked ?
			this.structure(obj, path) :
			this.array(obj, path) :

			Lang.isBoolean(obj) ?
			this.bool(obj, path) :

			Lang.isFunction(obj) ?
			this.func(obj, path) :
		
			Lang.isNull(obj) ?
			'<span id="p' + path + '" title="null" class="NULL">null</span>' :

			Lang.isNumber(obj) ?
			'<span id="p' + path + '" title="Number" class="NUMBER">' +
			obj.toString() + '</span>' :

			Lang.isString(obj) ?
			this.string(obj, path) :

			Lang.isUndefined(obj) ?
			'<span id="p' + path +
			'" title="undefined" class="UNDEF">undefined</span>' :

			(obj instanceof Error) ?
			this.err(obj, path) :

			(obj instanceof Date) ?
			this.date(obj, path) :

			(obj instanceof RegExp) ?
			this.regExp(obj, path) :

			Lang.isObject(obj) ?
			VIS.data.dataTables.checked ?
			this.structure(obj, path) :
			this.obj(obj, path) :


			isNaN(obj) ?
			'<span id="p' + path + '" title="NaN" class="ERR">NaN</span>' :

			(obj === Infinity) ?
			'<span id="p' + path +
			'" title="Infinity" class="ERR">Infinity</span>' :

			'<span id="p' + path + '" class="IDK">[Unknown Data Type]</span>';
	},


	/** Render a javascript array as HTML
	  * @param  {array}  a     The array to be rendered
	  * @param  {number} path  The path by which this array is reached.
	  * @return {string}
	  */
	array: function (a, path) {
		var x, body = '';

		for (x = 0; x < a.length; x++) {
			VIS.paths.push(
				VIS.paths[path] + '[<span class="NUMBER">' + x + '<\/span>]'
			);

			body +=
				'<tr id="p' + (VIS.paths.length - 1) + '"><th>' +
				x + '</th><td>' +
				this.display(a[x], VIS.paths.length - 1) + '</td></tr>';
		}

		return (body.length) ?
			'<table id="p' + path + '" class="ARRAY">' +
			'<caption>[-] Array, ' + VIS.formatSize(a.length) +
			(a.length === 1 ? ' item' : ' items') +
			'</caption><tbody>' + body + '</tbody></table>' :

			'<span id="p' + path +
			'" title="Array" class="ARRAY">[ Empty Array ]</span>';
	},

	/** Render a javascript boolean value as HTML
	  * @param  {boolean} b
	  * @param  {number}  path
	  * @return {string}
	  */
	bool: function (b, path) {
		return (b) ?
			'<span id="p' + path +
			'" title="Boolean" class="BOOL">true</span>' :

			'<span id="p' + path +
			'" title="Boolean" class="BOOL">false</span>';
	},

	/** Render a javascript string as HTML
	  * @param  {string} s
	  * @param  {number} path
	  * @return {string}
	  */
	string: function (s, path) {
		if (s.length === 0) {
			return '<span id="p' + path +
				'" title="String" class="EMPTY">[zero-length string]</span>';
		}

		// Check and see if this is secretly a date
		if (VIS.data.dates.checked && VIS.dateRE.test(s)) {
			return this.date(Lang.JSON.stringToDate(s), path);
		}

		var tag = VIS.data.preserve.checked ? 'pre' : 'span';

		if (VIS.data.trunc.checked && s.length > 70) {
			s = s.substring(0, 70) + '\u2026'; // 2026 = "..."
		}

		return '<' + tag + ' id="p' + path +
			'" title="String" class="STRING">' + VIS.html(s) +
			'</' + tag + '>';
	},

	/** Render a javascript regular expression as HTML
	  * @param  {RegExp} re
	  * @param  {number} path
	  * @return {string}
	  */
	regExp: function (re, path) {
		var output = "/" + VIS.html(re.source) + "/";
		if (re.global) {
			output += 'g';
		}
		if (re.ignoreCase) {
			output += 'i';
		}
		if (re.multiline) {
			output += 'm';
		}
		return '<span id="p' + path +
			'" title="RegEx" class="REGEXP">' + output + '</span>';
	},

	/** Render a javascript object as HTML
	  * @param  {object} o
	  * @param  {number} path
	  * @return {string}
	  */
	obj: function (o, path) {
		var x, body = [];

		for (x in o) {
			if (o.hasOwnProperty(x)) {
				VIS.paths.push(VIS.paths[path] + VIS.variable(x));

				body.push(
					'<tr id="px' + (VIS.paths.length - 1) + '"><th>' +
						VIS.html(x) + '</th><td>' +
						this.display(o[x], VIS.paths.length - 1) +
						'</td></tr>'
				);
			}
		}

		return (body.length) ?
			'<table id="p' + path + '" class="OBJ">' +
			'<caption>[-] Object, ' + VIS.formatSize(body.length) +
			(body.length === 1 ? ' property' : ' properties') +
			'</caption><tbody>' + body.join('') + '</tbody></table>' :

			'<span id="p' + path +
			'" title="Object" class="OBJ">{ Empty Object }</span>';
	},

	/** Render a javascript date object as HTML
	  * @param  {Date} d
	  * @param  {number} path
	  * @return {string}
	  */
	date: function (d, path) {
		if (isNaN(d)) {
			return '<span id="p' + path +
				'" title="Date" class="ERR">Invalid Date</span>';
		}

		function pad(num) {
			var s = num.toString();
			return (num < 10) ? '0' + s : s;
		}

		function format(yyyy, mm, dd, hh, nn, ss) {
			var hh12 = (hh === 0) ? 12 : (hh > 12) ? hh - 12 : hh,
				tt = (hh > 11) ? 'PM' : 'AM';

			return (
				yyyy + '-' +
				pad(mm) + '-' +
				pad(dd) + ' ' +
				pad(hh12) + ':' +
				pad(nn) + ':' +
				pad(ss) + ' ' + tt
			);
		}

		var local = format(
			d.getFullYear(),
			d.getMonth() + 1,
			d.getDate(),
			d.getHours(),
			d.getMinutes(),
			d.getSeconds()
		),

			utc = format(
				d.getUTCFullYear(),
				d.getUTCMonth() + 1,
				d.getUTCDate(),
				d.getUTCHours(),
				d.getUTCMinutes(),
				d.getUTCSeconds()
			),

			output = utc + ' UTC (' + local + ' Local)';

		return '<span id="p' + path + '" title="Date" class="DATE">' +
			output + '</span>';
	},

	/** Render a javascript error as HTML
	  * @param  {Error}  e
	  * @param  {number} path
	  * @return {string}
	  */
	err: function (e, path) {
		if (e.message === 'parseJSON') {
			return '<span id="p' + path +
				'" title="Error" class="ERR">Invalid JSON</span>';
		}

		return '<span id="p' + path + '" title="Error" class="ERR">' +
			VIS.html(e.message) + '</span>';
	},

	/** Render a javascript function as HTML
	  * @param  {function} f
	  * @param  {number}   path
	  * @return {string}
	  */
	func: function (f, path) {
		var i, s = f.toString();
		if (VIS.data.trunc.checked) {
			i = s.indexOf('{') + 50;
			if (i < s.length) {
				s = VIS.html(s.substring(0, i)) + '\u2026<br \/>}';
				return '<code id="p' + path +
					'" title="Function (truncated)" class="FUNC">' + s +
					'<\/code>';
			}
		}
		return '<code id="p' + path + '" title="Function" class="FUNC">' +
			VIS.html(s) + '<\/code>';
	},

	/** Detect a data table.  I define this as an object (or array) containing
	  * two or more items, in which at least 66% of the items are similar to
	  * one another. Two objects are similar if they have the same number of
	  * key values, and all of the key values of one object are found in the
	  * other object.
	  * <p>
	  * We are going to loop through the properties of the object (or items of
	  * the array), and for each object that we find, we are going to collect
	  * a list of its keys. Each list will be like a footprint.  We will keep
	  * a list of all distinct footprints we find.  Each time we generate a
	  * new footprint, we compare it to the others in the list.  If we have
	  * seen it before, we make a note of how many times we see it.  If we
	  * have not seen it before, we add it to our list of footprints. 
	  * When this is done, we will be able to see how many different footprints
	  * we have, and whether any one footprint occurs at least 66% of the time.
	  * <p>
	  * Items that are better represented as other values (dates, numbers,
	  * strings, booleans, etc) are counted towards the total length of the
	  * structure, but they do not contribute footprints.
	  * <p>
	  * After we decide which footprint is dominant, we begin generating a
	  * tabular structure.  We use the list of keys as the column names (plus
	  * one for the row number), then loop over each item in the object.  If
	  * that item's footprint matches, it is rendered into the tabular
	  * structure.  If it doesn't match, it gets a colspan, and is rendered
	  * normally.
	  * <p>
	  * If it is determined that this is NOT a valid structure, rendering
	  * will proceed with either VIS.array or VIS.object, as appropriate.
	  *
	  * @param  {object | array}  obj
	  * @param  {number}          path
	  * @return {string}
	  */
	structure: function (obj, path) {
		var structure = new STRUCTURE(obj);

		if (structure.isValid(2 / 3) === false) {
			return Lang.isArray(obj) ?
				this.array(obj, path) :
				this.obj(obj, path);
		}

		return structure.render(obj, path);
	}

};



/** Provides a few necessary methods for detecting and rendering a generic
  * data structure.
  * @constructor
  * @param {object | array} o
  */
STRUCTURE = function (o) {
	this.footPrints = [];
	this.length = 0;

	// Determine the mode of all subobject property sets.  This also updates
	// the length property.
	this.footPrint = this.scanObject(o);
};


/** A single foot print.  Used to help determine which set of properties
  * occurs the most often in a set of sets.
  *
  * @constructor
  * @param {array} keys
  */
STRUCTURE.Footprint = function (keys) {
	this.keys = keys.slice();
	this.count = 1;
};


STRUCTURE.Footprint.prototype = {

	/** Determine if this footprint is equal to another set of keys.
	  * @param  {array} keys
	  * @return {boolean}
	  */
	equals: function (keys) {
		var x;
		if (this.keys.length === keys.length) {
			for (x = 0; x < keys.length; x++) {
				if (this.keys[x] !== keys[x]) {
					return false;
				}
			}
		} else {
			return false;
		}
		return true;
	},

	/** Render this footprint as a set of HTML table cells.
	  * @return {string}
	  */
	render: function (row, path) {
		var keys   = this.keys,
			x      = 0,
			k      = keys.length,
			output = [];

		if (row) {
			for (; x < k; x++) {
				VIS.paths.push(VIS.paths[path] + VIS.variable(keys[x]));

				output[x] =
					'<td id="px' + (VIS.paths.length - 1) + '">' +
					HTML.display(row[keys[x]], VIS.paths.length - 1) +
					'<\/td>';
			}
		} else {
			for (; x < k; x++) {
				output[x] = '<th>' + VIS.html(keys[x]) + '<\/th>';
			}
		}

		return output.join('');
	}
};



/** Determine whether a given object is valid for inclusion as a row in a
  * datatable structure.
  * @param  {object} o
  * @return {boolean}
  */
STRUCTURE.isObject = function (o) {
	return (
		Lang.isObject(o)    === true  &&
		Lang.isArray(o)     === false &&
		Lang.isFunction(o)  === false &&
		o instanceof Error  === false &&
		o instanceof Date   === false &&
		o instanceof RegExp === false
	);
};


/** Sort an array numerically */
STRUCTURE.numericSort = function (a, b) {
	return a - b;
};


/** Extract a sorted list of keys from the given object.
  * @param  {object} o
  * @return a
  */
STRUCTURE.getKeys = function (o) {
	var a = [], y;

	for (y in o) {
		if (o.hasOwnProperty(y)) {
			a[a.length] = y;
		}
	}

	if (Lang.isArray(o)) {
		a.sort(STRUCTURE.numericSort);
	} else {
		a.sort();
	}

	return a;
};


STRUCTURE.prototype = {

	/** Scan an object for inclusion in this data structure.
	  *
	  * Updates the length property of this structure.
	  *
	  * @param  {object} o
	  * @return {array}
	  *
	  */
	scanObject: function (o) {
		var x, keys, length = 0;
		for (x in o) {
			if (o.hasOwnProperty(x)) {
				length++;
				if (STRUCTURE.isObject(o[x])) {
					keys = STRUCTURE.getKeys(o[x]);
					if (keys.length > 0) {
						this.addFootPrint(keys);
					}
				}
			}
		}

		this.length += length;
		return this.getMode();
	},


	/** Add a single footprint to our collection of footprints.
	  * @param {array} keys
	  */
	addFootPrint: function (keys) {
		var x, footPrints = this.footPrints;

		for (x = 0; x < footPrints.length; x++) {
			if (footPrints[x].equals(keys)) {
				footPrints[x].count++;
				return true;
			}
		}

		footPrints[footPrints.length] = new STRUCTURE.Footprint(keys);
		return true;
	},

	/** Return the mode of the footprints (i.e., the one that occurred the
	  * most often).
	  *
	  * @return {STRUCTURE.FootPrint}
	  */
	getMode: function () {
		var x,
			count      = 0,
			max        = null,
			footPrints = this.footPrints;

		for (x = 0; x < footPrints.length; x++) {
			if (footPrints[x].count > count) {
				max = footPrints[x];
				count = max.count;
			}
		}
		return max;
	},

	/** This is only valid as a structure if at least two items have been
	  * scanned, and if the the mode of all subobject property sets occurred
	  * at least X% of the time.
	  *
	  * @param  {number}  threshold
	  * @return {boolean}
	  */
	isValid: function (threshold) {
		return (
			this.length > 1 &&
			this.footPrint !== null &&
			this.footPrint.count >= (this.length * threshold)
		);
	},


	/** Render the given object using this data structure as a framework.
	  * @param {object | array} obj
	  * @param {number}         path
	  * @return {string}
	  */
	render: function (obj, path) {
		var x, keys, row, subPath,
			html       = VIS.html,
			properties = STRUCTURE.getKeys(obj),
			span       = this.footPrint.keys.length,
			o          = [],
			p          = 0,
			isArray    = Lang.isArray(obj);

		o[p++] =
			'<table id="p' + path + '" class="ARRAY">' +
			'<caption>[-] ' +
			(isArray ? 'Array' : 'Object') +
			' data structure, ' + VIS.formatSize(properties.length) +
			(isArray ? ' items' : ' properties') +
			'</caption><thead><tr><th>[key]</th>';

		o[p++] = this.footPrint.render();
		o[p++] = '</tr></thead><tbody>';

		path = VIS.paths[path];

		for (x = 0; x < properties.length; x++) {
			row = obj[properties[x]];
			keys = STRUCTURE.getKeys(row);

			subPath = 
				(isArray && isNaN(parseInt(properties[x], 10)) === false) ?
				'[<span class="NUMBER">' + properties[x] + '</span>]' :
				VIS.variable(properties[x]);

			VIS.paths.push(path + subPath);

			if (this.footPrint.equals(keys)) {
				o[p++] =
					'<tr id="p' + (VIS.paths.length - 1) + '"><th' +
					(isArray ? ' class="NUMBER">' : '>') +
					html(properties[x]) + '</th>';

				o[p++] = this.footPrint.render(row, VIS.paths.length - 1);
				o[p++] = '</tr>';

			} else {
				o[p++] =
					'<tr id="p' + (VIS.paths.length - 1) + '"><th><em' +
					(isArray ? ' class="NUMBER">' : '>') +
					html(properties[x]) + '</em></th><td colspan="' +
					span + '">';

				o[p++] = HTML.display(row, VIS.paths.length - 1);
				o[p++] = '</td></tr>';
			}
		}

		o[p++] = '</tbody></table>';
		return o.join('');
	}


};


COOKIE = {
	set: function (name, value) {
		var expires = new Date(),
			cookie =
				encodeURIComponent(name) + '=' +
				encodeURIComponent(value);

		expires.setFullYear(expires.getFullYear() + 1);
		expires = expires.toGMTString();
		cookie += '; expires=' + expires + '; path=/';

		document.cookie = cookie;
		return cookie;
	},
	get: function (name) {
		var cookie,
			cookies = document.cookie.split(/;\s*/),
			x = cookies.length;

		while (x--) {
			cookie = cookies[x];
			if (cookie.indexOf(name) === 0) {
				// +1 is for the equal sign
				return cookie.substring(name.length + 1);
			}
		}
		return null;
	},
	del: function (name) {
		var expires = new Date(),
			cookie = encodeURIComponent(name) + '=';

		expires.setDate(expires.getDate() - 1);
		expires = expires.toGMTString();

		cookie += '; expires=' + expires + '; path=/';
		document.cookie = cookie;
		return cookie;
	}
};

Event.onDOMReady(VIS.init, VIS, true);

(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $author$project$Editor$Bottom = {$: 'Bottom'};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = {$: 'GT'};
var $author$project$Structure$PInt = function (a) {
	return {$: 'PInt', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $author$project$Structure$Node = function (a) {
	return {$: 'Node', a: a};
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$Structure$addProperty = F2(
	function (_v0, _v1) {
		var role = _v0.a.a;
		var value = _v0.b;
		var data = _v1.a;
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{
					properties: A3($elm$core$Dict$insert, role, value, data.properties)
				}));
	});
var $author$project$Structure$addInt = F3(
	function (role, value, node) {
		return A2(
			$author$project$Structure$addProperty,
			_Utils_Tuple2(
				role,
				$author$project$Structure$PInt(value)),
			node);
	});
var $author$project$Structure$Role = function (a) {
	return {$: 'Role', a: a};
};
var $author$project$Structure$roleFromString = function (key) {
	return $author$project$Structure$Role(key);
};
var $author$project$Editor$roleMarginBottom = $author$project$Structure$roleFromString('margin-bottom');
var $author$project$Editor$roleMarginLeft = $author$project$Structure$roleFromString('margin-left');
var $author$project$Editor$roleMarginRight = $author$project$Structure$roleFromString('margin-right');
var $author$project$Editor$roleMarginTop = $author$project$Structure$roleFromString('margin-top');
var $author$project$Editor$addMargin = F3(
	function (side, space, node) {
		var key = function () {
			switch (side.$) {
				case 'Top':
					return $author$project$Editor$roleMarginTop;
				case 'Right':
					return $author$project$Editor$roleMarginRight;
				case 'Bottom':
					return $author$project$Editor$roleMarginBottom;
				default:
					return $author$project$Editor$roleMarginLeft;
			}
		}();
		return A3($author$project$Structure$addInt, key, space, node);
	});
var $author$project$Editor$ConstantCell = {$: 'ConstantCell'};
var $author$project$Editor$ContentCell = function (a) {
	return {$: 'ContentCell', a: a};
};
var $author$project$Structure$PString = function (a) {
	return {$: 'PString', a: a};
};
var $author$project$Structure$addText = F3(
	function (role, text, node) {
		return A2(
			$author$project$Structure$addProperty,
			_Utils_Tuple2(
				role,
				$author$project$Structure$PString(text)),
			node);
	});
var $author$project$Structure$Path = function (a) {
	return {$: 'Path', a: a};
};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Structure$emptyFeatures = {custom: $elm$core$Dict$empty, _default: _List_Nil};
var $author$project$Structure$createNodeInternal = F2(
	function (role, isa) {
		return $author$project$Structure$Node(
			{
				features: $author$project$Structure$emptyFeatures,
				isa: isa,
				path: $author$project$Structure$Path(
					_List_fromArray(
						[
							{index: 0, role: role}
						])),
				properties: $elm$core$Dict$empty
			});
	});
var $author$project$Structure$roleEmpty = $author$project$Structure$roleFromString('');
var $author$project$Structure$createNode = function (isa) {
	return A2($author$project$Structure$createNodeInternal, $author$project$Structure$roleEmpty, isa);
};
var $author$project$Editor$roleConstant = $author$project$Structure$roleFromString('constant');
var $author$project$Editor$constantCell = function (constantValue) {
	return A3(
		$author$project$Structure$addText,
		$author$project$Editor$roleConstant,
		constantValue,
		$author$project$Structure$createNode(
			$author$project$Editor$ContentCell($author$project$Editor$ConstantCell)));
};
var $author$project$Editor$StackCell = {$: 'StackCell'};
var $elm$core$Basics$True = {$: 'True'};
var $author$project$Structure$PBool = function (a) {
	return {$: 'PBool', a: a};
};
var $author$project$Structure$addBool = F3(
	function (role, value, node) {
		return A2(
			$author$project$Structure$addProperty,
			_Utils_Tuple2(
				role,
				$author$project$Structure$PBool(value)),
			node);
	});
var $author$project$Editor$roleIsHoriz = $author$project$Structure$roleFromString('isHoriz');
var $author$project$Editor$horizStackCell = A3(
	$author$project$Structure$addBool,
	$author$project$Editor$roleIsHoriz,
	true,
	$author$project$Structure$createNode(
		$author$project$Editor$ContentCell($author$project$Editor$StackCell)));
var $author$project$Editor$InputCell = {$: 'InputCell'};
var $author$project$Editor$InputEffect = function (a) {
	return {$: 'InputEffect', a: a};
};
var $author$project$Editor$InputEffectData = F2(
	function (path, role) {
		return {path: path, role: role};
	});
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $author$project$Editor$inputEffect = F2(
	function (path, role) {
		return $author$project$Editor$InputEffect(
			A2($author$project$Editor$InputEffectData, path, role));
	});
var $author$project$Structure$pathOf = function (_v0) {
	var path = _v0.a.path;
	return path;
};
var $author$project$Editor$roleInput = $author$project$Structure$roleFromString('input');
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $author$project$Structure$valueOf = F2(
	function (_v0, _v1) {
		var key = _v0.a;
		var properties = _v1.a.properties;
		return A2($elm$core$Dict$get, key, properties);
	});
var $author$project$Structure$tryTextOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$andThen,
			function (prop) {
				if (prop.$ === 'PString') {
					var v = prop.a;
					return $elm$core$Maybe$Just(v);
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Structure$valueOf, role, node));
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Structure$textOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$withDefault,
			'',
			A2($author$project$Structure$tryTextOf, role, node));
	});
var $author$project$Editor$EffectCell = function (a) {
	return {$: 'EffectCell', a: a};
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $author$project$Structure$appendTo = F2(
	function (child, list) {
		return $elm$core$List$reverse(
			A2(
				$elm$core$List$cons,
				child,
				$elm$core$List$reverse(list)));
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$Structure$updateCustomFeature = F4(
	function (_v0, child, appender, custom) {
		var key = _v0.a;
		var updater = function (mbChildren) {
			return $elm$core$Maybe$Just(
				function () {
					if (mbChildren.$ === 'Nothing') {
						return _List_fromArray(
							[child]);
					} else {
						var children = mbChildren.a;
						return A2(appender, child, children);
					}
				}());
		};
		return A3($elm$core$Dict$update, key, updater, custom);
	});
var $author$project$Structure$addToCustom = F3(
	function (role, child, _v0) {
		var data = _v0.a;
		var features = data.features;
		var featuresNew = _Utils_update(
			features,
			{
				custom: A4($author$project$Structure$updateCustomFeature, role, child, $author$project$Structure$appendTo, features.custom)
			});
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{features: featuresNew}));
	});
var $author$project$Editor$roleEffects = $author$project$Structure$roleFromString('effects');
var $author$project$Editor$withEffect = function (effect) {
	return A2(
		$author$project$Structure$addToCustom,
		$author$project$Editor$roleEffects,
		$author$project$Structure$createNode(
			$author$project$Editor$EffectCell(effect)));
};
var $author$project$Editor$inputCell = F2(
	function (role, nodeContext) {
		return A2(
			$author$project$Editor$withEffect,
			A2(
				$author$project$Editor$inputEffect,
				$author$project$Structure$pathOf(nodeContext),
				role),
			A3(
				$author$project$Structure$addText,
				$author$project$Editor$roleInput,
				A2($author$project$Structure$textOf, role, nodeContext),
				$author$project$Structure$createNode(
					$author$project$Editor$ContentCell($author$project$Editor$InputCell))));
	});
var $author$project$Structure$roleName = $author$project$Structure$roleFromString('name');
var $author$project$Structure$addToDefault = F2(
	function (child, _v0) {
		var data = _v0.a;
		var features = data.features;
		var featuresNew = function () {
			var _v1 = features._default;
			if (!_v1.b) {
				return _Utils_update(
					features,
					{
						_default: _List_fromArray(
							[child])
					});
			} else {
				var children = _v1;
				return _Utils_update(
					features,
					{
						_default: A2($author$project$Structure$appendTo, child, children)
					});
			}
		}();
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{features: featuresNew}));
	});
var $author$project$Editor$with = function (node) {
	return $author$project$Structure$addToDefault(node);
};
var $author$project$Tanks$editorFactionName = function (faction) {
	return A3(
		$author$project$Editor$addMargin,
		$author$project$Editor$Bottom,
		20,
		A2(
			$author$project$Editor$with,
			A2($author$project$Editor$inputCell, $author$project$Structure$roleName, faction),
			A2(
				$author$project$Editor$with,
				$author$project$Editor$constantCell('Faction:'),
				$author$project$Editor$horizStackCell)));
};
var $author$project$Tanks$Tank = {$: 'Tank'};
var $author$project$Tanks$roleFire = $author$project$Structure$roleFromString('fire');
var $author$project$Tanks$editorTankFireChance = function (tank) {
	return A2(
		$author$project$Editor$with,
		$author$project$Editor$constantCell('%'),
		A2(
			$author$project$Editor$with,
			A2($author$project$Editor$inputCell, $author$project$Tanks$roleFire, tank),
			A2(
				$author$project$Editor$with,
				$author$project$Editor$constantCell('Chance of fire:'),
				$author$project$Editor$horizStackCell)));
};
var $author$project$Tanks$roleKind = $author$project$Structure$roleFromString('kind');
var $author$project$Tanks$editorTankKind = function (tank) {
	return A2(
		$author$project$Editor$with,
		A2($author$project$Editor$inputCell, $author$project$Tanks$roleKind, tank),
		A2(
			$author$project$Editor$with,
			$author$project$Editor$constantCell('Kind:'),
			$author$project$Editor$horizStackCell));
};
var $author$project$Tanks$roleSignalRange = $author$project$Structure$roleFromString('signalRange');
var $author$project$Tanks$editorTankSignalRange = function (tank) {
	return A2(
		$author$project$Editor$with,
		$author$project$Editor$constantCell('m'),
		A2(
			$author$project$Editor$with,
			A2($author$project$Editor$inputCell, $author$project$Tanks$roleSignalRange, tank),
			A2(
				$author$project$Editor$with,
				$author$project$Editor$constantCell('Signale range:'),
				$author$project$Editor$horizStackCell)));
};
var $author$project$Tanks$roleViewRange = $author$project$Structure$roleFromString('viewRange');
var $author$project$Tanks$editorTankViewRange = function (tank) {
	return A2(
		$author$project$Editor$with,
		$author$project$Editor$constantCell('m'),
		A2(
			$author$project$Editor$with,
			A2($author$project$Editor$inputCell, $author$project$Tanks$roleViewRange, tank),
			A2(
				$author$project$Editor$with,
				$author$project$Editor$constantCell('View range:'),
				$author$project$Editor$horizStackCell)));
};
var $elm$core$Basics$False = {$: 'False'};
var $author$project$Editor$InsertionEffect = function (a) {
	return {$: 'InsertionEffect', a: a};
};
var $author$project$Editor$InsertionEffectData = F4(
	function (path, nodeToInsert, isReplace, role) {
		return {isReplace: isReplace, nodeToInsert: nodeToInsert, path: path, role: role};
	});
var $author$project$Editor$insertionEffect = F2(
	function (nodeContext, nodeToInsert) {
		return $author$project$Editor$InsertionEffect(
			A4(
				$author$project$Editor$InsertionEffectData,
				$author$project$Structure$pathOf(nodeContext),
				nodeToInsert,
				false,
				$author$project$Structure$roleEmpty));
	});
var $author$project$Tanks$editorTank = F2(
	function (tank, container) {
		return A2(
			$author$project$Editor$with,
			$author$project$Tanks$editorTankSignalRange(tank),
			A2(
				$author$project$Editor$with,
				$author$project$Tanks$editorTankViewRange(tank),
				A2(
					$author$project$Editor$with,
					$author$project$Tanks$editorTankFireChance(tank),
					A2(
						$author$project$Editor$with,
						$author$project$Tanks$editorTankKind(tank),
						A2(
							$author$project$Editor$with,
							A2(
								$author$project$Editor$withEffect,
								A2(
									$author$project$Editor$insertionEffect,
									tank,
									$author$project$Structure$createNode($author$project$Tanks$Tank)),
								A2($author$project$Editor$inputCell, $author$project$Structure$roleName, tank)),
							container)))));
	});
var $author$project$Tanks$ctorTank = $author$project$Structure$createNode($author$project$Tanks$Tank);
var $author$project$Editor$PlaceholderCell = {$: 'PlaceholderCell'};
var $author$project$Editor$rolePlaceholder = $author$project$Structure$roleFromString('placeholder');
var $author$project$Editor$placeholderCell = function (text) {
	return A3(
		$author$project$Structure$addText,
		$author$project$Editor$rolePlaceholder,
		text,
		$author$project$Structure$createNode(
			$author$project$Editor$ContentCell($author$project$Editor$PlaceholderCell)));
};
var $author$project$Editor$replacementEffect = F3(
	function (role, nodeContext, nodeToInsert) {
		return $author$project$Editor$InsertionEffect(
			A4(
				$author$project$Editor$InsertionEffectData,
				$author$project$Structure$pathOf(nodeContext),
				nodeToInsert,
				true,
				role));
	});
var $author$project$Structure$roleDefault = $author$project$Structure$Role('default');
var $author$project$Tanks$editorTanksPlaceholder = function (faction) {
	return A2(
		$author$project$Editor$withEffect,
		A3($author$project$Editor$replacementEffect, $author$project$Structure$roleDefault, faction, $author$project$Tanks$ctorTank),
		$author$project$Editor$placeholderCell('no tanks'));
};
var $author$project$Structure$getUnderDefault = function (_v0) {
	var features = _v0.a.features;
	return features._default;
};
var $author$project$Editor$roleIsGrid = $author$project$Structure$roleFromString('isGrid');
var $author$project$Editor$vertStackCell = A3(
	$author$project$Structure$addBool,
	$author$project$Editor$roleIsHoriz,
	false,
	$author$project$Structure$createNode(
		$author$project$Editor$ContentCell($author$project$Editor$StackCell)));
var $author$project$Editor$vertGridCell = A3($author$project$Structure$addBool, $author$project$Editor$roleIsGrid, true, $author$project$Editor$vertStackCell);
var $author$project$Tanks$editorTanks = function (faction) {
	var _v0 = $author$project$Structure$getUnderDefault(faction);
	if (!_v0.b) {
		return $author$project$Tanks$editorTanksPlaceholder(faction);
	} else {
		var tanks = _v0;
		return A3($elm$core$List$foldl, $author$project$Tanks$editorTank, $author$project$Editor$vertGridCell, tanks);
	}
};
var $author$project$Editor$RootCell = {$: 'RootCell'};
var $author$project$Structure$roleRoot = $author$project$Structure$roleFromString('root');
var $author$project$Structure$createRoot = function (isa) {
	return A2($author$project$Structure$createNodeInternal, $author$project$Structure$roleRoot, isa);
};
var $author$project$Editor$rootCell = $author$project$Structure$createRoot(
	$author$project$Editor$ContentCell($author$project$Editor$RootCell));
var $author$project$Tanks$editorFaction = function (faction) {
	return A2(
		$author$project$Editor$with,
		$author$project$Tanks$editorTanks(faction),
		A2(
			$author$project$Editor$with,
			$author$project$Tanks$editorFactionName(faction),
			$author$project$Editor$rootCell));
};
var $author$project$Tanks$Faction = {$: 'Faction'};
var $author$project$Tanks$initFaction = $author$project$Structure$createRoot($author$project$Tanks$Faction);
var $author$project$Runtime$Domain = F2(
	function (root, xform) {
		return {root: root, xform: xform};
	});
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $ianmackenzie$elm_geometry$Geometry$Types$Point2d = function (a) {
	return {$: 'Point2d', a: a};
};
var $ianmackenzie$elm_geometry$Point2d$fromCoordinates = $ianmackenzie$elm_geometry$Geometry$Types$Point2d;
var $ianmackenzie$elm_geometry$Point2d$origin = $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
	_Utils_Tuple2(0, 0));
var $author$project$Editor$initEditorModel = F2(
	function (dRoot, eRoot) {
		return {dRoot: dRoot, drag: $elm$core$Maybe$Nothing, eRoot: eRoot, mbSimulation: $elm$core$Maybe$Nothing, mousePos: $ianmackenzie$elm_geometry$Point2d$origin, runSimulation: true, runXform: true};
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Structure$tryBoolOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$andThen,
			function (prop) {
				if (prop.$ === 'PBool') {
					var v = prop.a;
					return $elm$core$Maybe$Just(v);
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Structure$valueOf, role, node));
	});
var $author$project$Structure$boolOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$withDefault,
			false,
			A2($author$project$Structure$tryBoolOf, role, node));
	});
var $author$project$Structure$replaceUnderFeature = F3(
	function (role, childrenNew, _v0) {
		var data = _v0.a;
		var features = data.features;
		var _v1 = role;
		var key = _v1.a;
		var featuresNew = _Utils_eq(role, $author$project$Structure$roleDefault) ? _Utils_update(
			features,
			{_default: childrenNew}) : _Utils_update(
			features,
			{
				custom: A3($elm$core$Dict$insert, key, childrenNew, features.custom)
			});
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{features: featuresNew}));
	});
var $author$project$Editor$roleDefault = $author$project$Structure$roleFromString('default');
var $author$project$Editor$griddifyI = F2(
	function (isGridParent, node) {
		var nodeNew = isGridParent ? A3($author$project$Structure$addBool, $author$project$Editor$roleIsGrid, true, node) : node;
		var isGrid = A2($author$project$Structure$boolOf, $author$project$Editor$roleIsGrid, nodeNew);
		var children = $author$project$Structure$getUnderDefault(nodeNew);
		return A3(
			$author$project$Structure$replaceUnderFeature,
			$author$project$Editor$roleDefault,
			A2(
				$elm$core$List$map,
				$author$project$Editor$griddifyI(isGrid),
				children),
			nodeNew);
	});
var $author$project$Editor$griddify = $author$project$Editor$griddifyI(false);
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$Structure$addFeaturePath = F2(
	function (parentPath, _v0) {
		var _default = _v0._default;
		var custom = _v0.custom;
		var addPath = F4(
			function (_v2, feature, index, _v3) {
				var parentSegments = _v2.a;
				var data = _v3.a;
				var segmentNew = {
					index: index,
					role: $author$project$Structure$Role(feature)
				};
				var pathNew = $author$project$Structure$Path(
					A2($author$project$Structure$appendTo, segmentNew, parentSegments));
				return $author$project$Structure$Node(
					_Utils_update(
						data,
						{
							features: A2($author$project$Structure$addFeaturePath, pathNew, data.features),
							path: pathNew
						}));
			});
		var indexUpdater = function (feature) {
			return $elm$core$List$indexedMap(
				A2(addPath, parentPath, feature));
		};
		var customNew = A2($elm$core$Dict$map, indexUpdater, custom);
		var _v1 = $author$project$Structure$roleDefault;
		var strDefault = _v1.a;
		var defaultNew = A2(indexUpdater, strDefault, _default);
		return {custom: customNew, _default: defaultNew};
	});
var $author$project$Structure$updatePaths = function (_v0) {
	var data = _v0.a;
	return $author$project$Structure$Node(
		_Utils_update(
			data,
			{
				features: A2($author$project$Structure$addFeaturePath, data.path, data.features)
			}));
};
var $author$project$Runtime$runDomainXform = function (domain) {
	return $author$project$Structure$updatePaths(
		$author$project$Editor$griddify(
			domain.xform(domain.root)));
};
var $author$project$Runtime$EditorMsg = function (a) {
	return {$: 'EditorMsg', a: a};
};
var $author$project$Editor$MouseMove = function (a) {
	return {$: 'MouseMove', a: a};
};
var $author$project$Editor$MouseUp = function (a) {
	return {$: 'MouseUp', a: a};
};
var $author$project$Editor$Tick = {$: 'Tick'};
var $author$project$Runtime$Tick = F2(
	function (a, b) {
		return {$: 'Tick', a: a, b: b};
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $gampleman$elm_visualization$Force$isCompleted = function (_v0) {
	var alpha = _v0.a.alpha;
	var minAlpha = _v0.a.minAlpha;
	return _Utils_cmp(alpha, minAlpha) < 1;
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Editor$mousePosition = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (x, y) {
			return $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
				_Utils_Tuple2(x, y));
		}),
	A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float));
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onMouseMove = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousemove');
var $elm$browser$Browser$Events$onMouseUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mouseup');
var $author$project$Runtime$subscriptions = function (model) {
	var tickSub = $elm$browser$Browser$Events$onAnimationFrame(
		$author$project$Runtime$Tick($author$project$Editor$Tick));
	var mouseMoveSub = $elm$browser$Browser$Events$onMouseMove(
		A2(
			$elm$json$Json$Decode$map,
			$author$project$Runtime$EditorMsg,
			A2($elm$json$Json$Decode$map, $author$project$Editor$MouseMove, $author$project$Editor$mousePosition)));
	var graphSubs = function () {
		var _v0 = model.editorModel.drag;
		if (_v0.$ === 'Nothing') {
			var _v1 = model.editorModel.mbSimulation;
			if (_v1.$ === 'Just') {
				var simulation = _v1.a;
				return $gampleman$elm_visualization$Force$isCompleted(simulation) ? _List_Nil : _List_fromArray(
					[tickSub]);
			} else {
				return model.editorModel.runSimulation ? _List_fromArray(
					[tickSub]) : _List_Nil;
			}
		} else {
			return _List_fromArray(
				[
					$elm$browser$Browser$Events$onMouseUp(
					A2(
						$elm$json$Json$Decode$map,
						$author$project$Runtime$EditorMsg,
						A2($elm$json$Json$Decode$map, $author$project$Editor$MouseUp, $author$project$Editor$mousePosition))),
					tickSub
				]);
		}
	}();
	return $elm$core$Platform$Sub$batch(
		A2($elm$core$List$cons, mouseMoveSub, graphSubs));
};
var $author$project$Editor$EdgeCell = {$: 'EdgeCell'};
var $author$project$Editor$GraphCell = {$: 'GraphCell'};
var $author$project$Editor$VertexCell = {$: 'VertexCell'};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Structure$primitiveToString = function (p) {
	switch (p.$) {
		case 'PString':
			var v = p.a;
			return v;
		case 'PBool':
			var b = p.a;
			return b ? 'True' : 'False';
		case 'PInt':
			var i = p.a;
			return $elm$core$String$fromInt(i);
		default:
			var f = p.a;
			return $elm$core$String$fromFloat(f);
	}
};
var $author$project$Structure$propsOf = function (_v0) {
	var properties = _v0.a.properties;
	return properties;
};
var $author$project$Structure$compareProperties = F3(
	function (mbRoles, l, r) {
		var filterRoles = function (dict) {
			if (mbRoles.$ === 'Nothing') {
				return dict;
			} else {
				var roles = mbRoles.a;
				return A2(
					$elm$core$Dict$filter,
					F2(
						function (k, _v2) {
							return A2(
								$elm$core$List$any,
								function (_v3) {
									var key = _v3.a;
									return _Utils_eq(key, k);
								},
								roles);
						}),
					dict);
			}
		};
		var lProps = filterRoles(
			$author$project$Structure$propsOf(l));
		var rProps = filterRoles(
			$author$project$Structure$propsOf(r));
		return (!_Utils_eq(
			$elm$core$List$length(
				$elm$core$Dict$keys(lProps)),
			$elm$core$List$length(
				$elm$core$Dict$keys(rProps)))) ? false : A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, lValue, b) {
					if (!b) {
						return false;
					} else {
						var mbRValue = A2($elm$core$Dict$get, k, rProps);
						if (mbRValue.$ === 'Nothing') {
							return false;
						} else {
							var rValue = mbRValue.a;
							return (!_Utils_eq(rValue, lValue)) ? false : _Utils_eq(
								$author$project$Structure$primitiveToString(rValue),
								$author$project$Structure$primitiveToString(lValue));
						}
					}
				}),
			true,
			lProps);
	});
var $author$project$Structure$isaOf = function (_v0) {
	var isa = _v0.a.isa;
	return isa;
};
var $author$project$Structure$pathSegmentAsId = F2(
	function (segment, idPart) {
		var idPartWSeparator = (idPart === '') ? '' : (idPart + '-');
		var _v0 = segment.role;
		var feature = _v0.a;
		return _Utils_ap(
			idPartWSeparator,
			_Utils_ap(
				feature,
				$elm$core$String$fromInt(segment.index)));
	});
var $author$project$Structure$pathAsId = function (_v0) {
	var segments = _v0.a;
	return A3($elm$core$List$foldl, $author$project$Structure$pathSegmentAsId, '', segments);
};
var $author$project$Structure$pathAsIdFromNode = function (_v0) {
	var path = _v0.a.path;
	return $author$project$Structure$pathAsId(path);
};
var $author$project$Structure$flatNodeComparer = F3(
	function (mbRoles, l, r) {
		return _Utils_eq(
			$author$project$Structure$isaOf(l),
			$author$project$Structure$isaOf(r)) && (_Utils_eq(
			$author$project$Structure$pathAsIdFromNode(l),
			$author$project$Structure$pathAsIdFromNode(r)) && A3($author$project$Structure$compareProperties, mbRoles, l, r));
	});
var $author$project$Structure$flatNodeListComparer = F3(
	function (mbRoles, lNodes, rNodes) {
		return !$elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function (v) {
					return !v;
				},
				A3(
					$elm$core$List$map2,
					$author$project$Structure$flatNodeComparer(mbRoles),
					lNodes,
					rNodes)));
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Structure$getAllUnderCustoms = function (_v0) {
	var features = _v0.a.features;
	return $elm$core$List$concat(
		$elm$core$Dict$values(features.custom));
};
var $author$project$Structure$nodesOfRec = F3(
	function (isa, node, result) {
		var _v0 = _Utils_eq(
			$author$project$Structure$isaOf(node),
			isa);
		if (_v0) {
			return A2($elm$core$List$cons, node, result);
		} else {
			var allChildren = A2(
				$elm$core$List$append,
				$author$project$Structure$getAllUnderCustoms(node),
				$author$project$Structure$getUnderDefault(node));
			return A3(
				$elm$core$List$foldl,
				$author$project$Structure$nodesOfRec(isa),
				result,
				allChildren);
		}
	});
var $author$project$Structure$nodesOf = F2(
	function (isa, root) {
		return A3($author$project$Structure$nodesOfRec, isa, root, _List_Nil);
	});
var $author$project$Editor$roleText = $author$project$Structure$roleFromString('text');
var $author$project$Editor$dictNameToVertex = function (cellGraph) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (v, d) {
				return A3(
					$elm$core$Dict$insert,
					A2($author$project$Structure$textOf, $author$project$Editor$roleText, v),
					v,
					d);
			}),
		$elm$core$Dict$empty,
		A2(
			$author$project$Structure$nodesOf,
			$author$project$Editor$ContentCell($author$project$Editor$VertexCell),
			cellGraph));
};
var $author$project$Editor$roleFrom = $author$project$Structure$roleFromString('propFrom');
var $author$project$Editor$roleTo = $author$project$Structure$roleFromString('propTo');
var $author$project$Editor$fromToPairs = function (cellGraph) {
	var fromToLookup = function (edge) {
		var lookup = function (key) {
			return A2(
				$elm$core$Dict$get,
				key,
				$author$project$Editor$dictNameToVertex(cellGraph));
		};
		var _v3 = _Utils_Tuple2(
			A2($author$project$Structure$textOf, $author$project$Editor$roleFrom, edge),
			A2($author$project$Structure$textOf, $author$project$Editor$roleTo, edge));
		var from = _v3.a;
		var to = _v3.b;
		return _Utils_Tuple2(
			lookup(from),
			lookup(to));
	};
	var edges = A2(
		$author$project$Structure$nodesOf,
		$author$project$Editor$ContentCell($author$project$Editor$EdgeCell),
		cellGraph);
	return A2(
		$elm$core$List$filterMap,
		function (tuple) {
			if (tuple.a.$ === 'Nothing') {
				var _v1 = tuple.a;
				return $elm$core$Maybe$Nothing;
			} else {
				if (tuple.b.$ === 'Nothing') {
					var _v2 = tuple.b;
					return $elm$core$Maybe$Nothing;
				} else {
					var from = tuple.a.a;
					var to = tuple.b.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(from, to));
				}
			}
		},
		A2($elm$core$List$map, fromToLookup, edges));
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Editor$graphComparer = F2(
	function (lRoot, rRoot) {
		var mbRGraph = $elm$core$List$head(
			A2(
				$author$project$Structure$nodesOf,
				$author$project$Editor$ContentCell($author$project$Editor$GraphCell),
				rRoot));
		var mbLGraph = $elm$core$List$head(
			A2(
				$author$project$Structure$nodesOf,
				$author$project$Editor$ContentCell($author$project$Editor$GraphCell),
				lRoot));
		var _v0 = _Utils_Tuple2(mbLGraph, mbRGraph);
		if (_v0.a.$ === 'Nothing') {
			if (_v0.b.$ === 'Nothing') {
				var _v1 = _v0.a;
				var _v2 = _v0.b;
				return true;
			} else {
				var _v3 = _v0.a;
				return false;
			}
		} else {
			if (_v0.b.$ === 'Nothing') {
				var _v4 = _v0.b;
				return false;
			} else {
				var lGraph = _v0.a.a;
				var rGraph = _v0.b.a;
				var rVertices = A2(
					$elm$core$List$sortBy,
					function (v) {
						return $author$project$Structure$pathAsIdFromNode(v);
					},
					A2(
						$author$project$Structure$nodesOf,
						$author$project$Editor$ContentCell($author$project$Editor$VertexCell),
						rGraph));
				var rFromTo = $author$project$Editor$fromToPairs(rGraph);
				var rEdges = A2(
					$elm$core$List$sortBy,
					function (e) {
						return $author$project$Structure$pathAsIdFromNode(e);
					},
					A2(
						$author$project$Structure$nodesOf,
						$author$project$Editor$ContentCell($author$project$Editor$EdgeCell),
						rGraph));
				var lVertices = A2(
					$elm$core$List$sortBy,
					function (v) {
						return $author$project$Structure$pathAsIdFromNode(v);
					},
					A2(
						$author$project$Structure$nodesOf,
						$author$project$Editor$ContentCell($author$project$Editor$VertexCell),
						lGraph));
				var lFromTo = $author$project$Editor$fromToPairs(lGraph);
				var numOfRealEdgesIsEqual = _Utils_eq(
					$elm$core$List$length(lFromTo),
					$elm$core$List$length(rFromTo));
				var lEdges = A2(
					$elm$core$List$sortBy,
					function (e) {
						return $author$project$Structure$pathAsIdFromNode(e);
					},
					A2(
						$author$project$Structure$nodesOf,
						$author$project$Editor$ContentCell($author$project$Editor$EdgeCell),
						lGraph));
				var flatIsEqual = A3(
					$author$project$Structure$flatNodeListComparer,
					$elm$core$Maybe$Just(_List_Nil),
					lVertices,
					rVertices) && A3(
					$author$project$Structure$flatNodeListComparer,
					$elm$core$Maybe$Just(_List_Nil),
					lEdges,
					rEdges);
				return ((!_Utils_eq(
					$elm$core$List$length(lVertices),
					$elm$core$List$length(rVertices))) || (!_Utils_eq(
					$elm$core$List$length(lEdges),
					$elm$core$List$length(rEdges)))) ? false : ((!flatIsEqual) ? false : ((!numOfRealEdgesIsEqual) ? false : true));
			}
		}
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Structure$PFloat = function (a) {
	return {$: 'PFloat', a: a};
};
var $author$project$Structure$asPFloat = function (f) {
	return $author$project$Structure$PFloat(f);
};
var $author$project$Editor$roleX = $author$project$Structure$roleFromString('x');
var $author$project$Editor$roleY = $author$project$Structure$roleFromString('y');
var $author$project$Structure$tryFloatOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$andThen,
			function (prop) {
				if (prop.$ === 'PFloat') {
					var v = prop.a;
					return $elm$core$Maybe$Just(v);
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Structure$valueOf, role, node));
	});
var $author$project$Structure$dropRootSegment = function (path) {
	var segments = path.a;
	if (segments.b) {
		var role = segments.a.role;
		var tail = segments.b;
		return _Utils_eq(role, $author$project$Structure$roleRoot) ? $author$project$Structure$Path(tail) : path;
	} else {
		return path;
	}
};
var $author$project$Structure$getUnderCustom = F2(
	function (_v0, _v1) {
		var key = _v0.a;
		var features = _v1.a.features;
		return A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Dict$get, key, features.custom));
	});
var $author$project$Structure$getUnder = F2(
	function (role, node) {
		return _Utils_eq(role, $author$project$Structure$roleDefault) ? $author$project$Structure$getUnderDefault(node) : A2($author$project$Structure$getUnderCustom, role, node);
	});
var $author$project$Structure$updateProperty = F2(
	function (_v0, n) {
		var key = _v0.a.a;
		var primitiveNew = _v0.b;
		var _v1 = n;
		var data = _v1.a;
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{
					properties: A3($elm$core$Dict$insert, key, primitiveNew, data.properties)
				}));
	});
var $author$project$Structure$updateChildrenUnder = F4(
	function (segment, tailSegments, kvp, parent) {
		var updateAt = function (child) {
			var _v1 = $author$project$Structure$pathOf(child);
			var pathSegmentsChild = _v1.a;
			var mbLastSegment = $elm$core$List$head(
				$elm$core$List$reverse(pathSegmentsChild));
			if (mbLastSegment.$ === 'Nothing') {
				return child;
			} else {
				var lastSegment = mbLastSegment.a;
				return _Utils_eq(lastSegment.index, segment.index) ? A3($author$project$Structure$updatePropertyRec, tailSegments, kvp, child) : child;
			}
		};
		var childrenNew = A2(
			$elm$core$List$map,
			updateAt,
			A2($author$project$Structure$getUnder, segment.role, parent));
		return A3($author$project$Structure$replaceUnderFeature, segment.role, childrenNew, parent);
	});
var $author$project$Structure$updatePropertyRec = F3(
	function (segments, kvp, parent) {
		if (!segments.b) {
			return A2($author$project$Structure$updateProperty, kvp, parent);
		} else {
			var segment = segments.a;
			var tail = segments.b;
			return A4($author$project$Structure$updateChildrenUnder, segment, tail, kvp, parent);
		}
	});
var $author$project$Structure$updatePropertyByPath = F3(
	function (root, path, kvp) {
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		return A3($author$project$Structure$updatePropertyRec, segmentsNoRoot, kvp, root);
	});
var $author$project$Editor$persistVertexPositions = F2(
	function (eRootOld, eRootNew) {
		var verticesOld = A2(
			$author$project$Structure$nodesOf,
			$author$project$Editor$ContentCell($author$project$Editor$VertexCell),
			eRootOld);
		var persistVertexPos = F2(
			function (vOld, rootNew) {
				var mby = A2($author$project$Structure$tryFloatOf, $author$project$Editor$roleY, vOld);
				var mbx = A2($author$project$Structure$tryFloatOf, $author$project$Editor$roleX, vOld);
				var _v0 = _Utils_Tuple2(mbx, mby);
				if (_v0.a.$ === 'Nothing') {
					var _v1 = _v0.a;
					return rootNew;
				} else {
					if (_v0.b.$ === 'Nothing') {
						var _v2 = _v0.b;
						return rootNew;
					} else {
						var x = _v0.a.a;
						var y = _v0.b.a;
						var rootNew1 = A3(
							$author$project$Structure$updatePropertyByPath,
							rootNew,
							$author$project$Structure$pathOf(vOld),
							_Utils_Tuple2(
								$author$project$Editor$roleX,
								$author$project$Structure$asPFloat(x)));
						var rootNew2 = A3(
							$author$project$Structure$updatePropertyByPath,
							rootNew1,
							$author$project$Structure$pathOf(vOld),
							_Utils_Tuple2(
								$author$project$Editor$roleY,
								$author$project$Structure$asPFloat(y)));
						return rootNew2;
					}
				}
			});
		return A3($elm$core$List$foldl, persistVertexPos, eRootNew, verticesOld);
	});
var $author$project$Editor$Drag = F3(
	function (mousePosStart, vertexPosStart, path) {
		return {mousePosStart: mousePosStart, path: path, vertexPosStart: vertexPosStart};
	});
var $author$project$Structure$asPBool = function (b) {
	return $author$project$Structure$PBool(b);
};
var $author$project$Editor$noUpdate = function (editorModel) {
	return _Utils_Tuple2(
		_Utils_update(
			editorModel,
			{runXform: false}),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Structure$nodeAtI = F2(
	function (parent, segments) {
		nodeAtI:
		while (true) {
			if (segments.b) {
				var segment = segments.a;
				var tail = segments.b;
				var mbChildAt = function (child) {
					var _v2 = $author$project$Structure$pathOf(child);
					var pathSegmentsChild = _v2.a;
					var mbLastSegment = $elm$core$List$head(
						$elm$core$List$reverse(pathSegmentsChild));
					if (mbLastSegment.$ === 'Nothing') {
						return $elm$core$Maybe$Just(child);
					} else {
						var lastSegment = mbLastSegment.a;
						return _Utils_eq(lastSegment.index, segment.index) ? $elm$core$Maybe$Just(child) : $elm$core$Maybe$Nothing;
					}
				};
				var getAtIndex = function (children) {
					return A2(
						$elm$core$List$filterMap,
						$elm$core$Basics$identity,
						A2($elm$core$List$map, mbChildAt, children));
				};
				var nextChild = getAtIndex(
					A2($author$project$Structure$getUnder, segment.role, parent));
				if (nextChild.b && (!nextChild.b.b)) {
					var child = nextChild.a;
					var $temp$parent = child,
						$temp$segments = tail;
					parent = $temp$parent;
					segments = $temp$segments;
					continue nodeAtI;
				} else {
					return $elm$core$Maybe$Nothing;
				}
			} else {
				return $elm$core$Maybe$Just(parent);
			}
		}
	});
var $author$project$Structure$nodeAt = F2(
	function (root, path) {
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		return A2($author$project$Structure$nodeAtI, root, segmentsNoRoot);
	});
var $author$project$Structure$floatOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($author$project$Structure$tryFloatOf, role, node));
	});
var $author$project$Editor$p2dFromCell = function (cell) {
	return $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
		_Utils_Tuple2(
			A2($author$project$Structure$floatOf, $author$project$Editor$roleX, cell),
			A2($author$project$Structure$floatOf, $author$project$Editor$roleY, cell)));
};
var $gampleman$elm_visualization$Force$State = function (a) {
	return {$: 'State', a: a};
};
var $gampleman$elm_visualization$Force$reheat = function (_v0) {
	var config = _v0.a;
	return $gampleman$elm_visualization$Force$State(
		_Utils_update(
			config,
			{alpha: 1.0}));
};
var $author$project$Structure$replaceAt = F4(
	function (pathAfter, child, candidate, result) {
		return _Utils_eq(
			pathAfter,
			$author$project$Structure$pathOf(candidate)) ? _Utils_ap(
			result,
			_List_fromArray(
				[child])) : _Utils_ap(
			result,
			_List_fromArray(
				[candidate]));
	});
var $author$project$Structure$replaceNodeUnderCustom = F4(
	function (role, child, pathAfter, _v0) {
		var data = _v0.a;
		var features = data.features;
		var appender = F2(
			function (child2, children) {
				return A3(
					$elm$core$List$foldl,
					A2($author$project$Structure$replaceAt, pathAfter, child2),
					_List_Nil,
					children);
			});
		var featuresNew = _Utils_update(
			features,
			{
				custom: A4($author$project$Structure$updateCustomFeature, role, child, appender, features.custom)
			});
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{features: featuresNew}));
	});
var $author$project$Structure$replaceNodeUnderDefault = F3(
	function (child, pathAfter, _v0) {
		var data = _v0.a;
		var features = data.features;
		var featuresNew = function () {
			var _v1 = features._default;
			if (!_v1.b) {
				return _Utils_update(
					features,
					{
						_default: _List_fromArray(
							[child])
					});
			} else {
				var children = _v1;
				return _Utils_update(
					features,
					{
						_default: A3(
							$elm$core$List$foldl,
							A2($author$project$Structure$replaceAt, pathAfter, child),
							_List_Nil,
							children)
					});
			}
		}();
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{features: featuresNew}));
	});
var $author$project$Structure$replaceChildAtPathRec = F4(
	function (nodeNew, pathAt, segments, parent) {
		if (segments.b) {
			if (!segments.b.b) {
				var role = segments.a.role;
				return _Utils_eq(role, $author$project$Structure$roleDefault) ? A3($author$project$Structure$replaceNodeUnderDefault, nodeNew, pathAt, parent) : A4($author$project$Structure$replaceNodeUnderCustom, role, nodeNew, pathAt, parent);
			} else {
				var segment = segments.a;
				var tail = segments.b;
				return A5($author$project$Structure$replaceChildrenForChildReplace, nodeNew, pathAt, segment, tail, parent);
			}
		} else {
			return parent;
		}
	});
var $author$project$Structure$replaceChildrenForChildReplace = F5(
	function (nodeNew, pathAt, segment, tailSegments, parent) {
		var replaceChildAt = function (child) {
			var _v0 = $author$project$Structure$pathOf(child);
			var pathSegmentsChild = _v0.a;
			var mbLastSegment = $elm$core$List$head(
				$elm$core$List$reverse(pathSegmentsChild));
			if (mbLastSegment.$ === 'Nothing') {
				return child;
			} else {
				var lastSegment = mbLastSegment.a;
				return _Utils_eq(lastSegment.index, segment.index) ? A4($author$project$Structure$replaceChildAtPathRec, nodeNew, pathAt, tailSegments, child) : child;
			}
		};
		var childrenNew = A2(
			$elm$core$List$map,
			replaceChildAt,
			A2($author$project$Structure$getUnder, segment.role, parent));
		return A3($author$project$Structure$replaceUnderFeature, segment.role, childrenNew, parent);
	});
var $author$project$Structure$replaceChildAtPath = F3(
	function (nodeNew, path, root) {
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		return A4($author$project$Structure$replaceChildAtPathRec, nodeNew, path, segmentsNoRoot, root);
	});
var $author$project$Editor$roleGrabbed = $author$project$Structure$roleFromString('grabbed');
var $author$project$Editor$roleMouseEnter = $author$project$Structure$roleFromString('mouseEnter');
var $author$project$Structure$addFloat = F3(
	function (role, value, node) {
		return A2(
			$author$project$Structure$addProperty,
			_Utils_Tuple2(
				role,
				$author$project$Structure$PFloat(value)),
			node);
	});
var $gampleman$elm_visualization$Force$Center = F2(
	function (a, b) {
		return {$: 'Center', a: a, b: b};
	});
var $gampleman$elm_visualization$Force$center = $gampleman$elm_visualization$Force$Center;
var $author$project$Editor$customEdgeForcesFromGraph = function (cellGraph) {
	var forceLookup = function (edge) {
		var mbLookupWithDefault = function (key) {
			return A2(
				$elm$core$Maybe$andThen,
				function (v) {
					return $elm$core$Maybe$Just(
						$author$project$Structure$pathAsIdFromNode(v));
				},
				A2(
					$elm$core$Dict$get,
					key,
					$author$project$Editor$dictNameToVertex(cellGraph)));
		};
		var _v0 = _Utils_Tuple2(
			A2($author$project$Structure$textOf, $author$project$Editor$roleFrom, edge),
			A2($author$project$Structure$textOf, $author$project$Editor$roleTo, edge));
		var from = _v0.a;
		var to = _v0.b;
		var mbSource = mbLookupWithDefault(from);
		var mbTarget = mbLookupWithDefault(to);
		var _v1 = _Utils_Tuple2(mbSource, mbTarget);
		if (_v1.a.$ === 'Nothing') {
			var _v2 = _v1.a;
			return $elm$core$Maybe$Nothing;
		} else {
			if (_v1.b.$ === 'Nothing') {
				var _v3 = _v1.b;
				return $elm$core$Maybe$Nothing;
			} else {
				var source = _v1.a.a;
				var target = _v1.b.a;
				return _Utils_eq(source, target) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
					{distance: 150, source: source, strength: $elm$core$Maybe$Nothing, target: target});
			}
		}
	};
	var edges = A2(
		$author$project$Structure$nodesOf,
		$author$project$Editor$ContentCell($author$project$Editor$EdgeCell),
		cellGraph);
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, forceLookup, edges));
};
var $gampleman$elm_visualization$Force$Links = F2(
	function (a, b) {
		return {$: 'Links', a: a, b: b};
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $gampleman$elm_visualization$Force$customLinks = F2(
	function (iters, list) {
		var counts = A3(
			$elm$core$List$foldr,
			F2(
				function (_v1, d) {
					var source = _v1.source;
					var target = _v1.target;
					return A3(
						$elm$core$Dict$update,
						target,
						A2(
							$elm$core$Basics$composeL,
							A2(
								$elm$core$Basics$composeL,
								$elm$core$Maybe$Just,
								$elm$core$Maybe$withDefault(1)),
							$elm$core$Maybe$map(
								$elm$core$Basics$add(1))),
						A3(
							$elm$core$Dict$update,
							source,
							A2(
								$elm$core$Basics$composeL,
								A2(
									$elm$core$Basics$composeL,
									$elm$core$Maybe$Just,
									$elm$core$Maybe$withDefault(1)),
								$elm$core$Maybe$map(
									$elm$core$Basics$add(1))),
							d));
				}),
			$elm$core$Dict$empty,
			list);
		var count = function (key) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				A2($elm$core$Dict$get, key, counts));
		};
		return A2(
			$gampleman$elm_visualization$Force$Links,
			iters,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var source = _v0.source;
					var target = _v0.target;
					var distance = _v0.distance;
					var strength = _v0.strength;
					return {
						bias: count(source) / (count(source) + count(target)),
						distance: distance,
						source: source,
						strength: A2(
							$elm$core$Maybe$withDefault,
							1 / A2(
								$elm$core$Basics$min,
								count(source),
								count(target)),
							strength),
						target: target
					};
				},
				list));
	});
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$Editor$initialAngle = $elm$core$Basics$pi * (3 - $elm$core$Basics$sqrt(5));
var $author$project$Editor$initialRadius = 10;
var $elm$core$Basics$sin = _Basics_sin;
var $author$project$Editor$forceEntityFromVertex = F2(
	function (index, cell) {
		var radius = $elm$core$Basics$sqrt(index) * $author$project$Editor$initialRadius;
		var mbY = A2($author$project$Structure$tryFloatOf, $author$project$Editor$roleY, cell);
		var mbX = A2($author$project$Structure$tryFloatOf, $author$project$Editor$roleX, cell);
		var angle = index * $author$project$Editor$initialAngle;
		var xNew = function () {
			if (mbX.$ === 'Nothing') {
				return radius * $elm$core$Basics$cos(angle);
			} else {
				var xc = mbX.a;
				return xc;
			}
		}();
		var yNew = function () {
			if (mbY.$ === 'Nothing') {
				return radius * $elm$core$Basics$sin(angle);
			} else {
				var yc = mbY.a;
				return yc;
			}
		}();
		return {
			id: $author$project$Structure$pathAsIdFromNode(cell),
			value: cell,
			vx: 0.0,
			vy: 0.0,
			x: xNew,
			y: yNew
		};
	});
var $gampleman$elm_visualization$Force$ManyBody = F2(
	function (a, b) {
		return {$: 'ManyBody', a: a, b: b};
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $gampleman$elm_visualization$Force$customManyBody = function (theta) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$Dict$fromList,
		$gampleman$elm_visualization$Force$ManyBody(theta));
};
var $gampleman$elm_visualization$Force$manyBodyStrength = function (strength) {
	return A2(
		$elm$core$Basics$composeL,
		$gampleman$elm_visualization$Force$customManyBody(0.9),
		$elm$core$List$map(
			function (key) {
				return _Utils_Tuple2(key, strength);
			}));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$pow = _Basics_pow;
var $gampleman$elm_visualization$Force$simulation = function (forces) {
	return $gampleman$elm_visualization$Force$State(
		{
			alpha: 1.0,
			alphaDecay: 1 - A2($elm$core$Basics$pow, 0.001, 1 / 300),
			alphaTarget: 0.0,
			forces: forces,
			minAlpha: 0.001,
			velocityDecay: 0.6
		});
};
var $gampleman$elm_visualization$Force$nTimes = F3(
	function (fn, times, input) {
		nTimes:
		while (true) {
			if (times <= 0) {
				return input;
			} else {
				var $temp$fn = fn,
					$temp$times = times - 1,
					$temp$input = fn(input);
				fn = $temp$fn;
				times = $temp$times;
				input = $temp$input;
				continue nTimes;
			}
		}
	});
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $ianmackenzie$elm_geometry$Vector2d$components = function (_v0) {
	var components_ = _v0.a;
	return components_;
};
var $ianmackenzie$elm_geometry$BoundingBox2d$maxX = function (_v0) {
	var boundingBox = _v0.a;
	return boundingBox.maxX;
};
var $ianmackenzie$elm_geometry$BoundingBox2d$maxY = function (_v0) {
	var boundingBox = _v0.a;
	return boundingBox.maxY;
};
var $ianmackenzie$elm_geometry$BoundingBox2d$minX = function (_v0) {
	var boundingBox = _v0.a;
	return boundingBox.minX;
};
var $ianmackenzie$elm_geometry$BoundingBox2d$minY = function (_v0) {
	var boundingBox = _v0.a;
	return boundingBox.minY;
};
var $ianmackenzie$elm_geometry$BoundingBox2d$dimensions = function (boundingBox) {
	return _Utils_Tuple2(
		$ianmackenzie$elm_geometry$BoundingBox2d$maxX(boundingBox) - $ianmackenzie$elm_geometry$BoundingBox2d$minX(boundingBox),
		$ianmackenzie$elm_geometry$BoundingBox2d$maxY(boundingBox) - $ianmackenzie$elm_geometry$BoundingBox2d$minY(boundingBox));
};
var $ianmackenzie$elm_geometry$Bootstrap$Point2d$coordinates = function (_v0) {
	var coordinates_ = _v0.a;
	return coordinates_;
};
var $ianmackenzie$elm_geometry$Geometry$Types$Vector2d = function (a) {
	return {$: 'Vector2d', a: a};
};
var $ianmackenzie$elm_geometry$Vector2d$fromComponents = $ianmackenzie$elm_geometry$Geometry$Types$Vector2d;
var $ianmackenzie$elm_geometry$Vector2d$from = F2(
	function (firstPoint, secondPoint) {
		var _v0 = $ianmackenzie$elm_geometry$Bootstrap$Point2d$coordinates(secondPoint);
		var x2 = _v0.a;
		var y2 = _v0.b;
		var _v1 = $ianmackenzie$elm_geometry$Bootstrap$Point2d$coordinates(firstPoint);
		var x1 = _v1.a;
		var y1 = _v1.b;
		return $ianmackenzie$elm_geometry$Vector2d$fromComponents(
			_Utils_Tuple2(x2 - x1, y2 - y1));
	});
var $ianmackenzie$elm_geometry$Vector2d$squaredLength = function (vector) {
	var _v0 = $ianmackenzie$elm_geometry$Vector2d$components(vector);
	var x = _v0.a;
	var y = _v0.b;
	return (x * x) + (y * y);
};
var $ianmackenzie$elm_geometry$Point2d$squaredDistanceFrom = F2(
	function (firstPoint, secondPoint) {
		return $ianmackenzie$elm_geometry$Vector2d$squaredLength(
			A2($ianmackenzie$elm_geometry$Vector2d$from, firstPoint, secondPoint));
	});
var $ianmackenzie$elm_geometry$Point2d$distanceFrom = F2(
	function (firstPoint, secondPoint) {
		return $elm$core$Basics$sqrt(
			A2($ianmackenzie$elm_geometry$Point2d$squaredDistanceFrom, firstPoint, secondPoint));
	});
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $ianmackenzie$elm_geometry$Vector2d$scaleBy = F2(
	function (scale, vector) {
		var _v0 = $ianmackenzie$elm_geometry$Vector2d$components(vector);
		var x = _v0.a;
		var y = _v0.b;
		return $ianmackenzie$elm_geometry$Vector2d$fromComponents(
			_Utils_Tuple2(x * scale, y * scale));
	});
var $ianmackenzie$elm_geometry$Vector2d$sum = F2(
	function (firstVector, secondVector) {
		var _v0 = $ianmackenzie$elm_geometry$Vector2d$components(secondVector);
		var x2 = _v0.a;
		var y2 = _v0.b;
		var _v1 = $ianmackenzie$elm_geometry$Vector2d$components(firstVector);
		var x1 = _v1.a;
		var y1 = _v1.b;
		return $ianmackenzie$elm_geometry$Vector2d$fromComponents(
			_Utils_Tuple2(x1 + x2, y1 + y2));
	});
var $ianmackenzie$elm_geometry$Vector2d$zero = $ianmackenzie$elm_geometry$Vector2d$fromComponents(
	_Utils_Tuple2(0, 0));
var $gampleman$elm_visualization$Force$ManyBody$applyForce = F4(
	function (alpha, theta, qtree, vertex) {
		var isFarAway = function (treePart) {
			var distance = A2($ianmackenzie$elm_geometry$Point2d$distanceFrom, vertex.position, treePart.aggregate.position);
			var _v2 = $ianmackenzie$elm_geometry$BoundingBox2d$dimensions(treePart.boundingBox);
			var width = _v2.a;
			return _Utils_cmp(width / distance, theta) < 0;
		};
		var calculateVelocity = F2(
			function (target, source) {
				var delta = A2($ianmackenzie$elm_geometry$Vector2d$from, target.position, source.position);
				var weight = (source.strength * alpha) / $ianmackenzie$elm_geometry$Vector2d$squaredLength(delta);
				return $elm$core$Basics$isNaN(weight) ? $ianmackenzie$elm_geometry$Vector2d$zero : A2($ianmackenzie$elm_geometry$Vector2d$scaleBy, weight, delta);
			});
		var useAggregate = function (treePart) {
			return A2(calculateVelocity, vertex, treePart.aggregate);
		};
		switch (qtree.$) {
			case 'Empty':
				return $ianmackenzie$elm_geometry$Vector2d$zero;
			case 'Leaf':
				var leaf = qtree.a;
				if (isFarAway(leaf)) {
					return useAggregate(leaf);
				} else {
					var applyForceFromPoint = F2(
						function (point, accum) {
							return _Utils_eq(point.key, vertex.key) ? accum : A2(
								$ianmackenzie$elm_geometry$Vector2d$sum,
								A2(calculateVelocity, vertex, point),
								accum);
						});
					var _v1 = leaf.children;
					var first = _v1.a;
					var rest = _v1.b;
					return A3(
						$elm$core$List$foldl,
						applyForceFromPoint,
						$ianmackenzie$elm_geometry$Vector2d$zero,
						A2($elm$core$List$cons, first, rest));
				}
			default:
				var node = qtree.a;
				if (isFarAway(node)) {
					return useAggregate(node);
				} else {
					var helper = function (tree) {
						return A4($gampleman$elm_visualization$Force$ManyBody$applyForce, alpha, theta, tree, vertex);
					};
					return A2(
						$ianmackenzie$elm_geometry$Vector2d$sum,
						helper(node.sw),
						A2(
							$ianmackenzie$elm_geometry$Vector2d$sum,
							helper(node.se),
							A2(
								$ianmackenzie$elm_geometry$Vector2d$sum,
								helper(node.ne),
								helper(node.nw))));
				}
		}
	});
var $ianmackenzie$elm_geometry$Point2d$coordinates = function (_v0) {
	var coordinates_ = _v0.a;
	return coordinates_;
};
var $gampleman$elm_visualization$Force$ManyBody$constructSuperPoint = F2(
	function (first, rest) {
		var initialStrength = first.strength;
		var initialPoint = $ianmackenzie$elm_geometry$Point2d$coordinates(first.position);
		var folder = F2(
			function (point, _v3) {
				var _v4 = _v3.a;
				var accumX = _v4.a;
				var accumY = _v4.b;
				var strength = _v3.b;
				var size = _v3.c;
				var _v2 = $ianmackenzie$elm_geometry$Point2d$coordinates(point.position);
				var x = _v2.a;
				var y = _v2.b;
				return _Utils_Tuple3(
					_Utils_Tuple2(accumX + x, accumY + y),
					strength + point.strength,
					size + 1);
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			folder,
			_Utils_Tuple3(initialPoint, initialStrength, 1),
			rest);
		var _v1 = _v0.a;
		var totalX = _v1.a;
		var totalY = _v1.b;
		var totalStrength = _v0.b;
		var totalSize = _v0.c;
		return {
			position: $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
				_Utils_Tuple2(totalX / totalSize, totalY / totalSize)),
			strength: totalStrength
		};
	});
var $gampleman$elm_visualization$Force$ManyBody$config = {
	combineAggregates: $gampleman$elm_visualization$Force$ManyBody$constructSuperPoint,
	combineVertices: $gampleman$elm_visualization$Force$ManyBody$constructSuperPoint,
	toPoint: function ($) {
		return $.position;
	}
};
var $gampleman$elm_visualization$Force$QuadTree$Empty = {$: 'Empty'};
var $gampleman$elm_visualization$Force$QuadTree$empty = $gampleman$elm_visualization$Force$QuadTree$Empty;
var $gampleman$elm_visualization$Force$QuadTree$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $gampleman$elm_visualization$Force$QuadTree$Node = function (a) {
	return {$: 'Node', a: a};
};
var $ianmackenzie$elm_geometry$BoundingBox2d$contains = F2(
	function (point, boundingBox) {
		var _v0 = $ianmackenzie$elm_geometry$Point2d$coordinates(point);
		var x = _v0.a;
		var y = _v0.b;
		return ((_Utils_cmp(
			$ianmackenzie$elm_geometry$BoundingBox2d$minX(boundingBox),
			x) < 1) && (_Utils_cmp(
			x,
			$ianmackenzie$elm_geometry$BoundingBox2d$maxX(boundingBox)) < 1)) && ((_Utils_cmp(
			$ianmackenzie$elm_geometry$BoundingBox2d$minY(boundingBox),
			y) < 1) && (_Utils_cmp(
			y,
			$ianmackenzie$elm_geometry$BoundingBox2d$maxY(boundingBox)) < 1));
	});
var $ianmackenzie$elm_geometry$BoundingBox2d$extrema = function (_v0) {
	var extrema_ = _v0.a;
	return extrema_;
};
var $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox2d = function (a) {
	return {$: 'BoundingBox2d', a: a};
};
var $ianmackenzie$elm_geometry$BoundingBox2d$fromExtrema = function (extrema_) {
	return ((_Utils_cmp(extrema_.minX, extrema_.maxX) < 1) && (_Utils_cmp(extrema_.minY, extrema_.maxY) < 1)) ? $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox2d(extrema_) : $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox2d(
		{
			maxX: A2($elm$core$Basics$max, extrema_.minX, extrema_.maxX),
			maxY: A2($elm$core$Basics$max, extrema_.minY, extrema_.maxY),
			minX: A2($elm$core$Basics$min, extrema_.minX, extrema_.maxX),
			minY: A2($elm$core$Basics$min, extrema_.minY, extrema_.maxY)
		});
};
var $elm$core$Basics$ge = _Utils_ge;
var $ianmackenzie$elm_geometry$BoundingBox2d$hull = F2(
	function (firstBox, secondBox) {
		return $ianmackenzie$elm_geometry$BoundingBox2d$fromExtrema(
			{
				maxX: A2(
					$elm$core$Basics$max,
					$ianmackenzie$elm_geometry$BoundingBox2d$maxX(firstBox),
					$ianmackenzie$elm_geometry$BoundingBox2d$maxX(secondBox)),
				maxY: A2(
					$elm$core$Basics$max,
					$ianmackenzie$elm_geometry$BoundingBox2d$maxY(firstBox),
					$ianmackenzie$elm_geometry$BoundingBox2d$maxY(secondBox)),
				minX: A2(
					$elm$core$Basics$min,
					$ianmackenzie$elm_geometry$BoundingBox2d$minX(firstBox),
					$ianmackenzie$elm_geometry$BoundingBox2d$minX(secondBox)),
				minY: A2(
					$elm$core$Basics$min,
					$ianmackenzie$elm_geometry$BoundingBox2d$minY(firstBox),
					$ianmackenzie$elm_geometry$BoundingBox2d$minY(secondBox))
			});
	});
var $gampleman$elm_visualization$Force$QuadTree$NE = {$: 'NE'};
var $gampleman$elm_visualization$Force$QuadTree$NW = {$: 'NW'};
var $gampleman$elm_visualization$Force$QuadTree$SE = {$: 'SE'};
var $gampleman$elm_visualization$Force$QuadTree$SW = {$: 'SW'};
var $ianmackenzie$elm_geometry$BoundingBox2d$midX = function (_v0) {
	var boundingBox = _v0.a;
	return boundingBox.minX + (0.5 * (boundingBox.maxX - boundingBox.minX));
};
var $ianmackenzie$elm_geometry$BoundingBox2d$midY = function (_v0) {
	var boundingBox = _v0.a;
	return boundingBox.minY + (0.5 * (boundingBox.maxY - boundingBox.minY));
};
var $ianmackenzie$elm_geometry$BoundingBox2d$centerPoint = function (boundingBox) {
	return $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
		_Utils_Tuple2(
			$ianmackenzie$elm_geometry$BoundingBox2d$midX(boundingBox),
			$ianmackenzie$elm_geometry$BoundingBox2d$midY(boundingBox)));
};
var $ianmackenzie$elm_geometry$BoundingBox2d$centroid = function (boundingBox) {
	return $ianmackenzie$elm_geometry$BoundingBox2d$centerPoint(boundingBox);
};
var $gampleman$elm_visualization$Force$QuadTree$quadrant = F2(
	function (boundingBox, point) {
		var _v0 = $ianmackenzie$elm_geometry$Point2d$coordinates(point);
		var x = _v0.a;
		var y = _v0.b;
		var _v1 = $ianmackenzie$elm_geometry$Point2d$coordinates(
			$ianmackenzie$elm_geometry$BoundingBox2d$centroid(boundingBox));
		var midX = _v1.a;
		var midY = _v1.b;
		var _v2 = $ianmackenzie$elm_geometry$BoundingBox2d$extrema(boundingBox);
		var minX = _v2.minX;
		var minY = _v2.minY;
		var maxX = _v2.maxX;
		var maxY = _v2.maxY;
		return (_Utils_cmp(y, midY) > -1) ? ((_Utils_cmp(x, midX) > -1) ? $gampleman$elm_visualization$Force$QuadTree$NE : $gampleman$elm_visualization$Force$QuadTree$NW) : ((_Utils_cmp(x, midX) > -1) ? $gampleman$elm_visualization$Force$QuadTree$SE : $gampleman$elm_visualization$Force$QuadTree$SW);
	});
var $ianmackenzie$elm_geometry$BoundingBox2d$singleton = function (point) {
	var _v0 = $ianmackenzie$elm_geometry$Point2d$coordinates(point);
	var x = _v0.a;
	var y = _v0.b;
	return $ianmackenzie$elm_geometry$BoundingBox2d$fromExtrema(
		{maxX: x, maxY: y, minX: x, minY: y});
};
var $gampleman$elm_visualization$Force$QuadTree$singleton = F2(
	function (toPoint, vertex) {
		return $gampleman$elm_visualization$Force$QuadTree$Leaf(
			{
				aggregate: _Utils_Tuple0,
				boundingBox: $ianmackenzie$elm_geometry$BoundingBox2d$singleton(
					toPoint(vertex)),
				children: _Utils_Tuple2(vertex, _List_Nil)
			});
	});
var $gampleman$elm_visualization$Force$QuadTree$insertBy = F3(
	function (toPoint, vertex, qtree) {
		switch (qtree.$) {
			case 'Empty':
				return $gampleman$elm_visualization$Force$QuadTree$Leaf(
					{
						aggregate: _Utils_Tuple0,
						boundingBox: $ianmackenzie$elm_geometry$BoundingBox2d$singleton(
							toPoint(vertex)),
						children: _Utils_Tuple2(vertex, _List_Nil)
					});
			case 'Leaf':
				var leaf = qtree.a;
				var maxSize = 32;
				var _v1 = leaf.children;
				var first = _v1.a;
				var rest = _v1.b;
				var newSize = 2 + $elm$core$List$length(rest);
				if (_Utils_cmp(newSize, maxSize) > -1) {
					var initial = $gampleman$elm_visualization$Force$QuadTree$Node(
						{
							aggregate: _Utils_Tuple0,
							boundingBox: A2(
								$ianmackenzie$elm_geometry$BoundingBox2d$hull,
								leaf.boundingBox,
								$ianmackenzie$elm_geometry$BoundingBox2d$singleton(
									toPoint(vertex))),
							ne: $gampleman$elm_visualization$Force$QuadTree$Empty,
							nw: $gampleman$elm_visualization$Force$QuadTree$Empty,
							se: $gampleman$elm_visualization$Force$QuadTree$Empty,
							sw: $gampleman$elm_visualization$Force$QuadTree$Empty
						});
					return A3(
						$elm$core$List$foldl,
						$gampleman$elm_visualization$Force$QuadTree$insertBy(toPoint),
						initial,
						A2($elm$core$List$cons, first, rest));
				} else {
					return $gampleman$elm_visualization$Force$QuadTree$Leaf(
						{
							aggregate: _Utils_Tuple0,
							boundingBox: A2(
								$ianmackenzie$elm_geometry$BoundingBox2d$hull,
								leaf.boundingBox,
								$ianmackenzie$elm_geometry$BoundingBox2d$singleton(
									toPoint(vertex))),
							children: _Utils_Tuple2(
								vertex,
								A2($elm$core$List$cons, first, rest))
						});
				}
			default:
				var node = qtree.a;
				var point = toPoint(vertex);
				if (A2($ianmackenzie$elm_geometry$BoundingBox2d$contains, point, node.boundingBox)) {
					var _v2 = A2($gampleman$elm_visualization$Force$QuadTree$quadrant, node.boundingBox, point);
					switch (_v2.$) {
						case 'NE':
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: node.aggregate,
									boundingBox: node.boundingBox,
									ne: A3($gampleman$elm_visualization$Force$QuadTree$insertBy, toPoint, vertex, node.ne),
									nw: node.nw,
									se: node.se,
									sw: node.sw
								});
						case 'SE':
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: node.aggregate,
									boundingBox: node.boundingBox,
									ne: node.ne,
									nw: node.nw,
									se: A3($gampleman$elm_visualization$Force$QuadTree$insertBy, toPoint, vertex, node.se),
									sw: node.sw
								});
						case 'NW':
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: node.aggregate,
									boundingBox: node.boundingBox,
									ne: node.ne,
									nw: A3($gampleman$elm_visualization$Force$QuadTree$insertBy, toPoint, vertex, node.nw),
									se: node.se,
									sw: node.sw
								});
						default:
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: node.aggregate,
									boundingBox: node.boundingBox,
									ne: node.ne,
									nw: node.nw,
									se: node.se,
									sw: A3($gampleman$elm_visualization$Force$QuadTree$insertBy, toPoint, vertex, node.sw)
								});
					}
				} else {
					var _v3 = $ianmackenzie$elm_geometry$BoundingBox2d$extrema(node.boundingBox);
					var minX = _v3.minX;
					var minY = _v3.minY;
					var maxX = _v3.maxX;
					var maxY = _v3.maxY;
					var _v4 = $ianmackenzie$elm_geometry$BoundingBox2d$dimensions(node.boundingBox);
					var width = _v4.a;
					var height = _v4.b;
					var _v5 = A2($gampleman$elm_visualization$Force$QuadTree$quadrant, node.boundingBox, point);
					switch (_v5.$) {
						case 'NE':
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: _Utils_Tuple0,
									boundingBox: $ianmackenzie$elm_geometry$BoundingBox2d$fromExtrema(
										{maxX: maxX + width, maxY: maxY + height, minX: minX, minY: minY}),
									ne: A2($gampleman$elm_visualization$Force$QuadTree$singleton, toPoint, vertex),
									nw: $gampleman$elm_visualization$Force$QuadTree$Empty,
									se: $gampleman$elm_visualization$Force$QuadTree$Empty,
									sw: qtree
								});
						case 'SE':
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: _Utils_Tuple0,
									boundingBox: $ianmackenzie$elm_geometry$BoundingBox2d$fromExtrema(
										{maxX: maxX + width, maxY: maxY, minX: minX, minY: minY - height}),
									ne: $gampleman$elm_visualization$Force$QuadTree$Empty,
									nw: qtree,
									se: A2($gampleman$elm_visualization$Force$QuadTree$singleton, toPoint, vertex),
									sw: $gampleman$elm_visualization$Force$QuadTree$Empty
								});
						case 'NW':
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: _Utils_Tuple0,
									boundingBox: $ianmackenzie$elm_geometry$BoundingBox2d$fromExtrema(
										{maxX: maxX, maxY: maxY + height, minX: minX - width, minY: minY}),
									ne: $gampleman$elm_visualization$Force$QuadTree$Empty,
									nw: A2($gampleman$elm_visualization$Force$QuadTree$singleton, toPoint, vertex),
									se: qtree,
									sw: $gampleman$elm_visualization$Force$QuadTree$Empty
								});
						default:
							return $gampleman$elm_visualization$Force$QuadTree$Node(
								{
									aggregate: _Utils_Tuple0,
									boundingBox: $ianmackenzie$elm_geometry$BoundingBox2d$fromExtrema(
										{maxX: maxX, maxY: maxY, minX: minX - width, minY: minY - height}),
									ne: qtree,
									nw: $gampleman$elm_visualization$Force$QuadTree$Empty,
									se: $gampleman$elm_visualization$Force$QuadTree$Empty,
									sw: A2($gampleman$elm_visualization$Force$QuadTree$singleton, toPoint, vertex)
								});
					}
				}
		}
	});
var $gampleman$elm_visualization$Force$QuadTree$fromList = function (toPoint) {
	return A2(
		$elm$core$List$foldl,
		$gampleman$elm_visualization$Force$QuadTree$insertBy(toPoint),
		$gampleman$elm_visualization$Force$QuadTree$empty);
};
var $gampleman$elm_visualization$Force$QuadTree$getAggregate = function (qtree) {
	switch (qtree.$) {
		case 'Empty':
			return $elm$core$Maybe$Nothing;
		case 'Leaf':
			var aggregate = qtree.a.aggregate;
			return $elm$core$Maybe$Just(aggregate);
		default:
			var aggregate = qtree.a.aggregate;
			return $elm$core$Maybe$Just(aggregate);
	}
};
var $gampleman$elm_visualization$Force$QuadTree$performAggregate = F2(
	function (config, vanillaQuadTree) {
		var combineAggregates = config.combineAggregates;
		var combineVertices = config.combineVertices;
		switch (vanillaQuadTree.$) {
			case 'Empty':
				return $gampleman$elm_visualization$Force$QuadTree$Empty;
			case 'Leaf':
				var leaf = vanillaQuadTree.a;
				var _v1 = leaf.children;
				var first = _v1.a;
				var rest = _v1.b;
				return $gampleman$elm_visualization$Force$QuadTree$Leaf(
					{
						aggregate: A2(combineVertices, first, rest),
						boundingBox: leaf.boundingBox,
						children: _Utils_Tuple2(first, rest)
					});
			default:
				var node = vanillaQuadTree.a;
				var newSw = A2($gampleman$elm_visualization$Force$QuadTree$performAggregate, config, node.sw);
				var newSe = A2($gampleman$elm_visualization$Force$QuadTree$performAggregate, config, node.se);
				var newNw = A2($gampleman$elm_visualization$Force$QuadTree$performAggregate, config, node.nw);
				var newNe = A2($gampleman$elm_visualization$Force$QuadTree$performAggregate, config, node.ne);
				var subresults = A2(
					$elm$core$List$filterMap,
					$gampleman$elm_visualization$Force$QuadTree$getAggregate,
					_List_fromArray(
						[newNw, newSw, newNe, newSe]));
				if (!subresults.b) {
					return $gampleman$elm_visualization$Force$QuadTree$Empty;
				} else {
					var x = subresults.a;
					var xs = subresults.b;
					return $gampleman$elm_visualization$Force$QuadTree$Node(
						{
							aggregate: A2(combineAggregates, x, xs),
							boundingBox: node.boundingBox,
							ne: newNe,
							nw: newNw,
							se: newSe,
							sw: newSw
						});
				}
		}
	});
var $gampleman$elm_visualization$Force$ManyBody$manyBody = F3(
	function (alpha, theta, vertices) {
		var withAggregates = A2(
			$gampleman$elm_visualization$Force$QuadTree$performAggregate,
			$gampleman$elm_visualization$Force$ManyBody$config,
			A2(
				$gampleman$elm_visualization$Force$QuadTree$fromList,
				function ($) {
					return $.position;
				},
				vertices));
		var updateVertex = function (vertex) {
			return _Utils_update(
				vertex,
				{
					velocity: A2(
						$ianmackenzie$elm_geometry$Vector2d$sum,
						vertex.velocity,
						A4($gampleman$elm_visualization$Force$ManyBody$applyForce, alpha, theta, withAggregates, vertex))
				});
		};
		return A2($elm$core$List$map, updateVertex, vertices);
	});
var $gampleman$elm_visualization$Force$ManyBody$wrapper = F4(
	function (alpha, theta, strengths, points) {
		var vertices = A2(
			$elm$core$List$map,
			function (_v2) {
				var key = _v2.a;
				var point = _v2.b;
				var x = point.x;
				var y = point.y;
				var strength = A2(
					$elm$core$Maybe$withDefault,
					0,
					A2($elm$core$Dict$get, key, strengths));
				return {
					key: key,
					position: $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
						_Utils_Tuple2(x, y)),
					strength: strength,
					velocity: $ianmackenzie$elm_geometry$Vector2d$zero
				};
			},
			$elm$core$Dict$toList(points));
		var updater = F2(
			function (newVertex, maybePoint) {
				if (maybePoint.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					var point = maybePoint.a;
					var _v1 = $ianmackenzie$elm_geometry$Vector2d$components(newVertex.velocity);
					var dvx = _v1.a;
					var dvy = _v1.b;
					return $elm$core$Maybe$Just(
						_Utils_update(
							point,
							{vx: point.vx + dvx, vy: point.vy + dvy}));
				}
			});
		var newVertices = A3($gampleman$elm_visualization$Force$ManyBody$manyBody, alpha, theta, vertices);
		var folder = F2(
			function (newVertex, pointsDict) {
				return A3(
					$elm$core$Dict$update,
					newVertex.key,
					updater(newVertex),
					pointsDict);
			});
		return A3($elm$core$List$foldl, folder, points, newVertices);
	});
var $gampleman$elm_visualization$Force$applyForce = F3(
	function (alpha, force, entities) {
		switch (force.$) {
			case 'Center':
				var x = force.a;
				var y = force.b;
				var n = $elm$core$Dict$size(entities);
				var _v1 = A3(
					$elm$core$Dict$foldr,
					F3(
						function (_v2, ent, _v3) {
							var sx0 = _v3.a;
							var sy0 = _v3.b;
							return _Utils_Tuple2(sx0 + ent.x, sy0 + ent.y);
						}),
					_Utils_Tuple2(0, 0),
					entities);
				var sumx = _v1.a;
				var sumy = _v1.b;
				var sx = (sumx / n) - x;
				var sy = (sumy / n) - y;
				return A2(
					$elm$core$Dict$map,
					F2(
						function (_v4, ent) {
							return _Utils_update(
								ent,
								{x: ent.x - sx, y: ent.y - sy});
						}),
					entities);
			case 'Collision':
				var _float = force.a;
				var collisionParamidDict = force.b;
				return entities;
			case 'Links':
				var iters = force.a;
				var lnks = force.b;
				return A3(
					$gampleman$elm_visualization$Force$nTimes,
					function (entitiesList) {
						return A3(
							$elm$core$List$foldl,
							F2(
								function (_v5, ents) {
									var source = _v5.source;
									var target = _v5.target;
									var distance = _v5.distance;
									var strength = _v5.strength;
									var bias = _v5.bias;
									var _v6 = _Utils_Tuple2(
										A2($elm$core$Dict$get, source, ents),
										A2($elm$core$Dict$get, target, ents));
									if ((_v6.a.$ === 'Just') && (_v6.b.$ === 'Just')) {
										var sourceNode = _v6.a.a;
										var targetNode = _v6.b.a;
										var y = ((targetNode.y + targetNode.vy) - sourceNode.y) - sourceNode.vy;
										var x = ((targetNode.x + targetNode.vx) - sourceNode.x) - sourceNode.vx;
										var d = $elm$core$Basics$sqrt(
											A2($elm$core$Basics$pow, x, 2) + A2($elm$core$Basics$pow, y, 2));
										var l = (((d - distance) / d) * alpha) * strength;
										return A3(
											$elm$core$Dict$update,
											source,
											$elm$core$Maybe$map(
												function (tn) {
													return _Utils_update(
														tn,
														{vx: tn.vx + ((x * l) * (1 - bias)), vy: tn.vy + ((y * l) * (1 - bias))});
												}),
											A3(
												$elm$core$Dict$update,
												target,
												$elm$core$Maybe$map(
													function (sn) {
														return _Utils_update(
															sn,
															{vx: sn.vx - ((x * l) * bias), vy: sn.vy - ((y * l) * bias)});
													}),
												ents));
									} else {
										var otherwise = _v6;
										return ents;
									}
								}),
							entitiesList,
							lnks);
					},
					iters,
					entities);
			case 'ManyBody':
				var theta = force.a;
				var entityStrengths = force.b;
				return A4($gampleman$elm_visualization$Force$ManyBody$wrapper, alpha, theta, entityStrengths, entities);
			case 'X':
				var directionalParamidDict = force.a;
				return entities;
			default:
				var directionalParamidDict = force.a;
				return entities;
		}
	});
var $gampleman$elm_visualization$Force$tick = F2(
	function (_v0, nodes) {
		var state = _v0.a;
		var updateEntity = function (ent) {
			return _Utils_update(
				ent,
				{vx: ent.vx * state.velocityDecay, vy: ent.vy * state.velocityDecay, x: ent.x + (ent.vx * state.velocityDecay), y: ent.y + (ent.vy * state.velocityDecay)});
		};
		var dictNodes = A3(
			$elm$core$List$foldl,
			function (node) {
				return A2($elm$core$Dict$insert, node.id, node);
			},
			$elm$core$Dict$empty,
			nodes);
		var alpha = state.alpha + ((state.alphaTarget - state.alpha) * state.alphaDecay);
		var newNodes = A3(
			$elm$core$List$foldl,
			$gampleman$elm_visualization$Force$applyForce(alpha),
			dictNodes,
			state.forces);
		return _Utils_Tuple2(
			$gampleman$elm_visualization$Force$State(
				_Utils_update(
					state,
					{alpha: alpha})),
			A2(
				$elm$core$List$map,
				updateEntity,
				$elm$core$Dict$values(newNodes)));
	});
var $ianmackenzie$elm_geometry$Point2d$translateBy = F2(
	function (vector, point) {
		var _v0 = $ianmackenzie$elm_geometry$Vector2d$components(vector);
		var vx = _v0.a;
		var vy = _v0.b;
		var _v1 = $ianmackenzie$elm_geometry$Point2d$coordinates(point);
		var px = _v1.a;
		var py = _v1.b;
		return $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
			_Utils_Tuple2(px + vx, py + vy));
	});
var $author$project$Editor$updateDrag = F3(
	function (eRoot, drag, mousePosCurrent) {
		var delta = A2($ianmackenzie$elm_geometry$Vector2d$from, drag.mousePosStart, mousePosCurrent);
		var _v0 = $ianmackenzie$elm_geometry$Point2d$coordinates(
			A2($ianmackenzie$elm_geometry$Point2d$translateBy, delta, drag.vertexPosStart));
		var xNew = _v0.a;
		var yNew = _v0.b;
		var eRootTemp = A3(
			$author$project$Structure$updatePropertyByPath,
			eRoot,
			drag.path,
			_Utils_Tuple2(
				$author$project$Editor$roleX,
				$author$project$Structure$asPFloat(xNew)));
		return A3(
			$author$project$Structure$updatePropertyByPath,
			eRootTemp,
			drag.path,
			_Utils_Tuple2(
				$author$project$Editor$roleY,
				$author$project$Structure$asPFloat(yNew)));
	});
var $author$project$Editor$tickGraphSimulations = function (editorModel) {
	var mbCellGraph = $elm$core$List$head(
		A2(
			$author$project$Structure$nodesOf,
			$author$project$Editor$ContentCell($author$project$Editor$GraphCell),
			editorModel.eRoot));
	if (mbCellGraph.$ === 'Nothing') {
		return $author$project$Editor$noUpdate(editorModel);
	} else {
		var cellGraph = mbCellGraph.a;
		var _v1 = editorModel.mbSimulation;
		if (_v1.$ === 'Nothing') {
			var vertices = A2(
				$author$project$Structure$nodesOf,
				$author$project$Editor$ContentCell($author$project$Editor$VertexCell),
				cellGraph);
			var forces = _List_fromArray(
				[
					A2(
					$gampleman$elm_visualization$Force$customLinks,
					1,
					$author$project$Editor$customEdgeForcesFromGraph(cellGraph)),
					A2(
					$gampleman$elm_visualization$Force$manyBodyStrength,
					-500,
					A2(
						$elm$core$List$map,
						function (v) {
							return $author$project$Structure$pathAsIdFromNode(v);
						},
						vertices)),
					A2($gampleman$elm_visualization$Force$center, 400, 300)
				]);
			return _Utils_Tuple2(
				_Utils_update(
					editorModel,
					{
						mbSimulation: $elm$core$Maybe$Just(
							$gampleman$elm_visualization$Force$simulation(forces)),
						runXform: false
					}),
				$elm$core$Platform$Cmd$none);
		} else {
			var simulation = _v1.a;
			var vertices = A2(
				$author$project$Structure$nodesOf,
				$author$project$Editor$ContentCell($author$project$Editor$VertexCell),
				cellGraph);
			var pathToGraph = $author$project$Structure$pathOf(cellGraph);
			var entities = A2(
				$elm$core$List$indexedMap,
				F2(
					function (i, v) {
						return A2($author$project$Editor$forceEntityFromVertex, i, v);
					}),
				vertices);
			var edges = A2(
				$author$project$Structure$nodesOf,
				$author$project$Editor$ContentCell($author$project$Editor$EdgeCell),
				cellGraph);
			var addPosToCell = function (e) {
				return A3(
					$author$project$Structure$addFloat,
					$author$project$Editor$roleY,
					e.y,
					A3($author$project$Structure$addFloat, $author$project$Editor$roleX, e.x, e.value));
			};
			var _v2 = A2($gampleman$elm_visualization$Force$tick, simulation, entities);
			var newSimulationState = _v2.a;
			var entitiesNew = _v2.b;
			var childrenNew = _Utils_ap(
				A2($elm$core$List$map, addPosToCell, entitiesNew),
				edges);
			var cellGraphNew = A3($author$project$Structure$replaceUnderFeature, $author$project$Editor$roleDefault, childrenNew, cellGraph);
			var eRootNew = A3($author$project$Structure$replaceChildAtPath, cellGraphNew, pathToGraph, editorModel.eRoot);
			var eRootWithDrag = function () {
				var _v3 = editorModel.drag;
				if (_v3.$ === 'Nothing') {
					return eRootNew;
				} else {
					var drag = _v3.a;
					return A3($author$project$Editor$updateDrag, eRootNew, drag, editorModel.mousePos);
				}
			}();
			return _Utils_Tuple2(
				_Utils_update(
					editorModel,
					{
						eRoot: eRootWithDrag,
						mbSimulation: $elm$core$Maybe$Just(newSimulationState),
						runXform: false
					}),
				$elm$core$Platform$Cmd$none);
		}
	}
};
var $author$project$Structure$splitLastPathSegment = function (_v0) {
	var segments = _v0.a;
	var tailReversed = function (t) {
		return $author$project$Structure$Path(
			$elm$core$List$reverse(t));
	};
	var reversed = $elm$core$List$reverse(segments);
	if (!reversed.b) {
		return _Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
	} else {
		if (!reversed.b.b) {
			var head = reversed.a;
			return _Utils_Tuple2(
				$elm$core$Maybe$Just(head),
				$elm$core$Maybe$Nothing);
		} else {
			var head = reversed.a;
			var tail = reversed.b;
			return _Utils_Tuple2(
				$elm$core$Maybe$Just(head),
				$elm$core$Maybe$Just(
					tailReversed(tail)));
		}
	}
};
var $author$project$Structure$sibling = F3(
	function (root, path, op) {
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		var split = $author$project$Structure$splitLastPathSegment(
			$author$project$Structure$Path(segmentsNoRoot));
		if (split.a.$ === 'Nothing') {
			var _v2 = split.a;
			return $elm$core$Maybe$Nothing;
		} else {
			if (split.b.$ === 'Nothing') {
				var _v3 = split.b;
				return $elm$core$Maybe$Nothing;
			} else {
				var last = split.a.a;
				var parentSegments = split.b.a.a;
				var lastNew = {
					index: A2(op, last.index, 1),
					role: last.role
				};
				return A2(
					$author$project$Structure$nodeAtI,
					root,
					_Utils_ap(
						parentSegments,
						_List_fromArray(
							[lastNew])));
			}
		}
	});
var $author$project$Structure$previousSibling = F2(
	function (root, path) {
		return A3($author$project$Structure$sibling, root, path, $elm$core$Basics$sub);
	});
var $author$project$Structure$deleteNodeAt = F2(
	function (segment, parent) {
		var mbChildAt = function (child) {
			var _v0 = $author$project$Structure$pathOf(child);
			var pathSegmentsChild = _v0.a;
			var mbLastSegment = $elm$core$List$head(
				$elm$core$List$reverse(pathSegmentsChild));
			if (mbLastSegment.$ === 'Nothing') {
				return $elm$core$Maybe$Just(child);
			} else {
				var lastSegment = mbLastSegment.a;
				return _Utils_eq(lastSegment.index, segment.index) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(child);
			}
		};
		var _delete = function (children) {
			return A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				A2($elm$core$List$map, mbChildAt, children));
		};
		var childrenNew = _delete(
			A2($author$project$Structure$getUnder, segment.role, parent));
		return A3($author$project$Structure$replaceUnderFeature, segment.role, childrenNew, parent);
	});
var $author$project$Structure$deleteNodeNested = F3(
	function (segment, tailSegments, parent) {
		var deleteRec = function (child) {
			var _v1 = $author$project$Structure$pathOf(child);
			var pathSegmentsChild = _v1.a;
			var mbLastSegment = $elm$core$List$head(
				$elm$core$List$reverse(pathSegmentsChild));
			if (mbLastSegment.$ === 'Nothing') {
				return child;
			} else {
				var lastSegment = mbLastSegment.a;
				return _Utils_eq(lastSegment.index, segment.index) ? A2($author$project$Structure$deleteNodeRec, tailSegments, child) : child;
			}
		};
		var childrenNew = A2(
			$elm$core$List$map,
			deleteRec,
			A2($author$project$Structure$getUnder, segment.role, parent));
		return A3($author$project$Structure$replaceUnderFeature, segment.role, childrenNew, parent);
	});
var $author$project$Structure$deleteNodeRec = F2(
	function (segments, parent) {
		if (segments.b) {
			if (!segments.b.b) {
				var segment = segments.a;
				return A2($author$project$Structure$deleteNodeAt, segment, parent);
			} else {
				var segment = segments.a;
				var tail = segments.b;
				return A3($author$project$Structure$deleteNodeNested, segment, tail, parent);
			}
		} else {
			return parent;
		}
	});
var $author$project$Structure$deleteNodeUnder = F2(
	function (path, root) {
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		return A2($author$project$Structure$deleteNodeRec, segmentsNoRoot, root);
	});
var $author$project$Editor$tryDelete = F5(
	function (editorModel, _v0, navFun, textLength, isAtDeletePos) {
		var path = _v0.path;
		if (!textLength) {
			return _Utils_Tuple2(
				_Utils_update(
					editorModel,
					{
						dRoot: $author$project$Structure$updatePaths(
							A2($author$project$Structure$deleteNodeUnder, path, editorModel.dRoot)),
						runXform: true
					}),
				$elm$core$Platform$Cmd$none);
		} else {
			if (isAtDeletePos) {
				var mbNext = A2(navFun, editorModel.dRoot, path);
				if (mbNext.$ === 'Nothing') {
					return $author$project$Editor$noUpdate(editorModel);
				} else {
					var next = mbNext.a;
					return _Utils_Tuple2(
						_Utils_update(
							editorModel,
							{
								dRoot: $author$project$Structure$updatePaths(
									A2(
										$author$project$Structure$deleteNodeUnder,
										$author$project$Structure$pathOf(next),
										editorModel.dRoot)),
								runXform: true
							}),
						$elm$core$Platform$Cmd$none);
				}
			} else {
				return $author$project$Editor$noUpdate(editorModel);
			}
		}
	});
var $author$project$Editor$updateOnBackspaceEffect = F3(
	function (editorModel, effect, cellContext) {
		if (effect.$ === 'DeletionEffect') {
			var effectData = effect.a;
			var selection = effectData.selection;
			var textLength = $elm$core$String$length(
				A2($author$project$Structure$textOf, $author$project$Editor$roleInput, cellContext));
			var isAtDeletePos = !selection.start;
			return A5($author$project$Editor$tryDelete, editorModel, effectData, $author$project$Structure$previousSibling, textLength, isAtDeletePos);
		} else {
			return $author$project$Editor$noUpdate(editorModel);
		}
	});
var $author$project$Structure$replaceChildrenForRangeReplace = F6(
	function (role, rangeNew, pathAt, segment, tailSegments, parent) {
		var replaceRangeAt = function (child) {
			var _v1 = $author$project$Structure$pathOf(child);
			var pathSegmentsChild = _v1.a;
			var mbLastSegment = $elm$core$List$head(
				$elm$core$List$reverse(pathSegmentsChild));
			if (mbLastSegment.$ === 'Nothing') {
				return child;
			} else {
				var lastSegment = mbLastSegment.a;
				return _Utils_eq(lastSegment.index, segment.index) ? A5($author$project$Structure$replaceRangeAtPathRec, role, rangeNew, pathAt, tailSegments, child) : child;
			}
		};
		var childrenNew = A2(
			$elm$core$List$map,
			replaceRangeAt,
			A2($author$project$Structure$getUnder, segment.role, parent));
		return A3($author$project$Structure$replaceUnderFeature, segment.role, childrenNew, parent);
	});
var $author$project$Structure$replaceRangeAtPathRec = F5(
	function (role, rangeNew, pathAt, segments, parent) {
		if (!segments.b) {
			return A3($author$project$Structure$replaceUnderFeature, role, rangeNew, parent);
		} else {
			var segment = segments.a;
			var tail = segments.b;
			return A6($author$project$Structure$replaceChildrenForRangeReplace, role, rangeNew, pathAt, segment, tail, parent);
		}
	});
var $author$project$Structure$replaceRangeAtPath = F4(
	function (role, rangeNew, path, root) {
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		return A5($author$project$Structure$replaceRangeAtPathRec, role, rangeNew, path, segmentsNoRoot, root);
	});
var $author$project$Editor$roleName = $author$project$Structure$roleFromString('name');
var $author$project$Editor$roleScope = $author$project$Structure$roleFromString('scope');
var $author$project$Editor$roleScopeValue = $author$project$Structure$roleFromString('scopeValue');
var $author$project$Editor$setScopeInformation = F2(
	function (domainModel, scopeData) {
		var optionNodes = A2(
			$elm$core$List$map,
			function (s) {
				return A3(
					$author$project$Structure$addText,
					$author$project$Editor$roleScopeValue,
					A2($author$project$Structure$textOf, $author$project$Editor$roleName, s),
					$author$project$Structure$createNode(scopeData.isa));
			},
			A2($author$project$Structure$nodesOf, scopeData.isa, domainModel));
		return A4($author$project$Structure$replaceRangeAtPath, $author$project$Editor$roleScope, optionNodes, scopeData.pathContextNode, domainModel);
	});
var $author$project$Editor$updateOnCreateScopeEffect = F2(
	function (editorModel, effect) {
		if (effect.$ === 'CreateScopeEffect') {
			var scopeData = effect.a;
			return _Utils_Tuple2(
				_Utils_update(
					editorModel,
					{
						dRoot: A2($author$project$Editor$setScopeInformation, editorModel.dRoot, scopeData),
						runXform: true
					}),
				$elm$core$Platform$Cmd$none);
		} else {
			return $author$project$Editor$noUpdate(editorModel);
		}
	});
var $author$project$Structure$nextSibling = F2(
	function (root, path) {
		return A3($author$project$Structure$sibling, root, path, $elm$core$Basics$add);
	});
var $author$project$Editor$updateOnDeleteEffect = F3(
	function (editorModel, effect, cellContext) {
		if (effect.$ === 'DeletionEffect') {
			var effectData = effect.a;
			var selection = effectData.selection;
			var textLength = $elm$core$String$length(
				A2($author$project$Structure$textOf, $author$project$Editor$roleInput, cellContext));
			var isAtDeletePos = _Utils_eq(selection.end, textLength);
			return A5($author$project$Editor$tryDelete, editorModel, effectData, $author$project$Structure$nextSibling, textLength, isAtDeletePos);
		} else {
			return $author$project$Editor$noUpdate(editorModel);
		}
	});
var $author$project$Structure$asPString = function (s) {
	return $author$project$Structure$PString(s);
};
var $author$project$Editor$updateOnInputEffect = F3(
	function (editorModel, effect, value) {
		if (effect.$ === 'InputEffect') {
			var path = effect.a.path;
			var role = effect.a.role;
			return _Utils_Tuple2(
				_Utils_update(
					editorModel,
					{
						dRoot: $author$project$Structure$updatePaths(
							A3(
								$author$project$Structure$updatePropertyByPath,
								editorModel.dRoot,
								path,
								_Utils_Tuple2(
									role,
									$author$project$Structure$asPString(value)))),
						runXform: true
					}),
				$elm$core$Platform$Cmd$none);
		} else {
			return $author$project$Editor$noUpdate(editorModel);
		}
	});
var $author$project$Structure$addChildAtPathRec = F4(
	function (role, nodeNew, segments, parent) {
		if (!segments.b) {
			return _Utils_eq(role, $author$project$Structure$roleDefault) ? A2($author$project$Structure$addToDefault, nodeNew, parent) : A3($author$project$Structure$addToCustom, role, nodeNew, parent);
		} else {
			var segment = segments.a;
			var tail = segments.b;
			return A5($author$project$Structure$addChildrenAtPathRec, role, nodeNew, segment, tail, parent);
		}
	});
var $author$project$Structure$addChildrenAtPathRec = F5(
	function (role, nodeNew, segment, tailSegments, parent) {
		var insertAt = function (child) {
			var _v0 = $author$project$Structure$pathOf(child);
			var pathSegmentsChild = _v0.a;
			var mbLastSegment = $elm$core$List$head(
				$elm$core$List$reverse(pathSegmentsChild));
			if (mbLastSegment.$ === 'Nothing') {
				return child;
			} else {
				var lastSegment = mbLastSegment.a;
				return _Utils_eq(lastSegment.index, segment.index) ? A4($author$project$Structure$addChildAtPathRec, role, nodeNew, tailSegments, child) : child;
			}
		};
		var childrenNew = A2(
			$elm$core$List$map,
			insertAt,
			A2($author$project$Structure$getUnder, segment.role, parent));
		return A3($author$project$Structure$replaceUnderFeature, segment.role, childrenNew, parent);
	});
var $author$project$Structure$addChildAtPath = F4(
	function (role, nodeNew, path, root) {
		var roleToAdd = _Utils_eq(role, $author$project$Structure$roleEmpty) ? $author$project$Structure$roleDefault : role;
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		return A4($author$project$Structure$addChildAtPathRec, roleToAdd, nodeNew, segmentsNoRoot, root);
	});
var $author$project$Structure$insertAfter = F4(
	function (pathAfter, child, candidate, result) {
		return _Utils_eq(
			pathAfter,
			$author$project$Structure$pathOf(candidate)) ? _Utils_ap(
			result,
			_List_fromArray(
				[candidate, child])) : _Utils_ap(
			result,
			_List_fromArray(
				[candidate]));
	});
var $author$project$Structure$insertAfterUnderCustom = F4(
	function (role, child, pathAfter, _v0) {
		var data = _v0.a;
		var features = data.features;
		var appender = F2(
			function (child2, children) {
				return A3(
					$elm$core$List$foldl,
					A2($author$project$Structure$insertAfter, pathAfter, child2),
					_List_Nil,
					children);
			});
		var featuresNew = _Utils_update(
			features,
			{
				custom: A4($author$project$Structure$updateCustomFeature, role, child, appender, features.custom)
			});
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{features: featuresNew}));
	});
var $author$project$Structure$insertAfterUnderDefault = F3(
	function (child, pathAfter, _v0) {
		var data = _v0.a;
		var features = data.features;
		var featuresNew = function () {
			var _v1 = features._default;
			if (!_v1.b) {
				return _Utils_update(
					features,
					{
						_default: _List_fromArray(
							[child])
					});
			} else {
				var children = _v1;
				return _Utils_update(
					features,
					{
						_default: A3(
							$elm$core$List$foldl,
							A2($author$project$Structure$insertAfter, pathAfter, child),
							_List_Nil,
							children)
					});
			}
		}();
		return $author$project$Structure$Node(
			_Utils_update(
				data,
				{features: featuresNew}));
	});
var $author$project$Structure$insertChildAfterPathRec = F4(
	function (nodeNew, pathAfter, segments, parent) {
		if (segments.b) {
			if (!segments.b.b) {
				var role = segments.a.role;
				return _Utils_eq(role, $author$project$Structure$roleDefault) ? A3($author$project$Structure$insertAfterUnderDefault, nodeNew, pathAfter, parent) : A4($author$project$Structure$insertAfterUnderCustom, role, nodeNew, pathAfter, parent);
			} else {
				var segment = segments.a;
				var tail = segments.b;
				return A5($author$project$Structure$insertChildren, nodeNew, pathAfter, segment, tail, parent);
			}
		} else {
			return parent;
		}
	});
var $author$project$Structure$insertChildren = F5(
	function (nodeNew, pathAfter, segment, tailSegments, parent) {
		var insertAt = function (child) {
			var _v0 = $author$project$Structure$pathOf(child);
			var pathSegmentsChild = _v0.a;
			var mbLastSegment = $elm$core$List$head(
				$elm$core$List$reverse(pathSegmentsChild));
			if (mbLastSegment.$ === 'Nothing') {
				return child;
			} else {
				var lastSegment = mbLastSegment.a;
				return _Utils_eq(lastSegment.index, segment.index) ? A4($author$project$Structure$insertChildAfterPathRec, nodeNew, pathAfter, tailSegments, child) : child;
			}
		};
		var childrenNew = A2(
			$elm$core$List$map,
			insertAt,
			A2($author$project$Structure$getUnder, segment.role, parent));
		return A3($author$project$Structure$replaceUnderFeature, segment.role, childrenNew, parent);
	});
var $author$project$Structure$insertChildAfterPath = F3(
	function (nodeNew, path, root) {
		var _v0 = $author$project$Structure$dropRootSegment(path);
		var segmentsNoRoot = _v0.a;
		return A4($author$project$Structure$insertChildAfterPathRec, nodeNew, path, segmentsNoRoot, root);
	});
var $author$project$Editor$D = {$: 'D'};
var $author$project$Editor$NavSelection = function (a) {
	return {$: 'NavSelection', a: a};
};
var $author$project$Editor$NavSelectionEffect = function (a) {
	return {$: 'NavSelectionEffect', a: a};
};
var $author$project$Editor$NavSelectionEffectData = F3(
	function (dir, pathSelectedCell, selection) {
		return {dir: dir, pathSelectedCell: pathSelectedCell, selection: selection};
	});
var $author$project$Editor$Selection = F3(
	function (start, end, dir) {
		return {dir: dir, end: end, start: start};
	});
var $author$project$Editor$updateSelectionOnEnter = function (cellContext) {
	return A2(
		$elm$core$Task$perform,
		$author$project$Editor$NavSelection,
		$elm$core$Task$succeed(
			$author$project$Editor$NavSelectionEffect(
				A3(
					$author$project$Editor$NavSelectionEffectData,
					$author$project$Editor$D,
					$author$project$Structure$pathOf(cellContext),
					A3($author$project$Editor$Selection, 0, 0, '')))));
};
var $author$project$Editor$updateOnInsertionEffect = F3(
	function (editorModel, effect, cellContext) {
		if (effect.$ === 'InsertionEffect') {
			var path = effect.a.path;
			var nodeToInsert = effect.a.nodeToInsert;
			var isReplace = effect.a.isReplace;
			var role = effect.a.role;
			if (isReplace) {
				var dRootNew = $author$project$Structure$updatePaths(
					A4($author$project$Structure$addChildAtPath, role, nodeToInsert, path, editorModel.dRoot));
				return _Utils_Tuple2(
					_Utils_update(
						editorModel,
						{dRoot: dRootNew, runXform: true}),
					$elm$core$Platform$Cmd$none);
			} else {
				var dRootNew = $author$project$Structure$updatePaths(
					A3($author$project$Structure$insertChildAfterPath, nodeToInsert, path, editorModel.dRoot));
				return _Utils_Tuple2(
					_Utils_update(
						editorModel,
						{dRoot: dRootNew, runXform: true}),
					$author$project$Editor$updateSelectionOnEnter(cellContext));
			}
		} else {
			return $author$project$Editor$noUpdate(editorModel);
		}
	});
var $author$project$Editor$Horiz = {$: 'Horiz'};
var $author$project$Editor$Vert = {$: 'Vert'};
var $author$project$Structure$parentOf = F2(
	function (root, path) {
		var _v0 = $author$project$Structure$splitLastPathSegment(
			$author$project$Structure$dropRootSegment(path));
		var mbPathToParent = _v0.b;
		return A2(
			$elm$core$Maybe$andThen,
			$author$project$Structure$nodeAtI(root),
			A2(
				$elm$core$Maybe$map,
				function (_v1) {
					var segments = _v1.a;
					return segments;
				},
				mbPathToParent));
	});
var $author$project$Editor$orientationOf = F2(
	function (root, cell) {
		var _v0 = $author$project$Structure$isaOf(cell);
		if (_v0.$ === 'ContentCell') {
			if (_v0.a.$ === 'StackCell') {
				var _v1 = _v0.a;
				return $elm$core$Maybe$Just(
					function () {
						var bO = A2($author$project$Structure$boolOf, $author$project$Editor$roleIsHoriz, cell);
						return bO ? $author$project$Editor$Horiz : $author$project$Editor$Vert;
					}());
			} else {
				return A2(
					$elm$core$Maybe$andThen,
					$author$project$Editor$orientationOf(root),
					A2(
						$author$project$Structure$parentOf,
						root,
						$author$project$Structure$pathOf(cell)));
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Editor$NoOp = {$: 'NoOp'};
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $author$project$Editor$findFirstInputCellRec = F3(
	function (root, candidates, recFun) {
		findFirstInputCellRec:
		while (true) {
			if (!candidates.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var head = candidates.a;
				var tail = candidates.b;
				var mbFirst = A2(recFun, root, head);
				if (mbFirst.$ === 'Nothing') {
					var $temp$root = root,
						$temp$candidates = tail,
						$temp$recFun = recFun;
					root = $temp$root;
					candidates = $temp$candidates;
					recFun = $temp$recFun;
					continue findFirstInputCellRec;
				} else {
					var first = mbFirst.a;
					return $elm$core$Maybe$Just(first);
				}
			}
		}
	});
var $author$project$Editor$findNextInputCell = F2(
	function (root, current) {
		findNextInputCell:
		while (true) {
			var mbNext = A2(
				$author$project$Structure$nextSibling,
				root,
				$author$project$Structure$pathOf(current));
			if (mbNext.$ === 'Just') {
				var next = mbNext.a;
				return A2(
					$elm$core$Maybe$withDefault,
					current,
					A2($author$project$Editor$findNextInputCellRec, root, next));
			} else {
				var mbParent = A2(
					$author$project$Structure$parentOf,
					root,
					$author$project$Structure$pathOf(current));
				if (mbParent.$ === 'Nothing') {
					return current;
				} else {
					var parent = mbParent.a;
					var $temp$root = root,
						$temp$current = parent;
					root = $temp$root;
					current = $temp$current;
					continue findNextInputCell;
				}
			}
		}
	});
var $author$project$Editor$findNextInputCellRec = F2(
	function (root, next) {
		var _v0 = $author$project$Structure$isaOf(next);
		_v0$2:
		while (true) {
			if (_v0.$ === 'ContentCell') {
				switch (_v0.a.$) {
					case 'InputCell':
						var _v1 = _v0.a;
						return $elm$core$Maybe$Just(next);
					case 'StackCell':
						var _v2 = _v0.a;
						var _v3 = $author$project$Structure$getUnderDefault(next);
						if (!_v3.b) {
							return $elm$core$Maybe$Just(
								A2($author$project$Editor$findNextInputCell, root, next));
						} else {
							var children = _v3;
							var mbFirst = A3($author$project$Editor$findFirstInputCellRec, root, children, $author$project$Editor$findNextInputCellRec);
							if (mbFirst.$ === 'Nothing') {
								return $elm$core$Maybe$Just(
									A2($author$project$Editor$findNextInputCell, root, next));
							} else {
								var first = mbFirst.a;
								return $elm$core$Maybe$Just(first);
							}
						}
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return $elm$core$Maybe$Just(
			A2($author$project$Editor$findNextInputCell, root, next));
	});
var $author$project$Editor$findPrevInputCell = F2(
	function (root, current) {
		findPrevInputCell:
		while (true) {
			var mbPrev = A2(
				$author$project$Structure$previousSibling,
				root,
				$author$project$Structure$pathOf(current));
			if (mbPrev.$ === 'Just') {
				var prev = mbPrev.a;
				return A2(
					$elm$core$Maybe$withDefault,
					current,
					A2($author$project$Editor$findPrevInputCellRec, root, prev));
			} else {
				var mbParent = A2(
					$author$project$Structure$parentOf,
					root,
					$author$project$Structure$pathOf(current));
				if (mbParent.$ === 'Nothing') {
					return current;
				} else {
					var parent = mbParent.a;
					var $temp$root = root,
						$temp$current = parent;
					root = $temp$root;
					current = $temp$current;
					continue findPrevInputCell;
				}
			}
		}
	});
var $author$project$Editor$findPrevInputCellRec = F2(
	function (root, prev) {
		var _v0 = $author$project$Structure$isaOf(prev);
		_v0$2:
		while (true) {
			if (_v0.$ === 'ContentCell') {
				switch (_v0.a.$) {
					case 'InputCell':
						var _v1 = _v0.a;
						return $elm$core$Maybe$Just(prev);
					case 'StackCell':
						var _v2 = _v0.a;
						var _v3 = $author$project$Structure$getUnderDefault(prev);
						if (!_v3.b) {
							return $elm$core$Maybe$Just(
								A2($author$project$Editor$findPrevInputCell, root, prev));
						} else {
							var children = _v3;
							var mbLast = A3(
								$author$project$Editor$findFirstInputCellRec,
								root,
								$elm$core$List$reverse(children),
								$author$project$Editor$findPrevInputCellRec);
							if (mbLast.$ === 'Nothing') {
								return $elm$core$Maybe$Just(
									A2($author$project$Editor$findPrevInputCell, root, prev));
							} else {
								var last = mbLast.a;
								return $elm$core$Maybe$Just(last);
							}
						}
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return $elm$core$Maybe$Just(
			A2($author$project$Editor$findPrevInputCell, root, prev));
	});
var $elm$browser$Browser$Dom$focus = _Browser_call('focus');
var $author$project$Editor$updateSelectionByOrientation = F3(
	function (editorModel, navData, orientation) {
		var mbCellSelected = A2($author$project$Structure$nodeAt, editorModel, navData.pathSelectedCell);
		if (mbCellSelected.$ === 'Nothing') {
			return $elm$core$Platform$Cmd$none;
		} else {
			var cellSelected = mbCellSelected.a;
			var moverTask = function (f) {
				return A2(
					$elm$core$Task$attempt,
					function (_v10) {
						return $author$project$Editor$NoOp;
					},
					$elm$browser$Browser$Dom$focus(
						$author$project$Structure$pathAsIdFromNode(
							A2(f, editorModel, cellSelected))));
			};
			var _v1 = _Utils_Tuple2(navData.dir, orientation);
			_v1$4:
			while (true) {
				if (_v1.b.$ === 'Vert') {
					switch (_v1.a.$) {
						case 'U':
							var _v2 = _v1.a;
							var _v3 = _v1.b;
							return moverTask($author$project$Editor$findPrevInputCell);
						case 'D':
							var _v4 = _v1.a;
							var _v5 = _v1.b;
							return moverTask($author$project$Editor$findNextInputCell);
						default:
							break _v1$4;
					}
				} else {
					switch (_v1.a.$) {
						case 'L':
							var _v6 = _v1.a;
							var _v7 = _v1.b;
							return (!navData.selection.start) ? moverTask($author$project$Editor$findPrevInputCell) : $elm$core$Platform$Cmd$none;
						case 'R':
							var _v8 = _v1.a;
							var _v9 = _v1.b;
							return (_Utils_cmp(
								navData.selection.start,
								$elm$core$String$length(
									A2($author$project$Structure$textOf, $author$project$Editor$roleInput, cellSelected))) > -1) ? moverTask($author$project$Editor$findNextInputCell) : $elm$core$Platform$Cmd$none;
						default:
							break _v1$4;
					}
				}
			}
			return $elm$core$Platform$Cmd$none;
		}
	});
var $author$project$Editor$updateSelection = F2(
	function (editorModel, navData) {
		var mbNodeContext = A2($author$project$Structure$nodeAt, editorModel, navData.pathSelectedCell);
		var mbOrientation = A2(
			$elm$core$Maybe$andThen,
			$author$project$Editor$orientationOf(editorModel),
			mbNodeContext);
		if (mbOrientation.$ === 'Nothing') {
			return $elm$core$Platform$Cmd$none;
		} else {
			var orientation = mbOrientation.a;
			return A3($author$project$Editor$updateSelectionByOrientation, editorModel, navData, orientation);
		}
	});
var $author$project$Editor$updateOnNavEffect = F2(
	function (effect, editorModel) {
		if (effect.$ === 'NavSelectionEffect') {
			var navData = effect.a;
			return A2($author$project$Editor$updateSelection, editorModel, navData);
		} else {
			return $elm$core$Platform$Cmd$none;
		}
	});
var $author$project$Editor$updateEditor = F2(
	function (msg, editorModel) {
		switch (msg.$) {
			case 'Tick':
				return $author$project$Editor$tickGraphSimulations(editorModel);
			case 'DragStart':
				var path = msg.a;
				var mbVertex = A2($author$project$Structure$nodeAt, editorModel.eRoot, path);
				if (mbVertex.$ === 'Nothing') {
					return $author$project$Editor$noUpdate(editorModel);
				} else {
					var vertex = mbVertex.a;
					var vertextPosStart = $author$project$Editor$p2dFromCell(vertex);
					var vertexGrabbed = A3($author$project$Structure$addBool, $author$project$Editor$roleGrabbed, true, vertex);
					var eRootNew = A3(
						$author$project$Structure$replaceChildAtPath,
						vertexGrabbed,
						$author$project$Structure$pathOf(vertex),
						editorModel.eRoot);
					return _Utils_Tuple2(
						_Utils_update(
							editorModel,
							{
								drag: $elm$core$Maybe$Just(
									A3($author$project$Editor$Drag, editorModel.mousePos, vertextPosStart, path)),
								eRoot: eRootNew,
								runXform: false
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'MouseMove':
				var mousePosNew = msg.a;
				var _v2 = editorModel.drag;
				if (_v2.$ === 'Just') {
					var drag = _v2.a;
					var mbSimNew = A2(
						$elm$core$Maybe$andThen,
						function (s) {
							return $elm$core$Maybe$Just(
								$gampleman$elm_visualization$Force$reheat(s));
						},
						editorModel.mbSimulation);
					return _Utils_Tuple2(
						_Utils_update(
							editorModel,
							{
								eRoot: A3($author$project$Editor$updateDrag, editorModel.eRoot, drag, mousePosNew),
								mbSimulation: mbSimNew,
								mousePos: mousePosNew,
								runXform: false
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							editorModel,
							{mousePos: mousePosNew, runSimulation: false, runXform: false}),
						$elm$core$Platform$Cmd$none);
				}
			case 'MouseUp':
				var mousePosNew = msg.a;
				var _v3 = editorModel.drag;
				if (_v3.$ === 'Just') {
					var drag = _v3.a;
					var mbVertex = A2($author$project$Structure$nodeAt, editorModel.eRoot, drag.path);
					if (mbVertex.$ === 'Nothing') {
						return $author$project$Editor$noUpdate(editorModel);
					} else {
						var vertex = mbVertex.a;
						var vertexGrabbed = A3($author$project$Structure$addBool, $author$project$Editor$roleGrabbed, false, vertex);
						var eRootNew = A3(
							$author$project$Structure$replaceChildAtPath,
							vertexGrabbed,
							$author$project$Structure$pathOf(vertex),
							editorModel.eRoot);
						return _Utils_Tuple2(
							_Utils_update(
								editorModel,
								{
									drag: $elm$core$Maybe$Nothing,
									eRoot: A3($author$project$Editor$updateDrag, eRootNew, drag, mousePosNew),
									runXform: false
								}),
							$elm$core$Platform$Cmd$none);
					}
				} else {
					return $author$project$Editor$noUpdate(editorModel);
				}
			case 'MouseEnter':
				var path = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						editorModel,
						{
							eRoot: A3(
								$author$project$Structure$updatePropertyByPath,
								editorModel.eRoot,
								path,
								_Utils_Tuple2(
									$author$project$Editor$roleMouseEnter,
									$author$project$Structure$asPBool(true))),
							runXform: false
						}),
					$elm$core$Platform$Cmd$none);
			case 'MouseLeave':
				var path = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						editorModel,
						{
							eRoot: A3(
								$author$project$Structure$updatePropertyByPath,
								editorModel.eRoot,
								path,
								_Utils_Tuple2(
									$author$project$Editor$roleMouseEnter,
									$author$project$Structure$asPBool(false))),
							runXform: false
						}),
					$elm$core$Platform$Cmd$none);
			case 'NoOp':
				return $author$project$Editor$noUpdate(editorModel);
			case 'Swallow':
				return $author$project$Editor$noUpdate(editorModel);
			case 'OnEnter':
				var effect = msg.a;
				var cellContext = msg.b;
				return A3($author$project$Editor$updateOnInsertionEffect, editorModel, effect, cellContext);
			case 'OnClick':
				var effect = msg.a;
				var cellContext = msg.b;
				return A3($author$project$Editor$updateOnInsertionEffect, editorModel, effect, cellContext);
			case 'OnDelete':
				var effect = msg.a;
				var cellContext = msg.b;
				return A3($author$project$Editor$updateOnDeleteEffect, editorModel, effect, cellContext);
			case 'OnBackspace':
				var effect = msg.a;
				var cellContext = msg.b;
				return A3($author$project$Editor$updateOnBackspaceEffect, editorModel, effect, cellContext);
			case 'OnInput':
				var effect = msg.a;
				var value = msg.b;
				return A3($author$project$Editor$updateOnInputEffect, editorModel, effect, value);
			case 'NavSelection':
				var effect = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						editorModel,
						{runXform: false}),
					A2($author$project$Editor$updateOnNavEffect, effect, editorModel.eRoot));
			default:
				var effect = msg.a;
				return A2($author$project$Editor$updateOnCreateScopeEffect, editorModel, effect);
		}
	});
var $author$project$Runtime$update = F2(
	function (msg, model) {
		var domain = model.domain;
		var editorModel = model.editorModel;
		var updateEditorOnly = function (eMsg) {
			var _v2 = A2($author$project$Editor$updateEditor, eMsg, editorModel);
			var editorModelUpdated = _v2.a;
			var editorCmd = _v2.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{editorModel: editorModelUpdated}),
				A2($elm$core$Platform$Cmd$map, $author$project$Runtime$EditorMsg, editorCmd));
		};
		switch (msg.$) {
			case 'Tick':
				var eMsg = msg.a;
				return updateEditorOnly(eMsg);
			case 'MouseMove':
				var eMsg = msg.a;
				return updateEditorOnly(eMsg);
			case 'MouseUp':
				var eMsg = msg.a;
				return updateEditorOnly(eMsg);
			default:
				var eMsg = msg.a;
				var updateSimul = F2(
					function (graphsDiffer, simul) {
						return ($gampleman$elm_visualization$Force$isCompleted(simul) || graphsDiffer) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(simul);
					});
				var _v1 = A2($author$project$Editor$updateEditor, eMsg, editorModel);
				var editorModelUpdated = _v1.a;
				var editorCmd = _v1.b;
				var modelNew = function () {
					if (editorModelUpdated.runXform) {
						var domainNew = _Utils_update(
							domain,
							{root: editorModelUpdated.dRoot});
						var rootENew = A2(
							$author$project$Editor$persistVertexPositions,
							editorModelUpdated.eRoot,
							$author$project$Runtime$runDomainXform(domainNew));
						var graphsDiffer = !A2($author$project$Editor$graphComparer, editorModel.eRoot, rootENew);
						var mbSimulNew = A2(
							$elm$core$Maybe$andThen,
							updateSimul(graphsDiffer),
							editorModelUpdated.mbSimulation);
						var editorModelNew = _Utils_update(
							editorModelUpdated,
							{eRoot: rootENew, mbSimulation: mbSimulNew, runSimulation: graphsDiffer});
						return _Utils_update(
							model,
							{domain: domainNew, editorModel: editorModelNew});
					} else {
						return _Utils_update(
							model,
							{editorModel: editorModelUpdated});
					}
				}();
				return _Utils_Tuple2(
					modelNew,
					A2($elm$core$Platform$Cmd$map, $author$project$Runtime$EditorMsg, editorCmd));
		}
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Editor$divRowAttributes = function (cell) {
	return A2($author$project$Structure$boolOf, $author$project$Editor$roleIsGrid, cell) ? _List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'display', 'table-row')
		]) : _List_Nil;
};
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Structure$tryIntOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$andThen,
			function (prop) {
				if (prop.$ === 'PInt') {
					var v = prop.a;
					return $elm$core$Maybe$Just(v);
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2($author$project$Structure$valueOf, role, node));
	});
var $author$project$Structure$intOf = F2(
	function (role, node) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($author$project$Structure$tryIntOf, role, node));
	});
var $author$project$Editor$roleIndent = $author$project$Structure$roleFromString('indent');
var $author$project$Editor$margins = function (cell) {
	var top = $elm$core$String$fromInt(
		A2($author$project$Structure$intOf, $author$project$Editor$roleMarginTop, cell)) + 'px ';
	var right = $elm$core$String$fromInt(
		A2($author$project$Structure$intOf, $author$project$Editor$roleMarginRight, cell) + 5) + 'px ';
	var indentMarginLeft = A2($author$project$Structure$boolOf, $author$project$Editor$roleIndent, cell) ? 20 : 0;
	var left = $elm$core$String$fromInt(
		A2($author$project$Structure$intOf, $author$project$Editor$roleMarginLeft, cell) + indentMarginLeft) + 'px';
	var bottom = $elm$core$String$fromInt(
		A2($author$project$Structure$intOf, $author$project$Editor$roleMarginBottom, cell)) + 'px ';
	return A2(
		$elm$html$Html$Attributes$style,
		'margin',
		_Utils_ap(
			top,
			_Utils_ap(
				right,
				_Utils_ap(bottom, left))));
};
var $author$project$Editor$paddings = function (_v0) {
	return A2($elm$html$Html$Attributes$style, 'padding', '0px 0px 0px 0px');
};
var $author$project$Editor$marginsAndPaddings = function (cell) {
	return _List_fromArray(
		[
			$author$project$Editor$margins(cell),
			$author$project$Editor$paddings(cell)
		]);
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Editor$OnClick = F2(
	function (a, b) {
		return {$: 'OnClick', a: a, b: b};
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $author$project$Structure$isasUnderCustom = F2(
	function (role, parent) {
		var children = (_Utils_eq(role, $author$project$Structure$roleEmpty) || _Utils_eq(role, $author$project$Structure$roleDefault)) ? $author$project$Structure$getUnderDefault(parent) : A2($author$project$Structure$getUnderCustom, role, parent);
		return A2($elm$core$List$map, $author$project$Structure$isaOf, children);
	});
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Editor$viewButtonCell = function (cell) {
	var _v0 = $author$project$Structure$isaOf(cell);
	if (_v0.$ === 'ContentCell') {
		var onClick = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$andThen,
				function (e) {
					if (e.$ === 'EffectCell') {
						var effect = e.a;
						return $elm$core$Maybe$Just(
							_List_fromArray(
								[
									$elm$html$Html$Events$onClick(
									A2($author$project$Editor$OnClick, effect, cell))
								]));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				},
				$elm$core$List$head(
					A2($author$project$Structure$isasUnderCustom, $author$project$Editor$roleEffects, cell))));
		return A2(
			$elm$html$Html$button,
			_Utils_ap(
				$author$project$Editor$marginsAndPaddings(cell),
				onClick),
			_List_fromArray(
				[
					$elm$html$Html$text(
					A2($author$project$Structure$textOf, $author$project$Editor$roleText, cell))
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$divCellAttributes = function (cell) {
	return A2($author$project$Structure$boolOf, $author$project$Editor$roleIsGrid, cell) ? _List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'display', 'table-cell')
		]) : _List_Nil;
};
var $elm$html$Html$label = _VirtualDom_node('label');
var $author$project$Editor$viewConstantCell = function (cell) {
	var _v0 = $author$project$Structure$isaOf(cell);
	if (_v0.$ === 'ContentCell') {
		return A2(
			$elm$html$Html$div,
			$author$project$Editor$divCellAttributes(cell),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$label,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id(
								$author$project$Structure$pathAsIdFromNode(cell)),
								A2($elm$html$Html$Attributes$style, 'font-weight', 'bold'),
								A2($elm$html$Html$Attributes$style, 'color', 'darkblue')
							]),
						$author$project$Editor$marginsAndPaddings(cell)),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Structure$textOf, $author$project$Editor$roleConstant, cell))
						]))
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $elm$virtual_dom$VirtualDom$nodeNS = function (tag) {
	return _VirtualDom_nodeNS(
		_VirtualDom_noScript(tag));
};
var $elm_community$typed_svg$TypedSvg$Core$node = $elm$virtual_dom$VirtualDom$nodeNS('http://www.w3.org/2000/svg');
var $elm_community$typed_svg$TypedSvg$g = $elm_community$typed_svg$TypedSvg$Core$node('g');
var $elm_community$typed_svg$TypedSvg$svg = $elm_community$typed_svg$TypedSvg$Core$node('svg');
var $elm_community$typed_svg$TypedSvg$Types$Paint = function (a) {
	return {$: 'Paint', a: a};
};
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$scaleFrom255 = function (c) {
	return c / 255;
};
var $avh4$elm_color$Color$rgb255 = F3(
	function (r, g, b) {
		return A4(
			$avh4$elm_color$Color$RgbaSpace,
			$avh4$elm_color$Color$scaleFrom255(r),
			$avh4$elm_color$Color$scaleFrom255(g),
			$avh4$elm_color$Color$scaleFrom255(b),
			1.0);
	});
var $author$project$Editor$colorGraphPrimary = A3($avh4$elm_color$Color$rgb255, 17, 77, 175);
var $ianmackenzie$elm_geometry$Vector2d$length = function (vector) {
	return $elm$core$Basics$sqrt(
		$ianmackenzie$elm_geometry$Vector2d$squaredLength(vector));
};
var $ianmackenzie$elm_geometry$Geometry$Types$Direction2d = function (a) {
	return {$: 'Direction2d', a: a};
};
var $ianmackenzie$elm_geometry$Bootstrap$Direction2d$unsafe = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d;
var $ianmackenzie$elm_geometry$Vector2d$direction = function (vector) {
	if (_Utils_eq(vector, $ianmackenzie$elm_geometry$Vector2d$zero)) {
		return $elm$core$Maybe$Nothing;
	} else {
		var normalizedVector = A2(
			$ianmackenzie$elm_geometry$Vector2d$scaleBy,
			1 / $ianmackenzie$elm_geometry$Vector2d$length(vector),
			vector);
		return $elm$core$Maybe$Just(
			$ianmackenzie$elm_geometry$Bootstrap$Direction2d$unsafe(
				$ianmackenzie$elm_geometry$Vector2d$components(normalizedVector)));
	}
};
var $ianmackenzie$elm_geometry$LineSegment2d$endpoints = function (_v0) {
	var endpoints_ = _v0.a;
	return endpoints_;
};
var $ianmackenzie$elm_geometry$LineSegment2d$vector = function (lineSegment) {
	var _v0 = $ianmackenzie$elm_geometry$LineSegment2d$endpoints(lineSegment);
	var p1 = _v0.a;
	var p2 = _v0.b;
	return A2($ianmackenzie$elm_geometry$Vector2d$from, p1, p2);
};
var $ianmackenzie$elm_geometry$LineSegment2d$direction = A2($elm$core$Basics$composeR, $ianmackenzie$elm_geometry$LineSegment2d$vector, $ianmackenzie$elm_geometry$Vector2d$direction);
var $ianmackenzie$elm_geometry$LineSegment2d$endPoint = function (_v0) {
	var _v1 = _v0.a;
	var end = _v1.b;
	return end;
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm_community$typed_svg$TypedSvg$Core$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$Basics$round = _Basics_round;
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $elm_community$typed_svg$TypedSvg$TypesToStrings$paintToString = function (paint) {
	switch (paint.$) {
		case 'Paint':
			var color = paint.a;
			return $avh4$elm_color$Color$toCssString(color);
		case 'Reference':
			var string = paint.a;
			return $elm$core$String$concat(
				_List_fromArray(
					['url(#', string, ')']));
		case 'ContextFill':
			return 'context-fill';
		case 'ContextStroke':
			return 'context-stroke';
		default:
			return 'none';
	}
};
var $elm_community$typed_svg$TypedSvg$Attributes$fill = A2(
	$elm$core$Basics$composeL,
	$elm_community$typed_svg$TypedSvg$Core$attribute('fill'),
	$elm_community$typed_svg$TypedSvg$TypesToStrings$paintToString);
var $ianmackenzie$elm_geometry$Geometry$Types$LineSegment2d = function (a) {
	return {$: 'LineSegment2d', a: a};
};
var $ianmackenzie$elm_geometry$LineSegment2d$fromEndpoints = $ianmackenzie$elm_geometry$Geometry$Types$LineSegment2d;
var $ianmackenzie$elm_geometry$LineSegment2d$from = F2(
	function (startPoint_, endPoint_) {
		return $ianmackenzie$elm_geometry$LineSegment2d$fromEndpoints(
			_Utils_Tuple2(startPoint_, endPoint_));
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Triangle2d = function (a) {
	return {$: 'Triangle2d', a: a};
};
var $ianmackenzie$elm_geometry$Triangle2d$fromVertices = $ianmackenzie$elm_geometry$Geometry$Types$Triangle2d;
var $ianmackenzie$elm_geometry_svg$Geometry$Svg$coordinatesString = function (point) {
	var _v0 = $ianmackenzie$elm_geometry$Point2d$coordinates(point);
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$String$fromFloat(x) + (',' + $elm$core$String$fromFloat(y));
};
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $ianmackenzie$elm_geometry_svg$Geometry$Svg$pointsAttribute = function (points) {
	return $elm$svg$Svg$Attributes$points(
		A2(
			$elm$core$String$join,
			' ',
			A2($elm$core$List$map, $ianmackenzie$elm_geometry_svg$Geometry$Svg$coordinatesString, points)));
};
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $ianmackenzie$elm_geometry_svg$Geometry$Svg$lineSegment2d = F2(
	function (attributes, lineSegment) {
		var _v0 = $ianmackenzie$elm_geometry$LineSegment2d$endpoints(lineSegment);
		var p1 = _v0.a;
		var p2 = _v0.b;
		return A2(
			$elm$svg$Svg$polyline,
			A2(
				$elm$core$List$cons,
				$ianmackenzie$elm_geometry_svg$Geometry$Svg$pointsAttribute(
					_List_fromArray(
						[p1, p2])),
				attributes),
			_List_Nil);
	});
var $ianmackenzie$elm_geometry$Direction2d$unsafe = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d;
var $ianmackenzie$elm_geometry$Direction2d$positiveX = $ianmackenzie$elm_geometry$Direction2d$unsafe(
	_Utils_Tuple2(1, 0));
var $ianmackenzie$elm_geometry$Triangle2d$vertices = function (_v0) {
	var vertices_ = _v0.a;
	return vertices_;
};
var $ianmackenzie$elm_geometry$Triangle2d$mapVertices = F2(
	function (_function, triangle) {
		var _v0 = $ianmackenzie$elm_geometry$Triangle2d$vertices(triangle);
		var p1 = _v0.a;
		var p2 = _v0.b;
		var p3 = _v0.c;
		return $ianmackenzie$elm_geometry$Triangle2d$fromVertices(
			_Utils_Tuple3(
				_function(p1),
				_function(p2),
				_function(p3)));
	});
var $ianmackenzie$elm_geometry$Point2d$addTo = F2(
	function (point, vector) {
		return A2($ianmackenzie$elm_geometry$Point2d$translateBy, vector, point);
	});
var $ianmackenzie$elm_geometry$Vector2d$rotateBy = function (angle) {
	var sine = $elm$core$Basics$sin(angle);
	var cosine = $elm$core$Basics$cos(angle);
	return function (vector) {
		var _v0 = $ianmackenzie$elm_geometry$Vector2d$components(vector);
		var x = _v0.a;
		var y = _v0.b;
		return $ianmackenzie$elm_geometry$Vector2d$fromComponents(
			_Utils_Tuple2((x * cosine) - (y * sine), (y * cosine) + (x * sine)));
	};
};
var $ianmackenzie$elm_geometry$Point2d$rotateAround = F2(
	function (centerPoint, angle) {
		return A2(
			$elm$core$Basics$composeR,
			$ianmackenzie$elm_geometry$Vector2d$from(centerPoint),
			A2(
				$elm$core$Basics$composeR,
				$ianmackenzie$elm_geometry$Vector2d$rotateBy(angle),
				$ianmackenzie$elm_geometry$Point2d$addTo(centerPoint)));
	});
var $ianmackenzie$elm_geometry$Triangle2d$rotateAround = F2(
	function (centerPoint, angle) {
		return $ianmackenzie$elm_geometry$Triangle2d$mapVertices(
			A2($ianmackenzie$elm_geometry$Point2d$rotateAround, centerPoint, angle));
	});
var $ianmackenzie$elm_geometry$LineSegment2d$startPoint = function (_v0) {
	var _v1 = _v0.a;
	var start = _v1.a;
	return start;
};
var $elm_community$typed_svg$TypedSvg$Attributes$stroke = A2(
	$elm$core$Basics$composeL,
	$elm_community$typed_svg$TypedSvg$Core$attribute('stroke'),
	$elm_community$typed_svg$TypedSvg$TypesToStrings$paintToString);
var $elm_community$typed_svg$TypedSvg$Types$Px = function (a) {
	return {$: 'Px', a: a};
};
var $elm_community$typed_svg$TypedSvg$Types$px = $elm_community$typed_svg$TypedSvg$Types$Px;
var $elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString = function (length) {
	switch (length.$) {
		case 'Cm':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'cm';
		case 'Em':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'em';
		case 'Ex':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'ex';
		case 'In':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'in';
		case 'Mm':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'mm';
		case 'Num':
			var x = length.a;
			return $elm$core$String$fromFloat(x);
		case 'Pc':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'pc';
		case 'Percent':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + '%';
		case 'Pt':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'pt';
		default:
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'px';
	}
};
var $elm_community$typed_svg$TypedSvg$Attributes$strokeWidth = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'stroke-width',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$strokeWidth = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$strokeWidth(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm$core$Basics$atan2 = _Basics_atan2;
var $ianmackenzie$elm_geometry$Bootstrap$Direction2d$components = function (_v0) {
	var components_ = _v0.a;
	return components_;
};
var $ianmackenzie$elm_geometry$Direction2d$components = $ianmackenzie$elm_geometry$Bootstrap$Direction2d$components;
var $ianmackenzie$elm_geometry$Direction2d$toAngle = function (direction) {
	var _v0 = $ianmackenzie$elm_geometry$Direction2d$components(direction);
	var xComponent_ = _v0.a;
	var yComponent_ = _v0.b;
	return A2($elm$core$Basics$atan2, yComponent_, xComponent_);
};
var $ianmackenzie$elm_geometry$Triangle2d$translateBy = function (vector) {
	return $ianmackenzie$elm_geometry$Triangle2d$mapVertices(
		$ianmackenzie$elm_geometry$Point2d$translateBy(vector));
};
var $ianmackenzie$elm_geometry$Point2d$translateIn = F3(
	function (direction, distance, point) {
		var _v0 = $ianmackenzie$elm_geometry$Point2d$coordinates(point);
		var px = _v0.a;
		var py = _v0.b;
		var _v1 = $ianmackenzie$elm_geometry$Direction2d$components(direction);
		var dx = _v1.a;
		var dy = _v1.b;
		return $ianmackenzie$elm_geometry$Point2d$fromCoordinates(
			_Utils_Tuple2(px + (distance * dx), py + (distance * dy)));
	});
var $ianmackenzie$elm_geometry$Vector2d$withLength = F2(
	function (length_, direction_) {
		var _v0 = $ianmackenzie$elm_geometry$Bootstrap$Direction2d$components(direction_);
		var dx = _v0.a;
		var dy = _v0.b;
		return $ianmackenzie$elm_geometry$Vector2d$fromComponents(
			_Utils_Tuple2(length_ * dx, length_ * dy));
	});
var $ianmackenzie$elm_geometry$Triangle2d$translateIn = F3(
	function (direction, distance, triangle) {
		return A2(
			$ianmackenzie$elm_geometry$Triangle2d$translateBy,
			A2($ianmackenzie$elm_geometry$Vector2d$withLength, distance, direction),
			triangle);
	});
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $ianmackenzie$elm_geometry_svg$Geometry$Svg$triangle2d = F2(
	function (attributes, triangle) {
		var _v0 = $ianmackenzie$elm_geometry$Triangle2d$vertices(triangle);
		var p1 = _v0.a;
		var p2 = _v0.b;
		var p3 = _v0.c;
		return A2(
			$elm$svg$Svg$polygon,
			A2(
				$elm$core$List$cons,
				$ianmackenzie$elm_geometry_svg$Geometry$Svg$pointsAttribute(
					_List_fromArray(
						[p1, p2, p3])),
				attributes),
			_List_Nil);
	});
var $author$project$Editor$edgeWithArrowHead = function (lineSegment) {
	var vecFromOriginToEndPoint = $ianmackenzie$elm_geometry$Vector2d$fromComponents(
		$ianmackenzie$elm_geometry$Point2d$coordinates(
			$ianmackenzie$elm_geometry$LineSegment2d$endPoint(lineSegment)));
	var headWidth = 15;
	var headLength = 15;
	var dir = A2(
		$elm$core$Maybe$withDefault,
		$ianmackenzie$elm_geometry$Direction2d$positiveX,
		$ianmackenzie$elm_geometry$LineSegment2d$direction(lineSegment));
	var angle = $ianmackenzie$elm_geometry$Direction2d$toAngle(dir);
	var arrowHead = A3(
		$ianmackenzie$elm_geometry$Triangle2d$translateIn,
		dir,
		-headLength,
		A2(
			$ianmackenzie$elm_geometry$Triangle2d$translateBy,
			vecFromOriginToEndPoint,
			A3(
				$ianmackenzie$elm_geometry$Triangle2d$rotateAround,
				$ianmackenzie$elm_geometry$Point2d$origin,
				angle,
				$ianmackenzie$elm_geometry$Triangle2d$fromVertices(
					_Utils_Tuple3(
						$ianmackenzie$elm_geometry$Point2d$fromCoordinates(
							_Utils_Tuple2(0, (-headWidth) / 2)),
						$ianmackenzie$elm_geometry$Point2d$fromCoordinates(
							_Utils_Tuple2(0, headWidth / 2)),
						$ianmackenzie$elm_geometry$Point2d$fromCoordinates(
							_Utils_Tuple2(headLength, 0)))))));
	return A2(
		$elm_community$typed_svg$TypedSvg$g,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$ianmackenzie$elm_geometry_svg$Geometry$Svg$lineSegment2d,
				_List_fromArray(
					[
						$elm_community$typed_svg$TypedSvg$Attributes$stroke(
						$elm_community$typed_svg$TypedSvg$Types$Paint($author$project$Editor$colorGraphPrimary)),
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$strokeWidth(2)
					]),
				A2(
					$ianmackenzie$elm_geometry$LineSegment2d$from,
					$ianmackenzie$elm_geometry$LineSegment2d$startPoint(lineSegment),
					A3(
						$ianmackenzie$elm_geometry$Point2d$translateIn,
						dir,
						-headLength,
						$ianmackenzie$elm_geometry$LineSegment2d$endPoint(lineSegment)))),
				A2(
				$ianmackenzie$elm_geometry_svg$Geometry$Svg$triangle2d,
				_List_fromArray(
					[
						$elm_community$typed_svg$TypedSvg$Attributes$fill(
						$elm_community$typed_svg$TypedSvg$Types$Paint($author$project$Editor$colorGraphPrimary))
					]),
				arrowHead)
			]));
};
var $ianmackenzie$elm_geometry$Direction2d$from = F2(
	function (firstPoint, secondPoint) {
		return $ianmackenzie$elm_geometry$Vector2d$direction(
			A2($ianmackenzie$elm_geometry$Vector2d$from, firstPoint, secondPoint));
	});
var $author$project$Editor$vertexProperties = function (vertex) {
	var noName = '<no value>';
	var name = A2(
		$elm$core$Maybe$withDefault,
		noName,
		A2($author$project$Structure$tryTextOf, $author$project$Editor$roleText, vertex));
	var nameContent = (name === '') ? noName : name;
	var widthContent = (nameContent === '') ? $elm$core$String$length(noName) : $elm$core$String$length(nameContent);
	var widthVertex = ($elm$core$String$length(nameContent) * 8.797) + 18;
	var heightVertex = 40;
	var halfW = widthVertex / 2;
	var halfH = heightVertex / 2;
	var midTranslation = $ianmackenzie$elm_geometry$Vector2d$fromComponents(
		_Utils_Tuple2(-halfW, -halfH));
	var posVertex = A2(
		$ianmackenzie$elm_geometry$Point2d$translateBy,
		midTranslation,
		$author$project$Editor$p2dFromCell(vertex));
	var posContent = A2(
		$ianmackenzie$elm_geometry$Point2d$translateBy,
		$ianmackenzie$elm_geometry$Vector2d$fromComponents(
			_Utils_Tuple2(9, 9)),
		posVertex);
	var edgeAngle = A2($elm$core$Basics$atan2, halfH, halfW);
	var angleAreas = _List_fromArray(
		[
			_Utils_Tuple2((2 * $elm$core$Basics$pi) - edgeAngle, edgeAngle),
			_Utils_Tuple2(edgeAngle, $elm$core$Basics$pi - edgeAngle),
			_Utils_Tuple2($elm$core$Basics$pi - edgeAngle, $elm$core$Basics$pi + edgeAngle),
			_Utils_Tuple2($elm$core$Basics$pi + edgeAngle, (2 * $elm$core$Basics$pi) - edgeAngle)
		]);
	return {angleAreas: angleAreas, content: nameContent, heightContent: 20, heightVertex: heightVertex, posContent: posContent, posVertex: posVertex, widthContent: widthContent, widthVertex: widthVertex};
};
var $author$project$Editor$vertexAnchorsForEdge = function (_v0) {
	var from = _v0.a;
	var to = _v0.b;
	var tProps = $author$project$Editor$vertexProperties(to);
	var tPos = $author$project$Editor$p2dFromCell(to);
	var fProps = $author$project$Editor$vertexProperties(from);
	var fPos = $author$project$Editor$p2dFromCell(from);
	var dir = A2(
		$elm$core$Maybe$withDefault,
		$ianmackenzie$elm_geometry$Direction2d$positiveX,
		A2($ianmackenzie$elm_geometry$Direction2d$from, fPos, tPos));
	var angle = $ianmackenzie$elm_geometry$Direction2d$toAngle(dir);
	var sectorFromAngle = function (isa) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$head(
				A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (i, _v2) {
								var lowest = _v2.a;
								var highest = _v2.b;
								return ((_Utils_cmp($elm$core$Basics$pi + angle, lowest) > -1) && (_Utils_cmp($elm$core$Basics$pi + angle, highest) < 0)) ? $elm$core$Maybe$Just(i) : $elm$core$Maybe$Nothing;
							}),
						isa))));
	};
	var fSector = sectorFromAngle(fProps.angleAreas);
	var tSector = sectorFromAngle(tProps.angleAreas);
	var translate = F4(
		function (s, pos, props, inv) {
			switch (s) {
				case 0:
					return A3(
						$ianmackenzie$elm_geometry$Point2d$translateIn,
						dir,
						(inv * ((-props.widthVertex) / 2)) / $elm$core$Basics$cos(angle),
						pos);
				case 1:
					return A3(
						$ianmackenzie$elm_geometry$Point2d$translateIn,
						dir,
						(inv * ((-props.heightVertex) / 2)) / $elm$core$Basics$cos(($elm$core$Basics$pi / 2) - angle),
						pos);
				case 2:
					return A3(
						$ianmackenzie$elm_geometry$Point2d$translateIn,
						dir,
						(inv * (props.widthVertex / 2)) / $elm$core$Basics$cos(angle),
						pos);
				case 3:
					return A3(
						$ianmackenzie$elm_geometry$Point2d$translateIn,
						dir,
						(inv * (props.heightVertex / 2)) / $elm$core$Basics$cos(($elm$core$Basics$pi / 2) - angle),
						pos);
				default:
					return pos;
			}
		});
	return _Utils_Tuple2(
		A4(translate, fSector, fPos, fProps, 1),
		A4(translate, tSector, tPos, tProps, -1));
};
var $author$project$Editor$viewEdgeCell = function (fromTo) {
	var _v0 = $author$project$Editor$vertexAnchorsForEdge(fromTo);
	var edgeStart = _v0.a;
	var edgeEnd = _v0.b;
	var edgeLine = A2($ianmackenzie$elm_geometry$LineSegment2d$from, edgeStart, edgeEnd);
	return $author$project$Editor$edgeWithArrowHead(edgeLine);
};
var $author$project$Editor$viewEdgeCells = function (cellGraph) {
	return A2(
		$elm$core$List$map,
		$author$project$Editor$viewEdgeCell,
		$author$project$Editor$fromToPairs(cellGraph));
};
var $elm_community$typed_svg$TypedSvg$Attributes$height = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'height',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$height = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$height(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$rect = $elm_community$typed_svg$TypedSvg$Core$node('rect');
var $elm_community$typed_svg$TypedSvg$Attributes$rx = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'rx',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$rx = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$rx(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$ry = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'ry',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$ry = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$ry(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm_community$typed_svg$TypedSvg$Core$foreignObject = $elm_community$typed_svg$TypedSvg$Core$node('foreignObject');
var $elm$html$Html$form = _VirtualDom_node('form');
var $elm$html$Html$input = _VirtualDom_node('input');
var $author$project$Editor$OnInput = F2(
	function (a, b) {
		return {$: 'OnInput', a: a, b: b};
	});
var $author$project$Editor$UpdateScope = function (a) {
	return {$: 'UpdateScope', a: a};
};
var $author$project$Editor$DeletionEffect = function (a) {
	return {$: 'DeletionEffect', a: a};
};
var $author$project$Editor$OnBackspace = F2(
	function (a, b) {
		return {$: 'OnBackspace', a: a, b: b};
	});
var $author$project$Editor$OnDelete = F2(
	function (a, b) {
		return {$: 'OnDelete', a: a, b: b};
	});
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Editor$decodeSelection = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (s, e, d) {
			return {dir: d, end: e, start: s};
		}),
	A2($elm$json$Json$Decode$field, 'selectionStart', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'selectionEnd', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'selectionDirection', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Editor$effectAttributeFromKey = function (dictKeyToMsg) {
	var canHandle = function (k) {
		var mbMsg = A2($elm$core$Dict$get, k, dictKeyToMsg);
		if (mbMsg.$ === 'Nothing') {
			return $elm$json$Json$Decode$fail('incorrect code: ' + k);
		} else {
			var msg = mbMsg.a;
			switch (msg.$) {
				case 'NavSelection':
					var effect = msg.a;
					if (effect.$ === 'NavSelectionEffect') {
						var navData = effect.a;
						return A2(
							$elm$json$Json$Decode$map,
							function (sel) {
								return $author$project$Editor$NavSelection(
									$author$project$Editor$NavSelectionEffect(
										_Utils_update(
											navData,
											{selection: sel})));
							},
							A2($elm$json$Json$Decode$field, 'target', $author$project$Editor$decodeSelection));
					} else {
						return $elm$json$Json$Decode$succeed(msg);
					}
				case 'OnDelete':
					var effect = msg.a;
					var cellContext = msg.b;
					if (effect.$ === 'DeletionEffect') {
						var effectData = effect.a;
						return A2(
							$elm$json$Json$Decode$map,
							function (sel) {
								return A2(
									$author$project$Editor$OnDelete,
									$author$project$Editor$DeletionEffect(
										_Utils_update(
											effectData,
											{selection: sel})),
									cellContext);
							},
							A2($elm$json$Json$Decode$field, 'target', $author$project$Editor$decodeSelection));
					} else {
						return $elm$json$Json$Decode$succeed(msg);
					}
				case 'OnBackspace':
					var effect = msg.a;
					var cellContext = msg.b;
					if (effect.$ === 'DeletionEffect') {
						var effectData = effect.a;
						return A2(
							$elm$json$Json$Decode$map,
							function (sel) {
								return A2(
									$author$project$Editor$OnBackspace,
									$author$project$Editor$DeletionEffect(
										_Utils_update(
											effectData,
											{selection: sel})),
									cellContext);
							},
							A2($elm$json$Json$Decode$field, 'target', $author$project$Editor$decodeSelection));
					} else {
						return $elm$json$Json$Decode$succeed(msg);
					}
				default:
					return $elm$json$Json$Decode$succeed(msg);
			}
		}
	};
	return A2(
		$elm$html$Html$Events$on,
		'keydown',
		A2(
			$elm$json$Json$Decode$andThen,
			canHandle,
			A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)));
};
var $author$project$Editor$OnEnter = F2(
	function (a, b) {
		return {$: 'OnEnter', a: a, b: b};
	});
var $author$project$Editor$keyFromDir = function (dir) {
	switch (dir.$) {
		case 'U':
			return 'ArrowUp';
		case 'D':
			return 'ArrowDown';
		case 'L':
			return 'ArrowLeft';
		default:
			return 'ArrowRight';
	}
};
var $author$project$Editor$inputEffectMap = F2(
	function (cell, effects) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (effect, dict) {
					switch (effect.$) {
						case 'InsertionEffect':
							return A3(
								$elm$core$Dict$insert,
								'Enter',
								A2($author$project$Editor$OnEnter, effect, cell),
								dict);
						case 'DeletionEffect':
							return A3(
								$elm$core$Dict$insert,
								'Delete',
								A2($author$project$Editor$OnDelete, effect, cell),
								A3(
									$elm$core$Dict$insert,
									'Backspace',
									A2($author$project$Editor$OnBackspace, effect, cell),
									dict));
						case 'NavSelectionEffect':
							var dir = effect.a.dir;
							return A3(
								$elm$core$Dict$insert,
								$author$project$Editor$keyFromDir(dir),
								$author$project$Editor$NavSelection(effect),
								dict);
						case 'InputEffect':
							return dict;
						default:
							return dict;
					}
				}),
			$elm$core$Dict$empty,
			effects);
	});
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $author$project$Editor$attributeFromEffectGroup = F2(
	function (cell, effectGroup) {
		switch (effectGroup.$) {
			case 'InputEffectGroup':
				var effects = effectGroup.a;
				if (effects.b && (!effects.b.b)) {
					var effect = effects.a;
					return $elm$core$Maybe$Just(
						$elm$html$Html$Events$onInput(
							$author$project$Editor$OnInput(effect)));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			case 'KeyboardEffectGroup':
				var effects = effectGroup.a;
				return $elm$core$Maybe$Just(
					$author$project$Editor$effectAttributeFromKey(
						A2($author$project$Editor$inputEffectMap, cell, effects)));
			default:
				var effects = effectGroup.a;
				if (effects.b && (!effects.b.b)) {
					var effect = effects.a;
					return $elm$core$Maybe$Just(
						$elm$html$Html$Events$onFocus(
							$author$project$Editor$UpdateScope(effect)));
				} else {
					return $elm$core$Maybe$Nothing;
				}
		}
	});
var $author$project$Editor$FocusEffectGroup = function (a) {
	return {$: 'FocusEffectGroup', a: a};
};
var $author$project$Editor$InputEffectGroup = function (a) {
	return {$: 'InputEffectGroup', a: a};
};
var $author$project$Editor$KeyboardEffectGroup = function (a) {
	return {$: 'KeyboardEffectGroup', a: a};
};
var $author$project$Editor$grouped = function (effectCells) {
	var updateGroup = F2(
		function (effect, mbEffectList) {
			if (mbEffectList.$ === 'Nothing') {
				return $elm$core$Maybe$Just(
					_List_fromArray(
						[effect]));
			} else {
				var effectList = mbEffectList.a;
				return $elm$core$Maybe$Just(
					A2($elm$core$List$cons, effect, effectList));
			}
		});
	var toEffectGroupList = F3(
		function (k, v, effectGroupList) {
			switch (k) {
				case 'input':
					return A2(
						$elm$core$List$cons,
						$author$project$Editor$InputEffectGroup(v),
						effectGroupList);
				case 'keyboard':
					return A2(
						$elm$core$List$cons,
						$author$project$Editor$KeyboardEffectGroup(v),
						effectGroupList);
				case 'focus':
					return A2(
						$elm$core$List$cons,
						$author$project$Editor$FocusEffectGroup(v),
						effectGroupList);
				default:
					return effectGroupList;
			}
		});
	var toDict = F2(
		function (effectCell, groupDict) {
			if (effectCell.$ === 'ContentCell') {
				return groupDict;
			} else {
				var effect = effectCell.a;
				switch (effect.$) {
					case 'InputEffect':
						return A3(
							$elm$core$Dict$update,
							'input',
							updateGroup(effect),
							groupDict);
					case 'InsertionEffect':
						return A3(
							$elm$core$Dict$update,
							'keyboard',
							updateGroup(effect),
							groupDict);
					case 'DeletionEffect':
						return A3(
							$elm$core$Dict$update,
							'keyboard',
							updateGroup(effect),
							groupDict);
					case 'NavSelectionEffect':
						return A3(
							$elm$core$Dict$update,
							'keyboard',
							updateGroup(effect),
							groupDict);
					default:
						return A3(
							$elm$core$Dict$update,
							'focus',
							updateGroup(effect),
							groupDict);
				}
			}
		});
	var dictGrouped = A3($elm$core$List$foldl, toDict, $elm$core$Dict$empty, effectCells);
	return A3($elm$core$Dict$foldl, toEffectGroupList, _List_Nil, dictGrouped);
};
var $author$project$Editor$L = {$: 'L'};
var $author$project$Editor$R = {$: 'R'};
var $author$project$Editor$U = {$: 'U'};
var $author$project$Editor$emptySelection = {dir: '', end: -1, start: -1};
var $author$project$Editor$navEffect = F2(
	function (dir, path) {
		return $author$project$Editor$EffectCell(
			$author$project$Editor$NavSelectionEffect(
				{dir: dir, pathSelectedCell: path, selection: $author$project$Editor$emptySelection}));
	});
var $author$project$Editor$navEffects = function (path) {
	return _List_fromArray(
		[
			A2($author$project$Editor$navEffect, $author$project$Editor$U, path),
			A2($author$project$Editor$navEffect, $author$project$Editor$D, path),
			A2($author$project$Editor$navEffect, $author$project$Editor$L, path),
			A2($author$project$Editor$navEffect, $author$project$Editor$R, path)
		]);
};
var $author$project$Editor$inputCellAttributesFromEffects = function (cell) {
	var effectGroups = $author$project$Editor$grouped(
		_Utils_ap(
			A2($author$project$Structure$isasUnderCustom, $author$project$Editor$roleEffects, cell),
			$author$project$Editor$navEffects(
				$author$project$Structure$pathOf(cell))));
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2(
			$elm$core$List$map,
			$author$project$Editor$attributeFromEffectGroup(cell),
			effectGroups));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$size = function (n) {
	return A2(
		_VirtualDom_attribute,
		'size',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $elm_community$typed_svg$TypedSvg$Attributes$width = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'width',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$width = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$width(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$x = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'x',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$x = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$x(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $ianmackenzie$elm_geometry$Point2d$xCoordinate = function (_v0) {
	var _v1 = _v0.a;
	var x = _v1.a;
	return x;
};
var $elm_community$typed_svg$TypedSvg$Attributes$y = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'y',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$y = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$y(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $ianmackenzie$elm_geometry$Point2d$yCoordinate = function (_v0) {
	var _v1 = _v0.a;
	var y = _v1.b;
	return y;
};
var $author$project$Editor$vertexContent = F2(
	function (cell, _v0) {
		var posContent = _v0.posContent;
		var widthVertex = _v0.widthVertex;
		var widthContent = _v0.widthContent;
		var heightContent = _v0.heightContent;
		var content = _v0.content;
		return A2(
			$elm_community$typed_svg$TypedSvg$Core$foreignObject,
			_List_fromArray(
				[
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$x(
					$ianmackenzie$elm_geometry$Point2d$xCoordinate(posContent)),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$y(
					$ianmackenzie$elm_geometry$Point2d$yCoordinate(posContent)),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$width(widthVertex),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$height(heightContent)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$form,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_Utils_ap(
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'border-width', '0px'),
										A2($elm$html$Html$Attributes$style, 'font-family', 'Consolas'),
										A2($elm$html$Html$Attributes$style, 'font-size', '16px'),
										A2($elm$html$Html$Attributes$style, 'border', 'none'),
										A2($elm$html$Html$Attributes$style, 'outline', 'none'),
										$elm$html$Html$Attributes$placeholder('<no value>'),
										$elm$html$Html$Attributes$value(content),
										$elm$html$Html$Attributes$size(widthContent),
										A2($elm$html$Html$Attributes$style, 'background-color', 'transparent'),
										$elm$html$Html$Attributes$disabled(
										A2($author$project$Structure$boolOf, $author$project$Editor$roleGrabbed, cell))
									]),
								$author$project$Editor$inputCellAttributesFromEffects(cell)),
							_List_Nil)
						]))
				]));
	});
var $author$project$Editor$DragStart = function (a) {
	return {$: 'DragStart', a: a};
};
var $author$project$Editor$MouseEnter = function (a) {
	return {$: 'MouseEnter', a: a};
};
var $author$project$Editor$MouseLeave = function (a) {
	return {$: 'MouseLeave', a: a};
};
var $elm_community$typed_svg$TypedSvg$circle = $elm_community$typed_svg$TypedSvg$Core$node('circle');
var $author$project$Editor$colorGraphBackground = A3($avh4$elm_color$Color$rgb255, 240, 248, 255);
var $elm_community$typed_svg$TypedSvg$Attributes$cx = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'cx',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$cx = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$cx(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$cy = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'cy',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$cy = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$cy(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Events$on = $elm$virtual_dom$VirtualDom$on;
var $elm_community$typed_svg$TypedSvg$Events$simpleOn = function (name) {
	return function (msg) {
		return A2(
			$elm_community$typed_svg$TypedSvg$Events$on,
			name,
			$elm$virtual_dom$VirtualDom$Normal(
				$elm$json$Json$Decode$succeed(msg)));
	};
};
var $elm_community$typed_svg$TypedSvg$Events$onMouseDown = $elm_community$typed_svg$TypedSvg$Events$simpleOn('mousedown');
var $elm_community$typed_svg$TypedSvg$Events$onMouseEnter = $elm_community$typed_svg$TypedSvg$Events$simpleOn('mouseenter');
var $elm_community$typed_svg$TypedSvg$Events$onMouseLeave = $elm_community$typed_svg$TypedSvg$Events$simpleOn('mouseleave');
var $elm_community$typed_svg$TypedSvg$Attributes$r = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'r',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$r = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$r(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $author$project$Editor$vertexDragHandle = F2(
	function (cell, _v0) {
		var posVertex = _v0.posVertex;
		var attriutes = _Utils_ap(
			_List_fromArray(
				[
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$r(5),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$cx(
					$ianmackenzie$elm_geometry$Point2d$xCoordinate(posVertex)),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$cy(
					$ianmackenzie$elm_geometry$Point2d$yCoordinate(posVertex)),
					$elm_community$typed_svg$TypedSvg$Attributes$stroke(
					$elm_community$typed_svg$TypedSvg$Types$Paint($author$project$Editor$colorGraphPrimary)),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$strokeWidth(2),
					$elm_community$typed_svg$TypedSvg$Attributes$fill(
					$elm_community$typed_svg$TypedSvg$Types$Paint($author$project$Editor$colorGraphBackground))
				]),
			A2($author$project$Structure$boolOf, $author$project$Editor$roleGrabbed, cell) ? _List_fromArray(
				[
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$r(8),
					$elm_community$typed_svg$TypedSvg$Attributes$fill(
					$elm_community$typed_svg$TypedSvg$Types$Paint($author$project$Editor$colorGraphPrimary))
				]) : (A2($author$project$Structure$boolOf, $author$project$Editor$roleMouseEnter, cell) ? _List_fromArray(
				[
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$r(8),
					$elm_community$typed_svg$TypedSvg$Events$onMouseDown(
					$author$project$Editor$DragStart(
						$author$project$Structure$pathOf(cell))),
					$elm_community$typed_svg$TypedSvg$Events$onMouseLeave(
					$author$project$Editor$MouseLeave(
						$author$project$Structure$pathOf(cell)))
				]) : _List_fromArray(
				[
					$elm_community$typed_svg$TypedSvg$Events$onMouseEnter(
					$author$project$Editor$MouseEnter(
						$author$project$Structure$pathOf(cell)))
				])));
		return A2($elm_community$typed_svg$TypedSvg$circle, attriutes, _List_Nil);
	});
var $avh4$elm_color$Color$white = A4($avh4$elm_color$Color$RgbaSpace, 255 / 255, 255 / 255, 255 / 255, 1.0);
var $author$project$Editor$viewVertexCell = function (cell) {
	var vertexProps = $author$project$Editor$vertexProperties(cell);
	var handle = A2($author$project$Editor$vertexDragHandle, cell, vertexProps);
	var content = A2($author$project$Editor$vertexContent, cell, vertexProps);
	return A2(
		$elm_community$typed_svg$TypedSvg$g,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm_community$typed_svg$TypedSvg$rect,
				_List_fromArray(
					[
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$width(vertexProps.widthVertex),
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$height(vertexProps.heightVertex),
						$elm_community$typed_svg$TypedSvg$Attributes$fill(
						$elm_community$typed_svg$TypedSvg$Types$Paint($avh4$elm_color$Color$white)),
						$elm_community$typed_svg$TypedSvg$Attributes$stroke(
						$elm_community$typed_svg$TypedSvg$Types$Paint($author$project$Editor$colorGraphPrimary)),
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$strokeWidth(2),
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$x(
						$ianmackenzie$elm_geometry$Point2d$xCoordinate(vertexProps.posVertex)),
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$y(
						$ianmackenzie$elm_geometry$Point2d$yCoordinate(vertexProps.posVertex)),
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$rx(4),
						$elm_community$typed_svg$TypedSvg$Attributes$InPx$ry(4)
					]),
				_List_Nil),
				content,
				handle
			]));
};
var $author$project$Editor$viewGraphCell = function (cellGraph) {
	return A2(
		$elm_community$typed_svg$TypedSvg$svg,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'width', '100%'),
				A2($elm$html$Html$Attributes$style, 'height', '800px'),
				A2($elm$html$Html$Attributes$style, 'background-color', 'AliceBlue')
			]),
		_List_fromArray(
			[
				A2(
				$elm_community$typed_svg$TypedSvg$g,
				_List_Nil,
				$author$project$Editor$viewEdgeCells(cellGraph)),
				A2(
				$elm_community$typed_svg$TypedSvg$g,
				_List_Nil,
				A2(
					$elm$core$List$map,
					$author$project$Editor$viewVertexCell,
					A2(
						$author$project$Structure$nodesOf,
						$author$project$Editor$ContentCell($author$project$Editor$VertexCell),
						cellGraph)))
			]));
};
var $author$project$Editor$viewInputCell = function (cell) {
	var _v0 = $author$project$Structure$isaOf(cell);
	if (_v0.$ === 'ContentCell') {
		var inputValue = A2($author$project$Structure$textOf, $author$project$Editor$roleInput, cell);
		var inputSize = (inputValue === '') ? $elm$core$String$length('<no value>') : $elm$core$String$length(inputValue);
		return A2(
			$elm$html$Html$div,
			$author$project$Editor$divCellAttributes(cell),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_Utils_ap(
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'border-width', '0px'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Consolas'),
								A2($elm$html$Html$Attributes$style, 'font-size', '16px'),
								A2($elm$html$Html$Attributes$style, 'border', 'none'),
								A2($elm$html$Html$Attributes$style, 'outline', 'none'),
								$elm$html$Html$Attributes$placeholder('<no value>'),
								$elm$html$Html$Attributes$value(inputValue),
								$elm$html$Html$Attributes$size(inputSize),
								$elm$html$Html$Attributes$id(
								$author$project$Structure$pathAsIdFromNode(cell))
							]),
						_Utils_ap(
							$author$project$Editor$marginsAndPaddings(cell),
							$author$project$Editor$inputCellAttributesFromEffects(cell))),
					_List_Nil)
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$Swallow = function (a) {
	return {$: 'Swallow', a: a};
};
var $author$project$Editor$viewPlaceholderCell = function (cell) {
	var _v0 = $author$project$Structure$isaOf(cell);
	if (_v0.$ === 'ContentCell') {
		var placeholderValue = A2($author$project$Structure$textOf, $author$project$Editor$rolePlaceholder, cell);
		var inputValue = '<' + ((placeholderValue === '') ? '...' : (placeholderValue + '>'));
		var inputSize = $elm$core$String$length(inputValue);
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_Utils_ap(
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'border-width', '0px'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Consolas'),
								A2($elm$html$Html$Attributes$style, 'font-size', '16px'),
								A2($elm$html$Html$Attributes$style, 'border', 'none'),
								A2($elm$html$Html$Attributes$style, 'outline', 'none'),
								A2($elm$html$Html$Attributes$style, 'color', '#888888'),
								A2($elm$html$Html$Attributes$style, 'font-style', 'italic'),
								$elm$html$Html$Attributes$value(inputValue),
								$elm$html$Html$Attributes$size(inputSize),
								$elm$html$Html$Events$onInput($author$project$Editor$Swallow),
								$elm$html$Html$Attributes$id(
								$author$project$Structure$pathAsIdFromNode(cell))
							]),
						_Utils_ap(
							$author$project$Editor$inputCellAttributesFromEffects(cell),
							$author$project$Editor$marginsAndPaddings(cell))),
					_List_Nil)
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $elm$html$Html$datalist = _VirtualDom_node('datalist');
var $elm$html$Html$Attributes$list = _VirtualDom_attribute('list');
var $elm$html$Html$option = _VirtualDom_node('option');
var $author$project$Editor$optionFromScope = function (scopeElement) {
	var scopeValue = A2($author$project$Structure$textOf, $author$project$Editor$roleConstant, scopeElement);
	return A2(
		$elm$html$Html$option,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$value(scopeValue)
			]),
		_List_Nil);
};
var $author$project$Editor$viewRefCell = function (cell) {
	var _v0 = $author$project$Structure$isaOf(cell);
	if (_v0.$ === 'ContentCell') {
		var options = A2(
			$elm$core$List$map,
			$author$project$Editor$optionFromScope,
			A2($author$project$Structure$getUnderCustom, $author$project$Editor$roleScope, cell));
		var inputValue = A2($author$project$Structure$textOf, $author$project$Editor$roleInput, cell);
		var inputSize = (inputValue === '') ? ($elm$core$String$length('<no target>') + 2) : ($elm$core$String$length(inputValue) + 2);
		var inputId = $author$project$Structure$pathAsIdFromNode(cell);
		var datalistId = inputId + '-datalist';
		return A2(
			$elm$html$Html$div,
			$author$project$Editor$divCellAttributes(cell),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$datalist,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(datalistId)
						]),
					options),
					A2(
					$elm$html$Html$input,
					_Utils_ap(
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'border-width', '0px'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Consolas'),
								A2($elm$html$Html$Attributes$style, 'font-size', '16px'),
								A2($elm$html$Html$Attributes$style, 'border', 'none'),
								A2($elm$html$Html$Attributes$style, 'outline', 'none'),
								$elm$html$Html$Attributes$placeholder('<no target>'),
								$elm$html$Html$Attributes$size(inputSize),
								$elm$html$Html$Attributes$id(inputId),
								$elm$html$Html$Attributes$list(datalistId),
								$elm$html$Html$Attributes$value(inputValue)
							]),
						_Utils_ap(
							$author$project$Editor$marginsAndPaddings(cell),
							$author$project$Editor$inputCellAttributesFromEffects(cell))),
					_List_Nil)
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$viewCell = function (cell) {
	var _v14 = $author$project$Structure$isaOf(cell);
	if (_v14.$ === 'ContentCell') {
		var _v15 = $author$project$Structure$getUnderDefault(cell);
		if (!_v15.b) {
			return _List_fromArray(
				[
					$elm$html$Html$text('')
				]);
		} else {
			var children = _v15;
			return A3($elm$core$List$foldl, $author$project$Editor$viewContent, _List_Nil, children);
		}
	} else {
		return _List_Nil;
	}
};
var $author$project$Editor$viewContent = F2(
	function (cell, html) {
		var _v12 = $author$project$Structure$isaOf(cell);
		if (_v12.$ === 'ContentCell') {
			var ccell = _v12.a;
			var htmlNew = function () {
				switch (ccell.$) {
					case 'SplitCell':
						return $author$project$Editor$viewSplitCell(cell);
					case 'RootCell':
						return $author$project$Editor$viewStackCell(cell);
					case 'ConstantCell':
						return $author$project$Editor$viewConstantCell(cell);
					case 'InputCell':
						return $author$project$Editor$viewInputCell(cell);
					case 'StackCell':
						return $author$project$Editor$viewStackCell(cell);
					case 'PlaceholderCell':
						return $author$project$Editor$viewPlaceholderCell(cell);
					case 'ButtonCell':
						return $author$project$Editor$viewButtonCell(cell);
					case 'RefCell':
						return $author$project$Editor$viewRefCell(cell);
					case 'GraphCell':
						return $author$project$Editor$viewGraphCell(cell);
					case 'VertexCell':
						return $elm$html$Html$text('');
					default:
						return $elm$html$Html$text('');
				}
			}();
			return $elm$core$List$reverse(
				A2(
					$elm$core$List$cons,
					htmlNew,
					$elm$core$List$reverse(html)));
		} else {
			return _List_Nil;
		}
	});
var $author$project$Editor$viewHorizSplit = function (cell) {
	var _v8 = $author$project$Structure$isaOf(cell);
	if (_v8.$ === 'ContentCell') {
		var _v9 = function () {
			var _v10 = $author$project$Structure$getUnderDefault(cell);
			if (!_v10.b) {
				return _Utils_Tuple2(
					_List_fromArray(
						[
							$elm$html$Html$text('Completely empty split cell')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('')
						]));
			} else {
				if (!_v10.b.b) {
					var first = _v10.a;
					return _Utils_Tuple2(
						$author$project$Editor$viewCell(first),
						_List_fromArray(
							[
								$elm$html$Html$text('Empty bottom side')
							]));
				} else {
					var first = _v10.a;
					var _v11 = _v10.b;
					var second = _v11.a;
					return _Utils_Tuple2(
						A2($author$project$Editor$viewContent, first, _List_Nil),
						A2($author$project$Editor$viewContent, second, _List_Nil));
				}
			}
		}();
		var top = _v9.a;
		var bottom = _v9.b;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('split top')
						]),
					top),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('split bottom')
						]),
					bottom)
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$viewHorizStackCell = function (cell) {
	var _v7 = $author$project$Structure$isaOf(cell);
	if (_v7.$ === 'ContentCell') {
		var displayAttrs = $author$project$Editor$divRowAttributes(cell);
		return A2(
			$elm$html$Html$div,
			A2(
				$elm$core$List$cons,
				$elm$html$Html$Attributes$id(
					$author$project$Structure$pathAsIdFromNode(cell)),
				_Utils_ap(
					$author$project$Editor$marginsAndPaddings(cell),
					_Utils_eq(displayAttrs, _List_Nil) ? _List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'flex')
						]) : displayAttrs)),
			$author$project$Editor$viewCell(cell));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$viewSplitCell = function (cell) {
	var _v6 = $author$project$Structure$isaOf(cell);
	if (_v6.$ === 'ContentCell') {
		var bO = A2($author$project$Structure$boolOf, $author$project$Editor$roleIsHoriz, cell);
		return bO ? $author$project$Editor$viewHorizSplit(cell) : $author$project$Editor$viewVertSplit(cell);
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$viewStackCell = function (cell) {
	var _v5 = $author$project$Structure$isaOf(cell);
	if (_v5.$ === 'ContentCell') {
		var bO = A2($author$project$Structure$boolOf, $author$project$Editor$roleIsHoriz, cell);
		return bO ? $author$project$Editor$viewHorizStackCell(cell) : $author$project$Editor$viewVertStackCell(cell);
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$viewVertSplit = function (cell) {
	var _v1 = $author$project$Structure$isaOf(cell);
	if (_v1.$ === 'ContentCell') {
		var _v2 = function () {
			var _v3 = $author$project$Structure$getUnderDefault(cell);
			if (!_v3.b) {
				return _Utils_Tuple2(
					_List_fromArray(
						[
							$elm$html$Html$text('Completely empty split cell')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('')
						]));
			} else {
				if (!_v3.b.b) {
					var first = _v3.a;
					return _Utils_Tuple2(
						A2($author$project$Editor$viewContent, first, _List_Nil),
						_List_fromArray(
							[
								$elm$html$Html$text('Empty right side')
							]));
				} else {
					var first = _v3.a;
					var _v4 = _v3.b;
					var second = _v4.a;
					return _Utils_Tuple2(
						A2($author$project$Editor$viewContent, first, _List_Nil),
						A2($author$project$Editor$viewContent, second, _List_Nil));
				}
			}
		}();
		var left = _v2.a;
		var right = _v2.b;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('split left')
						]),
					left),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('split right')
						]),
					right)
				]));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$viewVertStackCell = function (cell) {
	var _v0 = $author$project$Structure$isaOf(cell);
	if (_v0.$ === 'ContentCell') {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id(
						$author$project$Structure$pathAsIdFromNode(cell)),
						A2($elm$html$Html$Attributes$style, 'display', 'table')
					]),
				$author$project$Editor$marginsAndPaddings(cell)),
			$author$project$Editor$viewCell(cell));
	} else {
		return $elm$html$Html$text('');
	}
};
var $author$project$Editor$viewEditor = function (root) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'font-family', 'Consolas')
			]),
		function () {
			var _v0 = $author$project$Structure$isaOf(root);
			if (_v0.$ === 'ContentCell') {
				return $author$project$Editor$viewCell(root);
			} else {
				return _List_Nil;
			}
		}());
};
var $author$project$Runtime$view = function (model) {
	return A2(
		$elm$html$Html$map,
		$author$project$Runtime$EditorMsg,
		$author$project$Editor$viewEditor(model.editorModel.eRoot));
};
var $author$project$Runtime$projection = F2(
	function (dRoot, xform) {
		var dRootWithPaths = $author$project$Structure$updatePaths(dRoot);
		var domain = A2($author$project$Runtime$Domain, dRootWithPaths, xform);
		var eRoot = $author$project$Runtime$runDomainXform(domain);
		var initialModel = {
			domain: domain,
			editorModel: A2($author$project$Editor$initEditorModel, dRootWithPaths, eRoot)
		};
		var init = function (_v0) {
			return _Utils_Tuple2(initialModel, $elm$core$Platform$Cmd$none);
		};
		return $elm$browser$Browser$element(
			{init: init, subscriptions: $author$project$Runtime$subscriptions, update: $author$project$Runtime$update, view: $author$project$Runtime$view});
	});
var $author$project$Tanks$main = A2($author$project$Runtime$projection, $author$project$Tanks$initFaction, $author$project$Tanks$editorFaction);
_Platform_export({'Tanks':{'init':$author$project$Tanks$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));
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

	/**_UNUSED/
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

	/**/
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

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
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

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


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



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


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

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
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

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
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


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
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
	if (region.cn.aT === region.cW.aT)
	{
		return 'on line ' + region.cn.aT;
	}
	return 'on lines ' + region.cn.aT + ' through ' + region.cW.aT;
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
	return word >= 0
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



/**_UNUSED/
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

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

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
		impl.fq,
		impl.gT,
		impl.gq,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_enqueueEffects(managers, result.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, result.b, subscriptions(model));

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


function _Platform_export(exports)
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


function _Platform_export_UNUSED(exports)
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

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
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

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
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
		S: func(record.S),
		co: record.co,
		b0: record.b0
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
		var message = !tag ? value : tag < 3 ? value.a : value.S;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.co;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.b0) && event.preventDefault(),
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
		impl.fq,
		impl.gT,
		impl.gq,
		function(sendToApp, initialModel) {
			var view = impl.gV;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
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
		impl.fq,
		impl.gT,
		impl.gq,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.cg && impl.cg(sendToApp)
			var view = impl.gV;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.eB);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.gJ) && (_VirtualDom_doc.title = title = doc.gJ);
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
	var onUrlChange = impl.fY;
	var onUrlRequest = impl.fZ;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		cg: function(sendToApp)
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
							&& curr.dJ === next.dJ
							&& curr.c5 === next.c5
							&& curr.dE.a === next.dE.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		fq: function(flags)
		{
			return A3(impl.fq, flags, _Browser_getUrl(), key);
		},
		gV: impl.gV,
		gT: impl.gT,
		gq: impl.gq
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
		? { fj: 'hidden', eM: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { fj: 'mozHidden', eM: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { fj: 'msHidden', eM: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { fj: 'webkitHidden', eM: 'webkitvisibilitychange' }
		: { fj: 'hidden', eM: 'visibilitychange' };
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
		dT: _Browser_getScene(),
		gW: {
			aN: _Browser_window.pageXOffset,
			aO: _Browser_window.pageYOffset,
			aL: _Browser_doc.documentElement.clientWidth,
			at: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aL: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		at: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			dT: {
				aL: node.scrollWidth,
				at: node.scrollHeight
			},
			gW: {
				aN: node.scrollLeft,
				aO: node.scrollTop,
				aL: node.clientWidth,
				at: node.clientHeight
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
			dT: _Browser_getScene(),
			gW: {
				aN: x,
				aO: y,
				aL: _Browser_doc.documentElement.clientWidth,
				at: _Browser_doc.documentElement.clientHeight
			},
			e6: {
				aN: x + rect.left,
				aO: y + rect.top,
				aL: rect.width,
				at: rect.height
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



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
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
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
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
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
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
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
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
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
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
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
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
		return {$: 0, a: a, b: b, c: c, d: d};
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
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
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
		if (!builder.i) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.l),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.l);
		} else {
			var treeLen = builder.i * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.m) : builder.m;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.i);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.l) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.l);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{m: nodeList, i: (len / $elm$core$Array$branchFactor) | 0, l: tail});
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
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
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
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {c$: fragment, c5: host, dB: path, dE: port_, dJ: protocol, dK: query};
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
					if (_v1.$ === 1) {
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
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
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
		var task = _v0;
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
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$PressurePlot$Model = F2(
	function (events, predictions) {
		return {f: events, dF: predictions};
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$PressurePlot$init = _Utils_Tuple2(
	A2($author$project$PressurePlot$Model, _List_Nil, _List_Nil),
	$elm$core$Platform$Cmd$none);
var $author$project$PressurePlot$Move = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$PressurePlot$penMoveEvent = _Platform_incomingPort('penMoveEvent', $elm$json$Json$Decode$value);
var $author$project$PressurePlot$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$PressurePlot$penMoveEvent($author$project$PressurePlot$Move)
			]));
};
var $author$project$Pointer$Pen = 2;
var $author$project$Pointer$Event = function (pointerId) {
	return function (width) {
		return function (height) {
			return function (pressure) {
				return function (tangentialPressure) {
					return function (tiltX) {
						return function (tiltY) {
							return function (twist) {
								return function (altitudeAngle) {
									return function (azimuthAngle) {
										return function (pointerType) {
											return function (isPrimary) {
												return function (offsetX) {
													return function (offsetY) {
														return function (screenX) {
															return function (screenY) {
																return function (pageX) {
																	return function (pageY) {
																		return function (timeStamp) {
																			return {aq: altitudeAngle, ar: azimuthAngle, at: height, au: isPrimary, aw: offsetX, ax: offsetY, ay: pageX, az: pageY, aA: pointerId, aB: pointerType, aC: pressure, aD: screenX, aE: screenY, aG: tangentialPressure, d5: tiltX, d6: tiltY, aH: timeStamp, aJ: twist, aL: width};
																		};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Pointer$andMap = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $elm_community$json_extra$Json$Decode$Extra$withDefault = F2(
	function (fallback, decoder) {
		return A2(
			$elm$json$Json$Decode$map,
			$elm$core$Maybe$withDefault(fallback),
			$elm$json$Json$Decode$maybe(decoder));
	});
var $author$project$Pointer$andWithDefault = F2(
	function (a, decoder) {
		return $author$project$Pointer$andMap(
			A2($elm_community$json_extra$Json$Decode$Extra$withDefault, a, decoder));
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Pointer$Mouse = 0;
var $elm$core$Basics$pi = _Basics_pi;
var $author$project$Pointer$defaultEvent = {aq: $elm$core$Basics$pi / 2, ar: 0, at: 1, au: false, aw: 0, ax: 0, ay: 0, az: 0, aA: 0, aB: 0, aC: 0, aD: 0, aE: 0, aG: 0, d5: 0, d6: 0, aH: 0, aJ: 0, aL: 1};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Pointer$Touch = 1;
var $author$project$Pointer$inputTypeFromString = function (str) {
	switch (str) {
		case 'touch':
			return 1;
		case 'pen':
			return 2;
		default:
			return 0;
	}
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Pointer$eventDecoder = A3(
	$author$project$Pointer$andWithDefault,
	$author$project$Pointer$defaultEvent.aH,
	A2($elm$json$Json$Decode$field, 'timeStamp', $elm$json$Json$Decode$float),
	A3(
		$author$project$Pointer$andWithDefault,
		$author$project$Pointer$defaultEvent.az,
		A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float),
		A3(
			$author$project$Pointer$andWithDefault,
			$author$project$Pointer$defaultEvent.ay,
			A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
			A3(
				$author$project$Pointer$andWithDefault,
				$author$project$Pointer$defaultEvent.aE,
				A2($elm$json$Json$Decode$field, 'screenY', $elm$json$Json$Decode$float),
				A3(
					$author$project$Pointer$andWithDefault,
					$author$project$Pointer$defaultEvent.aD,
					A2($elm$json$Json$Decode$field, 'screenX', $elm$json$Json$Decode$float),
					A3(
						$author$project$Pointer$andWithDefault,
						$author$project$Pointer$defaultEvent.ax,
						A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float),
						A3(
							$author$project$Pointer$andWithDefault,
							$author$project$Pointer$defaultEvent.aw,
							A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
							A3(
								$author$project$Pointer$andWithDefault,
								$author$project$Pointer$defaultEvent.au,
								A2($elm$json$Json$Decode$field, 'isPrimary', $elm$json$Json$Decode$bool),
								A3(
									$author$project$Pointer$andWithDefault,
									$author$project$Pointer$defaultEvent.aB,
									A2(
										$elm$json$Json$Decode$field,
										'pointerType',
										A2($elm$json$Json$Decode$map, $author$project$Pointer$inputTypeFromString, $elm$json$Json$Decode$string)),
									A3(
										$author$project$Pointer$andWithDefault,
										$author$project$Pointer$defaultEvent.ar,
										A2($elm$json$Json$Decode$field, 'azimuthAngle', $elm$json$Json$Decode$float),
										A3(
											$author$project$Pointer$andWithDefault,
											$author$project$Pointer$defaultEvent.aq,
											A2($elm$json$Json$Decode$field, 'altitudeAngle', $elm$json$Json$Decode$float),
											A3(
												$author$project$Pointer$andWithDefault,
												$author$project$Pointer$defaultEvent.aJ,
												A2($elm$json$Json$Decode$field, 'twist', $elm$json$Json$Decode$int),
												A3(
													$author$project$Pointer$andWithDefault,
													$author$project$Pointer$defaultEvent.d6,
													A2($elm$json$Json$Decode$field, 'tiltY', $elm$json$Json$Decode$int),
													A3(
														$author$project$Pointer$andWithDefault,
														$author$project$Pointer$defaultEvent.d5,
														A2($elm$json$Json$Decode$field, 'tiltX', $elm$json$Json$Decode$int),
														A3(
															$author$project$Pointer$andWithDefault,
															$author$project$Pointer$defaultEvent.aG,
															A2($elm$json$Json$Decode$field, 'tangentialPressure', $elm$json$Json$Decode$float),
															A3(
																$author$project$Pointer$andWithDefault,
																$author$project$Pointer$defaultEvent.aC,
																A2($elm$json$Json$Decode$field, 'pressure', $elm$json$Json$Decode$float),
																A3(
																	$author$project$Pointer$andWithDefault,
																	$author$project$Pointer$defaultEvent.at,
																	A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float),
																	A3(
																		$author$project$Pointer$andWithDefault,
																		$author$project$Pointer$defaultEvent.aL,
																		A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
																		A3(
																			$author$project$Pointer$andWithDefault,
																			$author$project$Pointer$defaultEvent.aA,
																			A2($elm$json$Json$Decode$field, 'pointerId', $elm$json$Json$Decode$int),
																			$elm$json$Json$Decode$succeed($author$project$Pointer$Event))))))))))))))))))));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$PressurePlot$decodeBundle = A3(
	$elm$json$Json$Decode$map2,
	$author$project$PressurePlot$Model,
	A2(
		$elm$json$Json$Decode$field,
		'events',
		$elm$json$Json$Decode$list($author$project$Pointer$eventDecoder)),
	A2(
		$elm$json$Json$Decode$field,
		'predictions',
		$elm$json$Json$Decode$list($author$project$Pointer$eventDecoder)));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$PressurePlot$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 1:
				var ev = msg.a;
				return (ev.aB === 2) ? _Utils_Tuple2(
					A2(
						$author$project$PressurePlot$Model,
						_List_fromArray(
							[ev]),
						_List_Nil),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			default:
				var ev = msg.a;
				var _v1 = A2($elm$json$Json$Decode$decodeValue, $author$project$PressurePlot$decodeBundle, ev);
				if (!_v1.$) {
					var bundle = _v1.a;
					return _Utils_Tuple2(
						A2(
							$author$project$PressurePlot$Model,
							_Utils_ap(model.f, bundle.f),
							bundle.dF),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$PressurePlot$Down = function (a) {
	return {$: 1, a: a};
};
var $author$project$PressurePlot$NoOp = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 6, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Right = 2;
var $mdgriffith$elm_ui$Element$alignRight = $mdgriffith$elm_ui$Internal$Model$AlignX(2);
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $author$project$PressurePlot$blockContextMenu = function (msg) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'contextmenu',
		A2(
			$elm$json$Json$Decode$map,
			function (m) {
				return _Utils_Tuple2(m, true);
			},
			$elm$json$Json$Decode$succeed(msg)));
};
var $mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$AsColumn = 1;
var $mdgriffith$elm_ui$Internal$Model$asColumn = 1;
var $mdgriffith$elm_ui$Internal$Style$classes = {ei: 'a', bt: 'atv', el: 'ab', em: 'cx', en: 'cy', eo: 'acb', ep: 'accx', eq: 'accy', er: 'acr', cB: 'al', cC: 'ar', es: 'at', bu: 'ah', bv: 'av', ev: 's', ez: 'bh', eA: 'b', eC: 'w7', eE: 'bd', eF: 'bdt', a1: 'bn', eG: 'bs', a2: 'cpe', eQ: 'cp', eR: 'cpx', eS: 'cpy', Q: 'c', a4: 'ctr', a5: 'cb', a6: 'ccx', R: 'ccy', aS: 'cl', a7: 'cr', eV: 'ct', eY: 'cptr', eZ: 'ctxt', ff: 'fcs', c_: 'focus-within', fg: 'fs', fh: 'g', bE: 'hbh', bF: 'hc', c3: 'he', bG: 'hf', c4: 'hfp', fk: 'hv', fn: 'ic', fp: 'fr', fr: 'iml', fs: 'imlf', ft: 'imlp', fu: 'implw', fv: 'it', fA: 'i', dl: 'lnk', av: 'nb', dr: 'notxt', fV: 'ol', fX: 'or', af: 'oq', f0: 'oh', dz: 'pg', dA: 'p', f2: 'ppe', f7: 'ui', dP: 'r', ga: 'sb', gb: 'sbx', gc: 'sby', gd: 'sbt', gg: 'e', gi: 'cap', gj: 'sev', gp: 'sk', cq: 't', gu: 'tc', gv: 'w8', gw: 'w2', gx: 'w9', gy: 'tj', br: 'tja', gz: 'tl', gA: 'w3', gB: 'w5', gC: 'w4', gD: 'tr', gE: 'w6', gF: 'w1', gG: 'tun', d9: 'ts', ak: 'clr', gS: 'u', cv: 'wc', ee: 'we', cw: 'wf', ef: 'wfp', cx: 'wrp'};
var $mdgriffith$elm_ui$Internal$Model$Generic = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$div = $mdgriffith$elm_ui$Internal$Model$Generic;
var $mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$columnClass = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.Q);
var $mdgriffith$elm_ui$Internal$Model$gridClass = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.fh);
var $mdgriffith$elm_ui$Internal$Model$pageClass = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dz);
var $mdgriffith$elm_ui$Internal$Model$paragraphClass = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dA);
var $mdgriffith$elm_ui$Internal$Model$rowClass = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dP);
var $mdgriffith$elm_ui$Internal$Model$singleClass = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.gg);
var $mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Model$rowClass;
		case 1:
			return $mdgriffith$elm_ui$Internal$Model$columnClass;
		case 2:
			return $mdgriffith$elm_ui$Internal$Model$singleClass;
		case 3:
			return $mdgriffith$elm_ui$Internal$Model$gridClass;
		case 4:
			return $mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return $mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							$elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$AsEl = 2;
var $mdgriffith$elm_ui$Internal$Model$asEl = 2;
var $mdgriffith$elm_ui$Internal$Model$AsParagraph = 4;
var $mdgriffith$elm_ui$Internal$Model$asParagraph = 4;
var $mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? $mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : $mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var $mdgriffith$elm_ui$Internal$Flag$alignBottom = $mdgriffith$elm_ui$Internal$Flag$flag(41);
var $mdgriffith$elm_ui$Internal$Flag$alignRight = $mdgriffith$elm_ui$Internal$Flag$flag(40);
var $mdgriffith$elm_ui$Internal$Flag$centerX = $mdgriffith$elm_ui$Internal$Flag$flag(42);
var $mdgriffith$elm_ui$Internal$Flag$centerY = $mdgriffith$elm_ui$Internal$Flag$flag(43);
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 0:
			var px = x.a;
			return $elm$core$String$fromInt(px) + 'px';
		case 1:
			return 'auto';
		case 2:
			var i = x.a;
			return $elm$core$String$fromInt(i) + 'fr';
		case 3:
			var min = x.a;
			var len = x.b;
			return 'min' + ($elm$core$String$fromInt(min) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + ($elm$core$String$fromInt(max) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$Basics$round = _Basics_round;
var $mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return $elm$core$String$fromInt(
		$elm$core$Basics$round(x * 255));
};
var $mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'mv-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			return $elm$core$Maybe$Just(
				'tfrm-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 13:
			var name = style.a;
			return name;
		case 12:
			var name = style.a;
			var o = style.b;
			return name;
		case 0:
			var _class = style.a;
			return _class;
		case 1:
			var name = style.a;
			return name;
		case 2:
			var i = style.a;
			return 'font-size-' + $elm$core$String$fromInt(i);
		case 3:
			var _class = style.a;
			return _class;
		case 4:
			var _class = style.a;
			return _class;
		case 5:
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 7:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 6:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 8:
			var template = style.a;
			return 'grid-rows-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.f8)) + ('-cols-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.H)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.gk.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.gk.b)))))));
		case 9:
			var pos = style.a;
			return 'gp grid-pos-' + ($elm$core$String$fromInt(pos.dP) + ('-' + ($elm$core$String$fromInt(pos.cN) + ('-' + ($elm$core$String$fromInt(pos.aL) + ('-' + $elm$core$String$fromInt(pos.at)))))));
		case 11:
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector) {
					case 0:
						return 'fs';
					case 1:
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (sty) {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_v1 === '') {
							return '';
						} else {
							var styleName = _v1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				$mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
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
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
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
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
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
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = $mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2($elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2($elm$core$Set$insert, styleName, cache),
			A2($elm$core$List$cons, style, existing));
	});
var $mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
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
var $elm$core$String$fromFloat = _String_fromNumber;
var $mdgriffith$elm_ui$Internal$Model$formatColor = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return 'rgba(' + ($elm$core$String$fromInt(
		$elm$core$Basics$round(red * 255)) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(green * 255))) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(blue * 255))) + (',' + ($elm$core$String$fromFloat(alpha) + ')')))));
};
var $mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.da ? $elm$core$Maybe$Just('inset') : $elm$core$Maybe$Nothing,
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.b.a) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.b.b) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.cG) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.gh) + 'px'),
					$elm$core$Maybe$Just(
					$mdgriffith$elm_ui$Internal$Model$formatColor(shadow.cO))
				])));
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c_) + ':focus-within',
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.eD),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.ex),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										cG: shadow.cG,
										cO: shadow.cO,
										da: false,
										b: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.b)),
										gh: shadow.gh
									}));
						},
						focus.ge),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					]))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev) + (':focus .focusable, ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev) + '.focusable:focus')),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.eD),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.ex),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										cG: shadow.cG,
										cO: shadow.cO,
										da: false,
										b: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.b)),
										gh: shadow.gh
									}));
						},
						focus.ge),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					])))
		]);
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Left = 3;
var $mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Right = 2;
var $mdgriffith$elm_ui$Internal$Style$Self = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Content = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Bottom = 1;
var $mdgriffith$elm_ui$Internal$Style$CenterX = 4;
var $mdgriffith$elm_ui$Internal$Style$CenterY = 5;
var $mdgriffith$elm_ui$Internal$Style$Top = 0;
var $mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
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
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eV);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a5);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a7);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aS);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a6);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.R);
	}
};
var $mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.es);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.el);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cC);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cB);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.em);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.en);
	}
};
var $mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _v0 = values(alignment);
		var content = _v0.a;
		var indiv = _v0.b;
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$contentName(alignment),
				content),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						indiv)
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bE),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ez),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gd),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cq),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cw),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bF),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cw),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ef),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		$mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment) {
				case 0:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 1:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 2:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 3:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 4:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var $mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						values(alignment))
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$Above = 0;
var $mdgriffith$elm_ui$Internal$Style$Behind = 5;
var $mdgriffith$elm_ui$Internal$Style$Below = 1;
var $mdgriffith$elm_ui$Internal$Style$OnLeft = 3;
var $mdgriffith$elm_ui$Internal$Style$OnRight = 2;
var $mdgriffith$elm_ui$Internal$Style$Within = 4;
var $mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = 0;
	var _v0 = function () {
		switch (loc) {
			case 0:
				return 0;
			case 1:
				return 0;
			case 2:
				return 0;
			case 3:
				return 0;
			case 4:
				return 0;
			default:
				return 0;
		}
	}();
	return _List_fromArray(
		[0, 1, 2, 3, 4, 5]);
}();
var $mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
			_Utils_ap(
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gg),
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fn))),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev) + ':focus',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.f7),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fp),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.av),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.av),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gg),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				$mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2($elm$core$List$map, fn, $mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc) {
							case 0:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ei),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cw),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 1:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eA),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 2:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fX),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 3:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fV),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 4:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fp),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ez),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cx),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dr),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eZ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.f2),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a2),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ak),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.af),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.fk, $mdgriffith$elm_ui$Internal$Style$classes.ak)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.fk, $mdgriffith$elm_ui$Internal$Style$classes.af)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.ff, $mdgriffith$elm_ui$Internal$Style$classes.ak)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.ff, $mdgriffith$elm_ui$Internal$Style$classes.af)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.bt, $mdgriffith$elm_ui$Internal$Style$classes.ak)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.bt, $mdgriffith$elm_ui$Internal$Style$classes.af)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d9),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							$elm$core$String$join,
							', ',
							A2(
								$elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ga),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gb),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dP),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gc),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.Q),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gg),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eQ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eR),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eS),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a1),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eE),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eF),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eG),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cq),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fv),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gg),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dP),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ee),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dl),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c4),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cw),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a4),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.er,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ep,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.em),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ep,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.em),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ep,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.en),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.ep + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.er + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.ep)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gj),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.Q),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c3),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.Q),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bG),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cw),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ef),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.eo,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.eq,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.en),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.eq,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.en),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.eq,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.en),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.eq + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.eo + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.eq)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a4),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gj),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fh),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 1:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 2:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 3:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 4:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev + ':first-child'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.ev + ($mdgriffith$elm_ui$Internal$Style$selfName(3) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.ev))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.ev + ($mdgriffith$elm_ui$Internal$Style$selfName(2) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.ev))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fr),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background-color', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fu),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gg),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ft),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fs),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'transparent')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dA),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bE),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ez),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cq),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gg),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fp),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ez),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ei),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eA),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fX),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fV),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cq),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gg),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Internal$Style$Child,
										$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cq),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
											]))
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dP),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.Q),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fh),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gF),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gw),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gA),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gC),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gB),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gE),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eC),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gv),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gx),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fA),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gp),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gS),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gS),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gp)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gG),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gy),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.br),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gu),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gD),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.gz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var $mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var $mdgriffith$elm_ui$Internal$Style$commonValues = $elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			$elm$core$List$map,
			function (x) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + $elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							$elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 6)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 8, 32)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var $mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var $mdgriffith$elm_ui$Internal$Style$inputTextReset = '\ninput[type="search"],\ninput[type="search"]::-webkit-search-decoration,\ninput[type="search"]::-webkit-search-cancel-button,\ninput[type="search"]::-webkit-search-results-button,\ninput[type="search"]::-webkit-search-results-decoration {\n  -webkit-appearance:none;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$sliderReset = '\ninput[type=range] {\n  -webkit-appearance: none; \n  background: transparent;\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$thumbReset = '\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var $mdgriffith$elm_ui$Internal$Style$trackReset = '\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dP) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev) + (' { flex-basis: auto !important; } ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dP) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a4) + (' { flex-basis: auto !important; }}' + ($mdgriffith$elm_ui$Internal$Style$inputTextReset + ($mdgriffith$elm_ui$Internal$Style$sliderReset + ($mdgriffith$elm_ui$Internal$Style$trackReset + ($mdgriffith$elm_ui$Internal$Style$thumbReset + $mdgriffith$elm_ui$Internal$Style$explainer)))))))))))))));
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $mdgriffith$elm_ui$Internal$Style$Intermediate = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return {a3: closing, r: _List_Nil, U: _List_Nil, M: selector};
	});
var $mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_v0, rulesToRender) {
		var parent = _v0;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 0:
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								U: A2(
									$elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.U)
							});
					case 2:
						var _v2 = rule.a;
						var prop = _v2.a;
						var value = _v2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								r: A2(
									$elm$core$List$cons,
									{a3: '\n}', r: _List_Nil, U: props, M: '@supports (' + (prop + (':' + (value + (') {' + parent.M))))},
									rendered.r)
							});
					case 4:
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								r: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.M + (' + ' + selector), ''),
										adjRules),
									rendered.r)
							});
					case 1:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								r: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.M + (' > ' + child), ''),
										childRules),
									rendered.r)
							});
					case 3:
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								r: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											$mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.M, descriptor),
											''),
										descriptorRules),
									rendered.r)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								r: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.M, ''),
										batched),
									rendered.r)
							});
				}
			});
		return A3($elm$core$List$foldr, generateIntermediates, parent, rulesToRender);
	});
var $mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				function (_v3) {
					var x = _v3.a;
					var y = _v3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _v2 = rule.U;
		if (!_v2.b) {
			return '';
		} else {
			return rule.M + ('{' + (renderValues(rule.U) + (rule.a3 + '}')));
		}
	};
	var renderIntermediate = function (_v0) {
		var rule = _v0;
		return _Utils_ap(
			renderClass(rule),
			$elm$core$String$concat(
				A2($elm$core$List$map, renderIntermediate, rule.r)));
	};
	return $elm$core$String$concat(
		A2(
			$elm$core$List$map,
			renderIntermediate,
			A3(
				$elm$core$List$foldr,
				F2(
					function (_v1, existing) {
						var name = _v1.a;
						var styleRules = _v1.b;
						return A2(
							$elm$core$List$cons,
							A2(
								$mdgriffith$elm_ui$Internal$Style$renderRules,
								A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var $mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	$mdgriffith$elm_ui$Internal$Style$overrides,
	$mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap($mdgriffith$elm_ui$Internal$Style$baseSheet, $mdgriffith$elm_ui$Internal$Style$commonValues)));
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $mdgriffith$elm_ui$Internal$Model$staticRoot = function (opts) {
	var _v0 = opts.fI;
	switch (_v0) {
		case 0:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'div',
				_List_Nil,
				_List_fromArray(
					[
						A3(
						$elm$virtual_dom$VirtualDom$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								$elm$virtual_dom$VirtualDom$text($mdgriffith$elm_ui$Internal$Style$rules)
							]))
					]));
		case 1:
			return $elm$virtual_dom$VirtualDom$text('');
		default:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'elm-ui-static-rules',
				_List_fromArray(
					[
						A2(
						$elm$virtual_dom$VirtualDom$property,
						'rules',
						$elm$json$Json$Encode$string($mdgriffith$elm_ui$Internal$Style$rules))
					]),
				_List_Nil);
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
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
var $mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 0:
			return 'serif';
		case 1:
			return 'sans-serif';
		case 2:
			return 'monospace';
		case 3:
			var name = font.a;
			return '\"' + (name + '\"');
		case 4:
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.fL;
			return '\"' + (name + '\"');
	}
};
var $mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return name === 'smcp';
		case 1:
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var $mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.ea);
	} else {
		return false;
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _v0, existing) {
		var key = _v0.a;
		var val = _v0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var $mdgriffith$elm_ui$Internal$Model$renderStyle = F4(
	function (options, maybePseudo, selector, props) {
		if (maybePseudo.$ === 1) {
			return _List_fromArray(
				[
					selector + ('{' + (A3(
					$elm$core$List$foldl,
					$mdgriffith$elm_ui$Internal$Model$renderProps(false),
					'',
					props) + '\n}'))
				]);
		} else {
			var pseudo = maybePseudo.a;
			switch (pseudo) {
				case 1:
					var _v2 = options.fk;
					switch (_v2) {
						case 0:
							return _List_Nil;
						case 2:
							return _List_fromArray(
								[
									selector + ('-hv {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(true),
									'',
									props) + '\n}'))
								]);
						default:
							return _List_fromArray(
								[
									selector + ('-hv:hover {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(false),
									'',
									props) + '\n}'))
								]);
					}
				case 0:
					var renderedProps = A3(
						$elm$core$List$foldl,
						$mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props);
					return _List_fromArray(
						[selector + ('-fs:focus {' + (renderedProps + '\n}')), '.' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + (':focus ~ ' + (selector + ('-fs:not(.focus)  {' + (renderedProps + '\n}'))))), '.' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + (':focus ' + (selector + ('-fs  {' + (renderedProps + '\n}'))))), selector + ('-fs:focus-within {' + (renderedProps + '\n}')), '.focusable-parent:focus ~ ' + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + (selector + ('-fs {' + (renderedProps + '\n}'))))))]);
				default:
					return _List_fromArray(
						[
							selector + ('-act:active {' + (A3(
							$elm$core$List$foldl,
							$mdgriffith$elm_ui$Internal$Model$renderProps(false),
							'',
							props) + '\n}'))
						]);
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return '\"' + (name + '\"');
		case 1:
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + $elm$core$String$fromInt(index)));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$renderVariant, font.ea)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'translate3d(' + ($elm$core$String$fromFloat(x) + ('px, ' + ($elm$core$String$fromFloat(y) + ('px, ' + ($elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + ($elm$core$String$fromFloat(tx) + ('px, ' + ($elm$core$String$fromFloat(ty) + ('px, ' + ($elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + ($elm$core$String$fromFloat(sx) + (', ' + ($elm$core$String$fromFloat(sy) + (', ' + ($elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + ($elm$core$String$fromFloat(ox) + (', ' + ($elm$core$String$fromFloat(oy) + (', ' + ($elm$core$String$fromFloat(oz) + (', ' + ($elm$core$String$fromFloat(angle) + 'rad)')))))));
			return $elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderStyleRule = F3(
	function (options, rule, maybePseudo) {
		switch (rule.$) {
			case 0:
				var selector = rule.a;
				var props = rule.b;
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, selector, props);
			case 13:
				var name = rule.a;
				var prop = rule.b;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
						]));
			case 12:
				var name = rule.a;
				var transparency = rule.b;
				var opacity = A2(
					$elm$core$Basics$max,
					0,
					A2($elm$core$Basics$min, 1, 1 - transparency));
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'opacity',
							$elm$core$String$fromFloat(opacity))
						]));
			case 2:
				var i = rule.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			case 1:
				var name = rule.a;
				var typefaces = rule.b;
				var features = A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
				var families = _List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-family',
						A2(
							$elm$core$String$join,
							', ',
							A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-variant',
						A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
					]);
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, '.' + name, families);
			case 3:
				var _class = rule.a;
				var prop = rule.b;
				var val = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, prop, val)
						]));
			case 4:
				var _class = rule.a;
				var prop = rule.b;
				var color = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							prop,
							$mdgriffith$elm_ui$Internal$Model$formatColor(color))
						]));
			case 5:
				var cls = rule.a;
				var x = rule.b;
				var y = rule.c;
				var yPx = $elm$core$String$fromInt(y) + 'px';
				var xPx = $elm$core$String$fromInt(x) + 'px';
				var single = '.' + $mdgriffith$elm_ui$Internal$Style$classes.gg;
				var row = '.' + $mdgriffith$elm_ui$Internal$Style$classes.dP;
				var wrappedRow = '.' + ($mdgriffith$elm_ui$Internal$Style$classes.cx + row);
				var right = '.' + $mdgriffith$elm_ui$Internal$Style$classes.cC;
				var paragraph = '.' + $mdgriffith$elm_ui$Internal$Style$classes.dA;
				var page = '.' + $mdgriffith$elm_ui$Internal$Style$classes.dz;
				var left = '.' + $mdgriffith$elm_ui$Internal$Style$classes.cB;
				var halfY = $elm$core$String$fromFloat(y / 2) + 'px';
				var halfX = $elm$core$String$fromFloat(x / 2) + 'px';
				var column = '.' + $mdgriffith$elm_ui$Internal$Style$classes.Q;
				var _class = '.' + cls;
				var any = '.' + $mdgriffith$elm_ui$Internal$Style$classes.ev;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (row + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (wrappedRow + (' > ' + any)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (column + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_Utils_ap(_class, paragraph),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							'textarea' + (any + _class),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)')),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'height',
									'calc(100% + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::after'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-top',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::before'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-bottom',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								]))
						]));
			case 7:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'padding',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 6:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'border-width',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 8:
				var template = rule.a;
				var toGridLengthHelper = F3(
					function (minimum, maximum, x) {
						toGridLengthHelper:
						while (true) {
							switch (x.$) {
								case 0:
									var px = x.a;
									return $elm$core$String$fromInt(px) + 'px';
								case 1:
									var _v2 = _Utils_Tuple2(minimum, maximum);
									if (_v2.a.$ === 1) {
										if (_v2.b.$ === 1) {
											var _v3 = _v2.a;
											var _v4 = _v2.b;
											return 'max-content';
										} else {
											var _v6 = _v2.a;
											var maxSize = _v2.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v2.b.$ === 1) {
											var minSize = _v2.a.a;
											var _v5 = _v2.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
										} else {
											var minSize = _v2.a.a;
											var maxSize = _v2.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 2:
									var i = x.a;
									var _v7 = _Utils_Tuple2(minimum, maximum);
									if (_v7.a.$ === 1) {
										if (_v7.b.$ === 1) {
											var _v8 = _v7.a;
											var _v9 = _v7.b;
											return $elm$core$String$fromInt(i) + 'fr';
										} else {
											var _v11 = _v7.a;
											var maxSize = _v7.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v7.b.$ === 1) {
											var minSize = _v7.a.a;
											var _v10 = _v7.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
										} else {
											var minSize = _v7.a.a;
											var maxSize = _v7.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 3:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = $elm$core$Maybe$Just(m),
										$temp$maximum = maximum,
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
								default:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = minimum,
										$temp$maximum = $elm$core$Maybe$Just(m),
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
							}
						}
					});
				var toGridLength = function (x) {
					return A3(toGridLengthHelper, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, x);
				};
				var xSpacing = toGridLength(template.gk.a);
				var ySpacing = toGridLength(template.gk.b);
				var rows = function (x) {
					return 'grid-template-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.f8)));
				var msRows = function (x) {
					return '-ms-grid-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.H)));
				var msColumns = function (x) {
					return '-ms-grid-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.H)));
				var gapY = 'grid-row-gap:' + (toGridLength(template.gk.b) + ';');
				var gapX = 'grid-column-gap:' + (toGridLength(template.gk.a) + ';');
				var columns = function (x) {
					return 'grid-template-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.H)));
				var _class = '.grid-rows-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.f8)) + ('-cols-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.H)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.gk.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.gk.b)))))));
				var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msColumns + (msRows + '}')));
				return _List_fromArray(
					[base, supports]);
			case 9:
				var position = rule.a;
				var msPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'-ms-grid-row: ' + ($elm$core$String$fromInt(position.dP) + ';'),
							'-ms-grid-row-span: ' + ($elm$core$String$fromInt(position.at) + ';'),
							'-ms-grid-column: ' + ($elm$core$String$fromInt(position.cN) + ';'),
							'-ms-grid-column-span: ' + ($elm$core$String$fromInt(position.aL) + ';')
						]));
				var modernPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'grid-row: ' + ($elm$core$String$fromInt(position.dP) + (' / ' + ($elm$core$String$fromInt(position.dP + position.at) + ';'))),
							'grid-column: ' + ($elm$core$String$fromInt(position.cN) + (' / ' + ($elm$core$String$fromInt(position.cN + position.aL) + ';')))
						]));
				var _class = '.grid-pos-' + ($elm$core$String$fromInt(position.dP) + ('-' + ($elm$core$String$fromInt(position.cN) + ('-' + ($elm$core$String$fromInt(position.aL) + ('-' + $elm$core$String$fromInt(position.at)))))));
				var modernGrid = _class + ('{' + (modernPosition + '}'));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msPosition + '}'));
				return _List_fromArray(
					[base, supports]);
			case 11:
				var _class = rule.a;
				var styles = rule.b;
				var renderPseudoRule = function (style) {
					return A3(
						$mdgriffith$elm_ui$Internal$Model$renderStyleRule,
						options,
						style,
						$elm$core$Maybe$Just(_class));
				};
				return A2($elm$core$List$concatMap, renderPseudoRule, styles);
			default:
				var transform = rule.a;
				var val = $mdgriffith$elm_ui$Internal$Model$transformValue(transform);
				var _class = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				var _v12 = _Utils_Tuple2(_class, val);
				if ((!_v12.a.$) && (!_v12.b.$)) {
					var cls = _v12.a.a;
					var v = _v12.b.a;
					return A4(
						$mdgriffith$elm_ui$Internal$Model$renderStyle,
						options,
						maybePseudo,
						'.' + cls,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
							]));
				} else {
					return _List_Nil;
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$encodeStyles = F2(
	function (options, stylesheet) {
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$map,
				function (style) {
					var styled = A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing);
					return _Utils_Tuple2(
						$mdgriffith$elm_ui$Internal$Model$getStyleName(style),
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styled));
				},
				stylesheet));
	});
var $mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_v0) {
			var name = _v0.a;
			var val = _v0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, renderPair, rules)) + '}'));
	});
var $mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _v0) {
		var parentAdj = _v0.a;
		var textAdjustment = _v0.b;
		return _List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.cq + (', .' + (name + (' .' + (modifier + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.cq)))))))))), textAdjustment)
			]);
	});
var $mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _v0, otherFontName) {
		var full = _v0.a;
		var capital = _v0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_Utils_ap(
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.gi, capital),
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.fg, full)));
	});
var $mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.gi + (', ' + ('.' + (name + (' .' + $mdgriffith$elm_ui$Internal$Style$classes.gi))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.gi + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.cq + (', .' + (name + (' .' + ($mdgriffith$elm_ui$Internal$Style$classes.gi + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.cq)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {at: height / size, gh: size, eb: vertical};
	});
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
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.eL, adjustment.ey, adjustment.e2, adjustment.fC]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.e2,
		$elm$core$List$minimum(lines));
	var newBaseline = A2(
		$elm$core$Maybe$withDefault,
		adjustment.ey,
		$elm$core$List$minimum(
			A2(
				$elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.eL,
		$elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		eL: A3($mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		c0: A3($mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var $mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				$elm$core$String$fromFloat(converted.at)),
				_Utils_Tuple2(
				'vertical-align',
				$elm$core$String$fromFloat(converted.eb) + 'em'),
				_Utils_Tuple2(
				'font-size',
				$elm$core$String$fromFloat(converted.gh) + 'em')
			]));
};
var $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 1) {
					if (face.$ === 5) {
						var _with = face.a;
						var _v2 = _with.ek;
						if (_v2.$ === 1) {
							return found;
						} else {
							var adjustment = _v2.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.c0;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.eL;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		$elm$core$Maybe$Nothing,
		typefaces);
};
var $mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 4) {
			var url = font.b;
			return $elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_v2) {
		var name = _v2.a;
		var typefaces = _v2.b;
		var imports = A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2($elm$core$List$map, $elm$core$Tuple$first, rules);
	var fontAdjustments = function (_v1) {
		var name = _v1.a;
		var typefaces = _v1.b;
		var _v0 = $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_v0.$ === 1) {
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _v0.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					A2($mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontImports, rules)),
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontAdjustments, rules)));
};
var $mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 1) {
		var name = rule.a;
		var typefaces = rule.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var combine = F2(
			function (style, rendered) {
				return {
					bp: _Utils_ap(
						rendered.bp,
						A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing)),
					aY: function () {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_v1.$ === 1) {
							return rendered.aY;
						} else {
							var topLevel = _v1.a;
							return A2($elm$core$List$cons, topLevel, rendered.aY);
						}
					}()
				};
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			combine,
			{bp: _List_Nil, aY: _List_Nil},
			stylesheet);
		var topLevel = _v0.aY;
		var rules = _v0.bp;
		return _Utils_ap(
			$mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			$elm$core$String$concat(rules));
	});
var $mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		var _v0 = options.fI;
		switch (_v0) {
			case 0:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			case 1:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			default:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'elm-ui-rules',
					_List_fromArray(
						[
							A2(
							$elm$virtual_dom$VirtualDom$property,
							'rules',
							A2($mdgriffith$elm_ui$Internal$Model$encodeStyles, options, styleSheet))
						]),
					_List_Nil);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ff)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'static-stylesheet',
				$mdgriffith$elm_ui$Internal$Model$staticRoot(opts)),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
				children)) : A2(
			$elm$core$List$cons,
			_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
			children);
	});
var $mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ff)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			$mdgriffith$elm_ui$Internal$Model$staticRoot(opts),
			A2($elm$core$List$cons, dynamicStyleSheet, children)) : A2($elm$core$List$cons, dynamicStyleSheet, children);
	});
var $mdgriffith$elm_ui$Internal$Flag$heightBetween = $mdgriffith$elm_ui$Internal$Flag$flag(45);
var $mdgriffith$elm_ui$Internal$Flag$heightFill = $mdgriffith$elm_ui$Internal$Flag$flag(37);
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$core$Basics$not = _Basics_not;
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$core$Bitwise$and = _Bitwise_and;
var $mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _v0) {
		var fieldOne = _v0.a;
		var fieldTwo = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var $elm$html$Html$s = _VirtualDom_node('s');
var $elm$html$Html$u = _VirtualDom_node('u');
var $mdgriffith$elm_ui$Internal$Flag$widthBetween = $mdgriffith$elm_ui$Internal$Flag$flag(44);
var $mdgriffith$elm_ui$Internal$Flag$widthFill = $mdgriffith$elm_ui$Internal$Flag$flag(39);
var $mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 1) {
					var keyed = children.a;
					return A3(
						$elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return keyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return $elm$html$Html$div;
								case 'p':
									return $elm$html$Html$p;
								default:
									return $elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return unkeyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 0:
					return A2(createNode, 'div', attributes);
				case 1:
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						$elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.gg))
									]))
							]));
			}
		}();
		switch (parentContext) {
			case 0:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ev, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.R, $mdgriffith$elm_ui$Internal$Style$classes.er])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ev, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.R, $mdgriffith$elm_ui$Internal$Style$classes.ep])))
						]),
					_List_fromArray(
						[html])) : html));
			case 1:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ev, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.eq])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ev, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.eo])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $mdgriffith$elm_ui$Internal$Model$textElementClasses = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.cq + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.cv + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bF)))));
var $mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$textElementFillClasses = $mdgriffith$elm_ui$Internal$Style$classes.ev + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.cq + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.cw + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bG)))));
var $mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementFillClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_v8, _v9) {
				var key = _v8.a;
				var child = _v8.b;
				var htmls = _v9.a;
				var existingStyles = _v9.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.fl, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.d$ : _Utils_ap(styled.d$, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.fl, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.d$ : _Utils_ap(styled.d$, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _v6) {
				var htmls = _v6.a;
				var existingStyles = _v6.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.fl, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.d$ : _Utils_ap(styled.d$, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.fl, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.d$ : _Utils_ap(styled.d$, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 1) {
			var keyedChildren = children.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _v1.a;
			var styles = _v1.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.d$ : _Utils_ap(rendered.d$, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.ad,
						rendered.ds,
						rendered.W,
						$mdgriffith$elm_ui$Internal$Model$Keyed(
							A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.eO)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						fl: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.ad,
							rendered.ds,
							rendered.W,
							$mdgriffith$elm_ui$Internal$Model$Keyed(
								A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.eO))),
						d$: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _v3 = A3(
				$elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _v3.a;
			var styles = _v3.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.d$ : _Utils_ap(rendered.d$, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.ad,
						rendered.ds,
						rendered.W,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.eO)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						fl: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.ad,
							rendered.ds,
							rendered.W,
							$mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.eO))),
						d$: allStyles
					});
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 10, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					function () {
						switch (location) {
							case 0:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.av, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.ei]));
							case 1:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.av, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.eA]));
							case 2:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.av, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.fX]));
							case 3:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.av, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.fV]));
							case 4:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.av, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.fp]));
							default:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.av, $mdgriffith$elm_ui$Internal$Style$classes.gg, $mdgriffith$elm_ui$Internal$Style$classes.ez]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 3:
							return $elm$virtual_dom$VirtualDom$text('');
						case 2:
							var str = elem.a;
							return $mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 0:
							var html = elem.a;
							return html($mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.fl, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, $mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2($mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 0:
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 1:
				var existingBehind = existing.a;
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2($elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 2:
				var existingInFront = existing.a;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2($elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2($elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2($elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 0:
				return $mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 1:
				var name = old.a;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.bu + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cB);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.bu + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cC);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.bu + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.em);
	}
};
var $mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.bv + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.es);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.bv + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.el);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.bv + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.en);
	}
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 0:
				switch (component.$) {
					case 0:
						var x = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 1:
						var y = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 2:
						var z = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 1:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 1:
						var newY = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 2:
						var newZ = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 1:
						var newY = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 2:
						var newZ = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 3:
						var newMove = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 4:
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$height = $mdgriffith$elm_ui$Internal$Flag$flag(7);
var $mdgriffith$elm_ui$Internal$Flag$heightContent = $mdgriffith$elm_ui$Internal$Flag$flag(36);
var $mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_v0, _v1) {
		var one = _v0.a;
		var two = _v0.b;
		var three = _v1.a;
		var four = _v1.b;
		return A2($mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var $mdgriffith$elm_ui$Internal$Flag$none = A2($mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var $mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 0:
			var px = h.a;
			var val = $elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.c3 + (' ' + name),
				_List_fromArray(
					[
						A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bF,
				_List_Nil);
		case 2:
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bG,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.c4 + (' height-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.ev + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.Q + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$widthContent = $mdgriffith$elm_ui$Internal$Flag$flag(38);
var $mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 0:
			var px = w.a;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.ee + (' width-px-' + $elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + $elm$core$String$fromInt(px),
						'width',
						$elm$core$String$fromInt(px) + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.cv,
				_List_Nil);
		case 2:
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.cw,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.ef + (' width-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.ev + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.dP + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$borderWidth = $mdgriffith$elm_ui$Internal$Flag$flag(27);
var $elm$core$Basics$ge = _Utils_ge;
var $mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, $mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 3) {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 2:
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 7:
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$width = $mdgriffith$elm_ui$Internal$Flag$flag(6);
var $mdgriffith$elm_ui$Internal$Flag$xAlign = $mdgriffith$elm_ui$Internal$Flag$flag(30);
var $mdgriffith$elm_ui$Internal$Flag$yAlign = $mdgriffith$elm_ui$Internal$Flag$flag(29);
var $mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _v1 = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_v1.$ === 1) {
					return {
						W: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes),
							attrs),
						eO: children,
						ad: has,
						ds: node,
						d$: styles
					};
				} else {
					var _class = _v1.a;
					return {
						W: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						eO: children,
						ad: has,
						ds: node,
						d$: A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 0:
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 3:
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 1:
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2($elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 4:
						var flag = attribute.a;
						var style = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2($mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2($elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 10:
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2($mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 7:
						var width = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 0:
									var px = width.a;
									var $temp$classes = ($mdgriffith$elm_ui$Internal$Style$classes.ee + (' width-px-' + $elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3(
											$mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + $elm$core$String$fromInt(px),
											'width',
											$elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cv),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cw),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.ef + (' width-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.ev + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.dP + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v4 = $mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _v4.a;
									var newClass = _v4.b;
									var newStyles = _v4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 8:
						var height = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 0:
									var px = height.a;
									var val = $elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.c3 + (' ' + (name + (' ' + classes))),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.bF + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.bG + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.c4 + (' height-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.ev + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.Q + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v6 = $mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _v6.a;
									var newClass = _v6.b;
									var newStyles = _v6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 2:
						var description = attribute.a;
						switch (description.$) {
							case 0:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 1:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 2:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 3:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 4:
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											$mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + $elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 9:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 8:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 5:
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 6:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 9:
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 3:
									return styles;
								case 2:
									var str = elem.a;
									return styles;
								case 0:
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.d$);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3($mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 6:
						var x = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$untransformed = $mdgriffith$elm_ui$Internal$Model$Untransformed;
var $mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			$mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				$mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				$mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				$mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				$elm$core$List$reverse(attributes)));
	});
var $mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 8, a: a};
};
var $mdgriffith$elm_ui$Element$height = $mdgriffith$elm_ui$Internal$Model$Height;
var $mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		$elm$html$Html$Attributes$class(cls));
};
var $mdgriffith$elm_ui$Internal$Model$Content = {$: 1};
var $mdgriffith$elm_ui$Element$shrink = $mdgriffith$elm_ui$Internal$Model$Content;
var $mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 7, a: a};
};
var $mdgriffith$elm_ui$Element$width = $mdgriffith$elm_ui$Internal$Model$Width;
var $mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.eV + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aS)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$fill = $mdgriffith$elm_ui$Internal$Model$Fill(1);
var $mdgriffith$elm_ui$Element$htmlAttribute = $mdgriffith$elm_ui$Internal$Model$Attr;
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$AllowHover = 1;
var $mdgriffith$elm_ui$Internal$Model$Layout = 0;
var $mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	ex: $elm$core$Maybe$Nothing,
	eD: $elm$core$Maybe$Nothing,
	ge: $elm$core$Maybe$Just(
		{
			cG: 0,
			cO: A4($mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			b: _Utils_Tuple2(0, 0),
			gh: 3
		})
};
var $mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 0:
					var hoverable = opt.a;
					var _v4 = record.fk;
					if (_v4.$ === 1) {
						return _Utils_update(
							record,
							{
								fk: $elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 1:
					var focusStyle = opt.a;
					var _v5 = record.ff;
					if (_v5.$ === 1) {
						return _Utils_update(
							record,
							{
								ff: $elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _v6 = record.fI;
					if (_v6.$ === 1) {
						return _Utils_update(
							record,
							{
								fI: $elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			ff: function () {
				var _v0 = record.ff;
				if (_v0.$ === 1) {
					return $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _v0.a;
					return focusable;
				}
			}(),
			fk: function () {
				var _v1 = record.fk;
				if (_v1.$ === 1) {
					return 1;
				} else {
					var hoverable = _v1.a;
					return hoverable;
				}
			}(),
			fI: function () {
				var _v2 = record.fI;
				if (_v2.$ === 1) {
					return 0;
				} else {
					var actualMode = _v2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			$elm$core$List$foldr,
			combine,
			{ff: $elm$core$Maybe$Nothing, fk: $elm$core$Maybe$Nothing, fI: $elm$core$Maybe$Nothing},
			options));
};
var $mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 0:
				var html = el.a;
				return html($mdgriffith$elm_ui$Internal$Model$asEl);
			case 1:
				var styles = el.a.d$;
				var html = el.a.fl;
				return A2(
					html,
					mode(styles),
					$mdgriffith$elm_ui$Internal$Model$asEl);
			case 2:
				var text = el.a;
				return $mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return $mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = $mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _v0 = options.fI;
			if (_v0 === 1) {
				return $mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var $mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 3, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$bgColor = $mdgriffith$elm_ui$Internal$Flag$flag(8);
var $mdgriffith$elm_ui$Internal$Flag$fontColor = $mdgriffith$elm_ui$Internal$Flag$flag(14);
var $mdgriffith$elm_ui$Internal$Flag$fontFamily = $mdgriffith$elm_ui$Internal$Flag$flag(5);
var $mdgriffith$elm_ui$Internal$Flag$fontSize = $mdgriffith$elm_ui$Internal$Flag$flag(4);
var $mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return $mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var $elm$core$String$toLower = _String_toLower;
var $elm$core$String$words = _String_words;
var $mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 0:
						return 'serif';
					case 1:
						return 'sans-serif';
					case 2:
						return 'monospace';
					case 3:
						var name = font.a;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					case 4:
						var name = font.a;
						var url = font.b;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					default:
						var name = font.a.fL;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
				}
			}());
	});
var $mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			$mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			$mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontSize,
			$mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				$mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var $mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_v0, attrs, child) {
		var options = _v0.f$;
		return A3(
			$mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[$mdgriffith$elm_ui$Internal$Style$classes.f7, $mdgriffith$elm_ui$Internal$Style$classes.ev, $mdgriffith$elm_ui$Internal$Style$classes.gg]))),
				_Utils_ap($mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var $mdgriffith$elm_ui$Element$layout = $mdgriffith$elm_ui$Element$layoutWith(
	{f$: _List_Nil});
var $author$project$PressurePlot$Point = F2(
	function (x, y) {
		return {aN: x, aO: y};
	});
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
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
var $terezka$line_charts$LineChart$Colors$blue = A3($avh4$elm_color$Color$rgb255, 3, 169, 244);
var $author$project$PressurePlot$floatIndexedMap = F2(
	function (f, li) {
		return A2(
			$elm$core$List$indexedMap,
			F2(
				function (i, a) {
					return A2(f, i, a);
				}),
			li);
	});
var $author$project$Vector3$Line = F2(
	function (point, dir) {
		return {bz: dir, f3: point};
	});
var $author$project$Vector3$Plane = F2(
	function (point, normal) {
		return {dt: normal, f3: point};
	});
var $author$project$PenTilt$Spherical = F3(
	function (r, theta, phi) {
		return {dC: phi, bo: r, d4: theta};
	});
var $author$project$Vector2$Vector2 = F2(
	function (x, y) {
		return {aN: x, aO: y};
	});
var $elm$core$Basics$atan2 = _Basics_atan2;
var $author$project$Vector2$cross = F2(
	function (a, b) {
		return (a.aN * b.aO) - (a.aO * b.aN);
	});
var $author$project$Vector2$map2 = F3(
	function (f, v1, v2) {
		return A2(
			$author$project$Vector2$Vector2,
			A2(f, v1.aN, v2.aN),
			A2(f, v1.aO, v2.aO));
	});
var $author$project$Vector2$sumComponents = function (_v0) {
	var x = _v0.aN;
	var y = _v0.aO;
	return x + y;
};
var $author$project$Vector2$dot = F2(
	function (a, b) {
		return $author$project$Vector2$sumComponents(
			A3($author$project$Vector2$map2, $elm$core$Basics$mul, a, b));
	});
var $author$project$Vector2$angleBetween = F2(
	function (a, b) {
		var abSinAlpha = A2($author$project$Vector2$cross, a, b);
		var abCosAlpha = A2($author$project$Vector2$dot, a, b);
		return A2($elm$core$Basics$atan2, abSinAlpha, abCosAlpha);
	});
var $elm$core$Basics$atan = _Basics_atan;
var $author$project$Vector2$map = F2(
	function (f, v) {
		return A2(
			$author$project$Vector2$Vector2,
			f(v.aN),
			f(v.aO));
	});
var $author$project$Vector2$negate = $author$project$Vector2$map(
	function (c) {
		return -c;
	});
var $author$project$Vector3$Vector3 = F3(
	function (x, y, z) {
		return {aN: x, aO: y, ao: z};
	});
var $author$project$Vector3$map2 = F3(
	function (f, a, b) {
		return A3(
			$author$project$Vector3$Vector3,
			A2(f, a.aN, b.aN),
			A2(f, a.aO, b.aO),
			A2(f, a.ao, b.ao));
	});
var $author$project$Vector3$add = $author$project$Vector3$map2($elm$core$Basics$add);
var $author$project$Vector3$foldl = F3(
	function (f, inital, _v0) {
		var x = _v0.aN;
		var y = _v0.aO;
		var z = _v0.ao;
		return A3(
			$elm$core$List$foldl,
			f,
			inital,
			_List_fromArray(
				[x, y, z]));
	});
var $author$project$Vector3$sumComponents = A2($author$project$Vector3$foldl, $elm$core$Basics$add, 0);
var $author$project$Vector3$dot = F2(
	function (a, b) {
		return $author$project$Vector3$sumComponents(
			A3($author$project$Vector3$map2, $elm$core$Basics$mul, a, b));
	});
var $author$project$Vector3$subtract = $author$project$Vector3$map2($elm$core$Basics$sub);
var $author$project$Vector3$rel = F2(
	function (a, b) {
		return A2($author$project$Vector3$subtract, b, a);
	});
var $author$project$Vector3$map = F2(
	function (f, _v0) {
		var x = _v0.aN;
		var y = _v0.aO;
		var z = _v0.ao;
		return A3(
			$author$project$Vector3$Vector3,
			f(x),
			f(y),
			f(z));
	});
var $author$project$Vector3$scale = function (scaleFactor) {
	return $author$project$Vector3$map(
		$elm$core$Basics$mul(scaleFactor));
};
var $author$project$Vector3$pointFromLinePlaneIntersection = F2(
	function (line, plane) {
		var decentRate = A2($author$project$Vector3$dot, plane.dt, line.bz);
		var altitude = A2(
			$author$project$Vector3$dot,
			plane.dt,
			A2($author$project$Vector3$rel, line.f3, plane.f3));
		var t = altitude / decentRate;
		return (!(!decentRate)) ? $elm$core$Maybe$Just(
			A2(
				$author$project$Vector3$add,
				line.f3,
				A2($author$project$Vector3$scale, t, line.bz))) : $elm$core$Maybe$Nothing;
	});
var $author$project$PenTilt$Cartesian = F3(
	function (x, y, z) {
		return {aN: x, aO: y, ao: z};
	});
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$sin = _Basics_sin;
var $author$project$PenTilt$spherical_to_cartesian = function (_v0) {
	var r = _v0.bo;
	var theta = _v0.d4;
	var phi = _v0.dC;
	var z = r * $elm$core$Basics$cos(theta);
	var l = r * $elm$core$Basics$sin(theta);
	var x = l * $elm$core$Basics$cos(phi);
	var y = l * $elm$core$Basics$sin(phi);
	return A3($author$project$PenTilt$Cartesian, x, y, z);
};
var $author$project$Vector3$zero = A3($author$project$Vector3$Vector3, 0, 0, 0);
var $author$project$PressurePlot$frictionCorrectedPressure = F3(
	function (mu, v, uncorrectedPressure) {
		var pressurePlane = A2($author$project$Vector3$Plane, uncorrectedPressure, uncorrectedPressure);
		var netForceLine = A2(
			$author$project$Vector3$Line,
			$author$project$Vector3$zero,
			$author$project$PenTilt$spherical_to_cartesian(
				A3(
					$author$project$PenTilt$Spherical,
					1,
					$elm$core$Basics$atan(mu),
					A2(
						$author$project$Vector2$angleBetween,
						A2($author$project$Vector2$Vector2, 1, 0),
						$author$project$Vector2$negate(v)))));
		return A2(
			$elm$core$Maybe$withDefault,
			$author$project$Vector3$zero,
			A2($author$project$Vector3$pointFromLinePlaneIntersection, netForceLine, pressurePlane));
	});
var $terezka$line_charts$LineChart$Colors$green = A3($avh4$elm_color$Color$rgb255, 67, 160, 71);
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $mdgriffith$elm_ui$Internal$Model$unstyled = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Unstyled, $elm$core$Basics$always);
var $mdgriffith$elm_ui$Element$html = $mdgriffith$elm_ui$Internal$Model$unstyled;
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Vector2$lengthSquared = function (a) {
	return A2($author$project$Vector2$dot, a, a);
};
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$Vector2$length = A2($elm$core$Basics$composeR, $author$project$Vector2$lengthSquared, $elm$core$Basics$sqrt);
var $author$project$Vector3$lengthSquared = function (a) {
	return A2($author$project$Vector3$dot, a, a);
};
var $author$project$Vector3$length = A2($elm$core$Basics$composeR, $author$project$Vector3$lengthSquared, $elm$core$Basics$sqrt);
var $terezka$line_charts$Internal$Line$Series = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Line$SeriesConfig = F5(
	function (color, shape, dashing, label, data) {
		return {cO: color, cQ: dashing, _: data, be: label, dW: shape};
	});
var $terezka$line_charts$Internal$Line$line = F4(
	function (color_, shape_, label_, data_) {
		return A5($terezka$line_charts$Internal$Line$SeriesConfig, color_, shape_, _List_Nil, label_, data_);
	});
var $terezka$line_charts$LineChart$line = $terezka$line_charts$Internal$Line$line;
var $terezka$line_charts$Internal$Dots$None = 0;
var $terezka$line_charts$LineChart$Dots$none = 0;
var $author$project$PressurePlot$pressureVector3d = function (_v0) {
	var pressure = _v0.aC;
	var altitudeAngle = _v0.aq;
	var azimuthAngle = _v0.ar;
	return $author$project$PenTilt$spherical_to_cartesian(
		{dC: azimuthAngle, bo: pressure, d4: ($elm$core$Basics$pi / 2) - altitudeAngle});
};
var $terezka$line_charts$LineChart$Colors$red = A3($avh4$elm_color$Color$rgb255, 216, 27, 96);
var $author$project$Vector2$add = $author$project$Vector2$map2($elm$core$Basics$add);
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$map3 = _List_map3;
var $author$project$Vector2$subtract = $author$project$Vector2$map2($elm$core$Basics$sub);
var $author$project$Vector2$rel = F2(
	function (a, b) {
		return A2($author$project$Vector2$subtract, b, a);
	});
var $author$project$Vector2$scale = function (s) {
	return $author$project$Vector2$map(
		$elm$core$Basics$mul(s));
};
var $author$project$PressurePlot$velocity2d = function (events) {
	var p1 = A2(
		$elm$core$List$map,
		function (e) {
			return {aN: e.aD, aO: e.aE};
		},
		events);
	var p2 = A2($elm$core$List$drop, 1, p1);
	var p3 = A2($elm$core$List$drop, 2, p1);
	var vp2 = A4(
		$elm$core$List$map3,
		F3(
			function (a, b, c) {
				return A2(
					$author$project$Vector2$scale,
					1 / 2,
					A2(
						$author$project$Vector2$add,
						A2($author$project$Vector2$rel, a, b),
						A2($author$project$Vector2$rel, b, c)));
			}),
		p1,
		p2,
		p3);
	var curtaledEvents = $elm$core$List$reverse(
		A2(
			$elm$core$List$drop,
			1,
			$elm$core$List$reverse(
				A2($elm$core$List$drop, 1, events))));
	return _Utils_Tuple2(vp2, curtaledEvents);
};
var $terezka$line_charts$Internal$Area$None = {$: 0};
var $terezka$line_charts$Internal$Area$none = $terezka$line_charts$Internal$Area$None;
var $terezka$line_charts$LineChart$Area$default = $terezka$line_charts$Internal$Area$none;
var $terezka$line_charts$Internal$Axis$Values$Around = function (a) {
	return {$: 1, a: a};
};
var $terezka$line_charts$Internal$Axis$Values$around = $terezka$line_charts$Internal$Axis$Values$Around;
var $terezka$line_charts$Internal$Axis$Title$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Axis$Title$custom = F4(
	function (position, x, y, title) {
		return {
			b: _Utils_Tuple2(x, y),
			bn: position,
			gV: title
		};
	});
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$tspan = $elm$svg$Svg$trustedNode('tspan');
var $terezka$line_charts$Internal$Svg$label = F2(
	function (color, string) {
		return A2(
			$elm$svg$Svg$text_,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill(color),
					$elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$tspan,
					_List_Nil,
					_List_fromArray(
						[
							$elm$svg$Svg$text(string)
						]))
				]));
	});
var $terezka$line_charts$Internal$Axis$Title$atPosition = F3(
	function (position, x, y) {
		return A2(
			$elm$core$Basics$composeL,
			A3($terezka$line_charts$Internal$Axis$Title$custom, position, x, y),
			$terezka$line_charts$Internal$Svg$label('inherit'));
	});
var $terezka$line_charts$Internal$Axis$Title$atDataMax = function () {
	var position = F2(
		function (data, range) {
			return A2($elm$core$Basics$min, data.dm, range.dm);
		});
	return $terezka$line_charts$Internal$Axis$Title$atPosition(position);
}();
var $terezka$line_charts$Internal$Axis$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Axis$custom = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Axis$Ticks$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Axis$Ticks$custom = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Axis$Values$ceilingTo = F2(
	function (prec, number) {
		return prec * $elm$core$Basics$ceiling(number / prec);
	});
var $terezka$line_charts$Internal$Axis$Values$getBeginning = F2(
	function (min, interval) {
		var multiple = min / interval;
		return _Utils_eq(
			multiple,
			$elm$core$Basics$round(multiple)) ? min : A2($terezka$line_charts$Internal$Axis$Values$ceilingTo, interval, min);
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			$elm$core$List$any,
			function (c) {
				return (c !== '0') && (c !== '.');
			},
			$elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$Char$fromCode = _Char_fromCode;
var $myrho$elm_round$Round$increaseNum = function (_v0) {
	var head = _v0.a;
	var tail = _v0.b;
	if (head === '9') {
		var _v1 = $elm$core$String$uncons(tail);
		if (_v1.$ === 1) {
			return '01';
		} else {
			var headtail = _v1.a;
			return A2(
				$elm$core$String$cons,
				'0',
				$myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = $elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			$elm$core$String$cons,
			$elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$String$reverse = _String_reverse;
var $myrho$elm_round$Round$splitComma = function (str) {
	var _v0 = A2($elm$core$String$split, '.', str);
	if (_v0.b) {
		if (_v0.b.b) {
			var before = _v0.a;
			var _v1 = _v0.b;
			var after = _v1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _v0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var $myrho$elm_round$Round$toDecimal = function (fl) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(
			$elm$core$Basics$abs(fl)));
	if (_v0.b) {
		if (_v0.b.b) {
			var num = _v0.a;
			var _v1 = _v0.b;
			var exp = _v1.a;
			var e = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(
					A2($elm$core$String$startsWith, '+', exp) ? A2($elm$core$String$dropLeft, 1, exp) : exp));
			var _v2 = $myrho$elm_round$Round$splitComma(num);
			var before = _v2.a;
			var after = _v2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					function (_v3) {
						var a = _v3.a;
						var b = _v3.b;
						return a + ('.' + b);
					},
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$mapFirst($elm$core$String$fromChar),
						$elm$core$String$uncons(
							_Utils_ap(
								A2(
									$elm$core$String$repeat,
									$elm$core$Basics$abs(e),
									'0'),
								total))))) : A3($elm$core$String$padRight, e + 1, '0', total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _v0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var $myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if ($elm$core$Basics$isInfinite(fl) || $elm$core$Basics$isNaN(fl)) {
			return $elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _v0 = $myrho$elm_round$Round$splitComma(
				$myrho$elm_round$Round$toDecimal(
					$elm$core$Basics$abs(fl)));
			var before = _v0.a;
			var after = _v0.b;
			var r = $elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2($elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					$elm$core$String$padRight,
					r,
					'0',
					_Utils_ap(before, after)));
			var totalLen = $elm$core$String$length(normalized);
			var roundDigitIndex = A2($elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3($elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3($elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? $elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'1',
					A2(
						$elm$core$Maybe$map,
						$myrho$elm_round$Round$increaseNum,
						$elm$core$String$uncons(
							$elm$core$String$reverse(remains))))) : remains;
			var numLen = $elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					$elm$core$String$repeat,
					$elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				$elm$core$String$length(after)) < 0) ? (A3($elm$core$String$slice, 0, numLen - s, num) + ('.' + A3($elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3($elm$core$String$padRight, s, '0', after))));
			return A2($myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var $myrho$elm_round$Round$round = $myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _v0 = $elm$core$String$uncons(str);
			if (_v0.$ === 1) {
				return false;
			} else {
				if ('5' === _v0.a.a) {
					if (_v0.a.b === '') {
						var _v1 = _v0.a;
						return !signed;
					} else {
						var _v2 = _v0.a;
						return true;
					}
				} else {
					var _v3 = _v0.a;
					var _int = _v3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						$elm$core$Char$toCode(_int));
				}
			}
		}));
var $elm$core$String$toFloat = _String_toFloat;
var $terezka$line_charts$Internal$Axis$Values$correctFloat = function (prec) {
	return A2(
		$elm$core$Basics$composeR,
		$myrho$elm_round$Round$round(prec),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$toFloat,
			$elm$core$Maybe$withDefault(0)));
};
var $terezka$line_charts$Internal$Axis$Values$getMultiples = F3(
	function (magnitude, allowDecimals, hasTickAmount) {
		var defaults = hasTickAmount ? _List_fromArray(
			[1, 1.2, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10]) : _List_fromArray(
			[1, 2, 2.5, 5, 10]);
		return allowDecimals ? defaults : ((magnitude === 1) ? A2(
			$elm$core$List$filter,
			function (n) {
				return _Utils_eq(
					$elm$core$Basics$round(n),
					n);
			},
			defaults) : ((magnitude <= 0.1) ? _List_fromArray(
			[1 / magnitude]) : defaults));
	});
var $terezka$line_charts$Internal$Axis$Values$getPrecision = function (number) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(number));
	if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
		var before = _v0.a;
		var _v1 = _v0.b;
		var after = _v1.a;
		return $elm$core$Basics$abs(
			A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(after)));
	} else {
		var _v2 = A2(
			$elm$core$String$split,
			'.',
			$elm$core$String$fromFloat(number));
		if ((_v2.b && _v2.b.b) && (!_v2.b.b.b)) {
			var before = _v2.a;
			var _v3 = _v2.b;
			var after = _v3.a;
			return $elm$core$String$length(after);
		} else {
			return 0;
		}
	}
};
var $elm$core$Basics$e = _Basics_e;
var $elm$core$Basics$pow = _Basics_pow;
var $terezka$line_charts$Internal$Utils$magnitude = function (num) {
	return A2(
		$elm$core$Basics$pow,
		10,
		$elm$core$Basics$floor(
			A2($elm$core$Basics$logBase, $elm$core$Basics$e, num) / A2($elm$core$Basics$logBase, $elm$core$Basics$e, 10)));
};
var $terezka$line_charts$Internal$Axis$Values$getInterval = F3(
	function (intervalRaw, allowDecimals, hasTickAmount) {
		var magnitude = $terezka$line_charts$Internal$Utils$magnitude(intervalRaw);
		var multiples = A3($terezka$line_charts$Internal$Axis$Values$getMultiples, magnitude, allowDecimals, hasTickAmount);
		var normalized = intervalRaw / magnitude;
		var findMultipleExact = function (multiples_) {
			findMultipleExact:
			while (true) {
				if (multiples_.b) {
					var m1 = multiples_.a;
					var rest = multiples_.b;
					if (_Utils_cmp(m1 * magnitude, intervalRaw) > -1) {
						return m1;
					} else {
						var $temp$multiples_ = rest;
						multiples_ = $temp$multiples_;
						continue findMultipleExact;
					}
				} else {
					return 1;
				}
			}
		};
		var findMultiple = function (multiples_) {
			findMultiple:
			while (true) {
				if (multiples_.b) {
					if (multiples_.b.b) {
						var m1 = multiples_.a;
						var _v2 = multiples_.b;
						var m2 = _v2.a;
						var rest = _v2.b;
						if (_Utils_cmp(normalized, (m1 + m2) / 2) < 1) {
							return m1;
						} else {
							var $temp$multiples_ = A2($elm$core$List$cons, m2, rest);
							multiples_ = $temp$multiples_;
							continue findMultiple;
						}
					} else {
						var m1 = multiples_.a;
						var rest = multiples_.b;
						if (_Utils_cmp(normalized, m1) < 1) {
							return m1;
						} else {
							var $temp$multiples_ = rest;
							multiples_ = $temp$multiples_;
							continue findMultiple;
						}
					}
				} else {
					return 1;
				}
			}
		};
		var multiple = hasTickAmount ? findMultipleExact(multiples) : findMultiple(multiples);
		var precision = $terezka$line_charts$Internal$Axis$Values$getPrecision(magnitude) + $terezka$line_charts$Internal$Axis$Values$getPrecision(multiple);
		return A2($terezka$line_charts$Internal$Axis$Values$correctFloat, precision, multiple * magnitude);
	});
var $terezka$line_charts$Internal$Axis$Values$positions = F5(
	function (range, beginning, interval, m, acc) {
		positions:
		while (true) {
			var next = A2(
				$terezka$line_charts$Internal$Axis$Values$correctFloat,
				$terezka$line_charts$Internal$Axis$Values$getPrecision(interval),
				beginning + (m * interval));
			if (_Utils_cmp(next, range.dm) > 0) {
				return acc;
			} else {
				var $temp$range = range,
					$temp$beginning = beginning,
					$temp$interval = interval,
					$temp$m = m + 1,
					$temp$acc = _Utils_ap(
					acc,
					_List_fromArray(
						[next]));
				range = $temp$range;
				beginning = $temp$beginning;
				interval = $temp$interval;
				m = $temp$m;
				acc = $temp$acc;
				continue positions;
			}
		}
	});
var $terezka$line_charts$Internal$Axis$Values$values = F4(
	function (allowDecimals, exact, amountRough, range) {
		var intervalRough = (range.dm - range.bW) / amountRough;
		var interval = A3($terezka$line_charts$Internal$Axis$Values$getInterval, intervalRough, allowDecimals, exact);
		var intervalSafe = (!interval) ? 1 : interval;
		var beginning = A2($terezka$line_charts$Internal$Axis$Values$getBeginning, range.bW, intervalSafe);
		var amountRoughSafe = (!amountRough) ? 1 : amountRough;
		return A5($terezka$line_charts$Internal$Axis$Values$positions, range, beginning, intervalSafe, 0, _List_Nil);
	});
var $terezka$line_charts$Internal$Axis$Values$float = function (amount) {
	if (!amount.$) {
		var amount_ = amount.a;
		return A3($terezka$line_charts$Internal$Axis$Values$values, true, true, amount_);
	} else {
		var amount_ = amount.a;
		return A3($terezka$line_charts$Internal$Axis$Values$values, true, false, amount_);
	}
};
var $terezka$line_charts$Internal$Axis$Tick$Negative = 0;
var $terezka$line_charts$Internal$Axis$Tick$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Axis$Tick$custom = $elm$core$Basics$identity;
var $terezka$line_charts$LineChart$Colors$gray = A3($avh4$elm_color$Color$rgb255, 163, 163, 163);
var $terezka$line_charts$Internal$Axis$Tick$float = function (n) {
	return $terezka$line_charts$Internal$Axis$Tick$custom(
		{
			cO: $terezka$line_charts$LineChart$Colors$gray,
			bA: 0,
			fh: true,
			be: $elm$core$Maybe$Just(
				A2(
					$terezka$line_charts$Internal$Svg$label,
					'inherit',
					$elm$core$String$fromFloat(n))),
			bR: 5,
			bn: n,
			aL: 1
		});
};
var $terezka$line_charts$LineChart$Axis$Tick$float = $terezka$line_charts$Internal$Axis$Tick$float;
var $terezka$line_charts$Internal$Axis$Range$Padded = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $terezka$line_charts$Internal$Axis$Range$padded = $terezka$line_charts$Internal$Axis$Range$Padded;
var $terezka$line_charts$Internal$Axis$Line$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Axis$Line$custom = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Coordinate$smallestRange = F2(
	function (data, range_) {
		return {
			dm: A2($elm$core$Basics$min, data.dm, range_.dm),
			bW: A2($elm$core$Basics$max, data.bW, range_.bW)
		};
	});
var $terezka$line_charts$Internal$Axis$Line$rangeFrame = function (color) {
	return $terezka$line_charts$Internal$Axis$Line$custom(
		F2(
			function (data, range) {
				var smallest = A2($terezka$line_charts$Internal$Coordinate$smallestRange, data, range);
				return {cO: color, cW: smallest.dm, f: _List_Nil, cn: smallest.bW, aL: 1};
			}));
};
var $terezka$line_charts$Internal$Axis$default = F3(
	function (pixels_, title_, variable_) {
		return $terezka$line_charts$Internal$Axis$custom(
			{
				P: $terezka$line_charts$Internal$Axis$Line$rangeFrame($terezka$line_charts$LineChart$Colors$gray),
				ah: pixels_,
				ai: A2($terezka$line_charts$Internal$Axis$Range$padded, 20, 20),
				s: $terezka$line_charts$Internal$Axis$Ticks$custom(
					F2(
						function (data, range_) {
							var smallest = A2($terezka$line_charts$Internal$Coordinate$smallestRange, data, range_);
							var rangeSmall = smallest.dm - smallest.bW;
							var rangeLong = range_.dm - range_.bW;
							var diff = 1 - ((rangeLong - rangeSmall) / rangeLong);
							var amount = $elm$core$Basics$round((diff * pixels_) / 90);
							return A2(
								$elm$core$List$map,
								$terezka$line_charts$LineChart$Axis$Tick$float,
								A2(
									$terezka$line_charts$Internal$Axis$Values$float,
									$terezka$line_charts$Internal$Axis$Values$around(amount),
									smallest));
						})),
				gJ: A3($terezka$line_charts$Internal$Axis$Title$atDataMax, 0, 0, title_),
				al: A2($elm$core$Basics$composeL, $elm$core$Maybe$Just, variable_)
			});
	});
var $terezka$line_charts$LineChart$Axis$default = $terezka$line_charts$Internal$Axis$default;
var $terezka$line_charts$Internal$Axis$Intersection$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Data$Point = F2(
	function (x, y) {
		return {aN: x, aO: y};
	});
var $terezka$line_charts$Internal$Axis$Intersection$custom = F2(
	function (toX, toY) {
		return function (_v0) {
			var x = _v0.aN;
			var y = _v0.aO;
			return A2(
				$terezka$line_charts$Internal$Data$Point,
				toX(x),
				toY(y));
		};
	});
var $terezka$line_charts$Internal$Axis$Intersection$default = A2(
	$terezka$line_charts$Internal$Axis$Intersection$custom,
	function ($) {
		return $.bW;
	},
	function ($) {
		return $.bW;
	});
var $terezka$line_charts$LineChart$Axis$Intersection$default = $terezka$line_charts$Internal$Axis$Intersection$default;
var $terezka$line_charts$Internal$Container$Margin = F4(
	function (top, right, bottom, left) {
		return {cI: bottom, dh: left, dO: right, d7: top};
	});
var $terezka$line_charts$Internal$Container$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Container$custom = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Container$Static = 0;
var $terezka$line_charts$Internal$Container$static = 0;
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $terezka$line_charts$Internal$Container$styled = F2(
	function (id, styles) {
		return $terezka$line_charts$Internal$Container$custom(
			{
				a$: A2(
					$elm$core$List$map,
					function (_v0) {
						var p = _v0.a;
						var v = _v0.b;
						return A2($elm$html$Html$Attributes$style, p, v);
					},
					styles),
				a0: _List_Nil,
				bc: id,
				bf: A4($terezka$line_charts$Internal$Container$Margin, 60, 140, 60, 80),
				gh: $terezka$line_charts$Internal$Container$static
			});
	});
var $terezka$line_charts$Internal$Container$default = function (id) {
	return A2($terezka$line_charts$Internal$Container$styled, id, _List_Nil);
};
var $terezka$line_charts$LineChart$Container$default = $terezka$line_charts$Internal$Container$default;
var $terezka$line_charts$Internal$Dots$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Dots$Disconnected = function (a) {
	return {$: 1, a: a};
};
var $terezka$line_charts$Internal$Dots$Style = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Dots$style = F2(
	function (radius, variety) {
		return {ca: radius, ct: variety};
	});
var $terezka$line_charts$Internal$Dots$disconnected = F2(
	function (radius, border) {
		return A2(
			$terezka$line_charts$Internal$Dots$style,
			radius,
			$terezka$line_charts$Internal$Dots$Disconnected(border));
	});
var $terezka$line_charts$Internal$Dots$default = {
	bO: function (_v0) {
		return A2($terezka$line_charts$Internal$Dots$disconnected, 10, 2);
	},
	bQ: function (_v1) {
		return A2($terezka$line_charts$Internal$Dots$disconnected, 10, 2);
	}
};
var $terezka$line_charts$LineChart$Dots$default = $terezka$line_charts$Internal$Dots$default;
var $terezka$line_charts$Internal$Events$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Events$custom = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Events$default = $terezka$line_charts$Internal$Events$custom(_List_Nil);
var $terezka$line_charts$LineChart$Events$default = $terezka$line_charts$Internal$Events$default;
var $terezka$line_charts$LineChart$Colors$grayLightest = A3($avh4$elm_color$Color$rgb255, 243, 243, 243);
var $terezka$line_charts$Internal$Grid$Lines = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $terezka$line_charts$Internal$Grid$lines = $terezka$line_charts$Internal$Grid$Lines;
var $terezka$line_charts$Internal$Grid$default = A2($terezka$line_charts$Internal$Grid$lines, 1, $terezka$line_charts$LineChart$Colors$grayLightest);
var $terezka$line_charts$LineChart$Grid$default = $terezka$line_charts$Internal$Grid$default;
var $terezka$line_charts$Internal$Interpolation$Linear = 0;
var $terezka$line_charts$LineChart$Interpolation$linear = 0;
var $terezka$line_charts$LineChart$Interpolation$default = $terezka$line_charts$LineChart$Interpolation$linear;
var $terezka$line_charts$Internal$Junk$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Junk$Layers = F3(
	function (below, above, html) {
		return {ei: above, eA: below, fl: html};
	});
var $terezka$line_charts$Internal$Junk$none = F4(
	function (_v0, _v1, _v2, _v3) {
		return A3($terezka$line_charts$Internal$Junk$Layers, _List_Nil, _List_Nil, _List_Nil);
	});
var $terezka$line_charts$LineChart$Junk$default = $terezka$line_charts$Internal$Junk$none;
var $terezka$line_charts$Internal$Legends$Grouped = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $terezka$line_charts$Internal$Svg$Transfrom = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $terezka$line_charts$Internal$Svg$offset = F2(
	function (x, y) {
		return A2($terezka$line_charts$Internal$Svg$Transfrom, x, y);
	});
var $terezka$line_charts$Internal$Svg$addPosition = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v0.b;
		var xf = _v1.a;
		var yf = _v1.b;
		return A2($terezka$line_charts$Internal$Svg$Transfrom, xf + x, yf + y);
	});
var $terezka$line_charts$Internal$Svg$toPosition = A2(
	$elm$core$List$foldr,
	$terezka$line_charts$Internal$Svg$addPosition,
	A2($terezka$line_charts$Internal$Svg$Transfrom, 0, 0));
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $terezka$line_charts$Internal$Svg$transform = function (translations) {
	var _v0 = $terezka$line_charts$Internal$Svg$toPosition(translations);
	var x = _v0.a;
	var y = _v0.b;
	return $elm$svg$Svg$Attributes$transform(
		'translate(' + ($elm$core$String$fromFloat(x) + (', ' + ($elm$core$String$fromFloat(y) + ')'))));
};
var $terezka$line_charts$Internal$Legends$defaultLegend = F2(
	function (index, _v0) {
		var sample = _v0.cd;
		var label = _v0.be;
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__legend'),
					$terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A2($terezka$line_charts$Internal$Svg$offset, 20, index * 20)
						]))
				]),
			_List_fromArray(
				[
					sample,
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$terezka$line_charts$Internal$Svg$transform(
							_List_fromArray(
								[
									A2($terezka$line_charts$Internal$Svg$offset, 40, 4)
								]))
						]),
					_List_fromArray(
						[
							A2($terezka$line_charts$Internal$Svg$label, 'inherit', label)
						]))
				]));
	});
var $terezka$line_charts$Internal$Coordinate$lengthX = function (system) {
	return A2($elm$core$Basics$max, 1, (system.ba.gh.aL - system.ba.bf.dh) - system.ba.bf.dO);
};
var $terezka$line_charts$Internal$Coordinate$reachX = function (system) {
	var diff = system.aN.dm - system.aN.bW;
	return (diff > 0) ? diff : 1;
};
var $terezka$line_charts$LineChart$Coordinate$scaleSvgX = F2(
	function (system, value) {
		return (value * $terezka$line_charts$Internal$Coordinate$lengthX(system)) / $terezka$line_charts$Internal$Coordinate$reachX(system);
	});
var $terezka$line_charts$LineChart$Coordinate$toSvgX = F2(
	function (system, value) {
		return A2($terezka$line_charts$LineChart$Coordinate$scaleSvgX, system, value - system.aN.bW) + system.ba.bf.dh;
	});
var $terezka$line_charts$Internal$Coordinate$lengthY = function (system) {
	return A2($elm$core$Basics$max, 1, (system.ba.gh.at - system.ba.bf.cI) - system.ba.bf.d7);
};
var $terezka$line_charts$Internal$Coordinate$reachY = function (system) {
	var diff = system.aO.dm - system.aO.bW;
	return (diff > 0) ? diff : 1;
};
var $terezka$line_charts$LineChart$Coordinate$scaleSvgY = F2(
	function (system, value) {
		return (value * $terezka$line_charts$Internal$Coordinate$lengthY(system)) / $terezka$line_charts$Internal$Coordinate$reachY(system);
	});
var $terezka$line_charts$LineChart$Coordinate$toSvgY = F2(
	function (system, value) {
		return A2($terezka$line_charts$LineChart$Coordinate$scaleSvgY, system, system.aO.dm - value) + system.ba.bf.d7;
	});
var $terezka$line_charts$Internal$Svg$move = F3(
	function (system, x, y) {
		return A2(
			$terezka$line_charts$Internal$Svg$Transfrom,
			A2($terezka$line_charts$LineChart$Coordinate$toSvgX, system, x),
			A2($terezka$line_charts$LineChart$Coordinate$toSvgY, system, y));
	});
var $terezka$line_charts$Internal$Legends$defaultLegends = F8(
	function (toX, toY, offsetX, offsetY, hovered, _arguments, system, legends) {
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__legends'),
					$terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3(
							$terezka$line_charts$Internal$Svg$move,
							system,
							toX(system.aN),
							toY(system.aO)),
							A2($terezka$line_charts$Internal$Svg$offset, offsetX, offsetY)
						]))
				]),
			A2($elm$core$List$indexedMap, $terezka$line_charts$Internal$Legends$defaultLegend, legends));
	});
var $terezka$line_charts$Internal$Legends$hover = function (data) {
	return A2(
		$terezka$line_charts$Internal$Legends$Grouped,
		30,
		A5(
			$terezka$line_charts$Internal$Legends$defaultLegends,
			function ($) {
				return $.dm;
			},
			function ($) {
				return $.dm;
			},
			0,
			10,
			data));
};
var $terezka$line_charts$Internal$Legends$default = $terezka$line_charts$Internal$Legends$hover(_List_Nil);
var $terezka$line_charts$LineChart$Legends$default = $terezka$line_charts$Internal$Legends$default;
var $terezka$line_charts$Internal$Line$Config = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Line$Style = $elm$core$Basics$identity;
var $terezka$line_charts$Internal$Line$style = F2(
	function (width, color_) {
		return {cO: color_, aL: width};
	});
var $terezka$line_charts$Internal$Line$default = function (_v0) {
	return A2($terezka$line_charts$Internal$Line$style, 1, $elm$core$Basics$identity);
};
var $terezka$line_charts$LineChart$Line$default = $terezka$line_charts$Internal$Line$default;
var $terezka$line_charts$LineChart$defaultConfig = F2(
	function (toX, toY) {
		return {
			cD: $terezka$line_charts$LineChart$Area$default,
			a4: $terezka$line_charts$LineChart$Container$default('line-chart-1'),
			a8: $terezka$line_charts$LineChart$Dots$default,
			f: $terezka$line_charts$LineChart$Events$default,
			fh: $terezka$line_charts$LineChart$Grid$default,
			dc: $terezka$line_charts$LineChart$Interpolation$default,
			ae: $terezka$line_charts$LineChart$Axis$Intersection$default,
			bP: $terezka$line_charts$LineChart$Junk$default,
			di: $terezka$line_charts$LineChart$Legends$default,
			aT: $terezka$line_charts$LineChart$Line$default,
			aN: A3($terezka$line_charts$LineChart$Axis$default, 700, '', toX),
			aO: A3($terezka$line_charts$LineChart$Axis$default, 400, '', toY)
		};
	});
var $terezka$line_charts$Internal$Junk$addBelow = F2(
	function (below, layers) {
		return _Utils_update(
			layers,
			{
				eA: _Utils_ap(below, layers.eA)
			});
	});
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $terezka$line_charts$LineChart$chartAreaAttributes = function (system) {
	return _List_fromArray(
		[
			$elm$svg$Svg$Attributes$x(
			$elm$core$String$fromFloat(system.ba.bf.dh)),
			$elm$svg$Svg$Attributes$y(
			$elm$core$String$fromFloat(system.ba.bf.d7)),
			$elm$svg$Svg$Attributes$width(
			$elm$core$String$fromFloat(
				$terezka$line_charts$Internal$Coordinate$lengthX(system))),
			$elm$svg$Svg$Attributes$height(
			$elm$core$String$fromFloat(
				$terezka$line_charts$Internal$Coordinate$lengthY(system)))
		]);
};
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $terezka$line_charts$Internal$Events$toChartAttributes = F3(
	function (data, system, _v0) {
		var events = _v0;
		var order = function (_v1) {
			var outside = _v1.a;
			var event = _v1.b;
			return outside ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				A2(event, data, system));
		};
		return A2($elm$core$List$filterMap, order, events);
	});
var $terezka$line_charts$LineChart$chartAreaPlatform = F3(
	function (config, data, system) {
		var attributes = $elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('transparent')
					]),
					$terezka$line_charts$LineChart$chartAreaAttributes(system),
					A3($terezka$line_charts$Internal$Events$toChartAttributes, data, system, config.f)
				]));
		return A2($elm$svg$Svg$rect, attributes, _List_Nil);
	});
var $elm$svg$Svg$clipPath = $elm$svg$Svg$trustedNode('clipPath');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $terezka$line_charts$Internal$Utils$toChartAreaId = function (id) {
	return 'chart__chart-area--' + id;
};
var $terezka$line_charts$LineChart$clipPath = function (system) {
	return A2(
		$elm$svg$Svg$clipPath,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$id(
				$terezka$line_charts$Internal$Utils$toChartAreaId(system.bc))
			]),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$rect,
				$terezka$line_charts$LineChart$chartAreaAttributes(system),
				_List_Nil)
			]));
};
var $terezka$line_charts$Internal$Line$color = F3(
	function (_v0, _v1, data_) {
		var config = _v0;
		var line_ = _v1;
		var _v2 = config(
			A2(
				$elm$core$List$map,
				function ($) {
					return $.gU;
				},
				data_));
		var style_ = _v2;
		return style_.cO(line_.cO);
	});
var $terezka$line_charts$Internal$Container$properties = F2(
	function (f, _v0) {
		var properties_ = _v0;
		return f(properties_);
	});
var $terezka$line_charts$Internal$Container$sizeStyles = F3(
	function (_v0, width, height) {
		var properties_ = _v0;
		var _v1 = properties_.gh;
		if (!_v1) {
			return _List_fromArray(
				[
					A2(
					$elm$html$Html$Attributes$style,
					'height',
					$elm$core$String$fromFloat(height) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'width',
					$elm$core$String$fromFloat(width) + 'px')
				]);
		} else {
			return _List_Nil;
		}
	});
var $terezka$line_charts$LineChart$container = F4(
	function (config, _v0, junkHtml, plot) {
		var frame = _v0.ba;
		var userAttributes = A2(
			$terezka$line_charts$Internal$Container$properties,
			function ($) {
				return $.a$;
			},
			config.a4);
		var sizeStyles = A3($terezka$line_charts$Internal$Container$sizeStyles, config.a4, frame.gh.aL, frame.gh.at);
		var styles = A2(
			$elm$core$List$cons,
			A2($elm$html$Html$Attributes$style, 'position', 'relative'),
			sizeStyles);
		return A2(
			$elm$html$Html$div,
			_Utils_ap(styles, userAttributes),
			A2($elm$core$List$cons, plot, junkHtml));
	});
var $terezka$line_charts$Internal$Line$data = function (_v0) {
	var config = _v0;
	return config._;
};
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $terezka$line_charts$Internal$Junk$getLayers = F5(
	function (series, toX, toY, system, _v0) {
		var toLayers = _v0;
		return A4(toLayers, series, toX, toY, system);
	});
var $terezka$line_charts$Internal$Line$label = function (_v0) {
	var config = _v0;
	return config.be;
};
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $terezka$line_charts$Internal$Events$toContainerAttributes = F3(
	function (data, system, _v0) {
		var events = _v0;
		var order = function (_v1) {
			var outside = _v1.a;
			var event = _v1.b;
			return outside ? $elm$core$Maybe$Just(
				A2(event, data, system)) : $elm$core$Maybe$Nothing;
		};
		return A2($elm$core$List$filterMap, order, events);
	});
var $terezka$line_charts$Internal$Data$Data = F3(
	function (user, point, isReal) {
		return {fz: isReal, f3: point, gU: user};
	});
var $terezka$line_charts$LineChart$setY = F2(
	function (datum, y) {
		return A3(
			$terezka$line_charts$Internal$Data$Data,
			datum.gU,
			A2($terezka$line_charts$Internal$Data$Point, datum.f3.aN, y),
			datum.fz);
	});
var $terezka$line_charts$LineChart$normalize = function (datasets) {
	if (datasets.b) {
		var highest = datasets.a;
		var belows = datasets.b;
		var toPercentage = F2(
			function (highest_, datum) {
				return A2($terezka$line_charts$LineChart$setY, datum, (100 * datum.f3.aO) / highest_.f3.aO);
			});
		return A2(
			$elm$core$List$map,
			A2($elm$core$List$map2, toPercentage, highest),
			A2($elm$core$List$cons, highest, belows));
	} else {
		return datasets;
	}
};
var $terezka$line_charts$Internal$Utils$withFirst = F2(
	function (stuff, process) {
		if (stuff.b) {
			var first = stuff.a;
			var rest = stuff.b;
			return $elm$core$Maybe$Just(
				A2(process, first, rest));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $terezka$line_charts$LineChart$addBelows = F2(
	function (alldata, dataBelowAll) {
		var add = F2(
			function (below, datum) {
				return A2($terezka$line_charts$LineChart$setY, below, below.f3.aO + datum.f3.aO);
			});
		var iterate = F4(
			function (datum0, dataTop, dataBelowTop, result) {
				iterate:
				while (true) {
					var _v0 = _Utils_Tuple2(dataTop, dataBelowTop);
					if (_v0.a.b) {
						if (_v0.b.b) {
							var _v1 = _v0.a;
							var datum1 = _v1.a;
							var data = _v1.b;
							var _v2 = _v0.b;
							var datumBelow = _v2.a;
							var dataBelow = _v2.b;
							if (_Utils_cmp(datum1.f3.aN, datumBelow.f3.aN) > 0) {
								if (datumBelow.fz) {
									var $temp$datum0 = datum0,
										$temp$dataTop = A2($elm$core$List$cons, datum1, data),
										$temp$dataBelowTop = dataBelow,
										$temp$result = A2(
										$elm$core$List$cons,
										A2(add, datumBelow, datum0),
										result);
									datum0 = $temp$datum0;
									dataTop = $temp$dataTop;
									dataBelowTop = $temp$dataBelowTop;
									result = $temp$result;
									continue iterate;
								} else {
									var breakdata = _Utils_update(
										datum0,
										{fz: false});
									var $temp$datum0 = datum0,
										$temp$dataTop = A2($elm$core$List$cons, datum1, data),
										$temp$dataBelowTop = dataBelow,
										$temp$result = A2(
										$elm$core$List$cons,
										A2(add, datumBelow, datum0),
										result);
									datum0 = $temp$datum0;
									dataTop = $temp$dataTop;
									dataBelowTop = $temp$dataBelowTop;
									result = $temp$result;
									continue iterate;
								}
							} else {
								var $temp$datum0 = datum1,
									$temp$dataTop = data,
									$temp$dataBelowTop = A2($elm$core$List$cons, datumBelow, dataBelow),
									$temp$result = result;
								datum0 = $temp$datum0;
								dataTop = $temp$dataTop;
								dataBelowTop = $temp$dataBelowTop;
								result = $temp$result;
								continue iterate;
							}
						} else {
							var _v4 = _v0.a;
							var datum1 = _v4.a;
							var data = _v4.b;
							return result;
						}
					} else {
						if (_v0.b.b) {
							var _v3 = _v0.b;
							var datumBelow = _v3.a;
							var dataBelow = _v3.b;
							if (_Utils_cmp(datum0.f3.aN, datumBelow.f3.aN) < 1) {
								var $temp$datum0 = datum0,
									$temp$dataTop = _List_Nil,
									$temp$dataBelowTop = dataBelow,
									$temp$result = A2(
									$elm$core$List$cons,
									A2(add, datumBelow, datum0),
									result);
								datum0 = $temp$datum0;
								dataTop = $temp$dataTop;
								dataBelowTop = $temp$dataBelowTop;
								result = $temp$result;
								continue iterate;
							} else {
								var $temp$datum0 = datum0,
									$temp$dataTop = _List_Nil,
									$temp$dataBelowTop = dataBelow,
									$temp$result = A2($elm$core$List$cons, datumBelow, result);
								datum0 = $temp$datum0;
								dataTop = $temp$dataTop;
								dataBelowTop = $temp$dataBelowTop;
								result = $temp$result;
								continue iterate;
							}
						} else {
							return result;
						}
					}
				}
			});
		return $elm$core$List$reverse(
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$terezka$line_charts$Internal$Utils$withFirst,
					alldata,
					F2(
						function (first, rest) {
							return A4(iterate, first, rest, dataBelowAll, _List_Nil);
						}))));
	});
var $terezka$line_charts$LineChart$stack = function (dataset) {
	var stackBelows = F2(
		function (dataset_, result) {
			if (dataset_.b) {
				var data = dataset_.a;
				var belows = dataset_.b;
				return A2(
					stackBelows,
					belows,
					A2(
						$elm$core$List$cons,
						A3($elm$core$List$foldl, $terezka$line_charts$LineChart$addBelows, data, belows),
						result));
			} else {
				return result;
			}
		});
	return $elm$core$List$reverse(
		A2(stackBelows, dataset, _List_Nil));
};
var $terezka$line_charts$Internal$Axis$variable = function (_v0) {
	var config = _v0;
	return config.al;
};
var $terezka$line_charts$LineChart$toDataPoints = F2(
	function (config, lines) {
		var y = $terezka$line_charts$Internal$Axis$variable(config.aO);
		var x = $terezka$line_charts$Internal$Axis$variable(config.aN);
		var addPoint = function (datum) {
			var _v1 = _Utils_Tuple2(
				x(datum),
				y(datum));
			if (!_v1.a.$) {
				if (!_v1.b.$) {
					var x_ = _v1.a.a;
					var y_ = _v1.b.a;
					return $elm$core$Maybe$Just(
						A3(
							$terezka$line_charts$Internal$Data$Data,
							datum,
							A2($terezka$line_charts$Internal$Data$Point, x_, y_),
							true));
				} else {
					var x_ = _v1.a.a;
					var _v2 = _v1.b;
					return $elm$core$Maybe$Just(
						A3(
							$terezka$line_charts$Internal$Data$Data,
							datum,
							A2($terezka$line_charts$Internal$Data$Point, x_, 0),
							false));
				}
			} else {
				if (!_v1.b.$) {
					var _v3 = _v1.a;
					var y_ = _v1.b.a;
					return $elm$core$Maybe$Nothing;
				} else {
					var _v4 = _v1.a;
					var _v5 = _v1.b;
					return $elm$core$Maybe$Nothing;
				}
			}
		};
		var data = A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				$terezka$line_charts$Internal$Line$data,
				$elm$core$List$filterMap(addPoint)),
			lines);
		var _v0 = config.cD;
		switch (_v0.$) {
			case 0:
				return data;
			case 1:
				return data;
			case 2:
				return $terezka$line_charts$LineChart$stack(data);
			default:
				return $terezka$line_charts$LineChart$normalize(
					$terezka$line_charts$LineChart$stack(data));
		}
	});
var $terezka$line_charts$Internal$Coordinate$Frame = F2(
	function (margin, size) {
		return {bf: margin, gh: size};
	});
var $terezka$line_charts$Internal$Coordinate$Size = F2(
	function (width, height) {
		return {at: height, aL: width};
	});
var $terezka$line_charts$LineChart$Coordinate$Range = F2(
	function (min, max) {
		return {dm: max, bW: min};
	});
var $terezka$line_charts$LineChart$Coordinate$scaleDataX = F2(
	function (system, value) {
		return (value * $terezka$line_charts$Internal$Coordinate$reachX(system)) / $terezka$line_charts$Internal$Coordinate$lengthX(system);
	});
var $terezka$line_charts$Internal$Axis$Range$applyX = F2(
	function (range, system) {
		switch (range.$) {
			case 0:
				var padMin = range.a;
				var padMax = range.b;
				var _v1 = system;
				var frame = _v1.ba;
				var _v2 = frame;
				var size = _v2.gh;
				var system_ = _Utils_update(
					system,
					{
						ba: _Utils_update(
							frame,
							{
								gh: _Utils_update(
									size,
									{
										aL: A2($elm$core$Basics$max, 1, (size.aL - padMin) - padMax)
									})
							})
					});
				var scale = $terezka$line_charts$LineChart$Coordinate$scaleDataX(system_);
				return A2(
					$terezka$line_charts$LineChart$Coordinate$Range,
					system.aN.bW - scale(padMin),
					system.aN.dm + scale(padMax));
			case 1:
				var min = range.a;
				var max = range.b;
				return A2($terezka$line_charts$LineChart$Coordinate$Range, min, max);
			default:
				var toRange = range.a;
				return toRange(system.aN);
		}
	});
var $terezka$line_charts$LineChart$Coordinate$scaleDataY = F2(
	function (system, value) {
		return (value * $terezka$line_charts$Internal$Coordinate$reachY(system)) / $terezka$line_charts$Internal$Coordinate$lengthY(system);
	});
var $terezka$line_charts$Internal$Axis$Range$applyY = F2(
	function (range, system) {
		switch (range.$) {
			case 0:
				var padMin = range.a;
				var padMax = range.b;
				var _v1 = system;
				var frame = _v1.ba;
				var _v2 = frame;
				var size = _v2.gh;
				var system_ = _Utils_update(
					system,
					{
						ba: _Utils_update(
							frame,
							{
								gh: _Utils_update(
									size,
									{
										at: A2($elm$core$Basics$max, 1, (size.at - padMin) - padMax)
									})
							})
					});
				var scale = $terezka$line_charts$LineChart$Coordinate$scaleDataY(system_);
				return A2(
					$terezka$line_charts$LineChart$Coordinate$Range,
					system.aO.bW - scale(padMin),
					system.aO.dm + scale(padMax));
			case 1:
				var min = range.a;
				var max = range.b;
				return A2($terezka$line_charts$LineChart$Coordinate$Range, min, max);
			default:
				var toRange = range.a;
				return toRange(system.aO);
		}
	});
var $terezka$line_charts$Internal$Coordinate$ground = function (range_) {
	return _Utils_update(
		range_,
		{
			bW: A2($elm$core$Basics$min, range_.bW, 0)
		});
};
var $terezka$line_charts$Internal$Area$hasArea = function (config) {
	switch (config.$) {
		case 0:
			return false;
		case 1:
			return true;
		case 2:
			return true;
		default:
			return true;
	}
};
var $terezka$line_charts$Internal$Axis$pixels = function (_v0) {
	var config = _v0;
	return config.ah;
};
var $terezka$line_charts$Internal$Axis$range = function (_v0) {
	var config = _v0;
	return config.ai;
};
var $terezka$line_charts$Internal$Coordinate$maximum = function (toValue) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$map(toValue),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$maximum,
			$elm$core$Maybe$withDefault(1)));
};
var $terezka$line_charts$Internal$Coordinate$minimum = function (toValue) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$map(toValue),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$minimum,
			$elm$core$Maybe$withDefault(0)));
};
var $terezka$line_charts$Internal$Coordinate$range = F2(
	function (toValue, data) {
		var range_ = {
			dm: A2($terezka$line_charts$Internal$Coordinate$maximum, toValue, data),
			bW: A2($terezka$line_charts$Internal$Coordinate$minimum, toValue, data)
		};
		return _Utils_eq(range_.bW, range_.dm) ? _Utils_update(
			range_,
			{dm: range_.dm + 1}) : range_;
	});
var $terezka$line_charts$LineChart$toSystem = F2(
	function (config, data) {
		var yRange = A2(
			$terezka$line_charts$Internal$Coordinate$range,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.f3;
				},
				function ($) {
					return $.aO;
				}),
			data);
		var xRange = A2(
			$terezka$line_charts$Internal$Coordinate$range,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.f3;
				},
				function ($) {
					return $.aN;
				}),
			data);
		var size = A2(
			$terezka$line_charts$Internal$Coordinate$Size,
			$terezka$line_charts$Internal$Axis$pixels(config.aN),
			$terezka$line_charts$Internal$Axis$pixels(config.aO));
		var hasArea = $terezka$line_charts$Internal$Area$hasArea(config.cD);
		var container_ = A2($terezka$line_charts$Internal$Container$properties, $elm$core$Basics$identity, config.a4);
		var frame = A2($terezka$line_charts$Internal$Coordinate$Frame, container_.bf, size);
		var adjustDomainRange = function (domain) {
			return hasArea ? $terezka$line_charts$Internal$Coordinate$ground(domain) : domain;
		};
		var system = {
			ba: frame,
			bc: container_.bc,
			aN: xRange,
			cy: xRange,
			aO: adjustDomainRange(yRange),
			cz: yRange
		};
		return _Utils_update(
			system,
			{
				aN: A2(
					$terezka$line_charts$Internal$Axis$Range$applyX,
					$terezka$line_charts$Internal$Axis$range(config.aN),
					system),
				aO: A2(
					$terezka$line_charts$Internal$Axis$Range$applyY,
					$terezka$line_charts$Internal$Axis$range(config.aO),
					system)
			});
	});
var $terezka$line_charts$Internal$Axis$ticks = function (_v0) {
	var config = _v0;
	return config.s;
};
var $terezka$line_charts$Internal$Axis$Tick$properties = function (_v0) {
	var properties_ = _v0;
	return properties_;
};
var $terezka$line_charts$Internal$Axis$Ticks$ticks = F3(
	function (dataRange, range, _v0) {
		var values = _v0;
		return A2(
			$elm$core$List$map,
			$terezka$line_charts$Internal$Axis$Tick$properties,
			A2(values, dataRange, range));
	});
var $terezka$line_charts$LineChart$Coordinate$Point = F2(
	function (x, y) {
		return {aN: x, aO: y};
	});
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
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
var $terezka$line_charts$Internal$Svg$gridDot = F3(
	function (radius, color, point) {
		return A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx(
					$elm$core$String$fromFloat(point.aN)),
					$elm$svg$Svg$Attributes$cy(
					$elm$core$String$fromFloat(point.aO)),
					$elm$svg$Svg$Attributes$r(
					$elm$core$String$fromFloat(radius)),
					$elm$svg$Svg$Attributes$fill(
					$avh4$elm_color$Color$toCssString(color))
				]),
			_List_Nil);
	});
var $terezka$line_charts$LineChart$Coordinate$toSvg = F2(
	function (system, point) {
		return {
			aN: A2($terezka$line_charts$LineChart$Coordinate$toSvgX, system, point.aN),
			aO: A2($terezka$line_charts$LineChart$Coordinate$toSvgY, system, point.aO)
		};
	});
var $terezka$line_charts$Internal$Grid$viewDots = F5(
	function (system, verticals, horizontals, radius, color) {
		var dot = F2(
			function (x, y) {
				return A2(
					$terezka$line_charts$LineChart$Coordinate$toSvg,
					system,
					A2($terezka$line_charts$LineChart$Coordinate$Point, x, y));
			});
		var dots_ = function (g) {
			return A2(
				$elm$core$List$map,
				dot(g),
				horizontals);
		};
		var alldots = A2($elm$core$List$concatMap, dots_, verticals);
		return A2(
			$elm$core$List$map,
			A2($terezka$line_charts$Internal$Svg$gridDot, radius, color),
			alldots);
	});
var $terezka$line_charts$Internal$Utils$concat = F3(
	function (first, second, third) {
		return _Utils_ap(
			first,
			_Utils_ap(second, third));
	});
var $terezka$line_charts$Internal$Path$Line = function (a) {
	return {$: 1, a: a};
};
var $terezka$line_charts$Internal$Path$Move = function (a) {
	return {$: 0, a: a};
};
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $terezka$line_charts$Internal$Path$join = function (commands) {
	return A2($elm$core$String$join, ' ', commands);
};
var $terezka$line_charts$Internal$Path$bool = function (bool_) {
	return bool_ ? '1' : '0';
};
var $terezka$line_charts$Internal$Path$point = function (point_) {
	return $elm$core$String$fromFloat(point_.aN) + (' ' + $elm$core$String$fromFloat(point_.aO));
};
var $terezka$line_charts$Internal$Path$points = function (points_) {
	return A2(
		$elm$core$String$join,
		',',
		A2($elm$core$List$map, $terezka$line_charts$Internal$Path$point, points_));
};
var $terezka$line_charts$Internal$Path$toString = function (command) {
	switch (command.$) {
		case 9:
			return 'Z';
		case 0:
			var p = command.a;
			return 'M' + $terezka$line_charts$Internal$Path$point(p);
		case 1:
			var p = command.a;
			return 'L' + $terezka$line_charts$Internal$Path$point(p);
		case 2:
			var x = command.a;
			return 'H' + $elm$core$String$fromFloat(x);
		case 3:
			var y = command.a;
			return 'V' + $elm$core$String$fromFloat(y);
		case 4:
			var c1 = command.a;
			var c2 = command.b;
			var p = command.c;
			return 'C' + $terezka$line_charts$Internal$Path$points(
				_List_fromArray(
					[c1, c2, p]));
		case 5:
			var c1 = command.a;
			var p = command.b;
			return 'Q' + $terezka$line_charts$Internal$Path$points(
				_List_fromArray(
					[c1, p]));
		case 6:
			var c1 = command.a;
			var p = command.b;
			return 'Q' + $terezka$line_charts$Internal$Path$points(
				_List_fromArray(
					[c1, p]));
		case 7:
			var p = command.a;
			return 'T' + $terezka$line_charts$Internal$Path$point(p);
		default:
			var rx = command.a;
			var ry = command.b;
			var xAxisRotation = command.c;
			var largeArcFlag = command.d;
			var sweepFlag = command.e;
			var p = command.f;
			return 'A' + $terezka$line_charts$Internal$Path$join(
				_List_fromArray(
					[
						$elm$core$String$fromFloat(rx),
						$elm$core$String$fromFloat(ry),
						$elm$core$String$fromInt(xAxisRotation),
						$terezka$line_charts$Internal$Path$bool(largeArcFlag),
						$terezka$line_charts$Internal$Path$bool(sweepFlag),
						$terezka$line_charts$Internal$Path$point(p)
					]));
	}
};
var $terezka$line_charts$Internal$Path$Arc = F6(
	function (a, b, c, d, e, f) {
		return {$: 8, a: a, b: b, c: c, d: d, e: e, f: f};
	});
var $terezka$line_charts$Internal$Path$Close = {$: 9};
var $terezka$line_charts$Internal$Path$CubicBeziers = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $terezka$line_charts$Internal$Path$CubicBeziersShort = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $terezka$line_charts$Internal$Path$Horizontal = function (a) {
	return {$: 2, a: a};
};
var $terezka$line_charts$Internal$Path$QuadraticBeziers = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $terezka$line_charts$Internal$Path$QuadraticBeziersShort = function (a) {
	return {$: 7, a: a};
};
var $terezka$line_charts$Internal$Path$Vertical = function (a) {
	return {$: 3, a: a};
};
var $terezka$line_charts$Internal$Path$translate = F2(
	function (system, command) {
		switch (command.$) {
			case 0:
				var p = command.a;
				return $terezka$line_charts$Internal$Path$Move(
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 1:
				var p = command.a;
				return $terezka$line_charts$Internal$Path$Line(
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 2:
				var x = command.a;
				return $terezka$line_charts$Internal$Path$Horizontal(
					A2($terezka$line_charts$LineChart$Coordinate$toSvgX, system, x));
			case 3:
				var y = command.a;
				return $terezka$line_charts$Internal$Path$Vertical(
					A2($terezka$line_charts$LineChart$Coordinate$toSvgY, system, y));
			case 4:
				var c1 = command.a;
				var c2 = command.b;
				var p = command.c;
				return A3(
					$terezka$line_charts$Internal$Path$CubicBeziers,
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, c1),
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, c2),
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 5:
				var c1 = command.a;
				var p = command.b;
				return A2(
					$terezka$line_charts$Internal$Path$CubicBeziersShort,
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, c1),
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 6:
				var c1 = command.a;
				var p = command.b;
				return A2(
					$terezka$line_charts$Internal$Path$QuadraticBeziers,
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, c1),
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 7:
				var p = command.a;
				return $terezka$line_charts$Internal$Path$QuadraticBeziersShort(
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 8:
				var rx = command.a;
				var ry = command.b;
				var xAxisRotation = command.c;
				var largeArcFlag = command.d;
				var sweepFlag = command.e;
				var p = command.f;
				return A6(
					$terezka$line_charts$Internal$Path$Arc,
					rx,
					ry,
					xAxisRotation,
					largeArcFlag,
					sweepFlag,
					A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			default:
				return $terezka$line_charts$Internal$Path$Close;
		}
	});
var $terezka$line_charts$Internal$Path$description = F2(
	function (system, commands) {
		return $terezka$line_charts$Internal$Path$join(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					$terezka$line_charts$Internal$Path$translate(system),
					$terezka$line_charts$Internal$Path$toString),
				commands));
	});
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $terezka$line_charts$Internal$Path$viewPath = function (attributes) {
	return A2($elm$svg$Svg$path, attributes, _List_Nil);
};
var $terezka$line_charts$Internal$Path$view = F3(
	function (system, attributes, commands) {
		return $terezka$line_charts$Internal$Path$viewPath(
			_Utils_ap(
				attributes,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d(
						A2($terezka$line_charts$Internal$Path$description, system, commands))
					])));
	});
var $terezka$line_charts$Internal$Svg$horizontal = F5(
	function (system, userAttributes, y, x1, x2) {
		var attributes = A3(
			$terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke(
					$avh4$elm_color$Color$toCssString($terezka$line_charts$LineChart$Colors$gray)),
					$elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A3(
			$terezka$line_charts$Internal$Path$view,
			system,
			attributes,
			_List_fromArray(
				[
					$terezka$line_charts$Internal$Path$Move(
					{aN: x1, aO: y}),
					$terezka$line_charts$Internal$Path$Line(
					{aN: x1, aO: y}),
					$terezka$line_charts$Internal$Path$Line(
					{aN: x2, aO: y})
				]));
	});
var $terezka$line_charts$Internal$Svg$horizontalGrid = F3(
	function (system, userAttributes, y) {
		var attributes = A3(
			$terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke(
					$avh4$elm_color$Color$toCssString($terezka$line_charts$LineChart$Colors$gray)),
					$elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A5($terezka$line_charts$Internal$Svg$horizontal, system, attributes, y, system.aN.bW, system.aN.dm);
	});
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $terezka$line_charts$Internal$Svg$vertical = F5(
	function (system, userAttributes, x, y1, y2) {
		var attributes = A3(
			$terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke(
					$avh4$elm_color$Color$toCssString($terezka$line_charts$LineChart$Colors$gray)),
					$elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A3(
			$terezka$line_charts$Internal$Path$view,
			system,
			attributes,
			_List_fromArray(
				[
					$terezka$line_charts$Internal$Path$Move(
					{aN: x, aO: y1}),
					$terezka$line_charts$Internal$Path$Line(
					{aN: x, aO: y1}),
					$terezka$line_charts$Internal$Path$Line(
					{aN: x, aO: y2})
				]));
	});
var $terezka$line_charts$Internal$Svg$verticalGrid = F3(
	function (system, userAttributes, x) {
		var attributes = A3(
			$terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke(
					$avh4$elm_color$Color$toCssString($terezka$line_charts$LineChart$Colors$gray)),
					$elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A5($terezka$line_charts$Internal$Svg$vertical, system, attributes, x, system.aO.bW, system.aO.dm);
	});
var $terezka$line_charts$Internal$Grid$viewLines = F5(
	function (system, verticals, horizontals, width, color) {
		var attributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromFloat(width)),
				$elm$svg$Svg$Attributes$stroke(
				$avh4$elm_color$Color$toCssString(color))
			]);
		return _Utils_ap(
			A2(
				$elm$core$List$map,
				A2($terezka$line_charts$Internal$Svg$horizontalGrid, system, attributes),
				horizontals),
			A2(
				$elm$core$List$map,
				A2($terezka$line_charts$Internal$Svg$verticalGrid, system, attributes),
				verticals));
	});
var $terezka$line_charts$Internal$Grid$view = F4(
	function (system, xAxis, yAxis, grid) {
		var hasGrid = function (tick) {
			return tick.fh ? $elm$core$Maybe$Just(tick.bn) : $elm$core$Maybe$Nothing;
		};
		var horizontals = A2(
			$elm$core$List$filterMap,
			hasGrid,
			A3(
				$terezka$line_charts$Internal$Axis$Ticks$ticks,
				system.cz,
				system.aO,
				$terezka$line_charts$Internal$Axis$ticks(yAxis)));
		var verticals = A2(
			$elm$core$List$filterMap,
			hasGrid,
			A3(
				$terezka$line_charts$Internal$Axis$Ticks$ticks,
				system.cy,
				system.aN,
				$terezka$line_charts$Internal$Axis$ticks(xAxis)));
		if (!grid.$) {
			var radius = grid.a;
			var color = grid.b;
			return A5($terezka$line_charts$Internal$Grid$viewDots, system, verticals, horizontals, radius, color);
		} else {
			var width = grid.a;
			var color = grid.b;
			return A5($terezka$line_charts$Internal$Grid$viewLines, system, verticals, horizontals, width, color);
		}
	});
var $terezka$line_charts$Internal$Svg$End = 2;
var $terezka$line_charts$Internal$Svg$Start = 0;
var $terezka$line_charts$Internal$Svg$anchorStyle = function (anchor) {
	var anchorString = function () {
		switch (anchor) {
			case 0:
				return 'start';
			case 1:
				return 'middle';
			default:
				return 'end';
		}
	}();
	return $elm$svg$Svg$Attributes$style('text-anchor: ' + (anchorString + ';'));
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
var $terezka$line_charts$Internal$Utils$viewMaybe = F2(
	function (a, view) {
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$svg$Svg$text(''),
			A2($elm$core$Maybe$map, view, a));
	});
var $terezka$line_charts$Internal$Legends$viewFree = F5(
	function (system, placement, viewLabel, line, data) {
		var _v0 = function () {
			if (!placement) {
				return _Utils_Tuple3(data, 2, -10);
			} else {
				return _Utils_Tuple3(
					$elm$core$List$reverse(data),
					0,
					10);
			}
		}();
		var orderedPoints = _v0.a;
		var anchor = _v0.b;
		var xOffset = _v0.c;
		var transform = function (_v3) {
			var x = _v3.aN;
			var y = _v3.aO;
			return $terezka$line_charts$Internal$Svg$transform(
				_List_fromArray(
					[
						A3($terezka$line_charts$Internal$Svg$move, system, x, y),
						A2($terezka$line_charts$Internal$Svg$offset, xOffset, 3)
					]));
		};
		var viewLegend = function (_v2) {
			var point = _v2.f3;
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						transform(point),
						$terezka$line_charts$Internal$Svg$anchorStyle(anchor)
					]),
				_List_fromArray(
					[
						viewLabel(
						$terezka$line_charts$Internal$Line$label(line))
					]));
		};
		return A2(
			$terezka$line_charts$Internal$Utils$viewMaybe,
			$elm$core$List$head(orderedPoints),
			viewLegend);
	});
var $terezka$line_charts$Internal$Legends$viewFrees = F3(
	function (_v0, placement, view_) {
		var system = _v0.d1;
		var lines = _v0.bS;
		var data = _v0._;
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__legends')
				]),
			A3(
				$elm$core$List$map2,
				A3($terezka$line_charts$Internal$Legends$viewFree, system, placement, view_),
				lines,
				data));
	});
var $terezka$line_charts$Internal$Line$shape = function (_v0) {
	var config = _v0;
	return config.dW;
};
var $terezka$line_charts$LineChart$Coordinate$toDataX = F2(
	function (system, value) {
		return system.aN.bW + A2($terezka$line_charts$LineChart$Coordinate$scaleDataX, system, value - system.ba.bf.dh);
	});
var $terezka$line_charts$LineChart$Coordinate$toDataY = F2(
	function (system, value) {
		return system.aO.dm - A2($terezka$line_charts$LineChart$Coordinate$scaleDataY, system, value - system.ba.bf.d7);
	});
var $terezka$line_charts$LineChart$Coordinate$toData = F2(
	function (system, point) {
		return {
			aN: A2($terezka$line_charts$LineChart$Coordinate$toDataX, system, point.aN),
			aO: A2($terezka$line_charts$LineChart$Coordinate$toDataY, system, point.aO)
		};
	});
var $elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var $terezka$line_charts$Internal$Dots$varietyAttributes = F2(
	function (color, variety) {
		switch (variety.$) {
			case 0:
				var width = variety.a;
				return _List_fromArray(
					[
						$elm$svg$Svg$Attributes$stroke(
						$avh4$elm_color$Color$toCssString(color)),
						$elm$svg$Svg$Attributes$strokeWidth(
						$elm$core$String$fromInt(width)),
						$elm$svg$Svg$Attributes$fill('white')
					]);
			case 2:
				var width = variety.a;
				var opacity = variety.b;
				return _List_fromArray(
					[
						$elm$svg$Svg$Attributes$stroke(
						$avh4$elm_color$Color$toCssString(color)),
						$elm$svg$Svg$Attributes$strokeWidth(
						$elm$core$String$fromInt(width)),
						$elm$svg$Svg$Attributes$strokeOpacity(
						$elm$core$String$fromFloat(opacity)),
						$elm$svg$Svg$Attributes$fill(
						$avh4$elm_color$Color$toCssString(color))
					]);
			case 1:
				var width = variety.a;
				return _List_fromArray(
					[
						$elm$svg$Svg$Attributes$stroke('white'),
						$elm$svg$Svg$Attributes$strokeWidth(
						$elm$core$String$fromInt(width)),
						$elm$svg$Svg$Attributes$fill(
						$avh4$elm_color$Color$toCssString(color))
					]);
			default:
				return _List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill(
						$avh4$elm_color$Color$toCssString(color))
					]);
		}
	});
var $terezka$line_charts$Internal$Dots$viewCircle = F5(
	function (events, variety, color, area, point) {
		var radius = $elm$core$Basics$sqrt(area / $elm$core$Basics$pi);
		var attributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$cx(
				$elm$core$String$fromFloat(point.aN)),
				$elm$svg$Svg$Attributes$cy(
				$elm$core$String$fromFloat(point.aO)),
				$elm$svg$Svg$Attributes$r(
				$elm$core$String$fromFloat(radius))
			]);
		return A2(
			$elm$svg$Svg$circle,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2($terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var $terezka$line_charts$Internal$Dots$pathPlus = F2(
	function (area, point) {
		var side = $elm$core$Basics$sqrt(area / 5);
		var r6 = side / 2;
		var r3 = side;
		var commands = _List_fromArray(
			[
				'M' + ($elm$core$String$fromFloat(point.aN - r6) + (' ' + $elm$core$String$fromFloat((point.aO - r3) - r6))),
				'v' + $elm$core$String$fromFloat(r3),
				'h' + $elm$core$String$fromFloat(-r3),
				'v' + $elm$core$String$fromFloat(r3),
				'h' + $elm$core$String$fromFloat(r3),
				'v' + $elm$core$String$fromFloat(r3),
				'h' + $elm$core$String$fromFloat(r3),
				'v' + $elm$core$String$fromFloat(-r3),
				'h' + $elm$core$String$fromFloat(r3),
				'v' + $elm$core$String$fromFloat(-r3),
				'h' + $elm$core$String$fromFloat(-r3),
				'v' + $elm$core$String$fromFloat(-r3),
				'h' + $elm$core$String$fromFloat(-r3),
				'v' + $elm$core$String$fromFloat(r3)
			]);
		return A2($elm$core$String$join, ' ', commands);
	});
var $terezka$line_charts$Internal$Dots$viewCross = F5(
	function (events, variety, color, area, point) {
		var rotation = 'rotate(45 ' + ($elm$core$String$fromFloat(point.aN) + (' ' + ($elm$core$String$fromFloat(point.aO) + ')')));
		var attributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$d(
				A2($terezka$line_charts$Internal$Dots$pathPlus, area, point)),
				$elm$svg$Svg$Attributes$transform(rotation)
			]);
		return A2(
			$elm$svg$Svg$path,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2($terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var $terezka$line_charts$Internal$Dots$viewDiamond = F5(
	function (events, variety, color, area, point) {
		var side = $elm$core$Basics$sqrt(area);
		var rotation = 'rotate(45 ' + ($elm$core$String$fromFloat(point.aN) + (' ' + ($elm$core$String$fromFloat(point.aO) + ')')));
		var attributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$x(
				$elm$core$String$fromFloat(point.aN - (side / 2))),
				$elm$svg$Svg$Attributes$y(
				$elm$core$String$fromFloat(point.aO - (side / 2))),
				$elm$svg$Svg$Attributes$width(
				$elm$core$String$fromFloat(side)),
				$elm$svg$Svg$Attributes$height(
				$elm$core$String$fromFloat(side)),
				$elm$svg$Svg$Attributes$transform(rotation)
			]);
		return A2(
			$elm$svg$Svg$rect,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2($terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var $terezka$line_charts$Internal$Dots$viewPlus = F5(
	function (events, variety, color, area, point) {
		var attributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$d(
				A2($terezka$line_charts$Internal$Dots$pathPlus, area, point))
			]);
		return A2(
			$elm$svg$Svg$path,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2($terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var $terezka$line_charts$Internal$Dots$viewSquare = F5(
	function (events, variety, color, area, point) {
		var side = $elm$core$Basics$sqrt(area);
		var attributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$x(
				$elm$core$String$fromFloat(point.aN - (side / 2))),
				$elm$svg$Svg$Attributes$y(
				$elm$core$String$fromFloat(point.aO - (side / 2))),
				$elm$svg$Svg$Attributes$width(
				$elm$core$String$fromFloat(side)),
				$elm$svg$Svg$Attributes$height(
				$elm$core$String$fromFloat(side))
			]);
		return A2(
			$elm$svg$Svg$rect,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2($terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var $elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * $elm$core$Basics$pi) / 180;
};
var $elm$core$Basics$tan = _Basics_tan;
var $terezka$line_charts$Internal$Dots$pathTriangle = F2(
	function (area, point) {
		var side = $elm$core$Basics$sqrt(
			(area * 4) / $elm$core$Basics$sqrt(3));
		var height = ($elm$core$Basics$sqrt(3) * side) / 2;
		var fromMiddle = height - (($elm$core$Basics$tan(
			$elm$core$Basics$degrees(30)) * side) / 2);
		var commands = _List_fromArray(
			[
				'M' + ($elm$core$String$fromFloat(point.aN) + (' ' + $elm$core$String$fromFloat(point.aO - fromMiddle))),
				'l' + ($elm$core$String$fromFloat((-side) / 2) + (' ' + $elm$core$String$fromFloat(height))),
				'h' + $elm$core$String$fromFloat(side),
				'z'
			]);
		return A2($elm$core$String$join, ' ', commands);
	});
var $terezka$line_charts$Internal$Dots$viewTriangle = F5(
	function (events, variety, color, area, point) {
		var attributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$d(
				A2($terezka$line_charts$Internal$Dots$pathTriangle, area, point))
			]);
		return A2(
			$elm$svg$Svg$path,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2($terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var $terezka$line_charts$Internal$Dots$viewShape = F5(
	function (system, _v0, shape, color, point) {
		var radius = _v0.ca;
		var variety = _v0.ct;
		var view_ = function () {
			switch (shape) {
				case 1:
					return $terezka$line_charts$Internal$Dots$viewCircle;
				case 2:
					return $terezka$line_charts$Internal$Dots$viewTriangle;
				case 3:
					return $terezka$line_charts$Internal$Dots$viewSquare;
				case 4:
					return $terezka$line_charts$Internal$Dots$viewDiamond;
				case 5:
					return $terezka$line_charts$Internal$Dots$viewCross;
				case 6:
					return $terezka$line_charts$Internal$Dots$viewPlus;
				default:
					return F5(
						function (_v2, _v3, _v4, _v5, _v6) {
							return $elm$svg$Svg$text('');
						});
			}
		}();
		var size = (2 * $elm$core$Basics$pi) * radius;
		var pointSvg = A2($terezka$line_charts$LineChart$Coordinate$toSvg, system, point);
		return A5(view_, _List_Nil, variety, color, size, pointSvg);
	});
var $terezka$line_charts$Internal$Dots$viewSample = F5(
	function (_v0, shape, color, system, data) {
		var config = _v0;
		var _v1 = config.bQ(
			A2(
				$elm$core$List$map,
				function ($) {
					return $.gU;
				},
				data));
		var style_ = _v1;
		return A4($terezka$line_charts$Internal$Dots$viewShape, system, style_, shape, color);
	});
var $elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $terezka$line_charts$Internal$Area$opacity = function (config) {
	switch (config.$) {
		case 0:
			return 0;
		case 1:
			var opacity_ = config.a;
			return opacity_;
		case 2:
			var opacity_ = config.a;
			return opacity_;
		default:
			var opacity_ = config.a;
			return opacity_;
	}
};
var $terezka$line_charts$Internal$Line$toAreaAttributes = F3(
	function (_v0, _v1, area) {
		var serie = _v0;
		var style_ = _v1;
		return _List_fromArray(
			[
				$elm$svg$Svg$Attributes$class('chart__interpolation__area__fragment'),
				$elm$svg$Svg$Attributes$fill(
				$avh4$elm_color$Color$toCssString(
					style_.cO(serie.cO)))
			]);
	});
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $terezka$line_charts$Internal$Line$toSeriesAttributes = F2(
	function (_v0, _v1) {
		var serie = _v0;
		var style_ = _v1;
		return _List_fromArray(
			[
				$elm$svg$Svg$Attributes$style('pointer-events: none;'),
				$elm$svg$Svg$Attributes$class('chart__interpolation__line__fragment'),
				$elm$svg$Svg$Attributes$stroke(
				$avh4$elm_color$Color$toCssString(
					style_.cO(serie.cO))),
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromFloat(style_.aL)),
				$elm$svg$Svg$Attributes$strokeDasharray(
				A2(
					$elm$core$String$join,
					' ',
					A2($elm$core$List$map, $elm$core$String$fromFloat, serie.cQ))),
				$elm$svg$Svg$Attributes$fill('transparent')
			]);
	});
var $terezka$line_charts$Internal$Utils$viewIf = F2(
	function (condition, view) {
		return condition ? view(0) : $elm$svg$Svg$text('');
	});
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $terezka$line_charts$Internal$Line$viewSample = F5(
	function (_v0, line_, area, data_, sampleWidth) {
		var look = _v0;
		var style_ = look(
			A2(
				$elm$core$List$map,
				function ($) {
					return $.gU;
				},
				data_));
		var sizeAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$x1('0'),
				$elm$svg$Svg$Attributes$y1('0'),
				$elm$svg$Svg$Attributes$x2(
				$elm$core$String$fromFloat(sampleWidth)),
				$elm$svg$Svg$Attributes$y2('0')
			]);
		var rectangleAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$x('0'),
				$elm$svg$Svg$Attributes$y('0'),
				$elm$svg$Svg$Attributes$height('9'),
				$elm$svg$Svg$Attributes$width(
				$elm$core$String$fromFloat(sampleWidth))
			]);
		var lineAttributes = A2($terezka$line_charts$Internal$Line$toSeriesAttributes, line_, style_);
		var areaAttributes = A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$fillOpacity(
				$elm$core$String$fromFloat(
					$terezka$line_charts$Internal$Area$opacity(area))),
			A3($terezka$line_charts$Internal$Line$toAreaAttributes, line_, style_, area));
		var viewRectangle = function (_v1) {
			return A2(
				$elm$svg$Svg$rect,
				_Utils_ap(areaAttributes, rectangleAttributes),
				_List_Nil);
		};
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$line,
					_Utils_ap(lineAttributes, sizeAttributes),
					_List_Nil),
					A2(
					$terezka$line_charts$Internal$Utils$viewIf,
					$terezka$line_charts$Internal$Area$hasArea(area),
					viewRectangle)
				]));
	});
var $terezka$line_charts$Internal$Legends$viewSample = F4(
	function (_v0, sampleWidth, line, data) {
		var system = _v0.d1;
		var lineConfig = _v0.dk;
		var dotsConfig = _v0.cU;
		var area = _v0.cD;
		var shape = $terezka$line_charts$Internal$Line$shape(line);
		var dotPosition = A2(
			$terezka$line_charts$LineChart$Coordinate$toData,
			system,
			A2($terezka$line_charts$Internal$Data$Point, sampleWidth / 2, 0));
		var color = A3($terezka$line_charts$Internal$Line$color, lineConfig, line, data);
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__sample')
				]),
			_List_fromArray(
				[
					A5($terezka$line_charts$Internal$Line$viewSample, lineConfig, line, area, data, sampleWidth),
					A6($terezka$line_charts$Internal$Dots$viewSample, dotsConfig, shape, color, system, data, dotPosition)
				]));
	});
var $terezka$line_charts$Internal$Legends$viewGrouped = F3(
	function (_arguments, sampleWidth, container) {
		var toLegend = F2(
			function (line, data) {
				return {
					be: $terezka$line_charts$Internal$Line$label(line),
					cd: A4($terezka$line_charts$Internal$Legends$viewSample, _arguments, sampleWidth, line, data)
				};
			});
		var legends = A3($elm$core$List$map2, toLegend, _arguments.bS, _arguments._);
		return A2(container, _arguments.d1, legends);
	});
var $terezka$line_charts$Internal$Legends$view = function (_arguments) {
	var _v0 = _arguments.di;
	switch (_v0.$) {
		case 1:
			var placement = _v0.a;
			var view_ = _v0.b;
			return A3($terezka$line_charts$Internal$Legends$viewFrees, _arguments, placement, view_);
		case 2:
			var sampleWidth = _v0.a;
			var container = _v0.b;
			return A3(
				$terezka$line_charts$Internal$Legends$viewGrouped,
				_arguments,
				sampleWidth,
				container(_arguments));
		default:
			return $elm$svg$Svg$text('');
	}
};
var $terezka$line_charts$Internal$Area$opacityContainer = function (config) {
	switch (config.$) {
		case 0:
			return 1;
		case 1:
			var opacity_ = config.a;
			return 1;
		case 2:
			var opacity_ = config.a;
			return opacity_;
		default:
			var opacity_ = config.a;
			return opacity_;
	}
};
var $terezka$line_charts$Internal$Utils$unzip3 = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var a = _v0.a;
			var b = _v0.b;
			var c = _v0.c;
			var aas = _v1.a;
			var bs = _v1.b;
			var cs = _v1.c;
			return _Utils_Tuple3(
				A2($elm$core$List$cons, a, aas),
				A2($elm$core$List$cons, b, bs),
				A2($elm$core$List$cons, c, cs));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple3(_List_Nil, _List_Nil, _List_Nil),
		pairs);
};
var $terezka$line_charts$Internal$Line$viewNormal = function (_v0) {
	var areas = _v0.a;
	var lines = _v0.b;
	var dots = _v0.c;
	var view_ = F3(
		function (area_, line_, dots_) {
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('chart__line')
					]),
				_List_fromArray(
					[area_, line_, dots_]));
		});
	return A4($elm$core$List$map3, view_, areas, lines, dots);
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $terezka$line_charts$Internal$Data$isWithinRange = F2(
	function (system, point) {
		return _Utils_eq(
			A3($elm$core$Basics$clamp, system.aN.bW, system.aN.dm, point.aN),
			point.aN) && _Utils_eq(
			A3($elm$core$Basics$clamp, system.aO.bW, system.aO.dm, point.aO),
			point.aO);
	});
var $terezka$line_charts$Internal$Utils$part = F4(
	function (isReal, points, current, parts) {
		part:
		while (true) {
			if (points.b) {
				var first = points.a;
				var rest = points.b;
				if (isReal(first)) {
					var $temp$isReal = isReal,
						$temp$points = rest,
						$temp$current = _Utils_ap(
						current,
						_List_fromArray(
							[first])),
						$temp$parts = parts;
					isReal = $temp$isReal;
					points = $temp$points;
					current = $temp$current;
					parts = $temp$parts;
					continue part;
				} else {
					var $temp$isReal = isReal,
						$temp$points = rest,
						$temp$current = _List_Nil,
						$temp$parts = A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							current,
							$elm$core$Maybe$Just(first)),
						parts);
					isReal = $temp$isReal;
					points = $temp$points;
					current = $temp$current;
					parts = $temp$parts;
					continue part;
				}
			} else {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(current, $elm$core$Maybe$Nothing),
					parts);
			}
		}
	});
var $terezka$line_charts$Internal$Interpolation$linear = $elm$core$List$map(
	$elm$core$List$map($terezka$line_charts$Internal$Path$Line));
var $terezka$line_charts$Internal$Interpolation$First = {$: 0};
var $terezka$line_charts$Internal$Interpolation$Previous = function (a) {
	return {$: 1, a: a};
};
var $terezka$line_charts$Internal$Interpolation$monotoneCurve = F4(
	function (point0, point1, tangent0, tangent1) {
		var dx = (point1.aN - point0.aN) / 3;
		return A3(
			$terezka$line_charts$Internal$Path$CubicBeziers,
			{aN: point0.aN + dx, aO: point0.aO + (dx * tangent0)},
			{aN: point1.aN - dx, aO: point1.aO - (dx * tangent1)},
			point1);
	});
var $terezka$line_charts$Internal$Interpolation$slope2 = F3(
	function (point0, point1, t) {
		var h = point1.aN - point0.aN;
		return (!(!h)) ? ((((3 * (point1.aO - point0.aO)) / h) - t) / 2) : t;
	});
var $terezka$line_charts$Internal$Interpolation$sign = function (x) {
	return (x < 0) ? (-1) : 1;
};
var $terezka$line_charts$Internal$Interpolation$toH = F2(
	function (h0, h1) {
		return (!h0) ? ((h1 < 0) ? (0 * (-1)) : h1) : h0;
	});
var $terezka$line_charts$Internal$Interpolation$slope3 = F3(
	function (point0, point1, point2) {
		var h1 = point2.aN - point1.aN;
		var h0 = point1.aN - point0.aN;
		var s0h = A2($terezka$line_charts$Internal$Interpolation$toH, h0, h1);
		var s0 = (point1.aO - point0.aO) / s0h;
		var s1h = A2($terezka$line_charts$Internal$Interpolation$toH, h1, h0);
		var s1 = (point2.aO - point1.aO) / s1h;
		var p = ((s0 * h1) + (s1 * h0)) / (h0 + h1);
		var slope = ($terezka$line_charts$Internal$Interpolation$sign(s0) + $terezka$line_charts$Internal$Interpolation$sign(s1)) * A2(
			$elm$core$Basics$min,
			A2(
				$elm$core$Basics$min,
				$elm$core$Basics$abs(s0),
				$elm$core$Basics$abs(s1)),
			0.5 * $elm$core$Basics$abs(p));
		return $elm$core$Basics$isNaN(slope) ? 0 : slope;
	});
var $terezka$line_charts$Internal$Interpolation$monotonePart = F2(
	function (points, _v0) {
		var tangent = _v0.a;
		var commands = _v0.b;
		var _v1 = _Utils_Tuple2(tangent, points);
		_v1$4:
		while (true) {
			if (!_v1.a.$) {
				if (_v1.b.b && _v1.b.b.b) {
					if (_v1.b.b.b.b) {
						var _v2 = _v1.a;
						var _v3 = _v1.b;
						var p0 = _v3.a;
						var _v4 = _v3.b;
						var p1 = _v4.a;
						var _v5 = _v4.b;
						var p2 = _v5.a;
						var rest = _v5.b;
						var t1 = A3($terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p2);
						var t0 = A3($terezka$line_charts$Internal$Interpolation$slope2, p0, p1, t1);
						return A2(
							$terezka$line_charts$Internal$Interpolation$monotonePart,
							A2(
								$elm$core$List$cons,
								p1,
								A2($elm$core$List$cons, p2, rest)),
							_Utils_Tuple2(
								$terezka$line_charts$Internal$Interpolation$Previous(t1),
								_Utils_ap(
									commands,
									_List_fromArray(
										[
											A4($terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1)
										]))));
					} else {
						var _v9 = _v1.a;
						var _v10 = _v1.b;
						var p0 = _v10.a;
						var _v11 = _v10.b;
						var p1 = _v11.a;
						var t1 = A3($terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p1);
						return _Utils_Tuple2(
							$terezka$line_charts$Internal$Interpolation$Previous(t1),
							_Utils_ap(
								commands,
								_List_fromArray(
									[
										A4($terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t1, t1),
										$terezka$line_charts$Internal$Path$Line(p1)
									])));
					}
				} else {
					break _v1$4;
				}
			} else {
				if (_v1.b.b && _v1.b.b.b) {
					if (_v1.b.b.b.b) {
						var t0 = _v1.a.a;
						var _v6 = _v1.b;
						var p0 = _v6.a;
						var _v7 = _v6.b;
						var p1 = _v7.a;
						var _v8 = _v7.b;
						var p2 = _v8.a;
						var rest = _v8.b;
						var t1 = A3($terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p2);
						return A2(
							$terezka$line_charts$Internal$Interpolation$monotonePart,
							A2(
								$elm$core$List$cons,
								p1,
								A2($elm$core$List$cons, p2, rest)),
							_Utils_Tuple2(
								$terezka$line_charts$Internal$Interpolation$Previous(t1),
								_Utils_ap(
									commands,
									_List_fromArray(
										[
											A4($terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1)
										]))));
					} else {
						var t0 = _v1.a.a;
						var _v12 = _v1.b;
						var p0 = _v12.a;
						var _v13 = _v12.b;
						var p1 = _v13.a;
						var t1 = A3($terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p1);
						return _Utils_Tuple2(
							$terezka$line_charts$Internal$Interpolation$Previous(t1),
							_Utils_ap(
								commands,
								_List_fromArray(
									[
										A4($terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1),
										$terezka$line_charts$Internal$Path$Line(p1)
									])));
					}
				} else {
					break _v1$4;
				}
			}
		}
		return _Utils_Tuple2(tangent, commands);
	});
var $terezka$line_charts$Internal$Interpolation$monotoneSection = F2(
	function (points, _v0) {
		var tangent = _v0.a;
		var acc = _v0.b;
		var _v1 = function () {
			if (points.b) {
				var p0 = points.a;
				var rest = points.b;
				return A2(
					$terezka$line_charts$Internal$Interpolation$monotonePart,
					A2($elm$core$List$cons, p0, rest),
					_Utils_Tuple2(
						tangent,
						_List_fromArray(
							[
								$terezka$line_charts$Internal$Path$Line(p0)
							])));
			} else {
				return _Utils_Tuple2(tangent, _List_Nil);
			}
		}();
		var t0 = _v1.a;
		var commands = _v1.b;
		return _Utils_Tuple2(
			t0,
			A2($elm$core$List$cons, commands, acc));
	});
var $terezka$line_charts$Internal$Interpolation$monotone = function (sections) {
	return A3(
		$elm$core$List$foldr,
		$terezka$line_charts$Internal$Interpolation$monotoneSection,
		_Utils_Tuple2($terezka$line_charts$Internal$Interpolation$First, _List_Nil),
		sections).b;
};
var $terezka$line_charts$Internal$Interpolation$after = F2(
	function (a, b) {
		return _List_fromArray(
			[
				a,
				A2($terezka$line_charts$Internal$Data$Point, b.aN, a.aO),
				b
			]);
	});
var $terezka$line_charts$Internal$Interpolation$stepped = function (sections) {
	var expand = F2(
		function (result, section) {
			expand:
			while (true) {
				if (section.a.b) {
					if (section.a.b.b) {
						var _v1 = section.a;
						var a = _v1.a;
						var _v2 = _v1.b;
						var b = _v2.a;
						var rest = _v2.b;
						var broken = section.b;
						var $temp$result = _Utils_ap(
							result,
							A2($terezka$line_charts$Internal$Interpolation$after, a, b)),
							$temp$section = _Utils_Tuple2(
							A2($elm$core$List$cons, b, rest),
							broken);
						result = $temp$result;
						section = $temp$section;
						continue expand;
					} else {
						if (!section.b.$) {
							var _v3 = section.a;
							var last = _v3.a;
							var broken = section.b.a;
							return _Utils_ap(
								result,
								_List_fromArray(
									[
										A2($terezka$line_charts$Internal$Data$Point, broken.aN, last.aO)
									]));
						} else {
							var _v4 = section.a;
							var last = _v4.a;
							var _v5 = section.b;
							return result;
						}
					}
				} else {
					return result;
				}
			}
		});
	return A2(
		$elm$core$List$map,
		A2(
			$elm$core$Basics$composeR,
			expand(_List_Nil),
			$elm$core$List$map($terezka$line_charts$Internal$Path$Line)),
		sections);
};
var $terezka$line_charts$Internal$Interpolation$toCommands = F2(
	function (interpolation, data) {
		var pointsSections = $elm$core$List$map(
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Tuple$mapFirst(
					$elm$core$List$map(
						function ($) {
							return $.f3;
						})),
				$elm$core$Tuple$mapSecond(
					$elm$core$Maybe$map(
						function ($) {
							return $.f3;
						}))));
		var points = $elm$core$List$map(
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Tuple$first,
				$elm$core$List$map(
					function ($) {
						return $.f3;
					})));
		switch (interpolation) {
			case 0:
				return $terezka$line_charts$Internal$Interpolation$linear(
					points(data));
			case 1:
				return $terezka$line_charts$Internal$Interpolation$monotone(
					points(data));
			default:
				return $terezka$line_charts$Internal$Interpolation$stepped(
					pointsSections(data));
		}
	});
var $terezka$line_charts$Internal$Area$opacitySingle = function (config) {
	switch (config.$) {
		case 0:
			return 0;
		case 1:
			var opacity_ = config.a;
			return opacity_;
		case 2:
			var opacity_ = config.a;
			return 1;
		default:
			var opacity_ = config.a;
			return 1;
	}
};
var $terezka$line_charts$Internal$Path$toPoint = function (command) {
	switch (command.$) {
		case 9:
			return A2($terezka$line_charts$LineChart$Coordinate$Point, 0, 0);
		case 0:
			var p = command.a;
			return p;
		case 1:
			var p = command.a;
			return p;
		case 2:
			var x = command.a;
			return A2($terezka$line_charts$LineChart$Coordinate$Point, x, 0);
		case 3:
			var y = command.a;
			return A2($terezka$line_charts$LineChart$Coordinate$Point, 0, y);
		case 4:
			var c1 = command.a;
			var c2 = command.b;
			var p = command.c;
			return p;
		case 5:
			var c1 = command.a;
			var p = command.b;
			return p;
		case 6:
			var c1 = command.a;
			var p = command.b;
			return p;
		case 7:
			var p = command.a;
			return p;
		default:
			var rx = command.a;
			var ry = command.b;
			var xAxisRotation = command.c;
			var largeArcFlag = command.d;
			var sweepFlag = command.e;
			var p = command.f;
			return p;
	}
};
var $terezka$line_charts$Internal$Utils$towardsZero = function (_v0) {
	var max = _v0.dm;
	var min = _v0.bW;
	return A3($elm$core$Basics$clamp, min, max, 0);
};
var $terezka$line_charts$Internal$Utils$last = function (list) {
	return $elm$core$List$head(
		A2(
			$elm$core$List$drop,
			$elm$core$List$length(list) - 1,
			list));
};
var $terezka$line_charts$Internal$Utils$lastSafe = F2(
	function (first, rest) {
		return A2(
			$elm$core$Maybe$withDefault,
			first,
			$terezka$line_charts$Internal$Utils$last(rest));
	});
var $terezka$line_charts$Internal$Utils$viewWithEdges = F2(
	function (stuff, view) {
		if (stuff.b) {
			var first = stuff.a;
			var rest = stuff.b;
			return A3(
				view,
				first,
				rest,
				A2($terezka$line_charts$Internal$Utils$lastSafe, first, rest));
		} else {
			return $elm$svg$Svg$text('');
		}
	});
var $elm$svg$Svg$Attributes$clipPath = _VirtualDom_attribute('clip-path');
var $terezka$line_charts$Internal$Svg$withinChartArea = function (_v0) {
	var id = _v0.bc;
	return $elm$svg$Svg$Attributes$clipPath(
		'url(#' + ($terezka$line_charts$Internal$Utils$toChartAreaId(id) + ')'));
};
var $terezka$line_charts$LineChart$Junk$withinChartArea = $terezka$line_charts$Internal$Svg$withinChartArea;
var $terezka$line_charts$Internal$Line$viewArea = F5(
	function (_v0, line_, style_, interpolation, data_) {
		var system = _v0.d1;
		var lineConfig = _v0.dk;
		var area = _v0.cD;
		var ground = function (point) {
			return A2(
				$terezka$line_charts$Internal$Data$Point,
				point.aN,
				$terezka$line_charts$Internal$Utils$towardsZero(system.aO));
		};
		var commands = F3(
			function (first, middle, last) {
				return A3(
					$terezka$line_charts$Internal$Utils$concat,
					_List_fromArray(
						[
							$terezka$line_charts$Internal$Path$Move(
							ground(
								$terezka$line_charts$Internal$Path$toPoint(first))),
							$terezka$line_charts$Internal$Path$Line(
							$terezka$line_charts$Internal$Path$toPoint(first))
						]),
					interpolation,
					_List_fromArray(
						[
							$terezka$line_charts$Internal$Path$Line(
							ground(
								$terezka$line_charts$Internal$Path$toPoint(last)))
						]));
			});
		var attributes = A2(
			$elm$core$List$cons,
			$terezka$line_charts$LineChart$Junk$withinChartArea(system),
			A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$fillOpacity(
					$elm$core$String$fromFloat(
						$terezka$line_charts$Internal$Area$opacitySingle(area))),
				A3($terezka$line_charts$Internal$Line$toAreaAttributes, line_, style_, area)));
		return A2(
			$terezka$line_charts$Internal$Utils$viewWithEdges,
			interpolation,
			F3(
				function (first, middle, last) {
					return A3(
						$terezka$line_charts$Internal$Path$view,
						system,
						attributes,
						A3(commands, first, middle, last));
				}));
	});
var $terezka$line_charts$Internal$Dots$view = F2(
	function (_v0, data) {
		var system = _v0.d1;
		var dotsConfig = _v0.cU;
		var shape = _v0.dW;
		var color = _v0.cO;
		var _v1 = dotsConfig;
		var config = _v1;
		var _v2 = config.bO(data.gU);
		var style_ = _v2;
		return A5($terezka$line_charts$Internal$Dots$viewShape, system, style_, shape, color, data.f3);
	});
var $terezka$line_charts$Internal$Line$viewDot = F3(
	function (_arguments, _v0, _v1) {
		var lineConfig = _v0;
		var style_ = _v1;
		return $terezka$line_charts$Internal$Dots$view(
			{
				cO: style_.cO(lineConfig.cO),
				cU: _arguments.cU,
				dW: lineConfig.dW,
				d1: _arguments.d1
			});
	});
var $terezka$line_charts$Internal$Utils$viewWithFirst = F2(
	function (stuff, view) {
		if (stuff.b) {
			var first = stuff.a;
			var rest = stuff.b;
			return A2(view, first, rest);
		} else {
			return $elm$svg$Svg$text('');
		}
	});
var $terezka$line_charts$Internal$Line$viewSeries = F5(
	function (_v0, line_, style_, interpolation, data_) {
		var system = _v0.d1;
		var lineConfig = _v0.dk;
		var attributes = A2(
			$elm$core$List$cons,
			$terezka$line_charts$LineChart$Junk$withinChartArea(system),
			A2($terezka$line_charts$Internal$Line$toSeriesAttributes, line_, style_));
		return A2(
			$terezka$line_charts$Internal$Utils$viewWithFirst,
			data_,
			F2(
				function (first, _v1) {
					return A3(
						$terezka$line_charts$Internal$Path$view,
						system,
						attributes,
						A2(
							$elm$core$List$cons,
							$terezka$line_charts$Internal$Path$Move(first.f3),
							interpolation));
				}));
	});
var $terezka$line_charts$Internal$Line$viewSingle = F3(
	function (_arguments, line_, data_) {
		var style_ = function (_v1) {
			var look = _v1;
			return look(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.gU;
					},
					data_));
		}(_arguments.dk);
		var sections = A4(
			$terezka$line_charts$Internal$Utils$part,
			function ($) {
				return $.fz;
			},
			data_,
			_List_Nil,
			_List_Nil);
		var parts = A2($elm$core$List$map, $elm$core$Tuple$first, sections);
		var viewDots = A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__dots')
				]),
			A2(
				$elm$core$List$map,
				A3($terezka$line_charts$Internal$Line$viewDot, _arguments, line_, style_),
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeL,
						$terezka$line_charts$Internal$Data$isWithinRange(_arguments.d1),
						function ($) {
							return $.f3;
						}),
					$elm$core$List$concat(parts))));
		var commands = A2($terezka$line_charts$Internal$Interpolation$toCommands, _arguments.dc, sections);
		var viewAreas = function (_v0) {
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('chart__interpolation__area')
					]),
				A3(
					$elm$core$List$map2,
					A3($terezka$line_charts$Internal$Line$viewArea, _arguments, line_, style_),
					commands,
					parts));
		};
		var viewSeriess = A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__interpolation__line')
				]),
			A3(
				$elm$core$List$map2,
				A3($terezka$line_charts$Internal$Line$viewSeries, _arguments, line_, style_),
				commands,
				parts));
		return _Utils_Tuple3(
			A2(
				$terezka$line_charts$Internal$Utils$viewIf,
				$terezka$line_charts$Internal$Area$hasArea(_arguments.cD),
				viewAreas),
			viewSeriess,
			viewDots);
	});
var $terezka$line_charts$Internal$Line$viewStacked = F2(
	function (area, _v0) {
		var areas = _v0.a;
		var lines = _v0.b;
		var dots = _v0.c;
		var toList = F2(
			function (l, d) {
				return _List_fromArray(
					[l, d]);
			});
		var opacity = 'opacity: ' + $elm$core$String$fromFloat(
			$terezka$line_charts$Internal$Area$opacityContainer(area));
		var bottoms = $elm$core$List$concat(
			A3($elm$core$List$map2, toList, lines, dots));
		return _List_fromArray(
			[
				A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('chart__bottoms'),
						$elm$svg$Svg$Attributes$style(opacity)
					]),
				areas),
				A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('chart__tops')
					]),
				bottoms)
			]);
	});
var $terezka$line_charts$Internal$Line$view = F3(
	function (_arguments, lines, datas) {
		var container = $elm$svg$Svg$g(
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__lines')
				]));
		var buildSeriesViews = ($terezka$line_charts$Internal$Area$opacityContainer(_arguments.cD) < 1) ? $terezka$line_charts$Internal$Line$viewStacked(_arguments.cD) : $terezka$line_charts$Internal$Line$viewNormal;
		return container(
			buildSeriesViews(
				$terezka$line_charts$Internal$Utils$unzip3(
					A3(
						$elm$core$List$map2,
						$terezka$line_charts$Internal$Line$viewSingle(_arguments),
						lines,
						datas))));
	});
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $terezka$line_charts$LineChart$viewBoxAttribute = function (_v0) {
	var frame = _v0.ba;
	return $elm$svg$Svg$Attributes$viewBox(
		'0 0 ' + ($elm$core$String$fromFloat(frame.gh.aL) + (' ' + $elm$core$String$fromFloat(frame.gh.at))));
};
var $terezka$line_charts$Internal$Axis$Line$config = function (_v0) {
	var config_ = _v0;
	return config_;
};
var $terezka$line_charts$Internal$Axis$Title$config = function (_v0) {
	var title = _v0;
	return title;
};
var $terezka$line_charts$Internal$Axis$Intersection$getY = function (_v0) {
	var func = _v0;
	return A2(
		$elm$core$Basics$composeL,
		function ($) {
			return $.aO;
		},
		func);
};
var $terezka$line_charts$Internal$Axis$attributesLine = F2(
	function (system, _v0) {
		var events = _v0.f;
		var width = _v0.aL;
		var color = _v0.cO;
		return _Utils_ap(
			events,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat(width)),
					$elm$svg$Svg$Attributes$stroke(
					$avh4$elm_color$Color$toCssString(color)),
					$terezka$line_charts$Internal$Svg$withinChartArea(system)
				]));
	});
var $terezka$line_charts$Internal$Axis$viewHorizontalAxisLine = F3(
	function (system, axisPosition, config) {
		return A5(
			$terezka$line_charts$Internal$Svg$horizontal,
			system,
			A2($terezka$line_charts$Internal$Axis$attributesLine, system, config),
			axisPosition,
			config.cn,
			config.cW);
	});
var $terezka$line_charts$Internal$Axis$attributesTick = function (_v0) {
	var width = _v0.aL;
	var color = _v0.cO;
	return _List_fromArray(
		[
			$elm$svg$Svg$Attributes$strokeWidth(
			$elm$core$String$fromFloat(width)),
			$elm$svg$Svg$Attributes$stroke(
			$avh4$elm_color$Color$toCssString(color))
		]);
};
var $terezka$line_charts$Internal$Axis$Tick$isPositive = function (direction) {
	if (direction === 1) {
		return true;
	} else {
		return false;
	}
};
var $terezka$line_charts$Internal$Axis$lengthOfTick = function (_v0) {
	var length = _v0.bR;
	var direction = _v0.bA;
	return $terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? (-length) : length;
};
var $terezka$line_charts$Internal$Svg$Middle = 1;
var $terezka$line_charts$Internal$Axis$viewHorizontalLabel = F4(
	function (system, _v0, position, view) {
		var direction = _v0.bA;
		var length = _v0.bR;
		var yOffset = $terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? ((-5) - length) : (15 + length);
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3($terezka$line_charts$Internal$Svg$move, system, position.aN, position.aO),
							A2($terezka$line_charts$Internal$Svg$offset, 0, yOffset)
						])),
					$terezka$line_charts$Internal$Svg$anchorStyle(1)
				]),
			_List_fromArray(
				[view]));
	});
var $terezka$line_charts$Internal$Svg$xTick = F5(
	function (system, height, userAttributes, y, x) {
		var attributes = A3(
			$terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke(
					$avh4$elm_color$Color$toCssString($terezka$line_charts$LineChart$Colors$gray))
				]),
			userAttributes,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgX, system, x))),
					$elm$svg$Svg$Attributes$x2(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgX, system, x))),
					$elm$svg$Svg$Attributes$y1(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgY, system, y))),
					$elm$svg$Svg$Attributes$y2(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgY, system, y) + height))
				]));
		return A2($elm$svg$Svg$line, attributes, _List_Nil);
	});
var $terezka$line_charts$Internal$Axis$viewHorizontalTick = F3(
	function (system, point, tick) {
		var x = point.aN;
		var y = point.aO;
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__tick')
				]),
			_List_fromArray(
				[
					A5(
					$terezka$line_charts$Internal$Svg$xTick,
					system,
					$terezka$line_charts$Internal$Axis$lengthOfTick(tick),
					$terezka$line_charts$Internal$Axis$attributesTick(tick),
					y,
					x),
					A2(
					$terezka$line_charts$Internal$Utils$viewMaybe,
					tick.be,
					A3($terezka$line_charts$Internal$Axis$viewHorizontalLabel, system, tick, point))
				]));
	});
var $terezka$line_charts$Internal$Axis$viewHorizontalTitle = F3(
	function (system, at, _v0) {
		var title = _v0.gJ;
		var position = at(
			A2(title.bn, system.cy, system.aN));
		var _v1 = title.b;
		var xOffset = _v1.a;
		var yOffset = _v1.b;
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__title'),
					$terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3($terezka$line_charts$Internal$Svg$move, system, position.aN, position.aO),
							A2($terezka$line_charts$Internal$Svg$offset, xOffset + 15, yOffset + 5)
						])),
					$terezka$line_charts$Internal$Svg$anchorStyle(0)
				]),
			_List_fromArray(
				[title.gV]));
	});
var $terezka$line_charts$Internal$Axis$viewHorizontal = F3(
	function (system, intersection, _v0) {
		var config = _v0;
		var viewConfig = {
			ae: A2($terezka$line_charts$Internal$Axis$Intersection$getY, intersection, system),
			aT: A3($terezka$line_charts$Internal$Axis$Line$config, config.P, system.cy, system.aN),
			s: A3($terezka$line_charts$Internal$Axis$Ticks$ticks, system.cy, system.aN, config.s),
			gJ: $terezka$line_charts$Internal$Axis$Title$config(config.gJ)
		};
		var viewAxisLine = A2($terezka$line_charts$Internal$Axis$viewHorizontalAxisLine, system, viewConfig.ae);
		var at = function (x) {
			return {aN: x, aO: viewConfig.ae};
		};
		var viewTick = function (tick) {
			return A3(
				$terezka$line_charts$Internal$Axis$viewHorizontalTick,
				system,
				at(tick.bn),
				tick);
		};
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__axis--horizontal')
				]),
			_List_fromArray(
				[
					A3($terezka$line_charts$Internal$Axis$viewHorizontalTitle, system, at, viewConfig),
					viewAxisLine(viewConfig.aT),
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('chart__ticks')
						]),
					A2($elm$core$List$map, viewTick, viewConfig.s))
				]));
	});
var $terezka$line_charts$Internal$Axis$Intersection$getX = function (_v0) {
	var func = _v0;
	return A2(
		$elm$core$Basics$composeL,
		function ($) {
			return $.aN;
		},
		func);
};
var $terezka$line_charts$Internal$Axis$viewVerticalAxisLine = F3(
	function (system, axisPosition, config) {
		return A5(
			$terezka$line_charts$Internal$Svg$vertical,
			system,
			A2($terezka$line_charts$Internal$Axis$attributesLine, system, config),
			axisPosition,
			config.cn,
			config.cW);
	});
var $terezka$line_charts$Internal$Axis$viewVerticalLabel = F4(
	function (system, _v0, position, view) {
		var direction = _v0.bA;
		var length = _v0.bR;
		var xOffset = $terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? (5 + length) : ((-5) - length);
		var anchor = $terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? 0 : 2;
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3($terezka$line_charts$Internal$Svg$move, system, position.aN, position.aO),
							A2($terezka$line_charts$Internal$Svg$offset, xOffset, 5)
						])),
					$terezka$line_charts$Internal$Svg$anchorStyle(anchor)
				]),
			_List_fromArray(
				[view]));
	});
var $terezka$line_charts$Internal$Svg$yTick = F5(
	function (system, width, userAttributes, x, y) {
		var attributes = A3(
			$terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__tick'),
					$elm$svg$Svg$Attributes$stroke(
					$avh4$elm_color$Color$toCssString($terezka$line_charts$LineChart$Colors$gray))
				]),
			userAttributes,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgX, system, x))),
					$elm$svg$Svg$Attributes$x2(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgX, system, x) - width)),
					$elm$svg$Svg$Attributes$y1(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgY, system, y))),
					$elm$svg$Svg$Attributes$y2(
					$elm$core$String$fromFloat(
						A2($terezka$line_charts$LineChart$Coordinate$toSvgY, system, y)))
				]));
		return A2($elm$svg$Svg$line, attributes, _List_Nil);
	});
var $terezka$line_charts$Internal$Axis$viewVerticalTick = F3(
	function (system, point, tick) {
		var x = point.aN;
		var y = point.aO;
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__tick')
				]),
			_List_fromArray(
				[
					A5(
					$terezka$line_charts$Internal$Svg$yTick,
					system,
					$terezka$line_charts$Internal$Axis$lengthOfTick(tick),
					$terezka$line_charts$Internal$Axis$attributesTick(tick),
					x,
					y),
					A2(
					$terezka$line_charts$Internal$Utils$viewMaybe,
					tick.be,
					A3($terezka$line_charts$Internal$Axis$viewVerticalLabel, system, tick, point))
				]));
	});
var $terezka$line_charts$Internal$Axis$viewVerticalTitle = F3(
	function (system, at, _v0) {
		var title = _v0.gJ;
		var position = at(
			A2(title.bn, system.cz, system.aO));
		var _v1 = title.b;
		var xOffset = _v1.a;
		var yOffset = _v1.b;
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__title'),
					$terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3($terezka$line_charts$Internal$Svg$move, system, position.aN, position.aO),
							A2($terezka$line_charts$Internal$Svg$offset, xOffset + 2, yOffset - 10)
						])),
					$terezka$line_charts$Internal$Svg$anchorStyle(2)
				]),
			_List_fromArray(
				[title.gV]));
	});
var $terezka$line_charts$Internal$Axis$viewVertical = F3(
	function (system, intersection, _v0) {
		var config = _v0;
		var viewConfig = {
			ae: A2($terezka$line_charts$Internal$Axis$Intersection$getX, intersection, system),
			aT: A3($terezka$line_charts$Internal$Axis$Line$config, config.P, system.cz, system.aO),
			s: A3($terezka$line_charts$Internal$Axis$Ticks$ticks, system.cz, system.aO, config.s),
			gJ: $terezka$line_charts$Internal$Axis$Title$config(config.gJ)
		};
		var viewAxisLine = A2($terezka$line_charts$Internal$Axis$viewVerticalAxisLine, system, viewConfig.ae);
		var at = function (y) {
			return {aN: viewConfig.ae, aO: y};
		};
		var viewTick = function (tick) {
			return A3(
				$terezka$line_charts$Internal$Axis$viewVerticalTick,
				system,
				at(tick.bn),
				tick);
		};
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('chart__axis--vertical')
				]),
			_List_fromArray(
				[
					A3($terezka$line_charts$Internal$Axis$viewVerticalTitle, system, at, viewConfig),
					viewAxisLine(viewConfig.aT),
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('chart__ticks')
						]),
					A2($elm$core$List$map, viewTick, viewConfig.s))
				]));
	});
var $terezka$line_charts$LineChart$viewCustom = F2(
	function (config, lines) {
		var junkLineInfo = function (line_) {
			return _Utils_Tuple3(
				A3($terezka$line_charts$Internal$Line$color, config.aT, line_, _List_Nil),
				$terezka$line_charts$Internal$Line$label(line_),
				$terezka$line_charts$Internal$Line$data(line_));
		};
		var getJunk = A3(
			$terezka$line_charts$Internal$Junk$getLayers,
			A2($elm$core$List$map, junkLineInfo, lines),
			$terezka$line_charts$Internal$Axis$variable(config.aN),
			$terezka$line_charts$Internal$Axis$variable(config.aO));
		var data = A2($terezka$line_charts$LineChart$toDataPoints, config, lines);
		var dataAll = $elm$core$List$concat(data);
		var dataSafe = A2(
			$elm$core$List$map,
			$elm$core$List$filter(
				function ($) {
					return $.fz;
				}),
			data);
		var dataAllSafe = $elm$core$List$concat(dataSafe);
		var system = A2($terezka$line_charts$LineChart$toSystem, config, dataAllSafe);
		var viewLines = $terezka$line_charts$Internal$Line$view(
			{cD: config.cD, cU: config.a8, dc: config.dc, dk: config.aT, d1: system});
		var viewLegends = $terezka$line_charts$Internal$Legends$view(
			{
				cD: config.cD,
				_: dataSafe,
				cU: config.a8,
				di: config.di,
				dk: config.aT,
				bS: lines,
				d1: system,
				aN: $terezka$line_charts$Internal$Axis$variable(config.aN),
				aO: $terezka$line_charts$Internal$Axis$variable(config.aO)
			});
		var attributes = $elm$core$List$concat(
			_List_fromArray(
				[
					A2(
					$terezka$line_charts$Internal$Container$properties,
					function ($) {
						return $.a0;
					},
					config.a4),
					A3($terezka$line_charts$Internal$Events$toContainerAttributes, dataAll, system, config.f),
					_List_fromArray(
					[
						$terezka$line_charts$LineChart$viewBoxAttribute(system)
					])
				]));
		var addGrid = $terezka$line_charts$Internal$Junk$addBelow(
			A4($terezka$line_charts$Internal$Grid$view, system, config.aN, config.aO, config.fh));
		var junk = addGrid(
			A2(getJunk, system, config.bP));
		return A4(
			$terezka$line_charts$LineChart$container,
			config,
			system,
			junk.fl,
			A2(
				$elm$svg$Svg$svg,
				attributes,
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$defs,
						_List_Nil,
						_List_fromArray(
							[
								$terezka$line_charts$LineChart$clipPath(system)
							])),
						A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$class('chart__junk--below')
							]),
						junk.eA),
						A2(viewLines, lines, data),
						A3($terezka$line_charts$LineChart$chartAreaPlatform, config, dataAll, system),
						A3($terezka$line_charts$Internal$Axis$viewHorizontal, system, config.ae, config.aN),
						A3($terezka$line_charts$Internal$Axis$viewVertical, system, config.ae, config.aO),
						viewLegends,
						A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$class('chart__junk--above')
							]),
						junk.ei)
					])));
	});
var $terezka$line_charts$LineChart$view = F2(
	function (toX, toY) {
		return $terezka$line_charts$LineChart$viewCustom(
			A2($terezka$line_charts$LineChart$defaultConfig, toX, toY));
	});
var $author$project$PressurePlot$newMeathodChart = function (events) {
	var _v0 = $author$project$PressurePlot$velocity2d(events);
	var vel = _v0.a;
	var trimmedEvents = _v0.b;
	var uncorrPressures = A2($elm$core$List$map, $author$project$PressurePlot$pressureVector3d, trimmedEvents);
	var corrPressures = A3(
		$elm$core$List$map2,
		$author$project$PressurePlot$frictionCorrectedPressure(0.3),
		vel,
		uncorrPressures);
	var netforce = A2($elm$core$List$map, $author$project$Vector3$length, corrPressures);
	return $mdgriffith$elm_ui$Element$html(
		A3(
			$terezka$line_charts$LineChart$view,
			function ($) {
				return $.aN;
			},
			function ($) {
				return $.aO;
			},
			_List_fromArray(
				[
					A4(
					$terezka$line_charts$LineChart$line,
					$terezka$line_charts$LineChart$Colors$blue,
					$terezka$line_charts$LineChart$Dots$none,
					'net force',
					A2($author$project$PressurePlot$floatIndexedMap, $author$project$PressurePlot$Point, netforce)),
					A4(
					$terezka$line_charts$LineChart$line,
					$terezka$line_charts$LineChart$Colors$green,
					$terezka$line_charts$LineChart$Dots$none,
					'vel mag',
					A2(
						$author$project$PressurePlot$floatIndexedMap,
						F2(
							function (i, v) {
								return A2(
									$author$project$PressurePlot$Point,
									i,
									$author$project$Vector2$length(v) / 10);
							}),
						vel)),
					A4(
					$terezka$line_charts$LineChart$line,
					$terezka$line_charts$LineChart$Colors$red,
					$terezka$line_charts$LineChart$Dots$none,
					'raw p',
					A2(
						$author$project$PressurePlot$floatIndexedMap,
						F2(
							function (i, ev) {
								return A2($author$project$PressurePlot$Point, i, ev.aC);
							}),
						trimmedEvents))
				])));
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $author$project$Pointer$on = F2(
	function (event, tag) {
		return A2(
			$elm$html$Html$Events$on,
			event,
			A2($elm$json$Json$Decode$map, tag, $author$project$Pointer$eventDecoder));
	});
var $author$project$Pointer$onDown = $author$project$Pointer$on('pointerdown');
var $mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 7, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Flag$padding = $mdgriffith$elm_ui$Internal$Flag$flag(2);
var $mdgriffith$elm_ui$Element$padding = function (x) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + $elm$core$String$fromInt(x),
			x,
			x,
			x,
			x));
};
var $author$project$Vector3$direction = function (a) {
	return A2(
		$author$project$Vector3$scale,
		1 / $author$project$Vector3$length(a),
		a);
};
var $author$project$PressurePlot$dotVelPressure = F2(
	function (vel, press) {
		var vel3 = A3($author$project$Vector3$Vector3, vel.aN, vel.aO, 0);
		return A2(
			$author$project$Vector3$dot,
			$author$project$Vector3$direction(vel3),
			$author$project$Vector3$direction(press));
	});
var $author$project$PenTilt$Tilt = F3(
	function (r, tiltX, tiltY) {
		return {bo: r, d5: tiltX, d6: tiltY};
	});
var $author$project$Vector3$cross = F2(
	function (a, b) {
		return {aN: (a.aO * b.ao) - (a.ao * b.aO), aO: (a.ao * b.aN) - (a.aN * b.ao), ao: (a.aN * b.aO) - (a.aO * b.aN)};
	});
var $author$project$PenTilt$toCartesian = function (_v0) {
	var r = _v0.bo;
	var tiltX = _v0.d5;
	var tiltY = _v0.d6;
	var py = {
		aN: 0,
		aO: (-1) * $elm$core$Basics$cos(tiltY),
		ao: (-1) * $elm$core$Basics$sin(tiltY)
	};
	var px = {
		aN: (-1) * $elm$core$Basics$cos(tiltX),
		aO: 0,
		ao: (-1) * $elm$core$Basics$sin(tiltX)
	};
	var _v1 = A2(
		$author$project$Vector3$scale,
		r,
		$author$project$Vector3$direction(
			A2($author$project$Vector3$cross, py, px)));
	var x = _v1.aN;
	var y = _v1.aO;
	var z = _v1.ao;
	return A3($author$project$PenTilt$Cartesian, x, y, z);
};
var $author$project$PressurePlot$pressureVectorFromTilt = F3(
	function (pressure, tiltX, tiltY) {
		return $author$project$PenTilt$toCartesian(
			A3(
				$author$project$PenTilt$Tilt,
				pressure,
				$elm$core$Basics$degrees(tiltX),
				$elm$core$Basics$degrees(tiltY)));
	});
var $author$project$PressurePlot$velocity = function (eventList) {
	var timeStamps = A2(
		$elm$core$List$map,
		function ($) {
			return $.aH;
		},
		eventList);
	var t2 = A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$List$head(
			$elm$core$List$reverse(timeStamps)));
	var t1 = A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$List$head(timeStamps));
	var p1 = A2(
		$elm$core$List$map,
		function (e) {
			return {aN: e.aD, aO: e.aE};
		},
		eventList);
	var p2 = A2($elm$core$List$drop, 1, p1);
	var dt = (t2 - t1) / $elm$core$List$length(eventList);
	var displacment = A3($elm$core$List$map2, $author$project$Vector2$rel, p1, p2);
	return A2(
		$elm$core$List$map,
		function (disp) {
			return A2($author$project$Vector2$scale, 1 / dt, disp);
		},
		displacment);
};
var $author$project$PressurePlot$pressureOrentationChart = function (model) {
	var pressureVector = A2(
		$elm$core$List$drop,
		1,
		A2(
			$elm$core$List$map,
			function (ev) {
				return A3($author$project$PressurePlot$pressureVectorFromTilt, ev.aC, ev.d5, ev.d6);
			},
			model.f));
	var pressure = A2(
		$elm$core$List$drop,
		1,
		A2(
			$elm$core$List$map,
			function ($) {
				return $.aC;
			},
			model.f));
	var dotProduct = A3(
		$elm$core$List$map2,
		$author$project$PressurePlot$dotVelPressure,
		$author$project$PressurePlot$velocity(model.f),
		pressureVector);
	var angle = A3(
		$elm$core$List$map2,
		F2(
			function (pv3, v) {
				return function (a) {
					return (a * 180) / $elm$core$Basics$pi;
				}(
					A2(
						$author$project$Vector2$angleBetween,
						A2($author$project$Vector2$Vector2, pv3.aN, pv3.aO),
						v));
			}),
		pressureVector,
		$author$project$PressurePlot$velocity(model.f));
	return $mdgriffith$elm_ui$Element$html(
		A3(
			$terezka$line_charts$LineChart$view,
			function ($) {
				return $.aN;
			},
			function ($) {
				return $.aO;
			},
			_List_fromArray(
				[
					A4(
					$terezka$line_charts$LineChart$line,
					$terezka$line_charts$LineChart$Colors$blue,
					$terezka$line_charts$LineChart$Dots$none,
					'corr',
					A3($elm$core$List$map2, $author$project$PressurePlot$Point, angle, pressure))
				])));
};
var $mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Flag$spacing = $mdgriffith$elm_ui$Internal$Flag$flag(3);
var $mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y)));
	});
var $mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			$mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2($mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var $mdgriffith$elm_ui$Internal$Model$VariantActive = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$Font$tabularNumbers = $mdgriffith$elm_ui$Internal$Model$VariantActive('tnum');
var $mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$text = function (content) {
	return $mdgriffith$elm_ui$Internal$Model$Text(content);
};
var $mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$fontVariant = $mdgriffith$elm_ui$Internal$Flag$flag(48);
var $mdgriffith$elm_ui$Element$Font$variant = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontVariant, 'v-' + name);
		case 1:
			var name = _var.a;
			return A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontVariant, 'v-' + (name + '-off'));
		default:
			var name = _var.a;
			var index = _var.b;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$fontVariant,
				A3(
					$mdgriffith$elm_ui$Internal$Model$Single,
					'v-' + (name + ('-' + $elm$core$String$fromInt(index))),
					'font-feature-settings',
					'\"' + (name + ('\" ' + $elm$core$String$fromInt(index)))));
	}
};
var $author$project$PressurePlot$velocityChart = function (model) {
	return $mdgriffith$elm_ui$Element$html(
		A3(
			$terezka$line_charts$LineChart$view,
			function ($) {
				return $.aN;
			},
			function ($) {
				return $.aO;
			},
			_List_fromArray(
				[
					A4(
					$terezka$line_charts$LineChart$line,
					$terezka$line_charts$LineChart$Colors$blue,
					$terezka$line_charts$LineChart$Dots$none,
					'vel',
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (x, y) {
								return A2($author$project$PressurePlot$Point, x, y);
							}),
						A2(
							$elm$core$List$map,
							$author$project$Vector2$length,
							$author$project$PressurePlot$velocity(model.f))))
				])));
};
var $author$project$PressurePlot$view = function (model) {
	return A2(
		$mdgriffith$elm_ui$Element$layout,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				A2($elm$html$Html$Attributes$style, 'touch-action', 'none')),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				A2($elm$html$Html$Attributes$style, 'user-select', 'none')),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				$author$project$Pointer$onDown($author$project$PressurePlot$Down)),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				$author$project$PressurePlot$blockContextMenu($author$project$PressurePlot$NoOp)),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				$elm$html$Html$Attributes$id('pen'))
			]),
		A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$padding(20),
					$mdgriffith$elm_ui$Element$spacing(20),
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Font$variant($mdgriffith$elm_ui$Element$Font$tabularNumbers),
					$mdgriffith$elm_ui$Element$alignRight
				]),
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$text('Pen Info Charts'),
					A2(
					$mdgriffith$elm_ui$Element$column,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
						]),
					_List_fromArray(
						[
							$author$project$PressurePlot$pressureOrentationChart(model),
							$author$project$PressurePlot$newMeathodChart(model.f),
							$author$project$PressurePlot$velocityChart(model)
						]))
				])));
};
var $author$project$PressurePlot$main = $elm$browser$Browser$element(
	{
		fq: $elm$core$Basics$always($author$project$PressurePlot$init),
		gq: $author$project$PressurePlot$subscriptions,
		gT: $author$project$PressurePlot$update,
		gV: $author$project$PressurePlot$view
	});
_Platform_export({'PressurePlot':{'init':$author$project$PressurePlot$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));
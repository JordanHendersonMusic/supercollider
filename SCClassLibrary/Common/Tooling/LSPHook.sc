// Hooks let the class library extend sc_lsp (the language server) and sc_mcp (the MCP server). A hook is a subclass
// of LSPHook that overrides one of its class methods. The servers find hooks from the class hierarchy, without
// running anything, and then ask this sclang to run them, over OSC on the language port. Only messages from this
// machine are answered.
//
// Hook methods take plain data and answer plain data: numbers, strings, symbols, arrays and events.
//
// *completeSymbol { |context| } answers the symbols that fit where one is being typed, as an Array of Symbols or
// Strings, or of Events with \label and \detail. The context is an Event:
//   receiver   the class the enclosing call is sent to, e.g. \Pbind, or nil
//   selector   the call's selector, e.g. \new for Pbind(...), or nil
//   argument   the argument's index in the call, counting from 0, or -1 outside a call
//   previous   the text of the argument before, e.g. "\instrument", or nil
//   keyword    the keyword when written key: \..., e.g. \instrument, or nil
//   prefix     what has been typed of the symbol, a String
//
// Evaluating code for sc_mcp's evaluate tool is refused unless allowEval is true, and sc_mcp was started with
// --allow-eval. Every evaluation is posted first.

LSPHook {
	classvar <>allowEval = false, <>enabled = true, <>maxResults = 500;
	classvar responders;

	*completeSymbol { |context| ^nil }

	*initClass {
		StartUp.add { LSPHook.startResponders }
	}

	*startResponders {
		this.stopResponders;
		responders = [
			OSCFunc({ |msg, time, addr| this.prHello(msg, addr) }, '/lsp/hello'),
			OSCFunc({ |msg, time, addr| this.prCall(msg, addr) }, '/lsp/call'),
			OSCFunc({ |msg, time, addr| this.prEval(msg, addr) }, '/lsp/eval')
		];
		responders.do { |r| r.permanent = true };
	}

	*stopResponders {
		responders.do { |r| r.free };
		responders = nil;
	}

	*hooks { ^LSPHook.allSubclasses }

	*prAnswers { |addr| ^enabled and: { addr.ip == "127.0.0.1" } }

	*prReply { |addr, id, status, text|
		addr.sendMsg('/lsp/reply', id, status, text)
	}

	*prHello { |msg, addr|
		var hooks;
		if(this.prAnswers(addr).not) { ^this };
		hooks = ();
		this.hooks.do { |hook|
			var path = hook.filenameSymbol.asString;
			hooks[hook.name] = (size: File.fileSize(path) ? 0);
		};
		this.prReply(addr, msg[1], "ok", this.toJSON((version: 1, allowEval: allowEval, hooks: hooks)));
	}

	*prCall { |msg, addr|
		var id = msg[1], hook = msg[2].asSymbol.asClass, selector = msg[3].asSymbol, context, result, failed;
		if(this.prAnswers(addr).not) { ^this };
		if(hook.isNil or: { this.hooks.includes(hook).not }) {
			^this.prReply(addr, id, "error", "% isn't a hook".format(msg[2]));
		};
		context = ();
		msg.copyToEnd(4).pairsDo { |key, value|
			context[key.asSymbol] = if(value.isKindOf(Symbol)) { value.asString } { value };
		};
		#[\receiver, \selector, \keyword].do { |key|
			context[key] !? { context[key] = context[key].asSymbol };
		};
		failed = false;
		result = try { hook.perform(selector, context) } { |error| failed = true; error.errorString };
		if(failed) { ^this.prReply(addr, id, "error", result) };
		if(result.isKindOf(Collection) and: { result.isKindOf(String).not }) {
			result = result.asArray.keep(maxResults);
		};
		this.prReply(addr, id, "ok", this.toJSON(result));
	}

	*prEval { |msg, addr|
		var id = msg[1], code = msg[2].asString, function, result, failed = false;
		if(this.prAnswers(addr).not) { ^this };
		if(allowEval.not) {
			^this.prReply(addr, id, "error", "sclang doesn't allow it, set LSPHook.allowEval = true to allow it");
		};
		"LSPHook: evaluating code from sc_mcp:\n%\n".postf(code);
		function = code.compile;
		if(function.isNil) { ^this.prReply(addr, id, "error", "The code doesn't compile, see the post window.") };
		result = try { function.value } { |error| failed = true; error.errorString };
		if(failed) { ^this.prReply(addr, id, "error", result) };
		result = result.asCompileString;
		if(result.size > 8000) { result = result.keep(8000) ++ " ..." };
		this.prReply(addr, id, "ok", result);
	}

	// Plain data as JSON, anything else as its string.
	*toJSON { |object|
		^case
		{ object.isNil } { "null" }
		{ object === true } { "true" }
		{ object === false } { "false" }
		{ object.isKindOf(Integer) } { object.asString }
		{ object.isKindOf(Float) } { if(object.isNaN or: { object.abs == inf }) { "null" } { object.asString } }
		{ object.isKindOf(Symbol) or: { object.isKindOf(String) } } { this.prJSONString(object.asString) }
		{ object.isKindOf(Dictionary) } {
			"{" ++ object.keys.asArray.collect { |key|
				this.prJSONString(key.asString) ++ ":" ++ this.toJSON(object[key])
			}.join(",") ++ "}"
		}
		{ object.isKindOf(Collection) } { "[" ++ object.asArray.collect { |x| this.toJSON(x) }.join(",") ++ "]" }
		{ this.prJSONString(object.asString) }
	}

	*prJSONString { |string|
		^String.streamContents { |stream|
			stream.put($");
			string.do { |char|
				case
				{ char == $" } { stream << "\\\"" }
				{ char == $\\ } { stream << "\\\\" }
				{ char == $\n } { stream << "\\n" }
				{ char == $\t } { stream << "\\t" }
				{ char.ascii.inclusivelyBetween(0, 31) } { stream << "\\u" << char.ascii.asHexString(4) }
				{ stream.put(char) }
			};
			stream.put($");
		}
	}
}

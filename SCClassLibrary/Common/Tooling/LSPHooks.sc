// The hooks the class library comes with, see LSPHook.

// SynthDef names, for Synth(\..., \instrument, \..., and Pmono(\...
SynthDefNameHook : LSPHook {
	*completeSymbol { |context|
		var wanted = (context[\keyword] == \instrument)
			or: { context[\previous] == "\\instrument" }
			or: { context[\argument] == 0 and: { #[\Synth, \Pmono, \PmonoArtic, \SynthDef].includes(context[\receiver]) } };
		if(wanted.not) { ^nil };
		// The library's own SynthDefs, e.g. system_diskout_2, only when asked for.
		^SynthDescLib.all.values.collect { |lib| lib.synthDescs.keys.asArray }.flat.asSet.asArray.sort
			.reject { |name| name.asString.beginsWith("system_") and: { context[\prefix].beginsWith("sys").not } }
			.collect { |name| (label: name, detail: "SynthDef") }
	}
}

// Event keys, for the keys of a Pbind and its kin.
EventKeyHook : LSPHook {
	*completeSymbol { |context|
		var first, keys;
		first = switch(context[\receiver],
			\Pbind, 0, \Pmono, 1, \PmonoArtic, 1, \Pbindef, 1, \Pbindf, 1,
			nil
		);
		if(first.isNil or: { context[\argument] < first } or: { (context[\argument] - first).odd }) { ^nil };
		keys = Set.new;
		Event.partialEvents.do { |event| keys.addAll(event.keys) };
		keys.addAll(#[\instrument, \dur, \legato, \sustain, \amp, \freq, \note, \degree, \midinote, \octave, \scale,
			\root, \out, \pan, \group, \addAction, \type, \strum, \stretch, \tempo, \lag, \timingOffset]);
		^keys.asArray.sort.collect { |key| (label: key, detail: "event key") }
	}
}

// Names already in use, for Ndef(\..., Pdef(\..., Tdef(\..., Pdefn(\... and Fdef(\...
ProxyNameHook : LSPHook {
	*completeSymbol { |context|
		var names;
		if(context[\argument] != 0) { ^nil };
		names = switch(context[\receiver],
			\Ndef, { Ndef.all.values.collect { |space| space.envir.keys.asArray }.flat },
			\Pdef, { Pdef.all.keys.asArray },
			\Pbindef, { Pdef.all.keys.asArray },
			\Tdef, { Tdef.all.keys.asArray },
			\Pdefn, { Pdefn.all.keys.asArray },
			\Fdef, { Fdef.all.keys.asArray },
			{ ^nil }
		);
		^names.asSet.asArray.sort.collect { |name| (label: name, detail: context[\receiver].asString) }
	}
}
